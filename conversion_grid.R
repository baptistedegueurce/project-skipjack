library(dplyr)
library(stringr)
library(sf)
library(purrr)
library(readxl)

# ---- 1. Tes données ----
# Remplace ceci par tes vraies données, ex :
# df <- read.csv("mon_fichier.csv", stringsAsFactors = FALSE)
skj <- read.csv("S10/SIG/Projet/IOTC-2026-WPTT28-DATA06-SKJ-1982-2025/IOTC-2026-WPTT28-DATA06-SKJ-1982-2025-WIDE-FORMAT.csv")
df <- readxl::read_xlsx("S10/SIG/Projet/IOTC-2026-WPTT28-DATA06-SKJ-1982-2025/IOTC-2026-WPTT28-DATA06-SKJ-1982-2025-FIELDS-CODE-LISTS.xlsx", sheet = "FISHING GROUNDS")
# ---- 2. Extraction des bornes lat/lon via regex ----
# Format attendu : "DD° - DD°[N/S]; DDD° - DDD°E"
pattern <- "^(\\d+)°\\s*-\\s*(\\d+)°\\s*([NS])\\s*;\\s*(\\d+)°\\s*-\\s*(\\d+)°\\s*E$"

m <- str_match(str_trim(df$DESCRIPTION_FR), pattern)
colnames(m) <- c("full", "lat1", "lat2", "hemi", "lon1", "lon2")

df <- bind_cols(df, as.data.frame(m[, -1], stringsAsFactors = FALSE))

df <- df %>%
  mutate(
    lat1 = as.numeric(lat1),
    lat2 = as.numeric(lat2),
    lon1 = as.numeric(lon1),
    lon2 = as.numeric(lon2),
    lat_min = if_else(hemi == "S", -pmax(lat1, lat2), pmin(lat1, lat2)),
    lat_max = if_else(hemi == "S", -pmin(lat1, lat2), pmax(lat1, lat2)),
    lon_min = pmin(lon1, lon2),
    lon_max = pmax(lon1, lon2)
  )

# Lignes qui n'ont pas matché le format standard (ex : IRREGAREA)
non_matched <- df %>% filter(is.na(lat_min) | is.na(lon_min))
if (nrow(non_matched) > 0) {
  message("Attention : ", nrow(non_matched),
          " ligne(s) n'ont pas pu être converties automatiquement (format non standard) :")
  print(non_matched$DESCRIPTION_FR)
}

# ---- 3. Construction des polygones rectangulaires ----
make_poly <- function(lon_min, lon_max, lat_min, lat_max) {
  if (any(is.na(c(lon_min, lon_max, lat_min, lat_max)))) {
    return(st_polygon())  # polygone vide si non convertible
  }
  mat <- matrix(
    c(
      lon_min, lat_min,
      lon_max, lat_min,
      lon_max, lat_max,
      lon_min, lat_max,
      lon_min, lat_min
    ),
    ncol = 2, byrow = TRUE
  )
  st_polygon(list(mat))
}

geoms <- pmap(
  df[, c("lon_min", "lon_max", "lat_min", "lat_max")],
  make_poly
)

df_sf <- st_sf(df, geometry = st_sfc(geoms, crs = 4326))

# ---- 4. Vérification / export ----
print(df_sf)

# Export en GeoJSON ou Shapefile si besoin :
# st_write(df_sf, "iotc_grids.geojson", delete_dsn = TRUE)
st_write(skj_sf, "S10/SIG/Projet/iotc_grids.shp")

# ---- 5. Visualisation rapide ----
plot(st_geometry(df_sf), border = "steelblue", col = NA)

# ---- 6. Jointure data x SIG ----
skj_sf <- left_join(skj, df_sf %>% select(FISHING_GROUND_CODE, geometry), by = "FISHING_GROUND_CODE") %>%
  st_as_sf()
