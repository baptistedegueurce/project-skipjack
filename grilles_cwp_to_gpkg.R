# Conversion des codes de zones de pêche (grilles CWP / IOTC) en polygones SIG
# install.packages(c("sf", "dplyr", "readxl", "readr", "stringr"))
library(sf)
library(dplyr)
library(stringr)

SRC   <- "fishing_grounds.xlsx"   # ou .csv
CATCH <- NULL                     # ex. "captures.csv" (colonne FISHING_GROUND + tonnage, engin...)
OUT   <- "fishing_grounds.gpkg"

# Quadrant CWP -> signes latitude / longitude
QUAD <- list("1" = c(1, 1), "2" = c(-1, 1), "3" = c(-1, -1), "4" = c(1, -1))

make_box <- function(xs, ys) {
  xs <- sort(xs); ys <- sort(ys)
  st_polygon(list(matrix(c(xs[1], ys[1], xs[2], ys[1], xs[2], ys[2],
                           xs[1], ys[2], xs[1], ys[1]), ncol = 2, byrow = TRUE)))
}

# '00° - 10°S; 040° - 060°E' -> polygone
geom_from_description <- function(desc) {
  m <- str_match(desc, "(\\d+)°\\s*-\\s*(\\d+)°\\s*([NS]);\\s*(\\d+)°\\s*-\\s*(\\d+)°\\s*([EW])")
  if (is.na(m[1, 1])) return(NULL)
  sla <- ifelse(m[1, 4] == "S", -1, 1)
  slo <- ifelse(m[1, 7] == "W", -1, 1)
  make_box(slo * as.numeric(m[1, 5:6]), sla * as.numeric(m[1, 2:3]))
}

# Décodage du code CWP à 7 caractères, taille lue dans AREA_TYPE (GRIDlatxlon)
geom_from_code <- function(code, area_type) {
  m <- str_match(str_trim(area_type), "^GRID(\\d+)x(\\d+)$")
  q <- substr(code, 2, 2)
  if (is.na(m[1, 1]) || nchar(code) != 7 || !q %in% names(QUAD)) return(NULL)
  dlat <- as.numeric(m[1, 2]); dlon <- as.numeric(m[1, 3])
  lat  <- as.numeric(substr(code, 3, 4)); lon <- as.numeric(substr(code, 5, 7))
  s <- QUAD[[q]]
  make_box(s[2] * c(lon, lon + dlon), s[1] * c(lat, lat + dlat))
}

df <- if (grepl("xlsx$", SRC)) readxl::read_excel(SRC) else readr::read_csv(SRC)
df <- df %>% mutate(FISHING_GROUND = str_trim(as.character(FISHING_GROUND)))

g_desc <- lapply(df$DESCRIPTION_EN, geom_from_description)
g_code <- mapply(geom_from_code, df$FISHING_GROUND, df$AREA_TYPE, SIMPLIFY = FALSE)

# Contrôle de cohérence code <-> description
mismatch <- df$FISHING_GROUND[mapply(function(a, b)
  !is.null(a) && !is.null(b) && !isTRUE(all.equal(unclass(a), unclass(b))), g_desc, g_code)]
if (length(mismatch)) message("⚠ ", length(mismatch), " codes incohérents : ",
                              paste(head(mismatch, 20), collapse = ", "))

geoms <- mapply(function(a, b) if (!is.null(a)) a else b, g_desc, g_code, SIMPLIFY = FALSE)
ok <- !vapply(geoms, is.null, logical(1))
message(sum(!ok), " zones non-grille (irrégulières) à traiter à part : ",
        paste(head(df$FISHING_GROUND[!ok], 20), collapse = ", "))

gdf <- st_sf(df[ok, ], geometry = st_sfc(geoms[ok], crs = 4326))
cent <- st_coordinates(st_centroid(st_geometry(gdf)))
gdf$LON_C <- cent[, 1]
gdf$LAT_C <- cent[, 2]

st_write(gdf, OUT, layer = "grilles", delete_layer = TRUE)

# Jointure optionnelle avec les captures, une couche par taille de grille
if (!is.null(CATCH)) {
  catch <- readr::read_csv(CATCH) %>%
    mutate(FISHING_GROUND = str_trim(as.character(FISHING_GROUND)))
  joined <- inner_join(gdf, catch, by = "FISHING_GROUND")
  for (at in unique(joined$AREA_TYPE)) {
    st_write(filter(joined, AREA_TYPE == at), OUT,
             layer = paste0("captures_", at), delete_layer = TRUE)
  }
}

message("OK -> ", OUT, " (", nrow(gdf), " cellules)")
