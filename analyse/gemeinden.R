#### GEMEINDEN KANTON ZH ####

# Import und Verarbeitung Gemeindedaten

library(tidyverse)
library(sf)

# URL of the WFS service
wfs_all <- "https://maps.zh.ch/wfs/GemZHWFS"
# Projection: EPSG:2056 (CH1903+ / LV95)
crs <- 2056
# Parsing the URL
url <- httr2::url_parse(wfs_all)

# Grenzen
url$query <- list(service = "wfs",
                  version = "2.0.0",
                  request = "GetFeature",
                  typename = "ms:gem_grenzen",
                  srsName = paste0("EPSG:", crs)
)
request <- httr2::url_build(url)

# Gemeindegrenzen werden eingelesen, type = 6: Create a MULTIPOLYGON geometry column.
gemeinden_zh_gis <- 
  request %>% 
  read_sf(type = 6) %>% 
  st_transform(crs = sf::st_crs(crs)) %>%
  select(bfsnr = bfs, gemeinde = gemeindename)

gemeinden_zh <- gemeinden_zh_gis %>%
  filter(!is.na(gemeinde)) %>%
  distinct(bfsnr, .keep_all = T) %>%
  st_drop_geometry(.) %>% 
  arrange(gemeinde)
