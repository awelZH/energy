#### Wärmepumpen (Erdsonden) ####

# Für die Energiestatistik werden auch Angaben zu den Wärmepumpen ausgewertet
# Die Daten für die Erdsonden liegen als Geodaten bzw. als WFS-Geodienst vor

rm(list = ls())

# Scripts und libraries einlesen
source(here::here("analyse/gemeinden.R"))
library(readr)


#### INPUT DATEN ####

kennwerte <- read.csv(here::here("data/input/kennwerte.csv"), sep = ";")

# URL of the WFS service
wfs_all <- "https://maps.zh.ch/wfs/OGDZHWFS"
# Projection: EPSG:2056 (CH1903+ / LV95)
crs <- 2056
# Parsing the URL
url <- httr2::url_parse(wfs_all)


# Einlesen der Point Feature Class "Mastersonden", Quelle: WFS-Geodienst

# Mastersonden
url$query <- list(
  service = "wfs",
  version = "2.0.0",
  request = "GetFeature",
  typename = "ms:ogd-0182_giszhpub_gs_mastersonden_p",
  srsName = paste0("EPSG:", crs)
)
request <- httr2::url_build(url)

# Mastersonden werden eingelesen, type = 1: Create a POINT geometry column.
master <- 
  request %>% 
  read_sf(type = 1) %>% 
  st_transform(crs = sf::st_crs(crs))

# Verfügungsdatum Erdsonde ist leider (noch) nicht mitenthalten -> wird über separates csv eingelsen und an master gejoint
f_verfueg <- read_delim("https://raw.githubusercontent.com/awelZH/energy/refs/heads/main/data/gbs_dat_verfuegung.csv", delim = ",") %>%
  mutate(datum_verfuegung = as.Date(datum_verfuegung, format = "%d.%m.%Y")) %>% 
  group_by(gbs_id,datum_verfuegung) %>% 
  summarise()

master <- master %>% 
  left_join(f_verfueg, by = "gbs_id")

master_join <- st_join(master, gemeinden_zh_gis)




#### AUFBEREITUNG DATEN ####

dat_tmp <- tibble(master_join) %>%
  select(status) %>% 
  group_by(status) %>% 
  summarise(tot = n())

# Define the time range
start_year <- min(as.integer(format(master_join$datum_verfuegung, "%Y")), na.rm = T)
current_year <- max(as.integer(format(master_join$datum_verfuegung, "%Y")), na.rm = T)


# Create a sequence of years from 1983 to the current year
years <- tibble(jahr = seq(start_year, current_year))

# Extract unique combinations of bfsnr and gemeinde values
bfsnr_name_values <- master_join %>% st_set_geometry(NULL) %>% select(bfsnr, gemeinde) %>% distinct()

# Create a complete time series for each combination of bfsnr and gemeinde
complete_time_series <- bfsnr_name_values %>%
  crossing(jahr = seq(start_year, current_year))

erdsonden_final <- tibble(master_join) %>% 
  mutate(jahr = year(datum_verfuegung)) %>%
  group_by(bfsnr, gemeinde, jahr) %>% 
  summarise(
    erd_wp_waerme_jahr_kw_gem = sum(waermeentnahme, na.rm = TRUE),
    erd_wp_kaelte_jahr_kw_gem = sum(waermeeintrag, na.rm = TRUE),
    anzahl_jahr_gem = n(),
    .groups = 'drop'
  ) %>%
  right_join(complete_time_series, by = c("bfsnr", "gemeinde", "jahr")) %>%
  arrange(bfsnr, gemeinde, jahr) %>%
  group_by(bfsnr, gemeinde) %>%
  mutate(
    erd_wp_waerme_jahr_kw_gem = coalesce(erd_wp_waerme_jahr_kw_gem, 0),
    erd_wp_kaelte_jahr_kw_gem = coalesce(erd_wp_kaelte_jahr_kw_gem, 0),
    anzahl_jahr_gem = coalesce(anzahl_jahr_gem, 0),
    erd_wp_waerme_kw_gem = cumsum(erd_wp_waerme_jahr_kw_gem),
    erd_wp_kaelte_kw_gem = cumsum(erd_wp_kaelte_jahr_kw_gem),
    anzahl_gem = cumsum(anzahl_jahr_gem)
  ) %>%
  mutate(
    erd_wp_waerme_mwh_gem = erd_wp_waerme_kw_gem*kennwerte$betriebszeit_wp/1000,
    erd_wp_kaelte_mwh_gem = erd_wp_kaelte_kw_gem*kennwerte$betriebszeit_wp/1000
  ) %>%
  ungroup() %>%
  pivot_longer(
    cols = c(erd_wp_waerme_kw_gem, erd_wp_kaelte_kw_gem, erd_wp_waerme_mwh_gem, erd_wp_kaelte_mwh_gem, anzahl_gem),
    names_to = "variable",
    values_to = "wert"
  ) %>%
  mutate(
    einheit = case_when(
      variable == "erd_wp_waerme_kw_gem"   ~ "kW",
      variable == "erd_wp_kaelte_kw_gem"   ~ "kW",
      variable == "erd_wp_waerme_mwh_gem"   ~ "MWh",
      variable == "erd_wp_kaelte_mwh_gem"   ~ "MWh",
      variable == "anzahl_gem"    ~ "Anzahl"
    ),
    ort = gemeinde,
    rubrik = "Wärme",
    thema = "Erdsonden WP",
    subthema = case_when(
      variable == "erd_wp_waerme_kw_gem"   ~ "Wärme",
      variable == "erd_wp_kaelte_kw_gem"   ~ "Kälte",
      variable == "erd_wp_waerme_mwh_gem"   ~ "Wärme",
      variable == "erd_wp_kaelte_mwh_gem"   ~ "Kälte",
      variable == "anzahl_gem"    ~ "Anzahl"
    )
  ) %>%
  select(jahr, bfsnr, ort, rubrik, thema, subthema, wert, einheit)


write_excel_csv(erdsonden_final, here::here("data/output/erdsonden.csv"), delim = ";")
