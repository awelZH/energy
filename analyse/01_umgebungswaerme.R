#### Umgebungswärme ####

# Für die Energiestatistik werden auch Angaben zur Nutzung der Umgebungswärme ausgewertet. Die Daten werden aus diversen Quellen eingelesen, umgeformt und ausgewertet.

rm(list = ls())

# Scripts und libraries einlesen
source(here::here("analyse/gemeinden.R"))
library(readr)
library(readxl)
library(httr)
#library(stringr)
kennwerte <- read.csv(here::here("data/input/kennwerte.csv"), sep = ";")


######################################
####### DATEN FÖRDERPLATTFORM ########
######################################

#### INPUT DATEN ####

foerderung_wp_input <- read_excel(here::here("data/input/export_foerderplattform_umgebungswaerme.xlsx")) %>%
  set_names(c("Gesuch_ID", "Datum")) %>%
  mutate(
    typ = case_when(
      str_ends(Gesuch_ID, "05") ~ "luft_wp",
      str_ends(Gesuch_ID, "06") ~ "sole_wp"
      ),
    jahr = year(Datum)
    ) %>%
  group_by(jahr) %>%
  mutate(verhaeltnis_05_06_foerderung = suppressWarnings(as.numeric(sum(str_ends(Gesuch_ID, "05"), na.rm = TRUE)/sum(str_ends(Gesuch_ID, "06"), na.rm = TRUE)))) %>%
  filter(!is.na(verhaeltnis_05_06_foerderung), verhaeltnis_05_06_foerderung != Inf) %>%
  distinct(jahr, verhaeltnis_05_06_foerderung)

mean_verhaeltnis_foerderung <- mean(foerderung_wp_input$verhaeltnis_05_06_foerderung, na.rm = TRUE)


##########################
####### DATEN BFE ########
##########################

#### INPUT DATEN ####

# Schweizerische Statistik der erneuerbaren Energien temporär speichern

temp_file <- tempfile(fileext = ".xlsx")
GET("https://pubdb.bfe.admin.ch/de/publication/download/8787", write_disk(temp_file, overwrite = TRUE)) #s. https://pubdb.bfe.admin.ch/de/suche?keywords=404

input_bfe_wp <- read_excel(temp_file, sheet = "Anhang B", skip = 2)

bfe_wp <- input_bfe_wp %>%
  pivot_longer(cols = names(input_bfe_wp[(which(names(input_bfe_wp) == "Einheit")+1):(which(names(input_bfe_wp) == "Herkunft")-1)]), #Jahreszahlen befinden sich zw. den Spalten "Einheit" und "Herkunft"
               names_to = "jahr",       # Die neuen Spaltennamen (Jahreszahlen) werden hier gespeichert
               values_to = "Wert") %>%  # Die Werte werden in die neue Spalte "Wert" verschoben
  mutate(Jahr = as.numeric(sub("X", "", jahr))) %>%  # Entferne das 'X' und konvertiere in numerische Werte
  select(Technologie, Zeileninhalt, Einheit, Jahr, Wert) %>%
  filter(str_detect(str_to_lower(Technologie), "luft / wasser") |
           str_detect(str_to_lower(Technologie), "luft / luft") |
           str_detect(str_to_lower(Technologie), "sole / wasser") |
           str_detect(str_to_lower(Technologie), "wasser / wasser")) %>%
  mutate(Technologie = str_remove(Technologie, "^\\-\\s*")) %>% # Entfernt "- " am Anfang
  mutate(Einheit = ifelse(grepl("anzahl", Zeileninhalt, ignore.case = TRUE), "Anzahl", Einheit)) %>%
  mutate(Technologie = gsub("Luft / Wasser", "l_w_wp", Technologie)) %>%
  mutate(Technologie = gsub("Luft / Luft", "l_l_wp", Technologie)) %>%
  mutate(Technologie = gsub("Sole / Wasser", "s_w_wp", Technologie)) %>%
  mutate(Technologie = gsub("Wasser / Wasser", "w_w_wp", Technologie)) %>%
  select(-Zeileninhalt) %>%
  unite("Technologie_Einheit", Technologie, Einheit, sep = "_") %>%
  pivot_wider(names_from = Technologie_Einheit, values_from = Wert) %>%
  mutate_all(as.numeric) %>%
  rename_with(tolower) %>% #Spaltennamen klein schreiben
  select(jahr, l_w_wp_anzahl, l_l_wp_anzahl, s_w_wp_anzahl, w_w_wp_anzahl, l_w_wp_gwh, l_l_wp_gwh, s_w_wp_gwh, w_w_wp_gwh) %>%
  rename_with(~ paste0(., "_ch"), -jahr) %>%
  group_by(jahr) %>%
  mutate(l_wp_anzahl_gesamt = l_w_wp_anzahl_ch + l_l_wp_anzahl_ch) %>%
  mutate(verhaeltnis_05_06_bfe = l_wp_anzahl_gesamt/s_w_wp_anzahl_ch)
  


##########################
####### ERDSONDEN ########
##########################

#### INPUT DATEN ####

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
f_verfueg <- read_excel("K:/BD-AWEL-050-EN/Elektrizitätswirtschaft/Administration/Praktikum/Levi_Fuchs/Projekte/Energiestatistik_neu/R_Project/Erdsonden_WP/Auszug Total Ende 2024.xlsx") %>%
  mutate(datum_verfuegung = `Datum Verfügung`) %>% 
  select(GBS,datum_verfuegung)

master_join <- master %>% 
  left_join(f_verfueg, by = c("gbs_id" = "GBS")) %>%
  st_join(gemeinden_zh_gis)


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
    rubrik = "Umgebungswärme",
    thema = "Sole_Wasser_WP",
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



########################
####### LUFT-WP ########
########################

#### AUFBEREITUNG DATEN ####

erdsonden_sum <- erdsonden_final %>%
  group_by(jahr, rubrik, thema, subthema, einheit) %>%
  summarise(wert = sum(wert, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(
    names_from = c(subthema, einheit),
    values_from = wert
  )

umgebungswaerme_final <- bfe_wp %>%
  left_join(erdsonden_sum, by = "jahr") %>%
  left_join(foerderung_wp_input, by = "jahr") %>%
  mutate(
    l_wp_anzahl_zh_foerderung = Anzahl_Anzahl *
      if_else(
        is.na(verhaeltnis_05_06_foerderung),
        mean_verhaeltnis_foerderung,
        verhaeltnis_05_06_foerderung
      ),
    l_wp_waerme_kw_zh_foerderung = Wärme_kW *
      if_else(
        is.na(verhaeltnis_05_06_foerderung),
        mean_verhaeltnis_foerderung,
        verhaeltnis_05_06_foerderung
      ),
    l_wp_kaelte_kw_zh_foerderung = Kälte_kW *
      if_else(
        is.na(verhaeltnis_05_06_foerderung),
        mean_verhaeltnis_foerderung,
        verhaeltnis_05_06_foerderung
      ),
    l_wp_waerme_mwh_zh_foerderung = Wärme_MWh *
      if_else(
        is.na(verhaeltnis_05_06_foerderung),
        mean_verhaeltnis_foerderung,
        verhaeltnis_05_06_foerderung
      ),
    l_wp_kaelte_mwh_zh_foerderung = Kälte_MWh *
      if_else(
        is.na(verhaeltnis_05_06_foerderung),
        mean_verhaeltnis_foerderung,
        verhaeltnis_05_06_foerderung
      ),
    l_wp_anzahl_zh_bfe = Anzahl_Anzahl * verhaeltnis_05_06_bfe,
    l_wp_waerme_kw_zh_bfe = Wärme_kW * verhaeltnis_05_06_bfe,
    l_wp_kaelte_kw_zh_bfe = Kälte_kW * verhaeltnis_05_06_bfe,
    l_wp_waerme_mwh_zh_bfe = Wärme_MWh * verhaeltnis_05_06_bfe,
    l_wp_kaelte_mwh_zh_bfe = Kälte_MWh * verhaeltnis_05_06_bfe
  ) %>%
  ungroup() %>%
  select(-c(l_wp_anzahl_gesamt, verhaeltnis_05_06_bfe, rubrik, thema, verhaeltnis_05_06_foerderung)) %>%
  pivot_longer(
    cols = -jahr,           # alle Spalten außer 'jahr'
    names_to = "name",     # die Spaltennamen kommen in diese Spalte
    values_to = "wert"      # die Zellwerte kommen hier rein
  ) %>%
  mutate(
    einheit = case_when(
      str_detect(name, "kw|kW") ~ "kW",
      str_detect(name, "mwh|MWh") ~ "MWh",
      str_detect(name, "gwh") ~ "GWh",
      str_detect(name, "anzahl|Anzahl") ~ "Anzahl",
      TRUE ~ NA_character_   # für alle anderen Fälle
    ),
    rubrik = "Umweltwärme",
    ort = if_else(
      str_detect(name, "_ch"),
      "Schweiz",
      "Kanton"
    ),
    thema = case_when(
      str_detect(name, "l_w_wp") ~ "LuftWasserWP",
      str_detect(name, "l_l_wp") ~ "LuftLuftWP",
      str_starts(name, "l_wp") ~ "LuftWP",
      str_detect(name, "s_w_wp|Anzahl|Kälte|Wärme") ~ "SoleWasserWP",
      str_detect(name, "w_w_wp") ~ "WasserWasserWP",
      TRUE ~ NA_character_   # für alle anderen Fälle
    ),
    subthema = case_when(
      str_detect(name, "waerme|Wärme") & str_detect(name, "_foerderung") ~ "Wärme (GP)",
      str_detect(name, "kaelte|Kälte") & str_detect(name, "_foerderung") ~ "Kälte (GP)",
      str_detect(name, "anzahl|Anzahl") & str_detect(name, "_foerderung") ~ "Anzahl (GP)",
      str_detect(name, "waerme|Wärme") & str_detect(name, "_bfe") ~ "Wärme (BFE)",
      str_detect(name, "kaelte|Kälte") & str_detect(name, "_bfe") ~ "Kälte (BFE)",
      str_detect(name, "anzahl|Anzahl") & str_detect(name, "_bfe") ~ "Anzahl (BFE)",
      str_detect(name, "waerme|Wärme") ~ "Wärme",
      str_detect(name, "kaelte|Kälte") ~ "Kälte",
      str_detect(name, "anzahl|Anzahl") ~ "Anzahl",
      TRUE ~ "Energie"   # für alle anderen Fälle
    )
  ) %>%
  select(jahr, ort, rubrik, thema, subthema, wert, einheit)


write_excel_csv(umgebungswaerme_final, here::here("data/output/umgebungswaerme.csv"), delim = ";")



