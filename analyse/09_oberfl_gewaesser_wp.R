#### Oberflächen-Gewässer ####

# Für die Energiestatistik werden auch Angaben zu den Wärmepumpen ausgewertet. Ein Jahresstand der aktuellen Daten wird von Marco Calderoni bereinigt zur Verfügung gestellt. Die Standorte der Anlagen liegen als Geodaten bzw. als WFS-Geodienst vor und können daher direkt mit R eingelesen, umgeformt und ausgewertet werden. 


rm(list = ls())

# Scripts und libraries einlesen
source(here::here("analyse/gemeinden.R"))
library(readr)
library(readxl)
library(tcltk)


#### INPUT DATEN ####

# bisherige Anlage-Daten einlesen
gewaesser_wp_olddata <- read.csv(here::here("data/input/oberfl_gewaesser_anlagen_26_08_25.csv"), sep=";")

gewaesser_wp_jahresstand_input <- read_excel("K:/BD-AWEL-050-EN/Elektrizitätswirtschaft/Administration/Praktikum/Levi_Fuchs/Projekte/Energiestatistik_neu/R_Project/Oberflaechen_WP/oberflaechen_wp_jahresstand_2024.xlsx")

# URL of the WFS service
wfs_all <- "https://maps.zh.ch/wfs/OGDZHWFS"

# Projection: EPSG:2056 (CH1903+ / LV95)
crs <- 2056

#current year
current_year <- as.integer(format(Sys.Date(), "%Y"))

# Parsing the URL
url <- httr2::url_parse(wfs_all)

kennwerte <- read.csv(here::here("data/input/kennwerte.csv"), sep=";")

###########################################################
#### Einlesen Point Feature Class "Wasserrechte" (WFS) ####
###########################################################

# Oberflächen-WP Anlagen Punkte (bestehend) 
url$query <- list(
  service = "wfs",
  version = "2.0.0",
  request = "GetFeature",
  typename = "ms:ogd-0051_giszhpub_wb_wasserrechte_p",
  srsName = paste0("EPSG:", crs)
)
request_bestand <- httr2::url_build(url)

# Oberflächen-WP Anlagen Punkte (gelöscht)
url$query <- list(
  service = "wfs",
  version = "2.0.0",
  request = "GetFeature",
  typename = "ms:ogd-0051_giszhpub_wb_wasserrechte_geloescht_p",
  srsName = paste0("EPSG:", crs)
)
request_geloescht <- httr2::url_build(url)

# Oberflächen-WP Anlagen Linien (bestehend)
url$query <- list(
  service = "wfs",
  version = "2.0.0",
  request = "GetFeature",
  typename = "ms:ogd-0051_giszhpub_wb_wasserrechte_l",
  srsName = paste0("EPSG:", crs)
)
request_bestand_l <- httr2::url_build(url)

# Oberflächen-WP Anlagen Linien (gelöscht)
url$query <- list(
  service = "wfs",
  version = "2.0.0",
  request = "GetFeature",
  typename = "ms:ogd-0051_giszhpub_wb_wasserrechte_geloescht_l",
  srsName = paste0("EPSG:", crs)
)
request_geloescht_l <- httr2::url_build(url)


# Anlagen werden eingelesen, type = 1: Create a POINT geometry column.
wfs_bestand_input <- #bestehende Anlagen (Punkte)
  request_bestand %>% 
  sf::read_sf(type = 1) %>% 
  sf::st_transform(crs = sf::st_crs(crs))

wfs_geloescht_input <- #gelöschte Anlagen (Punkte)
  request_geloescht %>% 
  sf::read_sf(type = 1) %>% 
  sf::st_transform(crs = sf::st_crs(crs))

wfs_bestand_input_l <- #bestehende Anlagen (Linien)
  request_bestand_l %>% 
  sf::read_sf() %>%  # Einlesen der Daten
  sf::st_transform(crs = sf::st_crs(crs))  # Transformation in das gewünschte CRS

wfs_geloescht_input_l <- #gelöschte Anlagen (Linien)
  request_geloescht_l %>% 
  sf::read_sf() %>%  # Einlesen der Daten
  sf::st_transform(crs = sf::st_crs(crs))  # Transformation in das gewünschte CRS

wfs_input_prov <- rbind(wfs_bestand_input, wfs_geloescht_input) #Punktdaten bestehend und gelöscht zusammenführen 
wfs_input_l_prov <- rbind(wfs_bestand_input_l, wfs_geloescht_input_l) #Liniendaten bestehend und gelöscht zusammenführen 

#sortieren und Duplikate (basierend auf WRNr) entfernen 
wfs_input_tidy <- wfs_input_prov %>%
  mutate(knotentyp_priority = case_when(
    grepl("^effektiver Entnahmeort", knotentyp_txt) ~ 1,  # Beginnt mit "effektiver Entnahmeort"
    grepl("^Entnahme", knotentyp_txt) ~ 2,  # Beginnt mit "Entnahme"
    grepl("Rückgabe", knotentyp_txt) ~ 3,  # Enthält "Rückgabe"
    TRUE ~ 4  # Alle anderen
  )) %>%
  arrange(wasserrechts_nr, knotentyp_priority) %>%  # Sortiere nach wasserrechts_nr und der neuen Priorität
  distinct(wasserrechts_nr, .keep_all = TRUE)  # Behalte nur die erste Zeile für jede wasserrechts_nr



# Verschneiden Gemeindegrenzen und Liniendatensätze

lines_with_name <- sf::st_join(
  wfs_input_l_prov,    # Der Liniendatensatz
  gemeinden_zh_gis,             # Der Polygondatensatz der Gemeinden
  join = sf::st_intersects,  # Räumliche Beziehung: Schnittpunkte
  left = T           # Beibehaltung aller Linien 
) %>%
  filter(!is.na(wasserrechts_nr), bfsnr != 0) %>%  # NA-Werte in "wasserrechts_nr" ausschließen
  arrange(wasserrechts_nr, bfsnr, gemeinde) %>%  # Optional: nach sinnvollen Attributen sortieren
  group_by(wasserrechts_nr) %>%  # Gruppieren nach "wasserrechts_nr"
  slice(1) %>%  # Nur den ersten Eintrag jeder Gruppe behalten
  ungroup()

join_nearest <- wfs_input_tidy %>%
  mutate(nearest_polygon_index = st_nearest_feature(geometry, gemeinden_zh_gis)) %>%  # Finde den Index des nächsten Polygons
  # Extrahiere bfsnr basierend auf diesem Index aus den gemeinden_zh_gis
  mutate(nearest_bfsnr = gemeinden_zh_gis$bfsnr[nearest_polygon_index],
         nearest_gemeinde = gemeinden_zh_gis$gemeinde[nearest_polygon_index]) %>%
  # Wenn du die Geometrie nicht mehr benötigst, kannst du sie entfernen
  st_drop_geometry()


# Einlesen der bisherigen Anlagedaten und des neuen Jahresstands

gewaesser_wp_jahresstand <- gewaesser_wp_jahresstand_input %>%
  rename(Betrieb = colnames(gewaesser_wp_jahresstand_input)[6]) %>%  # Umbenennen der sechsten Spalte
  mutate(
    Betrieb = if_else(grepl("nicht", Betrieb, ignore.case = TRUE), "Nein", "Ja"),  # Bedingung anwenden
    jahr = current_year-1
  ) %>%
  rename_with(~ ifelse(grepl("KEk", ., ignore.case = TRUE), "GebKEkWfix", .)) %>%
  rename_with(~ ifelse(grepl("WEk", ., ignore.case = TRUE), "GebWEkWfix", .))



# Joins nach Priorität:

# 1. Join exakt zw. WFS Daten und Gemeindedatensatz (GIS-Daten)
# 2. Join basierend auf Gemeindenamen mit Gemeindedatensatz
# 3. Join intersect zw. WFS Daten und Gemeindedatensatz (GIS-Daten)
# 4. Join nearest polygon zw. WFS Daten und Gemeindedatensatz (GIS-Daten)
# 5. Join mit alten Daten (olddata)

#Join WFS-Daten und Gemeinden
join_exact <- st_join(wfs_input_tidy, gemeinden_zh_gis) %>%
  st_drop_geometry()

#Join WFS-Daten und Energiedaten
gewaesser_wp_jahresstand_final <- left_join(gewaesser_wp_jahresstand, join_exact, by = c("WRNr" = "wasserrechts_nr")) %>%
  mutate(
    gemeinde = coalesce(Gemeinde, gemeinde)
  ) %>%
  select(-Gemeinde) %>%
  left_join(gemeinden_zh, by = "gemeinde") %>%
  mutate(
    bfsnr = coalesce(na_if(bfsnr.x, 0), na_if(bfsnr.y, 0))
  )  %>% # 29 NA
  left_join(
    lines_with_name %>% select(wasserrechts_nr, bfsnr, gemeinde),
    by = c("WRNr" = "wasserrechts_nr")  # Join-Bedingung
  ) %>%
  # Füllen von NA-Werten in "bfsnr" und "gemeinde" mit den entsprechenden Werten aus dem Join
  mutate(
    bfsnr = coalesce(na_if(bfsnr.x, 0), na_if(bfsnr.y, 0)),
    gemeinde = coalesce(gemeinde.x, gemeinde.y)
  ) %>%
  # Entfernen der überflüssigen Spalten
  select(-bfsnr.x, -bfsnr.y, -gemeinde.x, -gemeinde.y, -geometry) %>% # 3 NA
  left_join(
    join_nearest %>% select(wasserrechts_nr, nearest_bfsnr, nearest_gemeinde),
    by = c("WRNr" = "wasserrechts_nr")  # Join-Bedingung
  ) %>%
  # Füllen von NA-Werten in "bfsnr" und "gemeinde" mit den entsprechenden Werten aus dem Join
  mutate(
    bfsnr = coalesce(na_if(bfsnr, 0), na_if(nearest_bfsnr, 0)),
    gemeinde = coalesce(gemeinde, nearest_gemeinde)
  ) %>%
  # Entfernen der überflüssigen Spalten
  select(-nearest_bfsnr, -nearest_gemeinde) %>% # 3 NA
  left_join(
    gewaesser_wp_olddata %>% select(anlage, bfsnr, ort),  # Nur die benötigten Spalten aus datensatz2
    by = c("WRNr" = "anlage")  # Join-Bedingung
  ) %>%
  mutate(
    bfsnr = coalesce(na_if(bfsnr.x, 0), na_if(bfsnr.y, 0)),
    ort = coalesce(gemeinde, ort)  # NA-Werte aus datensatz1 mit Werten aus datensatz2 füllen
  ) %>%
  select(-bfsnr.x, -bfsnr.y) %>%
  distinct() %>%
  select(bfsnr, ort, WRNr, GebKEkWfix, GebWEkWfix, jahr, Betrieb) %>%
  rename(anlage=WRNr, Wärme=GebKEkWfix, Kälte=GebWEkWfix, betrieb=Betrieb) %>%
  mutate(across(c(Wärme, Kälte), as.numeric)) %>%
  mutate(
    bfsnr = if_else(anlage == "b0224", 261, bfsnr),
    ort = if_else(anlage == "b0224", "Zürich", ort),
  ) %>%
  select(bfsnr, ort, anlage, Wärme, Kälte, jahr, betrieb) %>%
  pivot_longer(
    cols = c(Wärme, Kälte),
    names_to = "thema",
    values_to = "wert"
  ) %>%
  mutate(
    rubrik = "Oberflächengewässer",
    einheit = "kW"
  ) %>%
  select(jahr, anlage, ort, bfsnr, rubrik, thema, wert, einheit, betrieb) %>%
  arrange(jahr)


if (any(is.na(gewaesser_wp_jahresstand_final$bfsnr))) {
  tcltk::tkmessageBox(
    title = "Warnung",
    message = "Die Spalte bfsnr des Datensatzes <gewaesser_wp_jahresstand_final> enthält noch NA-Werte!",
    icon = "warning",
    type = "ok"
  )
}


# Max Jahre berechnen
max_olddata <- max(gewaesser_wp_olddata$jahr, na.rm = TRUE)
max_newdata <- max(gewaesser_wp_jahresstand_final$jahr, na.rm = TRUE)

if (max_newdata > max_olddata) {
  gewaesser_wp_final_anlage <- rbind(gewaesser_wp_jahresstand_final, gewaesser_wp_olddata)
} else {
  gewaesser_wp_final_anlage <- gewaesser_wp_olddata}

# Anlagen-Datensatz als csv speichern (für zukünftige Verwendung olddata)
write_excel_csv2(gewaesser_wp_final_anlage, here::here("data/output/oberfl_gewaesser_anlagen.csv"))



## Finalisieren

gewaesser_wp_final <- gewaesser_wp_final_anlage %>%
  filter(betrieb == "Ja") %>%
  group_by(jahr, rubrik, thema, einheit, bfsnr, ort)  %>%
  summarise(wert_kw = sum(wert, na.rm = TRUE), anzahl = n(), .groups = 'drop') %>%
  mutate(wert_mwh = wert_kw*kennwerte$betriebszeit_wp/1000) %>%
  pivot_longer(
    cols = c(wert_kw, anzahl, wert_mwh),
    names_to = "variable",
    values_to = "wert"
  ) %>%
  mutate(
    einheit = case_when(
      variable == "wert_kw"   ~ "kW",
      variable == "wert_mwh"  ~ "MWh",
      variable == "anzahl"    ~ "Anzahl",
      TRUE ~ einheit
    )
  ) %>%
  select(jahr, bfsnr, ort, rubrik, thema, wert, einheit)




write_excel_csv(gewaesser_wp_final, here::here("data/output/oberfl_gewaesser.csv"), delim = ";")
