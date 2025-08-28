#### Sonne ####

# Für die Energiestatistik werden auch Angaben zu Solaranlagen ausgewertet
# Die Daten für PV-Anlagen liegen OGD als csv vor und können daher direkt mit R eingelesen, umgeformt und ausgewertet werden. Die Daten für thermische Anlagen sind in der Teilenergiestatistik erneuerbare Energien und können daher direkt mit R eingelesen, umgeformt und ausgewertet werden.

#rm(list = ls())


# Scripts und libraries einlesen
source(here::here("analyse/gemeinden.R"))
source(here::here("analyse/functions.R"))
library(tcltk)



#### INPUT UND AUFBEREITUNG DATEN ####

## Some variables
# URL of the WFS service
wfs_all <- "https://maps.zh.ch/wfs/OGDZHWFS"
# Projection: EPSG:2056 (CH1903+ / LV95)
crs <- 2056
# Parsing the URL
url <- httr2::url_parse(wfs_all)


# Der Download-Link für die ZIP-Datei (Elektrizitätsproduktionsanlagen)
url <- "https://data.geo.admin.ch/ch.bfe.elektrizitaetsproduktionsanlagen/csv/2056/ch.bfe.elektrizitaetsproduktionsanlagen.zip"

download_dir <- file.path(Sys.getenv("USERPROFILE"), "Downloads")
zip_path <- file.path(download_dir, "ch.bfe.elektrizitaetsproduktionsanlagen.zip")
extract_dir <- file.path(download_dir, "elektrizitaetsanlagen_entpackt")

# Falls nötig, Zielordner anlegen
dir.create(extract_dir, showWarnings = FALSE)

# Datei herunterladen
download.file(url, destfile = zip_path, mode = "wb")

# ZIP entpacken
unzip(zip_path, exdir = extract_dir)

# Daten einlesen
electricity_plant_orig <- read.csv(file.path(extract_dir, "ElectricityProductionPlant.csv"), stringsAsFactors = FALSE)
plant_detail <- read.csv(file.path(extract_dir, "PlantDetail.csv"), stringsAsFactors = FALSE)

input_electricity_plant <- electricity_plant_orig %>%
  filter(Canton == "ZH", SubCategory == "subcat_2") %>%
  filter(Address != "Walter-von-Moos-Promenade 1-4") %>%
  mutate(PostCode = if_else(Address == "Grüningerstrasse 113", 8626, PostCode))

# räumliche Daten aus X/Y-Koordinaten erstellen. Einige Koordinaten fehlen. Deshalb nur mit den Daten, bei denen Koordinaten vorhanden sind. Andere Koordinaten werden später hinzugefügt anhand Bundesdatensatz (ortschaftenverzeichnis). Allerdings ist ein Join schwierig, da viele Daten falsch eingetragen wurden. Ausserdem gibt es sowohl mehrere Gemeinden für dieselbe PLZ als auch mehrere PLZ für dieselbe Gemeinde)

# Prioritätenreihenfolge
# 1. Koordinaten sind bei den Solaranlagen angegeben
# 2. PLZ und Gemeindename entsprechen beide dem Bundesdatensatz
# 3. Nur PLZ entspricht dem Bundesdatensatz
# 4. Nur Gemeindename entspricht dem Bundesdatensatz


# Der Download-Link für die ZIP-Datei (Ortschaftenverzeichnis PLZ)
url2 <- "https://data.geo.admin.ch/ch.swisstopo-vd.ortschaftenverzeichnis_plz/ortschaftenverzeichnis_plz/ortschaftenverzeichnis_plz_2056.csv.zip"

# Den ZIP-Download speichern
zip_path2 <- file.path(download_dir, "ortschaftenverzeichnis_plz_2056.csv.zip")
download.file(url2, destfile = zip_path2, mode = "wb")

# Entpacken des ZIP-Archivs
unzip("ortschaftenverzeichnis_plz_2056.csv.zip", exdir = download_dir)

# Einlesen der CSV-Dateien
plz <- read.csv(file.path(download_dir, "AMTOVZ_CSV_LV95/AMTOVZ_CSV_LV95.csv"), stringsAsFactors = FALSE, sep = ";") %>% 
  filter(Kantonskürzel == "ZH") %>%
  mutate(Gemeindename = str_remove_all(str_trim(.data[["Gemeindename"]]), "\\s*\\(ZH\\)"))

# Führe den join durch
df_merged <- input_electricity_plant %>%
  mutate(Municipality = ifelse(Municipality %in% c("Zürich-Flughafen", "Zürich Flughafen"), "Kloten", Municipality)) %>%
  left_join(plz %>% distinct(PLZ, Gemeindename, .keep_all = TRUE) %>% select(PLZ, Gemeindename, E, N), 
            by = c("PostCode" = "PLZ", "Municipality" = "Gemeindename")) %>%
  mutate(X_x = ifelse(is.na(X_x), E, X_x)) %>%
  mutate(X_y = ifelse(is.na(X_y), N, X_y)) %>%
  select(-c("E", "N")) %>%
  left_join(plz %>% distinct(PLZ, .keep_all = TRUE) %>% select(PLZ, E, N), by = c("PostCode" = "PLZ")) %>%
  mutate(X_x = ifelse(is.na(X_x), E, X_x)) %>%
  mutate(X_y = ifelse(is.na(X_y), N, X_y)) %>%
  select(-c("E", "N")) %>%
  left_join(plz %>% distinct(Gemeindename, .keep_all = TRUE) %>% select(Gemeindename, E, N), by = c("Municipality" = "Gemeindename")) %>%
  mutate(X_x = ifelse(is.na(X_x), E, X_x)) %>%
  mutate(X_y = ifelse(is.na(X_y), N, X_y)) %>%
  select(-c("E", "N")) %>%
  left_join(plant_detail[c("Date", "Power", "ElectricityProductionPlantR")], by = c("xtf_id" = "ElectricityProductionPlantR"))


# Hinweis NA-Zeilen
if (nrow(df_merged %>% filter(is.na(X_x))) > 0) {
  
  # NA-Zeilen mit Kontextinformationen holen
  na_zeilen <- df_merged %>%
    mutate(Zeilennummer = row_number()) %>%
    filter(is.na(X_x)) %>%
    select(Zeilennummer, Address, PostCode, Municipality, Canton)
  
  # Zeilen als Text erfassen
  zeilen_text <- capture.output(print(na_zeilen))
  
  # Meldung zusammensetzen
  nachricht <- paste(
    "Es gibt NA-Werte in den Koordinaten der Solaranlagen (Datensatz df_merged).\n",
    "Dies ist kein grosses Problem. Folgend die nicht gejointen Daten:\n\n",
    paste(zeilen_text, collapse = "\n")
  )
  
  # Eigenes Fenster erstellen
  tt <- tktoplevel()
  tkwm.title(tt, "NA-Hinweis")
  
  # Text-Widget + Scrollbar
  txt <- tktext(tt, width = 100, height = 30, wrap = "none")  # größer
  scr <- tkscrollbar(tt, command = function(...) tkyview(txt, ...))
  tkconfigure(txt, yscrollcommand = function(...) tkset(scr, ...))
  
  # Einfügen der Nachricht
  tkinsert(txt, "end", nachricht)
  tkconfigure(txt, state = "disabled")  # Nur lesen
  
  # Layout
  tkgrid(txt, scr)
  tkgrid.configure(scr, sticky = "ns")
  
  tkfocus(tt)
  tkraise(tt)
}



# GIS-Datensatz erstellen basierend auf Koordinaten (NA-Werte werden nicht beachtet)
electricity_plant_gis <- st_as_sf(df_merged, coords = c("X_x", "X_y"), crs = crs, na.fail = F)  # LV95 (CH1903+)



# Spatial join mit Gemeinden und PV-Datensatz erstellen
pv <- st_join(electricity_plant_gis, gemeinden_zh_gis) %>% st_drop_geometry() %>%
  filter(!is.na(bfsnr)) %>%
  mutate(Datum_Beginn = as.Date(BeginningOfOperation, format = "%Y-%m-%d"), Datum_Aktualisierung = as.Date(Date, format = "%Y-%m-%d")) %>%
  select(c("xtf_id","bfsnr", "gemeinde", "InitialPower", "TotalPower", "Power", "Datum_Beginn", "Datum_Aktualisierung")) %>%
  mutate(Datum_coalesce = coalesce(Datum_Aktualisierung, Datum_Beginn))


## Hinweis: Einige wenige fallen weg, da sie knapp neben den Kantonsgrenzen liegen (Umgang mit diesen noch abzuklären)


#### DATENVERARBEITUNG ####

# Define the time range
start_year <- min(as.integer(format(pv$Datum_Beginn, "%Y")), na.rm = T)
current_year <- max(as.integer(format(pv$Datum_Beginn, "%Y")), na.rm = T)


# Create a sequence of years from 1983 to the current year
years <- tibble(year_verfuegung = seq(start_year, current_year))

# Extract unique combinations of bfsnr and name values
bfsnr_name_values <- pv %>% select(bfsnr, gemeinde) %>% distinct()

# Create a complete time series for each combination of bfsnr and name
complete_time_series <- bfsnr_name_values %>%
  crossing(year_verfuegung = seq(start_year, current_year))


# wenn z.B. auf einem Dach nach einigen Jahren mehr Leistung installiert wird, wird dies in den Attributen "Power" und "Date" abgebildet. So muss im Zeitstrahl jeweils bis zum Zeitpunkt der Änderung (zukünftig "Aktualisierung") die ursprüngliche Leistung gewählt werden. Ab der Aktualisierung wird dann die neue Leistung gewählt.

pv_aktualisierung <- tibble(pv) %>% 
  mutate(Jahr_Aktualisierung = year(Datum_Aktualisierung)) %>%
  group_by(bfsnr, gemeinde, Jahr_Aktualisierung) %>% 
  summarise(
    # Wähle die richtige Power (InitialPower oder TotalPower) basierend auf dem Erweiterungsdatum
    strom_aktualisierung_kw = sum(Power),
    .groups = 'drop'
  )


solar_pv_final <- tibble(pv) %>% 
  mutate(year_verfuegung = year(Datum_Beginn)) %>%
  group_by(bfsnr, gemeinde, year_verfuegung) %>% 
  summarise(
    # Wähle die richtige Power (InitialPower oder TotalPower) basierend auf dem Erweiterungsdatum
    strom_beginn_kw = sum(TotalPower),
    anzahl = n(),
    .groups = 'drop'
  ) %>%
  left_join(pv_aktualisierung %>% select(!gemeinde), by = c("year_verfuegung" = "Jahr_Aktualisierung", "bfsnr")) %>%
  mutate(
    pv_strom_kw = rowSums(across(c(strom_beginn_kw, strom_aktualisierung_kw)), na.rm = TRUE),
    pv_strom_mwh = pv_strom_kw) %>%
  right_join(complete_time_series, by = c("bfsnr", "gemeinde", "year_verfuegung")) %>%
  arrange(bfsnr, gemeinde, year_verfuegung) %>%
  group_by(bfsnr, gemeinde) %>%
  mutate(
    pv_strom_kw = coalesce(pv_strom_kw, 0),
    pv_strom_mwh = coalesce(pv_strom_mwh, 0),
    anzahl = coalesce(anzahl, 0),
    cum_pv_strom_kw = cumsum(pv_strom_kw),
    cum_pv_strom_mwh = cumsum(pv_strom_mwh),
    cum_anzahl = cumsum(anzahl),
    jahr = year_verfuegung
  )  %>%
  ungroup() %>%
  pivot_longer(
    cols = c(cum_pv_strom_kw, cum_pv_strom_mwh, cum_anzahl),
    names_to = "variable",
    values_to = "wert"
  ) %>%
  mutate(
    einheit = case_when(
      variable == "cum_pv_strom_kw"   ~ "kW",
      variable == "cum_pv_strom_mwh"   ~ "MWh",
      variable == "cum_anzahl"    ~ "Anzahl"
    ),
    ort = gemeinde,
    rubrik = "Strom",
    thema = "PV",
  ) %>%
  select(jahr, ort, bfsnr, rubrik, thema, wert, einheit)


#Kantonsdaten 
#pv_final_zh <- pv_final_gem %>%
#  group_by(year_verfuegung) %>%  # Gruppiere nach Jahr
#  summarise(
#    total_pv_strom_mwh = sum(cum_pv_strom_mwh, na.rm = TRUE)  # Summiere die Werte #pro Jahr
#  ) %>%
#  arrange(year_verfuegung)  # Sortiere die Jahre aufsteigend


write_excel_csv(solar_pv_final, here::here("data/output/solar_pv.csv"), delim = ";")






