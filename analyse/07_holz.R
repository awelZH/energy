#### Holz ####

# Für die Energiestatistik werden auch Angaben zu Energieholz ausgewertet
# Die Daten werden aus der Holzenergiestatistik des Bundes bezogen und können mit R eingelesen, umgeformt und ausgewertet werden. Die Unübersichtlichkeit des Excels und die vielen Sheets erschweren eine automatisierte Verarbeitung. Im Jahr 2024 wurde die Statistik als xlsm herausgegeben. Code müsste angepasst werden, wenn wieder xlsx.


rm(list = ls())


# Scripts und libraries einlesen
library(openxlsx)
library(httr) # generic webservice package
library(tidyverse)
library(readr)

holz_olddata <- read.csv(here::here("data/output/holz.csv"), sep=";")

kennwerte <- read.csv(here::here("data/input/kennwerte.csv"), encoding="UTF-8", sep=";", header = T)

letztes_jahr <- as.integer(format(Sys.Date(), "%Y")) - 1


#### INPUT DATEN ####

# Datei, in der der zuletzt verwendete Link gespeichert wird
link_file <- file.path(here::here("data/input"), "last_download_link.txt")

# Link laden, der das letzte mal gespeichert wurde
default_link <- readLines(link_file, warn = FALSE)

# Popup mit vorbelegtem Link
user_link <- rstudioapi::showPrompt(
  title = "Download-Link festlegen",
  message = paste("Hier muss der aktuelle Download-Link der Schweizerischen Statistik der erneuerbaren Energien eingetragen werden. Aktueller Link (vermutlich aus dem Jahr", letztes_jahr,"):", default_link, 
                  "\nFalls es einen aktuelleren Link gibt, hier eingeben:"),
  default = default_link
)

# Falls Abbrechen oder leer → Standard verwenden
if (is.null(user_link) || user_link == "") {
  download_link <- default_link
} else {
  download_link <- user_link
}

# Link in Datei speichern → beim nächsten Start wieder verfügbar
writeLines(download_link, link_file)

# Download durchführen
temp_file <- tempfile(fileext = ".xlsm")
GET(download_link, write_disk(temp_file, overwrite = TRUE))

sheets <- getSheetNames(temp_file)

# Prüfen, welches Sheet existiert und dann einlesen (die Daten waren über die letzten Jahre nicht immer gleich)
if ("7.1" %in% sheets) {
  holz_input <- read.xlsx(temp_file, sheet = "7.1", colNames=FALSE)
} else if ("P" %in% sheets) {
  holz_input <- read.xlsx(temp_file, sheet = "P")
} else {
  stop("Keines der gewünschten Sheets ('7.1' oder 'P') gefunden!")
}


#### AUFBEREITUNG DATEN ####

#Jahreszahl des Datensatzes finden
jahr_datensatz <- holz_input %>% 
  mutate_all(as.character) %>%  
  pivot_longer(everything(), values_to = "text") %>%  
  mutate(jahr = str_extract(text, "\\b(20\\d{2}|19\\d{2})\\b")) %>%  # Findet Jahreszahlen ab 1900
  filter(!is.na(jahr)) %>%  # Nur Zeilen mit einer gefundenen Jahreszahl behalten
  slice(1) %>%  # Nimmt die erste gefundene Jahreszahl
  select(jahr) %>%
  mutate(jahr = as.integer(jahr)) %>%
  pull(jahr)


# 1. Zeile finden, in der "Summe" steht
summe_row <- which(apply(holz_input, 1, function(row) any(row == "Summe")))[1]  # Erste Zeile mit "Summe"

# 2. Falls gefunden, alle Zeilen darüber entfernen
if (!is.na(summe_row)) {
  holz_prov1 <- holz_input[summe_row:nrow(holz_input), ]  # Alles oberhalb der Zeile mit "Summe" abschneiden
  colnames(holz_prov1) <- holz_prov1[1, ]  # Erste Zeile als Spaltennamen setzen
  holz_prov1 <- holz_prov1[-1, ]  # Die Spaltennamen-Zeile selbst entfernen
} else {
  stop("'Summe' wurde nicht gefunden!")
}

# 3. Spaltenindex von "Summe" finden
summe_index <- which(names(holz_prov1) == "Summe")

# 4. Falls "Summe" existiert, die nächsten zwei Spalten auswählen
if (length(summe_index) > 0 && summe_index < ncol(holz_prov1)) {
  selected_cols <- c(1, summe_index, summe_index + 1)  # Erste Spalte + "Summe" + rechts davon
  
  # 4. Relevante Spalten auswählen
  holz_prov2 <- holz_prov1[, selected_cols]
  
  # 5. Spalte rechts neben "Summe" in "Leistung" umbenennen
  colnames(holz_prov2)[3] <- "holz_kw_zh"
} else {
  stop("Spalte 'Summe' wurde nicht gefunden oder es gibt keine Spalte daneben!")
}

holz_jahresstand_kw <- holz_prov2 %>%
  filter(.[[1]] == "Zürich") %>%
  mutate(jahr = jahr_datensatz, rubrik = "Wärme", thema = "Holz", wert = as.numeric(holz_kw_zh), einheit = "kW", ort = "Kanton ZH") %>%
  select(-c(Summe, holz_kw_zh, Kantone))

holz_jahresstand_mwh <- holz_jahresstand_kw %>%
  mutate(wert = wert*kennwerte$betriebszeit_holz/1000, einheit = "MWh")

holz_jahresstand <- rbind(holz_jahresstand_kw, holz_jahresstand_mwh)

holz_final <- holz_olddata %>% 
  rows_update(holz_jahresstand, by = c("jahr", "einheit"), unmatched = "ignore") %>%  # Erst vorhandene Werte aktualisieren
  bind_rows(anti_join(holz_jahresstand, holz_olddata, by = c("jahr", "einheit"))) %>% # Dann fehlende Jahre hinzufügen
  arrange(einheit, jahr)


write_excel_csv2(holz_final, here::here("data/output/holz.csv"))
