#### Strom ####

# Für die Energiestatistik werden auch Angaben zum Stromverbrauch ausgewertet
# Dabei werden Elcom-Daten und Bahnstrom-Daten berücksichtigt

rm(list = ls())

# Scripts und libraries einlesen
source(here::here("analyse/bevoelkerung.R"))
library(readxl)
library(janitor)


#### INPUT UND AUFBEREITUNG DATEN ####

strom_olddata <- read.csv(here::here("data/output/stromverbrauch_28_08_25.csv"), sep=";")


## Gebäude und Anlagen (Elcom Daten - Excel)

strom_elcom_input <- read_xlsx("K:/BD-AWEL-050-EN/Elektrizitätswirtschaft/Administration/Praktikum/Levi_Fuchs/Projekte/Energiestatistik_neu/R_Project/Strom/Energie Ausspeisung Endverbraucher Kanton ZH 2014 bis 2023.xlsx")

cutoff_index_start <- match("Jahr", strom_elcom_input[[names(strom_elcom_input)[1]]])
cutoff_index_end <- which(grepl("MWh", strom_elcom_input[[1]]))

# Alles ab Reihe "Summen" entfernen und weiteres entfernen
strom_elcom <- strom_elcom_input %>%
  filter(row_number() >= cutoff_index_start,
         row_number() <= cutoff_index_end) %>%
  row_to_names(row_number = 1) %>%
  rename(thema = Jahr) %>%                  # umbenennen
  mutate(across(-thema, as.character)) %>%
  pivot_longer(
    cols = -thema,
    names_to = "jahr",
    values_to = "wert"
  ) %>%
  mutate(
    jahr = as.integer(jahr),
    ort = "Kanton ZH",
    rubrik = "Stromverbrauch",
    thema = "Bauten und Anlagen",
    wert = as.numeric(wert),
    einheit = "MWh"
  ) %>%
  select(jahr, rubrik, thema, wert, einheit, ort)


strom_elcom_merged <- strom_olddata %>%
  # Full Join, um alle Jahre/Themen aus beiden zu behalten
  full_join(strom_elcom, 
            by = c("jahr", "rubrik", "thema", "einheit", "ort"),
            suffix = c("_old", "_new")) %>%
  mutate(
    wert = coalesce(wert_new, wert_old)  # wenn neuer Wert vorhanden, nimm den, sonst den alten
  ) %>%
  select(jahr, rubrik, thema, wert, einheit, ort)



## Bahnstrom (SBB - online ablesen und manuell eintragen)

if (interactive() && requireNamespace("rstudioapi", quietly = TRUE)) {
  
  aktuelles_jahr <- as.integer(format(Sys.Date(), "%Y"))
  ziel_jahr <- aktuelles_jahr - 1
  
  # Prüfen, ob das Zieljahr für Thema "Bahn" schon in den Daten vorhanden ist und Wert nicht NA
  jahr_vorhanden_bahn <- ziel_jahr %in% strom_olddata$jahr[strom_olddata$thema == "Bahn" & !is.na(strom_olddata$wert)]
  
  if (!jahr_vorhanden_bahn) {
    
    eingabe <- rstudioapi::showPrompt(
      title = paste("Bahn-Daten für", ziel_jahr, "eingeben"),
      message = paste(
        "Bitte die Daten für das neueste Jahr (Jahr, Energie in GWh) kommagetrennt eingeben:",
        "z.B. 2024,231.4",
        "Quelle: reporting.sbb.ch/infrastrukturen",
        "Falls keine Eingabe gewünscht: einfach so belassen und OK drücken.",
        sep = "\n"
      ),
      default = paste(ziel_jahr, ",", sep = "")
    )
    
    if (!is.null(eingabe) && eingabe != "") {
      teile <- strsplit(eingabe, ",")[[1]]
      jahr <- as.integer(trimws(teile[1]))
      wert_gwh <- as.numeric(trimws(teile[2]))
      
      # Wert in MWh (aus GWh) berechnen
      wert_mwh_gesamt <- wert_gwh * 1000
      
      # Verhältnis aus bevoelkerung_final holen
      verhältnis <- bevoelkerung_final %>%
        filter(jahr == jahr) %>%
        pull(verhaeltnis_ch_zh)
      
      # Sicherstellen, dass genau ein Wert da ist und kein NA
      if (length(verhältnis) != 1 || is.na(verhältnis)) {
        warning(paste("Kein Verhältnis CH/ZH für Jahr", jahr, "gefunden (vermutlich sind die CH Bevölkerungsdaten noch nicht vorhanden) – Daten werden nicht aktualisiert."))
        stromverbrauch_final <- strom_olddata
      } else {
        # Wert für Kanton ZH berechnen
        wert_mwh_zh <- wert_mwh_gesamt / verhältnis
        
        neue_zeile <- tibble(
          jahr = jahr,
          rubrik = "Stromverbrauch",
          thema = "Bahn",
          wert = wert_mwh_zh,
          einheit = "MWh",
          ort = "Kanton ZH"
        )
        
        # Alte Bahn-Daten für das Zieljahr entfernen (auch wenn NA drinsteht)
        strom_alte_bahn <- strom_olddata %>%
          filter(!(jahr == ziel_jahr & thema == "Bahn"))
        
        # Neue Daten anhängen und sortieren
        bahnstrom_neu <- bind_rows(strom_alte_bahn, neue_zeile) %>%
          arrange(jahr, thema)
        
        stromverbrauch_final <- bahnstrom_neu
      }
      
    } else {
      message("Keine Eingabe – fahre ohne neue Bahn-Daten fort.")
      stromverbrauch_final <- strom_olddata
    }
    
  } else {
    message("Bahn-Daten für ", ziel_jahr, " sind bereits vorhanden – kein Popup nötig.")
    stromverbrauch_final <- strom_olddata
  }
  
}


write_excel_csv(stromverbrauch_final, here::here("data/output/stromverbrauch.csv"), delim = ";")



