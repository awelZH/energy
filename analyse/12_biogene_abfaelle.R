#### Biogene Abfälle ####

# Für die Energiestatistik werden auch Angaben zu biogenen Abfällen ausgewertet. 
# Die Daten liegen als Excel-Daten vor und können mit R eingelesen, umgeformt und ausgewertet werden.


rm(list = ls())

#### ACHTUNG noch Jahresstand minus 2 ####
#source("C:/Users/Public/Git Repos/energy/analyse/gemeinden.R")
#gemeinden_zh <- st_drop_geometry(gemeinden_zh)

Jahr_heute <- as.integer(format(Sys.Date(), "%Y"))

bioabfaelle_olddata <- read.csv(here::here("data/output/biogene_abfaelle_28_08_25.csv"), sep=";")

bioabfaelle_neu <- read_excel(
  {
    files <- list.files("K:/BD-AWEL-050-EN/Elektrizitätswirtschaft/Administration/Praktikum/Levi_Fuchs/Projekte/Energiestatistik_neu/R_Project/Biogene_Abfaelle/", pattern = "^biogene_abfaelle_input_\\d{4}\\.xlsx$", full.names = TRUE)
    files[which.max(as.integer(str_extract(files, "\\d{4}")))]
  }
) %>%
  mutate(jahr = max(as.integer(str_extract(list.files("K:/BD-AWEL-050-EN/Elektrizitätswirtschaft/Administration/Praktikum/Levi_Fuchs/Projekte/Energiestatistik_neu/R_Project/Biogene_Abfaelle/", pattern = "^biogene_abfaelle_input_\\d{4}\\.xlsx$"), "\\d{4}"))))

bioabfaelle_jahresstand <- bioabfaelle_neu %>%
  setNames({
    spalten <- names(bioabfaelle_neu)
    spalten[spalten != "jahr"] <- as.character(bioabfaelle_neu[1, spalten != "jahr"])
    spalten
  }) %>%  # Spaltennamen setzen
  slice(-1) %>%  # erste Zeile entfernen
  select(anlage = "Standort", "jahr", wert = matches("(?i)mwh")) %>%
  filter(!if_all(everything(), is.na), anlage != "Total") %>%
  left_join(bioabfaelle_olddata %>% select(anlage, bfsnr, ort) %>% distinct(),
            by = "anlage") %>%
  mutate(
    rubrik = "Wärme",
    thema = "Biogene Abfälle",
    einheit = "MWh"
  ) %>%
  select(jahr, ort, bfsnr, anlage, rubrik, thema, wert, einheit)

# Max Jahre berechnen
max_olddata <- max(bioabfaelle_olddata$jahr, na.rm = TRUE)
max_newdata <- max(bioabfaelle_jahresstand$jahr, na.rm = TRUE)

if (max_newdata > max_olddata) {
  bioabfaelle_final <- rbind(bioabfaelle_olddata, bioabfaelle_jahresstand)
} else {
  bioabfaelle_final <- bioabfaelle_olddata}


# Datensatz als csv speichern (für zukünftige Verwendung olddata)
write_excel_csv(bioabfaelle_final, here::here("data/output/bioabfaelle.csv"), delim = ";")

