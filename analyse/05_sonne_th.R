#### Sonne ####

# Für die Energiestatistik werden auch Angaben zu thermischen Solaranlagen ausgewertet
# Die Daten für die Anlagen sind in der Teilenergiestatistik erneuerbare Energien und können daher direkt mit R eingelesen, umgeformt und ausgewertet werden.

rm(list = ls())


# Scripts und libraries einlesen
source(here::here("analyse/bevoelkerung.R"))
library(httr)
library(readxl)


#### INPUT UND AUFBEREITUNG DATEN ####

temp_file <- tempfile(fileext = ".xlsx")
GET("https://pubdb.bfe.admin.ch/de/publication/download/8787", write_disk(temp_file, overwrite = TRUE)) #s. https://pubdb.bfe.admin.ch/de/suche?keywords=404

input_bfe_solar_th <- read_excel(temp_file, sheet = "Anhang B", skip = 2)


#### DATENVERARBEITUNG ####

solar_th_final <- input_bfe_solar_th %>%
  pivot_longer(cols = names(input_bfe_solar_th[(which(names(input_bfe_solar_th) == "Einheit")+1):(which(names(input_bfe_solar_th) == "Herkunft")-1)]), #Jahreszahlen befinden sich zw. den Spalten "Einheit" und "Herkunft"
               names_to = "jahr",       # Die neuen Spaltennamen (Jahreszahlen) werden hier gespeichert
               values_to = "wert") %>%  # Die Werte werden in die neue Spalte "Wert" verschoben
  mutate(jahr = as.numeric(sub("X", "", jahr))) %>%  # Entferne das 'X' und konvertiere in numerische Werte
  select(Technologie, Zeileninhalt, Einheit, jahr, wert) %>%
  filter(str_starts(Technologie, "Unverglaste") | str_starts(Technologie, "Röhren")) %>%
  filter(Zeileninhalt == "Wärmeertrag") %>%
  mutate(
    Technologie = case_when(
      str_starts(Technologie, "Röhren") ~ "solar_th_verglast_gwh_ch",
      str_starts(Technologie, "Unverglaste") ~ "solar_th_unverglast_gwh_ch",
      TRUE ~ Technologie)) %>%
  pivot_wider(names_from = Technologie, values_from = wert) %>%
  select(jahr, solar_th_verglast_gwh_ch, solar_th_unverglast_gwh_ch) %>%
  left_join(bevoelkerung_final %>% select(jahr, verhaeltnis_ch_zh), by = "jahr") %>%
  mutate(
    "Kollektor verglast" = as.numeric(solar_th_verglast_gwh_ch) / verhaeltnis_ch_zh,
    "Kollektor unverglast" = as.numeric(solar_th_unverglast_gwh_ch) / verhaeltnis_ch_zh
  ) %>%
  pivot_longer(
    cols = c("Kollektor verglast", "Kollektor unverglast"),
    names_to = "subthema",
    values_to = "wert"
  ) %>%
  mutate(
    einheit = case_when(
      subthema == "solar_th_verglast_gwh_zh" ~ "Potenzial GWh",
      subthema == "solar_th_unverglast_gwh_zh" ~ "GWh"
    ),
    rubrik = "Wärme",
    thema = "Solar thermisch",
    ort = "Kanton ZH"
  ) %>%
  select(jahr, ort, rubrik, thema, subthema, wert)


write_excel_csv(solar_th_final, here::here("data/output/solar_th.csv"), delim = ";")
