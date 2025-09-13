#### Wärmepumpen (Erdsonden) ####

# Für die Energiestatistik werden auch Angaben zu den Wärmepumpen ausgewertet
# Die Daten für Grundwasser-WP werden als Excel-Daten von der Abteilung Gewässerschutz (Andrea Schildknecht) an die Abteilung Energie geliefert und können mit R eingelesen, umgeformt und ausgewertet werden.

rm(list = ls())


# Scripts und libraries einlesen
library(readxl)
source(here::here("analyse/functions.R"))
source(here::here("analyse/gemeinden.R"))
kennwerte <- read.csv(here::here("data", "input", "kennwerte.csv"), sep=";")


#### INPUT UND AUFBEREITUNG DATEN ####

# Einlesen des aktuellsten Excels (gemäss Jahreszahl im Dateiname)

input_grund_wp_orig <- read_excel(list.files(path = "K:/BD-AWEL-050-EN/Planung/Grundlagen/Kantonale Energie Statistik/Sammlung der Daten/Grundwasser WP", pattern = "^Statistik Energie Ende \\d{4}\\.xlsx$", full.names = TRUE)[which.max(as.integer(stringr::str_extract(list.files(path = "K:/BD-AWEL-050-EN/Planung/Grundlagen/Kantonale Energie Statistik/Sammlung der Daten/Grundwasser WP", pattern = "^Statistik Energie Ende \\d{4}\\.xlsx$", full.names = TRUE), "\\d{4}")))])

cutoff_index <- match("Summen", input_grund_wp_orig[[names(input_grund_wp_orig)[1]]])

# Alles ab Reihe "Summen" entfernen und weiteres entfernen
input_grund_wp <- input_grund_wp_orig %>%
  filter(row_number() < cutoff_index) %>%
  rename_with(~ "waermeentnahme_kw", .cols = matches("Entnahme")) %>%
  rename_with(~ "waermeeintrag_kw", .cols = matches("Eintrag")) %>%
  rename_with(~ "anlage", .cols = matches("GWR")) %>%
  rename_with(~ "datum", .cols = matches("Datum")) %>%
  mutate(datum = as.Date(as.numeric(datum), origin = "1900-01-01")) %>%
  mutate(jahr = as.integer(format(datum, "%Y"))) %>%
  join_gemeinden(gemeinden_zh) %>% #da noch keine BFS-Nr
  select(anlage, bfsnr, gemeinde, waermeentnahme_kw, waermeeintrag_kw, datum, jahr)

# Gemeinden nicht in beiden Datensätzen gleich geschrieben: " (ZH)" kommt im Grundwasser-Datensatz nicht vor 




#### DATENVERARBEITUNG ####

# Define the time range
erstes_jahr <- min(input_grund_wp$jahr)
aktuelles_jahr <- max(input_grund_wp$jahr)

# Create a sequence of years from 1983 to the current year
years <- tibble(jahr = seq(erstes_jahr, aktuelles_jahr))

# Extract unique combinations of bfsnr and name values
bfsnr_name_values <- input_grund_wp %>% select(bfsnr, gemeinde) %>% distinct()

# Create a complete time series for each combination of bfsnr and name
complete_time_series <- bfsnr_name_values %>%
  crossing(jahr = seq(erstes_jahr, aktuelles_jahr))

grundwasser_wp_final <- tibble(input_grund_wp) %>% 
  mutate(jahr = year(datum)) %>%
  group_by(bfsnr, gemeinde, jahr) %>% 
  summarise(
    total_waermeentnahme_kw = sum(waermeentnahme_kw, na.rm = TRUE),
    total_waermeeintrag_kw = sum(waermeeintrag_kw, na.rm = TRUE),
    anzahl = n(),
    .groups = 'drop'
  ) %>%
  right_join(complete_time_series, by = c("bfsnr", "gemeinde", "jahr")) %>%
  arrange(bfsnr, gemeinde, jahr) %>%
  group_by(bfsnr, gemeinde) %>%
  mutate(
    total_waermeentnahme_kw = coalesce(total_waermeentnahme_kw, 0),
    total_waermeeintrag_kw = coalesce(total_waermeeintrag_kw, 0),
    anzahl = coalesce(anzahl, 0),
    cum_waermeentnahme_kw = cumsum(total_waermeentnahme_kw),
    cum_waermeeintrag_kw = cumsum(total_waermeeintrag_kw),
    cum_anzahl = cumsum(anzahl)
  ) %>%
  mutate(
    cum_waermeentnahme_mwh = cum_waermeentnahme_kw*kennwerte$betriebszeit_wp/1000,
    cum_waermeeintrag_mwh = cum_waermeeintrag_kw*kennwerte$betriebszeit_wp/1000
  ) %>%
  ungroup() %>%
  pivot_longer(
    cols = c(cum_waermeentnahme_kw, cum_waermeeintrag_kw, cum_waermeentnahme_mwh, cum_waermeeintrag_mwh, cum_anzahl),
    names_to = "variable",
    values_to = "wert"
  ) %>%
  mutate(
    einheit = case_when(
      variable == "cum_waermeentnahme_kw"   ~ "kW",
      variable == "cum_waermeeintrag_kw"   ~ "kW",
      variable == "cum_waermeentnahme_mwh"   ~ "MWh",
      variable == "cum_waermeeintrag_mwh"   ~ "MWh",
      variable == "cum_anzahl"    ~ "Anzahl"
    ),
    ort = gemeinde,
    rubrik = "Grundwasser WP",
    thema = case_when(
      variable == "cum_waermeentnahme_kw"   ~ "Wärme",
      variable == "cum_waermeeintrag_kw"   ~ "Kälte",
      variable == "cum_waermeentnahme_mwh"   ~ "Wärme",
      variable == "cum_waermeeintrag_mwh"   ~ "Kälte",
      variable == "cum_anzahl"    ~ "Anzahl"
    )
  ) %>%
  select(jahr, bfsnr, ort, rubrik, thema, wert, einheit)


write_excel_csv(grundwasser_wp_final, here::here("data/output/grundwasser.csv"), delim = ";")
