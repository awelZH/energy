#### Grüngut ####

# Für die Energiestatistik werden auch Angaben zu Grüngut ausgewertet
# Die Daten liegen als Excel-Daten vor und können mit R eingelesen, umgeformt und ausgewertet werden.


rm(list = ls())

# Scripts und libraries einlesen
library(tidyverse) # a suite of packages for data wrangling, transformation, plotting, ...
library(readxl)


#### INPUT DATEN ####

current_year <- as.integer(format(Sys.Date(), "%Y"))

gruengut_orig <- read_excel("K:/BD-AWEL-050-EN/Elektrizitätswirtschaft/Administration/Praktikum/Levi_Fuchs/Projekte/Energiestatistik_neu/R_Project/Grüngut/Energiezahlen 20 Jahre.xlsx", col_names = F)

gruengut_input <- gruengut_orig %>%
  {
    # Suche nach einer Zeile, in der irgendwo "gwh" vorkommt (case-insensitive)
    start <- which(apply(., 1, function(row) any(grepl("gwh", row, ignore.case = TRUE))))[1]
    . <- slice(., start:n())
    colnames(.) <- .[1, ]
    slice(., -1)
  }



#### DATENAUFBEREITUNG ####

gruengut_final <- gruengut_input %>%
  rename(kategorie = `in GWh`) %>%  # Spalte umbenennen für Klarheit
  pivot_longer(
    cols = -kategorie,              # Alle Spalten außer "kategorie" ins Long-Format bringen
    names_to = "jahr",              # Neue Spalte für Jahre
    values_to = "wert"              # Neue Spalte für die Werte
  ) %>%
  mutate(jahr = as.integer(jahr)) %>%   # Jahr als echte Zahl (optional, falls nötig)
  pivot_wider(
    names_from = kategorie,   # Jede Kategorie wird eine eigene Spalte
    values_from = wert        # Die Werte kommen in diese Spalten
  ) %>%
  mutate(
    gruengut_abwaerme_mwh_zh = `Wärme verkauft` * 1000,
    gruengut_strom_mwh_zh = `Strom netto verkauft` * 1000,
    gruengut_biogas_mwh_zh = `Biogas in Erdgasnetz` * 1000,
    gruengut_holz_mwh_zh = `thermisch genutzt Holz und Siebüberlauf` * 1000,
  ) %>%
  pivot_longer(
    cols = c(gruengut_abwaerme_mwh_zh, gruengut_strom_mwh_zh, gruengut_biogas_mwh_zh, gruengut_holz_mwh_zh),
    names_to = "variable",
    values_to = "wert"
  ) %>%
  mutate(
    einheit = case_when(
      variable == "gruengut_abwaerme_mwh_zh"   ~ "MWh",
      variable == "gruengut_strom_mwh_zh"  ~ "MWh",
      variable == "gruengut_biogas_mwh_zh"    ~ "MWh",
      variable == "gruengut_holz_mwh_zh"    ~ "MWh"
    ),
    subthema = case_when(
      variable == "gruengut_abwaerme_mwh_zh"   ~ "Abwärme",
      variable == "gruengut_strom_mwh_zh"  ~ "Strom",
      variable == "gruengut_biogas_mwh_zh"    ~ "Biogas",
      variable == "gruengut_holz_mwh_zh"    ~ "Holz"
    ),
    rubrik = case_when(
      variable == "gruengut_abwaerme_mwh_zh"   ~ "Wärme",
      variable == "gruengut_strom_mwh_zh"  ~ "Wärme",
      variable == "gruengut_biogas_mwh_zh"    ~ "Wärme",
      variable == "gruengut_holz_mwh_zh"    ~ "Wärme"
    ),
    thema = "Grüngut",
    ort = "Kanton ZH"
  ) %>%
  select(jahr, ort, rubrik, thema, subthema, wert, einheit)


write_excel_csv(gruengut_final, here::here("data/output/gruengut.csv"), delim = ";")