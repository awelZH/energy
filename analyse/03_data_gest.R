### Automate Energiestatistik: csv

# save code here: C:\Users\Public\Git Repos\energy\analyse
# save code as 03_data_gest

# clear workspace without unloading packages
rm(list = ls())

# install packages: we need dplyr and readr
if (!requireNamespace("tidyverse", quietly = TRUE)) {
  install.packages("tidyverse")
}

# load libraries
library(readr)
library(dplyr)

# read data from https://opendata.swiss/de/dataset/energiebilanz-der-schweiz
data <- read_csv('https://www.uvek-gis.admin.ch/BFE/ogd/115/ogd115_gest_bilanz.csv')

# check names
names(data)

# check unique values
unique(data$Jahr)
unique(data$Rubrik)
unique(data$Energietraeger)

# Filtere die Daten und berechne die Heizöl-Differenz in einem Schritt
final_subset <- data |> 
  filter(Jahr >= 1990) |> 
  group_by(Jahr) |>
  summarise(
    TJ = sum(TJ[Rubrik == "Endverbrauch - Total" & Energietraeger == "Erdölprodukte"]) - 
      sum(TJ[Rubrik == "Endverbrauch - Verkehr" & Energietraeger == "Erdölprodukte"]),
    .groups = 'drop'
  ) |> 
  mutate(
    Rubrik = "Endverbrauch - Total",
    Energietraeger = "Heizöl"
  ) |> 
  bind_rows(
    data |> 
      filter(Jahr >= 1990 & 
               ((Rubrik == "Endverbrauch - Total" & Energietraeger %in% c("Gas", "Kohle")) | 
                  (Rubrik == "Endverbrauch - Verkehr" & Energietraeger %in% c("Erdölprodukte", "Elektrizität", "Gas", "Uebrige erneuerbare Energien")))
      )
  ) |> 
  select(Jahr, Rubrik, ET = Energietraeger, Wert = TJ) |> 
  mutate(Einheit = "TJ", Einheit_lang = "Terrajoule", Datenquelle = "BFE - https://opendata.swiss/de/dataset/energiebilanz-der-schweiz")

# Optional: Zeige das Ergebnis an
print(final_subset)


# save clean dataset to correct folder, here without row names
#write.csv(combined_gest,"C:/Users/Public/Git Repos/energy/data/output/combined_gest.csv", row.names = FALSE)

