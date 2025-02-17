### Automate Energiestatistik: csv

# save code here: C:\Users\Public\Git Repos\energy\analyse
# save code as 03_data_gest

# clear workspace without unloading packages
rm(list = ls())

# install packages: we need dplyr and readr
install.packages("tidyverse")

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

# create subset for: 1990+, Erdölprodukte, Total
subset1 <- data %>%
  filter(Jahr >= 1990 & Rubrik == "Endverbrauch - Total" & Energietraeger == "Erdölprodukte")

# create subset for: 1990+, Erdölprodukte, Erdgas
subset2 <- data %>%
  filter(Jahr >= 1990 & Rubrik == "Endverbrauch - Total" & Energietraeger == "Gas")

# create subset for: 1990+, Erdölprodukte, Koks und Kohle
subset3 <- data %>%
  filter(Jahr >= 1990 & Rubrik == "Endverbrauch - Total" & Energietraeger == "Kohle")

#########

# create subset for: 1990+, Verkehr, Erdölprodukte
subset4 <- data %>%
  filter(Jahr >= 1990 & Rubrik == "Endverbrauch - Verkehr" & Energietraeger == "Erdölprodukte")

# create subset for: 1990+, Verkehr, Elektrizität
subset5 <- data[data$Jahr>=1990& #select years
                  data$Rubrik=="Endverbrauch - Verkehr"& #from transport
                  data$Energietraeger=="Elektrizität", 
]

# create subset for: 1990+, Verkehr, Gas
subset6 <- data %>%
  filter(Jahr >= 1990 & Rubrik == "Endverbrauch - Verkehr" & Energietraeger == "Gas")

# create subset for: 1990+, Verkehr, Übrige Erneuerbare
subset7 <- data %>%
  filter(Jahr >= 1990 & Rubrik == "Endverbrauch - Verkehr" & Energietraeger == "Uebrige erneuerbare Energien")

#########

# filter data for Rubrik = "Endverbrauch - Verkehr" and Energietraeger = "Erdölprodukte"
data_x_y <- data %>%
  filter(Jahr >= 1990 & Rubrik == "Endverbrauch - Verkehr" & Energietraeger == "Erdölprodukte") %>%
  select(Jahr, TJ_x_y = TJ)  # renaming TJ to avoid conflicts

# filter data for Rubrik = "Endverbrauch - Total" and Energietraeger = "Erdölprodukte"
data_a_y <- data %>%
  filter(Jahr >= 1990 & Rubrik == "Endverbrauch - Total" & Energietraeger == "Erdölprodukte") %>%
  select(Jahr, TJ_a_y = TJ)  # renaming TJ to avoid conflicts

# join the two data frames by Jahr
fuel_data <- left_join(data_x_y, data_a_y, by = "Jahr")

# calculate the difference (=Heizöl) and prepare final dataset
combined_data <- fuel_data %>%
  mutate(
    difference = NA,  # empty column, needed for formatting
    Rubrik = "Endverbrauch - Total",  # set all Rubrik values to "Endverbrauch - Total"
    Energietraeger = "Heizöl",  # set all Energietraeger values to "Heizöl"
    TJ = TJ_a_y - TJ_x_y  # calculate the difference for TJ
  ) %>%
  select(Jahr, TJ_x_y, TJ_a_y, difference, Rubrik, Energietraeger, TJ)  # select and reorder columns

print(combined_data)

combined_data2 <- bind_rows(combined_data, subset2, subset3, subset4, subset5, subset6, subset7)

combined_gest <- combined_data2 %>%
  mutate(
    Einheit = "TJ",  # add unit
    Einheit_lang = "Terra Joule", # add unit long
    Ort = "Kanton ZH" # add canton
  ) %>%
  select(Jahr, Rubrik, Energietraeger, TJ, Einheit, Einheit_lang, Ort)  # select and reorder columns

print(combined_gest)

# # save file as csv - my test
# write.csv(combined_gest, "combined_gest.csv", row.names = TRUE)
# head(combined_gest)

# save clean dataset to correct folder, here without row names
write.csv(combined_gest,"C:/Users/Public/Git Repos/energy/data/output/combined_gest.csv", row.names = FALSE)

