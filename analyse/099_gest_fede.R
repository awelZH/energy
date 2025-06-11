library(dplyr)
library(tidyr)
library(readxl)
library(readr)

## Daten einlesen

##Schweizer Bevölkerung
##XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX wie kann man das automatisiert herunterladen?
ch_bev <- read_excel("C:/Users/BAA2757/Downloads/px-x-0102020000_104_20250606-114145.xlsx")


# Bevölkerungsdaten Zürich
url <- "https://www.web.statistik.zh.ch/ogd/daten/ressourcen/KTZH_00001141_00002139.xlsx"
destfile <- "bevoelkerung.xlsx"


if (!file.exists(destfile)) {
  download.file(url, destfile, mode = "wb") # Nur herunterladen, wenn Datei lokal noch nicht vorhanden
  message("Datei heruntergeladen.")
} else {
  message("Datei existiert bereits lokal.")
}

# Nur das Tabellenblatt "Gemeinden" einlesen
zh_bev<- read_excel(destfile, sheet = "Gemeinden", skip = 6)

zh_bev <- zh_bev %>%
  select(-ends_with("_REL"), -ends_with("_ABS"))


## Energiedaten
data <- read_csv('https://www.uvek-gis.admin.ch/BFE/ogd/115/ogd115_gest_bilanz.csv')

## Gasdaten (separat)
gasdaten <- read_excel(
  path = "C:/Users/Public/gitrepos/energy/data/input/Kantonale Energiestatistik Beispiel.xlsx",
  sheet = "Gas",       # Name oder Nummer des Sheets
  range = "I175:AC186"             # Zellbereich, den du brauchst
)




#### Datenaufbereitung ZH Bevölkerung####

## zh_bevölkerung in longformat bringen

zh_bev_long <- zh_bev %>%
  filter(GEMEINDE == "*KANTON ZÜRICH*") %>%           # nur kantonale Daten
  pivot_longer(
    cols = starts_with("TOTAL_"),
    names_to = "Jahr",
    names_prefix = "TOTAL_",
    values_to = "TOTAL"
  ) %>%
  mutate(Jahr = as.integer(Jahr))



#### Datenaufbereitung Gasdaten####

## gasdaten in longformat bringen und nur Total behalten (den Rest brauchen wir nicht)






#### Datenaufbereitung Energiestatistik####

## Funktion, die die nötigen subsets erstellt und ab 1990 die nötigen Werte in ein Data Frame packt

prepare_energy_dataframe <- function(data) {
  # Erdölprodukte CH - Gas 
  erdgas <- data %>%
    filter(Jahr >= 1990,
           Rubrik == "Endverbrauch - Total",
           Energietraeger == "Gas") %>%
    select(Jahr, Rubrik, Energietraeger, TJ)
  
  # Erdölprodukte CH - Kohle
  kohle <- data %>%
    filter(Jahr >= 1990,
           Rubrik == "Endverbrauch - Total",
           Energietraeger == "Kohle") %>%
    select(Jahr, Rubrik, Energietraeger, TJ)
  
  # Verkehr CH - Erdölprodukte inkl. Kerosin
  oel_verkehr <- data %>%
    filter(Jahr >= 1990,
           Rubrik == "Endverbrauch - Verkehr",
           Energietraeger == "Erdölprodukte") %>%
    select(Jahr, Rubrik, Energietraeger, TJ)
  
  # Heizöl = Endverbrauch Total - Verkehr (Erdölprodukte)
  heizoel <- data %>%
    filter(Jahr >= 1990,
           Rubrik %in% c("Endverbrauch - Total", "Endverbrauch - Verkehr"),
           Energietraeger == "Erdölprodukte") %>%
    select(Jahr, Rubrik, TJ) %>%
    pivot_wider(names_from = Rubrik, values_from = TJ) %>%
    mutate(
      Rubrik = "Endverbrauch - Total",
      Energietraeger = "Heizöl",
      TJ = `Endverbrauch - Total` - `Endverbrauch - Verkehr`
    ) %>%
    select(Jahr, Rubrik, Energietraeger, TJ)
  
  # Alle vier Datensätze zusammenführen
  bind_rows(erdgas, kohle, oel_verkehr, heizoel)
}

## Spucke die Werte in einem Data Frame aus
dataframe_energy <- prepare_energy_dataframe(data)










#### als Funktion geschrieben ####


berechne_emissionen <- function(jahr) {
  EF_HEIZOEL <- 265
  EF_TREIBSTOFFE <- 250
  EF_GAS <- 198
  gas_MWh_zh <-   #XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXx
  
  # Bevölkerungszahlen dynamisch holen
  zh_bev_jahr <- zh_bev_long %>% filter(Jahr == jahr) %>% pull(TOTAL)
  ch_bev_jahr <- ch_bev %>% filter(Jahr == jahr) %>% pull(Bevölkerung)
  
  bev_faktor_zh <- zh_bev_jahr / ch_bev_jahr
  
  # Energiedaten für Jahr filtern
  dataframe_jahr <- filter(dataframe_energy, Jahr == jahr)
  
  erdoel_MWh_zh <- sum(dataframe_jahr$TJ[c(1, 2, 4)]) / 3.6 * bev_faktor_zh * 1000
  treibstoff_MWh_zh <- dataframe_jahr$TJ[3] / 3.6 * bev_faktor_zh * 1000
  
  heizoel_MWh_zh <- erdoel_MWh_zh - gas_MWh_zh
  
  heizoel_CO2 <- heizoel_MWh_zh / 1000 * EF_HEIZOEL
  treibstoff_CO2 <- treibstoff_MWh_zh / 1000 * EF_TREIBSTOFFE
  gas_CO2 <- gas_MWh_zh / 1000 * EF_GAS
  
  total_CO2_zh <- heizoel_CO2 + treibstoff_CO2 + gas_CO2
  total_CO2_proKopf <- total_CO2_zh / zh_bev_jahr
  
  return(list(
    Jahr = jahr,
    Total_t_CO2 = round(total_CO2_zh, 0),
    t_CO2_pro_Kopf = round(total_CO2_proKopf, 2)
  ))
}

## Tabelle erstellen für alle Jahre ab 2012
jahre_ab_2012 <- intersect(zh_bev_long$Jahr, ch_bev$Jahr) #sind alle Jahre in beiden Datensätzen vorhanden?
jahre_ab_2012 <- jahre_ab_2012[jahre_ab_2012 >= 2012] #wähle gewünschtes Zeitintervall

emissions_liste <- lapply(jahre_ab_2012, berechne_emissionen) #schleife der Funktion berechne_emissionen über alle gefilterten Jahre
emissions_tabelle <- do.call(rbind, lapply(emissions_liste, as.data.frame))




## KEF-INdikator berechnen (aus dem Schnitt der vergangenen 4 Jahre, um kleine Ausnahmen auszugleichen)


XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXx
XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXx


#### Berechnungen manuell ####

dataframe_2023 <- filter(dataframe_energy, Jahr == 2023)

# Erdölprodukte für Zürich in tCo2 Äquivalente berechnen
# Total CH in TJ/3.6/Verhältnis CH/Zürcher Bevölkerung * 1000
erdoelpr_MWh_zh <- sum(dataframe_2023$TJ[c(1, 2, 4)])/3.6/(ch_bev$Bevölkerung[43]/zh_bev$TOTAL_2023[1])*1000
heizoel_MWh_zh <- (erdoelpr_MWh_zh - 4807711) #XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
heizoel_tCO2_zh <- heizoel_MWh_zh/1000*265 #265 ist der Kennwert zur Umrechnung

# Treibstoffe für Zürich tCo2 Äquivalente berechnen
verkehr_treibstoffe_MWh_zh <- dataframe_2023$TJ[3]/3.6/(ch_bev$Bevölkerung[43]/zh_bev$TOTAL_2023[1])*1000
verkehr_treibstoffe_tCo2_zh <- verkehr_treibstoffe_MWh_zh/1000*250 #250 ist der Kennwert zur Umrechnung

# Gas für Zürich in tCo2 Äquivalente berechnen
gas_MWh_zh <- 4807711
gas_tCo2_zh <- gas_MWh_zh/1000*198 #198 ist der Kennwert zur Umrechnung

# Total in Tonnen CO2
totall_tCo2_proKopf_zh <- sum(heizoel_tCO2_zh, verkehr_treibstoffe_tCo2_zh, gas_tCo2_zh)/zh_bev$TOTAL_2023[1]

