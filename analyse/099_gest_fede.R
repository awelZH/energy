library(dplyr)
library(tidyr)
library(readxl)
library(readr)
library(purrr)
library(zoo)
library(openxlsx)


## Daten einlesen

## Bevölkerungsdaten
ch_bev <- read_csv("data/input/bev_gem.csv")


## Energiedaten
data <- read_csv('https://www.uvek-gis.admin.ch/BFE/ogd/115/ogd115_gest_bilanz.csv')

## Gasdaten (separat)
gasdaten <- read_csv("data/input/gas_inp_gem.csv")


#### Datenaufbereitung ZH Bevölkerung####

## ch_bev auf kanton kürzen

zh_bev <- ch_bev %>%
  filter(kt == "Zürich") %>%  # nur kantonale Daten
  group_by(jahr) %>% 
  summarize(bevölkerung = sum(value, na.rm =TRUE)) %>% 
  ungroup

 
## ch_bev nach jahr gruppieren für total

ch_bev <- ch_bev %>%
  group_by(jahr) %>% 
  summarize(bevölkerung = sum(value, na.rm =TRUE)) %>% 
  ungroup


zh_bev <- zh_bev %>%
  mutate(jahr = as.numeric(jahr))

ch_bev <- ch_bev %>%
  mutate(jahr = as.numeric(jahr))


#### Datenaufbereitung Gasdaten####

## gasdaten in longformat bringen und nur Total behalten (den Rest brauchen wir nicht)

kantonale_gasdaten <- gasdaten %>%
  group_by(jahr) %>%
  summarise(kantonswert_MWh = sum(wert, na.rm = TRUE)) %>%
  ungroup()


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





## Sicherheitschecks, das jeweils 1 Zeile mit Bevölkerungszahl CH und ZH auftaucht

check_bevölkerung <- function(jahr_input, ch_bev_df = ch_bev, zh_bev_df = zh_bev) {
  zh_pop <- zh_bev_df %>% filter(jahr == jahr_input) %>% pull(bevölkerung)
  ch_pop <- ch_bev_df %>% filter(jahr == jahr_input) %>% pull(bevölkerung)
  cat("Jahr:", jahr_input, "| ZH Pop:", length(zh_pop), zh_pop, "| CH Pop:", length(ch_pop), ch_pop, "\n")
  tibble(
    Jahr = jahr_input,
    ZH_Bev = zh_pop,
    CH_Bev = ch_pop
  )
}
check_bevölkerung(2023)

## Sicherheitstest für Input-Werte

test_energie <- function(jahr_input, energy_df = dataframe_energy) {
  e_j <- energy_df %>% filter(Jahr == jahr_input)
  print(e_j)
  tibble(
    Jahr = jahr_input,
    n_zeilen = nrow(e_j)
  )
}
test_energie(2023)


# ------------------------------------------------------------
#  Funktion zur Berechnung CO2-Emissionen ####
# ------------------------------------------------------------

berechne_emissionen <- function(jahr,
                                energy_df = dataframe_energy,
                                gas_df    = kantonale_gasdaten,
                                ch_bev_df = ch_bev,
                                zh_bev_df = zh_bev,
                                ef_heizoel     = 265,
                                ef_treibstoffe = 250,
                                ef_gas         = 198) {
  
  # Bevölkerungszahlen holen
  zh_pop <- zh_bev_df %>% filter(jahr == !!jahr) %>% pull(bevölkerung)
  ch_pop <- ch_bev_df %>% filter(jahr == !!jahr) %>% pull(bevölkerung)
  pop_factor <- zh_pop / ch_pop
  
  # Energiedaten fürs Jahr
  e_j <- energy_df %>% filter(Jahr == !!jahr)
  
  tj_gas     <- e_j %>% filter(Energietraeger == "Gas")           %>% pull(TJ)
  tj_kohle   <- e_j %>% filter(Energietraeger == "Kohle")         %>% pull(TJ)
  tj_heizoel <- e_j %>% filter(Energietraeger == "Heizöl")        %>% pull(TJ)
  tj_verkehr <- e_j %>% filter(Energietraeger == "Erdölprodukte") %>% pull(TJ)
  
  # Gasdaten Kanton Zürich (MWh)
  gas_MWh_zh <- gas_df %>% filter(jahr == !!jahr) %>% pull(kantonswert_MWh)
  
  # Umrechnung
  erdoel_MWh_zh  <- (tj_gas + tj_kohle + tj_heizoel) / 3.6 * pop_factor * 1000
  heizoel_MWh_zh <- erdoel_MWh_zh - gas_MWh_zh
  verkehr_MWh_zh <- tj_verkehr / 3.6 * pop_factor * 1000
  
  # Emissionen (tCO2)
  heizoel_CO2 <- heizoel_MWh_zh / 1000 * ef_heizoel
  treibst_CO2 <- verkehr_MWh_zh / 1000 * ef_treibstoffe
  gas_CO2     <- gas_MWh_zh / 1000 * ef_gas
  
  total_CO2   <- heizoel_CO2 + treibst_CO2 + gas_CO2
  total_CO2_pKopf <- total_CO2 / zh_pop
  
  tibble(
    Jahr              = jahr,
    total_CO2_t       = round(total_CO2, 0),
    total_CO2_pKopf_t = round(total_CO2_pKopf, 2)
  )
}

# Für alle Jahre ab 2012:
jahre_ab_2012 <- zh_bev$jahr[zh_bev$jahr >= 2012 & zh_bev$jahr %in% ch_bev$jahr]
emissions_tabelle <- purrr::map_dfr(jahre_ab_2012, berechne_emissionen)

print(emissions_tabelle)


## KEF-INdikator berechnen (aus dem Schnitt der vergangenen 4 Jahre, um kleine Ausnahmen auszugleichen)


# Die wichtigsten Argumente von rollmean():

## x: Der Vektor, auf dem du den gleitenden Mittelwert berechnen willst (hier  pro-Kopf-Emissionen).
## k: Die Fenstergröße (wie viele Werte du für den Durchschnitt nimmst)
## fill: Was passiert, wenn das Fenster an den Rändern (also am Anfang deiner Daten) nicht komplett gefüllt werden kann
## align: Wie das Fenster auf die Daten ausgerichtet wird.
### "right" Mittelwert wird am rechten Rand des Fensters ausgegeben, also für das aktuelle Jahr plus die 3 Jahre davor.


# KEF: rollender Mittelwert über 4 Jahre (aktuell + 3 davor), align = "right"
emissions_tabelle <- emissions_tabelle %>%
  arrange(Jahr) %>%
  rename(CO2_Emissionen_pro_Kopf = total_CO2_pKopf_t,
         CO2_Emissionen_total = total_CO2_t) %>% 
  mutate(
    `KEF Indikator W11` = rollmean(CO2_Emissionen_pro_Kopf, k = 4, fill = NA, align = "right")
  )

print(emissions_tabelle)



#### Emissionstabelle als Excel - Tabelle

write.xlsx(emissions_tabelle, file="Tabelle_Energiestatistik.xlsx")











#### Berechnungen manuell ####

dataframe_2023 <- filter(dataframe_energy, Jahr == 2023)

# Erdölprodukte für Zürich in tCo2 Äquivalente berechnen
# Total CH in TJ/3.6/Verhältnis CH/Zürcher Bevölkerung * 1000
erdoelpr_MWh_zh <- sum(dataframe_2023$TJ[c(1, 2, 4)])/3.6/(ch_bev$bevölkerung[43]/zh_bev$bevölkerung[43])*1000
gas_MWh_zh <- kantonale_gasdaten$kantonswert_MWh[20]
heizoel_MWh_zh <- (erdoelpr_MWh_zh - gas_MWh_zh) 
heizoel_tCO2_zh <- heizoel_MWh_zh/1000*265 #265 ist der Kennwert zur Umrechnung

# Treibstoffe für Zürich tCo2 Äquivalente berechnen
verkehr_treibstoffe_MWh_zh <- dataframe_2023$TJ[3]/3.6/(ch_bev$bevölkerung[43]/zh_bev$bevölkerung[43])*1000
verkehr_treibstoffe_tCo2_zh <- verkehr_treibstoffe_MWh_zh/1000*250 #250 ist der Kennwert zur Umrechnung

# Gas für Zürich in tCo2 Äquivalente berechnen

gas_tCo2_zh <- gas_MWh_zh/1000*198 #198 ist der Kennwert zur Umrechnung

# Total in Tonnen CO2
totall_tCo2_proKopf_zh <- sum(heizoel_tCO2_zh, verkehr_treibstoffe_tCo2_zh, gas_tCo2_zh)/zh_bev$bevölkerung[43]




