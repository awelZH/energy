### Auswertung Gesamtenergiestatistik - Strom ###

rm(list = ls())


# Scripts und libraries einlesen


strom_elcom <- read.csv("https://www.web.statistik.zh.ch/ogd/daten/ressourcen/KTZH_00001661_00003118.csv") #Bauten und Anlagen


source(here::here("analyse/04_sonne_pv.R"))
#source(here::here("analyse/10_kva.R")) #noch nicht erstellt

#Strom Bahn
strom_bahn <- read.csv2(here::here("data/output/bahnstrom.csv"))


strom_wasserkraft <- read.csv2(here::here("data/input/wasserkraft.csv"))
strom_wind <- 0

strom_kva <- read.csv(here::here("data/output/kva.csv")) %>% #später mit "source"
group_by(jahr) %>%
  summarise(strom_kva_gesamt = sum(kva_stromprod_mwh_anlage, na.rm = TRUE)) %>%
  ungroup()

strom_holz <- read.csv2(here::here("data/input/holzkraftwerk.csv")) %>%
  group_by(jahr) %>%
  summarise(strom_holz_gesamt = sum(holz_strom_mwh, na.rm = TRUE)) %>%
  ungroup()


strom_guelle <- 4000
strom_biogen <- 49000

strom_pv <- solar_pv_final %>%
  group_by(jahr) %>%
  summarise(strom_pv_gesamt = sum(cum_pv_strom_mwh, na.rm = TRUE)) %>%
  ungroup()

strom_geothermie_tief <- 0


#### Auswertung ####

# DataFrame erstellen
strom_df <- data.frame(jahr = 1980:as.integer(format(Sys.Date(), "%Y"))) %>%
  left_join(strom_elcom %>% filter(Energiesektor == "Strom"), by = c("jahr" = "Jahr")) %>%
  left_join(strom_bahn, by = "jahr") %>%
  left_join(strom_wasserkraft, by = "jahr") %>%
  left_join(strom_kva, by = "jahr") %>%
  left_join(strom_holz, by = "jahr") %>%
  left_join(strom_pv, by = "jahr") %>%
  mutate(strom_elcom = Wert * 1000, strom_wind = strom_wind, strom_guelle = strom_guelle, strom_biogen = strom_biogen, strom_geothermie_tief = strom_geothermie_tief) %>%
  mutate(strom_gesamt = rowSums(select(., all_of(c("strom_elcom", "strom_bahn_mwh_zh"))), na.rm = F),
    strom_erneuerbar = rowSums(select(., all_of(c("wasser_klein_strom_mwh", "wasser_gross_strom_mwh",
                                                  "strom_kva_gesamt", "strom_holz_gesamt", "strom_pv_gesamt",
                                                  "strom_wind", "strom_guelle", "strom_biogen", "strom_geothermie_tief"))), na.rm = T)) %>%
  mutate(anteil_erneuerbar = 100/strom_gesamt*strom_erneuerbar) %>%
  select(jahr, strom_gesamt, strom_erneuerbar, anteil_erneuerbar)
