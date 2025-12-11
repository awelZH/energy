# EP - Erdölprodukte ----------------------------------------------------


# Import data -------------------------------------------------------------
# Schritt 1 : hier werden die Daten eingelesen

ds <- create_dataset('Q1')
Q1 <- download_data(ds)

Q1 <- Q1$data

ds <- create_dataset('EP')
EP <- download_data(ds)

EP <- EP$data


ds <- create_dataset('EG')
EG <- download_data(ds)

EG <- EG$data

# Dieses Objekt dient als Grundlage zur Weiterverarbeitung



## zusätzliche Daten für Errechnung Indikator



# Berechnungen -----------------------------------------------------

# Schritt 2 : Falls die zu publizierenden Werte noch berechnet werden müssen, können hier Aggregierungs- und Transformationsschritte vorgenommen werden.

# Beispiele :
# - neue Kategorien oder Totale bilden
# - Anteile berechnen
# - Umbenennung von Kategorien


#### Datenaufbereitung ZH Bevölkerung ####

## Q1 auf Kanton Zürich kürzen
zh_bev <- Q1 %>%
  dplyr::filter(Kanton == "Zürich") %>%  
  dplyr::group_by(Jahr) %>% 
  dplyr::summarize(bevölkerung = sum(`Ständige und nichtständige Wohnbevölkerung`, na.rm = TRUE)) %>% 
  dplyr::ungroup()

## Q1 nach Jahr gruppieren für Total CH
ch_bev <- Q1 %>%
  dplyr::group_by(Jahr) %>% 
  dplyr::summarize(bevölkerung = sum(`Ständige und nichtständige Wohnbevölkerung`, na.rm = TRUE)) %>% 
  dplyr::ungroup()

## Jahr von character → numeric
zh_bev <- zh_bev %>%
  dplyr::mutate(jahr = as.numeric(Jahr))

ch_bev <- ch_bev %>%
  dplyr::mutate(jahr = as.numeric(Jahr))




#### Datenaufbereitung Gasdaten####

kantonale_gasdaten <- EG %>%
  dplyr::group_by(jahr) %>%
  dplyr::summarise(kantonswert_MWh = sum(wert, na.rm = TRUE)) %>%
  dplyr::ungroup()


#### Datenaufbereitung Energiestatistik####

prepare_energy_dataframe <- function(data) {
  # Erdgas
  erdgas <- data %>%
    filter(Jahr >= 1990,
           Rubrik == "Endverbrauch - Total",
           Energietraeger == "Gas") %>%
    dplyr::select(Jahr, Rubrik, Energietraeger, TJ)
  
  # Kohle
  kohle <- data %>%
    filter(Jahr >= 1990,
           Rubrik == "Endverbrauch - Total",
           Energietraeger == "Kohle") %>%
    dplyr::select(Jahr, Rubrik, Energietraeger, TJ)
  
  # Verkehr Erdölprodukte
  oel_verkehr <- data %>%
    filter(Jahr >= 1990,
           Rubrik == "Endverbrauch - Verkehr",
           Energietraeger == "Erdölprodukte") %>%
    dplyr::select(Jahr, Rubrik, Energietraeger, TJ)
  
  # Heizöl
  heizoel <- data %>%
    filter(Jahr >= 1990,
           Rubrik %in% c("Endverbrauch - Total", "Endverbrauch - Verkehr"),
           Energietraeger == "Erdölprodukte") %>%
    dplyr::select(Jahr, Rubrik, TJ) %>%
    pivot_wider(names_from = Rubrik, values_from = TJ) %>%
    dplyr::mutate(
      Rubrik = "Endverbrauch - Total",
      Energietraeger = "Heizöl",
      TJ = `Endverbrauch - Total` - `Endverbrauch - Verkehr`
    ) %>%
    dplyr::select(Jahr, Rubrik, Energietraeger, TJ)
  
  bind_rows(erdgas, kohle, oel_verkehr, heizoel)
}

## Dataframe erzeugen basierend auf EP
dataframe_energy <- prepare_energy_dataframe(EP)


## Sicherheitschecks

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
  
  zh_pop <- zh_bev_df %>% filter(jahr == !!jahr) %>% pull(bevölkerung)
  ch_pop <- ch_bev_df %>% filter(jahr == !!jahr) %>% pull(bevölkerung)
  pop_factor <- zh_pop / ch_pop
  
  e_j <- energy_df %>% filter(Jahr == !!jahr)
  
  tj_gas     <- e_j %>% filter(Energietraeger == "Gas")           %>% pull(TJ)
  tj_kohle   <- e_j %>% filter(Energietraeger == "Kohle")         %>% pull(TJ)
  tj_heizoel <- e_j %>% filter(Energietraeger == "Heizöl")        %>% pull(TJ)
  tj_verkehr <- e_j %>% filter(Energietraeger == "Erdölprodukte") %>% pull(TJ)
  
  gas_MWh_zh <- gas_df %>% filter(jahr == !!jahr) %>% pull(kantonswert_MWh)
  
  erdoel_MWh_zh  <- (tj_gas + tj_kohle + tj_heizoel) / 3.6 * pop_factor * 1000
  heizoel_MWh_zh <- erdoel_MWh_zh - gas_MWh_zh
  verkehr_MWh_zh <- tj_verkehr / 3.6 * pop_factor * 1000
  
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

jahre_ab_2012 <- zh_bev$jahr[zh_bev$jahr >= 2012 & zh_bev$jahr %in% ch_bev$jahr]
emissions_tabelle <- purrr::map_dfr(jahre_ab_2012, berechne_emissionen)

print(emissions_tabelle)

emissions_tabelle <- emissions_tabelle %>%
  arrange(Jahr) %>%
  rename(CO2_Emissionen_pro_Kopf = total_CO2_pKopf_t,
         CO2_Emissionen_total = total_CO2_t) %>% 
  mutate(
    `KEF Indikator W11` = rollmean(CO2_Emissionen_pro_Kopf, k = 4, fill = NA, align = "right")
  )

print(emissions_tabelle)

# Die Voraussetzung für den letzten Schritt (3) ist ein Datensatz im long Format nach folgendem Beispiel:

# # A tibble: 216 × 5
#    Jahr  Gebiet  Treibstoff_Typ Einheit         Wert
#    <chr> <chr>   <chr>          <chr>          <dbl>
#  1 2005  Schweiz fossil         Anzahl  306455
#  2 2005  Schweiz fossil         Total   307161
#  3 2005  Schweiz fossil         Anteil       0.998
#  4 2005  Schweiz fossil-free    Anzahl     706
#  5 2005  Schweiz fossil-free    Total   307161

# Harmonisierung Datenstruktur / Bezeichnungen  ----------------------------------------------------------

# Schritt 3 : Hier werden die Daten in die finale Form gebracht

# - Angleichung der Spaltennamen / Kategorien und Einheitslabels an die Konvention
# - Anreicherung mit Metadaten aus der Datensatzliste

EP_export_data <- EP_computed %>%
# Beispiel - dieser Block dient nur der Veranschalichung und muss je nach Fall angepasst werden --------
# dplyr::filter(Einheit != 'Total') %>%
# dplyr::rename('Variable' = Treibstoff_Typ) %>%
# # Renaming values
# dplyr::mutate(Gebiet = dplyr::if_else(Gebiet == 'Zürich', 'Kanton Zürich', Gebiet),
#               Variable = dplyr::if_else(Variable == 'fossil', 'fossiler Treibstoff', 'fossilfreier Treibstoff'),
#               Einheit = dplyr::case_when(Einheit == 'Anzahl' ~ paste0(ds$dimension_label, ' [Anz.]'),
#                                          Einheit == 'Anteil' ~ paste0(ds$dimension_label, ' [%]'),
#                                          TRUE ~ Einheit)) %>%
# ----------------------
# Anreicherung  mit Metadaten
  dplyr::mutate(Indikator_ID = ds$dataset_id,
                Indikator_Name = ds$indicator_name,
                Datenquelle = ds$data_source) %>%
  dplyr::select(Jahr, Gebiet, Indikator_ID, Indikator_Name, Variable, Wert, Einheit, Datenquelle)

# assign data to be exported back to the initial ds object -> ready to export
ds$export_data <- EP_export_data

# Export CSV --------------------------------------------------------------

# Daten werden in den /output - Ordner geschrieben

export_data(ds)