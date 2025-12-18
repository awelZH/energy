# EP - Erdölprodukte ----------------------------------------------------

rm(list = c("create_dataset", "indicator_init"))

#################### !! sind die Gasdaten aktualisiert???? ----------------
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

EG <- gasdaten


# Berechnungen -----------------------------------------------------

# Schritt 2 : Falls die zu publizierenden Werte noch berechnet werden müssen, können hier Aggregierungs- und Transformationsschritte vorgenommen werden.

## Datenaufbereitung ZH Bevölkerung ####



Q1_clean <- Q1 %>%
  dplyr::rename(bevölkerung = `Ständige und nichtständige Wohnbevölkerung`) %>%
  dplyr::mutate(Jahr = as.numeric(Jahr))

zh_bev <- Q1_clean %>% dplyr::filter(Kanton == "Zürich")
ch_bev <- Q1_clean %>% dplyr::filter(Kanton == "Schweiz")

## Datenaufbereitung Gasdaten####

kantonale_gasdaten <- EG %>%
  dplyr::group_by(jahr) %>%
  dplyr::summarise(kantonswert_MWh = sum(wert, na.rm = TRUE)) %>%
  dplyr::ungroup()


## Datenaufbereitung Energiestatistik####

prepare_energy_dataframe <- function(EP) {
  
  # Hilfsfunktion: immer gleiche Filter + Auswahl
  get_energy <- function(rubrik, carrier, carrier_name = carrier[1]) {
  EP %>%
    dplyr::filter(
      Jahr >= 1990,
      Rubrik == rubrik,
      Energietraeger %in% carrier
    ) %>%
    dplyr::group_by(Jahr, Rubrik) %>%
    dplyr::summarise(
      TJ = sum(TJ, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::mutate(Energietraeger = carrier_name) %>%
    dplyr::select(Jahr, Rubrik, Energietraeger, TJ)
}

erdgas <- get_energy("Endverbrauch - Total", "Gas")
kohle <- get_energy("Endverbrauch - Total", "Kohle")
oel_verkehr <- get_energy("Endverbrauch - Verkehr", "Erdölprodukte")
# 4. Gas aus Verkehr
gas_verkehr <- get_energy("Endverbrauch - Verkehr", "Gas")
# Heizöl
heizoel <- EP %>%
  dplyr::filter(Jahr >= 1990,
                Rubrik %in% c("Endverbrauch - Total", "Endverbrauch - Verkehr"),
                Energietraeger == "Erdölprodukte") %>%
  dplyr::select(Jahr, Rubrik, TJ) %>%
  tidyr::pivot_wider(names_from = Rubrik, values_from = TJ) %>%
  dplyr::mutate(
    Rubrik = "Endverbrauch - Total",
    Energietraeger = "Heizöl",
    TJ = `Endverbrauch - Total` - `Endverbrauch - Verkehr`
  ) %>%
  dplyr::select(Jahr, Rubrik, Energietraeger, TJ)

dplyr::bind_rows(erdgas, kohle, oel_verkehr, heizoel, gas_verkehr)

}

## Dataframe erzeugen basierend auf EP
dataframe_energy <- prepare_energy_dataframe(EP)

## Sicherheitschecks

check_bevölkerung <- function(jahr_input, ch_bev_df = ch_bev, zh_bev_df = zh_bev) {
  zh_pop <- zh_bev_df %>% 
    dplyr::filter(Jahr == jahr_input) %>% 
    dplyr::pull(bevölkerung)
  ch_pop <- ch_bev_df %>% 
    dplyr::filter(Jahr == jahr_input) %>% 
    dplyr::pull(bevölkerung)
  cat("Jahr:", jahr_input, "| ZH Pop:", length(zh_pop), zh_pop, "| CH Pop:", length(ch_pop), ch_pop, "\n")
  dplyr::tibble(
    Jahr = jahr_input,
    ZH_Bev = zh_pop,
    CH_Bev = ch_pop
  )
}
check_bevölkerung(2024)

test_energie <- function(jahr_input, energy_df = dataframe_energy) {
  e_j <- energy_df %>% 
    dplyr::filter(Jahr == jahr_input)
  print(e_j)
  dplyr::tibble(
    Jahr = jahr_input,
    n_zeilen = nrow(e_j)
  )
}
test_energie(2024)

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
  
  zh_pop <- zh_bev_df %>% dplyr::filter(Jahr == !!jahr) %>% dplyr::pull(bevölkerung)
  ch_pop <- ch_bev_df %>% dplyr::filter(Jahr == !!jahr) %>% dplyr::pull(bevölkerung)
  pop_factor <- zh_pop / ch_pop
  
  e_j <- energy_df %>% dplyr::filter(Jahr == !!jahr)
  
  tj_gas     <- e_j %>% dplyr::filter(Energietraeger == "Gas")           %>% dplyr::pull(TJ)
  tj_kohle   <- e_j %>% dplyr::filter(Energietraeger == "Kohle")         %>% dplyr::pull(TJ)
  tj_heizoel <- e_j %>% dplyr::filter(Energietraeger == "Heizöl")        %>% dplyr::pull(TJ)
  tj_verkehr <- e_j %>% dplyr::filter(Energietraeger == "Erdölprodukte") %>% dplyr::pull(TJ)
  
  gas_MWh_zh <- gas_df %>% dplyr::filter(jahr == !!jahr) %>% dplyr::pull(kantonswert_MWh)
  
  erdoel_MWh_zh  <- (tj_gas + tj_kohle + tj_heizoel) / 3.6 * pop_factor * 1000
  heizoel_MWh_zh <- erdoel_MWh_zh - gas_MWh_zh
  verkehr_MWh_zh <- tj_verkehr/ 3.6 * pop_factor * 1000
  
  heizoel_CO2 <- heizoel_MWh_zh / 1000 * ef_heizoel
  treibst_CO2 <- verkehr_MWh_zh / 1000 * ef_treibstoffe
  gas_CO2     <- gas_MWh_zh / 1000 * ef_gas
  
  total_CO2   <- heizoel_CO2 + treibst_CO2 + gas_CO2
  total_CO2_pKopf <- total_CO2 / zh_pop
  
  dplyr::tibble(
    Jahr              = jahr,
    CO2_Emissionen_total       = round(total_CO2, 0),
    CO2_Emissionen_pro_Kopf = round(total_CO2_pKopf, 5)
  )
}

berechne_emissionen(2024)



# Schritt 3 : Hier werden die Daten in die finale Form gebracht

jahre_ab_2010 <- zh_bev$Jahr[zh_bev$Jahr >= 2010 & zh_bev$Jahr %in% ch_bev$Jahr]
emissions_tabelle <- purrr::map_dfr(jahre_ab_2010, berechne_emissionen)%>%
  dplyr::arrange(Jahr) %>%
  dplyr::mutate(
    `KEF Indikator W11` = zoo::rollmean(CO2_Emissionen_pro_Kopf, k = 4, fill = NA, align = "right")
  ) |> 
  print()

#### Emissionstabelle als Excel - Tabelle in energy > data > output

writexl::write.xlsx(emissions_tabelle, file="data/output/099_gest_tabelle_energiestatistik.xlsx")
