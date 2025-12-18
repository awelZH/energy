# -----------------------------
# Jahr festlegen
# -----------------------------
jahr <- 2024



# Bevölkerung
# -----------------------------
zh_pop <- zh_bev %>%
  dplyr::filter(Jahr == jahr) %>%
  dplyr::pull(bevölkerung)

ch_pop <- ch_bev %>%
  dplyr::filter(Jahr == jahr) %>%
  dplyr::pull(bevölkerung)

pop_factor <- zh_pop / ch_pop


# -----------------------------
# Energiedaten CH
# -----------------------------
e_2024 <- dataframe_energy %>%
  dplyr::filter(Jahr == jahr)

tj_gas <- e_2024 %>%
  dplyr::filter(Energietraeger == "Gas") %>%
  dplyr::pull(TJ)

tj_kohle <- e_2024 %>%
  dplyr::filter(Energietraeger == "Kohle") %>%
  dplyr::pull(TJ)

tj_heizoel <- e_2024 %>%
  dplyr::filter(Energietraeger == "Heizöl") %>%
  dplyr::pull(TJ)

tj_verkehr <- e_2024 %>%
  dplyr::filter(Energietraeger == "Erdölprodukte + Gas") %>%
  dplyr::pull(TJ)


# -----------------------------
# Umrechnung TJ → MWh (ZH)
# -----------------------------
tj_to_MWh_zh <- function(tj_ch) {
  tj_ch / 3.6 * 1000 * pop_factor
}

gas_MWh_zh_CH     <- tj_to_MWh_zh(tj_gas)
kohle_MWh_zh_CH   <- tj_to_MWh_zh(tj_kohle)
heizoel_MWh_zh_CH <- tj_to_MWh_zh(tj_heizoel)
verkehr_MWh_zh    <- tj_to_MWh_zh(tj_verkehr)


# -----------------------------
# Kantonale Gasdaten (ersetzen Hochrechnung)
# -----------------------------
gas_MWh_zh <- kantonale_gasdaten %>%
  dplyr::filter(jahr == jahr) %>%
  dplyr::pull(kantonswert_MWh)


# -----------------------------
# CO2-Berechnung
# -----------------------------
heizoel_CO2 <- heizoel_MWh_zh_CH / 1000 * 265
treibst_CO2 <- verkehr_MWh_zh    / 1000 * 250
gas_CO2     <- gas_MWh_zh        / 1000 * 198


total_CO2 <- heizoel_CO2 + treibst_CO2 + gas_CO2
total_CO2_pKopf <- total_CO2 / zh_pop


# -----------------------------
# Ergebnis
# -----------------------------
tibble::tibble(
  Jahr = jahr,
  CO2_Emissionen_total    = round(total_CO2, 0),
  CO2_Emissionen_pro_Kopf = round(total_CO2_pKopf, 2)
)