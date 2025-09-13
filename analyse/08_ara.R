#### ARA ####

# Für die Energiestatistik werden auch Angaben zu ARAs ausgewertet
# Die Daten (Jahresstand) werden aus Daten der EKZ und ewz (Werdhölzli) bezogen und können mit R eingelesen, umgeformt und ausgewertet werden. 


rm(list = ls())

# Scripts und libraries einlesen
source(here::here("analyse/gemeinden.R"))
library(readxl)


#### INPUT DATEN ####

ara_input_ekz <- as.data.frame(read_xls("K:/BD-AWEL-050-EN/Elektrizitätswirtschaft/Administration/Praktikum/Levi_Fuchs/Projekte/Energiestatistik_neu/R_Project/ARA/AWEL_2024_Frau_Baumann.xls", sheet = "Zusammenfassung_fuer_Kunden"))

ara_input_ewz <- read_xlsx("K:/BD-AWEL-050-EN/Elektrizitätswirtschaft/Administration/Praktikum/Levi_Fuchs/Projekte/Energiestatistik_neu/R_Project/ARA/Gelieferte Energiemengen aus gereinigtem Abwasser EDL.xlsx")

ara_olddata <- read.csv(here::here("data/input/ara_anlagen_28_08_25.csv"), sep=";")

ara_potenzial_2022 <- read.csv(here::here("data/input/ara_potenzial_2022.csv"), sep=";")

jahr_heute <- as.integer(format(Sys.Date(), "%Y"))

jahr_datensatz <- basename("K:/BD-AWEL-050-EN/Elektrizitätswirtschaft/Administration/Praktikum/Levi_Fuchs/Projekte/Energiestatistik_neu/R_Project/ARA/AWEL_2024_Frau_Baumann.xlsx") %>% str_extract("\\d{4}") %>%  # genau 4-stellige Zahl
  as.integer()



#### FUNKTIONEN ####

# Funktion für exakte, fuzzy & Teilstring-Suche
match_names <- function(ara_name, geo_names, threshold = 0.1) {
  # 1️⃣ Exakter Abgleich
  if (ara_name %in% geo_names) {
    return(ara_name)
  }
  
  # 2️⃣ Fuzzy Matching mit Jaro-Winkler
  distances <- stringdist::stringdist(ara_name, geo_names, method = "jw")
  best_match <- geo_names[which.min(distances)]
  best_score <- min(distances)
  
  if (best_score < threshold) {
    return(best_match)
  }
  
  # 3️⃣ Teilstring-Suche (z.B. "Effretikon" in "Illnau-Effretikon")
  contains_match <- geo_names[str_detect(geo_names, fixed(ara_name, ignore_case = TRUE))]
  if (length(contains_match) > 0) {
    return(contains_match[1])  # Erstes gefundene Match zurückgeben
  }
  
  return(NA)  # Falls nichts passt
}



#### AUFBEREITUNG DATEN ####

# EKZ Daten

ekz_prov <- ara_input_ekz %>%
  slice((which(grepl("mwh", ara_input_ekz, ignore.case = TRUE))[1]-1):n()) %>%
  set_names(as.character(unlist(.[1,])))

names(ekz_prov)[1] <- "ort"

ekz_prov2 <- ekz_prov %>% 
  select(ort, names(ekz_prov)[grepl("mwh", names(ekz_prov), ignore.case = TRUE)]) %>%
  slice(-1) %>%
  rename(wert = names(.)[2]) %>%
  filter(!is.na(ort) | !is.na(wert)) %>%
  filter(!grepl("total", ort, ignore.case = T)) %>%
  mutate(ort = ifelse(grepl("^ARA", ort), ort, NA)) %>%
  # Zuerst die 'ort'-Spalte mit dem zuletzt bekannten Wert nach unten füllen
  mutate(ort = zoo::na.locf(ort, na.rm = FALSE)) %>%
  # Dann die 'wert'-Spalte mit dem zuletzt bekannten Wert nach oben füllen
  arrange(desc(row_number())) %>%
  mutate(wert = round(as.numeric(zoo::na.locf(wert, na.rm = FALSE)),0)) %>%
  arrange(row_number()) %>%  # Reihenfolge wiederherstellen
  # Nur die Zeilen mit einer Anlage behalten
  distinct(ort, .keep_all = TRUE) %>%
  filter(!is.na(ort)) %>%
  mutate(clean_name = str_squish(str_remove_all(str_remove(ort, "^ARA "), "\\(.*\\)"))) %>%
  mutate(ort = str_remove(ort, "^ARA "))


# Fuzzy-Matching durchführen
ekz_final <- ekz_prov2 %>%
  mutate(matched_name = sapply(clean_name, function(x) match_names(x, gemeinden_zh$gemeinde, threshold = 0.1))) %>% # Join mit dem BFS-Nummern-Datensatz
  left_join(gemeinden_zh, by = c("matched_name" = "gemeinde")) %>%
  left_join(ara_olddata %>% select(ort, bfsnr), by = "bfsnr", relationship = "many-to-many") %>%
  distinct() %>%
  mutate(selected_value = case_when(grepl("\\(", ort.x) ~ ort.x, grepl("\\(", ort.y) ~ ort.y, TRUE ~ coalesce(ort.x, ort.y))) %>% #Auswahl des priorisierten Anlagenamens (grundsätzlich Namen mit Ergänzungen bevorzugen)
  mutate(jahr = jahr_datensatz, rubrik = "Wärme", thema = "ARA", einheit = "MWh",  ort = selected_value) %>%
  select(jahr, rubrik, thema, wert, einheit, ort, bfsnr) %>%
  distinct()

# ewz Daten (Werdhölzli)

ewz_final <- ara_input_ewz %>%
  slice((which(apply(ara_input_ewz, 1, function(row) any(grepl("total", row, ignore.case = TRUE))))[1]):n()) %>%
  { .[1, 1] <- "dummy"; . } %>%
  set_names(as.character(unlist(.[1,]))) %>%
  select(contains("dummy") | contains("total")) %>%
  filter(grepl("total", dummy, ignore.case = TRUE)) %>% # df[[1]] ist die erste Spalte 
  mutate(jahr = as.integer(str_remove(colnames(.)[ncol(.)], " *total")), wert = as.numeric(.[[ncol(.) - 1]]), ort = "Zürich (Werdhölzli)", bfsnr = 261, rubrik = "Wärme", thema = "ARA", einheit = "MWh") %>%  # .[[ncol(.)]] greift auf die letzte Spalte zu
  select(jahr, rubrik, thema, wert, einheit, ort, bfsnr) # Nur die gewünschten Spalten behalten


ekz_ewz_final <- rbind(ekz_final, ewz_final)


#### AKTUALISIERUNG ALTER DATEN ####

# Bei denjenigen Anlagen, zu welchen wir keine aktuellen Angaben erhalten, übernehmen wir die Werte aus dem Vorjahr. Die neuen werden dann angehängt. Da in den neuen Daten teilweise die Orte konkreter beschrieben sind (s. zum Uster Prioritätsgebiet 1 und 2), werden immer die Ortsnamen (Anlagenamen) de neueren Datensatzes bevorzugt.

# Max Jahre berechnen
max_olddata <- max(ara_olddata$jahr, na.rm = TRUE)
max_newdata <- max(ekz_ewz_final$jahr, na.rm = TRUE)


if (max_newdata > max_olddata) {
  ara_final_anlage <- ara_olddata %>%
    bind_rows(
      ara_olddata %>%
        filter(jahr == max_olddata) %>%
        mutate(jahr = max_newdata)
    ) %>%
    full_join(ekz_ewz_final, by = c("jahr", "bfsnr"), suffix = c("_old", "_new")) %>%
    mutate(
      wert   = coalesce(wert_new, wert_old),
      ort    = coalesce(ort_new, ort_old),
      rubrik = "Wärme",
      thema  = "ARA",
      einheit = "MWh"
    ) %>%
    select(jahr, rubrik, thema, wert, einheit, ort, bfsnr) %>%
    arrange(jahr)
} else {
  ara_final_anlage <- ara_olddata}


# Anlagen-Datensatz als csv speichern (für zukünftige Verwendung olddata)
write_excel_csv2(ara_final_anlage, here::here("data/output/ara_anlagen.csv"))


## Finalisieren

ara_final <- ara_final_anlage %>%
  left_join(ara_potenzial_2022 %>% select(bfsnr, wert), by = "bfsnr", suffix = c("", "_pot"), relationship = "many-to-many") %>%
  distinct() %>%
  group_by(bfsnr, jahr)  %>%
  mutate(wert_pot = wert_pot / n()) %>% 
  summarise(wert_gem = sum(wert, na.rm = TRUE), wert_pot_gem = sum(wert_pot, na.rm = TRUE), .groups = 'drop') %>%
  left_join(gemeinden_zh, by = "bfsnr") %>%
  mutate(wert_pot_ungenutzt_gem = wert_pot_gem - wert_gem,
         rubrik = "ARA",
         ort = gemeinde) %>%
  pivot_longer(
    cols = c(wert_gem, wert_pot_gem, wert_pot_ungenutzt_gem),
    names_to = "wert_pivot_long",
    values_to = "wert"
  ) %>%
  mutate(
    thema = case_when(
      wert_pivot_long == "wert_gem" ~ "Wärmepotenzial genutzt",
      wert_pivot_long == "wert_pot_gem" ~ "Wärmepotenzial",
      wert_pivot_long == "wert_pot_ungenutzt_gem" ~ "Wärmepotenzial ungenutzt"
    ),
    einheit = "MWh"
  ) %>%
  select(jahr, ort, bfsnr, rubrik, thema, wert, einheit) %>%
  arrange(ort, jahr)



write_excel_csv2(ara_final, here::here("data/output/ara.csv"))


