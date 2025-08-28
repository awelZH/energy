#### KVA ####

# Für die Energiestatistik werden auch Angaben zu KVAs ausgewertet
# Die Daten für KVA-Abwärme können als Excel-Daten vom Bund online bezogen werden und können mit R eingelesen, umgeformt und ausgewertet werden.


rm(list = ls())

# Scripts und libraries einlesen
library(tidyverse) # a suite of packages for data wrangling, transformation, plotting, ...
#library(sf)
library(readr)
library(readxl)


#### INPUT DATEN ####

current_year <- as.integer(format(Sys.Date(), "%Y"))

#alte daten einfügen
kva_olddata <- read.csv(here::here("data/output/kva_27_08_25.csv"), sep=";")


# Ordnerpfad anpassen
pfad_kva <- "K:/BD-AWEL-050-EN/Elektrizitätswirtschaft/Administration/Praktikum/Levi_Fuchs/Projekte/Energiestatistik_neu/R_Project/KVA/daten_leomorf"

# Alle Excel-Dateien im Ordner auflisten
excel_dateien <- list.files(path = pfad_kva, pattern = "\\.xlsm$", full.names = TRUE)



#### AUFBEREITUNG DATEN ####

allowed_values <- c("Fverk", "Dverk", "Sprod", "Sverk", "Den", "DenL1", "DenL2", "DenL3", "DenL4", "Amenge", "Aheizw")


# Betrachtungsjahr bestimmen

betrachtungsjahr <- NA

for (datei in excel_dateien) {
  if (!"Resultatezusammenfassung" %in% excel_sheets(datei)) next
  
  daten <- read_excel(datei, sheet = "Resultatezusammenfassung", col_names = FALSE)
  
  zeile_idx <- which(apply(daten, 1, function(x) any(str_detect(x, "Betriebsjahr"))))
  
  if (length(zeile_idx) > 0) {
    zeile_text <- paste(daten[zeile_idx, ], collapse = " ")
    betrachtungsjahr <- str_extract(zeile_text, "\\d{4}") |> as.integer()
    message("Betrachtungsjahr gefunden in ", basename(datei), ": ", betrachtungsjahr)
    break  # <-- sobald gefunden, abbrechen
  }
}

if (is.na(betrachtungsjahr)) {
  stop("In keiner Datei ein Betrachtungsjahr gefunden!")
}




# Initialisiere eine leere Liste, um DataFrames mit Namen zu speichern
kva_dataframes <- list()


########### AB HIER! BETRIEBSJAHR IN DEN CODE EINFÜGEN!!! ############

# Schleife durch alle Excel-Dateien
for (datei in excel_dateien) {
  if ("Eingabemaske1_Energie" %in% excel_sheets(datei)) {
    daten <- read_excel(datei, sheet = "Eingabemaske1_Energie")
    
    # Inhalte durchsuchen (KVA-Namen usw.)
    text_inhalt <- tolower(as.character(unlist(daten)))
    text_inhalt <- text_inhalt[!is.na(text_inhalt)]
    
    if (any(str_detect(text_inhalt, "winterthur"))) {
      name <- "KVA Winterthur"
    } else if (any(str_detect(text_inhalt, "hinwil"))) {
      name <- "KEZO Hinwil"
    } else if (any(str_detect(text_inhalt, "limeco"))) {
      name <- "KV Limeco"
    } else if (any(str_detect(text_inhalt, "hagenholz"))) {
      name <- "ERZ Hagenholz"
    } else if (any(str_detect(text_inhalt, "horgen"))) {
      name <- "KVA Horgen"
    } else {
      name <- "Unbekannt"
    }
    
    # "Bilanzzeitraum"-Zeile finden
    bezeichnung_zeile <- which(str_detect(tolower(daten[[2]]), "bilanzzeitraum"))
    if (length(bezeichnung_zeile) == 0) next  
    
    # Ab "Bilanzzeitraum"-Zeile beschneiden
    daten_beschnitten <- daten[bezeichnung_zeile[1]:nrow(daten), ]
    
    # Spalten bis zum globalen Betrachtungsjahr behalten
    spalten_betrachtungsjahr <- which(
      str_detect(as.character(unlist(daten_beschnitten[1, ])), as.character(betrachtungsjahr))
    )
    if (length(spalten_betrachtungsjahr) > 0) {
      daten_beschnitten <- daten_beschnitten[, 1:max(spalten_betrachtungsjahr)]
    }
    
    # Spaltennamen setzen (1. Zeile)
    if (nrow(daten_beschnitten) > 1) {
      colnames(daten_beschnitten) <- as.character(unlist(daten_beschnitten[1, ]))
      daten_beschnitten <- daten_beschnitten[-1, ]
    }
    
    # ---------------------- Weiterverarbeitung ----------------------
    
    # Die erste Spalte in "Kuerzel" umbenennen
    if (ncol(daten_beschnitten) > 0) {
      colnames(daten_beschnitten)[1] <- "Kuerzel"
      
      # Überprüfen, ob die Spalte "Einheit" existiert, sonst suchen und umbenennen
      if (!"Einheit" %in% colnames(daten_beschnitten)) {
        # Schleife durch alle Spalten (einschließlich leerer Spaltennamen)
        for (i in seq_along(daten_beschnitten)) {
          col <- daten_beschnitten[[i]]  # Spalteninhalte
          
          # Suche nach "[MWh]" in der Spalte
          if (any(str_detect(as.character(col), "\\[MWh\\]"), na.rm = TRUE)) {
            colnames(daten_beschnitten)[i] <- "Einheit"  # Spalte umbenennen
            break  # Sobald die Spalte gefunden wurde, Schleife abbrechen
          }
        }
      }
      
      # Spaltennamen überprüfen und ungültige Spalten (z.B. leere oder NA) entfernen
      daten_beschnitten <- daten_beschnitten[, !is.na(colnames(daten_beschnitten))]
      
      # Filtere nur die Zeilen, bei denen "Kuerzel" einen der gewünschten Werte enthält
      daten_beschnitten <- daten_beschnitten %>% filter(Kuerzel %in% allowed_values)
      
      # Überprüfen, ob die benötigten Spalten vorhanden sind
      required_columns <- c("Kuerzel", "Bilanzzeitraum", "Einheit", as.character(betrachtungsjahr))
      existing_columns <- required_columns[required_columns %in% colnames(daten_beschnitten)]
      
      # Nur die vorhandenen Spalten behalten
      daten_beschnitten <- daten_beschnitten[, existing_columns, drop = FALSE]
      
      # Den bearbeiteten DataFrame in die Liste speichern
      kva_dataframes[[name]] <- daten_beschnitten
    }
  } else {
    message(paste("Sheet 'Eingabemaske1_Energie' nicht gefunden in Datei:", basename(datei)))
  }
}

kva_list_pivot <- lapply(kva_dataframes, function(df) {
  df %>%
    select(-Bilanzzeitraum, -Einheit) %>%  # Entfernt unerwünschte Spalten
    pivot_longer(cols = c(`2023`), names_to = "Jahr", values_to = "Wert") %>%
    pivot_wider(names_from = Kuerzel, values_from = Wert)
})


# Ergebnis anzeigen
# print(df_pivot)
# print(df_list_pivot[["KVA Winterthur"]], n = 100)
print(kva_dataframes[["KEZO Hinwil"]], n = 100)
# print(dataframes_liste_orig[["KVA Horgen"]], n = 100)


kva_jaresstand <- map_df(names(kva_list_pivot), function(name) {
  df <- kva_list_pivot[[name]]  # Zugriff auf den jeweiligen DataFrame
  
  df %>%
    mutate(
      anlage = name,  # Name des DataFrames als neue Spalte
      jahr = Jahr,
      kva_waerme_mwh_anlage = as.numeric(Fverk),
      kva_stromprod_mwh_anlage = as.numeric(Sprod),
      kva_stromabsatz_mwh_anlage = as.numeric(Sverk),
      kva_dampf_mwh_anlage = rowSums(select(df, matches("^Den(L[0-9]*)?$")) %>% mutate_all(as.numeric), na.rm = TRUE),
      kva_abfall_t_anlage = as.numeric(Amenge),
      kva_hu_wert_anlage = as.numeric(Aheizw),
      jahr = as.integer(jahr),
      thema = "KVA"
    ) %>%
    left_join(kva_olddata %>% select(bfsnr, ort, anlage) %>% filter(!is.na(bfsnr)) %>% distinct(), by = "anlage")
})

# Ergebnis anzeigen
print(kva_jaresstand)




kva_jaresstand_final <- kva_jaresstand %>%
  pivot_longer(
    cols = c(kva_waerme_mwh_anlage, kva_stromprod_mwh_anlage, kva_stromabsatz_mwh_anlage, kva_dampf_mwh_anlage, kva_abfall_t_anlage, kva_hu_wert_anlage),
    names_to = "variable",
    values_to = "wert"
  ) %>%
  mutate(
    einheit = case_when(
      variable == "kva_waerme_mwh_anlage"   ~ "MWh",
      variable == "kva_stromprod_mwh_anlage"  ~ "MWh",
      variable == "kva_stromabsatz_mwh_anlage"    ~ "MWh",
      variable == "kva_dampf_mwh_anlage"    ~ "MWh",
      variable == "kva_abfall_t_anlage"    ~ "t",
      variable == "kva_hu_wert_anlage"    ~ "Hu"
    ),
    subthema = case_when(
      variable == "kva_waerme_mwh_anlage"   ~ "Wärme",
      variable == "kva_stromprod_mwh_anlage"  ~ "Stromproduktion",
      variable == "kva_stromabsatz_mwh_anlage"    ~ "Stromabsatz",
      variable == "kva_dampf_mwh_anlage"    ~ "Dampfproduktion",
      variable == "kva_abfall_t_anlage"    ~ "Abfallmenge",
      variable == "kva_hu_wert_anlage"    ~ "Heizwert Hu"
    ),
    rubrik = case_when(
      variable == "kva_waerme_mwh_anlage"   ~ "Wärme",
      variable == "kva_stromprod_mwh_anlage"  ~ "Strom",
      variable == "kva_stromabsatz_mwh_anlage"    ~ "Strom",
      variable == "kva_dampf_mwh_anlage"    ~ "Wärme",
      variable == "kva_abfall_t_anlage"    ~ "Sonstiges",
      variable == "kva_hu_wert_anlage"    ~ "Wärme"
    ),
  ) %>%
  select(jahr, bfsnr, ort, anlage, rubrik, thema, subthema, wert, einheit)



### DATENSÄTZE JAHRESSTAND UND OLDDATA VERBINDEN

kva_final <- bind_rows(kva_olddata, kva_jaresstand_final) %>%
  arrange(jahr) %>%  # Sicherstellen, dass ältere Werte zuerst kommen
  distinct(anlage, jahr, .keep_all = TRUE)  # Behalte nur die neuesten Werte

# Ergebnis anzeigen
print(kva_final)

write_excel_csv(kva_final, here::here("data/output/kva.csv"), delim = ";")


# Plot Beispiel

library(ggplot2)

kva_final %>%
  filter(subthema == "Wärme") %>%
  group_by(jahr) %>%
  summarise(summe_wert = sum(wert, na.rm = TRUE)) %>%
  ggplot(aes(x = jahr, y = summe_wert)) +
  geom_line(color = "steelblue", linewidth = 1) +
  geom_point(color = "darkred", size = 2) +
  labs(
    title = "Wärmeabsatz aller KVA pro Jahr",
    x = "Jahr",
    y = "Wärme [MWh]"
  ) +
  theme_minimal()


