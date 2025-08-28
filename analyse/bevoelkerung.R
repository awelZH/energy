#### BEVÖLKERUNG KANTON ZH & CH ####

# Import und Verarbeitung

library(tidyverse)

#Kanton
bev_zh_input <- read_csv("https://www.web.statistik.zh.ch/ogd/data/KANTON_ZUERICH_133.csv", show_col_types = F) 

bevoelkerung_gem <- bev_zh_input %>%
  filter(GEBIET_NAME != "Zürich - ganzer Kanton", !grepl("^(Region|Bezirk)", GEBIET_NAME)) %>%
  select(Gebiet = GEBIET_NAME, Jahr = INDIKATOR_JAHR, Anzahl = INDIKATOR_VALUE)

bevoelkerung_zh <- bev_zh_input %>%
  filter(GEBIET_NAME == "Zürich - ganzer Kanton") %>%
  select(gebiet = GEBIET_NAME, jahr = INDIKATOR_JAHR, anzahl = INDIKATOR_VALUE) %>%
  mutate(gebiet = "Kanton")


#Schweiz
bfs.url <- "https://www.pxweb.bfs.admin.ch/DownloadFile.aspx?file=px-x-0102020000_101"
tmpfile <- tempfile(fileext = ".px")

# Datei herunterladen
download.file(bfs.url, tmpfile, mode = "wb")

# PX-Datei einlesen
bev <- pxR::read.px(tmpfile)

# PX-Objekt in DataFrame umwandeln
bev_ch <- as.data.frame(bev)

bevoelkerung_ch <- bev_ch %>%
  filter(
    Demografische.Komponente == "Bestand am 31. Dezember",
    Geschlecht == "Geschlecht - Total",
    Staatsangehörigkeit..Kategorie. == "Staatsangehörigkeit (Kategorie) - Total",
    Kanton == "Schweiz") %>%
  mutate(jahr = as.numeric(as.character(Jahr))) %>%
  select(gebiet = Kanton, anzahl = value, jahr)


### FINALER DATENSATZ ###

bevoelkerung_final <- bevoelkerung_ch %>%
  left_join(bevoelkerung_zh, by = "jahr") %>%
  select(jahr, bevoelkerung_ch = anzahl.x, bevoelkerung_zh = anzahl.y) %>%
  mutate(verhaeltnis_ch_zh = bevoelkerung_ch/bevoelkerung_zh)
