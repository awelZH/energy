library(pxR)
library(tidyverse)
library(here)
require(httr)

bfs.url <- "https://www.pxweb.bfs.admin.ch/DownloadFile.aspx?"

# read px-file: Demografische Bilanz nach institutionellen Gliederungen
# https://www.bfs.admin.ch/asset/de/px-x-0102020000_201
# Datenquelle: 1981-2010 Statistik des jährlichen Bevölkerungsstandes (ESPOP), 
# ab 2011 Statistik der Bevölkerung und der Haushalte (STATPOP)
# Hinweis: Die Daten weisen den Gemeindestand per 31. Dezember des letzten produzierten Jahres aus. 
# Die nicht mehr bestehenden Thurgauer Gemeinden 4505 Neukirch an der Thur, 4670 Illighausen und 4695 Scherzingen 
# werden immer noch ausgewiesen, da eine rückwirkende Verteilung der Wohnbevölkerung für die Jahre vor 1994 nicht möglich war.
bev.px <- read.px(paste0(bfs.url, "file=px-x-0102020000_201"), encoding="utf-8")

# save as data frame:
bev <- as_tibble(bev.px)

# Inhalte einer bestimmten Variable 
dk <- levels(bev$Demografische.Komponente)
gk <- levels(bev$Geschlecht)
sk <- levels(bev$Staatsangehörigkeit..Kategorie.)
kk <- levels(bev$Kanton.......Bezirk........Gemeinde.........)

# Datenzusammenstellung dplyr
# Bevölkerung total pro Jahr und Gemeinde, Bestand am 31. Dezember, alle Gemeinden CH, Gemeindestand aktuell
bev_mod <- bev  |> 
  # filter(Kanton == "Zürich" & Beobachtungseinheit == "Tiere - Milchkühe" & Betriebsform == "Betriebsform - Total" & Betriebssystem == "Betriebssystem - Total" & Landwirtschaftliche.Produktionszone == "Landwirtschaftliche Produktionszone - Total") %>%
  filter(Demografische.Komponente ==  "Bestand am 31. Dezember" & Geschlecht == "Geschlecht - Total" & Staatsangehörigkeit..Kategorie. == "Staatsangehörigkeit (Kategorie) - Total") |> 
  select(gebiet = "Kanton.......Bezirk........Gemeinde.........", Jahr, value) |> 
  filter(grepl("\\d", gebiet)) |>  # Behalte nur Zeilen mit Zahlen
  mutate(gebiet = gsub("^\\.\\.{5}", "", gebiet)) |> 
  mutate(
    bfs = as.integer(substr(gebiet, 1, 4)),  # Extrahiere die ersten 4 Zeichen als BFS-Nummer
    gem = trimws(substr(gebiet, 5, nchar(gebiet)))  # Extrahiere den Rest als Gemeindename
  ) |> 
  select(bfs,gem,jahr = Jahr, value)

# Ergänzung Kanton als zusätzliches Attribut
# Gemeindeverzeichnis, Stand 2010-01-01 -> erste Fusionen in ZH ab 
# Fetch data from the API
url_gem <-"https://www.agvchapp.bfs.admin.ch/api/communes/levels?date=01-01-2023"
response <- GET(url_gem)
# Check if the request was successful
if (http_status(response)$category == "Success") {
  # Extract content from the response
  content <- content(response, "text", encoding = "UTF-8")
  # Gemeindeverzeichnis
  gem_set <- read_delim(content, delim = ",")
} else {
  # Handle the error
  print("Error: Unable to fetch data from the provided URL.")
}

gem_kt <- gem_set |> 
  select(bfs= BfsCode, name = Name, ktid = CantonId, kt = Canton) |> 
  arrange(bfs)

bev_gem <- bev_mod |> 
  dplyr::left_join(gem_kt) |> 
  dplyr::mutate(
    ktid = ifelse(bfs %in% c(4505, 4670, 4695), 20, ktid),
    kt = ifelse(bfs %in% c(4505, 4670, 4695), "Thurgau", kt)
  ) |> 
  dplyr::select(-c(name)) |> 
  dplyr::select(bfs, gem, ktid, kt, jahr, value) |> 
  dplyr::mutate(komponente = "Bestand am 31. Dezember")

bev_ch <- bev_gem |> 
  group_by(jahr) |> 
  summarise(bevch = sum(value))

bev_kt <- bev_gem |> 
  group_by(jahr, ktid, kt) |> 
  summarise(bevkt = sum(value))

# Export Bevoelkerungsbestand am 31.12.yyyy, alle Gemeinden CH, ab 1981 
write_delim(bev_gem, here("data/input/bev_gem.csv"), delim = ",")
