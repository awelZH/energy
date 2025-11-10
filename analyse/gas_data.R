#### Gasverbrauch ####
# Für die Energiestatistik werden auch Angaben zu den Gasverbräuchen benötigt
# Die Daten stammen direkt von den grösseren Energieversorgern im Kanton
# und werden einmal jährlich durch das AWEL (Abteilung LKS) nachgefragt.

# devtools::install_github("Azure/Microsoft365R")

require(tidyverse)
require(here)
require(readxl)
require(httr)
# require(Microsoft365R)

# Gasverbrauch gemäss Lieferung der einzelnen Energieversorger, Input aus Energiestatistik
dat_gas <- read_excel(here("data/input/Kantonale Energiestatistik.xlsx"), 
                      range = "Gas!A3:AD165") |> 
  dplyr::select(-c(3:4,7:9)) |> 
  dplyr::rename(bfs = !!1, gem = !!2, werk = !!3, lieferant = !!4) |>
  #dplyr::rename_with(~ as.character(2004:2024), 5:25)
  dplyr::rename_with(~ paste0("j", 2004:2024), 5:25)

# Gemeindeverzeichnis, Stand 2010-01-01 -> erste Fusionen in ZH ab 
# Fetch data from the API
url_gem <-"https://www.agvchapp.bfs.admin.ch/api/communes/snapshot?date=01-01-2010"
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

# Mutationsmeldungen, Stand ab 2010-01-01 bis aktuell -> erste Fusionen in ZH ab 2013
# Fetch data from the API
url <- "https://www.agvchapp.bfs.admin.ch/api/communes/mutations?includeTerritoryEx-change=true&startPeriod=01-01-2010&endPeriod=02-01-2025"
response <- GET(url)
# Check if the request was successful
if (http_status(response)$category == "Success") {
  # Extract content from the response
  content <- content(response, "text", encoding = "UTF-8")
  # Gemeindeverzeichnis
  gem_mut <- read_delim(content, delim = ",")
} else {
  # Handle the error
  print("Error: Unable to fetch data from the provided URL.")
}

# Subset gem_set_akt, nur Gemeinden (level == 3) Kanton ZH (Identifier < 300) mit leerem Bis (is.na(ValidTo))
gem_zh <- gem_set |> 
  # filter (BfsCode < 300 & Level == 3 & is.na(ValidTo)) |>
  filter (BfsCode < 300 & Level == 3) |> 
  select(bfshist = BfsCode, gem = Name) |> 
  arrange(bfshist)

# Subset gem_set_2000, nur Gemeinden (level == 3) Kanton ZH (Identifier < 300) mit leerem Bis (is.na(ValidTo))
gem_mut_zh <- gem_mut |> 
  filter (InitialCode < 300) |> 
  select(bfshist = InitialCode, bfsakt = TerminalCode, gemakt = TerminalName) |> 
  arrange(bfshist)

# Ergänzung und relocate BFS-Nummer
gas_zh <- dat_gas |> 
  left_join(gem_mut_zh, by = join_by(bfs == bfshist)) |> 
  relocate(bfsakt, gemakt, .before = werk) |> 
  mutate(
    bfsakt = ifelse(is.na(bfsakt), bfs, bfsakt),
    gemakt = ifelse(is.na(gemakt), gem, gemakt)
  ) |> 
  arrange(bfs)

# Allfällige Gemeindefusionen werden berücksichtigt
gas_zh_long <- gas_zh |> 
  select(-c(bfs,gem,werk,lieferant)) |> 
  group_by(bfsakt,gemakt) |> 
  summarise(across(j2004:j2024, \(x) sum(x, na.rm = TRUE))) |> 
  pivot_longer(!c(bfsakt,gemakt), names_to = "jahr", values_to = "wert") |> 
  mutate(jahr = gsub("^j", "", jahr),  jahr = as.numeric(jahr), 
         wert = round(wert,0),
         einheit = "MWh")

gas_zh_wide <- gas_zh_long |> 
  pivot_wider(id_cols = c(bfsakt, gemakt, einheit), names_from = jahr, values_from = wert)

# # Export der *.csv files
# # Gemeinden ZH
write_delim(gas_zh_long, here("data/output/gas_inp_gem.csv"), delim = ",")
# # Gemeinden ZH - Pivot wide
write_delim(gas_zh_wide, here("data/output/gas_inp_gem_wider.csv"), delim = ",")



