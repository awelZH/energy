#### FUNKTIONEN ####
library(fuzzyjoin)

join_gemeinden <- function(input_df, gemeinden_df, input_col = "Gemeinde", gemeinde_col = "gemeinde", max_dist = 0.1) {
  # 1. Gemeindespalten bereinigen
  input_clean <- input_df %>%
    mutate(gemeinde_clean = str_to_lower(.data[[input_col]])) %>%
    mutate(gemeinde_clean = str_remove_all(str_trim(.data[["gemeinde_clean"]]), "\\s*\\(zh\\)"))
  
  gemeinden_clean <- gemeinden_df %>%
    mutate(gemeinde_clean = str_to_lower(.data[[gemeinde_col]])) %>%
    mutate(gemeinde_clean = str_remove_all(str_trim(.data[["gemeinde_clean"]]), "\\s*\\(zh\\)"))
  
  # 2. Exakter Join
  joined_exact <- input_clean %>%
    left_join(gemeinden_clean, by = "gemeinde_clean")
  
  # 3. Fuzzy-Matching für nicht zugeordnete Gemeinden
  to_fuzzy_match <- joined_exact %>%
    filter(is.na(.data[[gemeinde_col]]))
  
  to_fuzzy_match_input <- to_fuzzy_match %>%
    select(all_of(input_col), gemeinde_clean)
  
  fuzzy_matched <- stringdist_left_join(
    to_fuzzy_match_input,
    gemeinden_clean,
    by = "gemeinde_clean",
    method = "jw",
    max_dist = max_dist,
    distance_col = "distanz"
  )
  
  # 4. Kombination beider Ergebnisse
  final_matched <- joined_exact %>%
    filter(!is.na(.data[[gemeinde_col]])) %>%
    bind_rows(fuzzy_matched)
  
  return(final_matched)
}

