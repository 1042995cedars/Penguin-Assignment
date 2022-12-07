cleaning <- function(data_raw){
  data_raw %>%
    clean_names() %>%
    remove_empty(c("rows", "cols")) %>%
    select(-starts_with("delta")) %>%
    select(-comments)
}

remove_empty_flipper_length <- function(data_clean){
  data_clean %>%
    filter(!is.na(flipper_length_mm)) %>%
    select(species, flipper_length_mm)
}

remove_empty_culmen_length <- function(data_clean){
  data_clean %>%
    filter(!is.na(culmen_length_mm)) %>%
    select(species, flipper_length_mm)
}
