get_value <- function(text, key, sep = '='){
  str_extract_all(text, paste0("(?<=", key, "\\s?", sep, "\\s?).+")) %>%
    unlist
}
