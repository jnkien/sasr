get_value <- function(text, key, sep = '='){
  str_extract(text, paste0("(?<=", key, "\\s?", sep, "\\s?)[^\\s]+")) %>%
    na.omit %>%
    as.vector
}
