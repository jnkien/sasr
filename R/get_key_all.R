get_key_all <- function(text) str_extract_all(text, "(?<=\\s)[^\\s]+(?==)") %>% unlist
