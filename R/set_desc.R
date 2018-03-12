set_desc <- function(text){
  words <- str_extract_all(text, "(?<=descending\\s)[^\\s]+") %>% unlist
  for (w in words){
    text %<>% gsub(paste0("descending ", w), paste0("desc(",w,")"), .)
  }

  return(text)
}
