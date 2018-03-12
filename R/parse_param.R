parse_param <- function(text, param){
  res <- text[grepl(paste0("^", param), text)] %>%
    str_extract(paste0("(?<=", param, "\\s).+"))

  if(param == "by"){res %<>% set_desc}

  res %<>% gsub("\\s", ",", .)

  return(res)
}
