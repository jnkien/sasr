parse_model_param <- function(text){
  model <- text[grepl("model", text)] %>%
    str_extract("(?<=model\\s).+") %>%
    gsub("\\s?=\\s?", "~", .) %>%
    gsub("\\s(?!$)", "+", ., perl = T)

  return(model)
}
