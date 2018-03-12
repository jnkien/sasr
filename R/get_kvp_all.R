get_kvp_all <- function(text){

  text_collapsed <- paste(text, collapse = " ")

  attrs <- sapply(text_collapsed, get_key_all) %>% na.omit

  res <- list()
  for (attr in attrs){
    res[[attr]] <-  get_value(text_collapsed, attr)
  }
  return(res)
}
