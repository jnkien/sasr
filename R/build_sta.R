build_sta <- function(text){

  sta_name <- get_sta_name(text)

  switch(
    sta_name,
    "rename" ={res <- build_sta_rename(text)},
    "set"    ={res <- build_sta_set(text)},
    {stop(glue("{sta_name} data statement not yet implemented!"))}
  )

  return(res)
}

get_sta_name <- function(text) str_extract(text[2], "(?<=^)[^\\s]+")

build_sta_rename <- function(text){

  dataset <- get_value(text = text, key = "data", sep = ' ')
  kvp     <- get_kvp_all(text)

  clean_kvp <- list()
  for (i in 1:length(kvp)){
    if(grepl("-", names(kvp[i]))){
      a <- str_split(names(kvp[i]), '-') %>% unlist
      v <- str_split(kvp[[i]], '-') %>% unlist
      for(j in 1:length(a)){
        clean_kvp[[a[j]]] <- v[j]
      }
    } else {
      clean_kvp %<>% c(kvp[i])
    }
  }
  rm(kvp)

  inner_rename <- clean_kvp %>%
    unlist %>%
    names %>%
    paste(clean_kvp %>% unlist, ., sep = '=') %>%
    paste0(collapse = ",")

  res <- glue("{dataset} %<>% rename({inner_rename})")

  return(res)
}

build_sta_set <- function(text){

  dataset     <- get_value(text = text, key = "data", sep = ' ')
  data_to_set <- get_value(text = text, key = "set", sep = ' ')

  res <- glue("{dataset} <- {data_to_set}")

  return(res)
}
