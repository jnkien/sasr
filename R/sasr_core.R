sasr_core <- function(text){

  text %<>%
    gsub("\n", " ", .) %>%
    gsub("\\s+", " ", .) %>%
    strsplit(";") %>%
    unlist %>%
    str_trim

  sta_type <- get_sta_type(text)

  switch(
    sta_type,
    "data"={res <- build_sta(text)},
    "proc"={res <- build_proc(text)},
    {stop(glue("Statement {sta_type} not yet implemented!"))}
  )

  return(res)
}

get_sta_type  <- function(text){
  text[1] %>%
    strsplit("\\s") %>%
    unlist %>%
    .[1] %>%
    tolower
}
