build_proc <- function(text){

  proc_name <- get_proc_name(text)

  switch(
    proc_name,
    "append"   ={res <- build_proc_append(text)},
    "compare"  ={res <- build_proc_compare(text)},
    "sort"     ={res <- build_proc_sort(text)},
    "print"    ={res <- build_proc_print(text)},
    "means"    ={res <- build_proc_means(text)},
    "import"   ={res <- build_proc_import(text)},
    "transpose"={res <- build_proc_transpose(text)},
    "reg"      ={res <- build_proc_reg(text)},
    {stop(glue("proc {proc_name} not yet implemented!"))}
  )

  return(res)
}

get_proc_name <- function(text){
  text[1] %>%
    strsplit("\\s") %>%
    unlist %>%
    .[2] %>%
    tolower
}

build_proc_append <- function(text){
  kvp  <- get_kvp_all(text)

  res  <- glue("{kvp$base} %<>% rbind({kvp$data})")

  options <- c("appendver", "force", "getsort")
  options_mask <- options %>%
    sapply(., function(x){
      grepl(x, text %>% tolower)
    }) %>%
    apply(., 2, any)

  if(any(options_mask)){
    warning(glue("[WARNING] '{options[options_mask] %>% paste0(collapse=', ')}' options are ignored!"))
  }

  return(res)
}

build_proc_compare <- function(text){
  kvp  <- get_kvp_all(text)
  var  <- parse_param(text, "var")
  with <- parse_param(text, "with")

  if(length(var) > 0 & length(with) > 0){
    res  <- glue("all.equal({kvp$base}${var}, {kvp$compare}${with})")
  } else {
    res  <- glue("all.equal({kvp$base}, {kvp$compare})")
  }
  if("out" %in% names(kvp)){
    res <- glue("{kvp$out} <- {res}")
  }

  return(res)
}

build_proc_sort <- function(text){
  kvp <- get_kvp_all(text)
  by  <- parse_param(text, "by")

  res <- glue("{kvp$out} <- {kvp$data} %>% arrange({by})")

  return(res)
}

build_proc_print <- function(text){
  kvp <- get_kvp_all(text)

  res <- glue("View({kvp$data})")

  return(res)
}

build_proc_means <- function(text){
  kvp  <- get_kvp_all(text)
  res  <- glue("summary({kvp$data})")

  return(res)
}

build_proc_import <- function(text){
  kvp <- get_kvp_all(text)

  getnames  <- ifelse(is.null(kvp$getnames), FALSE, ifelse(kvp$getnames %>% tolower == "no", FALSE, TRUE))
  datarow   <- ifelse(is.null(kvp$datarow), Inf, kvp$datarow)

  if(!is.null(kvp$delimiter)){
    switch(kvp$delimiter,
           "'09'x"={
             res <- glue("{kvp$out} <- read_tsv({kvp$datafile}, n_max = {datarow}, col_names = {getnames})")
           },
           {
             stop("[ERROR] Can't parse the SAS proc")
           })
  }
  if(!is.null(kvp$dbms) && kvp$dbms == "csv"){
    res <- glue("{kvp$out} <- read_csv({kvp$datafile}, n_max = {datarow}, col_names = {getnames})")
  }

  return(res)
}

build_proc_transpose <- function(text){
  kvp <- get_kvp_all(text)
  res <- glue("{kvp$out} <- t({kvp$data})")
  return(res)
}

build_proc_reg <- function(text){
  kvp   <- get_kvp_all(text)
  model <- parse_model_param(text)

  res <- glue("lm({model}, data={kvp$data})")

  return(res)
}
