#' Convert SAS code into R code
#'
#' Simply convert the SAS code into R code. The R code generated
#' use the following R packages: dplyr, magrittr, readr
#'
#' @param text the SAS code whatever the indentation
#'
#' @return The R code generated
#'
#' @examples
#'
#' sasr("
#' proc append base=dataset data=data_to_append getsort force;
#' run;
#' ")
#'
#' sasr("
#' proc compare base=dataset1 compare=adtaset2 printall;
#' run;
#'
#' proc compare base=dataset1 compare=adtaset2 printall;
#' var var1;
#' with var2;
#' run;
#' ")
#'
#' sasr("
#' proc sort data=dataset out=sorted_dataset;
#' by var1 descending var2;
#' run;
#' ")
#'
#' sasr("
#' data dataset;
#' rename var1=new_var1
#' var2-var3=new_var2-new_var3;
#' run;
#' ")
#'
#' sasr("
#' data new;
#' set old;
#' run;
#' ")
#'
#' sasr("
#' proc print data=sashelp.iris;
#' run;
#' ")
#'
#' sasr("
#' proc MEANS data=sashelp.iris;
#' run;
#' ")
#'
#' sasr("
#' proc import datafile='c:/temp/test.txt'
#' out=dataset
#' dbms=dlm
#' replace;
#' delimiter='09'x;
#' datarow=5;
#' run;
#' ")
#'
#' sasr("
#' proc import datafile='c:/temp/test.csv'
#' out=dataset
#' dbms=csv
#' replace;
#' getnames=Yes;
#' run;
#' ")
#'
#' sasr("
#' proc transpose data=dataset out=dataset_transposed;
#' run;
#' ")
#'
#' sasr("
#' proc reg data=dataset;
#' model y = x1 x2;
#' run;
#' ")
#'
#' sasr("
#' data dataset;
#' merge dataset1 dataset2;
#' by variable;
#' run;
#' ")
#'
#' @importFrom magrittr %>% %<>%
#' @importFrom stringr str_extract_all str_extract str_split str_trim
#' @importFrom glue glue
#' @importFrom stats na.omit
#'
#' @export

sasr <- function(text){
  blocks <- text %>%
    strsplit("run;") %>%
    unlist()

  blocks %<>% .[!grepl("^\\s*\n\\s*$", .)]

  res <- sapply(blocks, sasr_core) %>%
    paste(collapse = '\n\n') %>%
    glue

  return(res)
}

## quiets concerns of R CMD check re: the .'s that appear in pipelines
if(getRversion() >= "2.15.1")  utils::globalVariables(c("."))
