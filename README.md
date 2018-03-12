# sasr (in active dev)

[![CRAN status](https://www.r-pkg.org/badges/version/sasr)](https://cran.r-project.org/package=sasr)
[![lifecycle](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://www.tidyverse.org/lifecycle/#maturing)

Convert your SAS code into R scripts

## Motivation

Get rid of SAS software !

I believe people should go open source when they can. This package has the great ambition to convert anything that is related to SAS code into R code.

People that used to code in SAS can use this package to convert their snippets and ease the transition from SAS to R.

There is a lot of work to do and the development will be iterative by implementing features over time.

## Installation

```r
devtools::install_github("jnkien/sasr")
```

## Features

There is still a lot of work to do, the following links recap what has been done and what is missing.

* [list of SAS statements with their status of integration](statements.md)

* [list of SAS procedures with their status of integration](procedures.md)

## Usage

```r
library("sasr")
sasr(<text>)
sasr(<filename>) # not yet implemented
```

## Examples

```r
sasr("
proc sort data=dataset out=sorted_dataset;
by var1 descending var2;
run;
")
```

## Contributing

Use Git flow: create a feature and a PR on the develop branch, you welcome :)

## License

MIT © Jean-Noël Kien
