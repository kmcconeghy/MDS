## ---- echo = FALSE, message = FALSE--------------------------------------
knitr::opts_chunk$set(collapse = T, comment = "#>")
options(tibble.print_min = 4L, tibble.print_max = 4L)
library(mdsR)
library(tidyverse)

## ------------------------------------------------------------------------
not_mds_dta <- cars
mds_as_canon(not_mds_dta)

## ------------------------------------------------------------------------
mds_dta <- mdsR::mds_dta  
mds_dta <- mds_as_canon(mds_dta)

## ------------------------------------------------------------------------
mds_is_canon(mds_dta)

## ------------------------------------------------------------------------
mds_flagmiss(mds_dta)

