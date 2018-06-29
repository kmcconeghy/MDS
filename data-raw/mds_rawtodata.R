library(tidyverse)
#Set to data-raw wd
setwd("~/GitHub/mdsR/data-raw")

# Standard mds variable list into .Rda file
  #Input

# File to convert raw mds_varlist into .Rda file
  #Input
    mds_varlist <- readxl::read_xlsx('mdsR_dict.xlsx')

  #Standard
    devtools::use_data(mds_varlist, overwrite=T)


