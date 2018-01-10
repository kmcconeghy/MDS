library(tidyverse)

# Standard mds variable list into .Rda file
  #Input
    mds_stdpull <- readr::read_tsv('mds_stdpull.csv', col_names=F) %>%
    rename(Var = X1)

# File to convert raw mds_varlist into .Rda file

  #Set to data-raw wd
    setwd("~/GitHub/MDS/data-raw")

  #Input
    mds_varlist <- readxl::read_xlsx('mds_vardict.xlsx')

  #Format
    mds_std <- mds_varlist %>%
      select(Var, Label, Type, Length, Format, Description) %>%
      inner_join(., mds_stdpull, by=c('Var'))

  #Standard
    devtools::use_data(mds_std, overwrite=T)


