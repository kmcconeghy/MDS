# File to convert raw mds_varlist into .Rda file

#Set to data-raw wd
  setwd("~/GitHub/MDS/data-raw")

#Input
  mds_varlist <- readr::read_csv('mds_varlist.csv')

#Format
  mds_std <- mds_varlist

#Standard
  devtools::use_data(mds_std, overwrite=T)
