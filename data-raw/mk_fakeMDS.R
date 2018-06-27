#
# File to Generate a random MDS example dataset for work outside
# of Brown Network
#
# Kevin W. McConeghy
# 05.29.2018  

require('tidyverse')

# General Workflow

# Load up actual MDS data from Sanofi Burdens project
## This is a broad sample of any NH resident from 2013-2015
real_mds <- feather::read_feather("P:/sainf/k1m/sainf_inc/data/sainf_incchrt_mds.feather")

## 1. Random sample
## Sampled by person all mds assessments for 2 years  
  mds_sample <- distinct(bene_id_18900) %>%
    sample_n(., 1000) %>% #keep 1000 unique people
    inner_join(., real_mds, by=c('bene_id_18900'='bene_id_18900')) %>% #join back to pull only thier mds
    arrange(bene_id_18900, dmdate) %>%
    filter_if(dmdate-last(dmdate)<=730) #keep all assessments up to two years past

 rm(real_mds) #big file
 
## 2. Replace identifiable data with random
## drop dmpers  
 
## Random bene_id
  
## Random dmrecid
  
## Random facility_id
