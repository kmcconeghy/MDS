#
# File to Generate a random MDS example dataset for work outside
# of Brown Network
#
# Kevin W. McConeghy
# 05.29.2018

require('tidyverse')

# General Workflow
# 1. Select a random sample of 1000 people
# 2. Keep only assessments for two years
# 3. Replace person-ids with random, facility-ids with random
# 4. Shift Dates randomly

# Load up actual MDS data
## First 100000 records from all_mds3
setwd("P:/flun1/k1m/fake_mds")
real_mds <- haven::read_sas("mds_sample.sas7bdat")

## 1. Random sample
## Sampled by person all mds assessments for 2 years
mds_sample <- distinct(real_mds, bene_id_18900) %>%
  sample_n(., 1000) %>% #keep 1000 unique people
  inner_join(., real_mds, by=c('bene_id_18900'='bene_id_18900')) %>% #join back to pull only thier mds
  arrange(., bene_id_18900, dmdate) %>%
  group_by(bene_id_18900) %>%
  #2. keep all assessments up to two years past
  dplyr::filter(dmdate-last(dmdate)<=730) %>%
  ungroup()


## functions for generating random data
## Generate random repid
make_repid <- function() {
  state_samp <- sample(state.abb, 1)
  int_samp <- str_pad(sample(1:99999, 1),
                      width=10,
                      side='left',
                      pad='0')
  y <- paste0(state_samp,'-',int_samp)
  return(y)
}

## Generate random bene_id
make_beneid <- function() {
  y <- paste0('jjjjjjj',
              stringi::stri_rand_strings(1, 9, pattern='[a-z0-9]'))
  return(y)
}

## Make random facility id
make_accptid <- function() {
  y <- as.character(sample(1:999999, 1))
  return(y)
}

##make random provider id from repid
make_provid <- function(x) {
  state_abb <- str_extract(x, pattern="^.{2}")
  int_samp <- str_pad(sample(1:16000, 1),
                      width=10,
                      side='left',
                      pad='0')
  y <- paste0(state_abb,'-',int_samp)
  return(y)
}


## 3. Replace identifiable data with random

## drop dmpers
mds_sample_2 <- mds_sample %>%
  select(-starts_with('dmpers'))

## Random identifiers
### If Unlinked - 13 characters, 2 letters, -, 10 numbers
### If linked - 16 characters, 7 jsm followed by random sequence of [a-z0-9]
#### Make list of random bene_ids
mds_sample_3 <- mds_sample_2 %>%
  group_by(bene_id_18900) %>%
  mutate(DMREPID_rand = make_repid(),
         bene_id_rand = make_beneid()) %>%
  ungroup() %>%
  mutate(DMREPID = DMREPID_rand,
         bene_id_18900 = bene_id_rand) %>%
  select(-ends_with('_rand'))

## Random facilities
mds_sample_4 <- mds_sample_3 %>%
  group_by(accpt_id) %>%
  mutate(accpt_id_rand = make_accptid(),
         DMREPPROV = make_provid(DMREPID),
         M3A0100A = stringi::stri_rand_strings(1, 10, "[0-9]"),
         M3A0100B = accpt_id_rand,
         M3A0100C = "") %>%
  ungroup() %>%
  mutate(accpt_id = accpt_id_rand,
         mmxxid = accpt_id,
         mmxxidp = accpt_id,
         PROV1680_0 = accpt_id,
         PROV1680_1 = accpt_id) %>%
  select(-ends_with('_rand'))


## Random dmrecid
### sequential within person-facility, by date
mds_sample_5 <- mds_sample_4 %>%
  group_by(bene_id_18900, accpt_id) %>%
  mutate(DMRECID = paste0(str_extract(DMREPID, pattern="^.{2}"),
                          '-',
                          str_pad(row_number(),
                                  width=10,
                                  side='left',
                                  pad='0'))) %>%
  ungroup()

#4. Shift Dates
## Shift assessment dates
mds_sample_6 <- mds_sample_5 %>%
  rowwise() %>%
  mutate(DMASMDT = DMASMDT + sample(-1:1, 1),
         dmdate = DMASMDT) %>%
  ungroup()

## Shift birthdate / discharge date / admit date
shift_date <- function(x, x_wall, wall='min') {

  x1 <- c(as.integer(x + sample(-1:1,1)), as.integer(x_wall))

  if (wall=='min') {
    new_date <- lubridate::as_date(min(x1))
  } else if (wall=='max') {
    new_date <-  lubridate::as_date(max(x1))
  }

  return(new_date)
}

mds_sample_7 <- mds_sample_6 %>%
  rowwise() %>%
  mutate(M3A0900 = shift_date(M3A0900, dmdate, 'min'),
         dmdob = M3A0900,
         M3A2000 = shift_date(M3A2000, dmdate, 'max')) %>%
  ungroup() %>%
  group_by(bene_id_18900, M3A1600) %>% #Admit Date
  mutate(M3A1600_rand = shift_date(M3A1600, dmdate, 'min')) %>%
  ungroup() %>%
  mutate(M3A1600 = M3A1600_rand,
         dmadmit = M3A1600,
         M3A1900 = if_else(is.na(M3A1900), M3A1900, M3A1600)) %>%
  rowwise() %>%
  mutate(M3A2200 = if_else(!is.na(M3A2200),
                           shift_date(M3A2200, dmdate, 'min'),
                           M3A2200),
         M3A2300 = if_else(!is.na(M3A2300),
                           shift_date(M3A2300, dmdate, 'min'),
                           M3A2300),
         M3A2400B = if_else(!is.na(M3A2400B),
                            M3A2400B + sample(-1:1, 1),
                            M3A2400B),
         M3A2400C = if_else(!is.na(M3A2400C),
                            M3A2400C + sample(-1:1, 1),
                            M3A2400C),
         M3V0200B2 = if_else(is.na(M3V0200B2),
                             shift_date(M3V0200B2, dmdate, 'max'),
                             M3V0200B2),
         M3V0200C2 = if_else(is.na(M3V0200C2),
                             shift_date(M3V0200C2, dmdate, 'max'),
                             M3V0200C2),
         M3Z0500B = if_else(is.na(M3Z0500B),
                            shift_date(M3Z0500B, dmdate, 'max'),
                            M3Z0500B),
         PROV2740_0 = if_else(!is.na(PROV2740_0),
                              PROV2740_0 + sample(-1:1, 1),
                              PROV2740_0),
         PROV2740_1 = if_else(!is.na(PROV2740_1),
                              PROV2740_1 + sample(-1:1, 1),
                              PROV2740_1),
  ) %>%
  select(-ends_with('rand'))


#5. Other potential identifiers
mds_sample_8 <- mds_sample_7 %>%
  mutate(M3A1300A = "", #MRN
         M3_STATE_CD = str_extract(DMREPID, pattern="^.{2}"),
         dmstate = M3_STATE_CD) %>%
  select(-starts_with('PROV')) %>%
  select(-starts_with('F0')) %>%
  select(-starts_with('SURV')) %>%
  select(-starts_with('ss')) %>%
  rowwise() %>%
  mutate(M3_SUBM_DT = M3_SUBM_DT + sample(-1:1,1),
         M3_PROCTS = M3_PROCTS + sample(-1:1,1),
         M3_RES_AGE = M3_RES_AGE + sample(-1:1, 1),
         idage = M3_RES_AGE) %>%
  ungroup()
