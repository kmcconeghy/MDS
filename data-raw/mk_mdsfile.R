#
# File to Generate a random MDS example dataset for work outside
# of Brown Network
#
# Kevin W. McConeghy
# 05.29.2018

require('tidyverse')
require('mdsR')

## Random identifiers
### If Unlinked - 13 characters, 2 letters, -, 10 numbers
### If linked - 16 characters, 7 jsm followed by random sequence of [a-z0-9]
#### Make list of random bene_ids

mds_personid <- as_tibble()[1:1000, ] %>%
  rowwise() %>%
  mutate(DMREPID = mk_repid(),
         count = sample(1:20, 1)) %>%
  ungroup()

mds_fake <- tibble(DMREPID = rep(mds_personid$DMREPID,
                                     mds_personid$count)) %>%
  group_by(DMREPID) %>%
  mutate(bene_id_18900 = mk_beneid()) %>%
  ungroup()

## Random facilities

### make 100 random facilities
  rand_facs <- NA
  rand_facs[1:100] <- NA
  rand_facs <- sapply(rand_facs, function(x) mk_accptid())

mds_fake_2 <- mds_fake %>%
  group_by(DMREPID) %>%
  mutate(accpt_id = sample(rand_facs, 1),
         DMREPPROV = mk_provid(DMREPID),
         M3A0100A = stringi::stri_rand_strings(1, 10, "[0-9]"),
         M3A0100B = accpt_id,
         M3A0100C = "") %>%
  ungroup() %>%
  mutate(accpt_id = accpt_id,
         mmxxid = accpt_id,
         mmxxidp = accpt_id,
         PROV1680_0 = accpt_id,
         PROV1680_1 = accpt_id) %>%
  select(-ends_with('_rand'))


## Random dmrecid
### sequential within person-facility, by date
mds_fake_3 <- mds_fake_2 %>%
  group_by(bene_id_18900, accpt_id) %>%
  mutate(DMRECID = paste0(str_extract(DMREPID, pattern="^.{2}"),
                          '-',
                          str_pad(row_number(),
                                  width=10,
                                  side='left',
                                  pad='0'))) %>%
  ungroup()

#4. Dates

## Create Random AD / DC dates
  start_dt <- as.integer(ymd('2013-01-01'))
  stop_dt <- as.integer(ymd('2015-12-31'))

  sample_dts <- sample(start_dt:stop_dt, 1000)

## Shift assessment dates
  mds_fake_4 <- mds_fake_3 %>%
    group_by(bene_id_18900) %>%
      mutate(M3A0900 =
               sample(as.integer(ymd('1900-01-01')):as.integer(ymd('1950-12-31')),
                      1),
             M3A0900 = as_date(M3A0900),
             M3A1600 = sample(sample_dts, 1),
             M3A2000 =  M3A1600 + sample(1:1000, 1)) %>%
    ungroup()

  mds_fake_5 <- mds_fake_4 %>%
    group_by(bene_id_18900) %>%
    mutate(DMASMDT = if_else(row_number()==1L, M3A1600, NA_integer_),
           DMASMDT = if_else(row_number()==n(), M3A2000, DMASMDT)) %>%
    ungroup() %>%
    rowwise() %>%
    mutate(DMASMDT = if_else(is.na(DMASMDT),
                             sample(M3A1600: M3A2000, 1),
                             DMASMDT),
           DMASMDT = as_date(DMASMDT),
           dmdate = as_date(DMASMDT),
           M3A1600 = as_date(M3A1600),
           M3A2000 = as_date(M3A2000)) %>%
    arrange(bene_id_18900, DMASMDT)

  mds_dta <- mds_fake_5 %>%
    group_by(bene_id_18900) %>%
    mutate(M3A0800 = sample(1:2, size=1))

  mds_dta <- mds_as_canon(mds_dta, .report=F)

devtools::use_data(mds_dta, overwrite=T)
