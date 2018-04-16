cpt_exptime <- function(x, .start, .censor) {
  
  #Sanity checks
  mdsvars <- c("bene_id_18900", "dmdate", "accpt_id", 
               "M3A0310F")
  stopifnot(mdsvars %in% names(x))
  stopifnot(class(.start)=="Date")
  stopifnot(class(.censor)=="Date")
  
  cat("Adding lag variables...")
    x1.1 <- x %>%
      select(mdsvars) %>%
      distinct(.) %>%
      arrange(bene_id_18900, dmdate) %>% #Just in case
      group_by(bene_id_18900) %>% #Within Patient
      mutate(lag_fac = lag(accpt_id),
             lag_target = lag(dmdate),
             lag_rectype = lag(M3A0310F)) %>%
      dplyr::filter((dmdate>=.start)==T & (dmdate<=.censor)==T) %>%
      ungroup() 
  cat("done \n")
  
  cat("Counting up days between assessments...")
  #Count difference in days
    x1.2 <- x1.1 %>%
      group_by(bene_id_18900) %>%
      arrange(bene_id_18900, dmdate) %>% #Just in case
      mutate(
        #Compute difference in days between assessment, within person
        daydiff = if_else(dmdate==first(dmdate), 
                          as.integer(dmdate - .start), 
                          as.integer(dmdate - lag_target)),
        #Days in NH 
        daysin = daydiff,
        daysin = if_else(lag_rectype %in% c(10, 11, 12), 0L, daysin),
        daysin = if_else(dmdate==last(dmdate) & 
                           (M3A0310F %in% c(10,11,12))==F,
                         as.integer(.censor - lag_target), daysin),
        daysin = if_else(dmdate<=.censor & dmdate==last(dmdate) & (M3A0310F %in% c(10,11,12))==F, 
                         daydiff + as.integer(.censor - dmdate), daysin),
        daysin = if_else(dmdate ==  lag_target &
                           (M3A0310F %in% c(10,11,12))==T, 
                         0L, daysin),
        #Days out NH
        daysout = if_else(lag_target %in% c(10, 11, 12), daydiff, 0L),
        daysout = if_else(lag_target %in% c(10, 11, 12) & dmdate>=.censor & lag_target<=.censor,
                          as.integer(.censor - lag_target), 
                          daysout)
      ) %>%
      ungroup()
  cat("done \n")
  
  cat("Summarizing total days in NH...")
  #compute difference between days out and days in
    x1.3 <- x1.2 %>%
      group_by(bene_id_18900) %>%
      mutate(totdaysin = sum(daysin, na.rm=T),
             totdaysout = sum(daysout, na.rm=T),
             exptime = sum(daysin, na.rm=T) - sum(daysout, na.rm=T))
  cat('done \n')  
  
  x2 <- x1.3 %>%
    dplyr::select(bene_id_18900, exptime) %>%
    distinct(.)
  
  #Sanity checks
  if (nrow(x2)!=n_distinct(x2$bene_id_18900)) warning("output not 1 row per patient")
  
  return(x2)
}