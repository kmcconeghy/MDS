InHomeOnIndex <- function(x, .index_dt, quietly=T) {
  
  mdsvars <- c("bene_id_18900", "dmdate", "accpt_id", 
               "M3A0310F", "M3A2100", "lag_target", "lag_fac",
               "lag_rectype", "dc")
  
  ## Sanity Checks  
  stopifnot(mdsvars %in% names(x))
  stopifnot(class(.index_dt)=="Date")
  
  
if (quietly==F) cat("Applying rules for residence on index date...")
  x2 <- x %>%
    select(mdsvars) %>%
    group_by(bene_id_18900) %>%
    mutate(
      rule1 = if_else(.index_dt %within% interval(lag_target, dmdate)==T, T, F),
      rule2 = if_else(first(dmdate)==.index_dt & dc==F, T, F),
      rule3 = if_else(accpt_id == lag_fac, T, F),
      rule4 = if_else(lag_rectype %in% c(10, 11, 12), NA, T),
      rule5 = if_else(as.integer(dmdate-lag_target)<=120, T, F),
      index = if_else(rule1==T & rule3==T & rule4==T & rule5==T | rule2==T, T, F)) %>%
    ungroup()
if (quietly==F) cat("Done \n")  
  
if (quietly==F) {
    cat('Summary of rules \n',
        '*Identify people present in the NH on index date \n',
        'RULES: (A dataset sorted by bene_id, assessment date) \n',
        '1) DMDATE(Lag 1) < IndexDate <=dmdate \n',
        '2) IF FIRST DMDATE, ON INDEX AND NOT DC \n',
        '3) Facility same between DMDATE and DMDATE(Lag 1) \n', 
        '4) Previous assessment not a discharge \n',
        '5) Difference between current and previous assessment < 120 days) \n')

    x2 %>%
      group_by(bene_id_18900) %>%
      summarize( n_rule1 = sum(rule1, na.rm=T),
                 n_rule2 = sum(rule2, na.rm=T),
                 n_rule3 = sum(rule3, na.rm=T),
                 n_rule4 = sum(rule4, na.rm=T),
                 n_rule5 = sum(rule5, na.rm=T),
                 n_index = sum(index, na.rm=T))
}

  x3 <- x2 %>%
    dplyr::filter(index==T) %>%
    dplyr::distinct(bene_id_18900, index)

  if(nrow(x3)!=n_distinct(x3$bene_id_18900)) { warning("duplicate bene_id's in output \n")}
  if((nrow(x3)/n_distinct(x$bene_id_18900)) < 0.1) { warning("Less than 10% of sample in home on index \n")}
  
  return(x3)
}