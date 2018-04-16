cpt_cps <- function(x, report=T) {
  
  stopifnot(any(class(x)=="data.frame"))
  
  cpsvars <- c("M3C0500", "M3B0100", "M3B0700", "M3C0700", "M3C1000")
  
  stopifnot(cpsvars %in% names(x))
  
  #M3B0100 - Comatose, c(0,1)
  #M3B0700 - Ability to express oneself, values c(0,1,2,3)
  #M3C0500 - BIMS summary score, 0-15, 99-missing
  #M3C0700 - Short term memory okay, values c(0,1)
  #M3C1000 - Made decisions re: tasks of daily life, c(0,1,2,3)

  #Rule 1 = If Patient comatose or cant make decisions
  #Rule 2 = If Patient cant make decisions
  #Rule 3 = If patient has 1 or 2 deficiencies

## Kali's code for defining cognitively impaired (R-adapted)
df_return <- x %>%  
  mutate(bims = suppressWarnings(as.integer(M3C0500)),
         bims = if_else(M3C0500==99L, NA_integer_, bims),
         M3B0100 = suppressWarnings(as.integer(M3B0100)),
         M3B0700 = suppressWarnings(as.integer(M3B0700)),
         M3C0700 = suppressWarnings(as.integer(M3C0700)),
         M3C1000 = suppressWarnings(as.integer(M3C1000)),
         probct1 = 0L,
         probct1 = if_else(!(M3C1000 %in% c(0L, 1L, 2L, 3L) & 
                               M3B0700 %in% c(0L, 1L, 2L, 3L) & 
                               M3C0700 %in% c(0L, 1L)), NA_integer_, probct1),
         probct2 = 0L,
         probct2 = if_else(!(M3C1000 %in% c(0L, 1L, 2L, 3L) & 
                               M3B0700 %in% c(0L, 1L, 2L, 3L) & 
                               M3C0700 %in% c(0L, 1L)), NA_integer_, probct2),
         probct1 = if_else(!is.na(probct1) & M3C1000 %in% c(1L, 2L), 
                           probct1+1L, probct1),
         probct1 = if_else(!is.na(probct1) & M3B0700 %in% c(1L, 2L, 3L), 
                           probct1+1L, probct1),
         probct1 = if_else(!is.na(probct1) & M3C0700==1L, 
                           probct1+1L, probct1),
         probct2 = if_else(!is.na(probct2) & M3C1000==2L, 
                           probct2+1L, probct2),
         probct2 = if_else(!is.na(probct2) & M3B0700 %in% c(2L,3L), 
                           probct2+1L, probct2),
         rule1 = if_else(M3B0100 %in% c(1L) & !(M3C1000 %in% c(0L, 1L, 2L)), T, F),
         rule2 = if_else(M3C1000 %in% c(3L), T, F),
         rule3 = if_else(probct1 %in% c(2L, 3L), T, F),
         cps = NA_integer_, 
         cps = if_else(rule1==T, 6L, cps),
         cps = if_else(is.na(cps) & rule1==F & rule2==T & adl_eating==4, 6L, cps),
         cps = if_else(is.na(cps) & rule1==F & rule2==T & adl_eating<4, 5L, cps),
         cps = if_else(is.na(cps) & rule1==F & rule3==T & probct2==2L, 4L, cps),
         cps = if_else(is.na(cps) & rule1==F & rule3==T & probct2==1L, 3L, cps),
         cps = if_else(is.na(cps) & rule1==F & rule3==T & probct2 ==0L, 2L, cps),
         cps = if_else(rule1==F & rule2==T & is.na(adl_eating), NA_integer_, cps),
         cps = if_else(is.na(cps) & rule1==F & probct1==1L, 1L, cps),
         cps = if_else(is.na(cps) & rule1==F & probct1==0L, 0L, cps)) %>%
  select(-starts_with("probct")) %>%
  select(-starts_with("rule"))

  #Report
  cat("Cognitive function status complete \n")
  if (report==T) {
    cat("CFS report", "\n")
    print(summary(df_return$cps))
  }

  #Sanity checks
  if(max(df_return$cps, na.rm=T)>6L) { warning("CFS score exceeds 6 points, confirm
                                            input variables are correct")}
  if(min(df_return$cps, na.rm=T)<0L) { warning("CFS score < 0 points, confirm
                                            input variables are correct")}
return(df_return)
}