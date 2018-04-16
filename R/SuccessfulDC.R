SuccessfulDC <- function(x, .preindex, .postindex) {
  
  mdsvars <- c("bene_id_18900", "dmdate", "dmatype", "M3A2100")
  
  stopifnot(mdsvars %in% names(x))
  stopifnot(class(.preindex)=="Date")
  stopifnot(class(.postindex)=="Date")
  
  success_types <- c('Community', 'Rehab', 'Hospice', 'Deceased', 'Other')
  
  x2 <- x %>%
    dplyr::filter(dmdate %within% interval(.preindex, .postindex)) %>%
    mutate(dc_qual = if_else(M3A2100 %in% success_types, T, F)) %>%
    group_by(bene_id_18900) %>%
    dplyr::filter(!any(dc_qual==T)) %>%
    distinct(bene_id_18900, dc_qual) 
  
  if(nrow(x2)!=n_distinct(x2$bene_id_18900)) { warning("duplicate bene_id's in output")}
    
  return(x2)
}