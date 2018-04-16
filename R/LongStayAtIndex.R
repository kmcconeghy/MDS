LongStayAtIndex <- function(x, .preindex, .postindex) {
  
  mdsvars <- c("bene_id_18900", "dmdate", "accpt_id", 
               "dmatype")
  
  stopifnot(mdsvars %in% names(x))
  stopifnot(class(.preindex)=="Date")
  stopifnot(class(.postindex)=="Date")
  
  x2 <- x %>%
    dplyr::filter(dmdate %within% interval(.preindex, .postindex)) %>%
    dplyr::mutate(ls = if_else(str_detect(dmatype, '^5Q') | str_detect(dmatype, '^6Y'),
                        T, F)) %>%
    dplyr::filter(ls==T) %>%
    distinct(bene_id_18900, ls) 
  
  if(nrow(x2)!=n_distinct(x2$bene_id_18900)) { warning("duplicate bene_id's in output")}
  
  return(x2)
}