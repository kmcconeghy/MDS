#' @title SuccessfulDC: Find those who are discharged to community
#'
#' @description Queries a standardized MDS cohort dataset to identify those who
#' were discharged to community
#'
#' @usage SuccessfulDC <- function(x, .preindex, .postindex)
#'
#' @param x A dataframe class object, must contain
#' "bene_id_18900", "dmdate", "dmatype", "M3A2100"
#'
#' @param .preindex Date class object of length 1, start window to identify
#'
#' @param .postindex Date class object of length 1, stop window to identify
#'
#' @return an object of class data.frame, limited to those without discharge
#' to community in window
#'
#' @export
#'
#' @examples
#' require(mdsR)
#' mds_dta <- mdsR::mds_cohort
#' start <- lubridate::ymd('2017-01-01')
#' stop <- lubridate::ymd('2018-01-01')
#' mds_dta2 <- SuccessfulDC(mds_dta, start, stop)
#'
#' @references

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
