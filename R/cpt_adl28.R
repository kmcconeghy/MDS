#' @title Compute ADL 28-point score
#'
#' @description Scans MDS dataframe for 'std' named ADL items and computes
#' ADL score for each assessment.
#'
#' @usage cpt_adl28(x, report=T)
#'
#' @param x A data.frame class object, with M3G0110 items.
#'
#' @param report Generates a short report on computed ADL score
#'
#' @return A dataframe with added column adl_score
#'
#' @export
#'
#' @examples
#' require(mdsR)
#' mds_dta <- mdsR::mds_cohort
#' cpt_adl28(mds_dta)
#'
#' @references ADD LATER
#'
cpt_adl28 <- function(x, report=T) {

  stopifnot(any(class(x)=="data.frame"))

  adlvars <- c("M3G0110A1", "M3G0110B1", "M3G0110E1", "M3G0110G1", "M3G0110H1",
               "M3G0110I1", "M3G0110J1")

  stopifnot(adlvars %in% names(x))

  df_return <- x %>%
    dplyr::mutate(adl_bed 		= M3G0110A1,
           adl_transfer 	= M3G0110B1,
           adl_moving 		= M3G0110E1,
           adl_dress 		= M3G0110G1,
           adl_eating 		= M3G0110H1,
           adl_toilet 		= M3G0110I1,
           adl_hygiene 	= M3G0110J1) %>% # Recode 7,8 to 4
    dplyr::mutate(adl_bed 		= if_else(adl_bed %in% c(7,8), 4, adl_bed),
           adl_transfer 	= if_else(adl_transfer %in% c(7,8), 4, adl_transfer),
           adl_moving 		= if_else(adl_moving %in% c(7,8), 4, adl_moving),
           adl_dress 		= if_else(adl_dress %in% c(7,8), 4, adl_dress),
           adl_eating 		= if_else(adl_eating %in% c(7,8), 4, adl_eating),
           adl_toilet 		= if_else(adl_toilet %in% c(7,8), 4, adl_toilet),
           adl_hygiene 	= if_else(adl_hygiene %in% c(7,8), 4, adl_hygiene)) %>%
    mutate(adl_score = rowSums(select(., starts_with('adl_'))))

  #Report
  cat("ADL-scoring complete \n")
  if (report==T) {
    cat("ADL report", "\n",
        "Minimum ADL score: ", min(df_return$adl_score, na.rm=T),
        "Maximum ADL score: ", max(df_return$adl_score, na.rm=T),
        "Median value: ", median(df_return$adl_score, na.rm=T),
        "No. missing: ", sum(is.na(df_return$adl_score)==T)
    )
  }

  #Sanity checks
  if(max(df_return$adl_score, na.rm=T)>28) { warning("ADL score exceeds 28 points, confirm
                                            input variables are correct")}
  if(min(df_return$adl_score, na.rm=T)<0) { warning("ADL score < 0 points, confirm
                                            input variables are correct")}
  return(df_return)
}
