#' @title cpt_stay: Flag residents by stay length
#'
#' @description Scans a data.frame of minimum dataset assessments, assigns an
#' indicator according to an arbitrary length of stay. 100 days with no more
#' than 10 days out is a standard "long-stay" definition.
#'
#' @usage cpt_stay(mds_obj,
#'                 .stay_min = 100L,
#'                 .stay_maxout,
#'                 .report=T)
#'
#' @param mds_obj A data.frame class object which is "mds canon".
#' See ?mds_as_canon
#'
#' @param .stay_min A integer of length one. The length of stay to flag.
#' 100 days is the default.
#'
#' @param .stay_maxout A integer of length one. The maximum allowable days
#' outside of nursing home. 10 days is default.
#'
#' @param .keep_all A logical indicates whether to keep all the patients, or
#' if false only those meeting .stay_* rules. Default is false.
#'
#' @param .report A logical indicating a short report on cohort construction.
#' Default is true.
#'
#' @return A dataframe with one row per resident, cohort entry, cohort exit,
#' flag for rules equals T if .stay_* rules met,
#'
#' @export
#'
#' @examples
#' require(mdsR)
#' mds_dta <- mdsR::mds_cohort
#' longstay <- cpt_stay(mds_dta, .keep_all=T)
#'
#' @references ADD LATER
#'
cpt_stay <- function(mds_dta,
                     .stay_min = 100L,
                     .stay_maxout = 10L,
                     .report=T) {

  ## Scoping
    obj_nm <- rlang::quo(mds_obj)
    obj_strnm <- rlang::quo_text(obj_nm)

  ## Check if canon
    if (!mds_is_canon(mds_obj)) stop('"mds_obj" is not canon, see ?mds_as_canon')
    if (.report) {
      cat('Report: ', '\n')
      cat('...beginning stay computations', '\n')
    }

  ## Assessment date checks
    if (.report) {
      cat('Date of reference: ', 'dmdate', '\n')
    }

    mds_obj_2 <- mds_obj %>%
      select(bene_id_18900, dmdate) %>%
      arrange(bene_id_18900, dmdate, M3A0310F)

    if (sum(is.na(mds_obj_2$dmdate))>0) {
      warning('"dmdate" is missing for some assessments')
      }

    if (.report) {
      cat('No. of missing assessments: ', sum(is.na(mds_obj_2$dmdate)), '\n')
      cat('First assessment date: ', min(mds_obj_2$dmdate), '\n')
      cat('Last assessment date: ', max(mds_obj_2$dmdate), '\n')
    }

  ## Reshape stays



  ## Count days in




  ## Count days out



  ## Find those >=.stay_min length



  ## Exclude those <=.stay_maxout



  ## Report on procedure
    if (.report) {



      cat('...computation done', '\n')
    }

}
