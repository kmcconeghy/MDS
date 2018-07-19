#' @title mds_fmt: Format an MDS file
#'
#' @description Runs a series of functions to reformat and edit a file to make
#' final analytical file
#'
#' @usage mds_fmt(mds_obj, .quietly=F)
#'
#' @param mds_obj A data.frame class object with MDS items.
#'
#' @param .quietly If FALSE, will print some output to console
#'
#' @return A tibble class dataframe
#'
#' @export
#'
#' @examples
#' require(mdsR)
#' mds_dta <- mdsR::mds_dta
#' mds_fmt(mds_dta)
#'
mds_fmt <- function(mds_obj, .quietly=F) {

  #Sanity check
    stopifnot(mds_is_canon(mds_obj)==T)

  #Formatting
    cat('formatting variables...')

  ## Gender formatting
    if ('M3A0800' %in% names(mds_obj)) {
      mds_obj$mds_gender <- mds_gender(mds_obj$M3A0800)
      mds_obj$M3A0800 <- NULL

      if (.quietly==F) {
        cat('gender, ')
      }
    }

  ##

    cat('..done')

  ## Return data-frame
  return(mds_obj)
}
