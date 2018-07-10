#' @title mds_chkfile: Test object for canon MDS file
#'
#' @description Called when testing an object is MDS canonical
#'
#' @usage mds_chkfile(x, .report=T)
#'
#' @param x A data.frame class object.
#'
#' @return Prints output to console
#'
#' @export
#'
#' @examples
#' require(mdsR)
#' mds_dta <- mdsR::mds_cohort
#' mds_chkfile(mds_dta)
#'
#' @references
mds_chkfile <- function(mds_obj) {

  obj_name <- rlang::quo(mds_obj)

  ## Print summary statistics
    cat('General Report on Minimum Dataset File \n')
    cat('R global environment. object: ', paste0(rlang::quo_text(obj_name), '\n'))
    cat('Date Created: ', Scotty::TimeStamp(), 'Programmer: ', 'KWM',  '\n')


  ## Evaluate standard



  ##Pull non-standards
  var_nms <- mdsR::mds_varlist[mds_varlist$standard=='std', ]$item

}
