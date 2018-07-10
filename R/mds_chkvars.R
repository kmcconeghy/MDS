#' @title chk_std: Test if standard variable
#'
#' @description Called when testing a MDS data.frame is canonical
#'
#' @usage chk_std(x)
#'
#' @return logical
#'
#' @export
#'
chk_std <- function(x) {
  ## Pull standards
  var_nms <- mdsR::mds_varlist[mds_varlist$standard=='std', ]$item
  val <- dplyr::if_else(x %in% var_nms, T, F)
  return(val)
}

#' @title chk_DMREPID: Test Repository ID
#'
#' @description Called when testing a MDS data.frame is canonical
#'
#' @usage chk_DMREPID(x)
#'
#' @export
#'
chk_DMREPID <- function(x) {
  testthat::expect_is(x, character)
}

#' @title chk_DMRECID: Test Record ID
#'
#' @description Called when testing a MDS data.frame is canonical
#'
#' @usage chk_DMRECID(x)
#'
#' @export
#'
#'
chk_DMRECID <- function(x) {
  testthat::expect_is(x, character)
}
