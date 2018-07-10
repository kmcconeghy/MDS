#' @title chk_nonstd: Test if non-standard variable
#'
#' @description Called when testing a MDS data.frame is canonical
#'
#' @usage chk_nonstd(x)
#'
#' @return logical
#'
#' @export
#'
chk_nonstd <- function(x) {
  ## Pull standards
  var_nms <- mdsR::mds_varlist[mds_varlist$standard=='non-std', ]$item
  val <- dplyr::if_else(x %in% var_nms, T, F)
  return(val)
}
#' @title chk_bene_id_18900: Test Beneficiary ID
#'
#' @description Called when testing a MDS data.frame is Brown U. canonical
#'
#' @usage chk_bene_id_18900(x)
#'
#' @export
#'
chk_bene_id_18900 <- function(x) {
  testthat::expect_is(x, character)
}

#' @title chk_dmdate: Test Assessment date variable
#'
#' @description Called when testing a MDS data.frame is Brown U. canonical
#'
#' @usage chk_dmdate(x)
#'
#' @export
#'
chk_dmdate <- function(x) {
  testthat::expect_is(x, date)
}

#' @title chk_dmadate: Test admission date variable
#'
#' @description Called when testing a MDS data.frame is Brown U. canonical
#'
#' @usage chk_dmadate(x)
#'
#' @export
#'
chk_dmadmit <- function(x) {
  testthat::expect_is(x, date)
}

#' @title chk_dmtype: Test Assessment type variable
#'
#' @description Called when testing a MDS data.frame is Brown U. canonical
#'
#' @usage chk_dmtype(x)
#'
#' @export
#'
chk_dmtype <- function(x) {
  testthat::expect_is(x, numeric)
  testthat::expect_equal(dplyr::n_distinct(x), 3L)
}
