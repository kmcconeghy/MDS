#' @title chk_DMREPID: Test Repository ID
#'
#' @description Called when testing a MDS data.frame is canonical
#'
#' @usage chk_DMREPID(x)s
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
#' @usage chk_DMRECID(x)s
#'
#' @export
#'
#'
chk_DMRECID <- function(x) {
  testthat::expect_is(x, character)
}
