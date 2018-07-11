#' @title mds_canon: Test object for canon MDS file
#'
#' @description Called when testing an object is MDS canonical
#'
#' @usage mds_canon(x, .report=T)
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
#' mds_canon(mds_dta)
#'

mds_canon <- function(mds_obj, .report=T) {

  ## Scoping
    obj_nm <- rlang::quo(mds_obj)

  ## Check if canon
    test_df <- any(class(mds_obj)=='data.frame')
    test_tbl <- any(class(mds_obj)=='tbl')

  ## Contains core
    core <- c('DMRECID', 'DMREPID', 'dmdate')
    test_core <- all(core %in% names(mds_obj))

  ## Standard variables
    std_nms <- mdsR::mds_varlist[mds_varlist$standard=='std', ]$item
    test_std <-names(mds_obj) %in% std_nms
    unk_nms <- names(mds_obj)[test_std==F] #add more

  ## Non-standard
    nonstd_nms <- mdsR::mds_varlist[mds_varlist$standard=='non-std', ]$item
    test_nonstd <- names(mds_obj)[unk_nms] %in% nonstd_nms
    unlisted_nms <- names(mds_obj) #add more

  ## Class mds
    if (test_core==T) class(mds_obj) <- 'mds_core'
    if (all(test_core, test_std)) class(mds_obj) <- 'mds_std'
    test_mds <- if_else(any(class(mds_obj)=='mds_core'), T, F)

  ## Return
    if (.report==T) {
      cat(rlang::quo_text(obj_nm), ' is data.frame....', test_df, '\n')
      cat(rlang::quo_text(obj_nm), ' is tibble....', test_tbl, '\n')
      cat(rlang::quo_text(obj_nm), ' has core variables....', test_core, '\n')
      cat(rlang::quo_text(obj_nm), ' all variables "std"....', all(test_std), '\n')
      if (all(test_std)==F) { cat(' "non-std" variables....', paste0(unk_nms, sep=','), '\n') }
      cat(rlang::quo_text(obj_nm), ' is mds canon....', test_mds, '\n')
    }

    if (test_mds==T) {
      return(mds_obj)
    }
}
