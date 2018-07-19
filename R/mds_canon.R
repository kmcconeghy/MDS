#' @title mds_as_canon: Test object for canon MDS file
#'
#' @description Called when testing an object is MDS canonical
#'
#' @usage mds_as_canon(mds_obj, .report=T)
#'
#' @param mds_obj A data.frame class object.
#'
#' @param .report A logical vector, indicated whether to output to console
#'
#' @return Prints output to console, if canon returns data.frame with attribute
#'
#' @export
#'
#' @examples
#' require(mdsR)
#' mds_dta <- mdsR::mds_cohort
#' mds_as_canon(mds_dta)
#'
mds_as_canon <- function(mds_obj, .report=T) {

  ## Scoping
    obj_nm <- rlang::quo(mds_obj)
    obj_str <- rlang::quo_text(obj_nm)

  ## Check if canon
    test_df <- any(class(mds_obj)=='data.frame')
    test_tbl <- any(class(mds_obj)=='tbl')

  ## Contains core
    core <- c('DMRECID', 'DMREPID', 'dmdate')
    test_core <- all(core %in% names(mds_obj))

  ## Standard variables
    std_nms <- mdsR::mds_varlist[mds_varlist$standard=='std', ]$item
    test_std <-names(mds_obj) %in% std_nms

  ## Non-standard
    nonstd_nms <- mdsR::mds_varlist[mds_varlist$standard=='non_std', ]$item
    test_nonstd <- names(mds_obj) %in% nonstd_nms

  ## Unknown variables
    unk_nms <- names(mds_obj)[test_std==F & test_nonstd==F]

  ## Class mds
    if (test_core==F) attr(mds_obj, 'mds_canon') <- 'heresy'
    if (test_core==T) attr(mds_obj, 'mds_canon') <- 'mds_core'
    test_mds <- if_else(attr(mds_obj, 'mds_canon') %in% c('mds_core', 'mds_std'), T, F)

  ## Return
    if (.report==T) {
      cat(obj_str, ' is data.frame....', test_df, '\n')
      cat(obj_str, ' is tibble....', test_tbl, '\n')
      cat(obj_str, ' has core variables....', test_core, '\n')
      cat(obj_str, ' all variables "std"....', all(test_std), '\n')
      cat(obj_str, ' has "non-std" variables....', any(test_nonstd), '\n')
      if (is.null(unk_nms)==F) { cat('non-canon variables...', paste0(unk_nms, sep=','), '\n') }
      cat(obj_str, ' is mds canon....', test_mds, '\n')
    }

    if (test_mds==T) {
      return(mds_obj)
    }
}
#' @title mds_is_canon: Test if file is canon
#'
#' @description Returns logical for if file is canon
#'
#' @usage mds_is_canon(x)
#'
#' @export
#'
mds_is_canon <- function(x) {
  return(if_else(attr(x, 'mds_canon') %in% c('mds_core', 'mds_std'), T, F))
}
