#' @title mds_flagmiss: Generate report of missingness
#'
#' @description Will report % missing for each column in a table
#'
#' @usage mds_flagmiss(mds_object, .quietly=F, .cutoff=0.10, .table=T)
#'
#' @param mds_object A data.frame/mds class object with MDS items.
#'
#' @param .quietly A logical vector, whether to print output to console
#'
#' @param .cutoff a numeric between 0 and 1, identifyings proportion of
#' missingness to report
#'
#' @param .table A logical vector, whether to output a dataframe
#' with missing results
#'
#' @return No return
#'
#' @export
#'
#' @examples
#' require(mdsR)
#' mds_dta <- mdsR::mds_dta
#' mds_flagmiss(mds_dta)
#'
mds_flagmiss <- function(mds_object, .quietly=F, .cutoff=0.10, .table=T) {

  ## sanity check
  stopifnot((attr(mds_object, 'mds_canon') %in% c('mds_core', 'mds_std'))==T)

  varlist <- names(mds_object)

  count_na <- sapply(mds_object, function(x) sum(is.na(x)))
  count <- nrow(mds_object)

  count_na <- sapply(count_na, function(x) x/count)

  if (.quietly==F) {
    cat('MDS variables with >10% missingness... \n')
    if (max(count_na)<.cutoff) cat('...none')
    for (i in varlist) {
      if (count_na[i]>.cutoff) {
        cat('Var: ', names(count_na[i]), ' ', scales::percent(count_na[i]), '\n')
      }
    }
  }

  ## return table
  if (.table==T) {
    out <- data.frame(varlist, count_na)
    rownames(out) <- NULL
    return(out)
  }
}
