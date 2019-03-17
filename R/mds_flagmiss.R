#' @title mds_flagmiss: Generate report of missingness
#'
#' @description Will report % missing for each column in a table
#'
#' @usage mds_flagmiss(mds_object, .quietly=F, .cutoff=0.10, .table=F)
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
mds_flagmiss <- function(mds_object, .quietly=F, .cutoff=0.10, .table=F) {

  ## sanity check
  stopifnot((attr(mds_object, 'mds_canon') %in% c('mds_core', 'mds_std'))==T)

  varlist <- names(mds_object)

  count_na <- sapply(mds_object, function(x) sum(is.na(x)))
  count <- nrow(mds_object)

  prop_na <- sapply(count_na, function(x) x/count)

  if (!.quietly) {
    cat('MDS variables with >10% missingness...')
    if (max(prop_na)<.cutoff) {
      cat('...none \n')
    } else {
    for (i in varlist) {
      if (prop_na[i]>.cutoff) {
        cat('\n') #newline
        cat('Var: ', names(prop_na[i]), ' ', scales::percent(prop_na[i]), '\n')
        }
      }
    } # end else
  } # end  if(quietly)

  ## return table
  if (.table) {
    out <- data.frame(varlist, count_na, prop_na) %>%
      dplyr::filter(prop_na >= .cutoff)
    rownames(out) <- NULL
    return(out)
  }
}
