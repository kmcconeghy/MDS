#' @title mk_repid: Make random Repository ID
#'
#' @description Internal function for making fake MDS
#'
#' @usage mk_repid()
#'
#' @export
#'
#'
mk_repid <- function() {
  state_samp <- sample(state.abb, 1)
  int_samp <- str_pad(sample(1:99999, 1),
                      width=10,
                      side='left',
                      pad='0')
  y <- paste0(state_samp,'-',int_samp)
  return(y)
}

#' @title make_beneid: Make random Medicare Beneficiary ID
#'
#' @description Internal function for making fake MDS
#'
#' @usage mk_repid()
#'
#' @export
#'
#
mk_beneid <- function() {
  y <- paste0('jjjjjjj',
              stringi::stri_rand_strings(1, 9, pattern='[a-z0-9]'))
  return(y)
}

#' @title mk_accptid: Make random Brown U. facility ID
#'
#' @description Internal function for making fake MDS
#'
#' @usage mk_accptid()
#'
#' @export
#'
#
mk_accptid <- function() {
  y <- as.character(sample(1:999999, 1))
  return(y)
}

#' @title mk_provid: Make random provider facility ID
#'
#' @description Internal function for making fake MDS
#'
#' @usage mk_provid()
#'
#' @export
#'
#
mk_provid <- function(x) {
  state_abb <- str_extract(x, pattern="^.{2}")
  int_samp <- str_pad(sample(1:16000, 1),
                      width=10,
                      side='left',
                      pad='0')
  y <- paste0(state_abb,'-',int_samp)
  return(y)
}
