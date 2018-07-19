#' @title mds_gender: Reformat M3A0800 to gender variable
#'
#' @description
#'
#' @usage mds_gender(x)
#'
#' @param x A vector corresponding to numeric or integer
#'
#' @export
#'
mds_gender <- function(x) {
   x2 <- factor(x, levels=c(1, 2), labels = c('Male', 'Female'))
   return(x2)
  }
