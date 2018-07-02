#' @title numeric_cont: Compute mean (standard deviation)
#'
#' @description Given a single numeric vector, will compute mean and
#' standard deviation, returning results in string. Mostly called by
#' other functions.
#'
#' @usage numeric_cont(x, .digits=2)
#'
#' @param x A numeric vector
#'
#' @param .digits Significant figures to round to
#'
#' @return A character string of length 1, 'mean (SD)'
#'
#' @export
#'
#' @examples
#' require(mdsR)
#' mds_dta <- mdsR::mds_cohort
#' numeric_cont(mds_dta$adl, start, stop)
#'
#' @references
numeric_cont <- function(x, .digits=2) {

  mean <- signif(mean(x, na.rm=T), digits = .digits)
  sd <- signif(sd(x, na.rm=T), digits = .digits)

  .flag <- if_else(sum(is.na(x)) / length(x)>0.10, '*', '')

  result <- paste0(mean, ' (', sd, ')', .flag)
  return(result)
}
#' @title numeric_fctr: Compute number (proportion as percent)
#'
#' @description Given a single numeric vector or factor,
#' will compute no. equal to a .referent value and percent of total,
#' returning results in string. Mostly called by other functions.
#'
#' @usage numeric_fctr(x, .digits=2, .referent = TRUE)
#'
#' @param x A numeric or factor class vector
#'
#' @param .digits Significant figures to round to
#'
#' @param .referent Value to compute no. (%) for
#'
#' @return A character string of length 1, 'No. (%)'
#'
#' @export
#'
#' @examples
#' require(mdsR)
#' mds_dta <- mdsR::mds_cohort
#' numeric_fctr(mds_dta$M3A0800, .referent==1)
#'
#' @references
numeric_fctr <- function(x, .digits=2, .referent = TRUE) {

  if (class(x)=='numeric' | class(x)=='logical') .referent = as.numeric(.referent)

  n <- sum(x==.referent, na.rm=T)
  prop <- sum(x==.referent, na.rm=T) / length(x)

  .flag <- if_else(sum(is.na(x)) / length(x)>0.10, '*', '')

  n_nm <- format(n,  big.mark=",")
  perc_nm <- scales::percent(prop)

  result <- paste0(n_nm, ' (', perc_nm, ')', .flag)
  return(result)
}
#' @title numeric_dt: Compute median date, range.
#'
#' @description Given a single integer vector (with origin specified)
#' or data class vector, will compute median day and a range
#' (default min/max) and percent of total, returning results in string.
#' Mostly called by other functions.
#'
#' @usage numeric_dt(x, median = TRUE, range='min/max', origin=NULL)
#'
#' @param x A integer or date class vector
#'
#' @param nomedian A logical vector, indicates whether to report median or not.
#' Default = TRUE.
#'
#' @param range A character string to indicate range values. options:
#' 'min/max'.
#'
#' @param origin Default NULL. If specified,
#' must be a date class object of length 1. For use if date variable is in
#' raw integer format.
#'
#' @return A character string of length 1, 'Median date (date1, date2)'
#'
#' @export
#'
#' @examples
#' require(mdsR)
#' mds_dta <- mdsR::mds_cohort
#' numeric_dt(mds_dta$dmdate)
#'
#' @references
numeric_dt <- function(dt, median = TRUE, range='min/max', origin=NULL) {

  if (class(dt) !='Date') {
    stopifnot(class(dt)=='Integer')
    dt <- lubridate::as_date(dt, origin=origin)
  }


  .flag <- if_else(sum(is.na(dt)) / length(dt)>0.10, '*', '')

  if (median == TRUE) {
    dt_med <- format(median(dt))
  } else {dt_med <- ''}

  if (range == 'min/max') {
    range_dt = paste0(min(dt), ', ', max(dt))
  } else { range_dt = ''}

  result <- paste0(dt_med, ' (', range_dt, ')', .flag)
  return(result)
}
