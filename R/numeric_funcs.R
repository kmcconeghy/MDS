numeric_cont <- function(x, .digits=2) {

  mean <- signif(mean(x, na.rm=T), digits = .digits)
  sd <- signif(sd(x, na.rm=T), digits = .digits)

  .flag <- if_else(sum(is.na(x)) / length(x)>0.10, '*', '')

  result <- paste0(mean, ' (', sd, ')', .flag)
  return(result)
}

numeric_fctr <- function(x, .digits=2, .referent = "1") {

  if (class(x)=='numeric' | class(x)=='logical') .referent = as.numeric(.referent)

  n <- sum(x==.referent, na.rm=T)
  prop <- sum(x==.referent, na.rm=T) / length(x)

  .flag <- if_else(sum(is.na(x)) / length(x)>0.10, '*', '')

  n_nm <- format(n,  big.mark=",")
  perc_nm <- scales::percent(prop)

  result <- paste0(n_nm, ' (', perc_nm, ')', .flag)
  return(result)
}

numeric_dt <- function(x) {

  stopifnot(class(x) == 'Date')

  .flag <- if_else(sum(is.na(x)) / length(x)>0.10, '*', '')

  dt_min <- format(n,  big.mark=",")
  perc_nm <- scales::percent(prop)

  result <- paste0(n_nm, ' (', perc_nm, ')', .flag)
  return(result)
}
