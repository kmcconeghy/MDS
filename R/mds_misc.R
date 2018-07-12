#' @title mds_des: Quick function for general stats on mds file
#'
#' @description Called when evaluating an MDS data.frame, must be canonical
#'
#' @usage mds_des(x)
#'
#' @export
#'
mds_des <-  function(x) {
  res <- list()
  res$nrows <- nrow(x)
  res$ncols <- ncol(x)
  res$first_asmt <- min(x$dmdate, na.rm=T)
  res$last_asmt <- max(x$dmdate, na.rm=T)
  res$n_patients <- n_distinct(x$bene_id_18900)
  res$asmt_perpat <- round(nrows / res$n_patients,2)
  res$n_duprecs <- n_distinct(x[duplicated(x$DMRECID),])

  return(res)
}
