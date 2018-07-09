#' @title mds_rpt_sty: Set excel formats
#'
#' @description Called by mds_rpt_xls()
#'
#' @usage mds_rpt_sty()
#'
#' @export
#'
mds_rpt_sty <- function() {
  title_sty <- openxlsx::createStyle(fontColour = 'black',
                                       halign = 'left',
                                       valign = 'center',
                                       textDecoration = 'Bold',
                                       border = 'Bottom')

  resultcol_sty <- openxlsx::createStyle(fontColour = 'black',
                                           halign = 'center',
                                           valign = 'center')

  subhead_sty <- openxlsx::createStyle(fontColour = 'black',
                                         halign = 'left',
                                         valign = 'center')

  varcat_sty  <- openxlsx::createStyle(fontColour = 'black',
                                         halign = 'center',
                                         valign = 'center',
                                         textRotation = 90)
}
