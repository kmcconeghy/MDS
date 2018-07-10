#' @title mds_rpt_sty: Set excel formats
#'
#' @description Called by mds_rpt_xls()
#'
#' @usage mds_rpt_sty()
#'
#' @export
#'
mds_rpt_sty <- function() {

  xl_style <- list()

  xl_style$title <- openxlsx::createStyle(fontColour = 'black',
                                          halign = 'left',
                                          valign = 'center',
                                          textDecoration = 'Bold',
                                          border = 'Bottom')

  xl_style$subhead <- openxlsx::createStyle(fontColour = 'black',
                                            halign = 'left',
                                            textDecoration = 'Bold',
                                            valign = 'center')

  xl_style$result <- openxlsx::createStyle(fontColour = 'black',
                                           halign = 'left',
                                           valign = 'center')


  return(xl_style)
}
