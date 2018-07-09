#' @title Descriptive Report of MDS File
#'
#' @description Outputs select analyses of an MDS data.frame object
#'
#' @usage mds_rpt_xls(mds_obj, 'mds.xlsx', quietly=F)
#'
#' @param mds_obj A data.frame class object with MDS items.
#'
#' @param filepath Path to save excel file
#'
#' @param quietly If FALSE, will print some output to console
#'
#' @return No return
#'
#' @export
#'
#' @examples
#' require(mdsR)
#' mds_dta <- mdsR::mds_cohort
#' mds_rpt_xls(mds_dta, 'mds.xslx')
#'
#' @references
mds_rpt_xls <- function(mds_object,
                        filepath,
                        quietly = F) {


  ## Set-up file
  mds_rpt_style()

  obj_name_eq <- rlang::enquo(mds_object)

  ## Set-up workbook
  xlsx.wb <- openxlsx::createWorkbook()
  options('openxlsx.borderColour' = '#4F80DB')
  options('openxlsx.borderStyle' = 'thin')
  openxlsx::modifyBaseFont(xlsx.wb, fontSize = 10, fontName ='Arial Narrow')

  openxlsx::addWorksheet(xlsx.wb, sheetName = 'MDS File')

  ## Title Box
  if (quietly==F) {
    cat('General Report on Minimum Dataset File \n')
    cat('R gloal environ. object: ', paste0(obj_name_eq[2]), '\n')
    cat('Date Created: ', Scotty::TimeStamp(), 'Programmer: ', 'KWM',  '\n')
  }

  ##Title
  openxlsx::writeData(xlsx.wb, 1,
                      'General Report on Minimum Dataset File',
                      startRow=1, startCol=1)
  openxlsx::addStyle(xlsx.wb, 1, style=title_style, rows=1, cols=1)

  ## Subheading
  openxlsx::writeData(xlsx.wb, 1,
                      paste0('R gloal environ. object: ', obj_name_eq[2]),
                      startRow=2, startCol=1)
  openxlsx::addStyle(xlsx.wb, 1, style=subhead_style, rows=2, cols=1)

  openxlsx::writeData(xlsx.wb, 1, paste0('Date Created: ',
                                         Scotty::TimeStamp(),
                                         ', Programmer: ',
                                         'KWM'),
                      startRow=3, startCol=1)
  openxlsx::addStyle(xlsx.wb, 1, style=subhead_style, rows=3, cols=1)




  ## Save final workbook
  openxlsx::saveWorkbook(xlsx.wb, file=filepath, overwrite = T)
}
