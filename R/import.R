#' Reads in Imports Dictionary
#'
#' Reads in Imports Dictionary
#'
#' @param file path to file
#' @param method choice between reading dictionary from local or GoogleSheets source
#' @param \ldots  extra params
#' @return list containing table, file, and column-level dictionaries
#' @export
#' @examples
#' \dontrun{
#' ll <- import("~/Desktop/Dictionary.xlsx")
#' }
import <- function(file, method="excel", ...){
    if(method=="excel"){
        ll <- import_excel(file)
    } else {
        ll <- import_gsheets(file, ...)
    }
    #clean
    tfdct <- cleaner( ll[grep("table", tolower(names(ll)))][[1]]
                                )[c("level", "rule", "condition")]
    coldct <- cleaner( ll[grep("column", tolower(names(ll)))][[1]]
                                )[c("import", "file", "variable", "type", "rule"
                                    , "condition", "unique", "required", "compare")]
    setNames(list(tfdct, coldct),c("tfdct", "coldct"))
}


import_excel <- function(file){
    sheets <- readxl::excel_sheets(file)

    setNames(lapply(sheets, function(x) readxl::read_excel(file, sheet = x)),sheets)
}


import_gsheets <- function(file_name, new_user=FALSE){
    # auth
    gs <- googlesheets::gs_ls(new_user=new_user)
    ss <- googlesheets::gs_key(gs[gs$sheet_title==file_name, ]$sheet_key, lookup = FALSE, visibility = "private")

    # get data
    sheets <- grep("table|column", googlesheets::gs_ws_ls(ss), value = TRUE)
    setNames(lapply(sheets, function(x) googlesheets::gs_read(ss, ws = x)),sheets)

}
