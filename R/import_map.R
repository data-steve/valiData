#' Convenience Function to Imports and Map Dictionary
#'
#' Convenience Function to Imports and Map Dictionary: wraps up import() and mapper()
#'
#' @param file path to file
#' @param method choice between reading dictionary from local or GoogleSheets source
#' @param \ldots  extra params
#' @export
#' @examples
#' \dontrun{
#' ll <- import_map("~/Desktop/Dictionary.xlsx")
#' }
import_map <- function(file, method="excel", ...){
    ll <- import(file, method,...)
    mapper(ll)
}
