#' Apply Column Map to Dataframe
#'
#' Apply a column map from \code{read_column_map_dir} to a
#' \code{\link[base]{data.frame}}.
#'
#' @param data A data frame to run column-wise tests on.
#' @param colmap A column map from \code{read_column_map_dir}.
vc_column_apply <- function(data, colmap){
# browser()
    ## only check the headers that exist in both map and data
    map <- colmap[colnames(data)[colnames(data) %in% names(colmap)]]
    data <- data[names(map)]
# tryCatch({

    Map(function(x, y, z){

        y <- gsub("\\)$", paste0(", colnames = \"", z, "\")"), y)
        results <- unlist(invisible(lapply(y, function(w) {

            # tryCatch({

            eval(parse(text=w))
#
#             }, warning = function(w) {
#     browser()
# }, error = function(e) {
#     browser()
# })

            })))
        return(all(results))

    }, data, map, colnames(data))


# }, warning = function(w) {
#     browser()
# }, error = function(e) {
#     browser()
# })

}
