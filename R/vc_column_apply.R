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

    Map(function(x, y, z){
        #y <-
        #gsub("\\)$", paste0("data, ", z, "\")"), y)

        replacement <- paste0("\\1", paste0("data, ", shQuote(z), ", \\2"))
        y <- gsub(",\\s*\\)", ")", gsub("(^[^\\(]+\\()(.+$)", replacement, y))

        invisible(lapply(y, function(w) {
            eval(parse(text=w))
            }))

    }, data, map, colnames(data))

}

