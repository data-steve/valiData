#' Validate a Directory of Subdirectories of CSV Files
#'
#' Validate a directory of dubdirectories of .csv files.
#'
#' @param path Path to a directory with drecories of .csv files.
#' @param core_data_map A mapping (\code{list} of \code{data.frame}s).  One
#' \code{data.frame} per .csv file with a \code{header} (header name) &
#' \code{required} (logical,; Is column required?).
#' @param column_map A list (one per file) of lists (one per column in each file)
#' of the desired tests to run on columns (see \code{read_column_map_dir}).
#' @param delete logical.  If \code{TRUE} the old report is deleted.
#' @return Returns a report \file{~/`Reports/valiData_report.txt}.
#' @export
#' @importFrom magrittr %>%
#' @seealso \code{\link[valiData]{validate_file}}
#' @examples
#' \dontrun{
#' valiData("C:/Users/trinker/Desktop/myfolder", mymap)
#' }
valiData <- function(path, map, delete = TRUE, ...) {

    . <- NULL

    if (delete) {
        delete_old_reports(path)
    }

    report_path <- file.path(path,"`Reports")

    if (!file.exists(report_path)) dir.create(report_path)

    ## Directory Level ##
    ## check that path points to a directory
    is_directory <- vd_dir(path)
    if (!is_directory[['valid']]) {
        out <- list(is_directory = is_directory)
        class(out) <- 'valiData_dir_level'
        return(out)
    }

    ## check that directory has stuff in it
    non_empty_directory <- vd_non_empty(path)
    if (!is_directory[['valid']]) {
        out <- list(empty_directory = empty_directory)
        class(out) <- 'valiData_dir_level'
        return(out)
    }

    # report on which folders had no csv files to report on
    empty_folders <- vd_empty_subfolders(path)

    has_files <- length(dir(path, pattern = paste0("\\.", map[["file"]][["type"]],"$")) )>0

    # csv_subpaths is only for subfolders that containing csv files
    csv_subpaths <- get_paths_to_csvs(path)

    vld <- invisible(lapply(csv_subpaths, function(x){
        if (has_files){
            file_name <- tolower(tools::file_path_sans_ext(basename(x)))
        } else {
            file_name <- tolower(basename(dirname(x)))
        }

        ## Print csv folder name and path
        header_info <- header_file(
            basename(dirname(x)),
            gsub("/+|\\\\+", "/", gsub(path, "~/",  x, fixed=TRUE) )
        )
        validated <- validate_file(path = x, file_name = file_name , map=map)
        list(header_info, validated)
    }))

    class(vld) <- 'valiData'
    vld


}
