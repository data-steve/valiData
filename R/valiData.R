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

    ## Directory Level ##
    ## check that path points to a directory
    is_directory <- vd_dir(path)
    if (!is_directory[['valid']]) {
        out <- list(path=path, is_directory = is_directory)
        class(out) <- 'valiData_dir_level'
        return(out)
    }

    ## check that directory has stuff in it
    non_empty_directory <- valiData_dir_level(path)
    if (!is_directory[['valid']]) {
        out <- list(path=path, empty_directory = empty_directory)
        class(out) <- 'valiData_dir_level'
        return(out)
    }

## the following will now happen in report or print???
# ## Prep folder for report
# if (delete) {
#     delete_old_reports(path)
# }
#
# report_path <- file.path(path,"`Reports")
#
# if (!file.exists(report_path)) dir.create(report_path)


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

    out <- list(path = path, per_file = vld)

    class(out) <- 'valiData'
    out


}





#' Prints a valiData_dir_level Object
#'
#' Prints a valiData_dir_level object
#'
#' @param x A valiData_dir_level object.
#' @param as.report logical.  If \code{TRUE} a report will be generated in the
#' directory indicated by \code{x$path}.
#' @param delete logical.  If \code{TRUE} and \code{as.report = TRUE} any prior
#' instances of `report will be deleted.
#' @param \ldots ignored.
#' @method print valiData_dir_level
#' @export
print.valiData_dir_level <- function(x, as.report = FALSE, delete = TRUE, ...){

    if (isTRUE(as.report)){

        if (isTRUE(delete)) {
            delete_old_reports(x[['path']])
        }

        report_path <- file.path(x[['path']],"`Reports")

        if (!file.exists(report_path)) dir.create(report_path)

		# create file to capture console report
		sink(
			file.path(report_path, "valiData_report.txt"),
			append = file.exists(file.path(report_path, "valiData_report.txt")),
			split = TRUE
		)

    }

    cat(header(paste("Directory:", x[['path']]), char='~'))
    print(x[[2]])

    if (isTRUE(as.report)) sink()
}



#' Prints a valiData Object
#'
#' Prints a valiData object
#'
#' @param x A valiData object.
#' @param as.report logical.  If \code{TRUE} a report will be generated in the
#' directory indicated by \code{x$path}.
#' @param delete logical.  If \code{TRUE} and \code{as.report = TRUE} any prior
#' instances of `report will be deleted.
#' @param \ldots ignored.
#' @method print valiData
#' @export
print.valiData <- function(x, as.report = FALSE, delete = TRUE, ...){

    if (isTRUE(as.report)){

        if (isTRUE(delete)) {
            delete_old_reports(x[['path']])
        }

        report_path <- file.path(x[['path']],"`Reports")

        if (!file.exists(report_path)) dir.create(report_path)

		# create file to capture console report
		sink(
			file.path(report_path, "valiData_report.txt"),
			append = file.exists(file.path(report_path, "valiData_report.txt")),
			split = TRUE
		)

    }

###
#    Print stuff here
#####

    if (isTRUE(as.report)) sink()
}



