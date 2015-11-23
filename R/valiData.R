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
valiData <- function(path, core_data_map, column_map, delete = TRUE) {
	if (delete) {
		delete_old_reports(path)
	}

	report_path <- file.path(path,"`Reports")

	if (!file.exists(report_path)) dir.create(report_path)

	if (file.info(path)[["isdir"]]) {

		# create file to capture console report
		sink(
			file.path(report_path, "valiData_report.txt"),
			append = file.exists(file.path(report_path, "valiData_report.txt")),
			split = TRUE
		)

		# csv_subpaths is only for subfolders that containing csv files
		csv_subpaths   <- get_paths_to_csvs(path)
		all_subpaths <- get_paths_root_to_files(path)

		# report on which folders had no csv files to report on
		print(report_empty_subfolders(vf_empty_subfolders(all_subpaths, csv_subpaths)))

		invisible(lapply(csv_subpaths, function(x){
			map_folder <- basename(dirname(x))
			## Print csv folder name and path
			print(report_print_arbitrary(header_file(basename(dirname(x)),
				gsub("/+", "/", gsub("\\\\+", "/", gsub(path, "~/",  x, fixed=TRUE)))
				)))
			validate_file(file = x, map = core_data_map[[map_folder]], column_map[[map_folder]])
		}))

		sink()

	} else {
		map_folder <- strsplit(path, "/")[[1]] %>% .[length(.)-1]

		sink(file.path(report_path,
					   paste(map_folder,"valiData_report.txt", sep="_")
		), split = TRUE)
		validate_file(path= path, map = core_data_map[[map_folder]], column_map[[map_folder]])
		sink()
	}
}
