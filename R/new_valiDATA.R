valiData <- function(path, map, delete = TRUE) {
    . <- NULL
    if (delete) {
        delete_old_reports(path)
    }

    report_path <- file.path(path,"`Reports")

    if (!file.exists(report_path)) dir.create(report_path)

    if (file.info(path)[["isdir"]]) {
        if (length(dir(path))==0) stop("Your path is a directory with no contents.")
#
#         # create file to capture console report
#         sink(
#             file.path(report_path, "valiData_report.txt"),
#             append = file.exists(file.path(report_path, "valiData_report.txt")),
#             split = TRUE
#         )

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
            validated <- validate_file(file = x, file_name = file_name , map=map)
            list(header_info, validated)
        }))

        list(vld, empty_folders, csv_subpaths, has_files)


    } else {
        stop("Your path is not to a directory.\n\tvaliData works on directories so that\n\treports stay with the files they correspond to.")
    }
}
