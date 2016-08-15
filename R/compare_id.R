## This file is for internal use
compare_column <- function(path, column, parent, child, ignore.case = TRUE, ...) {

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
    non_empty_directory <- vd_non_empty(path)
    if (!is_directory[['valid']]) {
        out <- list(path=path, empty_directory = non_empty_directory)
        class(out) <- 'valiData_dir_level'
        return(out)
    }

    # csv_subpaths is only for subfolders that containing csv files
    csv_subpaths <- get_paths_to_csvs(path)
    parsed <- strsplit(csv_subpaths, "(\\\\|/)+")
    lens <- sapply(parsed, length) - 1
    folder <- unlist(Map(function(x, y){x[y]}, parsed, lens))

    parent_file <- csv_subpaths[match(parent, folder)]
    child_file <- csv_subpaths[match(child, folder)]

    parent_table <- suppressWarnings(readr::read_csv(parent_file))

    if (isTRUE(ignore.case)){
        colnames(parent_table) <- tolower(colnames(parent_table))
    }

    ## check for duplicate rows minus the personal identifier
    dupes <- vt_duplicated_rows(parent_table[, !parent_table %in% c(column)])
    if (!dupes[['valid']]) {return(dupes)}

    validated <- lapply(stats::na.omit(child_file), function(x){
        child_table <- suppressWarnings(readr::read_csv(x))
        if (isTRUE(ignore.case)){colnames(child_table) <- tolower(colnames(child_table))}
        vc_id_found(child_table, column, parent_table, ignore.case=ignore.case, parent=parent)
    })



    dir_info <- invisible(lapply(stats::na.omit(child_file), function(x){
        file_name <- basename(dirname(x))

        ## Print csv folder name and path
        header_file(
            basename(dirname(x)),
            gsub("/+|\\\\+", "/", gsub(path, "~/",  x, fixed=TRUE) )
        )

    }))

    out <- list(dir_info = dir_info, validated = validated)

    class(out) <- 'compare_column'
    out

}


vc_id_found <- function(data, x, data2, ignore.case, parent = 'the parent data', ...) {

    col <- sub_out_missing(data[[x]])
    is_na <- is.na(col)

    xc <- ifelse(ignore.case, tolower(x), x)
    is_valid <- data[[xc]] %in% data2[[xc]]

    is_valid[is_na] <- NA
    are_valid <- all(is_valid)
    if (!are_valid) {
        message <- sprintf("The following rows of %s contain elements not found in %s:\n\n%s\n\n\n\n",
            sQuote(x), parent, output_truncate(which(!is_valid)))
    } else {
        message <- NULL
    }

    vc_output <- list(column_name = x, valid = are_valid, message = message,
        passing = is_valid, missing = is_na, call = "vc_id_found")
    class(vc_output) <- "vc"
    vc_output
}


print.compare_column <- function(x, ...){
    invisible(Map(function(x, y){
        cat(x)
        if (y[['valid']]) {
            print(report_all_is_well())
        } else {
            print(y)
        }
    }, x[['dir_info']], x[['validated']]))
}


