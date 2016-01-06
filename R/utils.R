# read_csv_character <- function(file, ...){
# 	col_num <- ncol(readr::read_csv(file, n_max=1, ...))
# 	readr::read_csv(file, col_types = cols(rep("c", col_num)), ...)
# }

header <- function(x, len = nchar(x), char = "=", line.begin = "", additional =  NULL){
	border <-paste(rep(char, len), collapse="")
	paste(border, paste(paste0(line.begin, c(x, additional)), collapse="\n"), border, "\n", sep="\n")
}


header_file <- function(x, path, len = 90, char = "#", line.begin = "\t"){
	header(x = paste("Title:", x), len = len, char = char, line.begin = line.begin, additional = paste("Path: ", path))
}

#print(report_print_arbitrary(header_file("I like cookies", "path/to/chocolate/chip.csv")))

report_print_arbitrary <- function(x, ...){
	class(x) <- c("report_print_arbitrary", class(x))
	x
}

print.report_print_arbitrary <- function(x, ...){
    cat(x)
}


get_paths_to_csvs <- function(path){
    . <- NULL
	invisible(lapply(dir(path, pattern = ".csv$",recursive = TRUE), function(x) file.path(path,x))) %>%
		sapply(., "[[",1) %>%
		sapply(.,function(x) gsub("//","/", x), USE.NAMES = FALSE) %>%
		sapply(.,function(x) gsub("~",path.expand("~"), x), USE.NAMES = FALSE)
}

# report on which folders had no csv files to report on

get_paths_root_to_files <- function(path) {
    . <- NULL
	list.dirs(list.dirs(path, recursive=FALSE), recursive=FALSE) %>%
		sapply(.,function(x) gsub("//","/", x), USE.NAMES = FALSE)
}

find_lowest_folder <- function(path){
	. <- NULL
	paths <- list.dirs(list.dirs(path, recursive=FALSE), recursive=FALSE)
	lapply(1:length(paths), function(x) strsplit(paths, "/")[[x]] %>% .[length(.)]) %>%
		unlist() -> templates

	return(cbind(paths, templates))
}

delete_old_reports <- function(path){
    . <- NULL
	previous_reports_folders <- file.path(path,
										  dir(path, recursive=TRUE)[
										  	grepl(pattern="`Reports", dir(path, recursive=TRUE))
										  	]) %>%
		sapply(.,function(x) gsub("//","/", x), USE.NAMES = FALSE) %>%
		sapply(., function(x) {
			paste(strsplit(x, "/")[[1]] %>% .[1:length(.)-1], collapse="/")
		}
		, USE.NAMES=FALSE)

	invisible(lapply(previous_reports_folders, unlink, recursive = TRUE))

}

find_highest_parent_folder <- function(path) {
    . <- NULL
	sapply(get_paths_root_to_files(path), function(x) {
		strsplit(x, "/")[[1]]
	}, USE.NAMES = FALSE) %>%
		apply(., 1, function(x) !(length(unique(x))>1))  	%>%
		{paste(strsplit(get_paths_root_to_files(path)[1], "/")[[1]][.], collapse="/")}

}

## Makes a text tree diagram of a directory subdirectory, and files
## Use...
## dir_tree(system.file("texts", package = "tm"))
dir_tree <- function(path = ".", include.files = TRUE, all.files = TRUE, copy2clip = TRUE){

    if (isTRUE(include.files)){
        fls <- dir(path, all.files = all.files, full.names = TRUE, recursive = TRUE)
    } else {
        fls <- list.files(path, all.files = all.files)
    }
    parsed <- parse_path(fls)
    contents <- back(parsed, -(-1 + length(unlist(parse_path(path)))))
    y <- attributes(back(contents, -1))[["parsed"]]
    z <- data.frame(matrix(NA, ncol = max(sapply(y, length)), nrow = length(y)))
    for (i in seq_along(y)){
        z[i, 1:length(y[[i]])] <- y[[i]]
    }
    z[["pathString"]] <- contents

    out <- data.tree::as.Node(z)
    out2 <- capture.output(out)[-1]
    out2 <- gsub("^\\d+\\s+", "", out2)
    out2[-1] <- paste0("  ", out2[-1])
    #cat(paste(out2, collapse="\n"), "\n")
    if (copy2clip) clipr::write_clip(out2)
    out

}
