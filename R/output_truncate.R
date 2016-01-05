output_truncate <- function(locs){
     if (length(locs) > 100) {
        locs <- paste0(paste(locs[1:100]+1, collapse=", "), "...[truncated]...")
    } else {
        locs <- paste(locs+1, collapse=", ")
    }
 }