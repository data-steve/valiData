# helper function to shorten list of offending rows
# to be inserted in other function reporting
output_truncate <- function(locs){
  # added type conversion because Tyler had decided
  # that it would be wise <#sacrasm> to pass in locs as text
  if (is.character(locs)){
    locs <- suppressWarnings(as.numeric(locs))
  }

  if (length(locs) > 100) {
      paste0(paste(locs[1:100]+1, collapse=", "), "...[truncated]...")
  } else {
       paste(locs+1, collapse=", ")
  }
 }