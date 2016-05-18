vd_non_empty <- function(path){
  length(dir(path, pattern = paste0("\\.",test[["file"]][["type"]],"$")) )>0
}