#' Creates Dictionary Mapping
#'
#' Creates Dictionary Mapping as List of Lists of Functions
#'
#' @param dict_list dictionary list produced by import()
#' @export
#' @examples
#' \dontrun{
#' ll <- import("~/Desktop/Dictionary.xlsx")
#' ll <- mapper(ll)
#' }
mapper <- function(dict_list){
    map <- tf_map(dict_list[["tfdct"]])
    map$table$required_columns <- required_columns_map(dict_list[["coldct"]])
    map$column <- columns_map(dict_list[["coldct"]])
    stats::setNames(map, c("file_level", "table_level", "column_level") )
}

tf_map <- function(tfdct){
    tt <- split(tfdct, tfdct$level)
    lapply(tt, function(x){
        tf <- x[c("rule", "condition")]
        l <- as.list(trimws(tf$condition))
        setNames(ifelse(tolower(l)=="true", TRUE, ifelse(tolower(l)=="false", FALSE, l)), trimws(tf$rule))
    })
}


required_columns_map <- function(d){
    dd <- split(d, d[["file"]])
    lapply(dd, function(x){
        as.list(x[tolower(x$required)=="yes", "variable"])$variable
    })
}



columns_map <- function(coldct) {
    dd <- split(coldct, coldct[["file"]])
    lapply(dd, function(x){
        field <- trimws(x[["variable"]])
        unique_funs <- ifelse(tolower(x[["unique"]])=="yes", "vc_unique()", NA)
        type_funs <- paste0("vc_type(\"", x[["type"]], "\")")
        rule_funs <- apply(x, 1,
                           function(z) ifelse(z[["rule"]]=="nchar"
                                              , paste0("vc_nchar(", gsub("\r|\n","", z[["condition"]]), ")")
                                              , paste0("vc_categories(c("
                                                       , paste( shQuote(strsplit(
                                                           gsub("\r|\n","", z[["condition"]])
                                                           , "\\s*,\\s*")[[1]]), collapse=",")
                                                       , "))")     ) )
        compare_funs <- ifelse(any(grepl("<|>=|<=|>|==|!=|~=",x[["compare"]]))
                               , compare_compiler(x[["compare"]])
                               , NA)
        lapply(split(
            cbind( unique_funs, type_funs, rule_funs, compare_funs)
            , field), function(x) c(na.omit(x)))
    })
}

compare_compiler <- function(x){
    objects <- strsplit(x, "<|>=|<=|>|==|!=|~=")[[1]]
    comparison <- trimws(gsub(paste(objects,collapse ="|"),"", x))
    dateop <- ifelse(any(grepl("date",tolower(objects))),TRUE, FALSE)
    paste0('vc_compare('
           ,paste(shQuote(objects),collapse=" , ")
           ,","
           ,paste0(shQuote(comparison))
           ,', date=',dateop,')')
}


