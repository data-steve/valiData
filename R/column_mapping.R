pacman::p_load(dplyr, readr, readxl, tidyr, stringi, magrittr, googlesheets)







# a lot of invisible NAs can get imputed in Excel
# that need to be removed
cleaner <- function(d){
    colnames(d) <- tolower(colnames(d))
    #drop empty columns
    d <- d[!sapply(names(d),function(x) all(is.na(d[x])), USE.NAMES = FALSE)]
    # drop empty rows
    d[!rowSums(is.na(d))/ncol(d)==1,]
}

import_excel <- function(file){
    sheets <- readxl::excel_sheets(file)

    ll <- setNames(lapply(sheets, function(x) readxl::read_excel(file, sheet = x)),sheets)

    tfdct <- cleaner( ll[grep("table", tolower(names(ll)))][[1]]
                        )[c("level", "rule", "condition")]
    coldct <- cleaner(
                    ll[grep("column", tolower(names(ll)))][[1]]
                      )[c("import", "file", "variable", "type", "rule", "condition", "unique", "required", "compare")]

    setNames(list(tfdct, coldct),c("tfdct", "coldct"))
}



import_gsheets <- function(file_name, new_user=FALSE){
    # auth

    gs <- googlesheets::gs_ls(new_user=new_user)

    ss <- googlesheets::gs_key(gs[gs$sheet_title==file_name, ]$sheet_key, lookup = FALSE, visibility = "private")

    # get data
    sheets <- grep("table|column", googlesheets::gs_ws_ls(ss), value = TRUE)
    ll <- setNames(lapply(sheets, function(x) googlesheets::gs_read(ss, ws = x)),sheets)

     #clean
    tfdct <- cleaner( ll[grep("table", tolower(names(ll)))][[1]]
                                    )[c("level", "rule", "condition")]
    coldct <- cleaner( ll[grep("column", tolower(names(ll)))][[1]]
                                    )[c("import", "file", "variable", "type", "rule", "condition", "unique", "required", "compare")]
    # share
    setNames(list(tfdct, coldct),c("tfdct", "coldct"))
}

# file_name <- "Core_Data_Dictionary_DS_longforms.xlsx"
#
# ll2 <- import_gsheets("Core_Data_Dictionary_DS_longforms.xlsx")



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

# create
columns_map <- function(coldct) {
  dd <- split(coldct, coldct[["file"]])
  lapply(dd, function(x){
    field <- trimws(x[["variable"]])
    # field <- stringi::stri_pad_right(field, width =(max(nchar(field))+1))
    # required_funs <- ifelse(tolower(x[["required"]])=="yes", "vc_required()", NA)
    unique_funs <- ifelse(tolower(x[["unique"]])=="yes", "vc_unique()", NA)
    type_funs <- paste0("vc_type(\"", x[["type"]], "\")")
    rule_funs <- apply(x, 1, function(z) ifelse(z[["rule"]]=="nchar"
                        , paste0("vc_nchar(", gsub("\r|\n","", z[["condition"]]), ")")
                        , paste0("vc_categories(c("
                                 , paste( shQuote(strsplit(gsub("\r|\n","", z[["condition"]]), "\\s*,\\s*")[[1]]), collapse=",")
                                  , "))")     ) )
    compare_funs <- ifelse(any(grepl("<|>=|<=|>|==|!=|~=",x[["compare"]]))
                           , compare_compiler(x[["compare"]])
                           , NA)

    lapply(split(cbind( unique_funs, type_funs, rule_funs, compare_funs), field),function(x) c(na.omit(x)))
  })
}

tf_map <- function(tfdct){
  tt <- split(tfdct, tfdct$level)
  lapply(tt, function(x){
    tf <- x[c("rule", "condition")]
    l <- as.list(trimws(tf$condition))
  setNames(ifelse(tolower(l)=="true", TRUE, ifelse(tolower(l)=="false", FALSE, l)), trimws(tf$rule))
  })
}

# f <- tf_map(tfdct)
# str(f)

table_required <- function(d){
  dd <- split(d, d[["file"]])
  lapply(dd, function(x){
    as.list(x[tolower(x$required)=="yes", "variable"])$variable
  })
}


import_map <- function(file, method="excel", ...){
    if(method=="excel"){
        ll <- import_excel(file)
    } else {
        ll <- import_gsheets(file, ...)
    }

    map <- tf_map(ll[["tfdct"]])
    map$table$required_columns <- table_required(ll[["coldct"]])
    map$column <- columns_map(ll[["coldct"]])
    stats::setNames(map, c("file_level", "table_level", "column_level") )
}

home <- cl::l_drive_go("swiper/DataScience/valiData_remix")
file <- "Core_Data_Dictionary_DS_longforms.xlsx"
# path <- cl::go(home, file)
path1 <- cl::go("~/Desktop", file)
cl::tic()
map <- import_map(path1)
cl::toc()

#valiData()
# path <- 'C:\\Users\\trinker\\Desktop\\hofstra'
# path <- csv_subpaths[1]
