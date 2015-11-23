vc_nchar <- function(x, nchar, colnames = "the column" ){

	x[x %in% c("", "NULL", "NA", "N/A", "na", "n/a")] <- NA


	is_nchar <- nchar(x, keepNA=TRUE) <= nchar
	are_nchar <- all(is_nchar|is.na(x))
# if(8386 %in% 	which(!is_nchar)) browser()
	if (!are_nchar){
		message <- sprintf(
			"The following rows of column %s are not less than or equal to %s characters long:\n\n%s\n\n\n\n",
			sQuote(colnames)
			, nchar
			, paste(which(!is_nchar)
					,collapse=", "))
		cat(message)
	}

	return(are_nchar)
}
