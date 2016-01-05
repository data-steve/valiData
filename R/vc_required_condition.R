vc_required_condition <- function(x, colname_x = "The Empty Column", data){

    non_responses <- vc_non_response(x)
    if (length(non_responses[["locations"]]) > 1){

        institutions <- which(data[["Type"]] %in% c("institution", "Institution"))

        missings <- setdiff(non_responses[["locations"]], institutions)

		message <- sprintf(
			"The following report is actually an error if and only if \n
			the column causing the error is not ParentIdentifier OR\n
			it is ParentIdentifier and the missing observations aren't Instituion Level.\n\n
			The following rows of column %s are required and missing:\n\n%s\n\n\n\n",
			sQuote(colname_x)
			, paste(missings
					,collapse=", "))
		cat(message)
    }

    compared <- vc_compare(data[["ParentIdentifier"]], data[["OrgUnitIdentifier"]], "!=", colname_x ="ParentIdentifier", colname_y ="OrgUnitIdentifier")

    return(compared && length(non_responses[["locations"]]) != 0)
}
