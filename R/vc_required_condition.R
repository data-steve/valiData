vc_required_condition <- function(x, colname_x = "The Empty Column", data){

    non_responses <- vc_non_response(x)
    if (length(non_responses[["locations"]]) != 0){

        institutions <- which(data[["Type"]] %in% c("institution", "Institution"))

        missings <- setdiff(non_responses[["locations"]], institutions)

		message <- sprintf(
			"The following rows of column %s are required and missing (and `Type` is not equal to \"Institution\"):\n\n%s\n\n\n\n",
			sQuote(colname_x)
			, paste(missings+1
					,collapse=", "))
		cat(message)
    }

    compared <- vc_compare(data[["ParentIdentifier"]], data[["OrgUnitIdentifier"]], "!=", colname_x ="ParentIdentifier", colname_y ="OrgUnitIdentifier")

    return(compared && length(non_responses[["locations"]]) != 0)
}
