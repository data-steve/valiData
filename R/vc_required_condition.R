vc_required_condition <- function(x, colname_x = "The Empty Column", data){

    non_responses <- vc_non_response(x)
    compared <- vc_compare(data[["ParentIdentifier"]], data[["OrgUnitIdentifier"]], "!=", colname_x ="ParentIdentifier", colname_y ="OrgUnitIdentifier")
    # Steve 1/5/2016 We set length >1 because ParentIdentifier can be
    # empty when is it at institution level.
    # since vt_required is also reporting on it, we thought this more clarified
    if (!(compared && length(non_responses[["locations"]]) != 0)){

        institutions <- which(data[["Type"]] %in% c("institution", "Institution"))

        missings <- setdiff(non_responses[["locations"]], institutions)

		message <- sprintf(
			paste0("The following report is actually an error if and only if \n",
			"the column causing the error is not ParentIdentifier OR\n",
			"it is ParentIdentifier and the missing observations aren't Instituion Level.\n\n",
			"The following rows of column %s are required and missing:\n\n%s\n\n\n\n"),
			sQuote(colname_x)
			, output_truncate(missings))
		cat(message)
    }



    return(compared && length(non_responses[["locations"]]) != 0)
}
