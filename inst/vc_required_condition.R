vc_required_condition <- function(data, x, ...){

    compared <- vc_compare(data, "ParentIdentifier", "OrgUnitIdentifier", "!=")
    # Steve 1/5/2016 We set length >1 because ParentIdentifier can be
    # empty when is it at institution level.
    # since vt_required_columns is also reporting on it, we thought this more clarified
    if (!(compared && length(non_responses[["locations"]]) != 0)){

        institutions <- which(data[["Type"]] %in% c("institution", "Institution"))

        missings <- setdiff(non_responses[["locations"]], institutions)

		compared[["message"]] <- paste0(
			    "The following report is actually an error if and only if \n",
			    "the column causing the error is not ParentIdentifier OR\n",
			    "it is ParentIdentifier and the missing observations aren't Instituion Level.\n\n",
			    compared[["message"]]
		    )
    }

    compared[["call"]] <- 'vc_required_condition'
    compared

}
