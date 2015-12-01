#' Switch Method Based On Type Of Data in Column
#'
#' Switch Method Based On Type Of Data in Column
#'
#' @param x character vector
#' @param type :  date_utc, datetime, email, enumerated, integer, logical, numeric, zipcode, cipcode
#' guid,enumerated are handled outside by \code{vc_unique} and \code{vc_categories}
#' @param colname_x the vector's colname
#' @export

vc_type <- function(x, type, colname_x = "the column"  ){

	# message <- function(x, type, colname_x = "the column"  ) {
    # browser()
		switch(type,
			   "guid" = return(NULL),
               # "guid" = TRUE,
			   "credit" = vc_credits(x, colname_x = colname_x  ),
			   "datetime" = vc_iso_datetime(x, colname_x = colname_x  ),
			   "date_utc" = vc_utc_date(x, colname_x = colname_x  ),
			   "cip" = vc_cipcode(x, colname_x = colname_x  ),
			   "email" = vc_email(x, colname_x = colname_x  ),
			   "integer" = vc_integer(x, colname_x = colname_x  ),
			   "zip" = vc_zipcode(x, colname_x = colname_x  ),
			   "logical" = vc_categories(x, levels = c("true", "false"), colname_x = colname_x ),
			   "enumerated" = return(NULL),
               # "enumerated" = TRUE,
			   "string" = return(NULL)
               # "string" = TRUE
		)

	# }
}
