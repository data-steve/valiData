#' Switch Method Based On Type Of Data in Column
#'
#' Switch Method Based On Type Of Data in Column
#'
#' @param x character vector
#' @param type :  date_utc, datetime, email, enumerated, integer, logical, numeric, zipcode, cipcode
#' guid,enumerated are handled outside by \code{vc_unique} and \code{vc_categories}
#' @param colnames the vector's colname
#' @export

vc_type <- function(x, type, colnames = "the column"  ){

	# message <- function(x, type, colnames = "the column"  ) {
    # browser()
		switch(type,
			   "guid" = return(NULL),
               # "guid" = TRUE,
			   "credit" = vc_credits(x, colnames = colnames  ),
			   "datetime" = vc_iso_datetime(x, colnames = colnames  ),
			   "date_utc" = vc_date_utc(x, colnames = colnames  ),
			   "cip" = vc_cipcode(x, colnames = colnames  ),
			   "email" = vc_email(x, colnames = colnames  ),
			   "integer" = vc_integer(x, colnames = colnames  ),
			   "zip" = vc_zip(x, colnames = colnames  ),
			   "logical" = vc_categories(x, levels = c("true", "false"), colnames = colnames ),
			   "enumerated" = return(NULL),
               # "enumerated" = TRUE,
			   "string" = return(NULL)
               # "string" = TRUE
		)

	# }
}
