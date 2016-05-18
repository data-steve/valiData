#' Switch Method Based On Type Of Data in Column
#'
#' Switch Method Based On Type Of Data in Column
#'
#' @param data A data frame.
#' @param x Column name from \code{data} (character string).
#' @param type :  date_utc, datetime, email, enumerated, integer, logical, numeric, zipcode, cipcode
#' guid,enumerated are handled outside by \code{vc_unique} and \code{vc_categories}
#' @param \dots ignored.
#' @export
vc_type <- function(data, x, type, ...){

	# message <- function(x, type, colname_x = "the column"  ) {
    # browser()
		switch(type,
			   "guid" = return(NULL),
               # "guid" = TRUE,
			   "credit" = vc_credits(data, x),
			   "datetime" = vc_iso_datetime(data, x),
			   "date_utc" = vc_utc_date(data, x),
			   "cip" = vc_cipcode(data, x),
			   "email" = vc_email(data, x),
			   "integer" = vc_integer(data, x),
			   "zip" = vc_zipcode(data, x),
			   "logical" = vc_categories(data, x, levels = c("true", "false")),
			   "enumerated" = return(NULL),
               "numeric" = vc_numeric(data, x),
			   "string" = return(NULL)
		)

	# }
}
