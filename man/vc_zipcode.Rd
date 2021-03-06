% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/vc_zipcode.R
\name{vc_zipcode}
\alias{vc_zipcode}
\title{Validates If ZipCode}
\usage{
vc_zipcode(data, x, ...)
}
\arguments{
\item{data}{A data frame.}

\item{x}{Column name from \code{data} (character string).}

\item{\dots}{ignored.}
}
\description{
Validates If ZipCode
}
\examples{
dat <- data.frame(
    zip1 = c('14222', '14222-1234', '123456', 'dog', '', 'NULL', 'NA'),
    zip2 = c('14222', '14222-1234', '12345', 'n/a', '', 'NULL', 'NA'),
    stringsAsFactors = FALSE
)
vc_zipcode(dat, 'zip1')
vc_zipcode(dat, 'zip2')
}

