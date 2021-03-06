% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/vf_csv_broken.R
\name{vf_csv_broken}
\alias{vf_csv_broken}
\title{Validate that a CSV's Is Properly Quoted}
\usage{
vf_csv_broken(path, ...)
}
\arguments{
\item{path}{Path to a .csv file.}

\item{\dots}{ignored.}
}
\description{
\code{vf_csv_broken} - Validates that a .csv file has properly quoted character
columns (is not a broken .csv).
}
\examples{
loc <- file.path(tempdir(), 'temp.csv')
cat(
    paste(c(",x,y", "1, the dog, wen,1", "2,door,2"), collapse="\\n"),
    file=loc
)
vf_csv_broken(loc)
}

