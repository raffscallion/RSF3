% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sfUtils.R
\name{MergeTranches}
\alias{MergeTranches}
\title{MergeTranches}
\usage{
MergeTranches(X, Y)
}
\arguments{
\item{X}{SPDF the first tranche to concatenate}

\item{Y}{SPDF the second tranche to concatenate}
}
\value{
SPDF A merged SPDF containing both sets
}
\description{
Concatenates two tranches.
}
\examples{
MergeTranches(T1, T2)
}

