% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Tranche2.R
\name{InputTranche2}
\alias{InputTranche2}
\title{InputTranche2}
\usage{
InputTranche2(config, T1)
}
\arguments{
\item{config}{The config file for the input dataset}

\item{SPDF}{The Tranche 1 polygons, preprocessed in SF format}
}
\value{
SPDF
}
\description{
Formerly ProcessTranche2Dataset.R, this function assigns (associates) as many hotspots/point
 fire locations/reports as possible to the known fire perimeters (Tranche1) without clumping the hotspots first.
 Only then create via clumping or other methods a set of final/current Tranche2 polygons
 Assign (associate) the individual hotspots / point reports to these "Derived Polygons".
}
\examples{
InputTranche2('./config/MN_DNR_WF')
}

