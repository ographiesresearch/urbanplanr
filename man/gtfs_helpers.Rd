% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/gtfs.R
\name{gtfs_helpers}
\alias{gtfs_helpers}
\alias{gtfs_routes}
\alias{gtfs_stops}
\alias{gtfs_process}
\title{GTFS Helper Functions}
\usage{
gtfs_routes(gtfs, crs)

gtfs_stops(gtfs, crs)

gtfs_process(gtfs, crs)
}
\arguments{
\item{gtfs}{A \code{tidygtfs} object.}

\item{crs}{\code{crs} object or EPSG code.}
}
\value{
An \code{sf} object
}
\description{
\code{gtfs_routes()} takes a \code{tidygtfs} object and returns routes as \code{sf}.
\code{gtfs_stops()} takes a \code{tidygtfs} object and returns stops as \code{sf}.
\code{gtfs_process()} removes stops with no lat-longs and builds \code{sf} objects.
}
