% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/spatial.R
\name{st_elevate_points}
\alias{st_elevate_points}
\title{Add Z Dimension to Points from Column}
\usage{
st_elevate_points(df, cols = c("x", "y", "z"))
}
\arguments{
\item{df}{Simple features data frame or tibble. POINT type.}

\item{cols}{Character vector. Contains names of x, y, and z columns.}
}
\value{
Simple features data frame or tibble.
}
\description{
Add Z Dimension to Points from Column
}
