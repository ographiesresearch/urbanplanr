% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/spatial.R
\name{st_xyxy_to_lines}
\alias{st_xyxy_to_lines}
\title{XY-XY To Lines}
\usage{
st_xyxy_to_lines(
  df,
  names = c("x_h", "y_h", "x_w", "y_w"),
  retain_cols = FALSE
)
}
\arguments{
\item{df}{A dataframe with four columns containing two XY pairs.}

\item{names}{list of the names of the four columns in the form
\code{c("x_h", "y_h", "x_w", "y_w")}.}

\item{retain_cols}{Boolean. If \code{FALSE} (the default), do not retain the
OD coordinate columns.}
}
\value{
A dataframe with LINESTRING geometries.
}
\description{
Takes a dataframe with four columns containing two XY pairs (in the form
\verb{c('x_h}, \code{y_h}, \code{x_w}, \verb{y_w)}) and returns an sf dataframe with the same
}
