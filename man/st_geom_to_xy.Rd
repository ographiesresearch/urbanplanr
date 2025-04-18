% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/spatial.R
\name{st_geom_to_xy}
\alias{st_geom_to_xy}
\title{Geometry to XY Columns}
\usage{
st_geom_to_xy(df, cols = c("x", "y"), crs = 4326, retain_geom = FALSE, ...)
}
\arguments{
\item{df}{A dataframe with point geometries.}

\item{cols}{Character vector of names for x and y columns. (Default 
`c("x", "y")`.)}

\item{retain_geom}{Boolean. If `FALSE` (default), returns point geometry. If
`TRUE`, returns original geometries}

\item{...}{Arguments passed on to `st_multi_type_center()`.}
}
\value{
Simple features dataframe.
}
\description{
Takes a simple features dataframe containing geometries and returns
the frame with additional columns containing x and y coordinates. If
`retain_geom` is TRUE, returns with original (non-point) geometry.
}
