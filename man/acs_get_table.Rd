% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/acs.R
\name{acs_get_table}
\alias{acs_get_table}
\title{Get 5-Year ACS Estimates Based on Variable List}
\usage{
acs_get_table(
  table,
  states,
  year,
  census_unit,
  crs = 4326,
  county = NULL,
  geometry = TRUE,
  var_match = "",
  var_suffix = TRUE
)
}
\arguments{
\item{table}{The ACS table for which you would like to request all
variables.}

\item{states}{Character vector of state abbreviations.}

\item{year}{Integer. Final year of ACS estimates.}

\item{census_unit}{Census unit for which to download tables.}

\item{crs}{Target coordinate reference system: object of class \code{crs}.}

\item{county}{Character. County name.}

\item{geometry}{Boolean. If \code{TRUE} (default) retains Tiger/LINE geography.}

\item{var_match}{Character. Retain only variables matching this string.}

\item{var_suffix}{Boolean. If \code{TRUE} (default), adjusts for variable
suffixes.}
}
\value{
\code{sf} object if \code{geometry} is \code{TRUE}. Else data frame.
}
\description{
Get 5-Year ACS Estimates Based on Variable List
}
