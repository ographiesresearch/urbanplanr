% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/db.R
\name{db_exists}
\alias{db_exists}
\title{Test Whether Database Exists}
\usage{
db_exists(conn, dbname)
}
\arguments{
\item{conn}{Database connection like that created by \code{RPostgres::dbConnect()}.}

\item{dbname}{Character. Name of database.}
}
\value{
Boolean.
}
\description{
Test Whether Database Exists
}
