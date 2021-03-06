% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/clean_table.R
\name{add_na}
\alias{add_na}
\title{add_na}
\usage{
add_na(wiki_table, to_na = "", special_to_na = TRUE)
}
\arguments{
\item{wiki_table}{A dataframe}

\item{to_na}{A character string that when solitary in a dataframe cell is to be converted to NA. Default is "".}

\item{special_to_na}{A boolean denoting whether solitary special characters in dataframe cells are to be converted to NA. Default is TRUE.}
}
\value{
Cleaned dataframe
}
\description{
add_na
}
