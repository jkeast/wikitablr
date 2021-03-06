% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/read_table.R
\name{read_wiki_table}
\alias{read_wiki_table}
\title{read_wiki_table}
\usage{
read_wiki_table(
  url,
  table_number = 1,
  replace_linebreak = ", ",
  remove_footnotes = TRUE,
  to_na = "",
  special_to_na = TRUE,
  ...
)
}
\arguments{
\item{url}{A character vector of the url of a wikipedia page containing a table. Default is 1.}

\item{table_number}{A numeric denoting which number table the desired table is on the page (e.g. the second table on the page)}

\item{replace_linebreak}{A character string denoting what linebreaks within dataframe cells are to be replaced with. Default is ' ,'.}

\item{remove_footnotes}{A boolean denoting whether footnote markings are to be removed or not. Default is TRUE.}

\item{to_na}{A character string that when solitary in a dataframe cell is to be converted to NA. Default is ''.}

\item{special_to_na}{A boolean denoting whether solitary special characters in dataframe cells are to be converted to NA. Default is TRUE.}

\item{...}{passes arguments to clean_wiki_names()}
}
\value{
Dataframe of wikipedia table
}
\description{
read_wiki_table
}
\examples{
read_wiki_table('https://en.wikipedia.org/wiki/List_of_most-followed_Instagram_accounts')
read_wiki_table('https://en.wikipedia.org/wiki/List_of_cryptids', 3, remove_footnotes = FALSE)
read_wiki_table('https://en.wikipedia.org/wiki/List_of_Pixar_films')
}
