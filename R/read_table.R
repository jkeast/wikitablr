#' @name read_wikinodes
#' @title read_wikinodes
#' @importFrom dplyr %>%
#' @param url A character vector of the url of a wikipedia page containing a table. Default is 1.
#' @param replace_linebreak A character string denoting what linebreaks within dataframe cells are to be replaced with. Default is ' ,'.
#' @param ... passes arguments to read_wikitables()
#' @return list of xlm nodes
#' @examples
#'
#' @export


# You can learn more about package authoring with RStudio at: http://r-pkgs.had.co.nz/ Some
# useful keyboard shortcuts for package authoring: Build and Reload Package: 'Ctrl + Shift + B'
# Check Package: 'Ctrl + Shift + E' Test Package: 'Ctrl + Shift + T'
read_wikinodes <- function(url, replace_linebreak = ", ", ...) {
  # read in html from webpage
  wiki_nodes <- xml2::read_html(url)

  # replace line breaks in cells with comma
  comma <- rvest::xml_node(xml2::read_xml(paste("<wiki_table><span>", "</span></wiki_table>", sep = replace_linebreak)),
                           "span")
  xml2::xml_add_sibling(rvest::xml_nodes(wiki_table, "br"), comma)

  # extract table from html
  wiki_nodes <- wiki_nodes %>%
    rvest::html_nodes("table.wikitable")

}


#' @name read_wikitables
#' @title read_wikitables
#' @importFrom dplyr %>%
#' @param url A character vector of the url of a wikipedia page containing a table. Default is 1.
#' @param ... passes arguments from read_wikinodes
#' @return list of tbls
#' @examples
#'
#' @export

read_wikitables <- function(url, ...) {
  wiki_table <- read_wikinodes(url) %>%
    rvest::html_table(fill = TRUE)

  return(wiki_table)

}


read_tables <- function(url, ...) {

}

