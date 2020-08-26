#' @name read_wikinodes
#' @title read_wikinodes
#' @importFrom dplyr %>%
#' @param url A character vector of the URL of a wikipedia page
#' containing a table.
#' @param replace_linebreak A character string denoting what
#' linebreaks within dataframe cells are to be replaced with.
#' Default is \code{' ,'}.
#' @param ... arguments passed to \code{\link{read_wikitables}}
#' @return An \code{xml_nodeset}
#' @seealso \code{\link[rvest]{html_nodes}}
#' @export
#' @examples
#' url <- "https://en.wikipedia.org/wiki/List_of_presidents_of_the_United_States_by_age"
#' # returns xml_nodeset
#' prez_nodes <- read_wikinodes(url)
#' length(prez_nodes)

read_wikinodes <- function(url, replace_linebreak = ", ", ...) {
  # read in html from webpage
  wiki_nodes <- xml2::read_html(url)

  # replace line breaks in cells with comma
  comma <- rvest::xml_node(xml2::read_xml(paste("<wiki_table><span>", "</span></wiki_table>", sep = replace_linebreak)),
                           "span")
  xml2::xml_add_sibling(rvest::xml_nodes(wiki_nodes, "br"), comma)

  # extract table from html
  wiki_nodes %>%
    rvest::html_nodes("table.wikitable")
}


#' @rdname read_wikinodes
#' @importFrom dplyr %>%
#' @return A list of tbls
#' @seealso \code{\link[rvest]{html_table}}
#' @export
#' @examples
#' url <- "https://en.wikipedia.org/wiki/List_of_presidents_of_the_United_States_by_age"
#' raw <- read_wikitables2(url)
#' # pick out the biggest table
#' if (require(dplyr) && require(purrr)) {
#'   raw %>%
#'     arrange(desc(bytes)) %>%
#'     head(1) %>%
#'     pull(table) %>%
#'     purrr::pluck(1)
#'  }

read_wikitables <- function(url, ...) {
  raw <- read_wikinodes(url)
  out <- rvest::html_attrs(raw) %>%
    dplyr::bind_rows()
  out$table <- raw %>%
    rvest::html_table(fill = TRUE)
  out %>%
    dplyr::mutate(
      number = dplyr::row_number(),
      retrieved = Sys.time(),
      bytes = purrr::map(table, lobstr::obj_size),
      bytes = purrr::map_dbl(bytes, as.numeric)
    )
}

globalVariables(c("bytes"))
