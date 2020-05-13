#' @name clean_wiki_names
#' @title clean_wiki_names
#' @importFrom dplyr %>%
#' @param wiki_table A dataframe for which the column names will be cleaned
#' @param ... arguments passed to \code{\link[janitor]{clean_names}}
#' @return a cleaned dataframe

clean_wiki_names_single <- function(wiki_table, ...) {
  #removes all columns without a name
  wiki_table <- wiki_table[!is.na(names(wiki_table))]
  # remove footnotes (which are in brackets) from column names
  names(wiki_table) <- stringr::str_remove_all(names(wiki_table), "\\[.*]")
  # remove "(s)" from column names
  names(wiki_table) <- stringr::str_remove_all(names(wiki_table), "\\(s\\)")
  # remove special characters from column names
  names(wiki_table) <- stringr::str_replace_all(names(wiki_table), "[^a-zA-Z0-9 ]", "_")
  # convert to snake case
  wiki_table <- wiki_table %>%
    janitor::clean_names(...)

  return(wiki_table)
}


#' @rdname clean_wiki_names
#' @param wiki_tables a list of dataframes for which the column names will be cleaned
#' @return a list of cleaned dataframes
#' @export

clean_wiki_names <- function(wiki_tables, ...) {
  purrr::map(wiki_tables, clean_wiki_names_single, ... = ...)
}


#' @name add_na
#' @title add_na_single
#' @param wiki_table A dataframe
#' @param to_na A character string that when solitary in a dataframe cell is to be converted to NA. Default is "".
#' @param special_to_na A boolean denoting whether solitary special characters in dataframe cells are to be converted to NA. Default is TRUE.
#' @return Cleaned dataframe

add_na_single <- function(wiki_table, to_na = "", special_to_na = TRUE){
  #converts specified characters to NA
  wiki_table <- as.data.frame(purrr::map(wiki_table, function(x){is.na(x) <- which(x %in% c("", to_na));x}))

  if(special_to_na){
    #converts solitary special characters to NA
    wiki_table <- as.data.frame(
      purrr::map(wiki_table, function(x) {
        is.na(x) <- which(stringr::str_detect(x, "\\A[^a-zA-Z0-9]{1}$"))
        x
        }
      )
    )
  }

  return(wiki_table)
}

#' @rdname add_na
#' @param wiki_tables a list of dataframes
#' @return a list of cleaned dataframes
#' @export

add_na <- function(wiki_tables, to_na = "", special_to_na = TRUE) {
  purrr::map(wiki_tables, add_na_single)
}

#' @rdname add_na
#' @param wiki_table a dataframe
#' @return a cleaned dataframe
#' @export

special_to_na_single <- function(wiki_table) {
  wiki_table %>%
    dplyr::mutate_if(
      is.character, stringr::str_replace_all,
      pattern = "\\A[^a-zA-Z0-9]{1}$", replacement = "<special_char/>"
    ) %>%
    dplyr::mutate_if(
      is.character, list(~dplyr::na_if(., "<special_char/>"))
    )
}

#' @rdname add_na
#' @param wiki_tables a list of dataframes
#' @return a list of cleaned dataframes
#' @export

special_to_na <- function(wiki_tables) {
  purrr::map(wiki_tables, special_to_na_single)
}


#' @name remove_footnotes
#' @title remove_footnotes
#' @param wiki_table a dataframe
#' @param ... arguments passed to \code{\link{add_na}}
#' @return a cleaned dataframe

remove_footnotes_single <- function(wiki_table, ...){
  as.data.frame(purrr::map(wiki_table, ~stringr::str_remove_all(.x, "\\[.*]"))) %>%
    add_na(...) %>%
    janitor::remove_empty(which = "cols")
}

#' @name remove_footnotes
#' @title remove_footnotes
#' @param wiki_tables A list of dataframes
#' @return Cleaned dataframe
#' @export

remove_footnotes <- function(wiki_tables, ...) {
  purrr::map(wiki_tables, remove_footnotes_single, ...)
}

#' @name clean_rows
#' @title clean_rows
#' @param wiki_table A dataframe for which the rows will be cleaned
#' @return Cleaned dataframe

clean_rows_single <- function(wiki_table) {
  #removes all columns without a name
  wiki_table <- wiki_table[!is.na(names(wiki_table))]

  #removes rows with the same value across all columns
  #line of code pulled from Psidom's stack overflow answer here:
  #https://stackoverflow.com/questions/44398252/remove-rows-with-the-same-value-across-all-columns
  wiki_table <- wiki_table[rowSums(wiki_table[-1] != wiki_table[[2]], na.rm = TRUE) != 0,]

  wiki_table <- wiki_table %>%
    dplyr::mutate_all(as.character)

    tryCatch({i <- 1

    while(i <= nrow(wiki_table)){

      #remove repeats of header in rows
      suppressWarnings(
        if(colnames(wiki_table) == wiki_table[i,]){
          wiki_table <- wiki_table[-c(i),]
          i <- i-1
        }
      )
      i <- i+1
    }}, error = function(e) cat("Error when checking for header duplicates in rows: ",e$message, "\n")
  )

  return(wiki_table)
}

#' @rdname clean_rows
#' @param wiki_tables A list of dataframes for which the rows will be cleaned
#' @return a list of cleaned dataframes
#' @export

clean_rows <- function(wiki_tables) {
  purrr::map(wiki_tables, clean_rows_single)
}


#' @name convert_types
#' @title convert_types
#' @param wiki_table A dataframe for which the rows will be cleaned
#' @return Cleaned dataframe

convert_types_single <- function(wiki_table) {
  suppressWarnings(
    wiki_table <- wiki_table %>%
      dplyr::mutate_all(as.character)%>%
      dplyr::mutate_if(~all(!is.na(lubridate::dmy(.x))), lubridate::dmy)%>%
      dplyr::mutate_if(~all(!is.na(lubridate::mdy(.x))), lubridate::mdy)%>%
      dplyr::mutate_if(~all(!is.na(lubridate::ymd(.x))), lubridate::ymd)%>%
      dplyr::mutate_if(~class(.x) == "character" &&
                         all(!stringr::str_detect(.x, "[a-zA-Z]"), na.rm = TRUE), readr::parse_number))

  return(wiki_table)

}


#' @rdname convert_types
#' @param wiki_tables A list of dataframes for which the rows will be cleaned
#' @return a list of cleaned dataframes
#' @export
convert_types <- function(wiki_tables) {
  purrr::map(wiki_tables, convert_types_single)
}
