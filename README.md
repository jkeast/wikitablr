# wikitablr
Simple Reader for Wikipedia Tables

[![Travis-CI Build Status](https://travis-ci.org/jkeast/wikitablr.svg?branch=master)](https://travis-ci.org/jkeast/wikitablr) 

`wikitablr` is an R package that has the tools to simply webscrape tables from wikipedia, and clean for common formatting issues. The intention here is to empower beginners to explore data on practically any subject that interests them (as long as there's a wikipedia table on it), but anyone can utilize this package.

`wikitablr` also takes data that looks like this:
``` r
head(read_no_clean("https://en.wikipedia.org/wiki/List_of_songs_recorded_by_the_Beatles"))
```
and makes it look like this:
``` r
head(read_wiki_table("https://en.wikipedia.org/wiki/List_of_songs_recorded_by_the_Beatles"))
```

## Installation
`wikitablr` is not yet on CRAN. Install by running: 
```devtools::install_github("wikitablr")```
```library(wikitablr)```



