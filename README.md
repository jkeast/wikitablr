
<!-- README.md is generated from README.Rmd. Please edit that file -->
wikitablr <img src="man/figures/wikitablr_hex_logo.png" align="right" height=140/>
==================================================================================

<!-- badges: start -->
[![Travis-CI Build Status](https://travis-ci.org/jkeast/wikitablr.svg?branch=master)](https://travis-ci.org/jkeast/wikitablr) <!-- badges: end -->

`wikitablr` is an R package that has the tools to simply webscrape tables from wikipedia, and clean for common formatting issues. The intention here is to empower beginners to explore data on practically any subject that interests them (as long as there's a wikipedia table on it), but anyone can utilize this package.

`wikitablr` takes data that looks like this:

    #>   X1                                                       X2
    #> 1 NA Indicates song not written by the members of the Beatles
    #> 2 NA                                 Indicates live recording

and makes it look like this:

    #>                     song  core_catalogue_release                    songwriter
    #> 1  "Across the Universe" Let It Be, Past Masters             Lennon, McCartney
    #> 2        "Act Naturally"                   Help! Johnny Russell, Voni Morrison
    #> 3   "All I've Got to Do"        With the Beatles             Lennon, McCartney
    #> 4        "All My Loving"        With the Beatles             Lennon, McCartney
    #> 5     "All Together Now"        Yellow Submarine             Lennon, McCartney
    #> 6 "All You Need Is Love"    Magical Mystery Tour             Lennon, McCartney
    #>                 lead_vocal year ref
    #> 1              John Lennon 1969  44
    #> 2              Ringo Starr 1965  46
    #> 3                   Lennon 1963  47
    #> 4           Paul McCartney 1963  47
    #> 5 McCartney, (with Lennon) 1969  48
    #> 6                   Lennon 1967  49

Installation
------------

You can install the development version from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("jkeast/wikitablr")
```

Example
-------

`wikitablr` allows you to either read in and clean your code all at once:

``` r
#read in first table on page
head(read_wiki_table("https://en.wikipedia.org/wiki/List_of_colleges_and_universities_in_Massachusetts"))
#>                           school         location                control
#> 1 American International College      Springfield Private not-for-profit
#> 2                Amherst College          Amherst Private not-for-profit
#> 3             Anna Maria College           Paxton Private not-for-profit
#> 4             Assumption College        Worcester Private not-for-profit
#> 5                 Babson College        Wellesley Private not-for-profit
#> 6   Bard College at Simon's Rock Great Barrington Private not-for-profit
#>                                type enrollment_2013_2014 founded accreditation
#> 1               Master's university                 2177    1885            17
#> 2             Baccalaureate college                 1817    1821            18
#> 3               Master's university                 1455    1946            19
#> 4               Master's university                 2813    1904            20
#> 5         Special-focus institution                 3250    1919            21
#> 6 Baccalaureate/associate's college                  354    1964            22
```

Or use its cleaning functions seperately:

``` r
#read in first table from url without cleaning it
example <- read_wiki_raw("https://en.wikipedia.org/wiki/List_of_colleges_and_universities_in_Massachusetts")
head(example)
#>                           School Location[note 1]             Control[1]
#> 1 American International College      Springfield Private not-for-profit
#> 2                Amherst College          Amherst Private not-for-profit
#> 3             Anna Maria College           Paxton Private not-for-profit
#> 4             Assumption College        Worcester Private not-for-profit
#> 5                 Babson College        Wellesley Private not-for-profit
#> 6   Bard College at Simon's Rock Great Barrington Private not-for-profit
#>                             Type[1] Enrollment[16], (2013–2014)  Founded
#> 1               Master's university                   2,177[17] 1885[17]
#> 2             Baccalaureate college                   1,817[18] 1821[18]
#> 3               Master's university                   1,455[19] 1946[19]
#> 4               Master's university                   2,813[20] 1904[20]
#> 5         Special-focus institution                   3,250[21] 1919[21]
#> 6 Baccalaureate/associate's college                     354[22] 1964[22]
#>             Accreditation[16]
#> 1 AOTA, APTA, CCNE, NEASC[17]
#> 2                   NEASC[18]
#> 3      NASM, NEASC, NLNAC[19]
#> 4                   NEASC[20]
#> 5                   NEASC[21]
#> 6                   NEASC[22]
```

``` r
#clean names of table
example <- clean_wiki_names(example)

head(example)
#>                           school         location                control
#> 1 American International College      Springfield Private not-for-profit
#> 2                Amherst College          Amherst Private not-for-profit
#> 3             Anna Maria College           Paxton Private not-for-profit
#> 4             Assumption College        Worcester Private not-for-profit
#> 5                 Babson College        Wellesley Private not-for-profit
#> 6   Bard College at Simon's Rock Great Barrington Private not-for-profit
#>                                type enrollment_2013_2014  founded
#> 1               Master's university            2,177[17] 1885[17]
#> 2             Baccalaureate college            1,817[18] 1821[18]
#> 3               Master's university            1,455[19] 1946[19]
#> 4               Master's university            2,813[20] 1904[20]
#> 5         Special-focus institution            3,250[21] 1919[21]
#> 6 Baccalaureate/associate's college              354[22] 1964[22]
#>                 accreditation
#> 1 AOTA, APTA, CCNE, NEASC[17]
#> 2                   NEASC[18]
#> 3      NASM, NEASC, NLNAC[19]
#> 4                   NEASC[20]
#> 5                   NEASC[21]
#> 6                   NEASC[22]
```

``` r
#remove footnotes from table
head(remove_footnotes(example))
#>                           school         location                control
#> 1 American International College      Springfield Private not-for-profit
#> 2                Amherst College          Amherst Private not-for-profit
#> 3             Anna Maria College           Paxton Private not-for-profit
#> 4             Assumption College        Worcester Private not-for-profit
#> 5                 Babson College        Wellesley Private not-for-profit
#> 6   Bard College at Simon's Rock Great Barrington Private not-for-profit
#>                                type enrollment_2013_2014 founded
#> 1               Master's university                2,177    1885
#> 2             Baccalaureate college                1,817    1821
#> 3               Master's university                1,455    1946
#> 4               Master's university                2,813    1904
#> 5         Special-focus institution                3,250    1919
#> 6 Baccalaureate/associate's college                  354    1964
#>             accreditation
#> 1 AOTA, APTA, CCNE, NEASC
#> 2                   NEASC
#> 3      NASM, NEASC, NLNAC
#> 4                   NEASC
#> 5                   NEASC
#> 6                   NEASC
```

#### Read all tables

`wikitablr` also allows you to read in and clean all tables on a page at once, putting them into a list.

This is the first table on the wikipedia page "List of World Series champions":

``` r
example2 <- read_all_tables("https://en.wikipedia.org/wiki/List_of_World_Series_champions")

head(example2[[1]])
#>   year               winning_team         manager           games
#> 1 1903  Boston Americans (1, 1–0)   Jimmy Collins             5–3
#> 2 1904            No World Series No World Series No World Series
#> 3 1905   New York Giants (1, 1–0)     John McGraw             4–1
#> 4 1906 Chicago White Sox (1, 1–0)   Fielder Jones             4–2
#> 5 1907      Chicago Cubs (2, 1–1)    Frank Chance         4–0–(1)
#> 6 1908      Chicago Cubs (3, 2–1)    Frank Chance             4–1
#>                       losing_team       manager_1 ref
#> 1     Pittsburgh Pirates (1, 0–1)     Fred Clarke   8
#> 2                 No World Series No World Series   1
#> 3 Philadelphia Athletics (1, 0–1)     Connie Mack   9
#> 4           Chicago Cubs (1, 0–1)    Frank Chance  10
#> 5         Detroit Tigers (1, 0–1)   Hugh Jennings  11
#> 6         Detroit Tigers (2, 0–2)   Hugh Jennings  12
```

and the rest:

``` r
head(example2[[2]])
#>   apps                 team league series series_1 series_2 series_3 series_4
#> 1   40     New York Yankees     AL     27       13    0.675     2009     2009
#> 2   20 San Francisco Giants     NL      8       12      0.4     2014     2014
#> 3   20  Los Angeles Dodgers     NL      6       14      0.3     1988     2018
#> 4   19  St. Louis Cardinals     NL     11        8    0.579     2011     2013
#> 5   14    Oakland Athletics     AL      9        5    0.643     1989     1990
#> 6   13       Boston Red Sox     AL      9        4    0.692     2018     2018
#>   games games_1 games_2 games_3 games_4 founded
#> 1   134      90       1   0.598    2009    1901
#> 2    57      57       2     0.5    2014    1883
#> 3    48      64       0   0.429    2018    1884
#> 4    58      60       0   0.492    2013    1882
#> 5    41      34       0   0.547    1989    1901
#> 6    45      28       1    0.62    2018    1901

head(example2[[3]])
#>   apps                           team wins losses   win season
#> 1   40               New York Yankees   27     13 0.675   1921
#> 2   20  New York/San Francisco Giants    8     12   0.4   1905
#> 3   20   Brooklyn/Los Angeles Dodgers    6     14   0.3   1916
#> 4   19            St. Louis Cardinals   11      8 0.579   1926
#> 5   14 Philadelphia/Oakland Athletics    9      5 0.643   1905
#> 6   13                 Boston Red Sox    9      4 0.692   1903

head(example2[[4]])
#>   count                                   matchup record years
#> 1    11  Los Angeles Dodgers vs. New York Yankees      8  1941
#> 2     7 New York Yankees vs. San Francisco Giants      5  1921
#> 3     5  New York Yankees vs. St. Louis Cardinals      3  1926
#> 4     4       Atlanta Braves vs. New York Yankees      3  1957
#> 5     4    Boston Red Sox vs. St. Louis Cardinals      2  1946
#> 6     4           Chicago Cubs vs. Detroit Tigers      2  1907
```
