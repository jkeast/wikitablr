
<!-- README.md is generated from README.Rmd. Please edit that file -->
wikitablr <img src="image/800px-Wikipedia-logo-v2-en.svg.png" align="right" height=140/>
========================================================================================

<!-- badges: start -->
[![Travis-CI Build Status](https://travis-ci.org/jkeast/wikitablr.svg?branch=master)](https://travis-ci.org/jkeast/wikitablr) <!-- badges: end -->

`wikitablr` is an R package that has the tools to simply webscrape tables from wikipedia, and clean for common formatting issues. The intention here is to empower beginners to explore data on practically any subject that interests them (as long as there's a wikipedia table on it), but anyone can utilize this package.

`wikitablr` takes data that looks like this:

    #>                        Song Core catalogue release(s)
    #> 1  "Across the Universe"[f]     Let It BePast Masters
    #> 2           "Act Naturally"                     Help!
    #> 3      "All I've Got to Do"          With the Beatles
    #> 4           "All My Loving"          With the Beatles
    #> 5        "All Together Now"          Yellow Submarine
    #> 6 "All You Need Is Love"[g]      Magical Mystery Tour
    #>                 Songwriter(s)       Lead vocal(s)[e] Year   Ref(s)
    #> 1             LennonMcCartney            John Lennon 1969 [44][45]
    #> 2 Johnny RussellVoni Morrison            Ringo Starr 1965     [46]
    #> 3             LennonMcCartney                 Lennon 1963     [47]
    #> 4             LennonMcCartney         Paul McCartney 1963     [47]
    #> 5             LennonMcCartney McCartney(with Lennon) 1969     [48]
    #> 6             LennonMcCartney                 Lennon 1967 [49][50]

and makes it look like this:

    #>                     song  core_catalogue_release                    songwriter
    #> 1  "Across the Universe" Let It Be, Past Masters             Lennon, McCartney
    #> 2        "Act Naturally"                   Help! Johnny Russell, Voni Morrison
    #> 3   "All I've Got to Do"        With the Beatles             Lennon, McCartney
    #> 4        "All My Loving"        With the Beatles             Lennon, McCartney
    #> 5     "All Together Now"        Yellow Submarine             Lennon, McCartney
    #> 6 "All You Need Is Love"    Magical Mystery Tour             Lennon, McCartney
    #>                 lead_vocal year
    #> 1              John Lennon 1969
    #> 2              Ringo Starr 1965
    #> 3                   Lennon 1963
    #> 4           Paul McCartney 1963
    #> 5 McCartney, (with Lennon) 1969
    #> 6                   Lennon 1967

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

Or use its cleaning functions seperately:

``` r
#read in first table from url without cleaning it
example <- read_no_clean("https://en.wikipedia.org/wiki/List_of_colleges_and_universities_in_Massachusetts")
head(example)
#>                           School Location[note 1]             Control[1]
#> 1 American International College      Springfield Private not-for-profit
#> 2                Amherst College          Amherst Private not-for-profit
#> 3             Anna Maria College           Paxton Private not-for-profit
#> 4             Assumption College        Worcester Private not-for-profit
#> 5                 Babson College        Wellesley Private not-for-profit
#> 6   Bard College at Simon's Rock Great Barrington Private not-for-profit
#>                             Type[1] Enrollment[16](2013–2014)  Founded
#> 1               Master's university                 2,177[17] 1885[17]
#> 2             Baccalaureate college                 1,817[18] 1821[18]
#> 3               Master's university                 1,455[19] 1946[19]
#> 4               Master's university                 2,813[20] 1904[20]
#> 5         Special-focus institution                 3,250[21] 1919[21]
#> 6 Baccalaureate/associate's college                   354[22] 1964[22]
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
head(clean_wiki_names(example))
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
#>                           School Location.note.1.             Control.1.
#> 1 American International College      Springfield Private not-for-profit
#> 2                Amherst College          Amherst Private not-for-profit
#> 3             Anna Maria College           Paxton Private not-for-profit
#> 4             Assumption College        Worcester Private not-for-profit
#> 5                 Babson College        Wellesley Private not-for-profit
#> 6   Bard College at Simon's Rock Great Barrington Private not-for-profit
#>                             Type.1. Enrollment.16..2013.2014. Founded
#> 1               Master's university                     2,177    1885
#> 2             Baccalaureate college                     1,817    1821
#> 3               Master's university                     1,455    1946
#> 4               Master's university                     2,813    1904
#> 5         Special-focus institution                     3,250    1919
#> 6 Baccalaureate/associate's college                       354    1964
#>         Accreditation.16.
#> 1 AOTA, APTA, CCNE, NEASC
#> 2                   NEASC
#> 3      NASM, NEASC, NLNAC
#> 4                   NEASC
#> 5                   NEASC
#> 6                   NEASC
```

#### Read all tables

`wikitablr` also allows you to read in and clean all tables on a page at once, putting them into a list.

This is the first table on the wikipedia page "List of highest grossing films":

``` r
example2 <- read_all_tables("https://en.wikipedia.org/wiki/List_of_highest-grossing_films")

head(example2[[1]])
#>   rank peak                        title worldwide_gross year
#> 1    1    1            Avengers: Endgame  $2,797,800,564 2019
#> 2    2    1                       Avatar  $2,789,679,794 2009
#> 3    3    1                      Titanic  $2,187,463,944 1997
#> 4    4    3 Star Wars: The Force Awakens  $2,068,223,624 2015
#> 5    5    4       Avengers: Infinity War  $2,048,359,754 2018
#> 6    6    3               Jurassic World  $1,671,713,208 2015
```

and a couple more:

``` r
head(example2[[2]])
#>   rank              title          worldwide_gross_2019 year
#> 1    1 Gone with the Wind                $3,728,000,000 1939
#> 2    2             Avatar                $3,273,000,000 2009
#> 3    3            Titanic $2,516,000,000T$3,099,000,000 1997
#> 4    4          Star Wars                $3,061,000,000 1977
#> 5    5  Avengers: Endgame              AE$2,798,000,000 2019
#> 6    6 The Sound of Music                $2,564,000,000 1965

head(example2[[3]])
#>   year                 title
#> 1 1915 The Birth of a Nation
#> 2 1916           Intolerance
#> 3 1917             Cleopatra
#> 4 1918                Mickey
#> 5 1919       The Miracle Man
#> 6 1920         Way Down East
#>                                        worldwide_gross   budget
#> 1 $50,000,000–100,000,000, $20,000,000+R ($5,200,000)R $110,000
#> 2                                       $1,000,000*RIN $489,653
#> 3                                           $500,000*R $300,000
#> 4                                           $8,000,000 $250,000
#> 5                                          $3,000,000R $120,000
#> 6                            $5,000,000R ($4,000,000)R $800,000

head(example2[[4]])
#>   established                 title record_setting_gross
#> 1        1915 The Birth of a Nation          $5,200,000R
#> 2        1940 The Birth of a Nation        $15,000,000R‡
#> 3        1940    Gone with the Wind         $32,000,000R
#> 4        1963    Gone with the Wind        $67,000,000R‡
#> 5        1966    The Sound of Music        $114,600,000R
#> 6        1971    Gone with the Wind       $116,000,000R‡
```
