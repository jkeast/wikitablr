
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

This is the first table on the wikipedia page "List of colleges and universities in Massachusetts":

``` r
example2 <- read_all_tables("https://en.wikipedia.org/wiki/List_of_colleges_and_universities_in_Massachusetts")

head(example2[[1]])
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

This is the second table on the page:

``` r
head(example2[[2]])
#>   abbreviation                                    accrediting_agency
#> 1        AAMFT  American Association for Marriage and Family Therapy
#> 2         AANA            American Association of Nurse Anesthetists
#> 3         ACPE          Accreditation Council for Pharmacy Education
#> 4        ACCSC Accrediting Commission of Career Schools and Colleges
#> 5          ABA                              American Bar Association
#> 6        ABFSE           American Board of Funeral Service Education
```

And third:

``` r
head(example2[[3]])
#>                                school       location control
#> 1        Andover Theological Seminary        Andover Private
#> 2              Andover Junior College        Andover Private
#> 3       Andover Institute of Business        Andover Private
#> 4 Andover Newton Theological Seminary         Newton Private
#> 5                     Aquinas College Milton, Newton Private
#> 6                ArsDigita University      Cambridge Private
#>                        type founded closed
#> 1 Special-focus institution    1837   1965
#> 2       Associate's college    <NA>   1979
#> 3       Associate's college    1961   <NA>
#> 4 Special-focus institution    1965   2017
#> 5       Associate's college    1956   1999
#> 6 Special-focus institution    2000   2001
#>                                                  ref
#> 1                                               <NA>
#> 2                                               <NA>
#> 3                                               <NA>
#> 4 Moved to New Haven as part of Yale Divinity School
#> 5                                               <NA>
#> 6                                               <NA>
```
