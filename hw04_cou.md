hw04\_couBC
================
CouBC
2018-10-07

Task 1 - Data Re-Shaping
========================

In Window functions, we formed a tibble with 24 rows: 2 per year, giving the country with both the lowest and highest life expectancy (in Asia). Take that table (or a similar one for all continents) and reshape it so you have one row per year or per year \* continent combination.

First, I load gapminder and tidyverse.

``` r
library(gapminder)
library(tidyverse)
```

    ## ── Attaching packages ────────────────────────────────── tidyverse 1.2.1 ──

    ## ✔ ggplot2 3.0.0     ✔ purrr   0.2.5
    ## ✔ tibble  1.4.2     ✔ dplyr   0.7.6
    ## ✔ tidyr   0.8.1     ✔ stringr 1.2.0
    ## ✔ readr   1.1.1     ✔ forcats 0.3.0

    ## ── Conflicts ───────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()

Then I filter gapminder for the highest and lowest life expectancies in Asia and store this in R as tidy\_Asia.

``` r
tidy_Asia <- gapminder %>%
  filter(continent == "Asia") %>%
  select(year, country, lifeExp) %>%
  group_by(year) %>%
  filter(min_rank(desc(lifeExp)) < 2 | min_rank(lifeExp) < 2) %>% 
  arrange(year) %>%
  print(n = Inf)
```

    ## # A tibble: 24 x 3
    ## # Groups:   year [12]
    ##     year country     lifeExp
    ##    <int> <fct>         <dbl>
    ##  1  1952 Afghanistan    28.8
    ##  2  1952 Israel         65.4
    ##  3  1957 Afghanistan    30.3
    ##  4  1957 Israel         67.8
    ##  5  1962 Afghanistan    32.0
    ##  6  1962 Israel         69.4
    ##  7  1967 Afghanistan    34.0
    ##  8  1967 Japan          71.4
    ##  9  1972 Afghanistan    36.1
    ## 10  1972 Japan          73.4
    ## 11  1977 Cambodia       31.2
    ## 12  1977 Japan          75.4
    ## 13  1982 Afghanistan    39.9
    ## 14  1982 Japan          77.1
    ## 15  1987 Afghanistan    40.8
    ## 16  1987 Japan          78.7
    ## 17  1992 Afghanistan    41.7
    ## 18  1992 Japan          79.4
    ## 19  1997 Afghanistan    41.8
    ## 20  1997 Japan          80.7
    ## 21  2002 Afghanistan    42.1
    ## 22  2002 Japan          82  
    ## 23  2007 Afghanistan    43.8
    ## 24  2007 Japan          82.6

In order to reshape it so that I have one row per year, I will be spreading the table rather than gathering.

``` r
tidy_Asia %>%
  arrange(lifeExp)
```

    ## # A tibble: 24 x 3
    ## # Groups:   year [12]
    ##     year country     lifeExp
    ##    <int> <fct>         <dbl>
    ##  1  1952 Afghanistan    28.8
    ##  2  1957 Afghanistan    30.3
    ##  3  1977 Cambodia       31.2
    ##  4  1962 Afghanistan    32.0
    ##  5  1967 Afghanistan    34.0
    ##  6  1972 Afghanistan    36.1
    ##  7  1982 Afghanistan    39.9
    ##  8  1987 Afghanistan    40.8
    ##  9  1992 Afghanistan    41.7
    ## 10  1997 Afghanistan    41.8
    ## # ... with 14 more rows

``` r
#From here I can see that Afghanistan in 2007 has the maximum lowest lifeExp of 43.8. 
```

``` r
tidy_Asia %>% 
  mutate(max_Asia = lifeExp >50) %>% 
  mutate(min_Asia = lifeExp <50)
```

    ## # A tibble: 24 x 5
    ## # Groups:   year [12]
    ##     year country     lifeExp max_Asia min_Asia
    ##    <int> <fct>         <dbl> <lgl>    <lgl>   
    ##  1  1952 Afghanistan    28.8 FALSE    TRUE    
    ##  2  1952 Israel         65.4 TRUE     FALSE   
    ##  3  1957 Afghanistan    30.3 FALSE    TRUE    
    ##  4  1957 Israel         67.8 TRUE     FALSE   
    ##  5  1962 Afghanistan    32.0 FALSE    TRUE    
    ##  6  1962 Israel         69.4 TRUE     FALSE   
    ##  7  1967 Afghanistan    34.0 FALSE    TRUE    
    ##  8  1967 Japan          71.4 TRUE     FALSE   
    ##  9  1972 Afghanistan    36.1 FALSE    TRUE    
    ## 10  1972 Japan          73.4 TRUE     FALSE   
    ## # ... with 14 more rows

Task 2:
=======

Create your own cheatsheet patterned after Jenny’s but focused on something you care about more than comics.

You will likely need to iterate between your data prep and your joining to make your explorations comprehensive and interesting. For example, you will want a specific amount (or lack) of overlap between the two data.frames, in order to demonstrate all the different joins. You will want both the data frames to be as small as possible, while still retaining the expository value

``` r
places_visited <- "
city, country, beach
  Tokyo, Japan, no
  Dubai, United Arab Emirates, yes
  Istanbul, Turkey, yes
  Rome, Italy, no
  San Antonio, United States, no
  Merida, Mexico, no
"

places_visited <- read_csv(places_visited, skip = 1)
places_visited
```

    ## # A tibble: 6 x 3
    ##   city        country              beach
    ##   <chr>       <chr>                <chr>
    ## 1 Tokyo       Japan                no   
    ## 2 Dubai       United Arab Emirates yes  
    ## 3 Istanbul    Turkey               yes  
    ## 4 Rome        Italy                no   
    ## 5 San Antonio United States        no   
    ## 6 Merida      Mexico               no

``` r
airports <- "
airports, country, airport_quality
  Narita, Japan, 4
  Dubai, United Arab Emirates, 6
  Ataturk, Turkey, 2
  Leonardo da Vinci, Italy, 3
  San Antonio, United States, 1
  Schiphol, Netherlands, 5
"
airports <- read_csv(airports, skip = 1)
airports
```

    ## # A tibble: 6 x 3
    ##   airports          country              airport_quality
    ##   <chr>             <chr>                          <int>
    ## 1 Narita            Japan                              4
    ## 2 Dubai             United Arab Emirates               6
    ## 3 Ataturk           Turkey                             2
    ## 4 Leonardo da Vinci Italy                              3
    ## 5 San Antonio       United States                      1
    ## 6 Schiphol          Netherlands                        5

left\_join(places\_visited, airports)
-------------------------------------

``` r
ljpa <- left_join(places_visited, airports) %>% 
  knitr::kable()
```

    ## Joining, by = "country"

``` r
ljpa
```

| city        | country              | beach | airports          |  airport\_quality|
|:------------|:---------------------|:------|:------------------|-----------------:|
| Tokyo       | Japan                | no    | Narita            |                 4|
| Dubai       | United Arab Emirates | yes   | Dubai             |                 6|
| Istanbul    | Turkey               | yes   | Ataturk           |                 2|
| Rome        | Italy                | no    | Leonardo da Vinci |                 3|
| San Antonio | United States        | no    | San Antonio       |                 1|
| Merida      | Mexico               | no    | NA                |                NA|

``` r
#with left join places_visited then airports, I am missing Schiphol airport (Netherlands) from the airports table. This left_join is matched by 'country'.
```

left\_join(airports, places\_visited)
-------------------------------------

``` r
ljap <- left_join(airports, places_visited) %>% 
knitr::kable()
```

    ## Joining, by = "country"

``` r
ljap
```

| airports          | country              |  airport\_quality| city        | beach |
|:------------------|:---------------------|-----------------:|:------------|:------|
| Narita            | Japan                |                 4| Tokyo       | no    |
| Dubai             | United Arab Emirates |                 6| Dubai       | yes   |
| Ataturk           | Turkey               |                 2| Istanbul    | yes   |
| Leonardo da Vinci | Italy                |                 3| Rome        | no    |
| San Antonio       | United States        |                 1| San Antonio | no    |
| Schiphol          | Netherlands          |                 5| NA          | NA    |

``` r
#with left_join airport first, I retain Schiphol Airport (Netherlands) but lose Merida, Mexico as a place visited. Interpretation wise, this is an accurate depiction of my travel history as I've been to Schiphol airport but never been to Amsterdam. 
```

right\_join(places\_visited, airports)
--------------------------------------

``` r
rjpa <- right_join(places_visited, airports) 
```

    ## Joining, by = "country"

``` r
rjpa
```

    ## # A tibble: 6 x 5
    ##   city        country              beach airports          airport_quality
    ##   <chr>       <chr>                <chr> <chr>                       <int>
    ## 1 Tokyo       Japan                no    Narita                          4
    ## 2 Dubai       United Arab Emirates yes   Dubai                           6
    ## 3 Istanbul    Turkey               yes   Ataturk                         2
    ## 4 Rome        Italy                no    Leonardo da Vinci               3
    ## 5 San Antonio United States        no    San Antonio                     1
    ## 6 <NA>        Netherlands          <NA>  Schiphol                        5

``` r
# similar to left_join, I lose a bit of information. In Right_join(places_visited, airports) I lose out on visiting Merida, Mexico because I did not fly there. 
```

right\_join(airports, places\_visited)
--------------------------------------

``` r
rjap <- 
  right_join(airports, places_visited)
```

    ## Joining, by = "country"

``` r
rjap
```

    ## # A tibble: 6 x 5
    ##   airports          country              airport_quality city        beach
    ##   <chr>             <chr>                          <int> <chr>       <chr>
    ## 1 Narita            Japan                              4 Tokyo       no   
    ## 2 Dubai             United Arab Emirates               6 Dubai       yes  
    ## 3 Ataturk           Turkey                             2 Istanbul    yes  
    ## 4 Leonardo da Vinci Italy                              3 Rome        no   
    ## 5 San Antonio       United States                      1 San Antonio no   
    ## 6 <NA>              Mexico                            NA Merida      no

``` r
#Here it depicts that I've been to Merida, Mexico but gives no quality of rating of airport.
```

``` r
ijpa <- inner_join(places_visited, airports)
```

    ## Joining, by = "country"

``` r
ijpa
```

    ## # A tibble: 5 x 5
    ##   city        country              beach airports          airport_quality
    ##   <chr>       <chr>                <chr> <chr>                       <int>
    ## 1 Tokyo       Japan                no    Narita                          4
    ## 2 Dubai       United Arab Emirates yes   Dubai                           6
    ## 3 Istanbul    Turkey               yes   Ataturk                         2
    ## 4 Rome        Italy                no    Leonardo da Vinci               3
    ## 5 San Antonio United States        no    San Antonio                     1

``` r
#Inner_join retains only rows in both sets. Here I miss out on Merida and Schiphol Airport
```

``` r
ijap <- inner_join(airports, places_visited)
```

    ## Joining, by = "country"

``` r
ijap
```

    ## # A tibble: 5 x 5
    ##   airports          country              airport_quality city        beach
    ##   <chr>             <chr>                          <int> <chr>       <chr>
    ## 1 Narita            Japan                              4 Tokyo       no   
    ## 2 Dubai             United Arab Emirates               6 Dubai       yes  
    ## 3 Ataturk           Turkey                             2 Istanbul    yes  
    ## 4 Leonardo da Vinci Italy                              3 Rome        no   
    ## 5 San Antonio       United States                      1 San Antonio no

``` r
#Similarly, I miss out on Merida and Schiphol Airport
```

``` r
sjpa <- semi_join(places_visited, airports)
```

    ## Joining, by = "country"

``` r
sjpa
```

    ## # A tibble: 5 x 3
    ##   city        country              beach
    ##   <chr>       <chr>                <chr>
    ## 1 Tokyo       Japan                no   
    ## 2 Dubai       United Arab Emirates yes  
    ## 3 Istanbul    Turkey               yes  
    ## 4 Rome        Italy                no   
    ## 5 San Antonio United States        no

``` r
#in semi-join - only rows that have a match in places_visited and airports are retained, losing columns re: airport and airport quality
```

``` r
sjap <- semi_join(airports, places_visited)
```

    ## Joining, by = "country"

``` r
sjap
```

    ## # A tibble: 5 x 3
    ##   airports          country              airport_quality
    ##   <chr>             <chr>                          <int>
    ## 1 Narita            Japan                              4
    ## 2 Dubai             United Arab Emirates               6
    ## 3 Ataturk           Turkey                             2
    ## 4 Leonardo da Vinci Italy                              3
    ## 5 San Antonio       United States                      1

``` r
#Here I lose out on city and beach columns.
```
