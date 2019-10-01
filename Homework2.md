p8105\_hw2\_ebm2142
================
Elise Mantell
2019-10-04

### Problem 1

#### Read and clean the Mr. Trash Wheel sheet

``` r
Mr_TW = read_excel("./data/Trash-Wheel-Collection-Totals-8-6-19.xlsx", range = "A2:N408", sheet = 1) %>% 
janitor::clean_names() %>% 
filter(dumpster != "NA") %>% 
mutate(sports_balls = as.integer(round(sports_balls))) %>% 
rename("weight" = weight_tons, "volume" = volume_cubic_yards)
```

#### Read and clean precipitation data for 2017 and 2018. For each, omit rows without precipitation data and add a variable year

``` r
#2017 data
precip_17 = read_excel("./data/Trash-Wheel-Collection-Totals-8-6-19.xlsx", sheet = "2017 Precipitation", range = "A2:B14") %>% 
  janitor::clean_names() %>% 
  filter(total != "NA") %>% 
#adding variable
  mutate(year = 2017)

#2018 data
precip_18 = read_excel("./data/Trash-Wheel-Collection-Totals-8-6-19.xlsx",sheet = "2018 Precipitation", range = "A2:B14") %>% 
  janitor::clean_names() %>% 
  filter(total != "NA") %>% 
#adding variable
mutate(year = 2018)
```

#### Combine precipitation datasets and convert month to a character variable (the variable month.name is built into R and should be useful).

``` r
precip_17_18 =
  left_join(precip_17, precip_18, by = "month") %>% 
  mutate(month = month.name[month])
```

#### Write a paragraph about these data

  - Be sure to note the number of observations in both resulting
    datasets
  - Give examples of key variables
  - What was the total precipitation in 2018?
  - What was the median number of sports balls in a dumpster in 2017?

<!-- end list -->

``` r
# The number of observations in the precipitation dataset
nrow(precip_17_18)
```

    ## [1] 12

``` r
# The number of observations in the Mr Trashwheel dataset
nrow(Mr_TW)
```

    ## [1] 344

``` r
# The total precipitation in 2018 was
sum((pull(precip_18, total)))
```

    ## [1] 70.33

``` r
# The median number of sports balls in a dumpster in 2017 was
median(pull(Mr_TW %>% filter(year == 2017), sports_balls))
```

    ## [1] 8

### Problem 2

#### Clean the data in pols-month.csv. Use separate() to break up the variable mon into integer variables year, month, and day; replace month number with month name; create a president variable taking values gop and dem, and remove prez\_dem and prez\_gop; and remove the day variable.

``` r
pols_month = read_csv("./data/pols-month.csv") %>%
  janitor::clean_names() %>% 
  separate("mon", into = c("year", "month", "day")) %>% 
  mutate(month = month.name[as.integer(month)]) %>% 
  rename("gop" = prez_gop, "dem"= prez_dem)
```

    ## Parsed with column specification:
    ## cols(
    ##   mon = col_date(format = ""),
    ##   prez_gop = col_double(),
    ##   gov_gop = col_double(),
    ##   sen_gop = col_double(),
    ##   rep_gop = col_double(),
    ##   prez_dem = col_double(),
    ##   gov_dem = col_double(),
    ##   sen_dem = col_double(),
    ##   rep_dem = col_double()
    ## )

#### Second, clean the data in snp.csv using a similar process to the above. For consistency across datasets, arrange according to year and month, and organize so that year and month are the leading columns.

``` r
snp = read_csv("./data/snp.csv") %>% 
  janitor::clean_names() %>% 
  separate("date", into = c("day", "month", "year")) %>% 
  select("year", "month", "day", "close") %>% 
  mutate(month = month.name[as.integer(month)]) 
```

    ## Parsed with column specification:
    ## cols(
    ##   date = col_character(),
    ##   close = col_double()
    ## )
