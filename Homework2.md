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
  full_join(precip_17, precip_18) %>% 
  mutate(month = month.name[month])
```

    ## Joining, by = c("month", "total", "year")

#### Write a paragraph about these data

  - Be sure to note the number of observations in both resulting
    datasets
  - Give examples of key variables
  - What was the total precipitation in 2018?
  - What was the median number of sports balls in a dumpster in 2017?

The Mr Trashwheel dataset includes information about trash filtered
through the adorable googly-eyed trash wheel in the Baltimore Harbor. It
also includes information about precipitation levels (because they
affect the amount of trash coming through). Key variables include
“total” for the total precipitation, as well as variables describing
the trash contents (for example “sports\_balls”). The number of
observations in the precipitation dataset is 24. The number of
observations in the Mr Trashwheel dataset is 344. The total
precipitation in 2018 was 70.33. The median number of sports balls in a
dumpster in 2017 was
8

### Problem 2

#### Clean the data in pols-month.csv. Use separate() to break up the variable mon into integer variables year, month, and day; replace month number with month name; create a president variable taking values gop and dem, and remove prez\_dem and prez\_gop; and remove the day variable.

``` r
pols_month = read_csv("./data/pols-month.csv") %>%
  janitor::clean_names() %>% 
  separate("mon", into = c("year", "month", "day")) %>% 
  mutate(month = month.abb[as.integer(month)],month =  str_to_lower(month)) %>% 
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
  mutate(month = month.abb[as.integer(month)],month =  str_to_lower(month))
```

    ## Parsed with column specification:
    ## cols(
    ##   date = col_character(),
    ##   close = col_double()
    ## )

#### Third, tidy the unemployment data so that it can be merged with the previous datasets. This process will involve switching from “wide” to “long” format; ensuring that key variables have the same name; and ensuring that key variables take the same values.

``` r
unemployment = read_csv("./data/unemployment.csv", col_types = "cdddddddddddd") %>% 
  janitor::clean_names() %>% 
  pivot_longer(jan:dec, 
  names_to = "month", values_to = "rate") 
```

#### Join the datasets by merging snp into pols, and merging unemployment into the result.

``` r
snp_pols = left_join(snp, pols_month, by = c("year","month"))
snp_pols_unemp = left_join(snp_pols, unemployment, by = c("year","month"))
```

#### Write a short paragraph about these datasets. Explain briefly what each dataset contained, and describe the resulting dataset (e.g. give the dimension, range of years, and names of key variables).

``` r
nrow(snp_pols_unemp)
```

    ## [1] 787

``` r
range(pull(snp_pols_unemp, year))
```

    ## [1] "1950" "2015"

##### The dataset “pols-month” contains 822 observations of 9 variables related to the number of national politicians who are democratic or republican at a given time (“gop” and “dem” variables indicating if the president was a republican or democrat, with similar variables for senate, house of representatives, and governors). The dataset “snp” contains 787 observations the position of the Standard & Poor’s stock market index (S\&P) at close (represented by variable “close”). The dataset “unemployment” contains 68 observations of 13 variables, showing the unemployment percentage for each month of the year. The combined dataset containing all three of these includes 787 observations over the years 1950 to 2015.

### Problem 3

#### Load and tidy the data. The names of a categorical predictor and the case structure of string variables changed over time; you’ll need to address this in your data cleaning. Also, some rows seem duplicated, and these will need to be removed

``` r
baby_name = read_csv("./data/Popular_Baby_Names.csv") %>% 
janitor::clean_names() %>% 
distinct() %>% 
mutate_all(toupper) %>% 
rename("year" = year_of_birth, "name" = childs_first_name) %>% 
mutate(ethnicity = replace(ethnicity, ethnicity == "ASIAN AND PACI", "ASIAN AND PACIFIC ISLANDER")) %>% 
mutate(ethnicity = replace(ethnicity, ethnicity == "BLACK NON HISP","BLACK NON HISPANIC")) %>%
mutate(ethnicity = replace(ethnicity, ethnicity == "WHITE NON HISP", "WHITE NON HISPANIC"))%>%
mutate(year = as.numeric(year)) %>%
mutate(rank = as.numeric(rank)) %>%
mutate(count = as.numeric(count))
```

    ## Parsed with column specification:
    ## cols(
    ##   `Year of Birth` = col_double(),
    ##   Gender = col_character(),
    ##   Ethnicity = col_character(),
    ##   `Child's First Name` = col_character(),
    ##   Count = col_double(),
    ##   Rank = col_double()
    ## )

#### Produce a well-structured, reader-friendly table showing the rank in popularity of the name “Olivia” as a female baby name over time; this should have rows for ethnicities and columns for year. Produce a similar table showing the most popular name among male children over time.

``` r
baby_name %>%
  group_by(year) %>% 
  filter(name == "OLIVIA") %>%
  select(year, ethnicity, rank) %>%
  knitr::kable(digits = 1)
```

| year | ethnicity                  | rank |
| ---: | :------------------------- | ---: |
| 2016 | ASIAN AND PACIFIC ISLANDER |    1 |
| 2016 | BLACK NON HISPANIC         |    8 |
| 2016 | HISPANIC                   |   13 |
| 2016 | WHITE NON HISPANIC         |    1 |
| 2015 | ASIAN AND PACIFIC ISLANDER |    1 |
| 2015 | BLACK NON HISPANIC         |    4 |
| 2015 | HISPANIC                   |   16 |
| 2015 | WHITE NON HISPANIC         |    1 |
| 2014 | ASIAN AND PACIFIC ISLANDER |    1 |
| 2014 | BLACK NON HISPANIC         |    8 |
| 2014 | HISPANIC                   |   16 |
| 2014 | WHITE NON HISPANIC         |    1 |
| 2013 | ASIAN AND PACIFIC ISLANDER |    3 |
| 2013 | BLACK NON HISPANIC         |    6 |
| 2013 | HISPANIC                   |   22 |
| 2013 | WHITE NON HISPANIC         |    1 |
| 2012 | ASIAN AND PACIFIC ISLANDER |    3 |
| 2012 | BLACK NON HISPANIC         |    8 |
| 2012 | HISPANIC                   |   22 |
| 2012 | WHITE NON HISPANIC         |    4 |
| 2011 | ASIAN AND PACIFIC ISLANDER |    4 |
| 2011 | BLACK NON HISPANIC         |   10 |
| 2011 | HISPANIC                   |   18 |
| 2011 | WHITE NON HISPANIC         |    2 |

``` r
baby_name %>%
  group_by(year, ethnicity) %>% 
  filter(rank == 1, gender == "MALE") %>%
  select(year, ethnicity, rank, name) %>%
  knitr::kable(digits = 1)
```

| year | ethnicity                  | rank | name    |
| ---: | :------------------------- | ---: | :------ |
| 2016 | ASIAN AND PACIFIC ISLANDER |    1 | ETHAN   |
| 2016 | BLACK NON HISPANIC         |    1 | NOAH    |
| 2016 | HISPANIC                   |    1 | LIAM    |
| 2016 | WHITE NON HISPANIC         |    1 | JOSEPH  |
| 2015 | ASIAN AND PACIFIC ISLANDER |    1 | JAYDEN  |
| 2015 | BLACK NON HISPANIC         |    1 | NOAH    |
| 2015 | HISPANIC                   |    1 | LIAM    |
| 2015 | WHITE NON HISPANIC         |    1 | DAVID   |
| 2014 | ASIAN AND PACIFIC ISLANDER |    1 | JAYDEN  |
| 2014 | BLACK NON HISPANIC         |    1 | ETHAN   |
| 2014 | HISPANIC                   |    1 | LIAM    |
| 2014 | WHITE NON HISPANIC         |    1 | JOSEPH  |
| 2013 | ASIAN AND PACIFIC ISLANDER |    1 | JAYDEN  |
| 2013 | BLACK NON HISPANIC         |    1 | ETHAN   |
| 2013 | HISPANIC                   |    1 | JAYDEN  |
| 2013 | WHITE NON HISPANIC         |    1 | DAVID   |
| 2012 | ASIAN AND PACIFIC ISLANDER |    1 | RYAN    |
| 2012 | BLACK NON HISPANIC         |    1 | JAYDEN  |
| 2012 | HISPANIC                   |    1 | JAYDEN  |
| 2012 | WHITE NON HISPANIC         |    1 | JOSEPH  |
| 2011 | ASIAN AND PACIFIC ISLANDER |    1 | ETHAN   |
| 2011 | BLACK NON HISPANIC         |    1 | JAYDEN  |
| 2011 | HISPANIC                   |    1 | JAYDEN  |
| 2011 | WHITE NON HISPANIC         |    1 | MICHAEL |

#### Finally, for male, white non-hispanic children born in 2016, produce a scatter plot showing the number of children with a name (y axis) against the rank in popularity of that name (x axis)

``` r
babyname_plot =
  baby_name %>%
  filter(ethnicity == "WHITE NON HISPANIC", gender == "MALE", year == "2016")
  babyname_plot %>% 
  ggplot(aes(x = rank, y = count)) + 
  geom_point() +
labs(
    title = "Baby Name Rank vs. Count, White Males 2016",
    x = "Popularity Rank",
    y = "Count",
    caption = "Data from Open NYC")
```

![](Homework2_files/figure-gfm/plot%20dataframe-1.png)<!-- -->
