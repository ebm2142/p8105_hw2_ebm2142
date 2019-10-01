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
  left_join(precip_17, precip_18, by = 'month') %>% 
  mutate(month = month.name[month])
```
