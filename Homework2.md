p8105\_hw2\_ebm2142
================
Elise Mantell
2019-10-04

### Problem 1

#### Read and clean the Mr. Trash Wheel sheet

``` r
Mr_TW = read_excel("./data/HealthyHarborWaterWheelTotals2018-7-28.xlsx", range = "A2:N408", sheet = 1) %>% 
janitor::clean_names() %>% 
filter(dumpster != "NA") %>% 
mutate(sports_balls = as.integer(round(sports_balls))) %>% 
rename("weight" = weight_tons, "volume" = volume_cubic_yards)
```
