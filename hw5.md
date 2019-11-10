P8105 – Homework 5
================
Ngoc Duong – nqd2000
11/6/2019

``` r
library(readr)
library(tidyverse)
library(ggplot2)
library(purrr)
```

## Problem 1

``` r
#get dataset with missing values 
set.seed(10)

iris_with_missing = iris %>% 
  map_df(~replace(.x, sample(1:150, 20), NA)) %>%
  mutate(Species = as.character(Species)) %>% 
  janitor::clean_names()
```

``` r
# write a function that follows set rules 
rep_func = function(x){                #start function taking column x as argument
  if (is.numeric(x)) {                 #set condition if x is numeric then...
    replace(x,                         #replace in column x...
            is.na(x),                  #values that are NA...
            round(                     #with the average of the other observations that are non NA's
              mean(x, na.rm = TRUE),
              2))
    }
 else if (is.character(x)) {           #set condition if column x is character then...
              replace(x,               #replace in column x...
                      is.na(x),        #values that are NAs...
                      "virginica")     #with "virginica"
}}

iris_imputed = map_df(iris_with_missing, rep_func)  #"map" the function on iris_with_missing using map_df
```

After replacing missing values according to the set rules, a glimpse at
the “filled-in” dataset:

``` r
head(iris_imputed, 10)
```

    ## # A tibble: 10 x 5
    ##    sepal_length sepal_width petal_length petal_width species
    ##           <dbl>       <dbl>        <dbl>       <dbl> <chr>  
    ##  1         5.1          3.5         1.4         0.2  setosa 
    ##  2         4.9          3           1.4         0.2  setosa 
    ##  3         4.7          3.2         1.3         0.2  setosa 
    ##  4         4.6          3.1         1.5         1.19 setosa 
    ##  5         5            3.6         1.4         0.2  setosa 
    ##  6         5.4          3.9         1.7         0.4  setosa 
    ##  7         5.82         3.4         1.4         0.3  setosa 
    ##  8         5            3.4         1.5         0.2  setosa 
    ##  9         4.4          2.9         1.4         0.2  setosa 
    ## 10         4.9          3.1         3.77        0.1  setosa

## Problem 2

``` r
# iterate over file names and read in data for each subject using purrr::map and saving the result as a new variable in the dataframe
file.list =
  tibble(path = list.files(path = "./data",     #use list.files to list all csv file names in data folder
                           pattern="*.csv", 
                           full.names = TRUE)) %>%    
  mutate(data = map(path, read_csv)) %>%        #use map to apply read_csv to read all file names in (as list), and assign those to column data
  unnest()                                      #unnest list  
```
