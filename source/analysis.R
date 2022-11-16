library(tidyverse)
library(dplyr)
# The functions might be useful for A4
source("../source/a4-helpers.R")

## Test queries ----
#----------------------------------------------------------------------------#
# Simple queries for basic testing
#----------------------------------------------------------------------------#
# Return a simple string
test_query1 <- function() {
  return ("Hello world")
}

# Return a vector of numbers
test_query2 <- function(num=6) {
  v <- seq(1:num)
  return(v)
}

## Section 2  ---- 
#----------------------------------------------------------------------------#
incarceration_data <- read.csv("https://github.com/vera-institute/incarceration-trends/raw/master/incarceration_trends.csv")
# Comparing all races, which out of all has the highest population in jail in the most recent year?
highest_jail_pop <- incarceration_data %>% 
  drop_na %>% 
  filter(year == max(year)) %>% 
  group_by(aapi_prison_pop, black_prison_pop, latinx_prison_pop, white_prison_pop, other_race_prison_pop) %>% 
  summarize(
    sum_aapi = sum(aapi_prison_pop, na.rm = TRUE),
    sum_black = sum(black_prison_pop, na.rm = TRUE),
    sum_latinx = sum(latinx_prison_pop, na.rm = TRUE), 
    sum_white = sum(white_prison_pop, na.rm = TRUE),
    sum_other = sum(other_race_prison_pop, na.rm = TRUE)
  )

highest_aapi_jail <- incarceration_data %>% 
  drop_na %>% 
  filter(year == max(year)) %>% 
  mutate(max_aapi = max(aapi_prison_pop, na.rm = TRUE)) %>% 
  select(max_aapi)
#----------------------------------------------------------------------------#

## Section 3  ---- 
#----------------------------------------------------------------------------#
# Growth of the U.S. Prison Population
# Your functions might go here ... <todo:  update comment>
#----------------------------------------------------------------------------#
# This function ... <todo:  update comment>
get_year_jail_pop <- function() {
  # TODO: Implement this function 
return()   
}

# This function ... <todo:  update comment>
plot_jail_pop_for_us <- function()  {
  # TODO: Implement this function 
  return()   
} 

## Section 4  ---- 
#----------------------------------------------------------------------------#
# Growth of Prison Population by State 
# Your functions might go here ... <todo:  update comment>
# See Canvas
#----------------------------------------------------------------------------#

## Section 5  ---- 
#----------------------------------------------------------------------------#
# <variable comparison that reveals potential patterns of inequality>
# Your functions might go here ... <todo:  update comment>
# See Canvas
#----------------------------------------------------------------------------#

## Section 6  ---- 
#----------------------------------------------------------------------------#
# <a map shows potential patterns of inequality that vary geographically>
# Your functions might go here ... <todo:  update comment>
# See Canvas
#----------------------------------------------------------------------------#

## Load data frame ---- 


