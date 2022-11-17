library(tidyverse)
library(dplyr)
library(ggplot2)
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
# What is the proportion of black people to white people in prison in the country in the most recent year 'black_to_white_prison_proportion'  ?

white_in_prison <- incarceration_data %>% 
  drop_na() %>% 
  filter(year == max(year)) %>% 
  group_by(state) %>% 
  summarize(sum = sum(white_prison_pop))

total_white_in_prison <- sum(white_in_prison$sum)

black_in_prison <- incarceration_data %>% 
  drop_na() %>% 
  filter(year == max(year)) %>% 
  group_by(state) %>% 
  summarize(sum = sum(black_prison_pop))

total_black_in_prison <- sum(black_in_prison$sum)

black_to_white_prison_proportion <- round(total_black_in_prison / total_white_in_prison, 1)

# What is the proportion of males to females in prison in the country in the most recent year? 'male_to_female_prison_proportion'
male_in_prison <- incarceration_data %>% 
  drop_na() %>% 
  filter(year == max(year)) %>% 
  group_by(state) %>% 
  summarize(sum = sum(male_prison_pop))

total_male_in_prison <- sum(male_in_prison$sum)

female_in_prison <- incarceration_data %>% 
  drop_na() %>% 
  filter(year == max(year)) %>% 
  group_by(state) %>% 
  summarize(sum = sum(female_prison_pop))

total_female_in_prison <- sum(female_in_prison$sum)

male_to_female_prison_proportion <- round(total_male_in_prison / total_female_in_prison, 1)

# What is the proportion of adults to juveniles in jail in the country in the most recent year? 'adult_to_juvenile_prison_proportion'
adult_in_jail <- incarceration_data %>% 
  drop_na() %>% 
  filter(year == max(year)) %>% 
  group_by(state) %>% 
  summarize(sum = sum(male_adult_jail_pop, female_adult_jail_pop))

total_adult_in_jail <- round(sum(adult_in_jail$sum), 0)

juvenile_in_jail <- incarceration_data %>% 
  drop_na() %>% 
  filter(year == max(year)) %>% 
  group_by(state) %>% 
  summarize(sum = sum(male_juvenile_jail_pop, female_juvenile_jail_pop))

total_juvenile_in_jail <- round(sum(juvenile_in_jail$sum), 0)
#----------------------------------------------------------------------------#

## Section 3  ---- 
#----------------------------------------------------------------------------#
# Growth of the U.S. Prison Population
# Your functions might go here ... <todo:  update comment>
#----------------------------------------------------------------------------#
# This function ... <todo:  update comment>
get_year_jail_pop <- function() {
  year_jail_pop <- incarceration_data %>% 
    select(year, total_jail_pop) %>% 
    drop_na() %>% 
    filter(year >= 1970 & year <= 2018) %>% 
    group_by(year) %>% 
    summarize(total_jail_pop = sum(total_jail_pop))
  
return(year_jail_pop)   
}

# This function ... <todo:  update comment>
plot_jail_pop_for_us <- function()  {
  year_jail_pop <- get_year_jail_pop()
  p <- ggplot(year_jail_pop) +
    geom_col(
      mapping = aes(x = year, y = total_jail_pop)
    ) +
    scale_y_continuous(labels = scales::comma) + 
    labs(
      title = "Increase of Jail Population in U.S. (1970-2018)",
      x = "Year",
      y = "Total Jail Population"
    )
  return(p)   
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


