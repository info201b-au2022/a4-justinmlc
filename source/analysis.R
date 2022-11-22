library(tidyverse)
library(dplyr)
library(ggplot2)
library(mapproj)
# The functions might be useful for A4
# source("../source/a4-helpers.R")

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
incarceration_data <- read.csv("~/Documents/info201/assignments/a4-justinmlc/source/incarceration_trends.csv")
# What is the proportion of black people to white people in prison in the country in the most recent year 'black_to_white_prison_proportion'  ?

get_black_to_white_prison_proportion <- function() {
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
}
# What is the proportion of males to females in prison in the country in the most recent year? 'male_to_female_prison_proportion'
get_male_to_female_prison_proportion <- function() {
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
}
# What is the proportion of adults to juveniles in jail in the country in the most recent year? 'adult_to_juvenile_prison_proportion'
get_adult_to_juvenile_jail_proportion <- function() {
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

adult_to_juvenile_jail_proportion <- round(total_adult_in_jail / total_juvenile_in_jail, 1)
}
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
      title = "Increase of Prison Population in U.S. (1970-2018)",
      x = "Year",
      y = "Total Prison Population"
    )
  return(p)   
} 

## Section 4  ---- 
#----------------------------------------------------------------------------#
# Growth of Prison Population by State 
# Your functions might go here ... <todo:  update comment>
# See Canvas
#----------------------------------------------------------------------------#
get_jail_pop_by_states <- function(states) {
  states_jail_pop <- incarceration_data %>%
    select(state, year, total_jail_pop) %>% 
    filter(state %in% states) %>% 
    filter(year >= 1970 & year <= 2018) %>%  
    group_by(state, year) %>% 
    summarize(
      total_jail_pop = sum(total_jail_pop, na.rm = TRUE))
  return(states_jail_pop)
}


plot_jail_pop_by_states <- function(states) {
  states_jail_pop <- get_jail_pop_by_states(states)
  p <- ggplot(states_jail_pop) + 
    geom_line(
      mapping = aes(x = year, y = total_jail_pop, group = state, color = state)
    ) +
    labs(
      title = "Growth of Prison Population By State (1970-2018)",
      x = "Year",
      y = "Population in Prison"
    )
  return(p)
}
## Section 5  ---- 
#----------------------------------------------------------------------------#
# <variable comparison that reveals potential patterns of inequality>
# Your functions might go here ... <todo:  update comment>
# See Canvas
#----------------------------------------------------------------------------#
get_prop_female_in_prison <- function() {
  prop_female_in_prison <- incarceration_data %>% 
    drop_na() %>% 
    select(year, female_pop_15to64, female_prison_pop) %>% 
    filter(year >= 2000 & year <= 2018)
  return(prop_female_in_prison)
}

get_prop_male_in_prison <- function() {
  prop_male_in_prison <- incarceration_data %>% 
    drop_na() %>% 
    select(year, male_pop_15to64, male_prison_pop) %>% 
    filter(year >= 2000 & year <= 2018)
  return(prop_male_in_prison)
}

plot_prop_male_in_prison <- function() {
  prop_male_in_prison <- get_prop_male_in_prison()
    chart1 <- ggplot(prop_male_in_prison) +
      geom_point(
        mapping = aes(x = male_prison_pop, y = male_pop_15to64),
        color = "blue"
      ) +
      scale_y_continuous(labels = scales::comma) +
      labs(
        title = 'Proportion of Men Population to Men in Prison',
        x = 'Population of Men in Prison',
        y = 'Total Population of Men'
      )
  return(chart1)
}

plot_prop_female_in_prison <- function() {
  prop_female_in_prison <- get_prop_female_in_prison()
  chart2 <- ggplot(prop_female_in_prison) +
    geom_point(
      mapping = aes(x = female_prison_pop, y = female_pop_15to64),
      color = "pink"
    ) + 
    scale_y_continuous(labels = scales::comma) +
    labs(
      title = 'Proportion of Women Population to Women in Prison',
      x = 'Population of Women in Prison',
      y = 'Total Population of Women'
    )
  return(chart2)
}





# Function for plotting X: Gender Y: total pop of gender fill: pop in jail vs pop not
## Section 6  ---- 
#----------------------------------------------------------------------------#
# <a map shows potential patterns of inequality that vary geographically>
# Your functions might go here ... <todo:  update comment>
# See Canvas
#----------------------------------------------------------------------------#
get_map_data <- function() {
incarceration_data <- incarceration_data %>% 
  mutate(state = state.name[match(state, state.abb)]) %>% 
  mutate(state = tolower(state))

recent_incarcerations <- incarceration_data %>% 
  filter(year == max(year)) %>% 
  group_by(state) %>% 
  summarize(black_jail_pop = sum(black_jail_pop, na.rm = TRUE))
all_states <- map_data("state") %>% 
  rename(state = region) 

map_df <- all_states %>% 
  group_by(state) %>% 
  left_join(recent_incarcerations, by='state') 
  
return(map_df)
}

create_inequality_map <- function() {
  blank_theme <- theme_bw() + 
    theme(
      axis.line = element_blank(),        
      axis.text = element_blank(),        
      axis.ticks = element_blank(),       
      axis.title = element_blank(),       
      plot.background = element_blank(),  
      panel.grid.major = element_blank(), 
      panel.grid.minor = element_blank(), 
      panel.border = element_blank()
    )
  map_df <- get_map_data()
map <- ggplot(map_df) + 
  geom_polygon(
    mapping = aes(x = long, y = lat, group = state, fill = black_jail_pop),
    color = "white",
    size = .1
  ) + 
  coord_map() +
  scale_fill_continuous(low = '#132B43', high = "Red") + 
  labs(fill = "Jail Population",
       title = "Population of Black Americans in Jail",
       ) +
  blank_theme
return(map)
}
## Load data frame ---- 


