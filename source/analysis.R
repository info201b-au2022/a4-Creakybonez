library(tidyverse)

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

# Following is my date set named 'incarceration_inequality'
incarceration_inequality <- read.csv("https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends.csv")


# This is a aggregated data set that includes information for calculation
incarceration_inequality_aggregated <- incarceration_inequality %>%
  group_by(year) %>%
  select(
    total_pop,
    female_pop_15to64,
    male_pop_15to64,
    female_adult_jail_pop,
    male_adult_jail_pop,
    total_jail_pop
  )

#Used for calculating only with complete data entries
incarceration_inequaity_no_NA <- incarceration_inequality_aggregated[complete.cases(incarceration_inequality_aggregated), ]

#Used for calculating population averages across year-by-year
incarceration_inequality_summary <- incarceration_inequaity_no_NA %>%
  group_by(year,) %>%
  summarise(across(c(total_pop, female_pop_15to64, male_pop_15to64, female_adult_jail_pop, male_adult_jail_pop, total_jail_pop), sum))

#Calculate gender ratio year-by-year
incarceration_inequality_summary$male_to_female_ratio <- (incarceration_inequality_summary$male_adult_jail_pop / incarceration_inequality_summary$female_adult_jail_pop)

#A specific table of specific findings
summary_info <- list()
summary_info$num_observations <- nrow(incarceration_table)

#Total male to female incarceration ratio
summary_info$male_to_female_ratio <- mean(incarceration_inequality_summary$male_adult_jail_pop / incarceration_inequality_summary$female_adult_jail_pop)

#1970 male to female incarceration ratio
summary_info$male_to_female_ratio_1970 <- incarceration_inequality_summary %>%
  filter(year == min(year,na.rm = T)) %>%
  select(male_to_female_ratio) %>%
  pull()

#2018 male to female incarceration ratio
summary_info$male_to_female_ratio_2018 <- incarceration_inequality_summary %>%
  filter(year == max(year,na.rm = T)) %>%
  select(male_to_female_ratio) %>%
  pull()

#Change in male to female incarceration ratio over 48 years
summary_info$male_to_female_ratio_48_year_change <- summary_info$male_to_female_ratio_2018 - summary_info$male_to_female_ratio_1970

#----------------------------------------------------------------------------#

## Section 3  ---- 
#----------------------------------------------------------------------------#
# Growth of the U.S. Prison Population

#Total jail population per year function
get_year_jail_pop <- function() {
  incarceration_inequality_summary %>%
  group_by(year) %>%
  select(
    year,
    total_jail_pop, 
  )
}

#Call data
get_year_jail_pop()

#Create bar graph
plot_jail_pop_for_us <- ggplot(data = get_year_jail_pop() ) +
  geom_col(
    mapping = aes(x= year, y = total_jail_pop),
    color = "gray",
    alpha = .5
  )



print(plot_jail_pop_for_us + labs(
  title = "Increase of Jail Population in the U.S. (1970-2018)", 
  caption = "The year-over-year data detailing the jail population of the US from 1970 to 2018",
  y = "Total Jail Population", x = "Year"
))
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

#Aggregate from original data set
incarceration_inequality_aggregated_state <- incarceration_inequality %>%
  group_by(year) %>%
  select(
    total_jail_pop,
    state,
    year
  )
#For removing data with no values
incarceration_inequality_states_no_NA <- incarceration_inequality_aggregated_state[complete.cases(incarceration_inequality_aggregated_state), ]

#Grouping data by state
incarceration_inequality_state_summary <- incarceration_inequality_states_no_NA %>%
  group_by(state, year) %>%
  summarise(across(c(total_jail_pop), sum))



#Function for Specific State
get_jail_population_by_state <- function(state_abb){
  incarceration_inequality_state_summary <- incarceration_inequality_state_summary[incarceration_inequality_state_summary$state == state_abb, ]
  print(incarceration_inequality_state_summary)
}

#test
get_jail_population_by_state("AL")
#Test 2
incarceration_inequality_state_summary <- incarceration_inequality_state_summary[incarceration_inequality_state_summary$state == "AK", ]

#Create Graph By State
plot_jail_pop_by_states <- function(){
  specific_state_plot <- ggplot(data = incarceration_inequality_state_summary() ) +
    geom_col(
      mapping = aes(x= year, y = total_jail_pop),
      color = "gray",
      alpha = .5
    )
  
  print(specific_state_plot + labs(
    title = "Increase of Jail Population in U.S State (1970-2018)", 
    caption = "The year-over-year data detailing the jail population of US States from 1970 to 2018",
    y = "Total Jail Population", x = "Year"
  ))
  
}


state_summary_graph <- ggplot(data = incarceration_inequality_state_summary() ) +
  geom_col(
    mapping = aes(x= year, y = total_jail_pop),
    color = "gray",
    alpha = .5
  )

print(incarceration_inequality_state_summary + labs(
  title = "Increase of Jail Population in a U.S State (1970-2018)", 
  caption = "The year-over-year data detailing the jail population of US States from 1970 to 2018",
  y = "Total Jail Population", x = "Year"
))
  #----------------------------------------------------------------------------#

## Section 5  ---- 
#----------------------------------------------------------------------------#
#dataset determining
incarceration_inequality_aggregated_2 <- incarceration_inequality %>%
  group_by(year) %>%
  select(
    total_pop,
    female_pop_15to64,
    male_pop_15to64,
    female_adult_jail_pop,
    male_adult_jail_pop,
    total_jail_pop
  )

#remove NA data
incarceration_inequaity_no_NA_2 <- incarceration_inequality_aggregated_2[complete.cases(incarceration_inequality_aggregated_2), ]

get_year_jail_pop2 <- function() {
  incarceration_inequaity_no_NA_2 %>%
    group_by(year) %>%
    select(
      year,
      total_jail_pop, 
      male_adult_jail_pop,
    )
}

#call function
get_year_jail_pop2()

#create graph
plot_jail_pop_2 <- ggplot(data = get_year_jail_pop2() ) +
  geom_col(
    mapping = aes(x= year, y = total_jail_pop),
    color = "gray",
    alpha = .5
  )



print(plot_jail_pop_2 + labs(
  title = "Male Prisoner Gender Ratio (1970-2018)", 
  caption = "The year-over-year data detailing the jail population gender ratio of the US from 1970 to 2018",
  y = "Total Male Jail Population", x = "Year"
))

#----------------------------------------------------------------------------#

## Section 6  ---- 
#----------------------------------------------------------------------------#
install.packages("usmap")
library("usmap")

#data aggregation
aggregated_state <- incarceration_inequality %>%
  group_by(year) %>%
  select(
    total_jail_pop,
    state,
    year
  )

#remove NA values
aggregated_states_no_NA <- aggregated_state[complete.cases(aggregated_state), ]

#Combine by state
total_state_jail_pop <- aggregated_states_no_NA %>%
  group_by(state, year) %>%
  summarise(across(c(total_jail_pop), sum))

#find states with less than 3000 prisoners in the past 48 years
states_with_less_than_3000_prisoners <- total_state_jail_pop[total_state_jail_pop$total_jail_pop < 3000 , ]

#find unique states with less than 3000 prisoners in the past 48 years
unique_states_with_less_than_3000_prisoners <- unique(states_with_less_than_3000_prisoners$state)

#plot of states with less than 3000 prisoners in the past 48 years
us_map_data <- plot_usmap(include = c(unique_states_with_less_than_3000_prisoners)) + 
  labs(title = "States that Never Exceeded 3000 Prisoners from 1970 to 2018") +
  labs(caption = "States that never held more than 3000 prisoners from the years 1970 to 2018")


print(us_map_data)

#----------------------------------------------------------------------------#

## Load data frame ---- 


