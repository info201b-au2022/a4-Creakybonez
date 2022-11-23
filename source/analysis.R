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

get_year_jail_pop()


plot_jail_pop_for_us <- ggplot(data = college_table ) +
  geom_point(
    mapping = aes(x = State.abbreviation , y =average_expense_out_state),
    color = "blue",
    alpha = .3
  ) + coord_flip()

print(chart_1 + labs(
  title = "Chart #1 Average expense (out of state) of each state",
  y = "Price", x = "States"
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


