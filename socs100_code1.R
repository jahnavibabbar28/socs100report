# CSS First Summative Assessment

# socs100 report1 code 
# part 1a data exploration and contextualization

# load pacman if needed
if(!require("pacman")) install.packages("pacman")

# load required packages using pacman
pacman::p_load(
  tidyverse,
  skimr, 
  gapminder,
  ggplot2,
  dplyr,
  gridExtra,
  purrr,
  glue)

# import dataset into R
full_data <- read.csv("full_data.csv")

# creating a function to create an overview table with mean, sd and number of NAs
variable_overview <- function(x) {
  if (is.numeric(x)) {
    list(
      Mean = mean(x, na.rm = TRUE),
      SD = sd(x, na.rm = TRUE),
      MissingValues = sum(is.na(x))
    )
  } else {
    list(
      Mean = NA,
      SD = NA,
      MissingValues = sum(is.na(x))
    )
  }
}

# use map_dfr to automate variable_overview for all variables
overview <- map_dfr(full_data, variable_overview, .id = "Variable")

# print the summary table using kable
kable(overview, caption = "Dataset Overview")


# part 1b data processing and functional programming

# create a function which can automate calculating rate or percentage from 2 inputs
rate <- function(x, y) {
  if_else(y == 0, NA, (x / y ) * 100)
}

# tidy the data and create new variables using the rate function 
full_data <- full_data %>%
  select(date, location, new_cases, new_deaths, total_cases, total_deaths, weekly_cases, weekly_deaths) %>% # select relevant variables
  filter(total_cases>0) %>% # filter dataset to only include when first case of COVID-19 was recorded
  rename(countryincomelevel = location) %>% # rename location to countryincome level and filter out all other locations
  filter(countryincomelevel %in% c("High-income countries", "Upper-middle-income countries", "Lower-middle-income countries", "Low-income countries"))%>%
  mutate(
    cfr = rate(total_deaths, total_cases), # create cfr variable using rate
    weeklycfr = rate(weekly_deaths, weekly_cases) # create weekly cfr using rate
  )


# part 2a data visualisation and functional programming 

# filter out location by groups of countries by economic status
locations <- c("High-income countries", "Upper-middle-income countries", "Lower-middle-income countries", "Low-income countries")

# create a function to plot total cases over time for different countries grouped by economic status
plot_cases <- function(location_name) {
  full_data %>%
    filter(countryincomelevel == location_name) %>%
    ggplot(aes(x = date, y = total_cases)) +
    geom_point(color = "blue", size = 0.5) +
    labs(x = "Time", y = "Total COVID-19 Cases", title = glue("COVID-19 Cases in ", location_name)) +
    scale_y_continuous(limits = c(0, 500000000)) +
    theme_minimal()+
    theme(
      text = element_text(family = "times new roman"),           # Set the global font family
      plot.title = element_text(size = 14, face = "bold", family = "times new roman"), # Customize title font
      axis.title = element_text(size = 12, family = "times new roman"), # Customize axis title font
      axis.text = element_text(size = 10, family = "times new roman"),   # Customize axis text font
      axis.text.x = element_blank()
    )
}

# create a function to plot CFR over time for different countries 
plot_cfr <- function(location_name) {
  full_data %>%
    filter(countryincomelevel == location_name) %>%
    ggplot(aes(x = date, y = cfr)) +
    geom_point(color = "red", size = 0.5) +
    labs(x = "Time", y = "Case Fatality Rate (CFR)", title = glue("CFR in ", location_name)) +
    scale_y_continuous(limits = c(0, 10)) +
    theme_minimal()+
    theme(
      text = element_text(family = "times new roman"), # Set the global font family
      plot.title = element_text(size = 14, face = "bold", family = "times new roman"), # Customize title font
      axis.title = element_text(size = 12, family = "times new roman"), # Customize axis title font
      axis.text = element_text(size = 10, family = "times new roman"),   # Customize axis text font
      axis.text.x = element_blank()
    )
}

# plotting total cases over time for all economies 

# use map function to create plots for total cases over time for different locations
plots1 <- map(locations, plot_cases)
# use grid arrange to plot all four (total cases over time)
grid.arrange(grobs=plots1, ncol=2)

# plotting cfrs over time for all economies 

# use map function to create plots for CFR over time for different locations
plots2 <- map(locations, plot_cfr)
# use grid arrange to plot all four (total cases over time)
grid.arrange(grobs=plots2, ncol=2)