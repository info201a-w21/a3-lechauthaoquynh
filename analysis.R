#Assignment 3: Incarceration trends

# Loading data -----------------------------------------------------------------
# Install and load the packages
library(tidyverse)
install.packages("maps")
install.packages("mapproj")
install.packages("patchwork")
library(maps)
library(mapproj)
library(patchwork)

# Load incarceration_trends.csv file
incarceration_trends <- read.csv(
  "https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends.csv")  

# Variable: Black Jail Population ----------------------------------------------
#What is the average value of black jail population across all the counties most 
#recently? `recent_average_black_jail_pop`
recent_average_black_jail_pop <- incarceration_trends %>%
  filter(year == max(year, na.rm = T)) %>%
  summarize(mean_black_jail = mean(black_jail_pop, na.rm = T))

# When is the highest black jail population from the dataset?
#`year_highest_black_jail_pop`
year_highest_black_jail_pop <- incarceration_trends %>%
  filter(black_jail_pop == max(black_jail_pop, na.rm = T)) %>%
  pull(year)

# Where is the highest black jail population from the dataset?
#`place_highest_black_jail_pop`
place_highest_black_jail_pop <- incarceration_trends %>%
  filter(black_jail_pop == max(black_jail_pop, na.rm = T)) %>%
  select(state, county_name)

#Top 5 unique places have highest black jail population? `top_5_places` 
top_5_places <- incarceration_trends %>%
  group_by(county_name) %>%
  summarize(total_in_each_county = sum(black_jail_pop, na.rm = T)) %>%
  arrange(desc(total_in_each_county)) %>%
  slice(1:5)
top_5_chart <- top_5_places %>%
  left_join(incarceration_trends, by = "county_name", na.rm = T) %>%
  filter(black_jail_pop != "NA")
                        
# What is total black jail population in Washington in the most recent year?
#`total_wa`
total_wa <- incarceration_trends %>%
  filter(state == "WA") %>%
  filter(year == max(year, na.rm = T)) %>%
  summarize(total_black_jail = sum(black_jail_pop, na.rm = T))

# Trends over time chart ------------------------------------------------------
black_jail_pop_over_time_in_top_5 <- ggplot(data = top_5_chart) +
  geom_point(mapping = aes(x = year, y = black_jail_pop, color = county_name)) +
  geom_smooth(mapping = aes(x = year, y = black_jail_pop, color = county_name),
              se = FALSE) +
  labs(x = "Year", y = "Black Jail Population", title = "Top 5 Counties Has 
       Highest Black Jail Population Over Time") +
  scale_color_discrete("County Name")

# Variable comparison charts--------------------------------------------------
#Make a data frame for black and white jail pop? `two variables data`
two_variables_data <- data.frame(incarceration_trends$black_jail_pop,
                                 incarceration_trends$white_jail_pop)

#Graph the two variables comparison chart? `variables_comparison_chart`
variable_comparison_chart <- ggplot(data = two_variables_data) +
  geom_point(mapping = aes(x = incarceration_trends.black_jail_pop, 
                           y = incarceration_trends.white_jail_pop)) +
  labs(x = "Black Jail Pop", y = "White Jail Pop", title = "Comparison between
       Black Jail Population and White Jail Population in All Counties")

#Map -------------------------------------------------------------------------
#Get the `black_jail_pop` most recent data from the dataset? `black_jail_data`
black_jail_data <- incarceration_trends %>%
  filter(year == max(year, na.rm = T))

#Use map_data function to join `incarceration_trends` dataset with the map_data?
# `join_map`
join_map <- map_data("county") %>%
  unite(polyname, region, subregion, sep = ",") %>%
  left_join(county.fips, by = "polyname")

#Merge map data and incarceration data?`merge_map`
merge_map <- join_map %>%
  left_join(black_jail_data, by = "fips") %>%
  filter(black_jail_pop != "NA") 
  
# Incorporation blank theme
blank_theme <- theme_bw() +
  theme(
    axis.line = element_blank(),        # remove axis lines
    axis.text = element_blank(),        # remove axis labels
    axis.ticks = element_blank(),       # remove axis ticks
    axis.title = element_blank(),       # remove axis titles
    plot.background = element_blank(),  # remove gray background
    panel.grid.major = element_blank(), # remove major grid lines
    panel.grid.minor = element_blank(), # remove minor grid lines
    panel.border = element_blank()      # remove border around plot
  )

# Create map `black_jail_map`
black_jail_map <- ggplot(merge_map) +
  geom_polygon(
    mapping = aes(x = long, y = lat, group = group, fill = black_jail_pop),
    color = "gray", size = 0.3) +
  coord_map() +
  scale_fill_continuous(limits = c(0, max(merge_map$black_jail_pop)), na.value = 
                          "white", low = "yellow", high = "red") +
  blank_theme +
  ggtitle("Black Jail Population in the U.S")