library(tidyverse)
library(readr)

#Load in data
gapminder_data <- read_csv("~/Desktop/un-report/data/gapminder_data.csv")

#Summarising our data
summarise(gapminder_data, averagelifeExp=mean(lifeExp))

gapminder_data %>% summarise (averagelifeExp=mean(lifeExp))

gapminder_data_summarised <- gapminder_data %>%
  summarise (averagelifeExp=mean(lifeExp))

#Filtering our data using filter()
gapminder_data %>%
  filter(year == 2007) %>%
  summarise (average=mean(lifeExp))

#What is the earliest year in the data?
gapminder_data %>% summarise(firstyear=min(year))

#Mean gdp per capita for that year?
gapminder_data %>%
  filter(year == 1952) %>%
  summarise(averagegdp=mean(gdpPercap))

#What is the mean life expectancy by year?
gapminder_data %>%
  group_by(year) %>%
  summarise(average=mean(lifeExp))

#Multiple summary statistics at once
gapminder_data %>%
  group_by(continent)%>%
  summarise(average=mean(lifeExp), min=min(lifeExp))

#Adding new columns with mutate
gapminder_data %>%
  mutate(gdp=pop * gdpPercap,
         popInMillions=pop/1000000)

#Subset columns (or change their order) with select() -- would typically just do one or the other shown below
gapminder_data %>%
  select(pop, year)

gapminder_data %>%
  select(continent, country)

gapminder_data %>%
  select(gdpPercap, everything())
 
#remember: wide vs. long format data -- long format data is considered "tidier" because it's easier to pass into R functions

#Moving between long and wide data with pivot_wider() and pivot_longer()
gapminder_data %>%
  select(country, continent, year, lifeExp) %>%
  pivot_wider(names_from = year, values_from = lifeExp)

#Dataset for analysis
gapminder_data_2007 <- read_csv("~/Desktop/un-report/data/gapminder_data.csv") %>%
  filter(year == 2007 & continent == "Americans") %>%
  select(-year, -continent)

#Data cleaning
#Question: how does life expectancy relate to co2 emissions
#skips column names -- we'll put in our own column names
read_csv("~/Desktop/un-report/data/co2-un-data.csv", skip=2,
         col_names = c("region", "country", "year", "series", "value", 
                       "footnotes", "source")) 

#renames column named ...2 to country
read_csv("~/Desktop/un-report/data/co2-un-data.csv", skip=1) %>%
  rename(country=...2)

#makes all column headers lowercase
read_csv("~/Desktop/un-report/data/co2-un-data.csv", skip=1) %>%
  rename_all(tolower)

#Practicing select()
co2_emissions_dirty <- read_csv("~/Desktop/un-report/data/co2-un-data.csv", skip=2,
                                col_names = c("region", "country", "year", "series", "value", 
                                              "footnotes", "source")) 

co2_emissions_dirty %>% #mutate can create new columns in place of others
  select(country, year, series, value) %>%
  mutate(series=recode(series, "Emissions (thousand metric tons of carbon dioxide)" = "total_emission", 
                       "Emissions per capita (metric tons of carbon dioxide)" = "per_capita_emissions")) %>%
  pivot_wider(names_from = series, values_from = value)
  # to figure out number of observations per year
  count(year) 

#filtering to only 2005
co2_emissions <- co2_emissions_dirty %>%
  select(country, year, series, value) %>%
  mutate(series=recode(series, "Emissions (thousand metric tons of carbon dioxide)" = "total_emission", 
                         "Emissions per capita (metric tons of carbon dioxide)" = "per_capita_emissions")) %>%
  pivot_wider(names_from = series, values_from = value) %>%
  filter(year == 2005) %>%
  select(-year)

#joining data frames using inner_join() -- only joins columns that match
inner_join(gapminder_data_2007, co2_emissions)

anti_join(gapminder_data_2007, co2_emissions, by = "country")

co2_emissions <- read_csv("~/Desktop/un-report/data/co2-un-data.csv", skip =2, col_names = c("region", "country", "year", "series",
                                                                                             "value", "footnotes", "source")) %>%
  select(country, year, series, value) %>%
  mutate(series = recode(series, "Emissions (thousand metric tons of carbon dioxide)" = "total",
                         "Emissions per capita (metric tons of carbon dioxide)" = "per_capita")) %>%
  pivot_wider (names_from = series, values_from = value) %>%
  filter(year==2005) %>%
  select(-year) %>%
  mutate(country=recode(country,
                        "Bolivia (Plurin. State of" = "Bolivia", "United States of America" = "United States",
                        "Venezuela (Boliv. Rep. of)" = "Venezuela"))

#a second antijoin
anti_join(gapminder_data_2007, co2_emissions, by = "country")

gapminder_data_2007 <- read_csv("~/Desktop/un-report/data/gapminder_data.csv") %>%
  filter(year == 2007 & continent == "Americas") %>%
  select(-year, -continent) %>%
  mutate(country=record(country, "Puerto Rico" = "United States"))

anti_join(gapminder_data_2007, co2_emissions, by = "country")

gapminder_data_2007 <- read_csv("~/Desktop/un-report/data/gapminder_data.csv") %>%
  filter(year == 2007 & continent == "Americas") %>%
  select(-year, -continent) %>%
  mutate(country=recode(country, "Puerto Rico" = "United States")) %>%
  group_by(country) %>%
  summarise(lifeExp=sum(lifeExp * pop) / sum(pop),
            gdpPercap = sum(gdpPercap * pop)/sum(pop),
            pop=sum(pop))

inner_join(gapminder_data_2007, co2_emissions, by = "country")

gapminder_co2 <- inner_join(gapminder_data_2007, co2_emissions, by = "country")

gapminder_co2 %>% # | is OR operator
  mutate(region = if_else(country == "Canada" | country == "United States" | country == "Mexico", "north", "south"))

#write data
write_csv(gapminder_co2, "~/Desktop/un-report/data/gapminder_co2.csv")

#plotting co2 + gapminder data
#question: what percent of co2 emissions do countries in the Americas data produce?
ggplot(gapminder_co2, aes(x=gdpPercap, y=per_capita)) +
  geom_point() +
  labs(x="GDP (per capita)",
       y="CO2 emitted (per capita)",
       title="There is a strong association between a nation's GDP \nand the amount of CO2 it produces")

#fit a line to our data
ggplot(gapminder_co2, aes(x=gdpPercap, y=per_capita)) +
  geom_point() +
  labs(x="GDP (per capita)",
       y="CO2 emitted (per capita)",
       title="There is a strong association between a nation's GDP \nand the amount of CO2 it produces") +
  geom_smooth()

#fit a straight line to our data
ggplot(gapminder_co2, aes(x=gdpPercap, y=per_capita)) +
  geom_point() +
  labs(x="GDP (per capita)",
       y="CO2 emitted (per capita)",
       title="CO2 emissions increase as GDP increases") +
  geom_smooth(method="lm")

