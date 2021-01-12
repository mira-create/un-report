library(tidyverse)

gapminder_data <- read_csv("data/gapminder_data.csv")
View(gapminder_data)

summarize(gapminder_data, averageLifeExp = mean(lifeExp))

#same as code above - piping data into summarize function
gapminder_data %>% summarize(averageLifeExp = mean(lifeExp))

#piping function allows us to use chains of code - can pipe summary statistics into new function
#can avoid nesting function

gapminder_data %>% summarize(averagePopulation = mean(pop),
                             recent_year = max(year))

#filter - allows us to select specific rows
#we can filter out most recent year
gapminder_data %>% filter(year == 2007)%>% summarize(averageLifeExp = mean(lifeExp))

#find the average GDP per cap for first year in data set

gapminder_data %>% summarize(averagePopulation = mean(pop),
                             recent_year = min(year))
min(gapminder_data['year'])


gapminder_data %>% 
  filter(year == 1952)%>% 
  summarize(averageGDPperCap = mean(gdpPercap), first_year = min(year))

#group_by
gapminder_data %>% 
  group_by(year) %>% 
  summarize(averageLifeExp=mean(lifeExp))

#exercise: find mean life exp for each continent
gapminder_data %>% 
  group_by(continent) %>% 
  summarize(averageLifeExp=mean(lifeExp))

#mutate - add more columes to dataset
gapminder_data %>% 
  mutate(gdp = gdpPercap * pop)

gapminder_data %>% 
  mutate(popMillions = pop/1000000)

#select() - specify which colum we want to keep
gapminder_data %>% 
  select(year, pop)

gapminder_data %>% 
  select(-continent)

#exercise: create a dataset with the country, continent, year, and life exp colums
gapminder_data %>% 
  select(country, continent, year, lifeExp)

#arrange(year) - arrange rows

#long vs wide
#pivot_longer and pivot_wider

gapminder_data %>% 
  select(country, continent, year, lifeExp) %>% 
  pivot_wider(names_from = year, values_from = lifeExp)


#rename

#create a new dataset with only data from the Americas and 2007

gapminder_data %>% 
  filter(year==2007, continent == "Americas") %>% 
  select(-continent,-year)

gapminder_data <- read_csv("data/gapminder_data.csv") %>% 
  filter(year==2007, continent == "Americas") %>% 
  select(-continent,-year)
gapminder_data

#exercise - select only country, year, and series values
co2_emissions <- read_csv("data/co2-un-data.csv", skip=2,
         col_names = c("region", "country", "year", "series", "value", "footnotes", "source")) %>% 
  select(country, year, series, value) %>% 
  mutate(series = recode(series, "Emissions (thousand metric tons of carbon dioxide)" = "total_emissions",
                         "Emissions per capita (metric tons of carbon dioxide)" = "per_capita_emissions")) %>% 
  pivot_wider(names_from = series, values_from = value) %>% 
  filter(year==2005) %>% 
  select(-year) %>% 
  mutate(country=recode(country,
                        "Bolivia (Plurin. State of)" = "Bolivia",
                        "United States of America" = "United States",
                        "Venezuela (Boliv. Rep. of)" = "Venezuela"))

#goal:data from a year close to 2007
#a coulmn for country and we want cols for different types of co2 emissions (total, per cap)

View(co2_emissions)
#join data sets with functions as key

inner_join() #only keep rows in both data sets
outer_join() #keep all rows from both data sets

inner_join(gapminder_data, co2_emissions, by="country")

anti_join(gapminder_data,co2_emissions) #tells us rows in first data set that are not in second dataset

#change PR to be a part of the US
gapminder_data <- read_csv("data/gapminder_data.csv") %>% 
  filter(year==2007, continent == "Americas") %>% 
  select(-continent,-year) %>% 
  mutate(country = recode(country, "Puerto Rico" = "United States")) %>% 
  group_by(country) %>% 
  summarize(lifeExp = sum(lifeExp*pop)/sum(pop), 
            gdpPercap = sum(gdpPercap*pop)/sum(pop),
            pop=sum(pop))

anti_join(gapminder_data,co2_emissions) #tells us rows in first data set that are not in second dataset

gapminder_co2 <- inner_join(gapminder_data, co2_emissions, by="country")

#mutate and the if_else
#if_els(condition, true, false)
gdp_co2_region<- gapminder_co2 %>% 
  mutate(region=if_else(country == "Canada" |
                          country == "United States"|
                          country == "Mexico", "north", "south"))

#create scatter of gdp vs co2 emissions, color it by region
gdp_co2_region %>%  
  aes(x = gdpPercap, y = per_capita_emissions, color = region)+
  labs(x = "GDP Per Capita", y = "Emissions")+
  geom_point()

write_csv(gap_co2_region, "gapminder_co2.csv")
