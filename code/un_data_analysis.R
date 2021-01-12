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
