# installing packages...
install.packages("tidyverse")
install.packages("ggplot2")
install.packages("dplyr")
install.packages("janitor")
install.packages("scales")
install.packages("skimr")
install.packages("corrplot")
install.packages("DT")
install.packages("ploty")
install.packages("highcharter")
install.packages("lubridate")


# loading packages...
library("tidyverse")
library("ggplot2")
library("readr")
library("dplyr")
library("janitor")
library("scales")
library("skimr")
library("corrplot")
library("DT")
library("ploty")
library("highcharter")
library("lubridate")

# STEP 1: COLLECT DATA
# upload world happiness data here...


wh_2019 <- read.csv("2019.csv") # collecting data from 2019.csv file and storing it in wh_2019.
head(wh_2019) # checking the dataframe
View(wh_2019)

wh_2018 <- read.csv("2018.csv") # collecting data from 2018.csv file and storing it in wh_2018.
head(wh_2018) # checking the dataframe
View(wh_2018)

wh_2017 <- read.csv("2017.csv") # collecting data from 2017.csv file and storing it in wh_2018.
head(wh_2017) # checking the dataframe
View(wh_2017)

wh_2016 <- read.csv("2016.csv") # collecting data from 2016.csv file and storing it in wh_2018.
head(wh_2016) # checking the dataframe
View(wh_2016)

# STEP 2: WRANGLE DATA AND COMBINE INTO A SINGLE FILE
#====================================================
# Compare column names each of the files
# While the names don't have to be in the same order, they DO need to match perfectly before we can use a command to join them into one file
colnames(wh_2019)
colnames(wh_2018)
colnames(wh_2017)
colnames(wh_2016)

nrow(wh_2019)
nrow(wh_2018)
nrow(wh_2017)
nrow(wh_2016)

# Inspect the dataframes and look for incongruencies
str(wh_2019)
str(wh_2018)
str(wh_2017)
str(wh_2016)


# Skim for missing values
n_missing(wh_2019)
n_missing(wh_2018)
n_missing(wh_2017)
n_missing(wh_2016)

# Determine duplicate values
anyDuplicated(wh_2019)
anyDuplicated(wh_2018)
anyDuplicated(wh_2017)
anyDuplicated(wh_2016)

# Transform data 2019
wh_2019 <- wh_2019 %>% 
  rename(country = Country.or.region,
         score = Score,
        gdp_per_capita = GDP.per.capita,
        social_support =  Social.support,
        healthy_life_expectancy = Healthy.life.expectancy,
        freedom_to_make_life_choices = Freedom.to.make.life.choices,
        Country = Country.or.region,
        perceptions_of_corruption = Perceptions.of.corruption)
         
         




  

# Transform data 2018

wh_2018 <- wh_2018 %>% 
  rename(country = Country.or.region,
         score = Score,
         gdp_per_capita = GDP.per.capita,
         social_support =  Social.support,
         healthy_life_expectancy = Healthy.life.expectancy,
         freedom_to_make_life_choices = Freedom.to.make.life.choices,
         Country = Country.or.region,
         perceptions_of_corruption = Perceptions.of.corruption)






 
  
 # Transform data 2017
  wh_2017 <- wh_2017 %>% 
  rename(overall_rank = Happiness.Rank) %>% 
  relocate(overall_rank,
           .before = Country) %>% 
  rename(score = Happiness.Score) %>% 
  select(-Whisker.high,
         -Whisker.low) %>% 
  rename(gdp_per_capita = Economy..GDP.per.Capita.,
         social_support = Family,
         healthy_life_expectancy = Health..Life.Expectancy.,
         freedom_to_make_life_choices = Freedom,
         Country = country,
         perceptions_of_corruption = Trust..Government.Corruption.) %>% 
  select(-Dystopia.Residual)





# Transform data 2016
wh_2016 <- wh_2016 %>%
  select(-Region) %>% 
  rename(overall_rank = Happiness.Rank) %>% 
  relocate(overall_rank,
           .before = Country) %>% 
  rename(score = Happiness.Score) %>% 
  select(-Lower.Confidence.Interval,
         -Upper.Confidence.Interval) %>% 
  rename(gdp_per_capita = Economy..GDP.per.Capita.,
         social_support = Family,
         healthy_life_expectancy = Health..Life.Expectancy.,
         freedom_to_make_life_choices = Freedom,
         perceptions_of_corruption = Trust..Government.Corruption.) %>% 
  relocate(Generosity,
           .before = perceptions_of_corruption) %>% 
  select(-Dystopia.Residual)

View(wh_2016)

# Bind data frames
happiness <- bind_rows(wh_2016,
                       wh_2017,
                       wh_2018,
                       wh_2019)
nrow(happiness)
glimpse(happiness)

View(happiness)

# # Skim combined data
skim_without_charts(happiness)


# Average statistics by country
 happiness%>% 
  group_by(Country) %>% 
  summarise(mean_score = mean(score),
            mean_gdp_per_capita = mean(gdp_per_capita),
            mean_social_support = mean(social_support),
            mean_healthy_life_expectancy = mean(healthy_life_expectancy),
            mean_freedom_to_make_life_choices = mean(freedom_to_make_life_choices),
            mean_Generosity = mean(Generosity),
            mean_perceptions_of_corruption = mean(perceptions_of_corruption)) %>% 
  arrange(-mean_score)

 
 ## Highest ranked countries by overall happiness
 
 # Plot top countries by score
 happiness %>% 
   group_by(Country) %>% 
   summarise(mean_score = mean(score) %>% 
               round(2)) %>% 
   arrange(-mean_score) %>% 
   slice_head(n = 10) %>% 
   ggplot(aes(x = reorder(Country, mean_score),
              y = mean_score,
              fill = mean_score)) +
   geom_col(width = 0.85) +
   coord_flip() +
   guides(fill = "none") +
   labs(title = "Top 10 Countries by Average Happiness Score",
        subtitle = "2016 to 2019",
        x = NULL,
        y = "Average happiness score") +
   geom_text(aes(label = mean_score),
             color = "yellow",
             hjust = 1.5,
             size = 2)
 

 
 
  
 ##Countries ranked by GDP Per Capita Contributing to Happiness
 
 # Plot by gdp per capita
 happiness %>% 
   group_by(Country) %>% 
   summarise(mean_gdp_per_capita = mean(gdp_per_capita) %>% 
               round(2)) %>% 
   arrange(-mean_gdp_per_capita) %>% 
   slice_head(n = 10) %>% 
   ggplot(aes(x = reorder(Country, mean_gdp_per_capita),
              y = mean_gdp_per_capita,
              fill = mean_gdp_per_capita)) +
   geom_col(width = 0.75) +
   coord_flip() +
   guides(fill = "none") +
   labs(title = "Top 10 Countries by Average GDP Per Capita",
        subtitle = "2016 to 2019",
        x = NULL,
        y = "Average GDP per capita") +
   geom_text(aes(label = mean_gdp_per_capita),
             color = "yellow",
             hjust = 1.5,
             size = 2)
 
 
 
 ## Countries ranked by Social Support Contributing to Happiness
 
 # Plot top countries by social support
 happiness %>% 
   group_by(Country) %>% 
   summarise(mean_social_support = mean(social_support) %>% 
               round(2)) %>% 
   arrange(-mean_social_support) %>% 
   slice_head(n = 10) %>% 
   ggplot(aes(x = reorder(Country, mean_social_support),
              y = mean_social_support,
              fill = mean_social_support)) +
   geom_col(width = 0.75) +
   coord_flip() +
   guides(fill = "none") +
   labs(title = "Top 10 Countries by Average Social Support",
        subtitle = "2016 to 2019",
        x = NULL,
        y = "Average social support") +
   geom_text(aes(label = mean_social_support),
             color = "yellow",
             hjust = 1.5,
             size = 2)
 
 
 ##Countries ranked by Healthy Life Expectancy Contributing to Happiness
 # Plot top countries by healthy life expectancy
 happiness %>% 
   group_by(Country) %>% 
   summarise(mean_healthy_life_expectancy = mean(healthy_life_expectancy) %>% 
               round(2)) %>% 
   arrange(-mean_healthy_life_expectancy) %>% 
   slice_head(n = 10) %>% 
   ggplot(aes(x = reorder(Country, mean_healthy_life_expectancy),
              y = mean_healthy_life_expectancy,
              fill = mean_healthy_life_expectancy)) +
   geom_col(width = 0.75) +
   coord_flip() +
   guides(fill = "none") +
   labs(title = "Top 10 Countries by Average Healthy Life Expectancy",
        subtitle = "2016 to 2019",
        x = NULL,
        y = "Average healthy life expectancy") +
   geom_text(aes(label = mean_healthy_life_expectancy),
             color = "yellow",
             hjust = 1.5,
             size = 2)
 
 ##Countries ranked by Freedom Contributing to Happiness
 
 # Plot top countries by freedom
 happiness %>% 
   group_by(Country) %>% 
   summarise(mean_freedom_to_make_life_choices = mean(freedom_to_make_life_choices) %>% 
               round(2)) %>% 
   arrange(-mean_freedom_to_make_life_choices) %>% 
   slice_head(n = 10) %>% 
   ggplot(aes(x = reorder(Country, mean_freedom_to_make_life_choices),
              y = mean_freedom_to_make_life_choices,
              fill = mean_freedom_to_make_life_choices)) +
   geom_col(width = 0.75) +
   coord_flip() + 
   guides(fill = "none") +
   labs(title = "Top 10 Countries by Average Freedom to Make Life Choices",
        subtitle = "2016 to 2019",
        x = NULL,
        y = "Average freedom to make life choices") +
   geom_text(aes(label = mean_freedom_to_make_life_choices),
             color = "yellow",
             hjust = 1.5,
             size = 2)
 
 ##Countries ranked by Generosity Contributing to Happiness
 
 # Plot top countries by generosity
 happiness %>% 
   group_by(Country) %>% 
   summarise(mean_Generosity = mean(Generosity) %>% 
               round(2)) %>% 
   arrange(-mean_Generosity) %>% 
   slice_head(n = 10) %>% 
   ggplot(aes(x = reorder(Country, mean_Generosity),
              y = mean_Generosity,
              fill = mean_Generosity)) +
   geom_col(width = 0.75) +
   coord_flip() +
   guides(fill = "none") +
   labs(title = "Top 10 Countries by Average Generosity",
        subtitle = "2016 to 2019",
        x = NULL,
        y = "Average generosity") +
   geom_text(aes(label = mean_Generosity),
             color = "yellow",
             hjust = 1.5,
             size = 2)
 
 ##Countries ranked by Perceptions Corruption Contributing to Happiness
 
 # Plot top countries by absence of corruption
 happiness %>% 
   group_by(Country) %>% 
   summarise(mean_perceptions_of_corruption = mean(perceptions_of_corruption) %>% 
               round(2)) %>% 
   arrange(-mean_perceptions_of_corruption) %>% 
   slice_head(n = 10) %>% 
   ggplot(aes(x = reorder(Country, mean_perceptions_of_corruption),
              y = mean_perceptions_of_corruption,
              fill = mean_perceptions_of_corruption)) +
   geom_col(width = 0.75) +
   coord_flip() +
   guides(fill = "none") +
   labs(title = "Top 10 Countries by Average Perceptions of Corruption",
        subtitle = "2016 to 2019",
        x = NULL,
        y = "Average perceptions of corruption") +
   geom_text(aes(label = mean_perceptions_of_corruption),
             color = "yellow",
             hjust = 1.5,
             size = 2)
 
 
 

 
 

