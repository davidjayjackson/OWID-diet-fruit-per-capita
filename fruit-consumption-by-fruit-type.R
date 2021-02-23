## Worldwide: Fruit Consumption By Fruit Type (1961 - 2013)
### Data Source: http://ourworldindata.org

library(ggplot2)
library(scales)
library(dplyr)
library(tidyr)
library(gganimate)
library(gapminder)
library(ggthemes)
rm(list=ls())
fruit_type <- read.csv("./DATA/3-fruit-consumption-by-fruit-type.csv")
colnames(fruit_type) <-c("Country","Code","Year",
                         "Apples","Bananas","Citrus",
                         "Dates","Other","Grapefruit",
                         "Grapes","Lemons","Oranges",
                         "Pineapples","Plantains")
fruit_type <- fruit_type %>% select(Country:Dates,Grapefruit:Plantains,Other)
summary(fruit_type)

### Dplyr::pivot_lionger

fruit_type_long <- fruit_type %>% pivot_longer(cols = Apples:Other) %>%
  rename("Fruit" = "name") %>% rename("perCapita" = "value") %>% select(-Code)
summary(fruit_type_long)

### PLot of Fruit Consumption Wroldwide by Fruit Type

fruit_type_long %>% filter(Country =="World" & Fruit !="Other") %>% 
  ggplot() + geom_line(aes(x=Year,y=perCapita,col=Fruit),size=1.5) +
  labs(title="Worldwide: Fruit Consumption per Person by Fruit and Year",
       caption = "(Average fruit consumption per person, differentiated by fruit types, measured in Kilograms per Year)",
       y="Kilograms per Person") + guides(color = guide_legend(override.aes = list(size = 2))) +
  scale_y_continuous(
    labels = scales::number_format(accuracy = 0.01,
                                   decimal.mark = '.')) 

### Plot US Yearly Fruit Consumption

fruit_type_long %>% filter(Country =="United States" & Fruit !="Other") %>% 
  ggplot() + geom_line(aes(x=Year,y=perCapita,col=Fruit,),size=1.5) +
  labs(title="United States:Fruit Consumption per Person by Fruit and Year",
       caption = "(Average fruit consumption per person, differentiated by fruit types, measured in Kilograms per Year)",
       y="Kilograms per Person") + guides(color = guide_legend(override.aes = list(size = 2))) +
  scale_y_continuous(
    labels = scales::number_format(accuracy = 0.01,
                                   decimal.mark = '.')) 


### Fruits Consumption Per Person Per Year  for top 5 Countries

fruit <- read.csv("./DATA/1-fruit-consumption-per-capita.csv")
fruit <- fruit %>%select(-Code) 
colnames(fruit) <- c("Country","Year","Fruits")
fruits_top <- fruit %>% filter(Year =="2017") %>%  top_n(5,Fruits)
fruits_top <- as.data.frame(fruits_top)

countries_five <- fruits_top %>% select(Country) %>% left_join(fruit_type_long,by="Country") %>% na.omit()
head(countries_five)
tail(countries_five)

###
ggplot(countries_five) + geom_line(aes(x=Year,y=perCapita,col=Fruit),size=1.5) + 
  facet_wrap(~Country,scale="free_y",ncol=2) + labs(title="Top 5 Countries by Fruit")

countries_five %>%filter(Year >="2007") %>%
  ggplot() + geom_col(aes(x=reorder(Fruit,perCapita),y=perCapita)) + 
  facet_wrap(~Year,scale="free_y",ncol=2) + coord_flip() +
  labs(title="Fruit Consumption by Year",y="Kilograms/Person/Year")

