library(ggplot2)
library(scales)
library(dplyr)
library(tidyr)
library(gganimate)
library(gapminder)
library(ggthemes)

## Import Fruit Consumption data
rm(list=ls())
fruit <- read.csv("./DATA/1-fruit-consumption-per-capita.csv")
fruit <- fruit %>%select(-Code) 
colnames(fruit) <- c("Country","Year","Fruits")
fruits_top <- fruit %>% filter(Year =="2017") %>%  top_n(5,Fruits)
fruits_top <- as.data.frame(fruits_top)

ggplot(fruits_top) +
  geom_col(aes(x=reorder(Country,Fruits),y=Fruits)) + coord_flip() +
  labs(title="Top 5 countries by per Capita Fruit Production")

## Join fruits_top with fruit

countries_five <- fruits_top %>% select(Country) %>% left_join(fruit,by="Country")
head(countries_five)
tail(countries_five)
### Plot Top % Countries by Kg consumed per year per person.
##
ggplot(countries_five) + geom_line(aes(x=Year,y=Fruits,col=Country)) +
  labs(title = "Fruit Consumption Kg/Person/Year",subtitle = "( Top 5 countries)",
       y="Fruit Consumed per person Kg")

countries_five %>% filter(Year >="2010") %>%
  ggplot() + geom_col(aes(x=Country,y=Fruits)) + 
  facet_wrap(~Year,ncol=2) + coord_flip() +
  labs(title="Fruit Consumption by Country by Year/Kg/Person")


ggplot(countries_five) + geom_line(aes(x=Year,y=Fruits,col=Country)) +
  labs(title = "Fruit Consumption Kg/Person/Year",subtitle = "( Top 5 countries)",
       y="Fruit Consumed per person Kg") + facet_wrap(~Country,scale="free_y",ncol=2) +
  geom_smooth(aes(x=Year, y= Fruits),method="lm")


## Part 3: Fruit  Consumption by Type of Fruit, Year and Country

fruit_type <- read.csv("./DATA/3-fruit-consumption-by-fruit-type.csv")
colnames(fruit_type) <-c("Country","Code","Year",
                         "Apples","Bananas","Citrus",
                         "Dates","Other","Grapefruit",
                         "Grapes","Lemons","Oranges",
                         "Pineapples","Plantains")
fruit_type <- fruit_type %>% select(Country:Dates,Grapefruit:Plantains,Other)
summary(fruit_type)
fruit_type_long <- fruit_type %>% pivot_longer(cols = Apples:Other) %>%
  rename("Fruit" = "name") %>% rename("perCapita" = "value") %>% select(-Code)

### PLot of Fruit Consumption Wroldwide by Fruit Type
fruit_type_long %>% filter(Country =="World" & Fruit !="Other") %>% 
  ggplot() + geom_line(aes(x=Year,y=perCapita,col=Fruit,size=0.5)) +
  labs(title="Fruit Consumption per Person by Fruit and Year",
       caption = "(Average fruit consumption per person, differentiated by fruit types, measured in Kilograms per Year)",
       y="Kilograms per Person") + guides(color = guide_legend(override.aes = list(size = 2))) +
  scale_y_continuous(
    labels = scales::number_format(accuracy = 0.01,
                                   decimal.mark = '.')) 


