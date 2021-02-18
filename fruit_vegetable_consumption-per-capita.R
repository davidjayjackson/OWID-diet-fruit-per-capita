library(ggplot2)
library(scales)
library(dplyr)
library(tidyr)
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
### Plot Top % Countries by Kg cosummed per year per person.
##
ggplot(countries_five) + geom_line(aes(x=Year,y=Fruits,col=Country)) +
  labs(title = "Fruit Consumption Kg/Person/Year",subtitle = "( Top 5 countries)",
       y="Fruit Consumed per person Kg")
### Plot of US Fruit consumption 

fruit %>% filter(Country =="United States") %>%
  ggplot() + geom_line(aes(x=Year,y=Fruits)) +
  labs(title="US Fruit Consumption: Kg/Year/Person (1960-2017)",
       y="Kilograms Per Person")
  