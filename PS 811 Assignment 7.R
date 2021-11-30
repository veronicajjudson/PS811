## Create the following graphs in ggplot2.
## 1. Check out the base R built-in dataset, data("USArrests").
data("USArrests")
library(tidyverse)
head(USArrests)
## %>% (use control shift m)

## 2. Create a scatterplot that looks at the correlation between murder and assault arrests. Label the x and y axes and title the graph.
USArrests %>% ggplot(aes(Murder, Assault)) + 
  geom_point() + 
  labs(title = "Relationship between Murder and Assault in the United States", 
       x = "Murders", y = "Assaults")

## 3. Create a boxplot of rape arrests. Label the plot.
USArrests %>% ggplot(aes(y=Rape)) + 
  geom_boxplot(width = .1) + 
  labs(title = "Box Plot of Rape Arrests in the USA", 
       x = "Rape", y = "Arrests") + 
  theme(axis.ticks.x = element_blank(), 
        axis.ticks.y = element_blank())

## 4. Create a barplot of the number of rape arrests per state.
USArrests %>% 
  arrange(desc(Rape)) %>% 
  rownames_to_column("State") %>% 
  ggplot(aes(reorder(State, Rape), Rape)) + 
  geom_col(color = "white") + 
  scale_y_continuous(expand = c(0,0)) + 
  labs(title = "Rape Arrests by State per 100,000 residents", 
       subtitle = "USA, 1973", 
       x = "State", y = "Rape Arrests")

## 5. Create a histogram for the percent of urban population.
USArrests %>% ggplot(aes(x=UrbanPop)) + 
  geom_histogram(binwidth=3, fill="#69b3a2", color="#e9ecef", alpha=0.9) + 
  labs(title = "Percent of Urban Population", 
       x = "UrbanPop per 100,000 Residents", y = "Frequency") + 
  theme(axis.ticks.x = element_blank(), 
        axis.ticks.y = element_blank())

