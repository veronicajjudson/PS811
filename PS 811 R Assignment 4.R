setwd("C:\\Users\\Veronica\\Google Drive\\Veronica Documents\\UW-Madison\\Classes\\Fall 2021")

## Question 1/2 download the data
food <- read.csv("food_coded.csv")


## Question 3 Extract the data 
foodextract <- food[1:95,]


## Question 4 
##Look at the following variables using both name and column index/number.
foodindex <- foodextract[,c("GPA", "calories_chicken", "drink", "fav_cuisine", "father_profession", "mother_profession")]

##Question 5 
##Create a new variable for how healthy each person feels but convert the scale from 1 to 10 to 1 to 100.
library(scales)
food$newhealthy_feeling <- rescale(food$healthy_feeling, to = c(1, 100))

##Question 6 
##Filter to students who are female and have GPAs that are above 3.0.
##Female is 1, male is 2 
food$GPA1 <- as.numeric(as.character(food$GPA))
food[74, 63] <- 3.79
femalefilter <- food[food$Gender == "1" & food$GPA1 > 3,]
head(femalefilter)

##Question 7 
##Find the mean and standard deviation for the following variables, and summarize them in a data frame. 
      ##chicken_calories
      ##tortilla_calories
      ##turkey_calories
      ##waffle_calories
foodcode <- foodextract[,c("calories_chicken", "tortilla_calories", "turkey_calories", "waffle_calories")]

foodcode$mchic <- mean(foodcode$calories_chicken)
foodcode$sdchic <- sd(foodcode$calories_chicken)

foodcode$mtort <- mean(foodcode$tortilla_calories)
foodcode$sdtort <- sd(foodcode$tortilla_calories)

foodcode$mturk <- mean(foodcode$turkey_calories)
foodcode$sdturk <- sd(foodcode$turkey_calories)

foodcode$mwaff <- mean(foodcode$waffle_calories)
foodcode$sdwaff <- sd(foodcode$waffle_calories)

head(foodcode)

## Question 8 
## Summarize GPA and weight within the gender and cuisine variables.
food$weight <- as.numeric((food$weight))
food$weight[4] <- 240
food$weight[68] <- 144

food_men <- food[food$Gender == 1,]
food_women <- food[food$Gender == 2,]

men_GPA1_mean <- tapply(food_men$GPA1, food_men$cuisine, mean, na.rm = T)
women_GPA1_mean <- tapply(food_women$GPA1, food_women$cuisine, mean, na.rm = T)

men_weight_mean <- tapply(food_men$weight, food_men$cuisine, mean, na.rm = T)
women_weight_mean <- tapply(food_women$weight, food_women$cuisine, mean, na.rm = T)

men_GPA1_sd <- tapply(food_men$GPA1, food_men$cuisine, sd, na.rm = T)
women_GPA1_sd <- tapply(food_women$GPA1, food_women$cuisine, sd, na.rm = T)

men_weight_sd <- tapply(food_men$weight, food_men$cuisine, sd, na.rm = T)
women_weight_sd <- tapply(food_women$weight, food_women$cuisine, sd, na.rm = T)

##Tidyverse 
##Download the facebook-fact-check.csv
##Load the CSV file into your R environment.
library(tidyverse)
facebookdata <- read_csv("facebook-fact-check.csv")

##Extract the last 500 rows.
facebookdata %>% slice_tail(n = 500)
## %>% is called pipe 

##Look at the even-numbered column indices only. Identify them by name.
facebookdata %>% select(c(2,4,6,8,10,12)) %>% colnames()
## "post_id" "page" "date.published" "rating" "share_count" "comment_count" 

##Using `mutate`, create a new variable called `post_type_coded` that renames each post type to the following:
  
  ##link = 1
  ##photo = 2
  ##text = 3
  ##video = 4

facebookdata <- facebookdata %>% 
  mutate(post_type_coded = case_when(
    `Post Type`== "link" ~ 1,
    `Post Type` == "photo" ~ 2,
    `Post Type` == "text" ~ 3,
    `Post Type` == "video" ~ 4
  ))

##Arrange page names in reverse order.
facebookdata %>% arrange(desc(Page))

##Find the mean and standard deviation for the following variables, and summarize them.

  ##share_count
  ##reaction_count
  ##comment_count

facebookdata %>% 
  summarise(across(c(share_count, reaction_count, comment_count), 
                   list(mean = mean, sd = sd), na.rm = T))

##Summarize the mean and standard deviations in Question 7 with the "mainstream" values in the `category` variable.

facebookdata %>% 
  filter(Category == "mainstream") %>% 
  summarise(across(c(share_count, reaction_count, comment_count), 
                   list(mean = mean, sd = sd), na.rm = T)) 
