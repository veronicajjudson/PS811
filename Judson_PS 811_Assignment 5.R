setwd("C:\\Users\\Veronica\\Google Drive\\Veronica Documents\\UW-Madison\\Classes\\Fall 2021\\Intro to Stat Computing 811")

##Download the data 
library(tidyverse)
national <- read_csv("national.csv")

##LOOPS

## Question 1 
## Create a function for the mean, median, and standard deviation
function_mean <- function(x) {
  y <- sum(x) / length(x)
  return(y)
}

mean(national$christianity_protestant)
function_mean(national$christianity_protestant)

function_median <- function(lst) {
  n <- length(lst)
  s <- sort(lst)
  ifelse(n%%2==1,s[(n+1)/2],mean(s[n/2+0:1]))
}

median(national$christianity_protestant)
function_median(national$christianity_protestant)

function_sd <- function(x) {
  y <- sqrt(sum((x - mean(x))^2) / (length(x) - 1))
    return(y)
}

sd(national$christianity_protestant)
function_sd(national$christianity_protestant)


## Question 2 
## Create a function that finds the mean and excludes the lowest and highest value.
function_2 <- function(x) {
  sorted <- sort(x)
  y<- sorted[-c(1, length(sorted))]
  z <- mean(sorted)
  return(z)
}

## Question 3
## Apply the functions to the Christianity variables.
sapply(national[,4:9], function_mean)

sapply(national[,4:9], function_median)

sapply(national[,4:9], function_sd)

sapply(national[,4:9], function_2)


## Question 4
## Write a function that lists all the unique years with more than 300,000 Christians in total.
many_christians <- function(christianity_data) {
  x <- christianity_data %>% group_by(year, state) %>% 
    summarise(alotof_christians = christianity_all > 300000) %>% 
    filter(alotof_christians == T)
  return(x)
}

three_k_christians <- many_christians(national)

## LOOPS/APPLY

## Question 1
## Write a loop to find how many variables there are per observation.
loop_1 <- rep(0, ncol(national))
for (i in 1:length(national)) { 
  loop_1[i] <- ncol(national[i, ])
  }
loop_1

## Question 2
## Write a loop to find the mean number of Protestant Christians in each country (i.e., the state column). 
## Then use an apply family function to do the same.

protestant_data <- data.frame(state = unique(national$state),
                              prots = NA)
for (i in 1:length(protestant_data$state)) { 
  state_i <- protestant_data[i,1]
  x <- national %>% filter(state ==state_i) %>% 
    summarise(mean = mean(christianity_protestant, na.rm = T)) %>%
    pull(mean)
  protestant_data[i,2] <- x 
}

head(protestant_data)

tapply(national$christianity_protestant, national$state, mean, na.rm = T)
  
## Question 3
## Check the column type for each variable.
sapply(national,class)
