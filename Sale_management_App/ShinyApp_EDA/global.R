# DATA423 - DATA SCIENCE IN INDUSTRY - ASSIGNMENT 1
# Student: Giang Bui
# ID: 37306207
# Date: 06/03/2023

# Define Global for application that draws a histogram
library(shiny)
# library(thematic)
library(tidyr)
library(dplyr)
library(ggplot2)
library(DT)
library(vcd)
library(summarytools)
library(MASS)
library(RColorBrewer)
library(seriation)
library(datasets)
library(corrgram)
library(visdat)
library(tidytext)
library(tidyverse)
library(stringr)
library(matrixStats)

df <- read.csv("Ass1Data.csv", header = TRUE, stringsAsFactors = TRUE)
#colnames(df)[sapply(df, class) == 'character'] #Check all the variables belonging to character class
df <- df %>% 
  dplyr::mutate(Id_Group = factor(substr(ID, 1,1)), 
         Priority = factor(Priority, levels = c("Low", "Medium", "High"), ordered = TRUE),
         Price = factor(Price, levels = c(NA, "Cheap", "Fair", "Expensive"), ordered = TRUE),
         Speed = factor(Speed, levels = c(NA, "Slow", "Medium", "Fast"), ordered = TRUE),
         Duration = factor(Duration, levels = c(NA, "Short", "Long", "Very Long"), ordered = TRUE),
         Temp = factor(Temp, levels = c(NA, "Cold", "Warm", "Hot"), ordered = TRUE),
         Date = as.Date(Date,format = "%Y-%m-%d", sep = "-")
         )

num_data <- dplyr::select(df,1, 15:44) #numerical variables only
cat_data <- dplyr::select(df, 3, 5:14, 45) #categorical variables only
df_num_1 <- num_data %>% 
  dplyr::select("sensor1", "sensor2", "sensor3", "sensor4", "sensor5", "sensor6",
                "sensor7", "sensor8", "sensor9", "sensor10")
df_num_2 <- num_data %>% 
 dplyr::select("sensor11", "sensor12", "sensor13", "sensor14", "sensor15",
         "sensor16", "sensor17","sensor18", "sensor19", "sensor20")
df_num_3 <- num_data %>% 
  dplyr::select("sensor21", "sensor22", "sensor23", "sensor24", "sensor25", "sensor26",
         "sensor27", "sensor28","sensor30", "sensor29")

option1 <- c("Operator","Priority","Price","Speed","Duration","Temp","State","Location",
              "Agreed","Class", "Surface", "Id_Group")

option2 <- c("sensor1", "sensor2", "sensor3", "sensor4", "sensor5",
              "sensor6", "sensor7", "sensor8", "sensor9", "sensor10",
              "sensor11", "sensor12", "sensor13", "sensor14", "sensor15",
              "sensor16", "sensor17", "sensor18", "sensor19", "sensor20",
              "sensor21", "sensor22", "sensor23", "sensor24", "sensor25",
              "sensor26", "sensor27", "sensor28", "sensor29", "sensor30", "Y"
)
option2_1 <- c("sensor1", "sensor2", "sensor3", "sensor4", "sensor5",
               "sensor6", "sensor7", "sensor8", "sensor9", "sensor10")
option2_2 <- c("sensor11", "sensor12", "sensor13", "sensor14", "sensor15",
               "sensor16", "sensor17", "sensor18", "sensor19", "sensor20")
option2_3 <- c("sensor21", "sensor22", "sensor23", "sensor24", "sensor25",
               "sensor26", "sensor27", "sensor28", "sensor29", "sensor30")

#option2_default <- c("Y","sensor1", "sensor11", "sensor21", "sensor5", "sensor13")

special_sensor <- c("sensor5", "sensor6", "sensor9", "sensor14", "sensor18","sensor23", "sensor26", "sensor29")
normal_sensor <- c("sensor1", "sensor2", "sensor3", "sensor4", "sensor7", "sensor8", "sensor10",
                   "sensor11", "sensor12", "sensor13", "sensor15", "sensor16", "sensor17", "sensor19", "sensor20",
                   "sensor21", "sensor22", "sensor24", "sensor25", "sensor27", "sensor28",  "sensor30")
