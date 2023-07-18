# DATA423 - DATA SCIENCE IN INDUSTRY - ASSIGNMENT 2
# Student: Giang Bui
# ID: 37306207
# Date: 27/03/2023

# Define Global for application that draws a histogram
library(shiny)
library(ggplot2)
library(naniar)
library(UpSetR)
library(rpart)
library(caret)
library(rpart.plot)
library(recipes)
library(dplyr)
library(Metrics)
library(RColorBrewer)
library(DT)
library(summarytools)
library(corrgram)
option1 <- c("POLITICS", "HEALTHCARE_BASIS", "OBS_TYPE")
option2 <- c("POPULATION", "AGE25_PROPTN", "AGE_MEDIAN", "AGE50_PROPTN", 
             "POP_DENSITY", "GDP", "INFANT_MORT", "DOCS", "VAX_RATE",
             "HEALTHCARE_COST", "DEATH_RATE")
option3 <- c("POP_DENSITY", "DOCS", "GDP", "AGE50_PROPTN", "INFANT_MORT",
             "VAX_RATE", "AGE25_PROPTN", "POLITICS", "HEALTHCARE_COST", 
             "HEALTHCARE_BASIS", "AGE_MEDIAN","POPULATION")