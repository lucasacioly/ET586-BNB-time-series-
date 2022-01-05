# data manipulation

library(quantmod)
library(shiny)
library(shinydashboard)
library(dplyr)
library(ggplot2)
library(DT)
library(tidyverse)
library(lubridate)
library(corrplot)

# importar arquivo com os dados
master_df <- read.csv('BNB-USD-final_2.csv', header = TRUE, sep = ";")

parameters_list <- c('Abertura', 'Fechamento', 'Volume')

master_df$X <- NULL

master_df <- master_df %>% drop_na()
master_df$Date <- strptime(master_df$Date, format='%d/%m/%Y')

filter_date <- function(df, interval) {
  df[
    interval[1] <= df$Date &
      interval[2] >= df$Date,
  ] %>%
    return()
}