library (readr)
library(tidyverse)
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggthemes)
library(tidyverse)
library(plotly)
library(DT)
library(car)
library(nortest)
library(tseries)
library(RcmdrMisc)
library(leaflet)
library(shinyjs)
library(data.table)
library(formattable)
library(shinyBS)
library(scales)

customGreen0 = "#DeF7E9"

customGreen = "#71CA97"

customRed = "#ff7f7f"

# This script is for shiny to store all the data and packages 
data_score <-read.csv("covidDataScored.csv", stringsAsFactors = FALSE,check.names =  FALSE)[-1]
data_score$date <- as.Date(data_score$date, format = "%Y-%m-%d")
data_score$overallScore = as.double(data_score$overallScore)

#data with cluster
library(maps)
library(sqldf)
library(gsubfn)
library(proto)
library(RSQLite)

cluster_data = read.csv("../withclusters.csv")
world_map = map_data("world2")
a = 'select location, hdi_cluster from cluster_data'
result = sqldf(a)
clustered_location = unique(result)
for( i in 1:length(clustered_location$location)){
  if ( clustered_location$location[37] == 'Congo' ){
    clustered_location$location[37] = 'Republic of Congo'
  }
  if ( clustered_location$location[i] == 'Democratic Republic of Congo'){
    clustered_location$location[i] = 'Democratic Republic of the Congo'
  }
  if ( clustered_location$location[i] == 'United States'){
    clustered_location$location[i] = 'USA'
  }
  if ( clustered_location$location[i] == 'United Kingdom' ){
    clustered_location$location[i] = 'UK'
  }
  if ( clustered_location$location[i] == 'Czechia'){
    clustered_location$location[i] = 'Czech Republic'
  }
  if ( clustered_location$location[i] =='Timor' ){
    clustered_location$location[i] = 'Timor-Leste'
  }
  if ( clustered_location$location[i] == 'Trinidad and Tobago' ){
    clustered_location$location[i] = 'Trinidad'
  }
  if ( clustered_location$location[i] == "Cote d'Ivoire"){
    clustered_location$location[i] = 'Ivory Coast'
  }
  if ( clustered_location$location[i] == 'Eswatini' ){
    clustered_location$location[i] = 'Swaziland'
  }
}
clustered_world_map = left_join(world_map,clustered_location,by = c('region' = 'location'))
