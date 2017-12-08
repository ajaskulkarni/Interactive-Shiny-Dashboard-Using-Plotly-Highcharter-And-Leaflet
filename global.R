#### Install and Load libraries ####
library(shiny)
library(shinydashboard)
library(leaflet)
library(geojson)
library(geojsonio)
library(RColorBrewer)
library(plotly)
library(ggmap)
library(highcharter)
library(plyr)
library(scales)

#### Load data ####
data1 = read.csv("School_University_100.csv",                      
                 stringsAsFactors = FALSE, 
                 check.names = FALSE)

data2 = read.csv("Final_Data.csv",                      
                stringsAsFactors = FALSE, 
                check.names = FALSE)

data2_sorted = read.csv("Sorted_Final_Data.csv",                      
                 stringsAsFactors = FALSE, 
                 check.names = FALSE)

# URL for the data
url = "http://leafletjs.com/examples/choropleth/us-states.js"

# read as text file
doc = readLines(url)

# remove the javascript assignment at the gront
doc2 = gsub("var statesData = ", "", doc)

# write out as a temo file and read
write(doc2, file ="tempgeo.json")
states = geojsonio::geojson_read("tempgeo.json",what = "sp")
class(states)
names(states)

convertMenuItem = function(mi,tabName) {
  
  mi$children[[1]]$attribs['data-toggle'] = "tab"
  mi$children[[1]]$attribs['data-value'] = tabName
  
  if(length(mi$attribs$class) > 0 && mi$attribs$class == "treeview"){
    mi$attribs$class = NULL
  }
  
  mi
}