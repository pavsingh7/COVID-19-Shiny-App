#^&^#^^%*&@*(#":|?@##))@:?"?&^& READ DATA AND CREATE VARIABLE

library(dplyr)
library(tidyr)
library(purrr)


# lapply() -  for operations on list objects and returns a list object of same length of original set
# select() keeps only the variables you mention
# gather() collects a set of column names and places them into a single “key” column
# group_by() groups the data frames
# summarise() creates a new data frame. It will have one (or more) rows for each combination of grouping variables
# ungroup() removes grouping
# mutate() adds new variables and preserves existing ones

cleanSingle <- function(x){
  x %>%
    select(-Province.State, -Lat, -Long) %>% 
    gather(key="date", value="Value", -Country.Region) %>% 
    group_by(Country.Region, date) %>% 
    summarise("Value"=sum(Value)) %>% 
    ungroup() %>% 
    select(date, "country"=Country.Region, Value) %>% 
    mutate(date, date=as.Date(substring(date, 2, nchar(date)),"%m.%d.%y"))
}

read_github <- function(){
  # Load most updated data
  confirmed_w <- read.csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv")
  deaths_w <- read.csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv")
  recovered_w <- read.csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_recovered_global.csv")
  res <- list(confirmed_w, deaths_w, recovered_w) %>% 
    lapply(cleanSingle) %>% 
    reduce(left_join,by = c("date", "country"))
  
  colnames(res)[3:5] <- c("confirmed", "deaths", "recovered")
  
  return(res)  
}





############  #######################  ###########
############ TESTING
############  #######################  ###########

confirmed_w <- read.csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv")
deaths_w <- read.csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv")
recovered_w <- read.csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_recovered_global.csv")

confirmed_w
res <- list(confirmed_w, deaths_w, recovered_w)
res <- list(confirmed_w, deaths_w, recovered_w) %>% 
  lapply(cleanSingle) %>%  reduce(left_join,by = c("date", "country"))

read_github() #nice
  