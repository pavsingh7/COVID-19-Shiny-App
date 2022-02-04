
# The melt function takes data in wide format and stacks a set of columns into a single column of data
# https://www.r-bloggers.com/2012/04/melt/

library(reshape2)

tolong <- function(data){
  data %>% melt(
    id.vars=c("date", "country"),
    measured.vars=c("confirmed", "recovered", "dead"))
}

##### TESTING #####

