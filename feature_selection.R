library(tidyverse)

# Load natural gas consumption data
demand <- read_csv("./data/230222_Restlast.csv", col_types = cols(date = col_date(format = "%Y-%m-%d")))
demand <- subset(demand, date != as.Date("2019-01-01")) # Outlier

