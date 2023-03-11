# load required libraries
library(httr)
library(XML)
library(dplyr)

# retrieve XML data from URL
url <- "https://datenservice.tradinghub.eu/XmlInterface/getXML.ashx?ReportId=AggregatedConsumptionData&Start=01-01-2019&End=05-03-2023"
xml_data <- GET(url) %>% content(as="text")

# parse XML data and extract relevant information
xml_parsed <- xmlParse(xml_data)
xml_list <- xmlToList(xml_parsed)

# remove schema definition
xml_list <- xml_list[-1]

# convert to dataframe
df <- as.data.frame(do.call(rbind, xml_list), stringsAsFactors = FALSE)

# rename columns
colnames(df) <- c("Gasday", "HGasSLPsyn", "HGasSLPana", "LGasSLPsyn", "LGasSLPana", "HGasRLMmT", "LGasRLMmT", "HGasRLMoT", "LGasRLMoT", "Status", "Unit")
df <- do.call(rbind, xml_list)

# Write CSV
write.csv(df, "data/industry_including_powergen.csv", row.names = FALSE)
