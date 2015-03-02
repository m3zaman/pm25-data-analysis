download.file("https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip","data.zip", method="curl")
unzip("data.zip")

## Read RDS data from files
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

## Load dplyr package
library(dplyr)

## Filter the NEI data for Baltimore City, Maryland (fips == 24510)
NEI <- filter(NEI, fips == "24510")

## summarise the total pollution from all sources in variable d where p is the emission total
d <- summarise(group_by(NEI, year, type), pm25.emission = sum(Emissions))

## plot the data using ggplot2 plotting system
library(ggplot2)
## The actual graph here
plt <- qplot(year, pm25.emission, data = d, facets = . ~ type, geom = c('point', 'smooth'), method = "lm")
print(plt)
## Copy to PNG
dev.copy(png, filename="./plot3.png")
## Shutting the PNG device
dev.off()