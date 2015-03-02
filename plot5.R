##download.file("https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip","data.zip", method="curl")
##unzip("data.zip")

## Read RDS data from files
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

## Load dplyr package
library(dplyr)

## Filter the NEI data for Baltimore City, Maryland (fips == 24510)
NEI <- filter(NEI, fips == "24510")

## Filter sources from SCC which are related to "Mobile ... Vehicles"
## Def of Motor Vehicle http://en.wikipedia.org/wiki/Motor_vehicle
## I picked SCC$EI.Sector for this data source
s <- filter(SCC, grepl("^Mobile.+Vehicles$", as.character(SCC$EI.Sector)))

## merge data based on SCC
NEI <- merge(NEI, s, by.x="SCC", by.y="SCC")

## summarise the total pollution from all sources in variable d where p is the emission total
d <- summarise(group_by(NEI, year), p = sum(Emissions))

## create a linear model for year ~ p
lm.pm25 <- lm(d$p ~d$year)

## plot the data using base plotting system
plot(d$year, d$p, xlab = "Year", ylab = "Total Emission from selected sources (ton)",
     main="Emissions from Motor Vehicles in Baltimore City", pch = 19)

## draw the lm line
abline(lm.pm25, col = 'blue')

## Copy to PNG
dev.copy(png, filename="./plot5.png")
## Shutting the PNG device
dev.off()