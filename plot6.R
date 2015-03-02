download.file("https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip","data.zip", method="curl")
unzip("data.zip")

## Read RDS data from files
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

## Load dplyr package
library(dplyr)

## Filter the NEI data for Baltimore City, Maryland (fips == 24510)
bc <- filter(NEI, fips == "24510")

## Filter the NEI data for Los Angeles County, California ( fips == "06037")
la <- filter(NEI, fips == "06037")

## Filter sources from SCC which are related to "Mobile ... Vehicles"
## Def of Motor Vehicle http://en.wikipedia.org/wiki/Motor_vehicle
## I picked SCC$EI.Sector for this data source
s <- filter(SCC, grepl("^Mobile.+Vehicles$", as.character(SCC$EI.Sector)))

## merge data based on SCC for Baltimore
bc <- merge(bc, s, by.x="SCC", by.y="SCC")

## merge data based on SCC for Los Angeles
la <- merge(la, s, by.x="SCC", by.y="SCC")

## summarise the total pollution from all sources in variable d where p is the emission total
##Baltimore
dbc <- summarise(group_by(bc, year), p = sum(Emissions))
##Los Angeles
dla <- summarise(group_by(la, year), p = sum(Emissions))

## create a linear model for year ~ p for Baltimore
bc.lm.pm25 <- lm(dbc$p ~ dbc$year)

## create a linear model for year ~ p for Los Angeles
la.lm.pm25 <- lm(dla$p ~ dla$year)

## mfrow 1,2
par(mfrow = c(1, 2))

## plot the data using base plotting system for baltimore
plot(dbc$year, dbc$p, xlab = "Year", ylab = "Total Emission from selected sources (ton)",
     main="Emissions - Baltimore City", pch = 19)

## draw the lm line for baltimore
abline(bc.lm.pm25, col = 'blue')

## plot the data using base plotting system for Los Angeles
plot(dla$year, dla$p, xlab = "Year", ylab = "Total Emission from selected sources (ton)",
     main="Emissions - Los Angeles", pch = 19)

## draw the lm line for Los Angeles
abline(la.lm.pm25, col = 'blue')


## Copy to PNG
dev.copy(png, filename="./plot6.png")
## Shutting the PNG device
dev.off()