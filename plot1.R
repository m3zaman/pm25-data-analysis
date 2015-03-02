download.file("https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip","data.zip", method="curl")
unzip("data.zip")

## Read RDS data from files
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

## Load dplyr package
library(dplyr)

## summarise the total pollution from all sources in variable d where p is the emission total
d <- summarise(group_by(NEI, year), p = sum(Emissions))

## create a linear model for year ~ p
lm.pm25 <- lm(d$p ~d$year)

## plot the data using base plotting system
plot(d$year, d$p, xlab = "Year", ylab = "Total Emission from all sources (ton)",
     main="Emission is decreasing", pch = 19)

## draw the lm line
abline(lm.pm25, col = 'green')

## Copy to PNG
dev.copy(png, filename="./plot1.png")
## Shutting the PNG device
dev.off()