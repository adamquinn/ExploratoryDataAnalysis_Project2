plot1 <- function() {
  
  #read the data from source files
  NEI <- readRDS("./data/summarySCC_PM25.rds")
  SCC <- readRDS("./data/Source_Classification_Code.rds")
  
  #subset only the columns needed for plot
  smallNEI <- NEI[,4]
  smallNEI <- cbind(smallNEI,NEI[,6])
  
  #give columns proper names
  smallNEI <- as.data.frame(smallNEI)
  colnames(smallNEI)[1] <- "pollutant"
  colnames(smallNEI)[2] <- "year"
  
  #aggregate data by pollutant and year for plotting
  pollutantByYear <- aggregate(smallNEI$pollutant,list(smallNEI$year),FUN = sum)
  colnames(pollutantByYear)[1] <- "year"
  colnames(pollutantByYear)[2] <- "pollutant"
  
  #print((pollutantByYear))
  
  #plot data
  png(filename = "Plot1.png", bg="white", width=480, height=480)
  options(scipen=5)
  plot(pollutantByYear$year,pollutantByYear$pollutant, type="l", xlab="year", ylab="pollution", main="Total Emissions in the U.S. from 1999 - 2008")
  dev.off()
}