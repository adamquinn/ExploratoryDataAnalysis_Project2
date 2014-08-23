plot2 <- function() {
  
  #read the data from source file
  options(sqldf.driver = "SQLite")
  NEI <- readRDS("./data/summarySCC_PM25.rds")
  
  #read the data into the frame.  SQLDF package required
  slimFrame <- sqldf("select * from NEI where fips = '24510'")
  
  #subset only the columns needed for plot
  smallNEI <- slimFrame[,4]
  smallNEI <- cbind(smallNEI,slimFrame[,6])
  
  #give columns proper names
  smallNEI <- as.data.frame(smallNEI)
  colnames(smallNEI)[1] <- "pollutant"
  colnames(smallNEI)[2] <- "year"
  
  #aggregate data by pollutant and year for plotting
  pollutantByYear <- aggregate(smallNEI$pollutant,list(smallNEI$year),FUN = sum)
  colnames(pollutantByYear)[1] <- "year"
  colnames(pollutantByYear)[2] <- "pollutant"
  
  #plot data
  png(filename = "Plot2.png", bg="white", width=480, height=480)
  options(scipen=5)
  plot(pollutantByYear$year,pollutantByYear$pollutant, type="l", xlab="year", ylab="pollution", main="Total Emissions in Baltimore, MD from 1999 - 2008")
  dev.off()
} 