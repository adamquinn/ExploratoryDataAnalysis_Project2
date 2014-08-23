plot3<- function(){
  #read the data from the source files
  NEI <- readRDS("./data/summarySCC_PM25.rds")
  SCC <- readRDS("./data/Source_Classification_Code.rds") 
  
  #read the data into the frame. SQLDF pacakge required
  slimFrame <- sqldf("select * from NEI where fips = '24510'")
  
  #subset only the columns needed for the plot
  smallNEI <- slimFrame[,4]
  smallNEI <- cbind(smallNEI,slimFrame[,5])
  smallNEI <- cbind(smallNEI,slimFrame[,6])
  
  #give columns proper names
  smallNEI <- as.data.frame(smallNEI)
  colnames(smallNEI)[1] <- "pollutant"
  colnames(smallNEI)[2] <- "type"
  colnames(smallNEI)[3] <- "year"
  
  smallNEI$pollutant <- as.numeric(as.character(smallNEI$pollutant))
  
  
  #aggregate data by pollutant, year, and type
  pollutantByYear <- aggregate(smallNEI$pollutant, list(smallNEI$year,smallNEI$type),FUN = sum)
  
  
  colnames(pollutantByYear)[1] <- "year"
  colnames(pollutantByYear)[2] <- "type"
  colnames(pollutantByYear)[3] <- "pollutant"
  
  #create plot
  png(filename = "Plot3.png", bg="white", width=520, height=520)
  print({
    p <- qplot(year,pollutant,data=pollutantByYear,facets=.~type, main="Emission Output By source: Baltimore, MD")
    p + geom_line(aes(group = type))
  }
  )
  dev.off()
  
}