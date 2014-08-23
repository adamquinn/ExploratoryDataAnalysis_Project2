plot6 <- function(){
  
  #read data from source file
  baseFrame <- data.frame()
  NEI <- readRDS("./data/summarySCC_PM25.rds")
  SCC <- readRDS("./data/Source_Classification_Code.rds")
  
  names(SCC)[3] <- "shortName"
  
  #read data into frame - select all data from baltimore and california - based on the epa guide, section 4.6, any record with an scc code beginning with 22010 or 22300 should be included. SQLDF package required
  baseFrame <- sqldf("select sum(NEI.Emissions)as 'Emissions', NEI.year, NEI.fips from SCC
                     INNER JOIN NEI
                     ON SCC.SCC = NEI.SCC
                     where (SCC.SCC like '22010%' or SCC.SCC like '22300%') and (NEI.fips = '24510' or NEI.fips = '06037')
                     GROUP BY NEI.fips,NEI.year")
  
  #convert to factor to help with plotting
  as.factor(baseFrame$year)
  as.factor(baseFrame$fips)
  
  #split data by fips code into a list of data frames
  fipsFrame <- (split(baseFrame,baseFrame$fips))
  
  text <- vector()
  pct <- vector()
  fips <- vector()
  
  #outside loop will loop through each FIPS code and inside loop will loop through each record for that FIPS code
  i <- 1
  while(i <= length(fipsFrame)){
    
    j <- 1
    
    while (j < nrow(fipsFrame[[i]])){
      text[length(text)+1] <- paste(fipsFrame[[i]][j,2],fipsFrame[[i]][j+1,2],sep="-")
      pct[length(pct)+1] <- abs(((fipsFrame[[i]][j+1,1] - fipsFrame[[i]][j,1]) / fipsFrame[[i]][j,1]) * 100)
      fips[length(fips)+1] <- fipsFrame[[i]][j,3]
      j <- j+1
    }
    
    i <- i +1
  }
  
  #put records back in data frame
  PctChgFrame <- data.frame(text,pct,fips)
  
  png(filename = "Plot6.png", bg="white", width=520, height=520) 
  print({  
    ggplot(data = PctChgFrame, aes(x=text,y=pct, fill=fips)) + 
      geom_bar(stat="identity", position=position_dodge()) +
      ggtitle("Motor vehicle emissions: Balitimore vs. LA county") +
      xlab("Time Range") +
      ylab("% Change") +
      scale_fill_discrete("Location",labels=c("Los Angeles County","Baltimore City"))
  })
  dev.off()
}