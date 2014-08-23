plot5 <- function(){
  
  #read data from source files
  NEI <- readRDS("./data/summarySCC_PM25.rds")
  SCC <- readRDS("./data/Source_Classification_Code.rds")
  
  names(SCC)[3] <- "shortName"
  
  #read data into frame - based on the epa guide, section 4.6, any record with an scc code beginning with 22010 or 22300 should be included.  SQLDF package required
  slimFrame <- sqldf("select sum(NEI.Emissions)as 'Emissions', NEI.year from SCC
                      INNER JOIN NEI
                      ON SCC.SCC = NEI.SCC
                      where (SCC.SCC like '22010%' or SCC.SCC like '22300%') and NEI.fips = '24510'
                      GROUP BY NEI.year")
  
  #create plot
  png(filename = "Plot5.png", bg="white", width=520, height=520) 
  print({
    p <- qplot(year,Emissions,data=slimFrame, main="Emissions from motor vehicles: Baltimore, MD")
    p + geom_line()
  })
  
  dev.off()
  
} 
