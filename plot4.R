plot4 <- function(){
  NEI <- readRDS("./data/summarySCC_PM25.rds")
  SCC <- readRDS("./data/Source_Classification_Code.rds")
  
  names(SCC)[3] <- "shortName"
  
  #Read data into dataframe.  Decision was made to include any records containing the word 'coal' in teh EI_Sector. SQLDF Package required column of the table  
  slimFrame <- sqldf("select sum(NEI.Emissions) as 'Emissions', SCC.EI_Sector as 'sources', NEI.year from SCC
                      INNER JOIN NEI
                      ON SCC.SCC = NEI.SCC
                      where EI_Sector like '%coal%'
                      group by SCC.EI_Sector, NEI.year")
  
  #create plot  
  png(filename = "Plot4.png", bg="white", width=520, height=520)    
  print({
    ggplot(data=slimFrame, aes(x=year,y=Emissions,group=sources, colour=sources)) + 
      geom_line() + 
      geom_point() +
      scale_fill_discrete(name="Coal Combustion Related Sources") +
      ggtitle("Emissions from coal combustion-related sources ") +
      xlab("Year") +
      ylab("Emission Amount")
  })
  dev.off()
  
}