source("readin.R") #This reads in NEI and SCC
require(plyr)
require(gridExtra)

#Identify SCC codes that involve Motor Vehicles)
SCC_Vehicles <- SCC[grep("Vehicle",SCC$SCC.Level.Two),]

#Subset NEI to those SCC codes
NEI_Vehicles <- NEI[which(NEI$SCC %in% SCC_Vehicles[,1] & NEI$fips == "24510"),]


#Summarize by SCC code
summ_emissions <- ddply(NEI_Vehicles,.(year,SCC),summarise,Sum=sum(Emissions))  
#Append the SCC Level Two code
summ_emissions_scc <- merge(summ_emissions,SCC_Vehicles[,c(1,8)],by="SCC")
#Now create the 100% proportioned percent sum
summ_emissions_fin <- ddply(summ_emissions_scc, "year", transform,
                            percent_Sum = Sum / sum(Sum) * 100)
#Create PNG file
png(filename="plot5.png",height=960,width=720)

#Plot 1: Total level
p1<-ggplot(data=summ_emissions_scc,aes(x=year,y=Sum,fill=SCC.Level.Two))+geom_bar(stat="identity") +
  ggtitle("PM2.5 Emissions from Vehicle Sources, Baltimore City, 1999-2008") +
  scale_x_continuous(name="Year Collected",
                     breaks=c(1999,2002,2005,2008), 
                     labels=c("1999", "2002", "2005","2008")) +
  scale_y_continuous(name="Total PM2.5 Emissions for Vehicle Sources, Baltimore City")

#plot 2: Proportional to total Vehicle emissions of that year
p2<-ggplot(data=summ_emissions_fin,aes(x=year,y=percent_Sum,fill=SCC.Level.Two,label=Sum))+geom_bar(stat="identity") +
  ggtitle("PM2.5 Emissions from Vehicles By Secondary Source, Baltimore City, 1999-2008") +
  scale_x_continuous(name="Year Collected",
                     breaks=c(1999,2002,2005,2008), 
                     labels=c("1999", "2002", "2005","2008")) +
  scale_y_continuous(name="Percent of Total PM2.5 Emissions for Vehicle Sources, Baltimore City")

#Present both (using grid.arrange from gridExtra)
grid.arrange(p1,p2)
dev.off()