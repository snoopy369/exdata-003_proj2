source("readin.R") #This reads in NEI and SCC
require(plyr)
require(reshape)

#Identify SCC codes that involve Motor Vehicles)
SCC_Vehicles <- SCC[grep("Vehicle",SCC$SCC.Level.Two),]

#Subset NEI to those SCC codes
NEI_Vehicles <- NEI[which(NEI$SCC %in% SCC_Vehicles[,1] & NEI$fips %in% c("24510","06037")),]

countyNames <- data.frame(fips=c("24510","06037"),
                          countyName=c("Baltimore City","Los Angeles County"))

NEI_Vehicles_names <- merge(NEI_Vehicles,countyNames,by="fips")
#Summarize by SCC code
summ_emissions <- ddply(NEI_Vehicles_names,.(year,countyName,SCC),summarise,Sum=sum(Emissions))  
#Append the SCC Level Two code
summ_emissions_scc <- merge(summ_emissions,SCC_Vehicles[,c(1,8)],by="SCC")

#Create county/year variable (used for bars)
summ_emissions_scc$countyYear <- paste(summ_emissions_scc$countyName,summ_emissions_scc$year,
                                       sep=" ")

#Determine scaling factor
summ_emissions_1999 <- ddply(NEI_Vehicles_names[NEI_Vehicles_names$year==1999,],.(countyName),
                             summarise,Sum=sum(Emissions))  
#Add to dataset
summ_emissions_scaled <- merge(summ_emissions_scc,summ_emissions_1999,by="countyName")
#Calculate scaled sum
summ_emissions_scaled$Sum <- summ_emissions_scaled$Sum.x/summ_emissions_scaled$Sum.y

#Create PNG file
png(filename="plot6.png",height=480,width=720)

ggplot(data=summ_emissions_scaled,aes(x=countyYear,y=Sum,fill=countyName))+geom_bar(stat="identity")+
  ggtitle("PM2.5 Emissions from Vehicle Sources as % of 1999 Emissions, \nBaltimore City and Los Angeles County, 1999-2008") +
  scale_x_discrete(name="Year Collected",labels=c("1999","2002","2005","2008","1999",
                                                  "2002","2005","2008")) +
  scale_y_continuous(name="Total PM2.5 Emissions for Vehicle Sources Normalized to 1999 Levels")

dev.off()