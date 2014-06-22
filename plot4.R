source("readin.R") #This reads in NEI and SCC
require(plyr)
require(gridExtra)

#Identify SCC codes that involve coal
SCC_Coal <- SCC[grep("Coal",SCC$SCC.Level.Three),]

#Subset NEI to those SCC codes
NEI_Coal <- NEI[NEI$SCC %in% SCC_Coal[,1],]

#Summarize by SCC code
summ_emissions <- ddply(NEI_Coal,.(year,SCC),summarise,Sum=sum(Emissions))  
#Append the SCC Level One code (going all the way to Level 3 would be too many to graph)
summ_emissions_scc <- merge(summ_emissions,SCC_Coal[,c(1,7)],by="SCC")
#Now create the 100% proportioned percent sum
summ_emissions_fin <- ddply(summ_emissions_scc, "year", transform,
                                  percent_Sum = Sum / sum(Sum) * 100)
#Create PNG file
png(filename="plot4.png",height=960,width=720)

#Plot 1: Total level
p1<-ggplot(data=summ_emissions_scc,aes(x=year,y=Sum,fill=SCC.Level.One))+geom_bar(stat="identity") +
  ggtitle("PM2.5 Emissions from Coal Source, US, 1999-2008") +
  scale_x_continuous(name="Year Collected",
                     breaks=c(1999,2002,2005,2008), 
                     labels=c("1999", "2002", "2005","2008")) +
  scale_y_continuous(name="Total PM2.5 Emissions for Coal Sources")

#plot 2: Proportional to total Coal of that year
p2<-ggplot(data=summ_emissions_fin,aes(x=year,y=percent_Sum,fill=SCC.Level.One,label=Sum))+geom_bar(stat="identity") +
  ggtitle("PM2.5 Emissions from Coal By Primary Source, US, 1999-2008") +
  scale_x_continuous(name="Year Collected",
                     breaks=c(1999,2002,2005,2008), 
                     labels=c("1999", "2002", "2005","2008")) +
  scale_y_continuous(name="Percent of Total PM2.5 Emissions for Coal Sources")

#Present both (using grid.arrange from gridExtra)
grid.arrange(p1,p2)
dev.off()