source("readin.R") #This reads in NEI and SCC
require(ggplot2)
#Summarize the data for easier verification of line graph (and faster running of the graph!)
summ_emissions <- ddply(NEI[NEI$fips=="24510",],.(year,type),summarise,Sum=sum(Emissions))  

#Plot the chart
#Graph it in a png
png(filename="plot3.png",height=480,width=480)

ggplot(data=summ_emissions,aes(x=year,y=Sum,group=type,colour=type))+geom_line() +
  scale_x_continuous(name="Year Collected",breaks=c(1999,2002,2005,2008), labels=c("1999", "2002", "2005","2008")) +
  scale_y_continuous(name="Total PM2.5 Emissions")

dev.off()