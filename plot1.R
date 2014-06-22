source("readin.R") #This reads in NEI and SCC

#Summarize the data for easier verification of line graph (and faster running of the graph!)
summ_emissions <- ddply(NEI,.(year),summarise,Sum=sum(Emissions))  

#Graph it in a png
png(filename="plot1.png",height=480,width=480)
#Line graph
with(summ_emissions,plot(y=Sum,x=year,
                        type='l',
                        xlab="Year Collected",ylab="Total PM2.5 Emissions",
                        xaxt='n'))
#Title
title("Total PM2.5 Emissions Per Year")

#Make the x axis work better, as it is not acting like a discrete axis
axis(1,c('1999','2002','2005','2008'))

dev.off()