#nursing data 2018
#data collected by Rosina Plomp
#For nursing, start and end times of feedings were registered
#For formula feeding, start and end times are NOT accurate, 
# and often the default time (15min) was registered. 
#However, the nr of ml was registered accurately. 


library(tidyr)
library(data.table)
library(ggplot2)
library(pracma)

#########################
## Data pre-processing ##
#########################

# import data
setwd("E:/Bijscholen 2017/Nursing project/raw")
data <- read.csv("Nursing final dataset.csv", sep=";",row.names=NULL, header=FALSE)
data <- as.data.table(data)

#rename columns based on investigation
names(data)<- c("datatype","start_date_time","end_date_time","left0_right1_kv", "V5","ml","V7")

# select relevant data
data <- data[datatype=="5" | datatype=="15"] #leave out other types of data (measurements, pumping, etc.)
data <- data[,-c(1,5,7)] #leave out empty columns which pertain to other datatypes
    
# format dates: split the date and time into separate columns
data_extra1 <- separate(data, "start_date_time", into=c("start_date","start_time"), sep=" ")
data_extra2 <- separate(data, "end_date_time", into=c("end_date","end_time"), sep=" ")
data <- cbind(data,data_extra1[,c(1:2)],data_extra2[,c(2:3)])
  
# take only data for Tobias, since that of Leander contains too much missing data
data[,start_date := as.Date(start_date,format="%d.%m.%Y")]
data <- data[start_date > as.Date("08.02.2018",format="%d.%m.%Y")]

# make new feeding column: 0/1=breast, else=bottle
data[,feedingtype := ifelse(left0_right1_kv=="0","breast",
                            ifelse(left0_right1_kv=="1","breast","bottle"))]

# make new feeding column: 0=left, 1=right, else=formula
data[,feedingtype2 := ifelse(left0_right1_kv=="0","left",
                            ifelse(left0_right1_kv=="1","right","bottle"))]

# add column for feeding duration
data<-as.data.frame(data) #strptime runs into an issue with data.tables
data$start_date_time <- strptime(data$start_date_time,"%d.%m.%Y %H:%M:%S")
data$end_date_time <- strptime(data$end_date_time,"%d.%m.%Y %H:%M:%S")
data$duration<-data$end_date_time-data$start_date_time
data$duration<-as.numeric(data$duration)
data$duration<-(data$duration)/(60*60) #turn sec into hours
  
# add duration for bottle feeding: assume 10 ml/min
data$ml <- ifelse(data$ml=="__EMPTY__",NA,
                    ifelse(data$ml=="null",0,(as.numeric(as.character(data$ml)))))
data$duration <- ifelse(data$feedingtype=="bottle",(((data$ml)/10)/60),data$duration) #in hours

# add column with interval between feedings
data$interval<-1
for (i in 2:length(data$start_date)){
  Int <- (data$start_date_time[i] - data$end_date_time[i-1])
  data$interval[i] <- as.numeric(Int, units="hours")
}

# make column with age of Tobias in days
data$infant_age<- (strptime(data$end_date,"%d.%m.%Y")-
                             strptime("09.02.2018","%d.%m.%Y"))
data$infant_age<- as.numeric(data$infant_age, units="days") #in days
data$infant_age<- round(data$infant_age,digits=0) #in integer days

############################# graph total feeding time ############################
###################################################################################
  
#calculate and plot the total duration of feeding per day, 
#calculate duration feeding per day
data$start_date_time <- NULL
data$end_date_time <- NULL
data <- as.data.table(data)
TotSumByDay_feedingtype <- data[,(Total_duration=sum(duration)),by=c("infant_age","feedingtype")]

#scatterplot: total nursing per day for Tobias
ggplot(data=TotSumByDay_feedingtype,
     aes(x=infant_age, y=V1,color=feedingtype)) +
  geom_point() +
  ggtitle("Total time per day spent feeding") +
  ylab("Time per Day (hours)") +
  xlab("Age of infant (days)") +
  scale_colour_manual(values=c("dodgerblue1", "orangered1"),
                    name="Feeding type") +
  theme_light()

###############bar graph left/right/bottle per month ##############################
################################################################################### 

#make a column for month
data[,Month:=month.abb[month(start_date)]]
SumByMonth_feedingtype2 <- data[,(TimePerDay=sum(duration)),by=c("Month","feedingtype2")]   
SumByMonth_feedingtype2[,Month:=factor(Month,levels=c("Feb","Mar","Apr","May","Jun","Jul","Aug"))]

#scatterplot: total nursing per day for Tobias
ggplot(data=SumByMonth_feedingtype2,
       aes(x=Month, y=V1,fill=feedingtype2)) +
  geom_bar(stat="identity") +
  ggtitle("Hours per month spent feeding") +
  ylab("Time per Month (hours)") +
  xlab("Month") +
  #scale_x_discrete(labels=c(Feb","Mar","Apr","May","Jun","Jul","Aug")) +
  scale_fill_manual(values=c("dodgerblue1", "darkgoldenrod2","chocolate2"),
                    labels=c("bottle","left breast","right breast"),
                      name="Feeding type") +
  theme_light()

############### nr of feedings per day ##############################
###################################################################################

#remove feedings less than 1min
data2<-data
data2<-data[duration>(1/60)]

#remove intervals less than 0.5h (30min)
data2[,interval:=ifelse(interval>0.5,interval,0)]

#make separate column which just says '1' for each interval over 0.5h
data2[,interval01:=ifelse(interval>0,1,0)]

#aggregate to get nr of feedings per day
NrFeedsPerDay <- data2[,sum(interval01),by="infant_age"]

#scatterplot: total nursing per day for Tobias
ggplot(data=NrFeedsPerDay,
       aes(x=infant_age, y=V1)) +
  #geom_jitter(size=4,alpha=0.3,width=NULL, height=NULL) +
  geom_point(size=4,alpha=0.3) +
  ggtitle("Total number of feedings per day") +
  ylab("Nr of feedings") +
  xlab("Age of infant (days)") +
  theme_light()

############### Longest interval ##############################
###################################################################################
    
#aggregate to get longest interval per day
LongestInterval <- data[,.(Max.int=max(interval),var=first(feedingtype)),by="infant_age"]

#scatterplot: total nursing per day for Tobias
ggplot(data=LongestInterval,
  aes(x=infant_age, y=Max.int)) +
  geom_point() +
  ggtitle("Longest time in between feedings (maximum uninterrupted sleep time)") +
  ylab("Time per Day (hours)") +
  xlab("Age of infant (days)") +
  scale_colour_manual(values=c("dodgerblue1", "orangered1"),
                      name="Feeding type") +
  theme_light()







    
    
  