#libraries required ----

library(dplyr)
library(tidyr)
library(ggplot2)


# reading uber requestdata----
uber <- read.csv("Uber Request Data (1).csv",header = T,stringsAsFactors = F)

#Checking for data quality issues----
str(uber)

#checking if any columns have NA values
sum(is.na(uber$Request.id))
sum(is.na(uber$Pickup.point))
sum(is.na(uber$Driver.id))
sum(is.na(uber$Status))
sum(is.na(uber$request_time))
sum(is.na(uber$Drop_time))
#Since no rows have NA other than Driver id and drop time,its safe to continue without cleaning NA
#Checking for blanks
sum(uber$Request.id == "")
sum(uber$Pickup.point == "")
sum(uber$Driver.id == "",na.rm = T)
sum(uber$Status == "")
sum(uber$request_time == "")
sum(uber$Drop_time == "",na.rm = T)
#No blanks hence no change required
#Checking if any spelling mistakes are in Pickup point
uber1 <- group_by(uber$Pickup.point)
summarise(uber1)
#Checking if spelling mistakes are in Status
uber1 <- group_by(uber,Status)
summarise(uber1)


#Converting Request,Drop date and time to suitable time format----
#Seperating date and time
uber <- separate(data = uber, col = Request.timestamp, into = c("Req_date", "Req_time"), sep = " ")
uber <- separate(data = uber, col = Drop.timestamp, into = c("Drop_date", "Drop_time"), sep = " ")

#converting tosame format throughout the date
uber$Req_date <- gsub('-','/',uber$Req_date)
uber$Drop_date <- gsub('-','/',uber$Drop_date)
uber$Req_date <- as.Date(uber$Req_date,"%d/%m/%Y")
uber$Drop_date <- as.Date(uber$Drop_date,"%d/%m/%Y")


uber <-  separate(data = uber, col = Req_time, into = c("Req_hour", "Req_min","Req_sec"), sep = ":")
uber <-  separate(data = uber, col = Drop_time, into = c("Drop_hour", "Drop_min","Drop_sec"), sep = ":")


#Since there are no NA or blanks and no discreptancies uppending 0's to get the required DD/MM/YY HH:MM:SS format
uber$Req_hour <- uppzero(uber$Req_hour)
uber$Req_min <- uppzero(uber$Req_min)
uber$Req_sec <- uppzero(uber$Req_sec)

uber$request_time <- paste(uber$Req_date,uber$Req_hour,sep = " ")
uber$request_time <- paste(uber$request_time,uber$Req_min,uber$Req_sec,sep = ":")
uber$request_time <- as.POSIXlt(uber$request_time,format = "%Y-%m-%d %H:%M:%S")
#deleting created columns as date hour min etc since date time has been integrated
uber <- uber[,-5:-8]


#Since NA or blanks in drop time should not be uppended with 0s whenever Trip is incomplete keeping a for loop----
j <- 1
for(i in uber[])
{
  if(uber$Status[j] == "Trip Completed")
  {
    uber$Drop_hour  <- uppzero(uber$Drop_hour)
    uber$Drop_min <- uppzero(uber$Drop_min)
    uber$Drop_sec <- uppzero(uber$Drop_sec)
  }
  j <- j+1
}
uber$Drop_time <- paste(uber$Drop_date,uber$Drop_hour,sep = " ")
uber$Drop_time <- paste(uber$Drop_time,uber$Drop_min,uber$Drop_sec,sep = ":")
uber$Drop_time <- as.POSIXlt(uber$Drop_time,format = "%Y-%m-%d %H:%M:%S")
#Deleting columns created for time correction 
uber <- uber[,-5:-8]


#EDA ----
#Average Trip time 
#From Airport to the City
Airport <- subset(uber,uber$Pickup.point == "Airport")
mean(Airport$Drop_time - Airport$request_time,na.rm =T)
Airport$drive_time <- as.numeric(Airport$Drop_time - Airport$request_time)
#spread of drive time
boxplot(Airport$drive_time,na.rm=T)

#From City to the Airport
City <- subset(uber,uber$Pickup.point == "City")
mean(City$Drop_time - City$request_time,na.rm =T)
City$drive_time <- as.numeric(City$Drop_time - City$request_time)
boxplot(City$drive_time,na.rm=T)
#52 mins Thats almost an hour for both from and to city, to note is considerable time is spent on one drive


#to check busy hours----
#Dodged view
Req_plot_dod <- ggplot(uber,aes(x=format(uber$request_time,"%H"),fill = uber$Pickup.point))+geom_bar(position = "dodge")
Req_plot_dod <- Req_plot_dod+labs(title = "Request at DIfferent time at CITY/AIRPORT",x= "Time (Hrs)",y= "Count",fill="Place")
Req_plot_dod
#Stacked view of same plot
Req_plot <- ggplot(uber,aes(x=format(uber$request_time,"%H"),fill = uber$Pickup.point))+geom_bar(position = "stack")
Req_plot <- Req_plot+labs(title = "Request at DIfferent time at CITY/AIRPORT",x= "Time (Hrs)",y= "Count",fill="Place")
Req_plot
#morning 5-9 in the city and evening 5-10 at the airport


#to check for different time slots----
#5 -10 morning
#10-17 Mid day
#17- 22 evening
#22 - 5 night
#Segmented by clustering these on basis of demand 
uber$Time_Slot <- ifelse(format(uber$request_time,"%H")>5|format(uber$request_time,"%H")<10,"0 Morning",
                         ifelse(format(uber$request_time,"%H")>=10&format(uber$request_time,"%H")<17,"1 Mid Day",
                                ifelse(format(uber$request_time,"%H")>=17&format(uber$request_time,"%H")<=22,"2 Evening",
                                       ifelse(format(uber$request_time,"%H")>22|format(uber$request_time,"%H")<=5,"3 Night",
                                              ))))

plot1 <-ggplot(subset(uber,uber$Pickup.point == "Airport"),aes(x=Time_Slot,fill=factor(Status)))+geom_bar(position = "dodge")
plot1 <- plot1 + labs(title = "Airport",x= "Time Slot",y= "Count",fill="Status")
plot2 <-ggplot(subset(uber,uber$Pickup.point == "City"),aes(x=Time_Slot,fill=factor(Status)))+geom_bar(position = "dodge")
plot2 <- plot2 + labs(title = "City",x= "Time Slot",y= "Count",fill="Status")
grid.arrange(plot1,plot2,ncol = 2)

#Based on this graph 3 main problems faced by uber are
#1. No cars available in evening at the airport
#2. Cancellation at the City in the morning
#3. No cars available at the city in the morning



#Graph plotting supply on  Demand----
demvssup <-  ggplot() +geom_histogram(data = uber,aes(x=as.integer(format(uber$request_time,"%H")),colour = "Demand"),fill = "light yellow",binwidth = 2)+geom_freqpoly(data =subset(uber,uber$Status == "Trip Completed"),aes(x=as.integer(format(request_time,"%H")),color = "Supply"),fill = "Supply",binwidth=2)
demvssup <- demvssup+labs(title = "Supply on Demand" ,x= "Time (Hrs)",y= "Count",colour="Label")
demvssup 
