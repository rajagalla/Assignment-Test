##
library(tidyverse)
library(dplyr)




#importing the hotel data

hotel_dt = read.csv("hotel.csv")


#subset the data with 24 columns as per suggestion

hotel_dt = hotel_dt[,c(1:31)]


##creating the index as each record in the hotel data as one booking id


colnames(hotel_dt)

summary(hotel_dt)


str(hotel_dt)



hotel_dt$hotel = as.factor(hotel_dt$hotel)
hotel_dt$is_canceled = as.factor(hotel_dt$is_canceled)
hotel_dt$meal = as.factor(hotel_dt$meal)
hotel_dt$country = as.factor(hotel_dt$country)



##we can see that some of the variables in the data are not in valid format, we need to make sure the variables are in required format


##and looking at column names and data we understand there are fields like is_canceled variable which is whether booking is cancelled or not
# and here we are assuming that canceled booking are not useful in our analysis


hotel_dt = filter(hotel_dt,hotel_dt$is_canceled == 0)

##and we could observe the other variables like children and babies
#it means whether the in hotel booking has children included that means we could understand that families are occupying the hotel booking


#here we will merget the children and babies column values to children_total to count them

hotel_dt$total_children = hotel_dt$children+hotel_dt$babies

##we will create the flag of hotel bookings with children flag

hotel_dt$childrenstatus = ifelse(hotel_dt$total_children >0,"Yes","No")




###below we are trying to find the trend of bookings where booking with children or bookings without children are popular and in which months using histogram plot in resort hotel type or city hotel type


hotel_dt %>% 
  mutate(arrival_date_month = factor(arrival_date_month,levels = month.name)) %>%
  count(hotel,arrival_date_month,childrenstatus) %>%
  group_by(hotel,childrenstatus) %>%
  mutate(proportion= n/sum(n)) %>%
  ggplot(aes(arrival_date_month,proportion,fill=childrenstatus)) +
  geom_col(position = "dodge") +
  scale_y_continuous(labels = scales::percent_format()) +
  facet_wrap(~hotel,nrow = 2)


##from the above histogram results we could see that families with children are booking more august and september months and remaining months booking with out children are dominating in bookings


##and to plot mulitple varibles we could use ggally package

install.packages("GGally")

library(GGally)


hotel_dt %>%
  select(childrenstatus,adr,required_car_parking_spaces) %>%
  ggpairs(mapping = aes(color = childrenstatus))



####################Like the above we can identify the relation between variables by plotting on potential variables and can derive insights



###############################################################


###to forecast the hotel bookings data assuming the today date is 
#aug 31, 2017

##so lets get the data of required columns and create a time series data from hotel data

hotel_dt2 = hotel_dt[,c("arrival_date_year","arrival_date_month","arrival_date_day_of_month")]


hotel_dt3 = hotel_dt2 %>%
  group_by(arrival_date_year,arrival_date_month,arrival_date_day_of_month) %>% summarise(bookingscount = n())



hotel_dt3$arrival_date_month = month.name(hotel_dt3$arrival_date_month)




hotel_dt3$date <- str_c(hotel_dt3$arrival_date_year,"-",hotel_dt3$arrival_date_month,"-",hotel_dt3$arrival_date_day_of_month)

hotel_dt3$date <- as.Date(hotel_dt3$date,format="%Y-%b-%d")

hotel_dt3 = hotel_dt3[order(hotel_dt3$date,decreasing = FALSE),]




booking_ts = ts(hotel_dt3$bookingscount,start = c(2015,1),frequency = 365)

plot(booking_ts)



###################building arima models
##we are auto arima function create the best model for the given data


mdl_booking = auto.arima(booking_ts)

summary(mdl_booking)

checkresiduals(mdl_booking)


############forecast the next seven days booking using model

fct_bk_ts = forecast(mdl_booking, h = 7)


fct_bk_ts %>%
  autoplot()

fct_bk_ts




