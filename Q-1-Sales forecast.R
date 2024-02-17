library(stringr)
library(forecast)
#library(tidytext)


#import the data set

sales_dt = read.csv("sales.csv")

# to identify the strutcure of data set
str(sales_dt)

#converting the month variable into required format and then created the date column which will be used as timelable in the data set

sales_dt$Month <- str_c(sales_dt$Month,"-01") 
sales_dt$date <- as.Date(sales_dt$Month,format="%Y-%m-%d")

#creating the time series object from the data set

sales_ts = ts(sales_dt$SalesAmount,start = c(1964,01),end = c(1972,09),frequency = 12)


#plotting the sales data to identify any trends/seasonality
#by looking at the plots it is cleary identifiable that there is seasonalbe componenet

plot(sales_ts)


#splitting the data into train data and test data to creating the models and testing the models
#we are splitting data with last 15 months data into test data and remaning as traninng data

sales_train_ts =  window(sales_ts, start=1964, end=c(1971,04))
sales_test_ts = window(sales_ts, start=c(1971,05), end=c(1972,09))

autoplot(sales_train_ts) + autolayer(sales_test_ts)


##model building



##from the plot we can clearly it is not stationary and trend presence in data
#to check statinality we can run dickey muller test or we can directly find the how difference needed to make stationary using below method

#to check how many differences required to make data stationary

ndiffs(sales_ts) ##for difference lag

nsdiffs(sales_ts) ## for sesaonal difference lag

acf(sales_ts)
pacf(sales_ts)

sales_ts %>%
  diff(lag = 4) %>%
  ggtsdisplay()



sales_ts %>%
  diff(lag = 12) %>%
  diff() %>%
  ggtsdisplay()



##here we have found there is significant spike in acf graphs at 12 lag it means it has yearly seasonal trend and then there is difference of lag in moving avergae trend


sales_train_ts %>%
  arima(order = c(0,1,1),
        seasonal = c(0,1,1)) %>%
  residuals() %>%
  ggtsdisplay()



fit_mdl_1 = sales_train_ts %>%
  arima(order = c(1,1,1),
        seasonal = c(1,1,1))

summary(fit_mdl_1)


fit_mdl_2 = sales_train_ts %>%
  arima(order = c(0,1,2),
        seasonal = c(0,1,2))

summary(fit_mdl_2)

checkresiduals(fit_mdl_1)


fit_mdl_3 = sales_train_ts %>%
  arima(order = c(1,1,0),
        seasonal = c(1,1,0))


summary(fit_mdl_3)

checkresiduals(fit_mdl_3)


#we can build different models. and auto arima is the one modelling which will creates models and present the best model among all models

aa_mdl = auto.arima(sales_train_ts)
summary(aa_mdl)




##to check residual summary and and acf information of fitted models


checkresiduals(fit_mdl_1)
checkresiduals(fit_mdl_2)
checkresiduals(fit_mdl_3)
checkresiduals(aa_mdl)

##forecast the next 17 months sales data to using the above three models

fct_dt_mdl_1 = forecast(fit_mdl_1,h = 17)
fct_dt_mdl_2 = forecast(fit_mdl_2,h = 17)
fct_dt_mdl_3 = forecast(fit_mdl_3,h = 17)


# as we have acutal test data we are comparing the accuracy with test data and forecast data 

mdl_1_acc = accuracy(fct_dt_mdl_1, x = sales_test_ts)
mdl_2_acc = accuracy(fct_dt_mdl_2, x = sales_test_ts)
mdl_3_acc = accuracy(fct_dt_mdl_3, x = sales_test_ts)



summary(md_1)

checkresiduals(md_1)


#to forecast the next 17 months of sales after the training data

fct_ts = forecast(md_1,h=17)

fct_ts %>%
  autoplot()

# as we have acutal test data we are comparing the accuracy with test data and forecast data 

accuracy(fct_ts, x = sales_test_ts)


##plotting the curve of acutal data with forecast data

fct_ts %>%
  autoplot() +
  geom_line(
    aes(
      x = as.numeric(time(sales_test_ts)),
      y = as.numeric(sales_test_ts)
    ),
    col = "red"
  )
