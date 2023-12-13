#1/ required packages
library(tidyverse)
library(readxl)
library(ggplot2)
library(lmtest)
library(forecast)
library(xts)
library(tseries)

#2/ create dataset
#load data
data <- read_excel("D://học tập//NĂM 3//hk 6//gói ứng dụng trong tài chính//cuối kỳ//K204141927.xlsx", sheet = 1)

#all variable names
colnames(data)
#checking Na values
sum(is.na(data))

#create variables
data <- data %>% 
  mutate(DMS= round((long_term_debt/(long_term_debt + short_term_debt)),4),
         liquidity = round((total_current_assets/short_term_debt),4),
         profitability = round((EBT + interest_expense)/total_assets,4), # + interest_expense
         leverage = round(((long_term_debt+short_term_debt)/total_assets),4),
         asset_intensity = round((fixed_assets/total_assets),4)
  )
df <- data %>% 
  select(Time, DMS, liquidity, profitability, leverage, asset_intensity)
head(df)
#checking Na values
sum(is.na(df))

#3/descriptive statistics
#entire period
entire_period <- df %>% 
  summarise(variables = c('DMS','liquidity', 'profitability','leverage', 'asset_intensity'),
            obs = nrow(df),
            min = c(min(DMS), min(liquidity), min(profitability), min(leverage), min(asset_intensity)),
            mean = c(mean(DMS), mean(liquidity), mean(profitability), mean(leverage), mean(asset_intensity)),
            median = c(median(DMS), median(liquidity), median(profitability), median(leverage), median(asset_intensity)),
            std = c(sd(DMS), sd(liquidity), sd(profitability), sd(leverage), sd(asset_intensity)),
            max = c(max(DMS), max(liquidity), max(profitability), max(leverage), max(asset_intensity))
)
entire_period = data.frame(entire_period)
entire_period

#before Covid-19 pandemic
before_covid <- df[1:40, ]
before_covid_period <- before_covid %>% 
  summarise(variables = c('DMS','liquidity', 'profitability','leverage', 'asset_intensity'),
            obs = nrow(before_covid),
            min = c(min(DMS), min(liquidity), min(profitability), min(leverage), min(asset_intensity)),
            mean = c(mean(DMS), mean(liquidity), mean(profitability), mean(leverage), mean(asset_intensity)),
            median = c(median(DMS), median(liquidity), median(profitability), median(leverage), median(asset_intensity)),
            std = c(sd(DMS), sd(liquidity), sd(profitability), sd(leverage), sd(asset_intensity)),
            max = c(max(DMS), max(liquidity), max(profitability), max(leverage), max(asset_intensity))
  )
before_covid_period = data.frame(before_covid_period)
before_covid_period

#during Covid-19 pandemic
during_covid <- df[41:48, ]
during_covid_period <- during_covid %>% 
  summarise(variables = c('DMS','liquidity', 'profitability','leverage', 'asset_intensity'),
            obs = nrow(during_covid),
            min = c(min(DMS), min(liquidity), min(profitability), min(leverage), min(asset_intensity)),
            mean = c(mean(DMS), mean(liquidity), mean(profitability), mean(leverage), mean(asset_intensity)),
            median = c(median(DMS), median(liquidity), median(profitability), median(leverage), median(asset_intensity)),
            std = c(sd(DMS), sd(liquidity), sd(profitability), sd(leverage), sd(asset_intensity)),
            max = c(max(DMS), max(liquidity), max(profitability), max(leverage), max(asset_intensity))
  )
during_covid_period = data.frame(during_covid_period)
during_covid_period

#4/box & whisker plot and histogram
#box plot
boxplot(df$DMS,
        main = "Box plot of DIG's debt maturity structure for the entire period",
        col = "lightblue",
        xlab= "Debt maturity structure",
        horizontal = TRUE)

#before covid
boxplot(before_covid$DMS,
        main = "Box plot of DIG's debt maturity structure before Covid19",
        col = "lightblue",
        xlab= "Debt maturity structure",
        horizontal = TRUE)

#during covid
boxplot(during_covid$DMS,
        main = "Box plot of DIG's debt maturity structure during Covid19",
        col = "lightblue",
        xlab= "Debt maturity structure",
        horizontal = TRUE)

#histogram
ggplot(df, aes(x = DMS)) +
  geom_histogram(binwidth = 0.08, fill = "lightblue", color = "black") +
  labs(title = "Histogram of DIG's debt maturity structure for the entire period", 
       x = "Debt maturity structure", 
       y = "Frequency")

#before covid
ggplot(before_covid, aes(x = DMS)) +
  geom_histogram(binwidth = 0.07, fill = "lightblue", color = "black") +
  labs(title = "Histogram of DIG's debt maturity structure before Covid19", 
       x = "Debt maturity structure", 
       y = "Frequency")

#during covid
ggplot(during_covid, aes(x = DMS)) +
  geom_histogram(binwidth = 0.12, fill = "lightblue", color = "black") +
  labs(title = "Histogram of DIG's debt maturity structure during Covid19", 
       x = "Debt maturity structure", 
       y = "Frequency")

#5/ multiple regression
#5.1 regression with all the individual variables
#Check linearity & multicollinearity
par(mfrow=c(1,4))
plot(liquidity ~ DMS, data = df)
plot(profitability ~ DMS, data = df)
plot(leverage ~ DMS, data = df)
plot(asset_intensity ~ DMS, data = df)

#Multiple regression
dms.lm<-lm(DMS ~ liquidity + profitability + leverage + asset_intensity, data = df)
summary(dms.lm)

library(stargazer)
stargazer(dms.lm, type = "text") 

#Check multicollinearity
car::vif(dms.lm) #VIF >10 thì đa cộng tuyến

#check important assumptions for linear regression
par(mfrow=c(2,2))   
plot(dms.lm)

#check normality
shapiro.test(resid(dms.lm))# Null hypothesis is normality

# Check homoscedasticity
bptest(dms.lm) #Null hypothesis is homoscedasticity #p.value >5%, lúc đó phương sai đồng nhất, phương sai không đổi

#5.2 regression with the usual individual variables 
#and the interaction between Covid-19 dummy variable and the independent variables
#convert data
data <- data %>% 
  mutate(covid = ifelse(covid == 'covid_period',1,0)
)  
data$covid = factor(data$covid)
df2 <- data %>% 
  select(Time, DMS, liquidity, profitability, leverage, asset_intensity, covid)

dms2.lm<-lm(DMS ~ liquidity + leverage + asset_intensity + liquidity*covid + leverage*covid + asset_intensity*covid, data = df2) #liquidity*covid +  + leverage*covid
summary(dms2.lm)
stargazer(dms2.lm, type = "text") 

#Check multicollinearity
car::vif(dms2.lm)

#check important assumptions for linear regression
par(mfrow=c(2,2))   
plot(dms2.lm)

# Check normality
shapiro.test(resid(dms2.lm))# Null hypothesis is normality

# Check homoscedasticity
bptest(dms2.lm) #Null hypothesis is homoscedasticity

#5.3 Prediction
#use model 1 to predict the response value for all the quarters of the sample
pred_model = predict(dms.lm, data = df[,3:6])

pred_table =data.frame(Time = df$Time,
                       Actual_DMS = df$DMS,
                       Predicted_DMS = round(pred_model,4))
pred_table$Difference = abs(pred_table$Actual_DMS - pred_table$Predicted_DMS)
pred_table
summary(pred_table$Difference)

#6/ ARIMA model
class(df$Time)
date.time = seq(as.Date("2010/03/31"), by = "quarter", length.out = nrow(df)) 
date.time

# Create a xts Dataframe
dms = xts(df[,2],date.time)
class(dms)

#plot the data - look at the debt maturity structure of DIG
par(mfrow=c(1,1))
plot(dms)

#stationary check
par(mfrow=c(1,2))
print(adf.test(dms))

dmsdiff1 <- diff(dms, differences = 1)
par(mfrow=c(1,1))
plot(dmsdiff1) 
dmsdiff1 = na.omit(dmsdiff1)
print(adf.test(dmsdiff1))

dmsdiff2 <- diff(dms, differences = 2)
par(mfrow=c(1,1))
plot(dmsdiff2) 
dmsdiff2 = na.omit(dmsdiff2)
print(adf.test(dmsdiff2))

#graph the ACF and PACF looking for identifiable lags PACF -> p ACF -> q 
par(mfrow=c(1,2))
acf(dmsdiff2, main = 'ACF for differenced Series', lag.max = 40)
pacf(dmsdiff2, main = 'PACF for differenced Series', lag.max = 40)
acf(dmsdiff2, lag.max = 40, plot = FALSE)
pacf(dmsdiff2, lag.max = 40, plot=FALSE)
# p = 1,2,3 ; q = 1,2,18

#building ARIMA model
a = auto.arima(dms,seasonal=F) #ARIMA(0,1,0)
a
b = arima(dms, order = c(1,2,1)) #sigma^2 estimated as 0.007535:  log likelihood = 45.2,  aic = -84.4
b
arima(dms, order = c(1,2,2))#sigma^2 estimated as 0.007544:  log likelihood = 45.2,  aic = -82.4
arima(dms, order = c(1,2,18)) #sigma^2 estimated as 0.003476:  log likelihood = 54.93,  aic = -69.85
c = arima(dms, order = c(2,2,1)) #sigma^2 estimated as 0.007131:  log likelihood = 46.2,  aic = -84.4
c
arima(dms, order = c(2,2,2)) #sigma^2 estimated as 0.007129:  log likelihood = 46.21,  aic = -82.41
arima(dms, order = c(2,2,18)) #sigma^2 estimated as 0.002872:  log likelihood = 56.69,  aic = -71.38
arima(dms, order = c(3,2,1)) #sigma^2 estimated as 0.007434:  log likelihood = 46.43,  aic = -82.86
d = arima(dms, order = c(3,2,2)) #sigma^2 estimated as 0.006222:  log likelihood = 48.29,  aic = -84.59
d
tsdisplay(residuals(d), lag.max = 40 ,main = '(3,2,2) Model Residuals')
arima(dms, order = c(3,2,18)) #sigma^2 estimated as 0.00287:  log likelihood = 56.71,  aic = -69.41

#perform the portmanteau test
checkresiduals(d, lag = 40)
summary(d$residuals)

#ARMA roots table
plot(d)

#predict the DMS for the 4 quarters in 2022
dmsforecast = forecast(d,h = 4, level = c(95))
dmsforecast
par(mfrow=c(1,1))
plot(dmsforecast)

#comparison of actual DMS and forecasted DMS
actual_dms2022 <- read_excel("D://học tập//NĂM 3//hk 6//gói ứng dụng trong tài chính//cuối kỳ//K204141927.xlsx", 
                             sheet = 2)

actual_dms <- actual_dms2022 %>% select(Time, DMS)

dmsforecast_ = data.frame(forecast(d,h = 4, level = c(95)))
forecast_table = data.frame(Time = actual_dms$Time,
                            Actual_DMS = round((actual_dms$DMS),4),
                            Predicted_DMS = dmsforecast_$Point.Forecast)
forecast_table$Difference = abs(forecast_table$Actual_DMS - forecast_table$Predicted_DMS)
forecast_table