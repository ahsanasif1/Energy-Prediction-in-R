library(tidyverse)

gov_data <- read.csv('govhack3.csv')

#TASK 2.1 Cleaning up columns

ed_data_link <- 'govhack3.csv'
top_row <- read_csv(ed_data_link, col_names = FALSE, n_max = 1)
second_row <- read_csv(ed_data_link, n_max = 1)

column_names <- second_row %>% 
  unlist(., use.names=FALSE) %>% 
  make.unique(., sep = "__") # double underscore

column_names[2:8] <- str_c(column_names[2:8], '0', sep='__')

daily_attendance <- 
  read_csv(ed_data_link, skip = 2, col_names = column_names)

library(ggplot2)
library(lubridate)


New_Data <- daily_attendance %>% gather(key = "NewVariables", value = "NewValues", -Date )
New_Data <- New_Data %>% separate(NewVariables,into = c('Variables','Hospitals'),sep = "__")
New_Data <- New_Data %>% spread(key = 'Variables', value = 'NewValues' )


#Changing their data types from char to Date and numeric
New_Data$Date <- dmy(New_Data$Date)
New_Data$Admissions <- as.numeric(New_Data$Admissions)
New_Data$Attendance <- as.numeric(New_Data$Attendance)
New_Data$Tri_1 <- as.numeric(New_Data$Tri_1)
New_Data$Tri_2 <- as.numeric(New_Data$Tri_2)
New_Data$Tri_3 <- as.numeric(New_Data$Tri_3)
New_Data$Tri_4 <- as.numeric(New_Data$Tri_4)
New_Data$Tri_5 <- as.numeric(New_Data$Tri_5)

#Task 1

#Answer 2,3,4

library(rnoaa)
options(noaakey = "lAbaesautwWcTyWnvEcBPYbiJYXsqrCI")

perth1 <- ncdc(
      datasetid = 'GHCND',
       stationid = 'GHCND:ASN00009021',
       startdate = '2013-07-01',
       enddate = '2013-12-31',
       limit = 1000
   )$data
NROW(perth1)

perth2 <- ncdc(
       datasetid = 'GHCND',
       stationid = 'GHCND:ASN00009021',
       startdate = '2014-01-01',
       enddate = '2014-06-30',
       limit = 1000
   )$data
NROW(perth2)

final_perth <- rbind(perth1,perth2)

#Task 3

#1

#Picking the Royal Perth Hospital and tidying it up

RoyalPerthHospital <- New_Data %>% filter(Hospitals == '0')
#Replace na with 0 
RoyalPerthHospital[is.na(RoyalPerthHospital)] <- 0

#2

#linear regression on ED data taking Date as the predictor
ed_data <- lm(Attendance ~ Date, data = RoyalPerthHospital)
summary(ed_data) 
glance(ed_data)

#Augmenting ED demand data
ed_results <- augment(ed_data)

ed_results %>% ggplot(aes(sample = .std.resid)) + geom_qq() + geom_qq_line(col = 'red')
ed_results %>% ggplot(aes(x = .fitted, y = .resid )) + geom_point() + geom_quantile() + geom_smooth(colour = 'red')
ed_results %>% ggplot(aes(x = Date, y = .resid)) + geom_point() +   geom_quantile() + geom_smooth(colour = 'red') 
ed_results %>% ggplot(aes(x = .hat, y = .resid)) +geom_point() + geom_hline(yintercept = 0,linetype = 'dotted')
ed_results %>%  mutate(big_hat = .hat > 0.03) %>% ggplot(aes(x = .hat, y = .cooksd)) +geom_point(aes(color = big_hat)) + geom_hline(yintercept = 0,linetype = 'dotted')

#Converting the type of Date for GAM
RoyalPerthHospital <- transform(RoyalPerthHospital, numeric_date = as.numeric(Date))

#3

#Fitting gam and it's analysis

gamfit <- gam( Attendance ~ s(numeric_date),
               family=Gamma(link=log),
               data=RoyalPerthHospital)


plot(gamfit)
plot(gamfit, residuals = TRUE)
par(mfrow=c(2,2))
gam.check(gamfit)

summary(gamfit)
glance(gamfit)

#Augmenting the gam result
gam_result <- gamfit %>% augment
gam_result %>% ggplot(aes(x = numeric_date, y = .resid)) +geom_point() + geom_quantile() +geom_smooth(colour = 'red') 

#4

#Adding the weekdays in the hospital data

RoyalPerthHospital$Day <- weekdays(as.Date(RoyalPerthHospital$Date))
RoyalPerthHospital$Day=factor(RoyalPerthHospital$Day,levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))


#Fitting GAM for the weekly data

weekly_seasonality <- gam(Attendance ~ s(numeric_date, by = Day) + Day, data = RoyalPerthHospital)

#Augmenting the weekly data
gam_weeklyresult <- weekly_seasonality %>% augment
gam_weeklyresult %>% ggplot(aes(x = numeric_date, y = .resid)) +geom_point() + geom_quantile() +geom_smooth(colour = 'red')


#Plotting the weekly seasonality
plot(weekly_seasonality)
plot(weekly_seasonality, residuals = TRUE)

summary(weekly_seasonality)
glance(weekly_seasonality)


#6

#Type of day of the week in data
class(RoyalPerthHospital$Day)




#Task 4
#4.1
#Calulating the 95th percentile of the average temperature 

final_perth1 <- final_perth[(names(final_perth) %in% c('date','datatype','value'))] %>% spread(key = datatype, value = value)
final_perth2 <- final_perth1[(names(final_perth1) %in% c('date','TMAX','TMIN'))]
final_perth3 <- na.omit(final_perth2)
final_perth3$TAVG <- ((final_perth3$TMAX + final_perth3$TMIN)/2)
T95_perth <- quantile(final_perth3$TAVG,0.95)

library(RcppRoll)
EHFdata <- final_perth3 %>% mutate(avg30 = roll_meanr(lag(TAVG),30), avg3 = roll_meanl(TAVG,3)) %>% mutate(EHIsig = avg3 - T95_perth, EHIaccl = avg3 - avg30) %>% mutate(EHF = EHIsig*pmax(1,EHIaccl))

RoyalPerthHospital_EHF <- RoyalPerthHospital
RoyalPerthHospital_EHF$EHF <- EHFdata$EHF
RoyalPerthHospital_EHF %>% ggplot(aes(x = Date, y= EHF)) + geom_line() + labs(title = " Excess Heat Factor Data from July 2013 - June 2014")

#4.2

#Fitting lm model for the EHF

ehf_model <- lm(Attendance ~ Date + EHF, data = RoyalPerthHospital_EHF)
summary(ehf_model)
glance(ehf_model)

#Augmenting the ehf model
ehf_results <- ehf_model %>% augment

#Plotting the residuals 

ehf_results %>% ggplot(aes(sample = .std.resid)) +  geom_qq() + geom_qq_line(col = 'red')
ehf_results %>% ggplot(aes(x = .fitted, y = .resid)) + geom_point() + geom_quantile() + geom_smooth(colour = 'yellow') 

ehf_results %>% ggplot(aes(x = Date, y = .resid)) + geom_point() + geom_quantile() + geom_smooth(colour = 'yellow')
ehf_results %>% ggplot(aes(x = EHF, y = .resid)) + geom_point() + geom_quantile() + geom_smooth(colour = 'yellow') 

ehf_results %>% ggplot(aes(x = .hat, y = .resid)) + geom_point() +  geom_hline(yintercept = 0,linetype = 'dotted')
ehf_results %>% mutate(big_hat = .hat > 0.03) %>% ggplot(aes(x = .hat, y = .cooksd)) + geom_point(aes(color = big_hat)) + geom_hline(yintercept = 0,linetype = 'dotted')



ehf_gammodel <- gam( Attendance ~ s(numeric_date) + s(EHF),
                  family=Gamma(link=log),
                  data=RoyalPerthHospital_EHF)

plot(ehf_gammodel)
plot(ehf_gammodel, residuals = TRUE)


summary(ehf_gammodel)
glance(ehf_gammodel)

#4.3

ehf_weeklyseasonality <- gam(Attendance ~ s(numeric_date, by = Day) + Day +s(EHF), data = RoyalPerthHospital_EHF)
plot(ehf_weeklyseasonality)
plot(ehf_weeklyseasonality, residuals = TRUE)
summary(ehf_weeklyseasonality)
glance(ehf_weeklyseasonality)



