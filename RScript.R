
#Importing required libraries

library(tidyverse)
library(readxl)
library(pheatmap)
library(tseries)
library(forecast)
library(plotly)
library(Hmisc)
library(ggthemes)


#Viewing the sheets
excel_sheets("C:/Users/Pasindu/Desktop/MAS Project/Absent Details - 06 month.xlsx")

#Combining data set into single table
absent_data<-excel_sheets("C:/Users/Pasindu/Desktop/MAS Project/Absent Details - 06 month.xlsx")%>%
  map_df(~read_xlsx("C:/Users/Pasindu/Desktop/MAS Project/Absent Details - 06 month.xlsx",.))

#Checking missing values
sum((is.na(absent_data)))
which(is.na(absent_data))

#Exporting the data set as a csv file
write.csv(absent_data,file = "absent_data.csv",row.names = FALSE)

#Reading exported data
data<-read.csv("C:/Users/Pasindu/Desktop/MAS Project/absent_data.csv")

#We are going to replace  the missing data with mean values. But before we do that, we need to find out that
#what columns has missing values. We describe function in Hmisc package to find that
describe(data)

#Here family issues column and natural disaster column has those missing data. To confirm that we use
#is.na and sum functions.

sum(is.na(data$Natural.Disaster))
sum(is.na(data$Family.Issue))

#As we can see those 2 columns has the 2 missing values. So we replace those missing values with mean value
#of the each column.
#Finding the index of the missing values

which(is.na(data$Natural.Disaster))
which(is.na(data$Family.Issue))

#Replacing with mean. (Here we need to round these numbers unless entire column will convert into double numbers)
data$Natural.Disaster[160]<-round(mean(data$Natural.Disaster, na.rm = T))
data$Family.Issue[16]<-round(mean(data$Family.Issue, na.rm = T))

#Checking the structure of the data set
str(data)

#Here we need to convert several columns including previous 2 columns with missing values
data<-type.convert(data,as.is = TRUE)

#Above code we used function that automatically change the class of the column. But here Date column changed to character class
#so we need to convert it to Date class
data <- mutate(data,Date = as.Date(data$Date,format = "%Y-%m-%d"))

#Now we need to arrange the data set into a ascending order using date class
data%>%
  arrange(Date)
#Checking the missing values again
sum(is.na(data))
#There is no missing values and data set is sorted.So we need to export this data for further work
write.csv(data,file = "cleaned_data.csv",row.names = FALSE)

#Importing data to create data set that include total absents for a section in a day
data_section_wise<-read.csv("C:/Users/Pasindu/Desktop/MAS Project/cleaned_data.csv")

#When we import the data set, date column is always imported as character column we need to convert it to date class.
data_section_wise <- mutate(data_section_wise,Date = as.Date(data_section_wise$Date,format = "%Y-%m-%d"))
data_section_wise%>%
  arrange(Date)

#Getting the total of the section in an each day
data_section_wise<-data_section_wise%>%
  rowwise()%>%
  mutate(total_absent_section = sum(c_across(where(is.integer))))

#Arranging data set according to the date column
data_section_wise<-arrange(data_section_wise,Date)

#Exporting the data set
write.csv(data_section_wise,file = "cleaned section wise data set with daily total absents for a section.csv",row.names = FALSE)


#Importing the cleaned data set
data<-read.csv("C:/Users/Pasindu/Desktop/MAS Project/cleaned_data.csv")

#When we import the data set, date column is always imported as character column we need to convert it to date class.
data <- mutate(data,Date = as.Date(data$Date,format = "%Y-%m-%d"))
data%>%
  arrange(Date)


#Renaming column names and grouping variables according to date and shift
data<-data%>%
  group_by(Date,Shift)%>%
  summarise(Leave=sum(Leave),Fever = sum(Fever),Sick=sum(Sick),Injured_Surgery=sum(Injured.Surgery),Hospitalized=sum(Hospitalized),
            Natural_Disaster=sum(Natural.Disaster),Sick_Related=sum(Sick.Related),Family_member_Sick=sum(Family.member.Sick),
            Family_Issue=sum(Family.Issue),Baby_sitting=sum(Baby.sitting),Funeral=sum(Funeral),Interview_Educational_Other=sum(Interview.Educational...Other),
            Family_Related=sum(Family.Related),Work_Related=sum(Work.Related),Transport_Related=sum(Transport.Related),Unauthorized=sum(Unauthorized))

#Getting the total of the shift in an each day
data<-data%>%
  rowwise()%>%
  mutate(total_absent_shift = sum(c_across(where(is.integer))))

#Exporting above data set
write.csv(data,file = "cleaned data grouped by date and shift with column of sum of shift.csv",row.names = FALSE)

#Importing above data set
data<-read.csv("C:/Users/Pasindu/Desktop/MAS Project/cleaned data grouped by date and shift with column of sum of shift.csv")

#When we import the data set, date column is always imported as character column we need to convert it to date class.
data <- mutate(data,Date = as.Date(data$Date,format = "%Y-%m-%d"))
data%>%
  arrange(Date)

#Visualizing the data set
ggplot(data)+
  geom_line(mapping = aes(x = Date,y = total_absent_shift,colour = Shift))+
  scale_x_date(date_breaks = "1 month",date_labels = "%b")

#According to above chart shift A and B has a roughly similar pattern.So we can build another data set grouped by date column
data_date<-data%>%
  group_by(Date)%>%
  summarise(Leave=sum(Leave),Fever = sum(Fever),Sick=sum(Sick),Injured_Surgery=sum(Injured_Surgery),Hospitalized=sum(Hospitalized),
            Natural_Disaster=sum(Natural_Disaster),Sick_Related=sum(Sick_Related),Family_member_Sick=sum(Family_member_Sick),
            Family_Issue=sum(Family_Issue),Baby_sitting=sum(Baby_sitting),Funeral=sum(Funeral),Interview_Educational_Other=sum(Interview_Educational_Other),
            Family_Related=sum(Family_Related),Work_Related=sum(Work_Related),Transport_Related=sum(Transport_Related),Unauthorized=sum(Unauthorized))

#Adding total absent column to data set
data_date<-data_date%>%
  rowwise()%>%
  mutate(total_absent_shift = sum(c_across(where(is.integer))))


#Exporting above data set
write.csv(data_date,file = "C:/Users/Pasindu/Desktop/MAS Project/cleaned data grouped by date with column total absent.csv")

#Importing above data set
data<-read.csv("C:/Users/Pasindu/Desktop/MAS Project/cleaned data grouped by date with column total absent.csv")

#When we import the data set, date column is always imported as character column we need to convert it to date class.
data <- mutate(data,Date = as.Date(data$Date,format = "%Y-%m-%d"))
data%>%
  arrange(Date)

#Now we have a cleaned and sorted data set grouped by date. Now we can use this data set to analyze the data set and model a time series

ggplot(data)+
  geom_line(mapping = aes(x = Date,y = total_absent_shift,colour = "Red"))+
  scale_x_date(date_breaks = "1 month",date_labels = "%b")

#We can see seasonality property and stationary and non-stationary properties in this chart. So we can
#build a time series model for time ranges. To further analyze these properties we can use forecast
#package

#Here we use "acf" function to check the stationary of the data set. We already have a rough idea.
#But to confirm we can use a chart

acf(data$total_absent_shift)
#Here all the lines have surpass the blue line. That means data is not stationary.

#Checking the partial acf
pacf(data$total_absent_shift)
#Here we can see one line surpasses the blue line

#Selecting a ARIMA model
total_absent_model<-auto.arima(data$total_absent_shift,ic = "aic",trace = TRUE)

#Checking the acf of the model
acf(ts(total_absent_model$residuals))
#Checking the pacf of the model
pacf(ts(total_absent_model$residuals))

#Forecasting
forecast_total_absent<-forecast(total_absent_model,level = c(95),h=7)
forecast_total_absent
plot(forecast_total_absent)

#Time Series modeling

#Importing the data set
data_date_wise<-read.csv("C:/Users/Pasindu/Desktop/MAS Project/cleaned data grouped by date with column total absent.csv")
data_date_wise <- mutate(data_date_wise ,Date = as.Date(data_date_wise$Date,format = "%Y-%m-%d"))
#Creating ts object
absent_data_ts<-ts(data_date_wise$total_absent_shift,start = c(2022),frequency = 12*30)
autoplot(absent_data_ts)

#Converting to stationary series
absent_data_ts_diff1<-diff(absent_data_ts,differences = 1)
adf.test(absent_data_ts_diff1,k=12)

#This is stationary
plot(absent_data_ts_diff1)

#pacf plot
pacf(absent_data_ts_diff1)

#acf plot

acf(absent_data_ts_diff1)


absent_ts_model<-Arima(y=absent_data_ts,order = c(0.025,1,0.041))
print(absent_ts_model)


autoplot(forecast(absent_ts_model,h=12))

### Checking monthly average
data$weekly_absents<-ma(data$total_absent_shift,order=7)
data$monthly_absents<-ma(data$total_absent_shift,order=30)

data<-replace(data,TRUE,lapply(data,na.aggregate))


#Plotting moving average with daily absents
str(data)
ggplot()+
  geom_line(data = data,aes(x=Date,y=data$total_absent_shift,colour = "total_absent_shift"))+
  geom_line(data = data,aes(x=Date,y=data$weekly_absents,colour = "weekly_absents"))+
  geom_line(data = data,aes(x=Date,y=data$monthly_absents,colour = "monthly_absents"))


absent_ma_1<-ts(data$total_absent_shift,frequency = 12)
plot(absent_ma_1)




