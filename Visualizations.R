
#Importing required libraries
library(tidyverse)
library(readxl)
library(pheatmap)
library(tseries)
library(forecast)
library(plotly)
library(Hmisc)
library(FactoMineR)
library(factoextra)


#Importing section wise data set
data_section_wise<-read.csv("C:/Users/Pasindu/Desktop/MAS Project/cleaned section wise data set with daily total absents for a section.csv")
data_section_wise <- mutate(data_section_wise,Date = as.Date(data_section_wise$Date,format = "%Y-%m-%d"))

#Importing shift wise data set
data_shift_wise<-read.csv("C:/Users/Pasindu/Desktop/MAS Project/cleaned data grouped by date and shift with column of sum of shift.csv")
data_shift_wise <- mutate(data_shift_wise,Date = as.Date(data_shift_wise$Date,format = "%Y-%m-%d"))

#Importing date wise data set
data_date_wise<-read.csv("C:/Users/Pasindu/Desktop/MAS Project/cleaned data grouped by date with column total absent.csv")
data_date_wise <- mutate(data_date_wise,Date = as.Date(data_date_wise$Date,format = "%Y-%m-%d"))


#Using ggplot

#Section wise total absent
ggplot(data = data_section_wise)+
  geom_line(mapping = aes(x=Date,y=data_section_wise$total_absent_section,colour = Section))+
  scale_x_date(date_breaks = "1 month",date_labels = "%b")

#Shift wise total absent
ggplot(data = data_section_wise)+
  geom_line(mapping = aes(x=Date,y=data_section_wise$total_absent_section,colour = Shift))+
  scale_x_date(date_breaks = "1 month",date_labels = "%b")


#Using Plotly

#Date wise total absent
plot_ly(data = data_date_wise,
        x = ~Date,
        y = ~data_date_wise$total_absent_shift,
        mode = "line"
        
)
#Section wise
#Creating color palette
palette_1<-c("red","blue","green","orange")
plot_ly(data = data_section_wise,
        x = ~Date,
        y = ~data_section_wise$total_absent_section,
        type = "scatter",
        color = ~as.factor(Section),
        colors = palette_1,
        symbol = ~as.factor(Section),
        marker = list(size = 5)
        
)%>%
  layout(title = "Section Wise Daily Absents",
         xaxis = list(title = "Date"),
         yaxis = list(title = "Total Absents"))

#Shift wise
#Creating color palette
palette_2<-c("red","orange")
plot_ly(data = data_shift_wise,
        x = ~Date,
        y = ~data_shift_wise$total_absent_shift,
        type = "scatter",
        color = ~as.factor(Shift),
        colors = palette_2,
        symbol = ~as.factor(Shift),
        marker = list(size = 5)
        
)%>%
  layout(title = "Shift Wise Daily Absents",
         xaxis = list(title = "Date"),
         yaxis = list(title = "Total Absents"))


#Checking the connection between variables
pheatmap(data_date_wise[,3:19],
         cluster_rows = TRUE,
         cluster_cols = TRUE,
         clustering_method = "complete")


section_wise_pca<-PCA(data_section_wise[,4:20],
    scale.unit = TRUE,
    ncp = 5)

fviz_pca_ind(section_wise_pca,
             col.ind = data_section_wise$Section)










