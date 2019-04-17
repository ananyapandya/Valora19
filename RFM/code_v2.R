
#Setting up the enviornment

library(tidyverse)
library(ggplot2)
library(dplyr)
library(plotly)
library(stringr)
library(lubridate)
library(plyr)

#Import dataset from Kaggle

df_data_all <- read.csv(file = '../data_all.csv') 


#Data Cleaning

#exclude negative quantites
eRetail <- subset(df_data_all, Quantity > 0, select = c(1:8))

#create Amount variable
eRetail$Amount <- eRetail$Quantity * eRetail$UnitPrice

#separate date & time#
eRetail$Invoice_date <- format(as.POSIXct(strptime(eRetail$InvoiceDate,"%m.%d.%y %H:%M",tz="")) ,format = "%m/%d/%y")
eRetail$Invoice_time <- format(as.POSIXct(strptime(eRetail$InvoiceDate,"%m.%d.%y %H:%M",tz="")) ,format = "%H:%M")

#eRetail$Invoice_date<- str_trim(str_extract_all(eRetail$InvoiceDate, "^[0-9]+\\.[0-9]+\\.[0-9]+"))
#eRetail$Invoice_date <- str_replace_all(eRetail$Invoice_date, "\\.", "\\-" )
#eRetail$Invoice_time<- str_trim(str_extract_all(eRetail$InvoiceDate, " [0-9]+\\:[0-9]+$"))

#Dropping NA values

eRetail <- eRetail %>%
  drop_na()

#Now 397924 observations.

#*define the start date and end date of the one-year data frame.

#subset only necessary columns for later RFM Analysis
#by one year time frame
eRetail$Invoice_date <- as.Date(eRetail$Invoice_date, "%m/%d/%y")
startDate = "2010-01-12"
endDate = "2011-01-11"

RFM <- subset(eRetail, Invoice_date > startDate & Invoice_date < endDate )
length(unique(RFM$CustomerID))


#RFM Analysis

RFM2 <-ddply(RFM, .(CustomerID),summarize,sumAmount=sum(Amount),sumQuantity=sum(Quantity),Monetary=max(Amount),Frequency=length(unique(InvoiceDate)))

RFM3<-ddply(RFM, .(CustomerID,Country),summarize,First_date=min(as.Date(Invoice_date)),Last_date=max(as.Date(Invoice_date)))
RFM3$Recency<-as.numeric(difftime(endDate,RFM3$Last_date,units="days"))

RFM1 <- merge(RFM2,RFM3, by = "CustomerID")
summary(RFM1)

#Visualizations





