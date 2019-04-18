
#Setting up the enviornment

library(tidyverse)
library(ggplot2)
library(dplyr)
library(plotly)
library(stringr)
library(lubridate)
library(plyr)
library(factoextra)

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



#PRE-RFM Analysis

eRetail_UK <- subset(eRetail, eRetail$Country=="United Kingdom", select = c(CustomerID,InvoiceNo,StockCode,Description,Amount,Quantity,UnitPrice,Invoice_date,Invoice_time,InvoiceDate) )
RFM_Uk <- subset(RFM1, RFM1$Country=="United Kingdom", select = c(CustomerID,sumAmount,sumQuantity,Monetary,M,Frequency,Fq,Recency,R) )
eRFM_uk<- subset(RFM, RFM$Country=="United Kingdom", select = c(CustomerID,Invoice_date,Amount) )


UK1<-ddply(eRetail_UK, .(unique(Description)), summarize, sumAmount=sum(Amount), sumQuantity=sum(Quantity))
names(UK1) [1] <-"product"
UK1$product<-as.character(UK1$product)
head(UK1[order(-UK1$sumQuantity),])

UK2 <- subset(eRetail_UK, Description%in%c("PACK OF 12 RED APPLE TISSUES","HANGING CLEAR MINI BOTTLE","PACK OF 12 VINTAGE LEAF TISSUES ","CHERRY CROCHET FOOD COVER","GLASS CHALICE GREEN  LARGE "), select = c(Description,Invoice_date,Invoice_time,Quantity))

UK2$Invoice_month<-month(UK2$Invoice_date)
UK2$Decription<-as.character(UK2$Description)
ggplot(UK2, aes(x=Invoice_month, y= Quantity))+ facet_wrap(~Description, ncol=2) + 
  geom_bar(stat="identity") + 
  labs(title = "Sales by month", x = "Month", y = "Sales Volume")


#RFM Analysis

RFM2 <-ddply(RFM, .(CustomerID),summarize,sumAmount=sum(Amount),sumQuantity=sum(Quantity),Monetary=max(Amount),Frequency=length(unique(InvoiceDate)))

RFM3<-ddply(RFM, .(CustomerID,Country),summarize,First_date=min(as.Date(Invoice_date)),Last_date=max(as.Date(Invoice_date)))
RFM3$Recency<-as.numeric(difftime(endDate,RFM3$Last_date,units="days"))

RFM1 <- merge(RFM2,RFM3, by = "CustomerID")
summary(RFM1)

#Rearrange the columns
RFM1 <- RFM1[, c(1, 9, 5, 4, 2,3,6,7,8)]



#Products vs Sales

ggplot(RFM1, aes(x = Country, y = Fq)) + geom_jitter() +
  labs(title = "Purchase Frequency by Country", x = "Country", y = "Frequency")+coord_flip()+th 



#scale dataframe
RFM_scale <- RFM1[1:4]
row.names(RFM_scale) <- RFM_scale$CustomerID
RFM_scale$CustomerID <- NULL

RFM_scale <- scale(RFM_scale,scale = TRUE)
summary(RFM_scale)

#Different Methods to get optimal number of clusters
#Elbow Method
fviz_nbclust(RFM_scale, kmeans, method = "wss") +
  geom_vline(xintercept = 4, linetype = 2)+
  labs(subtitle = "Elbow method")

# Silhouette method
fviz_nbclust(RFM_scale, kmeans, method = "silhouette")+
  labs(subtitle = "Silhouette method")

#Gap Statistic
set.seed(123)
fviz_nbclust(RFM_scale, kmeans, nstart = 25,  method = "gap_stat", nboot = 50)+
  labs(subtitle = "Gap statistic method")

NbClust(data = NULL, diss = NULL, distance = "euclidean",
        min.nc = 2, max.nc = 15, method = NULL)


#Applying k-means with k = 4

kmeans.res <- kmeans(RFM_scale, 4)

plot(RFM1[c("Frequency", "Monetary")], col = kmeans.res$cluster)

