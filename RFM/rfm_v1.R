
#Setting up library
library(data.table)
library(dplyr)
library(ggplot2)
library(tidyverse)
#library(stringr)
#library(DT)
library(tidyr)
library(knitr)
library(rmarkdown)
library(plotly)
library("NbClust")
library("cluster")
library("factoextra")
library("magrittr")
library(shiny)


##Load Dataset
df_data <- fread('../data.csv')
glimpse(df_data)


### Data Cleaning
#Delete all negative Quantity and Price. We also need to delete NA customer ID
df_data <- df_data %>% 
  mutate(Quantity = replace(Quantity, Quantity<=0, NA),
         UnitPrice = replace(UnitPrice, UnitPrice<=0, NA))

df_data <- df_data %>%
  drop_na()


### Recode variables
#We should do some recoding and convert character variables to factors.

df_data <- df_data %>% 
  mutate(InvoiceNo=as.factor(InvoiceNo), StockCode=as.factor(StockCode), 
         InvoiceDate=as.Date(InvoiceDate, '%m/%d/%Y %H:%M'), CustomerID=as.factor(CustomerID), 
         Country=as.factor(Country))

df_data <- df_data %>% 
  mutate(total_dolar = Quantity*UnitPrice)

glimpse(df_data)


df_RFM <- df_data %>% 
  group_by(CustomerID) %>% 
  summarise(recency=as.numeric(as.Date("2012-01-01")-max(InvoiceDate)),
            frequenci=n_distinct(InvoiceNo), monitery= sum(total_dolar)/n_distinct(InvoiceNo)) 

summary(df_RFM)

kable(head(df_RFM))


df_RFM$Recency_group <- cut(df_RFM$recency, 
                            quantile(df_RFM$recency, 
                                     probs=seq(0,1,0.25)), 
                            ordered_result=T, 
                            include.lowest=T) # segment data into groups

df_RFM$Recency_group <- factor(df_RFM$Recency_group, 
                               labels=c("very recent", "recent", "old", "oldest")) # rename levels


df_RFM$Frequency_group <- cut(df_RFM$frequenci, 
                              c(0,1,3,10,188), 
                              ordered_result=T) #segment into four groups

df_RFM$Frequency_group <- factor(df_RFM$Frequency_group, 
                                 labels=c("very rare", "rare", "frequent", "very frequent"))


df_RFM$mon_value_group <- cut(df_RFM$monitery, 
                              quantile(df_RFM$monitery, probs=seq(0,1,0.25)), 
                              ordered_result=T, include.lowest=T) #segment into groups

df_RFM$mon_value_group <- factor(df_RFM$mon_value_group, 
                                 labels=c("small", "medium", "large", "very large")) #rename levels



#Visualize the results

ggplot(df_RFM, aes(Recency_group, Frequency_group)) +
  geom_count() +
  facet_grid(mon_value_group ~ .) +
  labs(x="Recency", y="Frequency", title="RFM analysis") 



mytext=paste("Frequency = ", df_RFM$frequenci, "\n" , "Monetary = ", df_RFM$monitery, "\n", "Row Number: ",rownames(df_RFM),  sep="")    
pp=plotly_build(p)   
style( pp, text=mytext, hoverinfo = "text", traces = c(1, 2, 3) )
pp %>% offline(height = 600)


p1 <- plot_ly(df_RFM, x = ~df_RFM$frequenci, y = ~df_RFM$monitery , type="scatter", mode = "markers", 
              marker=list( size=20 , opacity=0.5), color = ~df_RFM$recency)
p1 %>% offline(height = 600)