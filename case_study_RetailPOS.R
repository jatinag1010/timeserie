
library(lubridate)
library(ggplot2)
library(dplyr)
library(graphics)
library(forecast)
setwd("D:/BACKUP/cloudbyte")
online.retail<-read.csv("online.retail.csv",sep=",",header=T)

##############################################Business Understanding#########################
#The data is POS data where one row represents a item purchased. The data has been filtered for the
#data of a single SKU "JUMBO BAG RED RETROSPOT".
#The data is for the sales for the December 2010 to December 2011
#We can initially group the data based on month to find out the sales per month and then can see trends, seasonality to
#Predict the sales of the product in next months

#################################################Data Understanding & Data Preparation###########
#InvoiceNumber represents the invoice no of the bill when the item was purchased
#StockCode represents the code to internally identify the product
#Description represents the item description
#Quantity represents the quantity of the item purchased
#invoice date represents the date on which the item was purchased
#Unit Price represents the per unit price of the stock
#Customer ID represents the internally identified code for the customer

# Seperating the month from the invoice date 
online.retail$InvoiceDate<-as.POSIXlt(online.retail$InvoiceDate,format="%d-%m-%Y %H:%M")
online.retail$month<-format(online.retail$InvoiceDate,"%m")
online.retail$InvoiceDate<-as.POSIXct(online.retail$InvoiceDate)
online_month<-group_by(online.retail,month) %>% summarise(sum(Quantity))
colnames(online_month)<-c("month","quantity")
timeser <- ts(online_month$quantity)
plot(timeser)
 
####################################################Data Modelling##################################

#There is a seasonality in the data . After every 2 months there is a increase in sales
#of the product.#There is a trend of upward increase in the sales
lmfit_trend <- lm(quantity~ month, data=online_month)
summary(lmfit_trend)
#All the values are NA values which indicates there is no trend in the data

timevals <- c(1:nrow(online_month))
#Modelling the seasonal trend in R
online_month$quancos<-cos(online_month$quantity)
lmfit_seasonal <- lm(quantity~ month, data=online_month)
summary(lmfit_seasonal)
globalpred <- predict(lmfit)

#Considering that the values of quantity is a white noise, we intend to draw an
#autocorrelation function to determine whether the values are white noise or not

acf(online_month$quantity, level=95, lag.max=40, main="ACF Plot for White Noise")
#############################################################Result###########################
#The values of the quantity are white noise with no seasonal or trends. It therefore implies
#that the sales quantity for the "jumbo bag red Retrospot" cannot be predicted.




