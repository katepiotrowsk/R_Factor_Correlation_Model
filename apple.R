library(dplyr)

# sets path
setwd("~/Correlation_Model")

# reads csv file (use the csv2 function)
msciData <- read.csv2(file="MSCI_SUMMARY_SCORES3.csv", header=TRUE)
priceData <- read.csv(file="Price_Data_New_Copy.csv")

ticker <- priceData$BB_TICKER_CD
price <- priceData$PBD_PRICE_AMT
priceDate <- priceData$DATE_DIM_ID

# set-up data frame with name, date, price, y1
df <- priceData.frame(priceDate, ticker, price)

df$Month <- format(as.Date(price_date, format="%Y%m%d"),"%m")
df$Year <- format(as.Date(price_date, format="%Y%m%d"),"%Y")
df

#df[order(as.Date(df$date, format="%Y%m%d"))]
#print("Given Data")
#df

#print("Calculated Monthly Average")
#aggregate(price~Month+Year,df,mean)



#cor(df$Price, df$y1,
#    method = "spearman"
#)

#pvalues <- rcorr(as.matrix(df$Price, df$y1))
#print(pvalues)

#mat_1 <-as.dist(round(cor(df[,1:3]),2))
#mat_1
