library(lubridate)
library(xlsx)
library(dplyr)
library(doBy)
library(data.table)

Retail_Data_R <- read.csv(file.choose())
Retail_Data_T <- read.csv(file.choose())
str(Retail_Data_R)
str(Retail_Data_T)
dim(Retail_Data_R)
dim(Retail_Data_T)
Retail_Data_T$trans_date<-dmy(Retail_Data_T$trans_date) #date month year
length(Retail_Data_R$customer_id)
length(unique(Retail_Data_T$customer_id))
max(Retail_Data_T$trans_date)
min(Retail_Data_T$trans_date)
#working in doby library and groupwise summary statistic
Cust_Sum<- summaryBy(tran_amount~customer_id, data = Retail_Data_T,
                     FUN=c(sum,length))
Cust_Sum<- rename(Cust_Sum, Amt_sum = tran_amount.sum, Frequency=tran_amount.length)

# get the max data for each customer
# ave = group averages over level combinations of factors
Retail_Data_T$Max_date <- with(Retail_Data_T, ave(trans_date, customer_id, FUN=max))
DB <- merge(Retail_Data_R,Cust_Sum,by="customer_id")
DB1 <- merge(DB, Retail_Data_T, by="customer_id")
# remove duplicates based on customer_id columns
DB2 <- DB1[!duplicated(DB1$customer_id),]
DB2<- DB2[-c(2,5:6)]
# group_by work in dplyr library
df_RFM <- DB2 %>%
  group_by(customer_id) %>%
  summarise(recency=as.numeric(as.Date("2015-04-01")-max(Max_date)),
            frequenci=Frequency, monitery=Amt_sum)
summary(df_RFM)
head(df_RFM)

# CREATING R, F, M LEVELS

df_RFM$rankR = cut(df_RFM$recency, 5, labels = F) # RANKR 1 IS VERY RECENT WHILE RANKR 5 IS LEAST RECENT

df_RFM$rankF =  cut(df_RFM$frequenci, 5, labels = F) #rankF 1 is least frequent while rankF 5 IS MOST FREQUENT

df_RFM$rankM = cut(df_RFM$monitery, 5, labels = F)# rankM 1 is lowest sales while rankM

# total 
attach(df_RFM)
df_RFM$Total <- (rankR*100)+(rankF*10)+(rankM)
