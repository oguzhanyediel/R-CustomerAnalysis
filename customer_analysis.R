used_packages <- c("didrooRFM", "dplyr", "futile.logger", "ggplot2", "Hmisc",
                   "pastecs", "plyr", "psych", "readxl", "reshape2", "sqldf")
lapply(used_packages, require, character.only = TRUE)

flog.info("__author__ : Oğuzhan YEDİEL")
# It was taken in consideration Google R Style Guide 
# for codes to be read easily, shared, and verified!
# For example; Maximum Line Length = 80

# Below definitions were cited from
#   Introduction to Algorithmic Marketing: 
#     Artificial Intelligence for Marketing Operations - Ilya KATSOV

member_with_transac <- read_excel("~/Data.xlsx", 
                                  sheet = "Members with Transaction")
transac <- read_excel("~/Data.xlsx", 
                      sheet = "Transactions")

flog.info("Language of column names are changing...")
colnames(member_with_transac) <-
  c("UserID", "Date", "Condition", "LastLoginDate", "Sex",
    "DateOfBirth", "Bulletin", "SMS", "Order", "Language",
    "Country", "City", "FacebookMember", "MemberType")
colnames(transac) <-
  c("UserID", "OrderID", "OrderDate", "CampaignType", "ProductCode",
    "ProductName", "Option", "Barcode", "Full_ProductCode", "ShippingQuantity",
    "TotalGrossAmount", "ReducedAmount", "CampaignReducedAmount",
    "RemittanceReduced", "UsedPoint", "CommissionAmount", "TaxAmount",
    "CancellationAmount", "OrderAmount")
flog.info("Option column means size of any clothes such as socks or shoe")

# Definitions of the Data
glimpse(member_with_transac)
glimpse(transac)

summary(member_with_transac)
summary(transac)

# Several packages have 'describe()' function.
psych::describe(member_with_transac$Order)
Hmisc::describe(transac)
stat.desc(transac)

# Recency-Frequency-Monetary (RFM) is popular segmentation 
# for scoring the users based on their purchase behavior.

"
Recency: The number of time units that have passed since the customer
last purchased. Open Question: How recently did the customer purchase?

Frequency: The average number of purchases per time unit. Again,
the metric can be measured directly in units or scores.
Open Question: How often do they purchase?

Monetary: The total dollar amount spent per time unit. The monetary
metric is typically measured by using intervals or scores.
Open Question: How much do they spend (each time on average)?
"

flog.info("There are many options/techniques for RFM analysis.
          It was started with the easy one; didrooRFM package.
          After that, it was tried to develop manual ways
          by finding out R-F-M values.")

# RFM Analysis 1: "didrooRFM" package
customer_RFM_data <- transac[,c("OrderID", "UserID", 
                                "OrderDate", "OrderAmount")]
customer_RFM_data$OrderDate <- as.Date(customer_RFM_data$OrderDate)

RFM_analysis <- findRFM(
  customerdata =  customer_RFM_data
)

flog.info("Row count of RFM_analysis (%s) should be same 
          with the count of distinct customer (%s)", 
          nrow(RFM_analysis), 
          length(unique(customer_RFM_data$UserID)))

ddply(RFM_analysis, ~FinalCustomerClass, summarise, 
      number_of_distinct_orders=length(unique(CustomerID)))

customer_RFM_data[customer_RFM_data$UserID == 309742,] 
customer_RFM_data[customer_RFM_data$UserID == 310554,]

# RFM Analysis 2:
customer_RFM_data <- customer_RFM_data[customer_RFM_data$OrderAmount >= 0,]

upper_date <- as.Date(max(customer_RFM_data$OrderDate)) + 1

customer_RFM_data$days_since <-
  as.numeric(difftime(time1 = upper_date,
                      time2 = customer_RFM_data$OrderDate,
                      units = "days"))

customers = sqldf("SELECT UserID, 
                    MIN(days_since) AS 'recency', 
                    COUNT(*) AS 'frequency', 
                    AVG(OrderAmount) AS 'amount' 
                  FROM customer_RFM_data 
                  GROUP BY UserID")
summary(customers)

hist(customers$recency)
hist(customers$frequency)
hist(customers$amount)
hist(customers$amount, breaks = 100)
