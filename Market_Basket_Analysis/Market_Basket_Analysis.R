###BLOCK4 MARKET BASKET ANALYSIS####
getwd()
setwd("~/Desktop/R Sample CRM")
data.df <- read.csv("group_project.csv")

library(dplyr) 
library(tidyr)
library(car)
library(GGally)
library(ggplot2)
library(arules)
library(arulesViz)
library(zoo)
library(lubridate)
library(stringr)

#changing date from character to date structure
a <- as.Date(data.df$Order.Date,format="%m/%d/%Y") # Produces NA when format is not "%m/%d/%Y"
b <- as.Date(data.df$Order.Date,format="%d-%m-%Y") # Produces NA when format is not "%d-%m-%Y"
a[is.na(a)] <- b[!is.na(b)] # Combine both while keeping their ranks
data.df$Order.Date <- a

summary(data.df$Order.Date)

a <- as.Date(data.df$Ship.Date,format="%m/%d/%Y") # Produces NA when format is not "%m/%d/%Y"
b <- as.Date(data.df$Ship.Date,format="%d-%m-%Y") # Produces NA when format is not "%d-%m-%Y"
a[is.na(a)] <- b[!is.na(b)] # Combine both while keeping their ranks
data.df$Ship.Date <- a

summary(data.df$Ship.Date)

#splitting date to Q+Y

data.df$yq <- as.yearqtr(data.df$Order.Date, format = "%Y-%m-%d")
data.df$yq

#separate q with year
data.df$yq2 <- data.df$yq
data.df <- data.df %>%
  separate(yq2, into = c('Year','Quarter'), sep = " ") 

data.df <- data.df %>%
  select(-Quarter) 


#Create a new dataframe with product name and transactional ID
MBA_df <- data.df %>% 
  select(Customer.Name, Order.Date, Order.ID, Product.Name)

data.df %>%
  group_by(Category, Sub.Category)%>%
  summarise(n=n())

# using library stringr to remove comma in the product name
MBA_df <- MBA_df %>%
  mutate(Product.Name = str_remove(Product.Name, ","))

#create a transaction list
transaction <- data.frame(
  "TID" = MBA_df$Order.ID, "Product" = MBA_df$Product.Name)

transaction$TID <- as.factor(transaction$TID)

data_list <- split(transaction$Product, transaction$TID)

transaction.df <- as(data_list, "transactions")
inspect(transaction.df)

#checking the first/bottom 10 transactions
head(transaction.df, 10)
inspect(transaction.df[1:10])
inspect(tail(transaction.df))

#Top20 best selling product
itemFrequencyPlot(transaction.df, type = "absolute", topN = 20, col = 'orange')

#Bottom20 best selling product -graph is pretty awful
barplot((sort(table(unlist(LIST(transaction.df)))))[1:20], las = 1, col = "blue")

#setup the rules - frequent itemset
rules = apriori(transaction.df, parameter = list(support= 0.001, confidence = 0.5, target = "frequent itemsets"))
inspect(head(sort(rules, by = "support"),10))

#setup the rules - rules
rules2 = apriori(transaction.df, parameter = list(support= 0.0001, confidence = 0.1, target = "rules"))
inspect(head(sort(rules2, by = "lift"),10))
inspectDT(rules2)

#interactive subgraph
top20rules = head(sort(rules2, by = "confidence"), 20)
inspect(top20rules)

plot(top20rules, method = "graph", engine = "html")
ruleExplorer(rules2)

#What did Staples influence?
staples_lhs = apriori(transaction.df, parameter = list(support= 0.00001, confidence = 0.01, minlen = 2),
                      appearance = list(default = "rhs", lhs = "Staples"))

inspect(head(sort(staples_lhs, by = "lift"),20))

#what influenced buying staples?
staples_rhs = apriori(transaction.df, parameter = list(support= 0.00001, confidence = 0.01, minlen = 2),
                                  appearance = list(default = "lhs", rhs = "Staples"))

inspect(head(sort(staples_rhs, by = "lift"),10))
