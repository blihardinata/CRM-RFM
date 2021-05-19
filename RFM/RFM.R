getwd()
setwd("~/Desktop/R Sample CRM")
data.df <- read.csv("group_project.csv")

#BLOCK1: THE MOST PROFITABLE CUSTOMERS 
#please only use dplyr
library(dplyr) 
library(tidyr)
library(car)
library(GGally)
library(ggplot2)
variable.names(data.df)
head(data.df,20)
str(data.df)

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
library(zoo)
data.df$yq <- as.yearqtr(data.df$Order.Date, format = "%Y-%m-%d")
data.df$yq
library(lubridate)

#group by customer name to find total profit and discount

total_profit <- data.df %>%
  group_by(Customer.Name) %>%
  summarise(total_profit = sum(Profit),
            total_discount = sum(Discount),
            total_sales = sum(Sales))
total_profit

data.df <- total_profit %>% left_join(data.df)

#separate q with year
data.df$yq2 <- data.df$yq
data.df <- data.df %>%
  separate(yq2, into = c('Year','Quarter'), sep = " ") 

data.df <- data.df %>%
  select(-Quarter) 

data.df <- data.df %>%
  group_by(Year, Customer.Name) %>%
  mutate(yearly_sales = sum(Sales),
            yearly_discount = sum(Discount),
            yearly_profit = sum(Profit),
         yearly_shipping = sum(Shipping.Cost))

data.df$Year <- as.integer(data.df$Year)

str(data.df)

#rfm method - manual calculation
#new dataframe -> backup data as rfm.df

rfm.df <- data.df

#frequency method - 10 points per purchase

frequency <- rfm.df %>%
  group_by(Customer.Name) %>%
  count(Customer.Name)
frequency

rfm.df <- frequency %>% left_join(rfm.df) %>% rename(freq = n)

rfm.df <- rfm.df %>%
  mutate(frequency_score = freq * 10)

rfm.df %>%
  distinct(Customer.Name, freq, frequency_score)


#monetary value - 10% of the monetary value

rfm.df <- rfm.df %>%
  group_by(Customer.Name) %>%
  mutate(monetary = sum(Sales)*0.1)

rfm.df %>%
  distinct(Customer.Name, monetary)

#recency value 
recency <- rfm.df %>%
  group_by(Customer.Name) %>%
  summarise(last_date = max(Order.Date))
rfm.df <- recency %>% left_join(rfm.df)

analysis_date <- lubridate::as_date('2014-12-31')

rfm.df <- rfm.df %>%
  mutate(recency_days = analysis_date - rfm.df$last_date)

#recency points: 0-107 days = 20 points, 108-214days = 15 points, 215-321days = 10 points, 321 - 428days = 5 points
#update - recency points: 0-107 days = 15 points, 108-214days = 10 points, 215-321days = 5 points, 321 - 428days = 2 points
rfm.df <- rfm.df %>%
  mutate(recency_points = case_when(
    between(recency_days, 0, 107) ~ "20",
    between(recency_days, 108, 214) ~ "15",
    between(recency_days, 215, 321) ~ "10",
    TRUE ~ "5"
  ))
str(rfm.df)
rfm.df$recency_points <- as.integer(rfm.df$recency_points)

rfm.df %>%
  distinct(Customer.Name, last_date, recency_days, recency_points)

#RFM Score - recency = 20%, Frequency = 45%, Monetary = 35%
rfm.df <- rfm.df %>%
  mutate(rfm_score = (recency_points * 0.40 + frequency_score * 0.15 + monetary * 0.45))

test <- rfm.df %>%
  group_by(Customer.Name, Year) %>%
  select(Customer.Name, recency_points, frequency_score, monetary, rfm_score)

#rfm score is ranging from 302.779 - 1781.082 -> 301-1782 = customer segmentation
summary(rfm.df$rfm_score)
# Let's not use 5 number summary. Instead, let's do an increment of 296-7 ((max-min)/5 segments)
# updated with recency_points * 0.4 + frequency_score * 0.15 + monetary * 0.45. 1950/5
rfm.df <- rfm.df %>%
  mutate(customer_segments = cut(rfm_score,
                                 breaks = c(0,390, 780, 1170, 1560, 1950),
                                 labels = c("lost","at risk", "promising", "loyal", "champions"))) 

rfm.df %>% 
  group_by(Customer.Name, customer_segments) %>%
  distinct(Customer.Name)

rfm.df %>%
  filter(customer_segments == "champions") %>%
  distinct(Customer.Name)

#Show rfm scores for all
rfm.df %>%
  distinct(Customer.Name, recency_points, frequency_score, monetary, rfm_score, customer_segments)

#find the top 20% of the most profitable customers based on rfm score - pareto analysis
top20percent <- rfm.df %>%
  filter(rfm_score >=quantile(rfm_score,.80)) %>%
  distinct(Customer.Name, customer_segments)

top20percent

str(rfm.df)
#Use past value to generate the top 20% of customers and how many customers are matching?
#trial and error - what's the right relative weight for RFM scores?
#Assuming annual discount rate = 10% and measured quarterly

library(lubridate)

#days total
rfm.df <- rfm.df %>%
  group_by(Customer.Name) %>%
  mutate(days = (analysis_date - Order.Date)+1)

#Past customer value with 10.2% discount rate(0.00028)
str(rfm.df)

rfm.df$days = as.integer(rfm.df$days)

rfm.df <- rfm.df %>%
  group_by(Customer.Name) %>%
  mutate(Past_value = sum(Sales*((1.00028)^days)))

summary(rfm.df$Past_value)

#try if else
quantile(rfm.df$Past_value,.80)

top20percent_past <- rfm.df %>%
  filter(Past_value >= 24906.02) %>%
  distinct(Customer.Name, customer_segments)

top20percent_past

#what's the difference between RFM vs Past Lifetime Value

CLV_comparison <- setdiff(top20percent, top20percent_past)

#Where is the difference? Change the top 20% column with past value
top20percent_past <- rfm.df %>%
  filter(Past_value >= 24906.02) %>%
  distinct(Customer.Name, Past_value)

top20percent_past

CLV_comparison2 <- top20percent %>% full_join(top20percent_past)

#Trial and error to find the best match between two methods

#The discrepancy between two methods:(0.25+0.35+0.40)-rfm method missing 14, missing 10 from past value; 
#The discrepancy between two methods:(0.2+0.45+0.35) (missing 14 from rfm), (missing 16 from past_value);
#The discrepancy between two methods:(0.2+0.4+0.4) (missing 15 from rfm), (missing 11 from past_value);
#The discrepancy between two methods:(0.1+0.45+0.45) (missing 15 from rfm), (missing 11 from past_value);
#The discrepancy between two methods:(0.2+0.25+0.55) (missing 30 from rfm), (missing 8 from past_value);
#The discrepancy between two methods:(0.3+0.25+0.45) (missing 9 from rfm), (missing 8 from past_value); 
#The discrepancy between two methods:(0.40+0.15+0.45) (missing 8 from rfm), (missing 8 from past_value);BEST COMPARISON
#The discrepancy between two methods w 11% discount rate and *0.15 mon value:(0.40+0.15+0.45) (missing 13 from rfm), (missing 3 from past_value);
#The discrepancy between two methods w 8% discount rate and *0.15 mon value:(0.40+0.15+0.45) - not working
#The discrepancy between two methods w 10.5% discount rate :(0.40+0.15+0.45) - (missing 11 from rfm), (missing 6 from past_value)
#The discrepancy between two methods:(0.35+0.20+0.45) (missing 9 from rfm), (missing 9 from past_value);
#The discrepancy between two methods:(0.35+0.20+0.45) with 13 % (missing alot from rfm), (missing 1 from past_value);
#The discrepancy between two methods:(0.35+0.20+0.45) with 11 % (missing 14 from rfm), (missing 4 from past_value);


#remove 16 NA values listed under clv_comparison2 and create a new dataframe that concludes the top 20% of the most profitable customers. 
#combining two methods of finding customer profitability

top20.df <- rfm.df %>%
  filter(Past_value >= 24906.02, rfm_score >=quantile(rfm_score,.80)) %>%
  distinct(Customer.Name, customer_segments, rfm_score, Past_value)

#Use rfm.df to build a prediction model and compare the accuracy with customer lifetime value. 

#change variable into categorical
rfm.df <- rfm.df %>% 
  mutate(Segment = as.factor(Segment)) %>%
  mutate(Ship.Mode = as.factor(Ship.Mode)) %>%
  mutate(Order.Priority = as.factor(Order.Priority)) %>%
  mutate(customer_segments = as.factor(customer_segments)) %>%
  mutate(Category = as.factor(Category)) %>%
  mutate(Market = as.factor(Market)) %>%
  mutate(Year = as.factor(Year)) 

#checking outlier for CLV and Past_value
boxplot.stats(rfm.df$rfm_score)$out
boxplot.stats(rfm.df$Past_value)$out

#Checking at correlation between numerical values - ggcor automatically eliminates non-numerical variables. 
library(GGally)

ggcorr(rfm.df, method = c("everything", "pearson")) 

#outliers cannot be removed as all the outlier values are under "champion" or "the most profitable customers"
#checking correlation

#scatterplot matrix by customer segments - testing
rfm.df %>% ggpairs(columns = c("rfm_score", "total_profit", "Segment", "customer_segments", "Ship.Mode", "Market"),
                     aes(color = customer_segments),
                     upper = list(continous = wrap('cor', size = 9)),
                     lower = list(combo = wrap("facethist", bins =10)))

#linear regression model to build a prediction model 
str(rfm.df)

lm1 <- lm(rfm_score ~ Profit + customer_segments + Category + Year + Segment + 
            Market + freq + Quantity + Shipping.Cost + Discount + Sales, data = rfm.df)

summary(lm1)

lm2 <- lm(Past_value ~ Profit + customer_segments + Category + Year + Segment + 
            Market + freq + Quantity + Shipping.Cost + Discount + Sales, data = rfm.df)
summary(lm2)

#compare both model
anova(lm1, lm2)

#we are using rfm model
outlierTest(lm1)

vif(lm1)
#all VIF looks good - nothing above 5 so we are keeping all variables

library(MASS)
stepAIC(lm1, direction = "both")

#stepAIC advised to keep shipping cost, quantity, category and profit but the variables won't be able to make a coherent business decision. 
lm3 <- lm(rfm_score ~ customer_segments + Year + Segment + Quantity + Shipping.Cost + Discount + Sales, data = rfm.df)
summary(lm3)

#Based on lm3, build the prediction model and combine them

pred <- predict(lm3, rfm.df)
final.df <- cbind(rfm.df, pred)
final.df <- final.df %>% rename(prediction = 44) %>% mutate(AVGprediction = mean(prediction))

Unique.final <- final.df %>%
  mutate(error = rfm_score - AVGprediction) %>%
  distinct(Customer.Name, total_profit, total_sales, total_discount, Past_value, rfm_score, customer_segments, AVGprediction, error) 
  
head(Unique.final)

# use 2013-4 (dependent variable) -> X9 or X0 profit -> separate into 2013(ind) and 2014 to check the variance. use previous year to predict the new year
#create a new dataframe without year 2014
#use 2011-2013 profit to predict for 2014 and compare with the actual value

prediction2.df <- rfm.df %>%
  filter(Year == 2011:2013) 
  
str(prediction2.df)

prediction2.df %>% ggpairs(columns = c("rfm_score", "yearly_profit", "yearly_sales", "yearly_discount", "yearly_shipping", "customer_segments"),
                   aes(color = Year, alpha = 0.5),
                   upper = list(continous = wrap('cor', size = 5)),
                   lower = list(combo = wrap("facethist", bins =10)))

#linear regression with original dataset since we need year 2014 to display prediction value
prediction.df <- rfm.df

lm2014_1B <- lm(yearly_profit ~ customer_segments + Category + Year + Segment + Market + freq + Quantity + Shipping.Cost +
                 yearly_discount + yearly_sales + yearly_shipping, data = prediction.df)
summary(lm2014_1B)

vif(lm2014_1B)
#all VIF looks good - nothing above 5 so we are keeping all variables
outlierTest(lm2014_1B)

library(MASS)
lm2014_1b.aic <- stepAIC(lm2014_1B, direction = "both")

lm2014_2B <- lm(yearly_profit ~ yearly_discount + yearly_sales + yearly_shipping, 
               data = prediction.df[c(-10622, -10639, -10633, -10637, -10626, -10629, -10636, -10640, -10627, -10632),])
summary(lm2014_2B)

lm2014_2C <- lm(Profit ~ Discount + Shipping.Cost + Sales + Quantity + Market, 
                data = prediction.df[c(-10622, -10639, -10633, -10637, -10626, -10629, -10636, -10640, -10627, -10632),])
summary(lm2014_2C)

pred_2014_B <- predict(lm2014_2B, prediction.df)
final_1b.df <- cbind(prediction.df, pred_2014_B)

final_1b.df <- final_1b.df %>%
  group_by(Customer.Name, Year) %>%
  rename(prediction2 = 44) %>%
  mutate(error = yearly_profit - prediction2)

final2014.df <- final_1b.df %>%
  filter(Year == 2014) %>%
  distinct(Customer.Name, Year, yearly_sales, 
           yearly_discount, yearly_shipping, yearly_profit, prediction2, error, customer_segments)


write.csv(final_1b.df, "final_project.csv")

