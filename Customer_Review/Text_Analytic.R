#Data initialization section
library(dplyr) 
library(tidyr)
library(janitor)
library(ggplot2)
library(GGally)
#install.packages("tidyverse")
library(tidyverse)
getwd()

#get the dataset
#df <- read.csv("Document/Education/LMU/Portfolio/Tableau/Womens Clothing E-Commerce Reviews.csv")

#get the dataset from github
df <- read.csv("https://raw.githubusercontent.com/blihardinata/R/main/Customer_Review/Womens%20Clothing%20E-Commerce%20Reviews.csv")


#Check the overall dataset
summary(df)
str(df)

#check any NA
any(is.na(df))

#There is no NA despite there are blank cells. 
#Let's try using two methods to remove NA
df <- df[complete.cases(df),]
df <- remove_empty(df, which = c("rows"))

#The blank rows can't be removed. 
sum(df == "")
#it shows that there are 4697 blank cells total
sum(df$Title == "") #3810 blank cells under the column of title

#Subset all rows in which the titles are not blank. 
#1 = row, 2 = columns
#any refers to at least one cell, ALL refers to all rows
df <- df[!apply(df == "", 1, any), ]

df %>%
  summarise(n_of_rows = n()) #there are 19662 rows total

#Checking on the number of rows in each department. 
df %>%
  group_by(Department.Name) %>%
  summarise(n_of_rows = n()) 
  
#there are 19662 rows total

#Remove unnecessary columns
df <- df %>%
  select(-c(X, Positive.Feedback.Count))

#Adding columns for the rating label whether the label is positivee or negative
df <- df %>%
  mutate(rating_label = if_else(Rating <= 3, "Negative", "Positive"))


#Compute average ratings by department name
avg_by_dept = df %>% 
  group_by(Department.Name) %>%
  summarise(average_rating = mean(Rating))

ggplot(avg_by_dept, aes(x = Department.Name, y = average_rating, color = Department.Name)) + geom_col()

#Tokenizing and Cleaning

#install.packages("tidytext")
library(tidytext)

cleaned_df <- df %>%
  unnest_tokens(word, Review.Text)

cleaned_review <- cleaned_df %>%
  count(word) %>%
  arrange(desc(n))

#Applying common words/stopwords that do not give any insight to the analysis
#anti_join removes stopwords but it doesn't work. Hence, we are using match function

#match() will match all the values in the list of stopwords. Other words that do not match with the stopwords return as NA
#isna returns all NA value as True, hence, extracting out the non-stopwords

df2 <- cleaned_df[is.na(match(cleaned_df$word, stop_words$word)),]

#visualization for the word counts
word_counts <- df2 %>% 
  count(word) %>% 
  filter(n >=1000) %>%
  arrange(desc(n))

ggplot(word_counts, aes(x = word, y = n)) + geom_col() + coord_flip() + ggtitle('Total Word Counts')

#Adding custom stop words

custom <- tribble(
  ~word, ~lexicon,
  "4" , "CUSTOM",
  "2" , "CUSTOM",
  "6" , "CUSTOM"
)

stop_words2 <- stop_words %>%
  bind_rows(custom)

df2 <- cleaned_df[is.na(match(cleaned_df$word, stop_words2$word)),]

word_counts2 <- df2 %>% 
  count(Department.Name, word) %>% 
  filter(n >=1000) %>%
  arrange(desc(n))

#Visualizing words with facet wrap

fct_word_count <- df2 %>%
  count(Department.Name, word) %>% 
  group_by(Department.Name) %>%
  top_n(10, n) %>%
  ungroup() %>%
  mutate(word2 = fct_reorder(word, n))

ggplot(fct_word_count, aes (x = word2, y = n, fill = Department.Name)) +
  geom_col(show.legend = FALSE) +
  facet_wrap (~Department.Name, scales = "free_y") +
  coord_flip() +
  ggtitle("Word Counts Based on Department")

#creating a sentimental analysis with Bing
#when using join, column name must be the same!!

df3 <- merge(x = df2, y = get_sentiments("bing"), by = "word")

df3_tidy <- df2 %>% inner_join(get_sentiments("bing")) %>% count(Rating, sentiment) %>% spread(sentiment, n)

#With "bing" dictionary, it shows that there are more positive sentiments in 2-5 star rating and there is more negative sentime in 1 star rating. 

#install.packages("textdata")
library(textdata) #used to unload sentiment dictionaries, such as afinn and nrc

#install.packages("tm") to unload tm package for cast dtm
library(tm) #to unload cast_dtm

dtm_review <- df2 %>%
  count(word, Clothing.ID) %>%
  cast_dtm(Clothing.ID, word, n) %>%
  as.matrix()

dtm_review[20:25, 1000:1010]

#install.packages("topicmodels")
library(topicmodels)

lda_out <- LDA(
  dtm_review, 
  k = 3, 
  method = "Gibbs",
  control = list(seed = 42))

lda_topics <- lda_out %>%
  tidy(matrix = "beta") %>%
  arrange(desc(beta))

#Visualizing topics

word_probs <- lda_topics %>% 
  group_by(topic) %>% 
  top_n(15, beta) %>% 
  ungroup() %>%
  mutate(term2 = fct_reorder(term, beta))

ggplot(word_probs, aes (term2, beta, fill = as.factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap (~topic, scales = "free") +
  coord_flip() +
  ggtitle("The insights behind customer reviews")

#The first topic is a collection of words that appear to be connected with the overall positive feedback
  #with dresses (including skirt) with words like, love, flattering, beautiful, fits and length.  
  #It shows that customers love the overall aesthetics of the dress. 

#The 2nd topic is a collection of words that appear to be connected with the overall positive feedback 
  #with tops with words like sleeve, shirt, soft, fabric and colors. It also shows that the customers who reviewed
  #the top collections bought size XS and medium. 

#The 3rd topic is a collection of words that appear to be connected with overall positive feedback 
  #with outerwear with words like sweater and jacket. It also shows that the outerwear matches perfectly with
  #any pairs of pants and jeans that the customers bought. 


