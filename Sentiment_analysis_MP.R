install.packages("janeaustenr")
library(janeaustenr)
library(tidyverse)
install.packages("tidyr")
library(tidyr)
library(dplyr)
library(tm)
library(tidytext)
library(SnowballC)
library(textdata)

jane_book <- as.data.frame(austen_books(),stringAsFactors=F)
head(jane_book)
summary(jane_book)

## Choose the book "Mansfield Park" and add the "chapter" column using str_detect()
Mansfieldpark_df <- jane_book %>% group_by(book) %>% 
        filter(book=="Mansfield Park") %>%
        mutate(chapter=cumsum(str_detect(text,regex("^chapter",ignore_case=T)))) %>%
        ungroup() %>%
        filter(chapter>0)
head(Mansfieldpark_df)
tail(Mansfieldpark_df)

## Further cleaning the data frame by removing empty rows and rows containing "CHAPTER X" and "THE END"
Mansfieldpark_df <- Mansfieldpark_df %>% filter(text!="") %>% 
        filter(text!="THE END") %>% filter(!str_detect(text,"CHAPTER"))
## Dimension is now 13358x3
dim(Mansfieldpark_df) 

tidy_Mansfieldpark_df <- Mansfieldpark_df %>% 
        unnest_tokens(input=text,output=word,token="words")%>%
        anti_join(stop_words) %>% 
        count(chapter,word,sort=T) 
tidy_Mansfieldpark_df

## Customize the "stop words"-dictionary according to the project
custom <- add_row(stop_words,word="miss",lexicon="custom")
custom <- add_row(custom,word="sir",lexicon="custom")
custom <- add_row(custom,word="english",lexicon="custom")

## Tokenization, removing customized stop words and stemming words
tidy_Mansfieldpark_df <- Mansfieldpark_df %>% 
        unnest_tokens(input=text,output=word,token="words",drop=T,to_lower=T)%>%
        anti_join(custom) %>% 
        count(chapter,word,sort=T)
head(tidy_Mansfieldpark_df)

## Sentiment analysis using "afinn" lexicon
tidy_Mansfieldpark_df %>% 
        inner_join(get_sentiments("afinn")) %>%
        group_by(chapter)%>%
        summarise(sentiment=sum(value)) %>%
        arrange(-sentiment)

## Sentiment analysis using "bing" lexicon with the ratio of negative words in each chapter
total_word <- tidy_Mansfieldpark_df %>%
                group_by(chapter) %>%
                count()
tidy_Mansfieldpark_df %>%
        inner_join(get_sentiments("bing")) %>%
        group_by(chapter) %>%
        count(sentiment)%>%
        filter(sentiment=="positive")%>%
        transform(p=n/total_word$n)%>%
        arrange(desc(p))
## Sentiment analysis using "nrc" lexicon
as.data.frame(table(get_sentiments("nrc")$sentiment))

## 371 words expressing anger
anger <- get_sentiments("nrc") %>%
        filter(sentiment=="anger")
tidy_Mansfieldpark_df %>%
        inner_join(anger) %>%
        count(word,sort=T) 
 
## 875 words expressing negative sentiment
negative <- get_sentiments("nrc") %>%
        filter(sentiment=="negative")      
tidy_Mansfieldpark_df %>%
        inner_join(negative) %>%
        count(word,sort=T)

## 946 words expressing positive sentiment
positive <- get_sentiments("nrc") %>%
        filter(sentiment=="positive")      
tidy_Mansfieldpark_df %>%
        inner_join(positive) %>%
        count(word,sort=T)

## Short conclusion: In "Mansfield Park", there are more words with positive sentiment used than those with negative.
## In the chapters 40+, more negative descriptions have been detected, while in the chapters 30-35 more positive. 
## The last chapter (48) is evaluated as a little bit more positive than negative.
