install.packages("janeaustenr")
library(janeaustenr)
library(tidyverse)
install.packages("tidyr")
library(tidyr)
library(dplyr)
library(tm)
library(tidytext)
library(SnowballC)
library(topicmodels)

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

## Tokenization, removing stop words and stemming words
tidy_Mansfieldpark_df <- Mansfieldpark_df %>% 
        unnest_tokens(input=text,output=word,token="words")%>%
        anti_join(stop_words) %>% 
        mutate(word=wordStem(word)) %>%
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
        mutate(word=wordStem(word)) %>% count(chapter,word,sort=T)
head(tidy_Mansfieldpark_df)

## Cast the Document-termed Matrix for modeling
dtm <- tidy_Mansfieldpark_df %>%
        cast_dtm(document=chapter,term=word,value=n)
## Dimension = 48x4886; sparsity=88%
glimpse(dtm)
dtm

## LDA model with 4 topics
mod <- LDA(dtm,k=4,method="Gibbs",control=list(alpha=1,seed=10000))

## Examine the predetermined four topics
terms(mod,k=4,threshold=0.0075)

## Probabilities of topics in documents(chapters)
lda_gamma <- tidy(mod,matrix="gamma") %>% mutate(document=as.numeric(document)) %>%
                ggplot(aes(x=document,y=gamma))+geom_line(aes(color=factor(topic)))+
                scale_color_manual(values=brewer.pal(n=4, "Set1"), name="Topic")
lda_gamma

## Probabilities of words in topics (1/3)
lda_beta <- tidy(mod,matrix="beta") 
lda_beta

## Probabilities of words in topics (2/3): Top 10 terms for each topic
top10 <- lda_beta %>% group_by(topic) %>% top_n(10,beta) %>% ungroup()%>%
        arrange(topic,-beta)
top10

## Probabilities of words in topics (3/3): Visualization
top10 %>% mutate(term=reorder(term,beta)) %>% ggplot(aes(x=term,y=beta))+
                geom_col(aes(fill=as.factor(topic)),position=position_dodge())+
                facet_wrap(~topic,scales="free")+ coord_flip()

## Improvement: finding the best k
mod_perplexity <- vector(mode="list",length=10)
for (i in 2:10) {  
        mod <- LDA(dtm, k=i, method="Gibbs", control=list(alpha=0.5, iter=1000, seed=10000, thin=1))  
        mod_perplexity[i] <-  perplexity(mod, dtm)
}
mod_perplexity

## The "elbow point" appears when k=5       
x <- c(2:10)
y <- mod_perplexity[2:10]
plot(x,y,xlab="number of topics",ylab="perplexity score",type="o")

## Resumed run with k=5
new_mod <- LDA(dtm, k=5, model = mod,control=list(alpha=1,seed=10000,thin=1, iter=100))
perplexity(new_mod,dtm)  

## Short conclusion: Topic 3 mainly around our heroin Fanni is clearly in the chapters 30-40,
## while topic 2 depicts the life situation in Mansfield Park, topic 1 is around Mr. Rushworth
## and topic 4 the interactions of all the minor characters.





                




