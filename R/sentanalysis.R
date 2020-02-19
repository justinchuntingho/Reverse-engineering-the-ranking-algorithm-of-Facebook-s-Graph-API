# Title: Comparing the Sentiment between Selected and Non-selected Posts
# Author: Justin Chun-ting Ho (Justin.Ho@ed.ac.uk)
# Last Updated: 19 February 2020

################################### Loading Packages ################################### 
library(readtext)
library(quanteda)
library(magrittr)
library(stringr)
library(ggplot2)
library(scales)
library(tidytext)
library(tidyr)
library(dplyr)
library(qwraps2)
library(stargazer)
quanteda_options("verbose" = TRUE) 
Sys.setenv(TZ="Europe/London")

################################### Loading Datasets ################################### 
golden <- read.csv("data/golden.csv", stringsAsFactors = FALSE)
golden$date <- as.Date(golden$date)

api.sample <- read.csv("data/api_sample.csv", stringsAsFactors = FALSE)
api.sample$date <- as.Date(api.sample$date)

# Identifying which posts are chosen by the API
chosen <- golden$post_id %in% api.sample$post_id
golden$chosen <- 0
golden[chosen, ]$chosen <- 1

# Preparing the Text Corpus
snptext <- golden
snptext$text <- str_replace_all(snptext$post_message, "^Scottish National Party \\(SNP\\).*", "")
custom_stopwords <- c("facebook", "s")
snp <-  corpus(snptext)
docvars(snp, "ntoken") <- ntoken(snp)
docvars(snp, "doc_name") <- docnames(snp)
snptokens <- tokens(snp, remove_punct = TRUE, remove_numbers = TRUE, verbose = TRUE, remove_url = TRUE)
snpdfm <- dfm(snptokens, remove = c(stopwords('english'), custom_stopwords), stem = FALSE)
docvar <- docvars(snp)

###################################### Sentiment Analysis #############################################

# Getting dictionaries
bing <- get_sentiments("bing")
bingpos <- bing$word[bing$sentiment == "positive"]
bingneg <- bing$word[bing$sentiment == "negative"]
bingdict <- dictionary(list(positive = bingpos,
                            negative = bingneg))
sentdfm <- dfm(snpdfm, dictionary = bingdict)
sentmat <- as.data.frame(sentdfm)
sentmat["sentiment"] <- sentmat$positive - sentmat$negative
sentmat["emotionwords"] <- sentmat$positive + sentmat$negative

# Combining back to the docvar
docvar <- cbind(docvar, sentmat)
docsent <- select(docvar, post_id, positive, negative, sentiment, emotionwords)
docsent[is.na(docsent)] <- 0
sentiment_df <- left_join(golden, docsent, by = "post_id")

# # This bit isn't working since the data file is too big for Github
# snpcomtext <-  readtext("~/GitHub/SNPvUKIP Keyness/snp_comments.csv", text_field = "comment_message")
# snpaggcomtext <- aggregate(text ~ post_id + post_published, data = snpcomtext, paste, collapse = "...") %>% as.data.frame()
# rm(snpcomtext)
# aggcomcorpus <- corpus(snpaggcomtext)
# docnames(aggcomcorpus) <- docvars(aggcomcorpus, "post_id")
# docvars(aggcomcorpus, "comntoken") <- ntoken(aggcomcorpus)
# aggcomdfm <- tokens(aggcomcorpus, remove_punct = TRUE, remove_numbers = TRUE, remove_twitter = TRUE, verbose = TRUE, remove_url = TRUE) %>%
#   dfm(remove = c(stopwords('english'), custom_stopwords), stem = FALSE)
# 
# sentcomdfm <- dfm(aggcomdfm, dictionary = bingdict)
# 
# sentcommat <- as.data.frame(sentcomdfm)
# sentcommat["comsentiment"] <- sentcommat$positive - sentcommat$negative
# sentcommat["comemotionwords"] <- sentcommat$positive + sentcommat$negative
# sentcommat["post_id"] <- sentcommat["document"]
# sentcommat["compositive"] <- sentcommat["positive"]
# sentcommat["comnegative"] <- sentcommat["negative"]
# 
# sentcommat <- select(sentcommat, post_id, compositive, comnegative, comsentiment, comemotionwords)
# sentcommat[is.na(sentcommat)] <- 0
# saveRDS(sentcommat, "data/comments_sentiment.rds")

# I have saved the output from the previous chunck as a rds, read this instead
sentcommat <- readRDS("data/comments_sentiment.rds")

sentiment_df <- left_join(sentiment_df, sentcommat, by = "post_id")
sentiment_df[is.na(sentiment_df)] <- 0
sentiment_df_only2016 <- sentiment_df %>% filter(date > "2015-12-31") 
sentiment_df_only2016 <- sentiment_df_only2016 %>% filter(date < "2017-01-01") 

################################### Mann–Whitney U test ################################### 
sent.desc.stat <- raw_full_withsent_2016 %>% group_by(chosen) %>% summarise(Sentiment = mean_sd(sentiment, denote_sd="paren"),
                                                             Emotion = mean_sd(emotionwords, denote_sd="paren"),
                                                             Com.Sentiment = mean_sd(comsentiment, denote_sd="paren", na_rm = TRUE),
                                                             Com.Emotion = mean_sd(comemotionwords, denote_sd="paren", na_rm = TRUE))#
(sent.desc.stat <- t(sent.desc.stat))

sent.desc.mdn <- raw_full_withsent_2016 %>% group_by(chosen) %>% summarise(Sentiment = median(sentiment),
                                                                  Emotion = median(emotionwords),
                                                                  Com.Sentiment = median(comsentiment, na_rm = TRUE),
                                                                  Com.Emotion = median(comemotionwords, na_rm = TRUE))#
(sent.desc.stat <- t(sent.desc.mdn))

# Descriptive Statistics
stargazer(sent.desc.stat, no.space=TRUE,  single.row=TRUE, title = "Descriptive Statsitics")

# Mann–Whitney U test
wilcox.test(raw_full_withsent_2016[raw_full_withsent_2016$chosen == 0,]$sentiment, 
            raw_full_withsent_2016[raw_full_withsent_2016$chosen == 1,]$sentiment, paired=FALSE) 
wilcox.test(raw_full_withsent_2016[raw_full_withsent_2016$chosen == 0,]$comsentiment, 
            raw_full_withsent_2016[raw_full_withsent_2016$chosen == 1,]$comsentiment, paired=FALSE) 
wilcox.test(raw_full_withsent_2016[raw_full_withsent_2016$chosen == 0,]$emotionwords, 
            raw_full_withsent_2016[raw_full_withsent_2016$chosen == 1,]$emotionwords, paired=FALSE) 
wilcox.test(raw_full_withsent_2016[raw_full_withsent_2016$chosen == 0,]$comemotionwords, 
            raw_full_withsent_2016[raw_full_withsent_2016$chosen == 1,]$comemotionwords, paired=FALSE) 



