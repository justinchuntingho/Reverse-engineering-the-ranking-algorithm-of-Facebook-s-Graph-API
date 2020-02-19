# Title: Reverse engineering the Ranking Algorithm of Facebook's Graph API
# Author: Justin Chun-ting Ho (Justin.Ho@ed.ac.uk)
# Last Updated: 19 February 2020

set.seed(6080)

################################### Loading Datasets ################################### 
golden <- read.csv("data/golden.csv", stringsAsFactors = FALSE)
golden$date <- as.Date(golden$date)

api.sample <- read.csv("data/api_sample.csv", stringsAsFactors = FALSE)
api.sample$date <- as.Date(api.sample$date)

library(dplyr)
library(qwraps2)
library(stargazer)


# Identifying which posts are chosen by the API
chosen <- golden$post_id %in% api.sample$post_id
golden$chosen <- 0
golden[chosen, ]$chosen <- 1

# Selecting only photo, link, and videos
golden <- golden[!(golden$type %in% "event"),]
golden <-  golden[!(golden$type %in% "note"),]
golden <-  golden[!(golden$type %in% "status"),]

# Setting up for the logisitic regression
golden$chosen <- factor(golden$chosen)
golden$type <- factor(golden$type)
golden$type <- relevel(golden$type, "link")

# Selecting posts between 31 January 2016 and 1 January 2017
raw_reaction <- golden %>% filter(date > "2016-01-31") 
raw_reaction <- raw_reaction %>% filter(date < "2017-01-01") 

raw_reaction <- raw_reaction %>% filter(date < "2017-01-01") 
raw_reaction[is.na(raw_reaction)] <- 0

desc.stat <- raw_reaction %>% group_by(chosen) %>% summarise(Comments = mean_sd(comments_count_fb, denote_sd="paren"),
                                                             Comment.Likes = mean_sd(comment_likes_count, denote_sd="paren"),
                                                             Shares = mean_sd(shares_count_fb, denote_sd="paren"),
                                                             Likes = mean_sd(rea_LIKE, denote_sd="paren"),
                                                             Love = mean_sd(rea_LOVE, denote_sd="paren"),
                                                             Wow = mean_sd(rea_WOW, denote_sd="paren"),
                                                             Haha = mean_sd(rea_HAHA, denote_sd="paren"),
                                                             Sad = mean_sd(rea_SAD, denote_sd="paren"),
                                                             Angry = mean_sd(rea_ANGRY, denote_sd="paren"))#
stargazer(t(desc.stat), no.space=TRUE,  single.row=TRUE, title = "Descriptive Statsitics")

raw_reaction$log.comments <- log(raw_reaction$comments_count_fb+1, 2)
raw_reaction$log.comlikes <- log(raw_reaction$comment_likes_count+1, 2)
raw_reaction$log.shares <- log(raw_reaction$shares_count_fb+1, 2)
raw_reaction$log.likes <- log(raw_reaction$rea_LIKE+1, 2)
raw_reaction$log.love <- log(raw_reaction$rea_LOVE+1, 2)
raw_reaction$log.wow <- log(raw_reaction$rea_WOW+1, 2)
raw_reaction$log.haha <- log(raw_reaction$rea_HAHA+1, 2)
raw_reaction$log.sad <- log(raw_reaction$rea_SAD+1, 2)
raw_reaction$log.angry <- log(raw_reaction$rea_ANGRY+1, 2)

model0 <- glm(chosen ~ 1, data = raw_reaction, family = "binomial") # null model
model_baseline <- glm(chosen ~ type + log.comments + 
                        log.shares +
                        log.likes, 
                      data = raw_reaction, family = "binomial") # Broken down
model_full <- glm(chosen ~ type + log.comments + log.comlikes + 
                    log.shares +
                    log.likes + log.love + log.wow +log.haha + log.sad +
                    log.angry, 
                  data = raw_reaction, family = "binomial") # Broken down

stargazer(model_baseline, model_full)

# Likelihood Ratio Test
library(lmtest)
lrtest(model0, model_baseline, model_full)

# Pseudo R^2
library(pscl)
pR2(model_full)
pR2(model_baseline)

# Classification Rate (confusion matrix)
fitted.results <- predict(model_full,type='response')
fitted.results <- ifelse(fitted.results > 0.5,1,0)
misClasificError <- mean(fitted.results != raw_reaction$chosen)
print(paste('Accuracy',1-misClasificError))

library(caret)
conmat <- confusionMatrix(data=as.factor(fitted.results), as.factor(raw_reaction$chosen))
conmat

ConfMat <- as.data.frame.matrix(conmat$table)
stargazer(ConfMat, head = TRUE, title = "Table", summary = FALSE)

# ROC Curve
library(ROCR)
p <- predict(model_full, type="response")
pr <- prediction(p, raw_reaction$chosen)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf)

# AUC
auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc
