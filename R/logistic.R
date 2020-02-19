raw_full <- read.csv("data/snp_fullstats_full.tab", sep = "\t", stringsAsFactors = FALSE)
raw_full$date <- as.Date(raw_full$post_published)

raw_sample <- read.csv("data/snp_fullstats_sample.tab", sep = "\t", stringsAsFactors = FALSE)
raw_sample$date <- as.Date(raw_sample$post_published)
str(raw_full)
str(raw_sample)

chosen <- raw_full$post_id %in% raw_sample$post_id

raw_full$chosen <- 0
raw_full[chosen, ]$chosen <- 1

library(dplyr)
raw_full$type <- recode(raw_full$type, photo = "photo", link = "link", video = "video", event = NA, status = NA, note = NA)

raw_full <- raw_full[!(raw_full$type %in% "event"),]
raw_full <-  raw_full[!(raw_full$type %in% "note"),]
raw_full <-  raw_full[!(raw_full$type %in% "status"),]


raw_full$chosen <- factor(raw_full$chosen)
raw_full$type <- factor(raw_full$type)
raw_full$type <- relevel(raw_full$type, "link")

raw_reaction <- raw_full %>% filter(date > "2016-01-31") 
raw_reaction <- raw_reaction %>% filter(date < "2017-01-01") 

raw_reaction <- raw_reaction %>% filter(date < "2017-01-01") 
raw_reaction[is.na(raw_reaction)] <- 0

library(qwraps2)
library(stargazer)
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

t(table(raw_reaction$chosen, raw_reaction$type))

# # Getting Mean SD table
# library(qwraps2)
# mean_sd(raw_full$rea_ANGRY)
# # significant, Comments is the key!

model0 <- glm(chosen ~ 1, data = raw_reaction, family = "binomial") # null model
model1 <- glm(chosen ~ type, data = raw_reaction, family = "binomial") # type model
model2 <- glm(chosen ~ log(likes_count_fb+1) + log(comments_count_fb+1) + log(shares_count_fb+1), 
              data = raw_reaction, family = "binomial") # engagement model
model3 <- glm(chosen ~ type + log(rea_LIKE+1, 2) + log(comments_count_fb+1, 2) + log(shares_count_fb+1, 2), 
              data = raw_reaction, family = "binomial") # full model (likes)
model4 <- glm(chosen ~ type + log(engagement_fb+1) + log(comments_count_fb+1) + log(shares_count_fb+1), 
              data = raw_reaction, family = "binomial") # full model (engagement)
model5 <- glm(chosen ~ type + log(comments_count_fb+1) + log(comment_likes_count+1)+ 
                log(shares_count_fb+1) +
                log(rea_LIKE+1) + log(rea_LOVE+1) + log(rea_WOW+1) +log(rea_HAHA+1) +log(rea_SAD+1) +
                log(rea_ANGRY+1), 
              data = raw_reaction, family = "binomial") # Broken down
model6 <- glm(chosen ~ type + log(comment_likes_count+1)+ 
                log(shares_count_fb+1) +
                log(rea_LIKE+1) + log(rea_ANGRY+1), 
              data = raw_reaction, family = "binomial") # after wald test
model7 <- glm(chosen ~ type + log(comments_count_fb+1) + log(comment_likes_count+1)+ 
                log(shares_count_fb+1) +
                log(rea_LIKE+1) + log(rea_LOVE+1) + log(rea_WOW+1) +log(rea_HAHA+1) +
                log(rea_ANGRY+1), 
              data = raw_reaction, family = "binomial") # Broken down

model8 <- glm(chosen ~ type + log(comments_count_fb+1, 2) + log(comment_likes_count+1, 2)+ 
                log(shares_count_fb+1, 2) +
                log(rea_LIKE+1, 2) + log(rea_LOVE+1, 2) + log(rea_WOW+1, 2) +log(rea_HAHA+1, 2) +log(rea_SAD+1, 2) +
                log(rea_ANGRY+1, 2), 
              data = raw_reaction, family = "binomial") # Broken down

model8nl <- glm(chosen ~ type + comments_count_fb + comment_likes_count + 
                shares_count_fb +
                rea_LIKE + rea_LOVE + rea_WOW + rea_HAHA + rea_SAD +
                rea_ANGRY, 
              data = raw_reaction, family = "binomial") # Broken down

model9 <- glm(chosen ~ type + log(comment_likes_count+1, 2)+ 
                log(shares_count_fb+1, 2) +
                log(rea_LIKE+1, 2) + 
                log(rea_ANGRY+1, 2), 
              data = raw_reaction, family = "binomial") # Broken down

model10 <- glm(chosen ~ type + log(comments_count_fb+1, 2) + log(comment_likes_count+1, 2)+ 
                log(shares_count_fb+1, 2) +
                log(rea_LIKE+1, 2) + log(rea_LOVE+1, 2) + log(rea_WOW+1, 2) +log(rea_HAHA+1, 2) +log(rea_SAD+1, 2) +
                log(rea_ANGRY+1, 2) + sentiment + comsentiment + emotionwords + comemotionwords, 
              data = raw_reaction, family = "binomial") # Broken down

model11 <- glm(chosen ~ type + sentiment + comsentiment + log(emotionwords+1, 2) + log(comemotionwords+1, 2) , 
               data = raw_reaction, family = "binomial") # Broken down

modelsum <- c()
for(i in apropos("model[0-9]")){
  model <- get(i)
  modelsum <- rbind(modelsum, c(AIC(model),BIC(model),logLik(model)))
}
rownames(modelsum) <- apropos("model[0-9]")
colnames(modelsum) <- c("AIC", "BIC", "LogLik")
modelsum

summary(model8)

stargazer(model3, model8, model9, out = "58.html")
stargazer(model0)
stargazer(model5, model8, out = "58.html")

library(jtools)
library(ggplot2)
png("logistic.png", width = 1200, height = 600, res = 150)
plot_summs(model3, model8,
           point.shape = FALSE,
           color.class = "Set1",
           model.names = c("Baseline", "Full"),
           coefs = c("Photo" = "typephoto", 
                     "Video" = "typevideo", 
                     "Likes" = "log(rea_LIKE + 1, 2)", 
                     "Comments" = "log(comments_count_fb + 1, 2)", 
                     "Likes on Comment" = "log(comment_likes_count + 1, 2)", 
                     "Shares" = "log(shares_count_fb + 1, 2)",
                     "Love" = "log(rea_LOVE + 1, 2)", 
                     "Wow" = "log(rea_WOW + 1, 2)", 
                     "Haha" = "log(rea_HAHA + 1, 2)", 
                     "Sad" = "log(rea_SAD + 1, 2)", 
                     "Angry" = "log(rea_ANGRY + 1, 2)")) +
  labs(x = "b", y = "", title = "Estimated Logistic Regression Coefficients") + 
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_continuous(limits = c(-1,4))
dev.off()

plot_summs(model3, model8,
           point.shape = FALSE,
           color.class = "Set1",
           model.names = c("Baseline", "Full"),
           coefs = c("Photo" = "typephoto", 
                     "Video" = "typevideo", 
                     "Likes" = "log(rea_LIKE + 1, 2)", 
                     "Comments" = "log(comments_count_fb + 1, 2)", 
                     "Likes on Comment" = "log(comment_likes_count + 1, 2)", 
                     "Shares" = "log(shares_count_fb + 1, 2)",
                     "Love" = "log(rea_LOVE + 1, 2)", 
                     "Wow" = "log(rea_WOW + 1, 2)", 
                     "Haha" = "log(rea_HAHA + 1, 2)", 
                     "Sad" = "log(rea_SAD + 1, 2)", 
                     "Angry" = "log(rea_ANGRY + 1, 2)")) +
  labs(x = "b", y = "", title = "Estimated Logistic Regression Coefficients") + 
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_continuous(limits = c(-1,4))


anova(model5, test="Chisq")

#newdata <- subset(raw_full,select=c(31,1,13,14,16))

model0$BIC <- BIC(model0)
model3$BIC <- BIC(model3)
model5$BIC <- BIC(model5)
model8$BIC <- BIC(model8)
BIC(model8nl)


library(pscl)

model0$rsq <- pR2(model0)[4]
model3$rsq <- pR2(model3)[4]
model5$rsq <- pR2(model5)[4]


library(stargazer)
stargazer(model0, model3, model5, model8, out = "models.html",  no.space=TRUE,  single.row=TRUE,
          table.layout ="-#t-s=n",
          title ="Lobster linear regression",
          dep.var.caption="", 
          digits =2,
          star.cutoffs = c(.05, .01, 0.001),
          dep.var.labels ="Lobster size",
#          covariate.labels=c("Link", "Video", "Likes", "Love", "Wow", "Haha", "Sad", "Angry", "Comments", "Likes on Comment", "Shares", "Constant"),
          add.lines=list(c("Pseudo R-squared", round(pR2(model0)[4],3), round(pR2(model3)[4],3), round(pR2(model5)[4],3))))

stargazer(model3, model8,  out = "modellatex.html", no.space=TRUE,  single.row=TRUE,
#          table.layout ="-#t-=n",
          omit.table.layout ="ld",
          title ="Estimated Logistic Regression Coefficients",
          column.labels=c("Baseline","Full"),
          digits =2,
          star.cutoffs = c(.05, .01, 0.001),
          covariate.labels=c("Photo", "Video", "Likes", "Love", "Wow", "Haha", "Sad", "Angry", "Comments", "Likes on Comment", "Shares", "Constant"),
          add.lines=list(c("Pseudo R-squared", round(pR2(model3)[4],2), round(pR2(model8)[4],2))))

stargazer(model8, model8nl,  out = "modellatex.html", no.space=TRUE,  single.row=TRUE,
          #          table.layout ="-#t-=n",
          omit.table.layout ="ld",
          title ="Estimated Logistic Regression Coefficients",
          column.labels=c("Baseline","Full"),
          digits =2,
          star.cutoffs = c(.05, .01, 0.001),
          add.lines=list(c("Pseudo R-squared", round(pR2(model8)[4],2), round(pR2(model8nl)[4],2))))


library(ggstatsplot)
ggcoefstats(x = model5, palette = "Paired")

summary(raw_full$rea_ANGRY > 0)

########## Goodness of Fit ########## 
anova(model5, test = "Chisq")

# Likelihood Ratio Test
anova(model0, model5, test = "Chisq")
library(lmtest)
lrtest(model0, model3, model8, model10)
sum(residuals(model5, type = "pearson")^2)

# Pseudo R^2
library(pscl)
pR2(model8)

# Hosmer-Lemeshow Test (Not useful)
library(MKmisc)
HLgof.test(fit = fitted(model5), obs = raw_full$chosen)
library(ResourceSelection)
hoslem.test(raw_reaction$chosen, fitted(model8))


##########  Statistical Tests for Individual Predictors ########## 
# Wald Test
library(survey)
regTermTest(model5, "type")
coefs <- c("type", names(coef(model5))[-1:-3]) # List all coefficients
coef.tab <- c()
for(i in coefs){
  coef.tab<-rbind(coef.tab,regTermTest(model5, i))
}
coef.tab

# Variable Importance
library(caret)
varImp(model5)

##########  Validation of Predicted Values ########## 
# Classification Rate (confusion matrix)
fitted.results <- predict(model10,type='response')
fitted.results <- ifelse(fitted.results > 0.5,1,0)

misClasificError <- mean(fitted.results != raw_reaction$chosen)
print(paste('Accuracy',1-misClasificError))

library(caret)
conmat <- confusionMatrix(data=as.factor(fitted.results), as.factor(raw_reaction$chosen))
conmat

ConfMat <- as.data.frame.matrix(conmat$table)
stargazer(ConfMat, head = TRUE, title = "Table", summary = FALSE)

colnames(ConfMat)


# ROC Curve

library(ROCR)
p <- predict(model8, type="response")
pr <- prediction(p, raw_reaction$chosen)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf)

auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc


raw_reaction$log.comments <- log(raw_reaction$comments_count_fb+1, 2)
raw_reaction$log.comlikes <- log(raw_reaction$comment_likes_count+1, 2)
raw_reaction$log.shares <- log(raw_reaction$shares_count_fb+1, 2)
raw_reaction$log.likes <- log(raw_reaction$rea_LIKE+1, 2)
raw_reaction$log.love <- log(raw_reaction$rea_LOVE+1, 2)
raw_reaction$log.wow <- log(raw_reaction$rea_WOW+1, 2)
raw_reaction$log.haha <- log(raw_reaction$rea_HAHA+1, 2)
raw_reaction$log.sad <- log(raw_reaction$rea_SAD+1, 2)
raw_reaction$log.angry <- log(raw_reaction$rea_ANGRY+1, 2)


model3 <- glm(chosen ~ type + log.comments + 
                log.shares +
                log.likes, 
              data = raw_reaction, family = "binomial") # Broken down

model8 <- glm(chosen ~ type + log.comments + log.comlikes + 
                log.shares +
                log.likes + log.love + log.wow +log.haha + log.sad +
                log.angry, 
              data = raw_reaction, family = "binomial") # Broken down

library(reghelper)

summary(model8)
beta(model3)

library(ggplot2)
raw_reaction %>% group_by(date, chosen) %>% 
  summarise(postperday = n()) %>% 
  ggplot(aes(x = date, y = postperday, color = chosen)) +
  geom_line()


#%>% 
  group_by(chosen) %>% 
  summarise(postpercent = n()/postperday)

