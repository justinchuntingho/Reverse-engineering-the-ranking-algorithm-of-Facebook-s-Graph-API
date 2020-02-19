# Title: Bootsrapping Approach to Analyse the Bias of Facebook's Graph API
# Author: Justin Chun-ting Ho (Justin.Ho@ed.ac.uk)
# Last Updated: 19 February 2020


########################## 1. Likes, Comments, and Shares ############################## 
set.seed(6080)

################################### Loading Datasets ################################### 
golden <- read.csv("data/golden.csv", stringsAsFactors = FALSE)
golden$date <- as.Date(golden$date)

api.sample <- read.csv("data/api_sample.csv", stringsAsFactors = FALSE)
api.sample$date <- as.Date(api.sample$date)

################################### Loading Required Packages ################################### 
library(lubridate)
library(dplyr)
library(rlang)
library(magrittr)
library(tidyr)
library(reshape2)
library(ggplot2)
library(gridExtra)

################################### Defining Functions ################################### 
rowMedian <- function(x, na.rm = FALSE) apply(x, 1, median, na.rm = na.rm) 
rowSD <- function(x, na.rm = FALSE) apply(x, 1, sd, na.rm = na.rm)

get_mean_bydate <- function(data, var){
  quo_var <- enquo(var)
  tib <- data %>% group_by(date) %>% summarise(mean = mean(!! quo_var, na.rm = TRUE))
  return(tib)
}

get_median_bydate <- function(data, var){
  quo_var <- enquo(var)
  tib <- data %>% group_by(date) %>% summarise(median = median(!! quo_var, na.rm = TRUE))
  return(tib)
}

get_mean <- function(data, var){
  quo_var <- enquo(var)
  tib <- data %>% summarise(mean = mean(!! quo_var, na.rm = TRUE))
  return(tib)
}

get_median <- function(data, var){
  quo_var <- enquo(var)
  tib <- data %>% summarise(median = median(!! quo_var, na.rm = TRUE))
  return(tib)
}

get_SD <- function(data, var){
  quo_var <- enquo(var)
  tib <- data %>% summarise(sd = sd(!! quo_var, na.rm = TRUE))
  return(tib)
}

get_samples_stat <- function(data, var, fun = get_mean){
  quo_var <- enquo(var)
  quo_fun <- enquo(fun)
  value.list <- lapply(data, quo_text(quo_fun), var = (!! quo_var)) %>% unlist()
  return(value.list) # Return the df
}

# Get standardised score
get_score <- function(data, var){
  quo_var <- enquo(var)
  # Calculate mean of all times
  mean.of.all <- data %>% extract2(quo_text(quo_var)) %>% mean()
  # Calculate sd of all times
  sd.of.all <- data %>% extract2(quo_text(quo_var)) %>% sd()
  # Calculate std.score for every time point 
  tib <- data %>% group_by(date) %>% summarise(score = (mean(!! quo_var) - mean.of.all)/sd.of.all)
  return(tib)
}

# get df contains of score, this one calls the get_score function
# Row = each day, Col = bootstrape samples
get_daily_df <- function(data, var, fun = get_score){
  quo_var <- enquo(var)
  quo_fun <- enquo(fun)
  df.list <- lapply(data, quo_text(quo_fun), var = (!! quo_var))
  tb <- tibble(unique(golden$date)) # Getting all the dates from Golden
  colnames(tb) <- "date"
  for (i in df.list){
    tb <- left_join(tb, i, by = "date")
  }
  colnames(tb) <- c("date", 1:(ncol(tb)-1))
  return(tb) # Return the df
}

# Return mean, median, sd of samples score by date
get_daily_stat <- function(df){
  df <- data.frame(date=df[,1], 
             mean=rowMeans(df[,-1], na.rm = TRUE), 
             median=rowMedian(df[,-1], na.rm=TRUE),
             sd = rowSD(df[,-1], na.rm=TRUE))
  return(df)
}
get_daily_stat(testing.tb)

# This will create two dfs, one score df and one daily stat
get_daily_all <- function(data, var){
  quo_var <- enquo(var)
  df <- get_daily_df(data, !! quo_var)
  assign(paste(quo_text(quo_var), "df", sep="."), df, envir=globalenv())
  assign(paste(quo_text(quo_var), "dailystat", sep="."), get_daily_stat(df), envir=globalenv())
  cat(paste("Created objects:", paste(quo_text(quo_var), "df", sep="."), paste(quo_text(quo_var), "dailystat", sep="."), sep = " "))
}

# Row = one bootstrape sample
transpose_df <- function(df){
  t.df <- as_tibble(cbind(t(df[,-1])))
  colnames(t.df) <- df[,1]
  return(t.df)
}

count_type <- function(data){
  data %>% extract2("type") %>% plyr::count()
}

get_type <- function(data){
  df.list <- lapply(data, count_type)
  tb <- tibble(c("event", "link", "note", "photo", "status", "video"))
  colnames(tb) <- "x"
  for (i in df.list){
    tb <- left_join(tb, i, by = "x")
  }
  tb <- mutate_if(tb, is.numeric, funs(ifelse(is.na(.), 0, .)))
  colnames(tb) <- c("type", 1:(ncol(tb)-1))
  t.tb <- transpose_df(tb)
  colnames(t.tb) <- c("event", "link", "note", "photo", "status", "video")
  return(t.tb) # Return the df
}

############################################ Bootstrapping ###########################################

# Generating Bootstrapping samples
bootsamples <- lapply(1:1000, function(i) sample_n(golden, 598, replace = FALSE))

# Setting colours for the plots
col.test <- "#D55E00"
col.nor <- "#009E73"
col.ci <- "#0072B2"

# Simple analyis
get_mean(golden, shares_count_fb)
get_mean(api.sample, shares_count_fb)
get_median(golden, shares_count_fb)
get_median(api.sample, shares_count_fb)
get_SD(golden, likes_count_fb)
get_SD(api.sample, likes_count_fb)

# Get mean likes of samples
samples.like <- get_samples_stat(bootsamples, likes_count_fb)
samples.share <- get_samples_stat(bootsamples, shares_count_fb)
samples.comment <- get_samples_stat(bootsamples, comments_count_fb)

golden$likes_count_fb %>% mean()
golden$shares_count_fb %>% mean()
golden$comments_count_fb %>% mean()

api.like.mean <- get_mean(api.sample, likes_count_fb) %>% as.numeric()
api.share.mean <- get_mean(api.sample, shares_count_fb) %>% as.numeric()
api.comment.mean <- get_mean(api.sample, comments_count_fb) %>% as.numeric()

count.mean.df <-data.frame(c(golden$likes_count_fb %>% mean(), api.like.mean), 
                           c(golden$comments_count_fb %>% mean(),api.comment.mean),
                           c(golden$shares_count_fb %>% mean(), api.share.mean))
rownames(count.mean.df) <- c("Full Data", "Partial Data")
colnames(count.mean.df) <- c("Like", "Comment", "Share")

# shares_count_fb
plot.like <- qplot(samples.like, geom="blank") +
  stat_function(aes(), colour = col.nor, fun = dnorm, args = list(mean = mean(samples.like), sd = sd(samples.like))) +
  geom_histogram(aes(y = ..density..), binwidth = 5, alpha = 0.4) +
  geom_vline(xintercept = mean(samples.like) + sd(samples.like)*3, linetype = "dotted", color=col.ci, alpha=0.8) +
  geom_vline(xintercept = mean(samples.like) - sd(samples.like)*3, linetype = "dotted", color=col.ci, alpha=0.8) +
  geom_vline(xintercept = mean(samples.like) + sd(samples.like)*2, linetype = "dotted", color=col.ci, alpha=0.8) +
  geom_vline(xintercept = mean(samples.like) - sd(samples.like)*2, linetype = "dotted", color=col.ci, alpha=0.8) +
  geom_vline(xintercept = api.like.mean, linetype = "dashed", color=col.test, alpha=0.8) +
  annotate("text", x = c(api.like.mean, mean(samples.like) + sd(samples.like)*2, mean(samples.like) - sd(samples.like)*2, mean(samples.like) + sd(samples.like)*3, mean(samples.like) - sd(samples.like)*3), 
           y=0, color=c(col.test, rep(col.ci, 4)), label = c("Partial Data", "2σ", "2σ", "3σ", "3σ"), angle = 90, alpha=1, hjust = -0.1, vjust = 0) +
  labs(x = "Mean", y = "Density", title = "Likes") + theme_bw() + theme(plot.title = element_text(hjust = 0.5))

plot.comment <- qplot(samples.comment, geom="blank") +
  stat_function(aes(), colour = col.nor, fun = dnorm, args = list(mean = mean(samples.comment), sd = sd(samples.comment))) +
  geom_histogram(aes(y = ..density..), binwidth = 1, alpha = 0.4) +
  geom_vline(xintercept = mean(samples.comment) + sd(samples.comment)*3, linetype = "dotted", color=col.ci, alpha=0.8) +
  geom_vline(xintercept = mean(samples.comment) - sd(samples.comment)*3, linetype = "dotted", color=col.ci, alpha=0.8) +
  geom_vline(xintercept = mean(samples.comment) + sd(samples.comment)*2, linetype = "dotted", color=col.ci, alpha=0.8) +
  geom_vline(xintercept = mean(samples.comment) - sd(samples.comment)*2, linetype = "dotted", color=col.ci, alpha=0.8) +
  geom_vline(xintercept = api.comment.mean, linetype = "dashed", color=col.test, alpha=0.8) +
  annotate("text", x = c(api.comment.mean, mean(samples.comment) + sd(samples.comment)*2, mean(samples.comment) - sd(samples.comment)*2, mean(samples.comment) + sd(samples.comment)*3, mean(samples.comment) - sd(samples.comment)*3), 
           y=0, color=c(col.test, rep(col.ci, 4)), label = c("Partial Data", "2σ", "2σ", "3σ", "3σ"), angle = 90, alpha=1, hjust = -0.1, vjust = 0) +
  labs(x = "Mean", y = "Density", title = "Comments") + theme_bw() + theme(plot.title = element_text(hjust = 0.5)) 

plot.share <- qplot(samples.share, geom="blank") +
  stat_function(aes(), colour = col.nor, fun = dnorm, args = list(mean = mean(samples.share), sd = sd(samples.share))) +
  geom_histogram(aes(y = ..density..), binwidth = 5, alpha = 0.4) +
  geom_vline(xintercept = mean(samples.share) + sd(samples.share)*3, linetype = "dotted", color=col.ci, alpha=0.8) +
  geom_vline(xintercept = mean(samples.share) - sd(samples.share)*3, linetype = "dotted", color=col.ci, alpha=0.8) +
  geom_vline(xintercept = mean(samples.share) + sd(samples.share)*2, linetype = "dotted", color=col.ci, alpha=0.8) +
  geom_vline(xintercept = mean(samples.share) - sd(samples.share)*2, linetype = "dotted", color=col.ci, alpha=0.8) +
  geom_vline(xintercept = api.share.mean, linetype = "dashed", color=col.test, alpha=0.8) +
  annotate("text", x = c(api.share.mean, mean(samples.share) + sd(samples.share)*2, mean(samples.share) - sd(samples.share)*2, mean(samples.share) + sd(samples.share)*3, mean(samples.share) - sd(samples.share)*3), 
           y=0, color=c(col.test, rep(col.ci, 4)), label = c("Partial Data", "2σ", "2σ", "3σ", "3σ"), angle = 90, alpha=1, hjust = -0.1, vjust = 0) +
  labs(x = "Mean", y = "Density", title = "Shares") + theme_bw() + theme(plot.title = element_text(hjust = 0.5)) 

png("post_metrics.png", width = 1000, height = 250, res = 120)
grid.arrange(plot.like, plot.comment, plot.share, ncol=3)
dev.off()



###################### Type ###########################
api.type.df <- count_type(api.sample) %>% transpose_df()
colnames(api.type.df) <- c("event", "link", "note", "photo", "video")
golden.type.df <- count_type(golden) %>% transpose_df()
colnames(golden.type.df) <- c("event", "link", "note", "photo", "status", "video")

type.table <- left_join(count_type(golden), count_type(api.sample), by = "x")
type.table <- transpose_df(type.table) %>% mutate_if(is.numeric, funs(ifelse(is.na(.), 0, .)))
rownames(type.table) <- c("Full Data", "Partial Data")
type.table %>% knitr::kable()

type.df <- get_type(bootsamples)

plot.photo <- qplot(type.df$photo, geom="blank") +
  stat_function(aes(), colour = col.nor, fun = dnorm, args = list(mean = mean(type.df$photo), sd = sd(type.df$photo))) +
  geom_histogram(aes(y = ..density..), binwidth = 1, alpha = 0.4) +
  geom_vline(xintercept = mean(type.df$photo) + sd(type.df$photo)*3, linetype = "dotted", color=col.ci, alpha=0.8) +
  geom_vline(xintercept = mean(type.df$photo) - sd(type.df$photo)*3, linetype = "dotted", color=col.ci, alpha=0.8) +
  geom_vline(xintercept = mean(type.df$photo) + sd(type.df$photo)*2, linetype = "dotted", color=col.ci, alpha=0.8) +
  geom_vline(xintercept = mean(type.df$photo) - sd(type.df$photo)*2, linetype = "dotted", color=col.ci, alpha=0.8) +
  geom_vline(xintercept = mean(api.type.df$photo), linetype = "dashed", color=col.test, alpha=0.8) +
  annotate("text", x = c(mean(api.type.df$photo), mean(type.df$photo) + sd(type.df$photo)*2, mean(type.df$photo) - sd(type.df$photo)*2, 
                         mean(type.df$photo) + sd(type.df$photo)*3, mean(type.df$photo) - sd(type.df$photo)*3), 
           y=0, color=c(col.test, rep(col.ci, 4)), label = c("Partial Data", "2σ", "2σ", "3σ", "3σ"), angle = 90, alpha=1, hjust = -0.1, vjust = 0) +
  labs(x = "Count", y = "Density", title = "Photo") + theme_bw() + theme(plot.title = element_text(hjust = 0.5))

plot.video <- qplot(type.df$video, geom="blank") +
  stat_function(aes(), colour = col.nor, fun = dnorm, args = list(mean = mean(type.df$video), sd = sd(type.df$video))) +
  geom_histogram(aes(y = ..density..), binwidth = 1, alpha = 0.4) +
  geom_vline(xintercept = mean(type.df$video) + sd(type.df$video)*3, linetype = "dotted", color=col.ci, alpha=0.8) +
  geom_vline(xintercept = mean(type.df$video) - sd(type.df$video)*3, linetype = "dotted", color=col.ci, alpha=0.8) +
  geom_vline(xintercept = mean(type.df$video) + sd(type.df$video)*2, linetype = "dotted", color=col.ci, alpha=0.8) +
  geom_vline(xintercept = mean(type.df$video) - sd(type.df$video)*2, linetype = "dotted", color=col.ci, alpha=0.8) +
  geom_vline(xintercept = api.type.df$video, linetype = "dashed", color=col.test, alpha=0.8) +
  annotate("text", x = c(mean(api.type.df$video), mean(type.df$video) + sd(type.df$video)*2, mean(type.df$video) - sd(type.df$video)*2, mean(type.df$video) + sd(type.df$video)*3, mean(type.df$video) - sd(type.df$video)*3), 
           y=0, color=c(col.test, rep(col.ci, 4)), label = c("Partial Data", "2σ", "2σ", "3σ", "3σ"), angle = 90, alpha=1, hjust = -0.1, vjust = 0) +
  labs(x = "Count", y = "Density", title = "Video") + theme_bw() + theme(plot.title = element_text(hjust = 0.5))

plot.link <- qplot(type.df$link, geom="blank") +
  stat_function(aes(), colour = col.nor, fun = dnorm, args = list(mean = mean(type.df$link), sd = sd(type.df$link))) +
  geom_histogram(aes(y = ..density..), binwidth = 1, alpha = 0.4) +
  geom_vline(xintercept = mean(type.df$link) + sd(type.df$link)*3, linetype = "dotted", color=col.ci, alpha=0.8) +
  geom_vline(xintercept = mean(type.df$link) - sd(type.df$link)*3, linetype = "dotted", color=col.ci, alpha=0.8) +
  geom_vline(xintercept = mean(type.df$link) + sd(type.df$link)*2, linetype = "dotted", color=col.ci, alpha=0.8) +
  geom_vline(xintercept = mean(type.df$link) - sd(type.df$link)*2, linetype = "dotted", color=col.ci, alpha=0.8) +
  geom_vline(xintercept = api.type.df$link, linetype = "dashed", color=col.test, alpha=0.8) +
  annotate("text", x = c(mean(api.type.df$link), mean(type.df$link) + sd(type.df$link)*2, mean(type.df$link) - sd(type.df$link)*2, mean(type.df$link) + sd(type.df$link)*3, mean(type.df$link) - sd(type.df$link)*3), 
           y=0, color=c(col.test, rep(col.ci, 4)), label = c("Partial Data", "2σ", "2σ", "3σ", "3σ"), angle = 90, alpha=1, hjust = -0.1, vjust = 0) +
  labs(x = "Count", y = "Density", title = "Link") + theme_bw() + theme(plot.title = element_text(hjust = 0.5)) 

library(gridExtra)
png("post_type.png", width = 1000, height = 250, res = 120)
grid.arrange(plot.photo, plot.video, plot.link, ncol=3)
dev.off()

############################# 2. Top Term Analysis ################################# 

################## Loading Packages and Defining Functions ######################### 
library(quanteda)
library(Kendall)

get_topfeat_count <- function(data){
  dfm <- dfm(corpus(data, text_field = "post_message"), tolower = TRUE, stem = TRUE, remove = c(stopwords("en"), "s", "aspx"),
             remove_numbers = TRUE, remove_punct = TRUE, remove_symbols = TRUE, remove_hyphens = TRUE, remove_url = TRUE)
  topfeat.count <- topfeatures(dfm, 10000)
  return(topfeat.count)
}

xy_to_kendall <- function(xy, k){
  top.n <- filter(xy, rank.x <= k | rank.y <= k)
  top.n$rank.x[top.n$rank.x > k] <- k + 1
  top.n$rank.y[top.n$rank.y > k] <- k + 1
  top.n
  Kendall(top.n$rank.x, top.n$rank.y)
}

api.topfeat <- get_topfeat_count(api.sample) %>% names()
golden.topfeat <- get_topfeat_count(golden) %>% names()

# Creating the ranks, 10 for each category
# Getting the stats for Api.sample v golden
rank <- c(1:3000) %>% sort()

x <- data.frame(golden.topfeat, rank[1:length(golden.topfeat)])
y <- data.frame(api.topfeat, rank[1:length(api.topfeat)])
colnames(x) <- c("word", "rank")
colnames(y) <- c("word", "rank")
x.y <- full_join(x, y, by = "word")
row.names(x.y) <- x.y[,1]
x.y <- as.data.frame(x.y[,-1])

api.kendall.df <- c()
for (i in 1:length(k)){
  kendall.result <- xy_to_kendall(x.y, k[i])
  api.kendall.df <- rbind(api.kendall.df, c(i, k[i], kendall.result$tau, kendall.result$sl))
}
colnames(api.kendall.df) <- c("bootsample","top_n","tau", "p.value")
api.kendall.df <- as.data.frame(api.kendall.df)

# Defining the steps
k <- c(1:180 * 10)
get_kendall <- function(bootsample, k){
  # Create topfeat for golden
  rank <- c(1:3000)
  x <- data.frame(golden.topfeat, rank[1:length(golden.topfeat)])
  colnames(x) <- c("word", "rank")
  
  # Loop throught bootsamples
  df <- c()
  for (i in 1:length(bootsample)){
    topfeat <- names(get_topfeat_count(bootsample[[i]]))
    y <- data.frame(topfeat, rank[1:length(topfeat)])
    colnames(y) <- c("word", "rank")
    x.y <- full_join(x, y, by = "word")
    row.names(x.y) <- x.y[,1]
    x.y <- as.data.frame(x.y[,-1])
    print(paste0(i/length(bootsample)*100, "%"))
    # Loop throught each k, xy to kendall
    for (j in 1:length(k)){
      kendall.result <- xy_to_kendall(x.y, k[j])
      df <- rbind(df, c(i, k[j], kendall.result$tau, kendall.result$sl))
      # print(paste0("Finished k: ", k[i]))
    }
  }
  df <- as.data.frame(df)
  colnames(df) <- c("bootsample","top_n","tau", "p.value")
  return(df)
}

# Calculate the scores for each step (it could take a while)
kendall.df <- get_kendall(bootsamples, k)

# Select steps for visualisation
select.n <- c(1:10 * 10,50 * 3:10,1000,1500)
kendall.plot <- kendall.df %>% filter(top_n %in% select.n) %>% group_by(top_n) %>% summarise(value = mean(tau), sd = sd(tau), p.value = mean(p.value), ymax = value+sd*3, ymin = value-sd*3, group = "Bootstrap Samples")
api.kendall.plot <- api.kendall.df %>% filter(top_n %in% select.n) %>% transmute(top_n = top_n, value = tau, sd = 0, ymax = value, ymin = value, group = "Partial Dataset")
combined.plot <- bind_rows(kendall.plot, api.kendall.plot)

png("kendall.png", width = 1000, height = 500, res = 150)
ggplot(combined.plot, aes(x=top_n, y=value, group = group)) + 
  geom_line(aes(color = group),alpha = .5, size = 1) +
  geom_ribbon(aes(ymin=ymin, ymax=ymax), alpha = .2, fill = col.ci) +
  ylim(0,1.1) +
  scale_x_continuous(breaks = c(0, 100, 500, 1000, 1500)) +
  labs(x = "n", y = "τβ", title = "Kendall’s Tau of Top Terms") + 
  theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_color_manual(name = "Dataset", values = c(col.nor, col.test))
dev.off()

######################## Word Cloud ######################## 

golden.dfm <- dfm(corpus(golden, text_field = "post_message"), tolower = TRUE, stem = TRUE, remove = c(stopwords("en"), "s", "aspx"),
           remove_numbers = TRUE, remove_punct = TRUE, remove_symbols = TRUE, remove_hyphens = TRUE, remove_url = TRUE)
api.dfm <- dfm(corpus(api.sample, text_field = "post_message"), tolower = TRUE, stem = TRUE, remove = c(stopwords("en"), "s", "aspx"),
                  remove_numbers = TRUE, remove_punct = TRUE, remove_symbols = TRUE, remove_hyphens = TRUE, remove_url = TRUE)

png("cloud_golden.png", width = 600, height = 600, res = 120)
textplot_wordcloud(golden.dfm, min.freq = 2, max_words = 200, random.order = FALSE,
                   rot.per = .1, color = "black")
dev.off()
png("cloud_api.png", width = 600, height = 600,  res = 120)
textplot_wordcloud(api.dfm, min.freq = 2, max_words = 200, random.order = FALSE,
                   rot.per = .1, color = "black")
dev.off()



