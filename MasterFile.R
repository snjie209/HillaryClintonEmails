setwd("~/Group05")
set.seed(1)
# Load in packages

library(tm) #general text mining package
library(SnowballC) #package for stemming
library(dplyr) #data analysis package, also loads in magrittr for %>% notation

# Load in user functions for analysis

source('FunctionsFile.R')

# Store email length

hil <- read.delim("HRC_train.tsv", sep = "", header = F)

colnames(hil) <- c("sender", "email")

email_length <- nchar(as.character(hil$email))

# We separate the vectors in the emails data frame 
# for use in the model estimations

senders <- hil$sender
emails <- hil$email

# Run Preprocessing function

test.filepath <- "HRC_test.tsv"

train.filepath <- "HRC_train.tsv"

hil.test <- preprocessing(test.filepath,
                          ShowTerms = F,
                          RemoveCommonWords = F) %>%
  as.data.frame()


hil.train <- preprocessing(train.filepath,
                           ShowTerms = T,
                           RemoveCommonWords = F) %>%
  as.data.frame()

# Only include columns in test and training set
# for terms that appear in both document term matrices

IntersectFiles <- FindIntersect(test_emails = hil.test,
                                train_emails = hil.train,
                                return_which = "both")

hil.train <- IntersectFiles$train
hil.test <- IntersectFiles$test

rm(IntersectFiles)

# Append power features 

hil.train <- AddPowerFeatures(hil.train)
hil.test <- AddPowerFeatures(hil.test)

##############################################################
###### Building A Supervised Classifier: Random Forests ###### 
##############################################################


library(randomForest)


# Since the ideal subset size for random forests for a classification 
# is the square root of the total number of predictors, 
# we will do so for this problem

mpred <- (floor(sqrt(ncol(dtm.data))))

##### INSERT FUNCTION THAT APPLIES FitRFClean FUNCTION TO EACH WORD VECTOR #####

rf.full.model <- FitRFClean(words_of_interest = names(hil.train),
                            train_emails = hil.train,
                            senders = senders)

rf.full.model.200 <- FitRFClean(words_of_interest = names(hil.train),
                                train_emails = hil.train,
                                senders = senders,
                                ntree = 200)


rf.full.model.300 <- FitRFClean(words_of_interest = names(hil.train),
                                train_emails = hil.train,
                                senders = senders,
                                ntree = 300)


importance.200 <- data.frame(importance(rf.full.model.200[[1]]),
                             word = rownames(importance(rf.full.model.200[[1]]))) %>%
  arrange(desc(MeanDecreaseGini))
important.words.200 <- as.character(importance.200$word)

importance.100 <- data.frame(importance(rf.full.model[[1]]),
                             word = rownames(importance(rf.full.model[[1]]))) %>%
  arrange(desc(MeanDecreaseGini))
important.words.100 <- as.character(importance.100$word)

importance.300 <- data.frame(importance(rf.full.model.300[[1]]),
                             word = rownames(importance(rf.full.model.300[[1]]))) %>%
  arrange(desc(MeanDecreaseGini))
important.words.300 <- as.character(importance.300$word)

rf.200.1000 <- FitRFClean(words_of_interest = important.words.200[1:1000],
                          train_emails = hil.train,
                          senders = senders,
                          ntree = 200)

rf.200.2000 <- FitRFClean(words_of_interest = important.words.200[1:2000],
                          train_emails = hil.train,
                          senders = senders,
                          ntree = 200)


rf.200.3000 <- FitRFClean(words_of_interest = important.words.200[1:3000],
                          train_emails = hil.train,
                          senders = senders,
                          ntree = 200)

rf.200.4000 <- FitRFClean(words_of_interest = important.words.200[1:3000],
                          train_emails = hil.train,
                          senders = senders,
                          ntree = 200)

rf.200.5000 <- FitRFClean(words_of_interest = important.words.200[1:3000],
                          train_emails = hil.train,
                          senders = senders,
                          ntree = 200)

rf.100.1000 <- FitRFClean(words_of_interest = important.words.100[1:1000],
                          train_emails = hil.train,
                          senders = senders,
                          ntree = 100)

rf.100.2000 <- FitRFClean(words_of_interest = important.words.100[1:2000],
                          train_emails = hil.train,
                          senders = senders,
                          ntree = 100)

rf.100.3000 <- FitRFClean(words_of_interest = important.words.100[1:3000],
                          train_emails = hil.train,
                          senders = senders,
                          ntree = 100)

rf.100.4000 <- FitRFClean(words_of_interest = important.words.100[1:4000],
                          train_emails = hil.train,
                          senders = senders,
                          ntree = 100)


rf.100.5000 <- FitRFClean(words_of_interest = important.words.100[1:5000],
                          train_emails = hil.train,
                          senders = senders,
                          ntree = 100)


rf.300.1000 <- FitRFClean(words_of_interest = important.words.300[1:1000],
                          train_emails = hil.train,
                          senders = senders,
                          ntree = 300)

rf.300.2000 <- FitRFClean(words_of_interest = important.words.300[1:2000],
                          train_emails = hil.train,
                          senders = senders,
                          ntree = 300)

rf.300.3000 <- FitRFClean(words_of_interest = important.words.300[1:3000],
                          train_emails = hil.train,
                          senders = senders,
                          ntree = 300)

rf.300.4000 <- FitRFClean(words_of_interest = important.words.300[1:4000],
                          train_emails = hil.train,
                          senders = senders,
                          ntree = 300)


rf.300.5000 <- FitRFClean(words_of_interest = important.words.300[1:5000],
                          train_emails = hil.train,
                          senders = senders,
                          ntree = 300)


rf_tree_errors <- list(rf.100.1000, rf.100.2000, rf.100.3000, rf.100.4000, rf.100.5000, rf.200.1000, rf.200.2000, rf.200.3000, rf.200.4000, rf.200.5000, rf.300.1000, rf.300.2000, rf.300.3000, rf.300.4000, rf.300.5000, rf.full.model, rf.full.model.200, rf.full.model.300)
rf_oobs <- sapply(rf_tree_errors, function(x) x[[2]])
rf_ntree <- as.factor(sapply(rf_tree_errors, function(x) x[[1]]$ntree))
rf_words <- c(1000, 2000, 3000, 4000, 5000, 1000, 2000, 3000, 4000, 5000,1000, 2000, 3000, 4000, 5000, 8878, 8878, 8878)

rf_plotdata <- data.frame(rf_words, rf_oobs, rf_ntree) %>%
  group_by(rf_words) %>%
  arrange(rf_words)

ggplot(data = rf_plotdata, aes(x = rf_words, y = rf_oobs, col = rf_ntree)) + 
  geom_line() + geom_point() + coord_cartesian(xlim = c(0, 10000), ylim = c(0.24, 0.28))+
  scale_x_discrete(limits = seq(from = 0, to = 9000, by = 1000)) +
  scale_color_manual(values = c("#3342FF", "#BB0600", "#5BBB00")) +
  labs(title = "Random Forest Out of Bound Error Plot", 
       x = "Power Words", y = "OOB Error", color = "Tree Size")


## put in the other models here

RF.test.predictions <- predict(rf.200.1000[[1]], hil.test)

RF.test.predictions <- TestPrediction(rf.200.1000, hil.test)

export_txt(RF.test.predictions, "predict")


############ SVM ##################


error_vector<-rep(0,0)

words1800<-importance.200 %>%
  group_by(word) %>%
  arrange(desc(MeanDecreaseGini))%>%
  select(word)%>%
  head(1700)

intersect(names(hil.train), words1800$word)

subset.hil.train<-hil.train[, intersect(names(hil.train), words1800$word)]

subset.hil.train<-cbind(senders,subset.hil.train)

tune.fit<-tune(svm, subset.hil.train,subset.hil.train[,1], kernel = "linear", 
               tunecontrol = tune.control(cross=5),cost = 0.00001, scale = FALSE)
error_vector<-c(error_vector,tune.fit$performances$error)


tune.fit<-tune(svm, subset.hil.train,subset.hil.train[,1], kernel = "linear", 
               tunecontrol = tune.control(cross=5),cost = 0.0001, scale = FALSE)
error_vector<-c(error_vector,tune.fit$performances$error)


tune.fit<-tune(svm, subset.hil.train,subset.hil.train[,1], kernel = "linear", 
               tunecontrol = tune.control(cross=5),cost = 0.00012, scale = FALSE)
error_vector<-c(error_vector,tune.fit$performances$error)


tune.fit<-tune(svm, subset.hil.train,subset.hil.train[,1], kernel = "linear", 
               tunecontrol = tune.control(cross=5),cost = 0.00011, scale = FALSE)
error_vector<-c(error_vector,tune.fit$performances$error)


tune.fit<-tune(svm, subset.hil.train,subset.hil.train[,1], kernel = "linear", 
               tunecontrol = tune.control(cross=5),cost = 0.0000999, scale = FALSE)
error_vector<-c(error_vector,tune.fit$performances$error)


tune.fit<-tune(svm, subset.hil.train,subset.hil.train[,1], kernel = "polynomial", 
               tunecontrol = tune.control(cross=5),cost = 0.1, scale = FALSE,degree =2)
error_vector<-c(error_vector,tune.fit$performances$error)


tune.fit<-tune(svm, subset.hil.train,subset.hil.train[,1], kernel = "radial", 
               tunecontrol = tune.control(cross=5),cost = 0.1, scale = FALSE,gamma = 1)
error_vector<-c(error_vector,tune.fit$performances$error)

svm.fit<-svm(hil.train,hil.train[,1], kernel = "linear", 
             tunecontrol = tune.control(cross=5),cost = 0.0000999, scale = FALSE)

q_pred<-predict(svm.fit, hil.train)

##############################################################
##### K-Means Cluster Analysis Using 100 important terms #####

# Load dependencies
library(gtools)

# Conduct kmeans clustering, K = 5 clearly
# First, extract top 100 features from random forests

top.100 <- data.frame(importance(rf.200.1000[[1]]),
                      word = rownames(importance(rf.200.1000[[1]]))) %>%
  arrange(desc(MeanDecreaseGini))


top.100vec <- head(as.character(top.100$word), 100)
top.100.no.char <- head(as.character(top.100$word), 101)[-which(top.100$word == "email_chars")]

WFM <- hil.train[, colnames(hil.train) %in% top.100vec]

word.kmean <- kmeans(WFM , centers = 5, iter.max = 50, nstart = 50)

first_guess <- LowErrorRate(word.kmean, y = senders)

WFM_nochar <- hil.train[, colnames(hil.train) %in% top.100.no.char] 

for(i in 1:nrow(WFM_nochar)){
  WFM_nochar[i,] = WFM_nochar[i,] / hil.train$email_chars[i]
}

nochar.kmean <- kmeans(WFM_nochar, centers = 5, iter.max = 50, nstart = 50)
normalized_guess <- LowErrorRate(nochar.kmean, y = senders)

WFM_standard <- hil.train[, colnames(hil.train) %in% top.100vec] %>%
  apply(2, function(x){
    x = (x - mean(x)) / sd(x)
  })

standard.kmean <- kmeans(WFM_standard, centers = 5, iter.max = 50, nstart = 50)
standard.guess <- LowErrorRate(standard.kmean, y = senders)

word.kmean <- kmeans(WFM , centers = 5, iter.max = 50, nstart = 50)