
######### Stat 154 - Final Project Funtions ##########

##############################################################
######            Pre-Processing Function               ###### 
##############################################################

preprocessing <- function(filepath,
                          ShowTerms = T,
                          RemoveCommonWords = F,
                          common_words =  c("department", "state", "unclassified", "case",
                                            "date", "doc", "subject", "release", "call",
                                            "message", "monday", "tuesday", "wednesday",
                                            "thursday", "friday", "saturday", "sunday", "mon", 
                                            "tue", "wed", "thurs", "fri", "sat", "sun", "part")) {
  # input: filepath of a raw data set
  
  # function: performs raw cleaning, stemming, removal of stop words
  
  # ShowTerms : logical, if true -> display number of terms at various steps in process
  # RemoveCommonWords : logical, if true -> remove words that show up consistently in each email
  # common_words : character, vector of words to remove if RemoveCommonWords == T
  
  # output: a document term matrix in matrix format
  
  hil <- read.delim(filepath, sep = "", header = F) # load in the dataset
  
  feature_filter <- function(hil) { 
    
    hil.df <- hil #for future reference
    emails <- hil.df$email
    
    hil <- DataframeSource(hil['email'])
    hil <-  Corpus(hil)
    
    if(ShowTerms == T){ 
      hilraw.dtm <- DocumentTermMatrix(hil)
      #Total raw features
      print("Raw Number of Terms")
      print(nTerms(hilraw.dtm))
    }
    
    ### Begin Preprocessing ###
    
    ##Raw cleaning
    
    
    # Make everything lowercase
    
    hil <- tm_map(hil, tolower)
    
    # Remove punctuation
    
    hil <- tm_map(hil, removePunctuation)
    
    # Remove numbers
    
    hil <- tm_map(hil, removeNumbers)
    
    # Remove excess white space from this process
    
    hil <- tm_map(hil, stripWhitespace)
    
    # Remove stop words (and check which words we are removing)
    
    stopwords("SMART")
    hil <- tm_map(hil, removeWords, stopwords("SMART"))
    
    if(ShowTerms == T){  
      hil1 <- tm_map(hil, PlainTextDocument)
      hilstop.dtm <- DocumentTermMatrix(hil1)
      # Total features after removing stop words
      print("Number of Terms After Stop Word Removal")
      print(nTerms(hilstop.dtm))
    }  
    
    # Remove words that show up consistently in all emails
    # These ones probably don't have much predictive power
    if(RemoveCommonWords == T){
      hil <- tm_map(hil, removeWords, commonWords)
      if(ShowTerms == T){
        hil1 <- tm_map(hil, PlainTextDocument)
        hilstop.dtm <- DocumentTermMatrix(hil1)
        # Total features after removing common words
        print("Number of Terms After Removing Common Words")
        print(nTerms(hilstop.dtm))
      }  
    }
    
    
    # Time to stem
    # Use a simple function in SnowballC
    
    hil <- tm_map(hil, stemDocument)
    
    if(ShowTerms == T){ 
      #Total raw features
      hil1 <- tm_map(hil1, stemDocument)
      hilstem.dtm <- DocumentTermMatrix(hil1)
      print("Number of Words After Stemming")
      print(nTerms(hilstem.dtm))
    }
    
    ##### Create Word Matrices for Custom Classifier #####
    hil <- tm_map(hil, PlainTextDocument) 
    hil.dtm <- DocumentTermMatrix(hil)
    
    dtm.mat <- as.matrix(hil.dtm)
    dtm.mat
  }
  
  
  if (ncol(hil) == 2) { # if preprocessing the training set, which has two columns: sender and email column
    colnames(hil) <- c("sender", "email") # changes column names to the following
    senders <- hil$sender
    dtm.mat <- feature_filter(hil)
    dtm.mat <- cbind( senders, dtm.mat)
    dtm.mat
  } else if (ncol(hil) == 1) { # if preprocessing test set, which only has the email column
    colnames(hil) <- c("email")
    dtm.mat <- feature_filter(hil)
    dtm.mat
  }
  
  
}


################## Power Word Function ########################


PowerWordList <- function(dtm, nsend = 5,
                          top_words = 150){
  sender_list <- list()
  for(i in 1:nsend){
    dtm.mat.i <- dtm[dtm[,1] == i, ]
    dtm.mat.i <- dtm.mat.i[, -1]
    word.freq.i <- sort(colSums(dtm.mat.i), decreasing = T)
    word.freq.i <- word.freq.i[1:top_words]
    if(i == 1){
      best_words <- names(word.freq.i)
    } else{
      best_words <- c(best_words, names(word.freq.i))
    }
  }
  return(unique(best_words))
}

################## Find word intersection Function #####################

FindIntersect <- function(test_emails,
                          train_emails,
                          return_which = c("train", "test", "both")){
  ### Subsets test and training sets to only include terms that appear in both matrices
  ### Also keeps out words that R doesn't like as variable names
  
  ### Make sure we don't get errors but using non-kosher r words
  if(length(which(names(train_emails) %in% c("break","function"))) > 0){
    train_emails <- train_emails[, -which(names(train_emails) %in% c("break","function"))]
  }
  
  ### Subset out any words that do not appear in both the training and test set
  in_both_dtms <- intersect(names(train_emails), names(test_emails))
  
  train.intersect <- train_emails[ , names(train_emails) %in% in_both_dtms] %>%
    cbind(senders)
  
  
  test.intersect <- test_emails[,names(test_emails) %in% in_both_dtms]
  
  ### Return the one that you want back
  if(return_which == "train"){
    return(train.intersect)
  }
  
  if(return_which == "test"){
    return(test.intersect)
  }
  if(return_which == "both"){
    return(list(train = train.intersect,
                test = test.intersect))
  }
  
}

################## Random Forest Estimation Function ####################

FitRFClean <- function(words_of_interest,
                       train_emails,
                       senders,
                       ...){
  # words_of_interest : character vector of words to test
  # test_emails : df, dtm for test set
  # train_emails : df, dtm for training set
  
  
  ### Subset train and test to only include columns for words of interest
  train_emails <- train_emails[, colnames(train_emails) %in% words_of_interest]
  
  ### Fit model to training set
  rf.model <- randomForest(as.factor(senders) ~ .,
                           data = train_emails,
                           mtry = floor(sqrt(ncol(train_emails))), 
                           ...)
  
  ### Return out-of-bag error
  rf.oob <- 1 - ( sum(predict(rf.model) == senders) / length(senders) )
  
  return(list(Model = rf.model, Error_Rate = rf.oob))
  
}

########## K-Means Level Changing Function ##########

SwitchLevels <- function(clusters,
                         new_levels){
  # Reconfigures level of clusters to different mapping
  
  old_levels <- 1:5
  mapping <- data.frame(old = old_levels, new = new_levels)
  clusters.df <- data.frame(old = clusters, index = 1:length(clusters))
  new.df <- merge(clusters.df, mapping, all.x = T) %>%
    arrange(index)
  return(new.df$new)
}

########## K Means Best Model Function ##########

LowErrorRate <- function(kmeans.fit,
                         y = senders){
  # Returns best error rate among all cluster configurations,
  # Mapping used to find best error rate,
  # and the corrected kmeans cluster predictions for each observation
  
  # kmeans.fit : kmeans model
  # y : numeric or factor containing actual senders for training set
  
  # Extract class assignments given by model fit
  clusters <- kmeans.fit$cluster
  
  # Calculate all permutations of class assignments
  perms <- permutations(length(unique(y)), length(unique(y)))
  
  # Calculate new class assignments for each permutation
  all_mappings <- apply(perms, 1, SwitchLevels, clusters = clusters)
  
  # Calculate error rate for each new set of class assignments
  error_rates <- 1 - (apply(all_mappings, 2, function(x){
    sum(x == y) / length(y)
  }))
  
  # Return assignment with lowest error rate
  return(list(Best_Error_Rate = error_rates[which.min(error_rates)], 
              Mapping_To_Senders <- data.frame(
                kmeans_label = 1:5,
                true_label = perms[which.min(error_rates), ]),
              Adjusted_Clusters <- all_mappings[, which.min(error_rates)]))
}


##################### Apply RF Models to Sets of Words Function ##############


RFApply <- function(dtm.train, 
                    dtm.test,
                    nsend = 5, list_words) { # constructs a list of best_n words 
  
  stopifnot(is.vector(list_words) == TRUE)
  
  power_list = list()
  runtimes = list()
  info = list()
  
  for (i in 1:length(list_words)) {
    power_list[[i]] <- PowerWordList(dtm.train, nsend, top_words = list_words[i])
  }
  
  fit_list <- lapply(power_list, FitRFClean, 
                     test_emails = dtm.test, 
                     train_emails = dtm.train, 
                     senders = senders)
  
  return(fit_list)
}

##################### Append Power Features to data frame function ####################
<<<<<<< HEAD
=======
  
  AddPowerFeatures <- function(
    dtm.df,
    sender1 = c("house select benghazi comm", "condfidential reason"),
    sender2 = c("human rights", "thank you"),
    sender3 = c("peace talks"),
    sender4 = c("private residence", "situation room", "en route", "conference room", "mini "),
    sender5 = c("prime minister", "white house", "gordon brown", "tony blair"),
    email_vec = as.character(emails)
  ){
    # Create vector of all power features
    powerf <- c(sender1,sender2,sender3,sender4,sender5)
    
    # Create empty matrix to store number of times power feature shows up in each email
    powerfmat <- matrix(nrow = nrow(dtm.df), ncol = length(powerf))
    colnames(powerfmat) <- gsub(" ", "_", powerf)
    for (i in 1:nrow(dtm.df)) {
      for (j in 1:length(powerf)) {
        if (gregexpr(powerf[j], emails[i])[[1]] == -1) {
          powerfmat[i, j] <- 0}
        else 
        {powerfmat[i, j] <- length(gregexpr(powerf[j], emails[i])[[1]])
        }
      }
    }
    
    
    
    ## Email length Power Feature
    email_chars <- nchar(email_vec)
    powerfmat <- as.data.frame(cbind(powerfmat, email_chars))
    
    # Merge Power Feature Matrix with original data frame
    return(cbind(dtm.df, powerfmat))
    

##################### ROC Curve Function ####################



rocplot <- function(models, senders = senders) {
  
  #model is the output of running FitRFClean for a particular sequence of power word lengths
  
  labels <- rep(0, length(senders))
  plots <- list()
  predictions <- as.numeric(as.vector(models$Model$predicted))
  
  pred_and_send <- data.frame(predictions, senders)
  
  binary <- function(vec1,vec2) {
    
    stopifnot(length(vec1) == length(vec2))
    bin <- rep(1, length(vec1))
    
    for (i in 1:length(vec1)) {
      if (vec1[i] == vec2[i]) {
        bin[i] <- 1
      } else {
        bin[i] <- 0
      }
    }
    bin
    
  }
  
  labels <- binary(pred_and_send[,1], pred_and_send[,2])
  
  predob <- prediction(predictions, labels)
  perf <- performance(predob, "tpr", "fpr")
  data.perf <- data.frame(perf@x.values, perf@y.values)
  names(data.perf) <- c("xval", "yval")
  p <- qplot(xval, yval, data = data.perf, geom = "blank", main = "ROC curve", xlab = "False Positive Rate (1-Specificity)", ylab = "True Positive Rate (Sensitivity)" ) 
  plots <- p + geom_line(data = data.perf, aes(x = xval, y = yval, colour = "#5BBB00"), size = 2, alpha = 0.7) + scale_color_manual(values = c("#5BBB00"))
  
  print(plots)
}
    
##################### Test Prediction Function ####################
    

    
TestPrediction <- function(bestmodel, test_set) {
  predict(bestmodel[[1]], test_set)
}

##################### Export Predictions to Txt File Function ####################
    
    
export_txt <- function(testpredict, filename) { # filename must be a string
  wd <- getwd()
  capture.output(testpredict, file = paste0(wd, filename, ".txt"))
}
    
=======

>>>>>>> 529764572041e442e2711013852c561914f84eed
