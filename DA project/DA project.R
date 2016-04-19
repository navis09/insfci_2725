library(readr)
library(gbm)
library(rpart)
cat("Reading data\n")
setwd('/Users/HenryYang/Downloads')
train = read_csv("train.csv")
test  = read_csv("test.csv")
desc = read_csv("product_descriptions.csv")

trees <- 500
cat("Merge description with train and test data \n")
train <- merge(train,desc, by.x = "product_uid", by.y = "product_uid", all.x = TRUE, all.y = FALSE)
test <- merge(test,desc, by.x = "product_uid", by.y = "product_uid", all.x = TRUE, all.y = FALSE)

t <- Sys.time()
word_match <- function(words,title,desc){
  n_title <- 0
  n_desc <- 0
  words <- unlist(strsplit(words," "))
  nwords <- length(words)
  for(i in 1:length(words)){
    n_char <- nchar(words[1])
    pattern <- paste("(^| )",words[i],"($| )",sep="")
    n_title <- n_title + grepl(pattern,title,perl=TRUE,ignore.case=TRUE)
    n_desc <- n_desc + grepl(pattern,desc,perl=TRUE,ignore.case=TRUE)
    
  }
  return(c(n_title,nwords,n_desc))
}
clean_text <- function(text){
  gsub(" ","  ", text)
  gsub("??", "degrees", text)
  gsub(" v ", "volts", text)
}

train$product_title <- clean_text(train$product_title)
train$product_description <- clean_text(train$product_description)
train$search_term <- clean_text(train$search_term)

test$product_title <- clean_text(test$product_title)
test$product_description <- clean_text(test$product_description)
test$search_term <- clean_text(test$search_term)

cat("Get number of words and word matching title in train\n")
train_words <- as.data.frame(t(mapply(word_match,train$search_term,train$product_title,train$product_description)))
train$nmatch_title <- train_words[,1]
train$nwords <- train_words[,2]
train$nmatch_desc <- train_words[,3]

cat("Get number of words and word matching title in test\n")
test_words <- as.data.frame(t(mapply(word_match,test$search_term,test$product_title,test$product_description)))
test$nmatch_title <- test_words[,1]
test$nwords <- test_words[,2]
test$nmatch_desc <- test_words[,3]

rm(train_words,test_words)

cat("A simple linear model on number of words and number of words that match\n")
gbm_model <- gbm.fit(train[,7:9],train$relevance,distribution = "gaussian",interaction.depth = 3,shrinkage=0.05,n.trees=1000)
test_relevance <- predict(gbm_model,test[,6:8],n.trees=1000)
test_relevance <- ifelse(test_relevance>3,3,test_relevance)
test_relevance <- ifelse(test_relevance<1,1,test_relevance)

submission <- data.frame(id=test$id,relevance=test_relevance)
write_csv(submission,"gbm3_submission.csv")
print(Sys.time()-t)
