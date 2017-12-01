# setwd("jhu-capstone")

library(readr)
library(tm)

cleanCorpus <- function(corpus_name) {
  # remove RT
  result <- gsub("RT : ", "", corpus_name)
  # replace clock times
  result <- gsub("([0-9]|0[0-9]|1[0-9]|2[0-3]):[0-5][0-9]", "", result)
  # remove colon - related to preventing unigrams like "00pm"
  result <- gsub(":", "", result)
  # replace numbers like "2017" but not "1st"
  result <- gsub("\\d+(?!\\w)", "", perl = TRUE, result)
  # remove commas (related to number/char combos like "1,000,000th")
  result <- gsub(",", "", result)
  # remove heart text emojis
  result <- gsub("<3", "", result)
  # remove smiley https://stackoverflow.com/questions/5862490/how-to-match-emoticons-with-regular-expressions
  result <- gsub("((?::|;|=)(?:-)?(?:\\)|D|P))", "", result)
  return(result)
}

profanity <- read_lines("http://www.bannedwordlist.com/lists/swearWords.txt")

#load("corpus_train.Rdata")
corpus_train$text <- iconv(corpus_train$text, "latin1", "ASCII", sub="")
corpus_train$text <- cleanCorpus(corpus_train$text)
corpus_train <- VCorpus(VectorSource(corpus_train))
corpus_train <-tm_map(corpus_train,content_transformer(tolower))
# http://rpubs.com/eKtorpKamprad/DS_Capstone_WK2_Milestone
corpus_train <- tm_map(corpus_train, removeWords, profanity)
corpus_train_clean <- corpus_train
corpus_train_clean <- data.frame(text=unlist(sapply(corpus_train_clean, `[`, "content")), 
                                 stringsAsFactors=F)
save(corpus_train_clean, file = "corpus_train_clean.Rdata")

#load("corpus_test.Rdata")
corpus_test$text <- iconv(corpus_test$text, "latin1", "ASCII", sub="")
corpus_test$text <- cleanCorpus(corpus_test$text)
corpus_test <- VCorpus(VectorSource(corpus_test))
corpus_test <-tm_map(corpus_test,content_transformer(tolower))
corpus_test <- tm_map(corpus_test, removeWords, profanity)
corpus_test_clean <- corpus_test
corpus_test_clean <- data.frame(text=unlist(sapply(corpus_test_clean, `[`, "content")), 
                                 stringsAsFactors=F)
save(corpus_test_clean, file = "corpus_test_clean.Rdata")