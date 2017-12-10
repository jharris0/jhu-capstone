# Pre-process training and test set corpuses. Includes removing profanity and
# numbers. Punctuation and capitalization are not modified in this stage.

library(readr)
library(tm)

# Comments related to cleanCorpus function below:
# 1: Remove "RT" from Twitter content
# 2: Replace clock times
# 3: Remove colon - related to preventing unigrams like "00pm"
# 4: Remove numbers like "2017" but not "1st"
# 5: Remove commas (related to number/char combos like "1,000,000th")
# 6: Remove heart text emojis
# 7: Remove smileys
# https://stackoverflow.com/questions/5862490/how-to-match-emoticons-with-regular-expressions

cleanCorpus <- function(corpus_name) {
  result <- gsub("RT : ", "", corpus_name) # 1
  result <- gsub("([0-9]|0[0-9]|1[0-9]|2[0-3]):[0-5][0-9]", "", result) # 2
  result <- gsub(":", "", result) # 3
  result <- gsub("\\d+(?!\\w)", "", perl = TRUE, result) # 4
  result <- gsub(",", "", result) # 5
  result <- gsub("<3", "", result) # 6
  result <- gsub("((?::|;|=)(?:-)?(?:\\)|D|P))", "", result) # 7
  return(result)
}

profanity <- read_lines("http://www.bannedwordlist.com/lists/swearWords.txt")

load("corpus_train.Rdata")
# Next line is required to resolve some errors encountered related to encoding
corpus_train$text <- iconv(corpus_train$text, "latin1", "ASCII", sub="")
corpus_train$text <- cleanCorpus(corpus_train$text)
corpus_train <- VCorpus(VectorSource(corpus_train))
corpus_train <-tm_map(corpus_train,content_transformer(tolower))
# Acknowledgement: http://rpubs.com/eKtorpKamprad/DS_Capstone_WK2_Milestone
corpus_train <- tm_map(corpus_train, removeWords, profanity)
corpus_train_clean <- corpus_train
corpus_train_clean <- data.frame(text=unlist(sapply(corpus_train_clean,
                                                    `[`, "content")),
                                 stringsAsFactors=F)
save(corpus_train_clean, file = "corpus_train_clean.Rdata")

load("corpus_test.Rdata")
corpus_test$text <- iconv(corpus_test$text, "latin1", "ASCII", sub="")
corpus_test$text <- cleanCorpus(corpus_test$text)
corpus_test <- VCorpus(VectorSource(corpus_test))
corpus_test <-tm_map(corpus_test,content_transformer(tolower))
corpus_test <- tm_map(corpus_test, removeWords, profanity)
corpus_test_clean <- corpus_test
corpus_test_clean <- data.frame(text=unlist(sapply(corpus_test_clean,
                                                   `[`, "content")), 
                                 stringsAsFactors=F)
save(corpus_test_clean, file = "corpus_test_clean.Rdata")

# Next step for training data: build_ngram_tables.R
# Next step for test data: build_ngrams_test_set.R