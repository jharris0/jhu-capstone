# setwd("jhu-capstone")

library(readr)
library(tm)
library(topicmodels)

sampleCorpus <- function(corpus_name, percent) {
  totalLines <- length(corpus_name)
  linesToRead <- round(percent/100 * totalLines)
  result <- sample(corpus_name, linesToRead)
  return(result)
}

cleanCorpus <- function(corpus_name) {
  # remove RT
  result <- gsub("RT : ", "", corpus_name)
  # replace clock times
  result <- gsub("([0-9]|0[0-9]|1[0-9]|2[0-3]):[0-5][0-9]", "time1", result)
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

set.seed(2017)

con <- file("corpora/en_us/en_us.blogs.txt", encoding = "UTF-8")
blogs <- read_lines(con)
blogs_sample <- sampleCorpus(blogs, 2)
rm(blogs)

con <- file("corpora/en_us/en_us.news.txt", encoding = "UTF-8")
news <- read_lines(con)
news_sample <- sampleCorpus(news, 2)
rm(news)

con <- file("corpora/en_us/en_us.twitter.txt", encoding = "UTF-8")
twitter <- read_lines(con)
twitter_sample <- sampleCorpus(twitter, 2)
rm(twitter)

corpus <- c(blogs_sample, news_sample, twitter_sample)
corpus <- iconv(corpus, "latin1", "ASCII", sub="")
corpus_clean <- cleanCorpus(corpus)
tm_corpus <- VCorpus(VectorSource(corpus_clean))
tm_corpus <-tm_map(tm_corpus,content_transformer(tolower))
# http://rpubs.com/eKtorpKamprad/DS_Capstone_WK2_Milestone
profanity <- read_lines("http://www.bannedwordlist.com/lists/swearWords.txt")
tm_corpus <- tm_map(tm_corpus, removeWords, profanity)
dtm <- DocumentTermMatrix(tm_corpus, control = list(stopwords = TRUE,
                                                    removePunctuation = TRUE,
                                                    minWordLength = 3))
# https://stackoverflow.com/questions/13944252/remove-empty-documents-from-documenttermmatrix-in-r-topicmodels
ui <- unique(dtm$i)
dtm.new <- dtm[ui,]

k6_model <- LDA(dtm.new, k = 6, control = list(seed = 2017))

save(k6_model, file = "k6_model.Rdata")

