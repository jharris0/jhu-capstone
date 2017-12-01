# sequentially ingest and sample each corpus

library(readr)
library(dplyr)

setwd("jhu-capstone")

sampleCorpus <- function(corpus_name, percent) {
        totalLines <- length(corpus_name)
        linesToRead <- round(percent/100 * totalLines)
        result <- sample(corpus_name, linesToRead)
        return(result)
}

set.seed(2017)

con <- file("corpora/en_us/en_us.blogs.txt", encoding = "UTF-8")
blogs <- read_lines(con)

blogs_sample <- sampleCorpus(blogs, 15)
rm(blogs)
blogs_sample <- as.data.frame(blogs_sample) %>% rename(text = blogs_sample)

con <- file("corpora/en_us/en_us.news.txt", encoding = "UTF-8")
news <- read_lines(con)

news_sample <- sampleCorpus(news, 15)
rm(news)
news_sample <- as.data.frame(news_sample) %>% rename(text = news_sample)

con <- file("corpora/en_us/en_us.twitter.txt", encoding = "UTF-8")
twitter <- read_lines(con)

twitter_sample <- sampleCorpus(twitter, 15)
rm(twitter)
twitter_sample <- as.data.frame(twitter_sample) %>% rename(text = twitter_sample)

corpus <- bind_rows(blogs_sample, news_sample, twitter_sample)
nrow_corpus <- nrow(corpus)
sample <- sample.int(nrow_corpus, size = floor(.75*nrow_corpus), replace = F)
corpus_train <- corpus[sample,]
corpus_train <- as.data.frame(corpus_train) %>% rename(text = corpus_train)
corpus_test <- corpus[-sample,]
corpus_test <- as.data.frame(corpus_test) %>% rename(text = corpus_test)
#rm(blogs_sample, news_sample, twitter_sample)
save(corpus_train, file = "corpus_train.Rdata")
save(corpus_test, file = "corpus_test.Rdata")
