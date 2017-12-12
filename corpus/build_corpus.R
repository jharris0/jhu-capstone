# Corpus needs to be downloaded from:
# https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip
# Below is code to sequentially ingest and sample English portions of the
# corpus. Output is a training set and test set in data frame format, ready
# for pre-processing.

library(readr)
library(dplyr)

sampleCorpus <- function(corpus_name, percent) {
        totalLines <- length(corpus_name)
        linesToRead <- round(percent/100 * totalLines)
        result <- sample(corpus_name, linesToRead)
        return(result)
}

set.seed(2017)

# Sample blogs, news and twitter content
con <- file("corpora/en_us/en_us.blogs.txt", encoding = "UTF-8")
blogs <- read_lines(con)
blogs_sample <- sampleCorpus(blogs, 60)
rm(blogs) # to free up memory
blogs_sample <- as.data.frame(blogs_sample) %>% rename(text = blogs_sample)

con <- file("corpora/en_us/en_us.news.txt", encoding = "UTF-8")
news <- read_lines(con)
news_sample <- sampleCorpus(news, 60)
rm(news)
news_sample <- as.data.frame(news_sample) %>% rename(text = news_sample)

con <- file("corpora/en_us/en_us.twitter.txt", encoding = "UTF-8")
twitter <- read_lines(con)
twitter_sample <- sampleCorpus(twitter, 60)
rm(twitter)
twitter_sample <- as.data.frame(twitter_sample) %>% rename(text = twitter_sample)

# Combine content data frames, then split into training and test sets
corpus <- bind_rows(blogs_sample, news_sample, twitter_sample)
nrow_corpus <- nrow(corpus)
# 75/25 training/test split
sample <- sample.int(nrow_corpus, size = floor(.75*nrow_corpus), replace = F)
corpus_train <- corpus[sample,]
corpus_train <- as.data.frame(corpus_train) %>% rename(text = corpus_train)
corpus_test <- corpus[-sample,]
corpus_test <- as.data.frame(corpus_test) %>% rename(text = corpus_test)

# Save training and test sets to disk
save(corpus_train, file = "corpus_train.Rdata")
save(corpus_test, file = "corpus_test.Rdata")

# Next step: Pre-processing using 'pre-process_corpus.R'