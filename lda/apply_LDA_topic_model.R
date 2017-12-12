# setwd("jhu-capstone")

library(topicmodels)
library(tm)
library(dplyr)

load("k5_model.Rdata")
load("bigrams_final.Rdata")
load("trigrams_final.Rdata")
load("fourgrams_final.Rdata")
load("fivegrams_final.Rdata")

predictTopics <- function(ngram_table, number_rows, ngram_type = 2) {
  lda_topics <- list()
  for (i in 1:number_rows) {
    row <- ngram_table[i,]
    if (ngram_type == 2) {
      string <- paste(row$word1, row$word2, sep = " ")
    } else if (ngram_type == 3) {
      string <- paste(row$word1_2, row$word3, sep = " ")
    } else if (ngram_type == 4) {
      string <- paste(row$word1_2_3, row$word4, sep = " ")
    } else if (ngram_type == 5) {
      string <- paste(row$word1_2_3_4, row$word5, sep = " ")
    }
    vcorp_string <- VCorpus(VectorSource(string))
    dtm_string <- DocumentTermMatrix(vcorp_string)
    if (sum(dtm_string) == 0) {
      lda_topics[[i]] <- 0
    } else {
      topics <- posterior(k5_model, dtm_string)
      top_topic <- apply(topics$topics, 1, which.max)
      top_topic_df <- top_topic[[1]]
      lda_topics[[i]] <- top_topic_df
    }
  }
  m <- matrix(lda_topics, ncol = 1)
  df <- as.data.frame(m, stringsAsFactors = FALSE)
  names(df) <- "topic"
  return(df)
}

rows_to_predict <- 2000

total_rows <- nrow(bigrams_final)
bi_topics <- predictTopics(bigrams_final, rows_to_predict, 2)
bi_topics[(rows_to_predict+1):total_rows,] <- 0
bigrams_final_wTopic <- cbind(bigrams_final, bi_topics)
save(bigrams_final_wTopic, file = "bigrams_final_wTopic.Rdata")

total_rows <- nrow(trigrams_final)
tri_topics <- predictTopics(trigrams_final, rows_to_predict, 3)
tri_topics[(rows_to_predict+1):total_rows,] <- 0
trigrams_final_wTopic <- cbind(trigrams_final, tri_topics)
save(trigrams_final_wTopic, file = "trigrams_final_wTopic.Rdata")

total_rows <- nrow(fourgrams_final)
four_topics <- predictTopics(fourgrams_final, rows_to_predict, 4)
four_topics[(rows_to_predict+1):total_rows,] <- 0
fourgrams_final_wTopic <- cbind(fourgrams_final, four_topics)
save(fourgrams_final_wTopic, file = "fourgrams_final_wTopic.Rdata")

total_rows <- nrow(fivegrams_final)
five_topics <- predictTopics(fivegrams_final, rows_to_predict, 5)
five_topics[(rows_to_predict+1):total_rows,] <- 0
fivegrams_final_wTopic <- cbind(fivegrams_final, five_topics)
save(fivegrams_final_wTopic, file = "fivegrams_final_wTopic.Rdata")
