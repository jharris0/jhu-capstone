# tokenize the corpus
library(dplyr)
library(tidyr)
library(data.table)

# setwd("C:/Users/Jesse/gitrepos/jhu-capstone")

# load(file = "corpus_train_clean.Rdata")

tokenizeNgrams <- function(x, level) {
  library(tidytext)
  ngrams <- x %>%
    unnest_tokens(sentence, text, token = "sentences") %>%
    unnest_tokens(ngram, sentence, token = "ngrams", n = level)
  return(ngrams)
}

corpus_df <- corpus_train_clean %>%
  mutate(linenumber = row_number()) %>%
  select(linenumber, text)

corpus_bigrams <- tokenizeNgrams(corpus_df, 2)
corpus_trigrams <- tokenizeNgrams(corpus_df, 3)
corpus_fourgrams <- tokenizeNgrams(corpus_df, 4)
corpus_fivegrams <- tokenizeNgrams(corpus_df, 5)

save(corpus_bigrams, file = "corpus_bigrams.Rdata")
save(corpus_trigrams, file = "corpus_trigrams.Rdata")
save(corpus_fourgrams, file = "corpus_fourgrams.Rdata")
save(corpus_fivegrams, file = "corpus_fivegrams.Rdata")

# load("corpus_bigrams.Rdata")
# load("corpus_trigrams.Rdata")
# load("corpus_fourgrams.Rdata")
# load("corpus_fivegrams.Rdata")

library(tidyr) # reloading to overcome "could not find function 'separate' error

# Bigrams
bi_1 <- corpus_bigrams %>%
  separate(ngram, c("word1", "word2"), sep = " ")
setDT(bi_1)
setkey(bi_1, word1)
bi_2 <- bi_1[,.N, by = word1][order(-N)][1:75000]
bi_3 <- semi_join(bi_1, bi_2, by = "word1")
setDT(bi_3)
setkey(bi_3, word1)
bi_4 <- bi_3[,.N,by=.(word1, word2)]
bi_4[,prob:=N/sum(N),by=.(word1)]
bi_4[,N2:=sum(N),by=.(word1)]
bi_5 <- bi_4[N > 1]
bi_6 <- list()

for(i in unique(bi_5$word1)) {
  dt_tmp <- bi_5[word1 == i]
  dt_tmp <- dt_tmp[order(-prob)]
  tmp <- dt_tmp[1:4,.(word1, word2, N2, prob)]
  bi_6[[i]] <- tmp
}

bigrams_final <- bind_rows(bi_6) %>% select(word1, word2, N2, prob) %>%
  arrange(desc(N2), word1, desc(prob))
bigrams_final <- bigrams_final[complete.cases(bigrams_final),]
save(bigrams_final, file = "bigrams_final.Rdata")

# Trigrams
tri_1 <- corpus_trigrams %>%
  separate(ngram, c("word1", "word2", "word3"), sep = " ") %>%
  mutate(word1_2 = paste(word1, word2)) %>% select(word1_2, word3)
setDT(tri_1)
setkey(tri_1, word1_2)
tri_2 <- tri_1[,.N, by = word1_2][order(-N)][1:75000]
tri_3 <- semi_join(tri_1, tri_2, by = "word1_2")
setDT(tri_3)
setkey(tri_3, word1_2)
tri_4 <- tri_3[,.N,by=.(word1_2, word3)]
tri_4[,prob:=N/sum(N),by=.(word1_2)]
tri_4[,N2:=sum(N),by=.(word1_2)]
tri_5 <- tri_4[N > 1]
tri_6 <- list()

for(i in unique(tri_5$word1_2)) {
  dt_tmp <- tri_5[word1_2 == i]
  dt_tmp <- dt_tmp[order(-prob)]
  tmp <- dt_tmp[1:4,.(word1_2, word3, N2, prob)]
  tri_6[[i]] <- tmp
}

trigrams_final <- bind_rows(tri_6) %>% select(word1_2, word3, N2, prob) %>%
  arrange(desc(N2), word1_2, desc(prob))
trigrams_final <- trigrams_final[complete.cases(trigrams_final),]
save(trigrams_final, file = "trigrams_final.Rdata")

# Fourgrams
four_1 <- corpus_fourgrams %>%
  separate(ngram, c("word1", "word2", "word3", "word4"), sep = " ") %>%
  mutate(word1_2_3 = paste(word1, word2, word3)) %>%
  select(word1_2_3, word4)
setDT(four_1)
setkey(four_1, word1_2_3)
four_2 <- four_1[,.N, by = word1_2_3][order(-N)][1:75000]
four_3 <- semi_join(four_1, four_2, by = "word1_2_3")
setDT(four_3)
setkey(four_3, word1_2_3)
four_4 <- four_3[,.N,by=.(word1_2_3, word4)]
four_4[,prob:=N/sum(N),by=.(word1_2_3)]
four_4[,N2:=sum(N),by=.(word1_2_3)]
four_5 <- four_4[N > 1]
four_6 <- list()

for(i in unique(four_5$word1_2_3)) {
  dt_tmp <- four_5[word1_2_3 == i]
  dt_tmp <- dt_tmp[order(-prob)]
  tmp <- dt_tmp[1:4,.(word1_2_3, word4, N2, prob)]
  four_6[[i]] <- tmp
}

fourgrams_final <- bind_rows(four_6) %>% select(word1_2_3, word4, N2, prob) %>%
  arrange(desc(N2), word1_2_3, desc(prob))
fourgrams_final <- fourgrams_final[complete.cases(fourgrams_final),]

save(fourgrams_final, file = "fourgrams_final.Rdata")

# Fivegrams
five_1 <- corpus_fivegrams %>%
  separate(ngram, c("word1", "word2", "word3", "word4", "word5"), sep = " ") %>%
  mutate(word1_2_3_4 = paste(word1, word2, word3, word4)) %>%
  select(word1_2_3_4, word5)
setDT(five_1)
setkey(five_1, word1_2_3_4)
five_2 <- five_1[,.N, by = word1_2_3_4][order(-N)][1:75000]
five_3 <- semi_join(five_1, five_2, by = "word1_2_3_4")
setDT(five_3)
setkey(five_3, word1_2_3_4)
five_4 <- five_3[,.N,by=.(word1_2_3_4, word5)]
five_4[,prob:=N/sum(N),by=.(word1_2_3_4)]
five_4[,N2:=sum(N),by=.(word1_2_3_4)]
five_5 <- five_4[N > 1]
five_6 <- list()

for(i in unique(five_5$word1_2_3_4)) {
  dt_tmp <- five_5[word1_2_3_4 == i]
  dt_tmp <- dt_tmp[order(-prob)]
  tmp <- dt_tmp[1:4,.(word1_2_3_4, word5, N2, prob)]
  five_6[[i]] <- tmp
}

fivegrams_final <- bind_rows(five_6) %>% select(word1_2_3_4, word5, N2, prob) %>%
  arrange(desc(N2), word1_2_3_4, desc(prob))
fivegrams_final <- fivegrams_final[complete.cases(fivegrams_final),]
save(fivegrams_final, file = "fivegrams_final.Rdata")