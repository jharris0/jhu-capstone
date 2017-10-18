# tokenize the corpus
library(dplyr)
library(tidyr)

setwd("C:/Users/Jesse/gitrepos/jhu-capstone")

load(file = "corpus.Rdata")

tokenizeNgrams <- function(x, level) {
        library(tidytext)
        ngrams <- x %>%
                unnest_tokens(sentence, text, token = "sentences") %>%
                unnest_tokens(ngram, sentence, token = "ngrams", n = level)
        return(ngrams)
}

corpus_df <- corpus %>%
        mutate(linenumber = row_number()) %>%
        select(linenumber, text)

corpus_bigrams <- tokenizeNgrams(corpus_df, 2)
corpus_trigrams <- tokenizeNgrams(corpus_df, 3)
corpus_fourgrams <- tokenizeNgrams(corpus_df, 4)

rm(corpus, corpus_df)

# Generate table of best bigram matches
bigrams_step1 <- corpus_bigrams %>%
        separate(ngram, c("word1", "word2"), sep = " ")
bigrams_step2 <- bigrams_step1 %>% count(word1, sort = TRUE) %>%
        filter(row_number() <= 3000)
bigrams_step3 <- semi_join(bigrams_step1, bigrams_step2, by = "word1")
bigrams_step4 <- bigrams_step3 %>% group_by(word1)
setDT(bigrams_step4)
setkey(bigrams_step4, word1)
bigrams_step5 <- list()

for(i in unique(bigrams_step4$word1)) {
        dt_tmp <- bigrams_step4[.(i)]
        dt_tmp <- dt_tmp[,.N,by=.(word1,word2)][order(-N)]
        tmp <- dt_tmp[1,.(word1, word2)]
        bigrams_step5[[i]] <- tmp
}

bigrams_final <- bind_rows(bigrams_step5) %>% select(word1, word2)
rm(corpus_bigrams, bigrams_step1, bigrams_step2, bigrams_step3, bigrams_step4,
   bigrams_step5)

save(bigrams_final, file = "bigrams_final.Rdata")

# Generate table of best trigram matches
trigrams_step1 <- corpus_trigrams %>%
        separate(ngram, c("word1", "word2", "word3"), sep = " ") %>%
        mutate(word1_2 = paste(word1, word2)) %>% select(word1_2, word3)
trigrams_step2 <- trigrams_step1 %>% count(word1_2, sort = TRUE) %>%
        filter(row_number() <= 5000)
trigrams_step3 <- semi_join(trigrams_step1, trigrams_step2, by = "word1_2")
trigrams_step4 <- trigrams_step3 %>% group_by(word1_2)
setDT(trigrams_step4)
setkey(trigrams_step4, word1_2)
trigrams_step5 <- list()

for(i in unique(trigrams_step4$word1_2)) {
        dt_tmp <- trigrams_step4[.(i)]
        dt_tmp <- dt_tmp[,.N,by=.(word1_2,word3)][order(-N)]
        tmp <- dt_tmp[1,.(word1_2, word3)]
        trigrams_step5[[i]] <- tmp
}

trigrams_final <- bind_rows(trigrams_step5) %>% select(word1_2, word3)
rm(corpus_trigrams, trigrams_step1, trigrams_step2, trigrams_step3,
   trigrams_step4, trigrams_step5)

save(trigrams_final, file = "trigrams_final.Rdata")

# Generate table of best fourgram matches
fourgrams_step1 <- corpus_fourgrams %>%
        separate(ngram, c("word1", "word2", "word3", "word4"), sep = " ") %>%
        mutate(word1_2_3 = paste(word1, word2, word3)) %>%
        select(word1_2_3, word4)
fourgrams_step2 <- fourgrams_step1 %>% count(word1_2_3, sort = TRUE) %>%
        filter(row_number() <= 10000)
fourgrams_step3 <- semi_join(fourgrams_step1, fourgrams_step2, by = "word1_2_3")
fourgrams_step4 <- fourgrams_step3 %>% group_by(word1_2_3)
setDT(fourgrams_step4)
setkey(fourgrams_step4, word1_2_3)
fourgrams_step5 <- list()

for(i in unique(fourgrams_step4$word1_2_3)) {
        dt_tmp <- fourgrams_step4[.(i)]
        dt_tmp <- dt_tmp[,.N,by=.(word1_2_3,word4)][order(-N)]
        tmp <- dt_tmp[1,.(word1_2_3,word4)]
        fourgrams_step5[[i]] <- tmp
}

fourgrams_final <- bind_rows(fourgrams_step5) %>% select(word1_2_3, word4)
rm(corpus_fourgrams, fourgrams_step1, fourgrams_step2, fourgrams_step3,
   fourgrams_step4, fourgrams_step5)

save(fourgrams_final, file = "fourgrams_final.Rdata")