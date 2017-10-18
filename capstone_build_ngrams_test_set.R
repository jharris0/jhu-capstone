# tokenize test set for 6-grams
library(dplyr)
library(tidyr)

load(file = "corpus_test.Rdata")

tokenizeNgrams <- function(x, level) {
  library(tidytext)
  ngrams <- x %>%
    unnest_tokens(sentence, text, token = "sentences") %>%
    unnest_tokens(ngram, sentence, token = "ngrams", n = level)
  return(ngrams)
}

corpus_df <- corpus_test %>%
  mutate(linenumber = row_number()) %>%
  select(linenumber, text)

corpus_sixgrams <- tokenizeNgrams(corpus_df, 6)

sixgrams_test_set <- corpus_sixgrams %>%
  separate(ngram, c("word1", "word2", "word3", "word4", "word5", "word6"), sep = " ") %>%
  mutate(word1_2_3_4_5 = paste(word1, word2, word3, word4, word5)) %>%
  select(word1_2_3_4_5, word6)

save(sixgrams_test_set, file = "sixgrams_test_set.Rdata")