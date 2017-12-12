# Tokenize test set for 7-grams
library(dplyr)
library(tidyr)

tokenizeNgrams <- function(x, level) {
        library(tidytext)
        ngrams <- x %>%
                unnest_tokens(sentence, text, token = "sentences") %>%
                unnest_tokens(ngram, sentence, token = "ngrams", n = level)
        return(ngrams)
}

load(file = "corpus_test_clean.Rdata")

# Prep data for tokenizing using tidytext package
corpus_df <- corpus_test_clean %>%
        mutate(linenumber = row_number()) %>%
        select(linenumber, text)

# Do the tokenizing
corpus_sevengrams <- tokenizeNgrams(corpus_df, 7)

# Split into two columns: first 6 words, and 7th word
sevengrams_test_set <- corpus_sevengrams %>%
        separate(ngram, c("word1", "word2", "word3", "word4", "word5", "word6", "word7"), sep = " ") %>%
        mutate(word1_2_3_4_5_6 = paste(word1, word2, word3, word4, word5, word6)) %>%
        select(word1_2_3_4_5_6, word7)

save(sevengrams_test_set, file = "sevengrams_test_set.Rdata")