library(dplyr)
library(data.table)

load("bigrams_final.Rdata")
load("trigrams_final.Rdata")
load("fourgrams_final.Rdata")
load("fivegrams_final.Rdata")
load("sevengrams_test_set.Rdata")

setDT(bigrams_final)
setDT(trigrams_final)
setDT(fourgrams_final)
setDT(fivegrams_final)

setkey(bigrams_final, word1)
setkey(trigrams_final, word1_2)
setkey(fourgrams_final, word1_2_3)
setkey(fivegrams_final, word1_2_3_4)

# unAdverb <- function(string) {
#         tmp <- string
#         tmp <- gsub("very|really|extremely|totally|seriously|super|ultra|pretty|quite|kinda|sorta", "", tmp)
#         return(tmp)
# }

getNext <- function(string) {
        tmp <- strsplit(string, " ") %>% unlist
        # main branch
        last_four <- tail(tmp, n = 4)
        four_words <- paste(last_four[1], last_four[2], last_four[3],
                            last_four[4])
        three_words <- paste(last_four[2], last_four[3], last_four[4])
        two_words <- paste(last_four[3], last_four[4])
        one_word <- last_four[4]
        
        match_4 <- fivegrams_final[.(four_words)]
        names(match_4)[names(match_4) == "word5"] <- "prediction"
        match_4$gram <- "fourgram"

        match_3 <- fourgrams_final[.(three_words)]
        names(match_3)[names(match_3) == "word4"] <- "prediction"
        match_3$prob <- match_3$prob * 0.4
        match_3$gram <- "trigram"

        match_2 <- trigrams_final[.(two_words)]
        names(match_2)[names(match_2) == "word3"] <- "prediction"
        match_2$prob <- match_2$prob * 0.16
        match_2$gram <- "bigram"

        match_1 <- bigrams_final[.(one_word)]
        names(match_1)[names(match_1) == "word2"] <- "prediction"
        match_1$prob <- match_1$prob * 0.064
        match_1$gram <- "unigram"
        
        df_matches <- rbind(match_1[,2:5], match_2[,2:5], match_3[,2:5],
                            match_4[,2:5])

        df_matches <- rbind(df_matches, data.frame(prediction = "the",
                                                   N2 = 0,
                                                   prob = 0.00001,
                                                   gram = "no_match"))
        
        # order by prob descending and slice first row
        final_prediction <- df_matches[order(-prob)][1,]
        
        # return predicted word and n-gram type
        return(c(as.character(final_prediction$prediction),
                 as.character(final_prediction$gram)))
}

set.seed(112)

sample_size <- 3000
test_set <- sample_n(sevengrams_test_set, sample_size)
getNext_output <- lapply(test_set$word1_2_3_4_5_6, getNext)
nGram_type <<- lapply(getNext_output, '[[', 2)
predictions <<- lapply(getNext_output, '[[', 1)

results <- test_set$word7 == predictions & !is.na(predictions)
correct_percentage <- sum(results) / sample_size * 100

results_by_ngram_type <- do.call(rbind.data.frame, Map('c', results, nGram_type))
colnames(results_by_ngram_type) <- c("result","ngram_type")

correct_by_ngram <- results_by_ngram_type %>% group_by(ngram_type, result) %>% 
  summarise (n = n()) %>% mutate(freq = n / sum(n)) %>%
  group_by(ngram_type) %>% mutate(sum_freq = sum(n))

correct_by_ngram <- correct_by_ngram %>% filter(result == "TRUE") %>%
  mutate(accuracy = freq * 100) %>% mutate(accuracy = round(accuracy, 2)) %>%
  mutate(pct_of_total = sum_freq / sample_size * 100) %>%
  select(-c(n, sum_freq, freq)) %>%
  arrange(desc(accuracy))

print(correct_percentage)
print(correct_by_ngram)