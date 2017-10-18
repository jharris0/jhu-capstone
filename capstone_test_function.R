library(dplyr)
library(data.table)

load("bigrams_final.Rdata")
load("trigrams_final.Rdata")
load("fourgrams_final.Rdata")
load("fivegrams_final.Rdata")
load("sixgrams_test_set.Rdata")

setDT(bigrams_final)
setDT(trigrams_final)
setDT(fourgrams_final)
setDT(fivegrams_final)

setkey(bigrams_final, word1)
setkey(trigrams_final, word1_2)
setkey(fourgrams_final, word1_2_3)
setkey(fivegrams_final, word1_2_3_4)

getNext <- function(string) {
        tmp <- strsplit(string, " ")
        tmp_length <- length(tmp[[1]])
        
        if(tmp_length >= 4) {
                last_four <- tail(tmp[[1]], n =4)
                four_words <- paste(last_four[1], last_four[2], last_four[3],
                                    last_four[4])
                three_words <- paste(last_four[2], last_four[3], last_four[4])
                two_words <- paste(last_four[3], last_four[4])
                one_word <- last_four[4]
                fivegrams_option <- fivegrams_final[.(four_words)]
                
                if (!is.na(fivegrams_option$word5)) {
                        return(c(fivegrams_option$word5, "fivegram"))
                } else {
                        fourgrams_option <- fourgrams_final[.(three_words)]
                        if (!is.na(fourgrams_option$word4)) {
                        return(c(fourgrams_option$word4, "fourgram"))
                } else {
                        trigrams_option <- trigrams_final[.(two_words)]
                        if (!is.na(trigrams_option$word3)) {
                                return(c(trigrams_option$word3, "trigram"))
                        } else {
                                bigrams_option <- bigrams_final[.(one_word)]
                                if (!is.na(bigrams_option$word2)) {
                                        return(c(bigrams_option$word2, "bigram"))
                                } else {
                                        return(c(NA, NA))
                                }
                                # last else  only works if length of string 4, 3
                                # or greater - replicate for other cases later
                                
                        }
                }
                }      
        } 
        else if (tmp_length >= 3) {
                last_three <- tail(tmp[[1]], n = 3)
                three_words <- paste(last_three[1], last_three[2],
                                     last_three[3])
                two_words <- paste(last_three[2], last_three[3])
                one_word <- last_three[3]
                fourgrams_option <- fourgrams_final[.(three_words)]
                if (!is.na(fourgrams_option$word4)) {
                        return(c(fourgrams_option$word4, "fourgram"))
                } else {
                        trigrams_option <- trigrams_final[.(two_words)]
                        if (!is.na(trigrams_option$word3)) {
                                return(c(trigrams_option$word3, "trigram"))
                        } else {
                                bigrams_option <- bigrams_final[.(one_word)]
                                if (!is.na(bigrams_option$word2)) {
                                return(c(bigrams_option$word2, "bigram"))
                                } else {
                                        return(c(NA, NA))
                                }
                                # last else  only works if length of string 3 or
                                # greater - replicate for other cases later
                                
                        }
                }
        } else if(tmp_length == 2) {
                last_two <- tail(tmp[[1]], n = 2)
                two_words <- paste(last_two[1], last_two[2])
                one_word <- last_two[2]
                trigrams_option <- trigrams_final[.(two_words)]
                if (!is.na(trigrams_option$word3)) {
                        return(c(trigrams_option$word3, "trigram"))
                } else {
                        bigrams_option <- bigrams_final[.(one_word)]
                        return(c(bigrams_option$word2, "bigram"))
                }
        } else if(tmp_length == 1) {
                last_one <- tail(tmp[[1]], n = 1)
                one_word <- last_one
                bigrams_option <- bigrams_final[.(one_word)]
                return(c(bigrams_option$word2, "bigram"))
        }
}

sample_size <- 10000
test_set <- sample_n(sixgrams_test_set, sample_size)
getNext_output <- lapply(test_set$word1_2_3_4_5, getNext)
nGram_type <- lapply(getNext_output, '[[', 2)
predictions <- lapply(getNext_output, '[[', 1)

total_NA <- sum(is.na(predictions))
sample_size_excl_NA <- sample_size - total_NA
results <- test_set$word6 == predictions & !is.na(predictions)
NA_percentage <- total_NA / sample_size * 100
correct_percentage <- sum(results) / sample_size * 100
correct_percentage_excl_NA <- sum(results) / sample_size_excl_NA * 100

nGram_type_NA <- sum(is.na(nGram_type))
nGram_type_bigram <- nGram_type == "bigram"
nGram_type_trigram <- nGram_type == "trigram"
nGram_type_fourgram <- nGram_type == "fourgram"
nGram_type_fivegram <- nGram_type == "fivegram"

nGram_type_total <- sample_size - nGram_type_NA
bigram_percentage <- sum(nGram_type_bigram) / nGram_type_total * 100
trigram_percentage <- sum(nGram_type_trigram) / nGram_type_total * 100
fourgram_percentage <- sum(nGram_type_fourgram) / nGram_type_total * 100
fivegram_percentage <- sum(nGram_type_fivegram) / nGram_type_total * 100

results_by_ngram_type <- do.call(rbind.data.frame, Map('c', results, nGram_type))
colnames(results_by_ngram_type) <- c("result","ngram_type")

correct_by_ngram <- results_by_ngram_type %>% group_by(ngram_type, result) %>% 
                                 summarise (n = n()) %>%
                                 mutate(freq = n / sum(n))

print(paste0(correct_percentage, "% correct (including NAs), ",
             correct_percentage_excl_NA, "% correct (not including NAs), ",
             NA_percentage, "% NA"))
print(paste0("2-gram ratio (%): ", bigram_percentage, ", 3-gram ratio (%):",
             trigram_percentage, ", 4-gram ratio (%): ", fourgram_percentage,
             " 5-gram ratio (%): ", fivegram_percentage))
print(correct_by_ngram)