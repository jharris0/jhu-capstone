library(dplyr)
library(data.table)
setDT(bigrams_final)
setDT(trigrams_final)
setDT(fourgrams_final)
setkey(bigrams_final, word1)
setkey(trigrams_final, word1_2)
setkey(fourgrams_final, word1_2_3)

getNext <- function(string) {
        tmp <- strsplit(string, " ")
        tmp_length <- length(tmp[[1]])
        if (tmp_length >= 3) {
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
                                return(c(bigrams_option$word2, "bigram"))
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