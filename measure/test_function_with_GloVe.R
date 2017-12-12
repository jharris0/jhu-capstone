# topic model aware algorithm

library(dplyr)
library(data.table)
library(tm)
library(text2vec)

load("bigrams_final.Rdata")
load("trigrams_final.Rdata")
load("fourgrams_final.Rdata")
load("fivegrams_final.Rdata")
load("sevengrams_test_set.Rdata")
load("word_vectors.Rdata")

setDT(bigrams_final)
setDT(trigrams_final)
setDT(fourgrams_final)
setDT(fivegrams_final)

setkey(bigrams_final, word1)
setkey(trigrams_final, word1_2)
setkey(fourgrams_final, word1_2_3)
setkey(fivegrams_final, word1_2_3_4)

stp_words <- tm::stopwords("en")

getNext <- function(string) {
        
        tmp <- strsplit(string, " ") %>% unlist
        
        # GET VECTORS FOR INPUT STRING
        tmp_wv <- tmp
        # remove stop words
        tmp_wv <- tmp_wv[-which(tmp_wv %in% stp_words)]
        # remove any words of length = 1
        indices <- which(nchar(tmp_wv) == 1)
        if (length(indices) != 0) {
                tmp_wv <- tmp_wv[-indices]
        }
        # get last six (or less) words
        tmp_wv <- tail(tmp_wv, 6)
        # pre-assign vector object
        vector <- 0
        # iterate through the six words and build vectors
        
        if (all(tmp_wv %in% dimnames(word_vectors)[[1]]) && length(tmp_wv) > 0) {
                no_vector <- FALSE
                for(i in 1:length(tmp_wv)) {
                        vec_tmp <- word_vectors[tmp_wv[i], , drop = FALSE]
                        vector <- vector + vec_tmp
                }        
        } else {
                no_vector <- TRUE
        }
        
        
        # get distance
        if (no_vector != TRUE) {
                cos_sim <- sim2(x = word_vectors, y = vector, method = "cosine", norm = "l2")        
        }
        
        # BUILD MATCH TABLE FROM NGRAM TABLES
        
        last_four <- tail(tmp, n = 4)
        four_words <- paste(last_four[1], last_four[2], last_four[3],
                            last_four[4])
        three_words <- paste(last_four[2], last_four[3], last_four[4])
        two_words <- paste(last_four[3], last_four[4])
        one_word <- last_four[4]

        match_4 <- fivegrams_final[.(four_words)]
        names(match_4)[names(match_4) == "word5"] <- "prediction"
        match_4$ngrm_lvl <- "fourgram"

        match_3 <- fourgrams_final[.(three_words)]
        names(match_3)[names(match_3) == "word4"] <- "prediction"
        match_3$prob <- match_3$prob * 0.4
        match_3$ngrm_lvl <- "trigram"

        match_2 <- trigrams_final[.(two_words)]
        names(match_2)[names(match_2) == "word3"] <- "prediction"
        match_2$prob <- match_2$prob * 0.16
        match_2$ngrm_lvl <- "bigram"

        match_1 <- bigrams_final[.(one_word)]
        names(match_1)[names(match_1) == "word2"] <- "prediction"
        match_1$prob <- match_1$prob * 0.064
        match_1$ngrm_lvl <- "unigram"
        
        df_matches <- rbind(match_1[,2:5], match_2[,2:5], match_3[,2:5],
                            match_4[,2:5])
                
        df_matches <- rbind(df_matches, data.frame(prediction = "the",
                                                   N2 = 0,
                                                   prob = 0.00001,
                                                   ngrm_lvl = "no_match"))
        
        df_matches <- df_matches[complete.cases(df_matches)]
        df_matches$prediction <- as.character(df_matches$prediction)
        
        if (no_vector != TRUE) {
                for (i in 1:length(df_matches$prediction)) {
                        if (df_matches$prediction[i] %in% dimnames(word_vectors)[[1]]) {
                                df_matches$dist[i] <- (cos_sim[df_matches$prediction[i],1] + 0.9)
                        } else {
                                df_matches$dist[i] <- 1
                        }
                }        
        } else {
                df_matches$dist <- 1
        }
        
        df_matches$prob_adj <- df_matches$prob * df_matches$dist
        
        final_prediction <- df_matches[order(-prob_adj)][1,]
        
        # return predicted word and n-gram type
        return(c(as.character(final_prediction$prediction),
                 as.character(final_prediction$ngrm_lvl),
                 as.character(no_vector)))
                
}
                
set.seed(78)

sample_size <- 1000
test_set <- sample_n(sevengrams_test_set, sample_size)
getNext_output <- lapply(test_set$word1_2_3_4_5_6, getNext)
nGram_type <<- lapply(getNext_output, '[[', 2)
predictions <<- lapply(getNext_output, '[[', 1)
is_vector <- lapply(getNext_output, '[[', 3)

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