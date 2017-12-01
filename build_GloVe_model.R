# setwd("jhu-capstone")

library(readr)
library(tm)
library(text2vec)

sampleCorpus <- function(corpus_name, percent) {
  totalLines <- length(corpus_name)
  linesToRead <- round(percent/100 * totalLines)
  result <- sample(corpus_name, linesToRead)
  return(result)
}

cleanCorpus <- function(corpus_name) {
  # remove RT
  result <- gsub("RT : ", "", corpus_name)
  # replace clock times
  result <- gsub("([0-9]|0[0-9]|1[0-9]|2[0-3]):[0-5][0-9]", "", result)
  # remove colon - related to preventing unigrams like "00pm"
  result <- gsub(":", "", result)
  # replace numbers like "2017" but not "1st"
  result <- gsub("\\d+(?!\\w)", "", perl = TRUE, result)
  # remove commas (related to number/char combos like "1,000,000th")
  result <- gsub(",", "", result)
  # remove heart text emojis
  result <- gsub("<3", "", result)
  # remove smiley https://stackoverflow.com/questions/5862490/how-to-match-emoticons-with-regular-expressions
  result <- gsub("((?::|;|=)(?:-)?(?:\\)|D|P))", "", result)
  # result <- gsub("[[:punct:]]", test, replacement=" ")
  result <- gsub("\"", "", result)
  return(result)
}

set.seed(123)

con <- file("corpora/en_us/en_us.blogs.txt", encoding = "UTF-8")
blogs <- read_lines(con)
blogs_sample <- sampleCorpus(blogs, 60)
rm(blogs)

con <- file("corpora/en_us/en_us.news.txt", encoding = "UTF-8")
news <- read_lines(con)
news_sample <- sampleCorpus(news, 60)
rm(news)

con <- file("corpora/en_us/en_us.twitter.txt", encoding = "UTF-8")
twitter <- read_lines(con)
twitter_sample <- sampleCorpus(twitter, 60)
rm(twitter)

# next 10 lines are possibly the most inefficient way to do this
corpus <- c(blogs_sample, news_sample, twitter_sample)
corpus <- iconv(corpus, "latin1", "ASCII", sub="")
corpus <- cleanCorpus(corpus)
vcorpus <- VCorpus(VectorSource(corpus))
vcorpus <-tm_map(vcorpus,content_transformer(tolower))
# http://rpubs.com/eKtorpKamprad/DS_Capstone_WK2_Milestone
profanity <- read_lines("http://www.bannedwordlist.com/lists/swearWords.txt")
vcorpus <- tm_map(vcorpus, removeWords, profanity)
vcorpus <- tm_map(vcorpus, removePunctuation)
#corpus_clean <- Corpus(VectorSource(vcorpus))
corpus_clean_1 <- data.frame(text=unlist(sapply(vcorpus, `[`, "content")),
                                 stringsAsFactors=F)
corpus_clean_2 <- as.matrix(corpus_clean_1)
corpus_clean_3 <- as.vector(corpus_clean_2)
corpus_clean <- corpus_clean_3

# save(corpus_clean, file = "corp.Rdata")

corpus_clean <- gsub(",|\"", "", corpus_clean)
corpus_clean <- gsub("\\n", "", corpus_clean)

# https://cran.r-project.org/web/packages/text2vec/vignettes/glove.html
tokens <- space_tokenizer(corpus_clean)
it = itoken(tokens, progressbar = FALSE)
vocab <- create_vocabulary(it)

pruned_vocab <- prune_vocabulary(vocabulary = vocab, term_count_min = 10L)

stp_words <- tm::stopwords("en")

pruned_vocab <- pruned_vocab[-which(pruned_vocab$term %in% stp_words),]
pruned_vocab <- pruned_vocab[-which(nchar(pruned_vocab$term) == 1),]

vectorizer <- vocab_vectorizer(pruned_vocab)
tcm <- create_tcm(it, vectorizer, skip_grams_window = 6L)

glove = GlobalVectors$new(word_vectors_size = 50, vocabulary = pruned_vocab, x_max = 10)
wv_main = glove$fit_transform(tcm, n_iter = 20)

wv_context = glove$components
word_vectors = wv_main + t(wv_context)

save(word_vectors, file = "word_vectors.Rdata")

# berlin = word_vectors["paris", , drop = FALSE] -
#   word_vectors["france", , drop = FALSE] +
#   word_vectors["germany", , drop = FALSE] 
#   # # word_vectors["bought", , drop = FALSE] 
#   #word_vectors["beer", , drop = FALSE] #+
#   # word_vectors["case", , drop = FALSE] -
# cos_sim = sim2(x = word_vectors, y = berlin, method = "cosine", norm = "l2")
# head(sort(cos_sim[,1], decreasing = TRUE), 60)
# 
# cos_sim["coffee",1]
# 





