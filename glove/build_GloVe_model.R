# Builds a GloVe model using text2vec package. On a Windows VM with 16 GB RAM
# and 8 vCPUs, building the model using the settings shown below took about
# 3-4 minutes, if I recall correctly.

library(readr)
library(tm)
library(text2vec)

sampleCorpus <- function(corpus_name, percent) {
  totalLines <- length(corpus_name)
  linesToRead <- round(percent/100 * totalLines)
  result <- sample(corpus_name, linesToRead)
  return(result)
}

# See "pre-process_corpus.R" for details on the function below
cleanCorpus <- function(corpus_name) {
  result <- gsub("RT : ", "", corpus_name)
  result <- gsub("([0-9]|0[0-9]|1[0-9]|2[0-3]):[0-5][0-9]", "", result)
  result <- gsub(":", "", result)
  result <- gsub("\\d+(?!\\w)", "", perl = TRUE, result)
  result <- gsub(",", "", result)
  result <- gsub("<3", "", result)
  result <- gsub("((?::|;|=)(?:-)?(?:\\)|D|P))", "", result)
  result <- gsub("\"", "", result)
  return(result)
}

set.seed(123)

# Following section ingests text from corpus at specified sample percentage
con <- file("corpora/en_us/en_us.blogs.txt", encoding = "UTF-8")
blogs <- read_lines(con)
blogs_sample <- sampleCorpus(blogs, 60)
rm(blogs) # To preserve memory

con <- file("corpora/en_us/en_us.news.txt", encoding = "UTF-8")
news <- read_lines(con)
news_sample <- sampleCorpus(news, 60)
rm(news)

con <- file("corpora/en_us/en_us.twitter.txt", encoding = "UTF-8")
twitter <- read_lines(con)
twitter_sample <- sampleCorpus(twitter, 60)
rm(twitter)

# Prep data for GloVe model building
corpus <- c(blogs_sample, news_sample, twitter_sample)
corpus <- iconv(corpus, "latin1", "ASCII", sub="")
corpus <- cleanCorpus(corpus)
# Convert to VCorpus so we can run some tm package text processing functions
vcorpus <- VCorpus(VectorSource(corpus))
vcorpus <-tm_map(vcorpus,content_transformer(tolower))
profanity <- read_lines("http://www.bannedwordlist.com/lists/swearWords.txt")
vcorpus <- tm_map(vcorpus, removeWords, profanity)
vcorpus <- tm_map(vcorpus, removePunctuation)
# Unconvert from VCorpus; this method is much faster than tm's Corpus command
corpus_clean_1 <- data.frame(text=unlist(sapply(vcorpus, `[`, "content")),
                             stringsAsFactors=F)
corpus_clean_2 <- as.matrix(corpus_clean_1)
corpus_clean_3 <- as.vector(corpus_clean_2)
corpus_clean <- corpus_clean_3
# Remove some leftover junk symbols
corpus_clean <- gsub(",|\"", "", corpus_clean)
corpus_clean <- gsub("\\n", "", corpus_clean)

# https://cran.r-project.org/web/packages/text2vec/vignettes/glove.html
# Tokenize corpus. Also eliminate some tokens that probably don't have
# significant predictive value; this also helps reduce the size of the model.
tokens <- space_tokenizer(corpus_clean)
it = itoken(tokens, progressbar = FALSE)
vocab <- create_vocabulary(it)
# Get rid of any tokens that appear less than 10 times
pruned_vocab <- prune_vocabulary(vocabulary = vocab, term_count_min = 10L)
# Get rid of tokens that are English stop words
stp_words <- tm::stopwords("en")
pruned_vocab <- pruned_vocab[-which(pruned_vocab$term %in% stp_words),]
# Get rid of tokens that are single characters
pruned_vocab <- pruned_vocab[-which(nchar(pruned_vocab$term) == 1),]

vectorizer <- vocab_vectorizer(pruned_vocab)
tcm <- create_tcm(it, vectorizer, skip_grams_window = 6L)

glove = GlobalVectors$new(word_vectors_size = 50, vocabulary = pruned_vocab, x_max = 10)
wv_main = glove$fit_transform(tcm, n_iter = 20)

# Next step sort of recommended by package developer, possibly unnecessary?
wv_context = glove$components
word_vectors = wv_main + t(wv_context)

save(word_vectors, file = "word_vectors.Rdata")

# GloVe model usage example, taken from package documentation:
#
#   berlin = word_vectors["paris", , drop = FALSE] -
#     word_vectors["france", , drop = FALSE] +
#     word_vectors["germany", , drop = FALSE] 
#   cos_sim = sim2(x = word_vectors, y = berlin, method = "cosine", norm = "l2")
#   head(sort(cos_sim[,1], decreasing = TRUE), 60)
#
#   Should give results like Berlin, Rome, etc.
# 
# To get distance for a specific term from a selection of other terms:
#
#   cos_sim["rome",1]





