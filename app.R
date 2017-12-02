library(shiny)
library(shinydashboard)
library(dplyr)
library(data.table)
library(tm)
library(text2vec)
library(stringr)

# in function, need to add part to set everything to lower case

stp_words <- stopwords("en")

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
        four_words <- tail(tmp, 4) %>% paste(collapse = " ")
        three_words <- tail(tmp, 3) %>% paste(collapse = " ")
        two_words <- tail(tmp, 2) %>% paste(collapse = " ")
        one_word <- tail(tmp, 1)
        
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
        final_prediction <- df_matches[order(-prob_adj)]
        # return predicted word and n-gram type
        return(final_prediction)
}

load("data/bigrams_final.Rdata")
load("data/trigrams_final.Rdata")
load("data/fourgrams_final.Rdata")
load("data/fivegrams_final.Rdata")
load("data/word_vectors.Rdata")

setDT(bigrams_final)
setDT(trigrams_final)
setDT(fourgrams_final)
setDT(fivegrams_final)

setkey(bigrams_final, word1)
setkey(trigrams_final, word1_2)
setkey(fourgrams_final, word1_2_3)
setkey(fivegrams_final, word1_2_3_4)

ui <- dashboardPage(
        dashboardHeader(title = "NextWord Predictor!",
                        titleWidth = 260),
        dashboardSidebar(
                width = 260,
                textInput("someText", label = "Type or paste some text here:", width = "100%"),
                actionButton(inputId = "submit",
                             label = "Predict Next Word")
        ),
        dashboardBody(
                tags$head(tags$style(HTML('.main-header .logo {
                                          font-family: "Segoe UI", Frutiger, "Frutiger Linotype", "Dejavu Sans", "Helvetica Neue", Arial, sans-serif;
                                          font-weight: bold;
                                          font-size: 24px;
                                          letter-spacing: -2px;
                                          font-style: normal;
                                          font-variant: normal;
                                          font-weight: 500;}'))),
                fluidRow(
                        box(
                                width  = 5,
                                title = "Welcome!",
                                background = "teal",
                                collapsible = TRUE,
                                "This web app is an interface to a word prediction algorithm I created for the
                                Johns Hopkins University Data Science Specialization on Coursera. The algorithm
                                combines an n-gram backoff model and a GloVe model to predict the next word in a text
                                sequence.",
                                br(),br(),
                                "Code for the algorithm and this web app are available at https://github.com/jharris0/next-word.",
                                br(),br(),
                                "To get started, please input some text at left and click the \"Predict Next Word\" button."
                        )
                        ),
                conditionalPanel(
                        condition = "output.dataLoaded!=1",
                        fluidRow(
                                box(
                                        width  = 5,
                                        title = "Best Guess", solidHeader = TRUE, status = "success",
                                        tags$div(
                                                textOutput("original_text"),
                                                textOutput("prediction_text")
                                                ),
                                        tags$style("#original_text, #prediction_text {display:inline; font-size:1.3em; margin-bottom:1em}"),
                                        tags$style("#prediction_text {font-weight:bold}")
                                        )
                                ),
                        fluidRow(
                                box(
                                        width  = 5,
                                        collapsible = TRUE,
                                        tags$p(
                                        "The following table shows the possible options generated by the algorithm,
                                               ranked by adjusted probability (\"prob_adj\"). Some notes:"),
                                        tags$ul(
                                                tags$li("The \"prob\" value includes \"stupid backoff\" discounting for lower level n-grams." ),
                                                tags$li("Adjusted probability can exceed 1 due to how GloVe model data is used to boost
                                                probability values."),
                                                tags$li("GloVe model vectors are only used when input tokens and output options both exist
                                                        in the GloVe model. The model excludes common English stop words, so many predictions won't
                                                        use vectors, and adjusted probability will be the same as the standard probability. When a
                                                        vector value hasn't been calculated for a prediction, \"dist\" is set to 1.")
                                        ),
                                        title = "Under the Hood", solidHeader = TRUE, status = "primary",
                                        tableOutput("ngram_table")
                                )
                        )
                        )
                )
        )

server <- function(input, output) {
        rv_data = reactiveValues(prediction_table = NULL)
        observeEvent(eventExpr = input[["submit"]],
                     handlerExpr = {
                             text <- str_to_lower(input$someText)
                             text <- gsub('[[:punct:] ]+',' ', text)
                             prediction_table <- getNext(text)
                             rv_data$prediction_table <- prediction_table %>% select(-N2)
                             rv_data$top_prediction <- prediction_table[1,]
                             rv_data$input_text <- input$someText
                     })
        output$dataLoaded <- reactive({
                is.null(rv_data$prediction_table)
        })
        outputOptions(output, "dataLoaded", suspendWhenHidden = FALSE)
        output$original_text <- renderText({
                paste0(rv_data$input_text, " ")
                })
        output$prediction_text <- renderText({
                as.character(rv_data$top_prediction$prediction)
                })
        output$ngram_table <- renderTable(rv_data$prediction_table, digits = 4)
        }

shinyApp(ui = ui, server = server)