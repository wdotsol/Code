# Data Science assignment ----
# -------------------> Preamble <---------------------
# Assignment 1 Data science 
cat("\014")
rm(list = ls())
set.seed(123)
Sys.setlocale("LC_TIME", "English")
dirpath  = dirname(rstudioapi::getSourceEditorContext()$path) # get the folder of the script

setwd(dirpath) # move to the script folder

# Load libraries
library(tm)
library(pdftools)
library(stringr)
library(dplyr)
library(SnowballC)
library(wordcloud)
library(tidyverse)
library(ggplot2)
library(gofastr)
library(ldatuning)
library(topicmodels)
library(reshape2)
library(pals)


# Task 1 ----

# Q1 ----
# Create a path where the transcipts are located
path <- "Transcripts"

files <- list.files(path = path,
                    pattern = "pdf$")

files <- paste(paste(path, "/", sep=""), files, sep="")

# Put the pdfs together in one dataframe with every pdf on one row
filestextDF <- data.frame(Document = files,
                          text = sapply(files, function(x) 
                            paste0(pdf_text(x), collapse = ' '))) 
rownames(filestextDF) <- 1:nrow(filestextDF)


# Create empty lists to store text before and after "Presentation"
before_presentation <- list()
after_presentation <- list()

# Loop through each row of the data frame
for (i in 1:nrow(filestextDF)) {
  # Get the text from the current row
  text <- filestextDF$text[i]
  
  # Split the text on the word "Presentation"
  split_text <- strsplit(text, "Presentation", fixed = TRUE)
  
  # Assign the text before "Presentation to 'sbefore' and the other part to 'safter'
  sbefore <- split_text[[1]][1]
  safter <- split_text[[1]][2]
  
  # Store the text in two lists (they are converted into data frames after the loop)
  before_presentation[[i]] <- sbefore
  after_presentation[[i]] <- safter
}


# Convert the lists to data frames
introduction <- data.frame(text = unlist(before_presentation))
body <- data.frame(text = unlist(after_presentation))


# add a doc_id to create a corpus
introduction <- cbind(doc_id = seq_len(nrow(introduction)), introduction)
body <- cbind(doc_id = seq_len(nrow(body)), body)


# Create corpus for introduction and text
corpusintro <- VCorpus(DataframeSource(introduction))
corpustext <- VCorpus(DataframeSource(body))

# remove capitals, white spaces, stop words, and special characters and perform stemming
# On text corpus
# - remove punctuation
corpustext <- tm_map(corpustext, removePunctuation)
# - convert text to lowercase
corpustext <- tm_map(corpustext, content_transformer(tolower))
# - remove stopwords (e.g., the, of, in, etc.)
corpustext <- tm_map(corpustext, removeWords, stopwords("english"))
# - remove special characters
removeSpecialChars <- function(x) gsub("[^a-zA-Z0-9 ]","",x)
corpustext <- tm_map(corpustext, content_transformer(removeSpecialChars))
# - stem the words
corpustext <- tm_map(corpustext, stemDocument)
# - remove numbers
corpustext <- tm_map(corpustext, removeNumbers)
# - remove white space
corpustext <- tm_map(corpustext, stripWhitespace)

# On introduction data
# - remove punctuation
corpusintro <- tm_map(corpusintro, removePunctuation)
# - convert text to lowercase
corpusintro<- tm_map(corpusintro, content_transformer(tolower))
# - remove stopwords (e.g., the, of, in, etc.)
corpusintro <- tm_map(corpusintro, removeWords, stopwords("english"))
# - remove special characters
removeSpecialChars <- function(x) gsub("[^a-zA-Z0-9 ]","",x)
corpusintro <- tm_map(corpusintro, content_transformer(removeSpecialChars))
# - remove numbers
corpusintro <- tm_map(corpusintro, removeNumbers)
# - stem the words
corpusintro <- tm_map(corpusintro, stemDocument)
# - remove white space
corpusintro <- tm_map(corpusintro, stripWhitespace)

print(stopwords("english"))

# Make a list of all the words from the introduction corpus
introwords <- unlist(lapply(corpusintro, function(doc) {
  paste(unlist(strsplit(as.character(doc$content), "\\s+")), collapse = " ")
}))
# Split the words by spaces
words <- unlist(strsplit(introwords, "\\s+"))
# Only keep unique words, so we don't have words more than 1 time in our list
uniquewords <- unique(words)
# Remove all the unique words from the intro corpus from the text corpus and 
# create a new corpus with this
corpus <- tm_map(corpustext, removeWords, uniquewords)


# Remove irrelevant words
myStopWords <- c("like", "think", "thank", "quarter", "just", "year", "will", "also", "now", "see", 
                 "pleas", "across", "lot", "can", "there", "time", "one", "two", "look", "much", 
                 "question", "around", "take", "yes", "think", "welcom", "come", "get", "want", 
                 "next", "follow", "yeah", "let", "today", "morn", "say", "that", "talk", "set", 
                 "last", "thing", "littl", "bit", "go", "ahead", "make", "sense", "dont", "know", 
                 "afternoon", "youv", "seen", "weve", "line", "okay", "everyon", "hey", "ladie", "gentlemen")

corpus <- tm_map(corpus, removeWords, myStopWords)


# Q2 ----
library(wordcloud)

# List of companies to search for
companies <- c("Amazon", "Apple", "ASML", "Caterpillar", "Coca-Cola", "Tesla")

# Function to find the company from the title
find_company <- function(title) {
  for (company in companies) {
    if (grepl(company, title)) {
      return(company)
    }
  }
  return(NA)
}

company_list <- sapply(filestextDF$Document, find_company)

set.seed(123)
# Wordcloud unigrams Amazon
wordcloud(words = corpus[1:19], scale=c(3,0.5), max.words=100, random.order=FALSE, rot.per=0.35, use.r.layout=FALSE, colors=brewer.pal(8, "Dark2"))
title(main = paste("Unigram word cloud for", companies[1]))
# Worcloud unigrams Apple
wordcloud(words = corpus[20:40], scale=c(3,0.5), max.words=100, random.order=FALSE, rot.per=0.35, use.r.layout=FALSE, colors=brewer.pal(8, "Dark2"))
title(main = paste("Unigram word cloud for", companies[2]))
# wordcloud unigrams ASML
wordcloud(words = corpus[41:56], scale=c(3,0.5), max.words=100, random.order=FALSE, rot.per=0.35, use.r.layout=FALSE, colors=brewer.pal(8, "Dark2"))
title(main = paste("Unigram word cloud for", companies[3]))
# wordcloud unigrams Caterpillar
wordcloud(words = corpus[57:75], scale=c(3,0.5), max.words=100, random.order=FALSE, rot.per=0.35, use.r.layout=FALSE, colors=brewer.pal(8, "Dark2"))
title(main = paste("Unigram word cloud for", companies[4]))
# wordcloud unigrams Coca Cola
wordcloud(words = corpus[76:95], scale=c(3,0.5), max.words=100, random.order=FALSE, rot.per=0.35, use.r.layout=FALSE, colors=brewer.pal(8, "Dark2"))
title(main = paste("Unigram word cloud for", companies[5]))
# wordcloud unigrams Tesla
wordcloud(words = corpus[96:114], scale=c(3,0.5), max.words=100, random.order=FALSE, rot.per=0.35, use.r.layout=FALSE, colors=brewer.pal(8, "Dark2"))
title(main = paste("Unigram word cloud for", companies[6]))

#Bigrams
set.seed(123)
library(RWeka)
# When constructing the term document matrix (tdm) we need to use function “BigramTokenizer” to create tokens
BigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2))
# Create term document matrix 
tdm_bigram_amazon = TermDocumentMatrix(corpus[1:19], control = list(tokenize = BigramTokenizer))
tdm_bigram_apple = TermDocumentMatrix(corpus[20:40], control = list(tokenize = BigramTokenizer))
tdm_bigram_asml = TermDocumentMatrix(corpus[41:56], control = list(tokenize = BigramTokenizer))
tdm_bigram_cat = TermDocumentMatrix(corpus[57:75], control = list(tokenize = BigramTokenizer))
tdm_bigram_cocacola = TermDocumentMatrix(corpus[76:95], control = list(tokenize = BigramTokenizer))
tdm_bigram_tesla = TermDocumentMatrix(corpus[96:114], control = list(tokenize = BigramTokenizer))

# Extract the frequency of each bigram and analyse the thirty most frequent ones.
# (1) Transform document-term matrix to matrix, (2) sum frequency of occurrence across all rows to get total frequency across all documents, 
#     and (3)sort from high to low, until the 30 most frequent terms have been reached
freq_amazon = sort(rowSums(as.matrix(tdm_bigram_amazon)),decreasing = TRUE)[1:30]
freq_apple = sort(rowSums(as.matrix(tdm_bigram_apple)),decreasing = TRUE)[1:30]
freq_asml = sort(rowSums(as.matrix(tdm_bigram_asml)),decreasing = TRUE)[1:30]
freq_cat = sort(rowSums(as.matrix(tdm_bigram_cat)),decreasing = TRUE)[1:30]
freq_cocacola = sort(rowSums(as.matrix(tdm_bigram_cocacola)),decreasing = TRUE)[1:30]
freq_tesla = sort(rowSums(as.matrix(tdm_bigram_tesla)),decreasing = TRUE)[1:30]

# Create a data frame with in column 1 the bigrams and in column 2 the corresponding frequency (number)
freq_amazon.df = data.frame(word=names(freq_amazon), frequency=freq_amazon)
freq_apple.df = data.frame(word=names(freq_apple), frequency=freq_apple)
freq_asml.df = data.frame(word=names(freq_asml), frequency=freq_asml)
freq_cat.df = data.frame(word=names(freq_cat), frequency=freq_cat)
freq_cocacola.df = data.frame(word=names(freq_cocacola), frequency=freq_cocacola)
freq_tesla.df = data.frame(word=names(freq_tesla), frequency=freq_tesla)


# Plot the bigram wordcloud
wordcloud(freq_amazon.df$word, freq_amazon.df$frequency, scale=c(3,0.5), max.words=100, random.order=FALSE, use.r.layout=FALSE, colors=brewer.pal(8,"Dark2"))
title(main = paste("Bigram word cloud for", companies[1]))
wordcloud(freq_apple.df$word, freq_apple.df$frequency, scale=c(3,0.5), max.words=1000, random.order=FALSE, use.r.layout=FALSE, colors=brewer.pal(8,"Dark2"))
title(main = paste("Bigram word cloud for", companies[2]))
wordcloud(freq_asml.df$word, freq_asml.df$frequency, scale=c(3,0.5), max.words=1000, random.order=FALSE, use.r.layout=FALSE, colors=brewer.pal(8,"Dark2"))
title(main = paste("Bigram word cloud for", companies[3]))
wordcloud(freq_cat.df$word, freq_cat.df$frequency, scale=c(3,0.5), max.words=1000, random.order=FALSE, use.r.layout=FALSE, colors=brewer.pal(8,"Dark2"))
title(main = paste("Bigram word cloud for", companies[4]))
wordcloud(freq_cocacola.df$word, freq_cocacola.df$frequency ,scale=c(3,0.5), max.words=1000, random.order=FALSE, use.r.layout=FALSE, colors=brewer.pal(8,"Dark2"))
title(main = paste("Bigram word cloud for", companies[5]))
wordcloud(freq_tesla.df$word, freq_tesla.df$frequency, scale=c(3,0.5), max.words=1000, random.order=FALSE, use.r.layout=FALSE, colors=brewer.pal(8,"Dark2"))
title(main = paste("Bigram word cloud for", companies[6]))


# Q3----
# Calculate the sentiment for each firm/quarter observation using the Loughran and McDonald sentiment dictionary. In one graph, plot the time-varying sentiment for all firms over
# all quarters. Interpret your results on a separate slide.

library(SentimentAnalysis)
set.seed(123)
# Add column for company name to the dataframe
filestextDF$company <- company_list

# Search for the dates in the document file
dates <- c("Q1_2019", "Q2_2019", "Q3_2019", "Q4_2019", "Q1_2020", "Q2_2020", "Q3_2020", "Q4_2020", "Q1_2021", "Q2_2021", "Q3_2021", "Q4_2021", "Q1_2022", "Q2_2022", "Q3_2022", "Q4_2022", "Q1_2023", "Q2_2023", "Q3_2023", "Q4_2023", "Q1_2024")


print(quarters)
find_date <- function(title, data) {
  for (date in dates) {
    if (grepl(date, title)) {
      return(date)
    }
  }
  return(NA)
}
date_list <- sapply(filestextDF$Document, find_date)

# Add date to the dataframe and convert years and quarter to date format with zoo package. To do so we also convert the dataset to as_tibble
library(zoo)
library(ggplot2)
library(ggthemes)

filestextDF$date <- date_list
filestextDF <- filestextDF %>% fortify.zoo %>% as_tibble()
filestextDF$date <- as.Date(as.yearqtr(filestextDF$date, format = "Q%q_%Y"))

# extract sentiment
sentiment <- analyzeSentiment(corpus) 
# Add column for company and date to the sentiment df
sentiment$date <- filestextDF$date
sentiment$company <- filestextDF$company


# Plot the sentiment for all firms over all quarters
sentiment %>%
  ggplot(aes(date)) +
  geom_line(aes(y = SentimentLM, color = company)) +
  labs(y = "Sentiment",
       title = "Time-varying sentiment per firm",
       x = "", 
       color = "") + theme_economist_white() 


# Q4 ----
inspect(corpus[[4]])
tail(DictionaryLM$negative, 1000)
# Dictionary-based methods do not capture the meaning of words in a sentence. It assigns a negative, positive or neutral score to a word based on the presence in the negative, positive or neutral dictionary.  
# For example question is in the negative dictionary. However, these conference calls only consists of questions asked to the management. So, people say in the text often: Thank you for your question. Dictionary-based methods will score this with a negative score when this does not say anything about the score. We therefore removed the word question.
# Another example, in the conference call for Amazon 2022 Q1 there is a question: Are you seeing signs of consumer softness or weakening?. Weakening will be assigned a negative score. However, this is only a question and the answer on the question is positive. There is no weakening of customer demand. Dictionary-based methods will assign a falsely negative score. 
# An approved method are large language models. These are deep learning algorithms that can understand and generate human language. These algorithms can be trained that an question like are you seeing signs of customer weakening is not directly something negative and that the answer no, we don't see customer weakening will be something positive instead of negative.


#Question 5 ----
#5.a
topicprob <- read.csv("Word_Weights_By_Topic_Phi.csv")

top20_recession <- head(topicprob[order(topicprob$Recession, decreasing = TRUE),], 20)$term
top20_China <- head(topicprob[order(topicprob$China, decreasing = TRUE),], 20)$term
top20_environment <- head(topicprob[order(topicprob$Environment, decreasing = TRUE),], 20)$term
top20_lawsuit <- head(topicprob[order(topicprob$Lawsuits, decreasing = TRUE),], 20)$term

top_terms_df <- data.frame(Recession = top20_recession,
                           China = top20_China,
                           Environment = top20_environment,
                           Lawsuit = top20_lawsuit)


# Unlist the topic words into four different vectors
unlisted_columns <- lapply(top_terms_df[, c("Recession", "China", "Environment", "Lawsuit")], function(x) unname(unlist(x)))
Recession <- unlisted_columns[[1]]
China <- unlisted_columns[[2]]
Environment <- unlisted_columns[[3]]
Lawsuit <- unlisted_columns[[4]]

# Add columns with zeros to filextextDF per topic 
filestextDF$rec <- replicate(dim(filestextDF)[1], 0)
filestextDF$china <- replicate(dim(filestextDF)[1], 0)
filestextDF$environment <- replicate(dim(filestextDF)[1], 0)
filestextDF$lawsuit <- replicate(dim(filestextDF)[1], 0)

# This is a loop that iterates over each row of the filestextDF. It removes punctuations and converts it to lower case
for (r in 1:dim(filestextDF)[1]) {
  text <- filestextDF[r,c('text')]
  text <- gsub("\\d|[[:punct:]]", "", text)
  text <- tolower(text)
  
  # check proportion of total words that overlaps with the topic words. We do this per topic.
  
  for (kw in 1:length(Recession)){
    filestextDF[r,c('rec')] <- filestextDF[r,c('rec')] + str_count(text, Recession[kw]) / (lengths(gregexpr("[A-z]\\W+", text)) + 1) # this counts the occurrences of the keyword in the preprocessed text using str_count and it divides it by the total number of words. It adds these numbers to the dataset in a new column.
  }
  for (kw in 1:length(China)){
    filestextDF[r,c('china')] <- filestextDF[r,c('china')] + str_count(text, China[kw]) / (lengths(gregexpr("[A-z]\\W+", text)) + 1)
  }
  for (kw in 1:length(Environment)){
    filestextDF[r,c('environment')] <- filestextDF[r,c('environment')] + str_count(text, Environment[kw]) / (lengths(gregexpr("[A-z]\\W+", text)) + 1)
  }
  for (kw in 1:length(Lawsuit)){
    filestextDF[r,c('lawsuit')] <- filestextDF[r,c('lawsuit')] + str_count(text, Lawsuit[kw]) / (lengths(gregexpr("[A-z]\\W+", text)) + 1)
  }
}

# Plot the count of recession words in the transctipts across time
filestextDF %>%
  ggplot(aes(date)) +
  geom_line(aes(y = rec, color = company), size = 0.5) +
  labs(y = "Loading on the China topic",
       title = "Time-varying loading on the recession topic",
       x = "", 
       color = "") + theme_economist_white() 

# Plot the count of china words in the transctipts across time
filestextDF %>%
  ggplot(aes(date)) +
  geom_line(aes(y = china, color = company), size = 0.5) + ylab("Loading on the China topic") + 
  labs(y = "Loading on the China topic",
  title = "Time-varying loading on the China topic",
  x = "", 
 color = "") + theme_economist_white()

# Plot the count of environment words in the transctipts across time
filestextDF %>%
  ggplot(aes(date)) +
  geom_line(aes(y = environment, color = company), size = 0.5) + ylab("Loading on the environment topic") +  
  labs(y = "Loading on the environment topic",
       title = "Time-varying loading on the environment topic (%)",
       x = "", 
       color = "") + theme_economist_white()

# Plot the count of lawsuit words in the transctipts across time
filestextDF %>%
  ggplot(aes(date)) +
  geom_line(aes(y = lawsuit, color = company), size = 0.5) + ylab("Loading on the lawsuit topic") +  
  labs(y = "Loading on the lawsuit topic",
       title = "Time-varying loading on the lawsuit topic",
       x = "", 
       color = "") + theme_economist_white()

#Question 6 ----
#Document term matrices per company
#Amazon
index_amazon <- grep('Amazon', filestextDF$Document)
tdm_ama <- TermDocumentMatrix(corpus[index_amazon])
tdm_ama_idf = filter_tf_idf(tdm_ama, min = 0.001, verbose = FALSE)

#Apple
index_apple <- grep('Apple', filestextDF$Document)
tdm_app <- TermDocumentMatrix(corpus[index_apple])
tdm_app_idf = filter_tf_idf(tdm_app, min = 0.001, verbose = FALSE)

#ASML
index_asml <- grep('ASML', filestextDF$Document)
tdm_asm <- TermDocumentMatrix(corpus[index_asml])
tdm_asm_idf = filter_tf_idf(tdm_asm, min = 0.001, verbose = FALSE)

#Caterpillar
index_caterpillar <- grep('Caterpillar', filestextDF$Document)
tdm_cat <- TermDocumentMatrix(corpus[index_caterpillar])
tdm_cat_idf = filter_tf_idf(tdm_cat, min = 0.001, verbose = FALSE)

#Coca-Cola
index_cocacola <- grep('Coca-Cola', filestextDF$Document)
tdm_coc <- TermDocumentMatrix(corpus[index_cocacola])
tdm_coc_idf = filter_tf_idf(tdm_coc, min = 0.001, verbose = FALSE)

#Tesla
index_tesla <- grep('Tesla', filestextDF$Document)
tdm_tes <- TermDocumentMatrix(corpus[index_tesla])
tdm_tes_idf = filter_tf_idf(tdm_tes, min = 0.001, verbose = FALSE)

merged_tdm = c(tdm_ama_idf, tdm_app_idf, tdm_asm_idf, tdm_cat_idf, tdm_coc_idf, tdm_tes_idf)
merged_tdm_idf = filter_tf_idf(merged_tdm, min = 0.001, verbose = FALSE)

#6.a
#Grid of 5, min 10 topics
result <- FindTopicsNumber(
  merged_tdm_idf, #
  topics = seq(from = 10, to = 50, by = 5), # grid of topics
  metrics = "CaoJuan2009", # estimation criterion
  method = "Gibbs", # estimation method
  control = list(seed = 123), # set RNG
  mc.cores = 2L,
  verbose = TRUE
)
FindTopicsNumber_plot(result)

#6.b
K <- result$topics[which(result$CaoJuan2009 == min(result$CaoJuan2009))]
topicModel <- LDA(
  t(merged_tdm_idf), # transpose of term-document matrix
  K, # number of topics
  method= "Gibbs",  # estimation method
  control = list(seed = 1234), # set RNG
  verbose = TRUE
)

# Posterior results
tmResult <- posterior(topicModel)
# Document-topic probability distribution
theta <- tmResult$topics

#6.c
termstop10 = terms(topicModel, 10)
topicNames <- c('')

#6.d
#The most recent transcript date is different per company. For example, ASML has q3_23, while Apple has q1_24. These index numbers were retrieved manually
index_last_apple = 25
index_last_asml = 53
#For all other companies, this is the end of their index

index_list <- list(index_amazon, index_caterpillar, index_cocacola, index_tesla)
index_latest <- unlist(lapply(index_list, function(x) x[length(x)]))
index_latest <- c(index_latest, index_last_apple, index_last_asml)
index_latest = sort(index_latest) # making sure all indexes are properly sorted

topicProportions <- theta[index_latest,] # get their topic proportions
colnames(topicProportions) <- topicNames # assign to each column its topic name

vizDataFrame <- melt(cbind(data.frame(topicProportions), 
                           document = factor(companies)), 
                     variable.name = "topic", 
                     id.vars = "document")
ggplot(data = vizDataFrame, 
       aes(topic, value, fill = document)) +
  geom_bar(stat="identity") + # provide the y values and skip data aggregation
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +  
  coord_flip() +
  facet_wrap(~ document, 
             ncol = length(index_latest)) # divide in as many subplots as the number of documents

#6.e----
# Select two topics (ideally, topics with a clear economic interpretation) and show the time-varying loadings of all firms to these two topics.
# first show the 20 keywords per topic
topic20 <- terms(topicModel, 20)
topic20 <- topic20 %>% fortify.zoo %>% as_tibble()

# unlist topic 10 and 11
covid <- unlist(topic20$`Topic 10`)
AI <- unlist(topic20$`Topic 11`)

# Add columns with zeros to filextextDF per topic 
filestextDF$covid <- replicate(dim(filestextDF)[1], 0)
filestextDF$AI <- replicate(dim(filestextDF)[1], 0)


# This is a loop that iterates over each row of the filestextDF. It removes punctuations and converts it to lower case
for (r in 1:dim(filestextDF)[1]) {
  text <- filestextDF[r,c('text')]
  text <- gsub("\\d|[[:punct:]]", "", text)
  text <- tolower(text)
  
  # check proportion of total words that overlaps with the topic words. We do this per topic.
  
  for (kw in 1:length(covid)){
    filestextDF[r,c('covid')] <- filestextDF[r,c('covid')] + str_count(text, covid[kw]) / (lengths(gregexpr("[A-z]\\W+", text)) + 1) # this counts the occurrences of the keyword in the preprocessed text using str_count and it divides it by the total number of words. It adds these numbers to the dataset in a new column.
  }
  for (kw in 1:length(AI)){
    filestextDF[r,c('AI')] <- filestextDF[r,c('AI')] + str_count(text, AI[kw]) / (lengths(gregexpr("[A-z]\\W+", text)) + 1)
  }
}

filestextDF %>%
  ggplot(aes(date)) +
  geom_line(aes(y = covid, color = company)) +  
  labs(y = "Loading on the covid topic",
       title = "Time-varying loading on the COVID-19 topic",
       x = "", 
       color = "") + theme_economist_white()

filestextDF %>%
  ggplot(aes(date)) +
  geom_line(aes(y = AI, color = company)) +  
  labs(y = "Loading on the AI topic",
       title = "Time-varying loading on the AI topic",
       x = "", 
       color = "") + theme_economist_white()

