# This R file Identifies the most common words in the book “Around the 
#World in 80 Days” using text mining techniques (word/n-gram frequencies 
#and by incorporating Term Frequency-Inverse Document Frequency (TF-IDF) 
#and topic modelling) to find the main context of the book.
#
# Author: Ann Gline Gomez
# Last Modified: 18 Nov 2025

## Loading the required  libraries
pacman::p_load(tidyverse, tidytext, ggwordcloud, topicmodels, reshape2, zoo, textclean, wordnet, textstem)

## Loading the clean data
data <- read.csv("around_world_80_days.csv") %>% select(-X)
data

# some additional cleaning steps
## replace contractions
#data$text <- replace_contraction(data$text)
#data

## the above code didn't work. this will only work if "'" is used for
## contractions. but our text uses "’"
data$text <- data$text %>% str_replace_all("’", "'")

## replace contractions
data$text <- replace_contraction(data$text)

# removing plurals
data$text <- data$text %>% str_replace_all("'s","")

# removing underscores from words
data$text <- data$text %>% str_replace_all("_","")


## Setting the Graph Theme for all graphs
theme_set(theme_bw() + theme(axis.title.y = element_text(vjust = +3), axis.title.x = element_text(vjust = -0.75)))  # Set the theme for all graphs

# Zipf's Law

# finding the frequency of each words by rank
freq_by_rank  <- data %>%
  unnest_tokens(word, text)%>% # Separating the tokens
  count(word, sort=TRUE) %>% #counts each word's frequency
  mutate(rank=row_number(),
         term_frequency = n/sum(n)) #adding columns rank and term_frequency

# Visualizing Zipf's Law by plotting rank and term_frequency on x and y axis
#on logarithmic scales

freq_by_rank %>%
  ggplot(aes(rank, term_frequency)) +
  geom_line(linewidth=1.1, alpha=0.8) +
  scale_x_log10() +
  scale_y_log10() +
  labs(x="Rank", y="Term Frequency", title="Zipf's Law")

# Finding the slope
lm(log10(term_frequency) ~ log10(rank), data = freq_by_rank)

# Removing the tail ends
freq_by_rank_subset <- freq_by_rank %>% 
  filter(rank < 1000, rank > 10)

lm(log10(term_frequency) ~ log10(rank), data = freq_by_rank_subset)

#log scale graph along with line of slope -1
freq_by_rank %>% 
  ggplot(aes(rank, term_frequency)) + 
  geom_abline(intercept = -0.69, slope = -1.1, color = "gray50", linetype = 2) +
  geom_line(linewidth = 1.1, alpha = 0.8) + 
  scale_x_log10() +
  scale_y_log10() +
  labs(x = "Rank", y = "Term Frequency", title="Zipf's Law along with line of slope = -1")


# term frequency analysis


book_words <- data %>%
  unnest_tokens(word, text) %>% # separating tokens
  count(chapter, word, sort=TRUE) # counts word count in each chapter

# find total number of words
total_words <- book_words %>%
  group_by(chapter) %>%
  summarize(total=sum(n))

book_words <- tibble(left_join(book_words, total_words))

# Visualize Term Frequency Distribution in chapter 1 to 4
book_words %>% filter(chapter<=4) %>% ggplot(aes(n/total)) +
  geom_histogram(show.legend = FALSE) +
  xlim(NA, 0.09)+
  facet_wrap(~chapter, ncol = 2, scales = "free_y")+
  labs(
    title="Term Frequency Distribution in chapter 1 to 4"
  )

# n-grams

# bigram
data_bigrams <- data %>%
  unnest_tokens(bigram, text, token="ngrams", n=2) %>%
  filter(!is.na(bigram))
head(data_bigrams,15)

# finding most common bigrams
data_bigrams %>%
  count(bigram, sort=TRUE)

# Separating the above bigrams into word1 and word2 
separated_bigrams <- data_bigrams %>%
  separate(bigram, c("word1", "word2"), sep=" ")
head(separated_bigrams,15)

# removing stop words from both the parts
filtered_bigrams <- separated_bigrams %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)
head(filtered_bigrams,15)

# new count of bigram
bigram_counts <- filtered_bigrams %>% 
  count(word1, word2, sort = TRUE)
head(bigram_counts,15)

# most common bigrams without stop words
united_bigrams <- filtered_bigrams %>%
  unite(bigram, word1, word2, sep = " ")

head(united_bigrams,15)

# Visualize bigrams by chapter (1-4)
data %>%
  unnest_tokens(bigram, text, token = "ngrams", n = 2) %>%
  separate(bigram, c("word1", "word2"), sep = " ") %>%
  filter(!word1 %in% stop_words$word,
         !word2 %in% stop_words$word) %>%
  count(chapter, word1, word2, sort = TRUE) %>%
  unite(bigram, word1, word2, sep = " ") %>%
  filter(chapter <= 4) %>%
  group_by(chapter) %>%
  slice_max(n, n = 10, with_ties = FALSE) %>%
  ungroup() %>%
  ggplot(aes(n, reorder_within(bigram, n, chapter), fill = chapter)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~chapter, ncol = 2, scales = "free") +
  labs(x = "Frequency", y = "Top Bigrams", title="Plot of Bigrams") + 
  scale_y_reordered()

# trigram
data_trigrams <- data %>%
  unnest_tokens(trigram, text, token="ngrams", n=3) %>%
  filter(!is.na(trigram))
head(data_trigrams,15)

# finding most common trigrams
#data_trigrams %>%
#count(trigram, sort=TRUE)

# Separating the above trigrams into word, word2 and word3 
separated_trigrams <- data_trigrams %>%
  separate(trigram, c("word1", "word2", "word3"), sep=" ")
#separated_trigrams

# removing stop words from both the parts
filtered_trigrams <- separated_trigrams %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)%>%
  filter(!word3 %in% stop_words$word)
#filtered_trigrams

# new count of trigram
trigram_counts <- filtered_trigrams %>% 
  count(word1, word2, word3, sort = TRUE)
#trigram_counts

# most common trigrams without stop words
united_trigrams <- filtered_trigrams %>%
  unite(trigram, word1, word2, word3, sep = " ")

#united_trigrams

# Visualize trigrams by chapter (1-4)
data %>%
  unnest_tokens(trigram, text, token = "ngrams", n = 3) %>%
  separate(trigram, c("word1", "word2", "word3"), sep = " ") %>%
  filter(!word1 %in% stop_words$word,
         !word2 %in% stop_words$word,
         !word3 %in% stop_words$word) %>%
  count(chapter, word1, word2, word3, sort = TRUE) %>%
  unite(trigram, word1, word2, word3, sep = " ") %>%
  filter(chapter <= 4) %>%
  group_by(chapter) %>%
  slice_max(n, n = 10, with_ties = FALSE) %>%
  ungroup() %>%
  ggplot(aes(n, reorder_within(trigram, n, chapter), fill = chapter)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~chapter, ncol = 2, scales = "free") +
  labs(x = "Frequency", y = "Top trigrams", title="Plot of Trigrams") + 
  scale_y_reordered()

# TF-IDF

book_words <- data %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words, by="word") %>%
  count(chapter, word, sort = TRUE)
unique(book_words)

book_tfidf <- book_words %>%
  bind_tf_idf(word, chapter, n)

# Visualising the TF-IDF
book_tfidf %>%
  filter(chapter <= 4) %>%
  group_by(chapter) %>%
  slice_max(tf_idf, n = 10) %>%
  ungroup() %>%
  ggplot(aes(tf_idf, reorder_within(word, tf_idf, chapter), fill = chapter)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~chapter, ncol = 2, scales = "free") +
  labs(x = "tf-idf", y = NULL) + 
  scale_y_reordered()

# applying lemmatization
book_words <- data %>%
  unnest_tokens(word, text) %>%
  mutate(word=lemmatize_words(word))%>%
  anti_join(stop_words, by="word") %>%
  count(chapter, word, sort = TRUE)
#unique(book_words)

book_tfidf <- book_words %>%
  bind_tf_idf(word, chapter, n)

# Visualising the TF-IDF
book_tfidf %>%
  filter(chapter <= 4) %>%
  group_by(chapter) %>%
  slice_max(tf_idf, n = 10) %>%
  ungroup() %>%
  ggplot(aes(tf_idf, reorder_within(word, tf_idf, chapter), fill = chapter)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~chapter, ncol = 2, scales = "free") +
  labs(x = "tf-idf after applying lemmatization", y = NULL) + 
  scale_y_reordered()

# Synonym based tf-idf
setDict("./dict")

# function to get synonyms using Wordnet

get_synonym <- function(word){
  filter <- getTermFilter("ExactMatchFilter", word, TRUE)
  terms <- getIndexTerms("NOUN", 5, filter)
  
  # if terms is found then find all words from index
  if(!is.null(terms)){
    lemma <- sapply(terms, getLemma)
    return(tolower(unique(lemma)))
  }
  
  # if terms is found then return the same word
  return(tolower(word))
}

# creating a synonym tibble

# filtering chapters <= 4
book_words <- book_words %>% filter(chapter <= 4)
#finding the unique words
unique_words <- unique(book_words$word)

synonym_tibble <- tibble( 
  word=unique_words,
  synonym=sapply(unique_words, function(term){
    synonyms <- get_synonym(term)
    if (is.null(synonyms) || length(synonyms) == 0 || all(is.na(synonyms))) {
      return(term)
    }
    else{
      return(synonyms[1])
    }
  })
)

# same synonym words are merged here
book_synonym <- book_words %>%
  left_join(synonym_tibble, by="word") %>%
  group_by(chapter, synonym) %>%
  summarise(n=sum(n), .groups="drop")
head(book_synonym)

book_syn_tfidf <- book_synonym %>%
  bind_tf_idf(synonym, chapter, n)


# Visualising the TF-IDF
book_syn_tfidf %>%
  filter(chapter <= 4) %>%
  group_by(chapter) %>%
  slice_max(tf_idf, n = 10, with_ties = FALSE) %>%
  ungroup() %>%
  ggplot(aes(tf_idf, reorder_within(synonym, tf_idf, chapter), fill = chapter)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~chapter, ncol = 2, scales = "free") +
  labs(x = "synonym-based term weighting", y = NULL) + 
  scale_y_reordered()

## Topic Modelling

# Use LDA - 6.2

# turning one-token-per-row table into a DocumentTermMatrix
book_words <- data %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words, by="word") %>%
  count(chapter, word, sort = TRUE)
book_words
book_dtm <- book_words %>%
  cast_dtm(chapter, word, n)
book_dtm

# topic model (LDA) with 4 topics

book_lda <- LDA(book_dtm, k = 4, control = list(seed = 1234))
book_lda

# per topic per word probabilities, beta

chapter_topics <- tidy(book_lda, matrix = "beta")
chapter_topics

# top 15 terms per topic:

top_15_terms <- chapter_topics %>%
  group_by(topic) %>%
  slice_max(beta, n = 15) %>%
  ungroup() %>%
  arrange(topic, -beta)
top_15_terms

top_15_terms %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(beta, term, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~topic, scales = "free") + 
  scale_y_reordered()+
  labs(title="Top 15 words per topic")

# probability of topics in chapters, gamma

gamma_chapters <- tidy(book_lda, matrix = "gamma")
gamma_chapters

gamma_chapters %>%
  mutate(topic = factor(topic)) %>%
  mutate(document = as.numeric(document)) %>%
  ggplot(aes(document, gamma, color = topic)) +
  labs(title="Topic distribution in chapters")+
  geom_smooth(se = FALSE)

