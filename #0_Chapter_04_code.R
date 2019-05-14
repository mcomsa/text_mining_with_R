library(dplyr)
library(tidytext)
library(janeaustenr)

austen_bigrams <- austen_books() %>%
  unnest_tokens(bigram, text, token = "ngrams", n = 2)

bigram_counts <- austen_bigrams %>%
  count(bigram, sort = TRUE)

library(tidyr)

bigrams_separated <- austen_bigrams %>% 
  separate(bigram, c("word1", "word2"), sep = " ")

#removing bigrams where one of the words is a stop word
#bigrams_filtered <- bigrams_separated %>% 
#  filter(!word1 %in% stop_words$word) %>% 
#  filter(!word2 %in% stop_words$word)

#my alternative filtering in one step
bigrams_filtered <- bigrams_separated %>% 
  filter(!(word1 %in% stop_words$word | word2 %in% stop_words$word))

#new bigram counts 
bigram_counts <- bigrams_filtered %>% 
  count(word1, word2, sort = TRUE)

#Most common trigrams
trigram_counts <- austen_books() %>% 
  unnest_tokens(trigram, text, token = "ngrams", n = 3) %>% 
  separate(trigram, c("word1", "word2", "word3"), sep = " ") %>% 
  filter(!word1 %in% stop_words$word,
         !word2 %in% stop_words$word,
         !word3 %in% stop_words$word) %>% 
  count(word1, word2, word3, sort = TRUE)

#most common streets
bigrams_filtered %>% 
  filter(word2 == "street") %>% 
  count(book, word1, sort = TRUE)


#uniting the two columns back into one 
bigrams_united <- bigrams_filtered %>% 
  unite(bigram, word1, word2, sep = " ")

#tf-idf of bigrams
bigram_tf_idf <- bigrams_united %>% 
  count(book, bigram) %>% 
  bind_tf_idf(bigram, book, n) %>% 
  arrange(desc(tf_idf))


austen_books() %>% 
  unnest_tokens(trigram, text, token = "ngrams", n = 3) %>% 
  separate(trigram, c("word1", "word2", "word3"), sep = " " ) %>% 
  filter(!word1 %in% stop_words$word,
         !word2 %in% stop_words$word,
         !word3 %in% stop_words$word) %>%
  count(word1, word2, word3, sort = TRUE)


bigrams_filtered %>% 
  filter(word2 == "street") %>% 
  count(book, word1, sort = TRUE)

bigram_tf_idf <- bigrams_united %>% 
  count(book, bigram) %>% 
  bind_tf_idf(bigram, book, n) %>% 
  arrange(desc(tf_idf))

bigram_tf_idf

#which words are negated by being preceded by "not"
bigrams_separated %>% 
  filter(word1 == "not") %>% 
  count(word1, word2, sort = TRUE)


AFINN <- get_sentiments("afinn")

#most frequent negated words that were associated with a sentiment
not_words <- bigrams_separated %>% 
  filter(word1 == "not") %>% 
  inner_join(AFINN, by = c(word2 = "word")) %>%
  count(word2, score, sort = TRUE) %>% 
  ungroup()

library(ggplot2)


not_words %>% 
  mutate(contribution = n*score) %>%
  arrange(desc(abs(contribution))) %>% 
  head(20) %>% 
  mutate(word2 = reorder(word2, contribution)) %>% 
  ggplot(aes(word2, n*score, fill = n * score > 0)) + 
  geom_col(show.legend = FALSE) + 
  xlab("Words preceded by \"not\"") + 
  ylab("Sentiment score * number of occurrences") + 
  coord_flip()
not_words

## Expanding to consider other negation words 

negation_words <- c("not", "no", "never", "without")

negated_words <- bigrams_separated %>% 
  filter(word1 %in% negation_words) %>% 
  inner_join(AFINN, by = c(word2 = "word")) %>% 
  count(word1, word2, score, sort = TRUE) %>% 
  ungroup()


#the following code is not in the text. It is my attempt to produce a plot that is shown in the 
#book. It is not quite right.


negated_words_plot <- negated_words %>% mutate(contribution = score * n) %>%
  arrange(desc(abs(contribution))) %>%
  head(60) %>% 
  mutate(word2 = reorder(word2, contribution)) %>% 
  ggplot(aes(word2, contribution, fill = contribution > 0)) + 
  geom_col(show.legend = FALSE) + 
  facet_wrap(~word1, ncol = 2, scales = "free") + 
  xlab("Words preceded by negation term") +
  ylab("Sentiment score * # of occurrences") + 
  coord_flip()

library(igraph)

bigram_counts

#filter for only relatively common combinations

bigram_graph <- bigram_counts %>% 
  filter(n > 20) %>%
  graph_from_data_frame()

library(ggraph)
set.seed(2017)

ggraph(bigram_graph, layout = "fr") +
  geom_edge_link() +
  geom_node_point() + 
  geom_node_text(aes(label = name), vjust = 1, hjust = 1)

set.seed(2016)

a <- grid::arrow(type = "closed", length = unit(.15, "inches"))

final_chapter_4_plot <- ggraph(bigram_graph, layout = "fr") + 
  geom_edge_link(aes(edge_alpha = n), show.legend = FALSE,
                 arrow = a, end_cap = circle(.07, 'inches')) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1) + 
  theme_void()
  
    
          

