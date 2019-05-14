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

#tf-idf of bigrams
bigram_tf_idf <- bigrams_united %>% 
  count(book, bigram) %>% 
  bind_tf_idf(bigram, book, n) %>% 
  arrange(desc(tf_idf))



#uniting the two columns back into one 
bigrams_united <- bigrams_filtered %>% 
  unite(bigram, word1, word2, sep = " ")





