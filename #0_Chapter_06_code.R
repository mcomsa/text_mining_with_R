library(topicmodels)

data("AssociatedPress")
AssociatedPress

ap_lda <- LDA(AssociatedPress, k = 2, control = list(seed = 1234))
ap_lda

library(tidytext)

ap_topics <- tidy(ap_lda, matrix = "beta")
ap_topics

library(ggplot2)
library(dplyr)

ap_top_terms <- ap_topics %>% 
  group_by(topic) %>% 
  top_n(10, beta) %>%
  ungroup() %>% 
  arrange(topic, -beta)

top_terms_plot <- ap_top_terms %>% 
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) + 
  geom_col(show.legend = FALSE) + 
  facet_wrap(~topic, scales = "free") +
  coord_flip()

#using the log ratio of the betas for the two topics to find the words that have the biggest difference
#between the two topics

library(tidyr)

beta_spread <- ap_topics %>% 
  mutate(topic = paste0("topic", topic)) %>%
  spread(topic, beta) %>%
  filter(topic1 > .001 | topic2 > .001) %>%
  mutate(log_ratio = log2(topic2/topic1))

#plotting terms. This is not the exact plot in the text. The text shows the 
# top 10 terms for each topic. I have shown the 20 terms with the biggest 
#difference. Happens to be 11-9 for topic 2 vs topic 1.

beta_spread %>% 
  top_n(20, abs(log_ratio)) %>% 
  arrange(log_ratio) %>%
  mutate(term = reorder(term, log_ratio)) %>%
  ggplot(aes(term, log_ratio, fill = log_ratio > 0 )) + 
  geom_col() + 
  coord_flip()

#document-topic probabilities

ap_documents <- tidy(ap_lda, matrix = "gamma")
ap_documents

#noticing that document 6 has very low contribution from topic 1.

tidy(AssociatedPress) %>%
  filter(document == 6) %>%
  arrange(desc(count))

#article seems to be completely about politics/national news. Nothing to do with the economy
#or finance.


#Example: Library Heist

titles <- c("Twenty Thousand Leagues under the Sea", "The War of the Worlds", 
            "Pride and Prejudice", "Great Expectations")

library(gutenbergr)

books <- gutenberg_works(title %in% titles) %>%
  gutenberg_download(meta_fields = "title")

table(books$title)

library(stringr)



reg <- regex("^chapter ", ignore_case = TRUE)
by_chapter <- books %>% 
  group_by(title) %>%
  mutate(chapter = cumsum(str_detect(text, reg))) %>%
  ungroup() %>%
  filter(chapter > 0) %>%
  unite(document, title, chapter)

by_chapter_word <- by_chapter %>% 
  unnest_tokens(word, text)

word_counts <- by_chapter_word %>%
  anti_join(stop_words) %>% 
  count(document, word, sort = TRUE) %>%
  ungroup()

word_counts

chapters_dtm <- word_counts %>%
  cast_dtm(document, word, n)

chapters_dtm

#creating a 4 topic model. Because we know there are 4 original books

chapters_lda <- LDA(chapters_dtm, k = 4, control = list(seed = 1234))
chapters_lda 

chapter_topics <- tidy(chapters_lda, matrix = "beta")
chapter_topics 


top_terms <- chapter_topics %>%
  group_by(topic) %>%
  top_n(5, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

lib_heist_top_terms_plot <- top_terms %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) + 
  facet_wrap(~topic, scales = "free") + 
  coord_flip()
  
  
#the following is my code
chapters_prediction <- tidy(chapters_lda, matrix = "gamma") %>%
  separate(document, c("title", "chapter"), sep = "_", convert = TRUE) %>%
  group_by(title, chapter) %>% 
  top_n(1, gamma) %>% 
  ungroup()

#following line is the combination of two lines in the text
chapters_gamma <- tidy(chapters_lda, matrix = "gamma") %>%
  separate(document, c("title", "chapter"), sep = "_", convert = TRUE)

#reorder titles in order of topic 1, topic 2, etc before plotting

box_plot_predictions <- chapters_gamma %>%
  mutate(title = reorder(title, gamma*topic)) %>%
  ggplot(aes(factor(topic), gamma)) +
  geom_boxplot() + 
  facet_wrap(~title)

#this code should achieve the same as my code up above
chapter_classifications <- chapters_gamma %>%
  group_by(title, chapter) %>%
  top_n(1, gamma) %>%
  ungroup()

chapter_classifications
  

book_topics <- chapter_classifications %>%
  count(title, topic) %>%
  group_by(title) %>%
  top_n(1, n) %>%
  ungroup() %>%
  transmute(consensus = title, topic)

chapter_classifications %>% 
  inner_join(book_topics, by = "topic") %>%
  filter(title != consensus)
















