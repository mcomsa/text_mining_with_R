library(dplyr)
library(janeaustenr)
library(tidytext)

book_words <- austen_books() %>% 
  unnest_tokens(word, text) %>%
  count(book, word, sort = TRUE) %>% 
  ungroup()

total_words <- book_words %>% 
  group_by(book) %>% 
  summarise(total = sum(n))

book_words <- left_join(book_words, total_words)

library(ggplot2)

ggplot(book_words, aes(n/total, fill = book)) +
  geom_histogram(show.legend = FALSE) + 
  xlim(NA, 0.0009) +
  facet_wrap(~book, ncol = 2, scales = "free_y")

#zipf's law code below. Plotting term frequency vs rank should give a straight line according to the law

freq_by_rank <- book_words %>% 
  group_by(book) %>% 
  mutate(rank = row_number(),
         `term frequency` = n/total)

freq_by_rank

zipf_plot <- freq_by_rank %>% 
  ggplot(aes(rank, `term frequency`, color = book)) +
  geom_line(size = 1.1, alpha = 0.8, show.legend = FALSE) +
  scale_x_log10() +
  scale_y_log10()

rank_subset <- freq_by_rank %>% 
  filter(rank < 500, 
         rank > 10)

lm(log10(`term frequency`) ~log10(rank), data = rank_subset)


fited_zipf_plot <- freq_by_rank %>% 
  ggplot(aes(rank, `term frequency`, color = book)) +
  geom_abline(intercept = -0.62, slope = -1.1, color = "gray50", linetype = 2) + 
  geom_line(size = 1.1, alpha = 0.8, show.legend = FALSE) +
  scale_x_log10() +
  scale_y_log10()

book_words <- book_words %>% 
  bind_tf_idf(word, book, n) 

book_words


book_words %>%
  select(-total) %>% 
  arrange(desc(tf_idf))

high_tf_idf_plot <- book_words %>% 
  arrange(desc(tf_idf)) %>%
  mutate(word = factor(word, levels = rev(unique(word)))) %>%
  group_by(book) %>%
  top_n(15) %>%
  ungroup() %>% 
  ggplot(aes(word, tf_idf, fill = book)) + 
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "tf-idf") +
  facet_wrap(~book, ncol = 2, scales = "free") +
  coord_flip()


library(gutenbergr)
physics <- gutenberg_download(c(37729, 14725, 13476),
                              meta_fields = "author")
library(readr)
write_csv(physics, "data/physics.csv")

physics <- read_csv("data/physics.csv")


conn = file("data/relativity.txt", "r")
relativity <- readLines(conn)

relativity <- tibble(gutenberg_id =  5001, text = relativity, author = "Einstein, Albert")
physics <- bind_rows(physics, relativity)


physics$text <- ifelse(is.na(physics$text), " ", physics$text)



physics_words <- physics %>%
  unnest_tokens(word, text) %>% 
  count(author, word, sort = TRUE) %>% 
  ungroup()

physics_words


plot_physics <- physics_words %>% 
  bind_tf_idf(word, author, n) %>% 
  arrange(desc(tf_idf)) %>% 
  mutate(word = factor(word, levels = rev(unique(word)))) %>% 
  mutate(author = factor(author, levels = c("Galilei, Galileo",
                                            "Huygens, Christiaan",
                                            "Tesla, Nikola",
                                            "Einstein, Albert")))


plot_physics %>% 
  group_by(author) %>% 
  top_n(15, tf_idf) %>%
  ungroup() %>% 
  mutate(word = reorder(word, tf_idf)) %>% 
  ggplot(aes(word, tf_idf, fill = author)) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "tf-idf") +
  facet_wrap(~author, ncol = 2, scales = "free") +
  coord_flip()


library(stringr)

#checking to see the occurrences of the word "eq" are
physics %>% 
  filter(str_detect(text, "eq\\.")) %>% 
  select(text)

#checking to see what the occurrences of the word "K1" are 

physics %>% 
  filter(str_detect(text, "K1")) %>% 
  select(text)

physics %>% 
  filter(str_detect(text, "AK")) %>% 
  select(text)


#some custom stopwords for this corpus
mystopwords <- data_frame(word = c("eq", "co", "rc", "ac", "ak", "bn",
                                   "fig", "file", "cg", "cb", "cm"))
physics_words <- anti_join(physics_words, mystopwords, by = "word")
plot_physics <- physics_words %>%
  bind_tf_idf(word, author, n) %>% 
  arrange(desc(tf_idf)) %>% 
  mutate(word = factor(word, levels = rev(unique(word)))) %>% 
  group_by(author) %>%
  top_n(15, tf_idf) %>%
  ungroup %>%
  mutate(author = factor(author, levels = c("Galilei, Galileo",
                                            "Huygens, Christiaan", 
                                            "Tesla, Nikola",
                                            "Einstein, Albert")))

ggplot(plot_physics, aes(word, tf_idf, fill = author)) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "tf-idf") +
  facet_wrap(~author, ncol = 2, scales = "free") +
  coord_flip()
  



