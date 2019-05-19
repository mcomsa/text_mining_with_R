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

