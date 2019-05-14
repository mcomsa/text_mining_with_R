library(tm)

data("AssociatedPress", package = "topicmodels")
AssociatedPress

terms <- Terms(AssociatedPress)
head(terms)

library(dplyr)
library(tidytext)

ap_td <- tidy(AssociatedPress)
ap_td

ap_sentiments <- ap_td %>% 
  inner_join(get_sentiments("bing"), by = c(term = "word"))

ap_sentiments

library(ggplot2)

ap_sentiment_plot <- ap_sentiments %>% 
  count(sentiment, term, wt = count) %>% 
  ungroup() %>%
  filter(n >= 200) %>% 
  mutate(n = ifelse(sentiment == "negative", -n, n)) %>% 
  mutate(term = reorder(term, n)) %>% 
  ggplot(aes(term, n, fill = sentiment)) +
  geom_bar(stat = "identity") + 
  ylab("Contribution to sentiment") +
  coord_flip()