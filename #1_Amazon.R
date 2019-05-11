library(readr)
library(tidytext)
library(dplyr)
library(tidyr)


keep_cols <- c("id", "name", "asins", "reviews.date", "reviews.didPurchase",
               "reviews.doRecommend", "reviews.numHelpful", "reviews.rating", 
               "reviews.title", "reviews.text")




reviews <- read_csv("data/Amazon_reviews.csv") %>% 
  select(id, name, asins, keep_cols) %>% 
  unite(review, reviews.title, reviews.text, sep = " ") 


data(stop_words)
tidy_reviews_1 <- filter(reviews, asins == "B01AHB9CN2") %>% 
  unnest_tokens(word, review) %>%
  anti_join(stop_words)


frequency <- tidy_reviews_1 %>% 
  mutate(word = str_extract(word, "[a-z']+")) %>%
  count(reviews.rating, word) %>% 
  group_by(reviews.rating) %>% 
  mutate(proportion = n/sum(n)) %>% 
  select(-n) %>%
  spread(reviews.rating, proportion) %>% 
  gather(reviews.rating, proportion, `1`:`4`)


ggplot(frequency, aes(x = proportion, y = `5`, color = abs(`5` - proportion))) +
  geom_abline(color = "gray40", lty = 2) +
  geom_jitter(alpha = 0.1, size = 2.5, width = 0.3, height = 0.3) +
  geom_text(aes(label = word), check_overlap = TRUE, vjust = 1.5) +
  scale_x_log10(labels = percent_format()) +
  scale_y_log10(labels = percent_format()) + 
  scale_color_gradient(limits = c(0, 0.001),
                       low = "darkslategray4", high = "gray75") +
  facet_wrap(~reviews.rating, ncol = 2) + 
  theme(legend.position = "none") +
  labs(y = "5 Star review", x = NULL)




