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


#tidying dfm (document feature matrix) objects


library(methods)

data("data_corpus_inaugural", package = "quanteda")
inaug_dfm <- quanteda::dfm(data_corpus_inaugural, verbose = FALSE)

inaug_dfm

inaug_td <- tidy(inaug_dfm)
inaug_td

inaug_tf_idf <- inaug_td %>% 
  bind_tf_idf(term, document, count) %>% 
  arrange(desc(tf_idf))

table(inaug_td$document)


inaug_tf_idf



library(tidyr)

year_term_counts <- inaug_td %>% 
  extract(document, "year", "(\\d+)", convert = TRUE) %>% 
  complete(year, term, fill = list(count = 0)) %>%
  group_by(year) %>% 
  mutate(year_total = sum(count))

year_term_extras <- year_term_counts$term[1:16]
#my own exploration: what is the word that maximises tf-idf for each inauguration speech
max_inaug_tf_idf <- inaug_tf_idf %>% 
  filter(!term %in% year_term_extras ) %>%
  group_by(document) %>% 
  summarise(tf_idf = max(tf_idf)) %>% 
  inner_join(inaug_tf_idf)

year_term_counts %>%
  filter(term %in% c("god", "america", "foreign", 
                     "union", "constitution", "freedom")) %>% 
  ggplot(aes(year, count/year_total)) +
  geom_point() + 
  geom_smooth() + 
  facet_wrap(~term, scales = "free_y") + 
  scale_y_continuous(labels = scales::percent_format()) + 
  ylab("% frequency of word in inaugural address")


#converting tidytext to dtm matrices

ap_dtm <- ap_td %>% 
  cast_dtm(document, term, count)

#casting into a dfm object

ap_dfm <- ap_td %>% 
  cast_dfm(term, document, count)

library(Matrix)

#cast into a Matrix object 

m <- ap_td %>%
  cast_sparse(document, term, count)

class(m)
dim(m)

#casting the jane austen dataset into a dtm matrix
library(janeaustenr)

austen_dtm <- austen_books() %>% 
  unnest_tokens(word, text) %>%
  count(book, word) %>%
  cast_dtm(book, word, n)


austen_dtm

data("acq")
acq[[1]]
acq[[1]]$content

acq_td <- tidy(acq)

acq_tokens <- acq_td %>% 
  select(-places) %>%
  unnest_tokens(word, text) %>% 
  anti_join(stop_words, by = "word")

acq_tokens %>% 
  count(word, sort = TRUE)

#tf-idf

tokens_sorted_tfidf <- acq_tokens %>% 
  count(id, word) %>% 
  bind_tf_idf(word, id, n) %>%
  arrange(desc(tf_idf))

company <- c("Microsoft", "Apple", "Google", "Amazon", "Facebook",
             "Twitter", "IBM", "Yahoo", "Netflix")
symbol <- c("MSFT", "AAPL", "GOOG", "AMZN", "FB", "TWTR", "IBM", "YHOO", "NFLX")




download_articles <- function(symbol) {
  WebCorpus(YahooNewsSource(paste0("NASDAQ:",symbol)))
}

stock_articles <- tibble(company = company,
                         symbol = symbol) %>%
  mutate(corpus = map(symbol, download_articles))


stock_tokens <- stock_articles %>%
  unnest(map(corpus, tidy)) %>%
  unnest_tokens(word, text) %>%
  select(company, datetimestamp, word, id, heading)

stock_tokens