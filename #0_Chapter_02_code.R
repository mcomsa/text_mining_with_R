library(tidytext)

#table(sentiments$lexicon)

#afinn <- sentiments[sentiments$lexicon == "AFINN", ]
#bing <- sentiments[sentiments$lexicon == "bing", ]
#loughran <- sentiments[sentiments$lexicon == "loughran", ]
#nrc <- sentiments[sentiments$lexicon == "nrc", ]

#library(readr)    
#write_csv(afinn, "data/afinn.csv")
#write_csv(bing, "data/bing.csv")
#write_csv(loughran, "data/loughran.csv")
#write_csv(nrc, "data/nrc.csv")
#write_csv(sentiments, "data/sentiments.csv")

get_sentiments("afinn")
get_sentiments("bing")
get_sentiments("nrc")
get_sentiments("loughran")

library(janeaustenr)
library(dplyr)
library(stringr)

tidy_books <- austen_books() %>%
  group_by(book) %>%
  mutate(linenumber = row_number(),
         chapter = cumsum(str_detect(text, regex("^chapter [\\divxlc]",
                                                 ignore_case = TRUE)))) %>%
  ungroup() %>%
  unnest_tokens(word, text)


nrcjoy <- get_sentiments("nrc") %>% 
  filter(sentiment == "joy")

tidy_books %>% 
  filter(book == "Emma") %>%
  inner_join(nrcjoy) %>% 
  count(word, sort = TRUE)

library(tidyr)

janeaustensentiment <- tidy_books %>%
  inner_join(get_sentiments("bing")) %>% 
  count(book, index = linenumber %/% 80, sentiment) %>% 
  spread(sentiment, n, fill = 0) %>% 
  mutate(sentiment = positive - negative)

library(ggplot2)

ggplot(janeaustensentiment, aes(index, sentiment, fill = book)) + 
  geom_col(show.legend = FALSE) + 
  facet_wrap(~book, ncol = 2, scales = "free_x")


pride_prejudice <- tidy_books %>% 
  filter(book == "Pride & Prejudice")

pride_prejudice

afinn <- pride_prejudice %>% 
  inner_join(get_sentiments("afinn")) %>% 
  group_by(index = linenumber %/% 80) %>% 
  summarise(sentiment = sum(score)) %>% 
  mutate(method = "AFINN")

bing_and_nrc <- bind_rows(
  pride_prejudice %>% 
    inner_join(get_sentiments("bing")) %>% 
    mutate(method = "Bing et al."),
  pride_prejudice %>% 
    inner_join(get_sentiments("nrc") %>% 
                 filter(sentiment %in% c("positive", 
                                         "negative"))) %>% 
    mutate(method = "NRC")) %>% 
  count(method, index = linenumber %/%  80, sentiment) %>% 
  spread(sentiment, n, fill = 0) %>% 
  mutate(sentiment = positive - negative)

bind_rows(afinn,
          bing_and_nrc) %>% 
  ggplot(aes(index, sentiment, fill = method)) +
  geom_col(show.legend = FALSE) + 
  facet_wrap(~method, ncol = 1, scales = "free_y")

get_sentiments("nrc") %>% 
  filter(sentiment %in% c("positive",
                          "negative")) %>% 
  count(sentiment)

get_sentiments("bing") %>% 
  count(sentiment)


bing_word_counts <- tidy_books %>% 
  inner_join(get_sentiments("bing")) %>% 
  count(word, sentiment, sort = TRUE) %>%
  ungroup()

bing_word_counts


bing_word_counts %>% 
  group_by(sentiment) %>% 
  top_n(10) %>% 
  ungroup() %>% 
  mutate(word = reorder(word, n)) %>% 
  ggplot(aes(word, n, fill = sentiment)) + 
  geom_col(show.legend = FALSE) + 
  facet_wrap(~sentiment, scales = "free_y") +
  labs(y = "Contribution to sentiment", 
       x = NULL) + 
  coord_flip()

custom_stop_words <- bind_rows(tibble(word = c("miss"), 
                                          lexicon = c("custom")), 
                               stop_words)
#the above line differs from the book to account for the deprecation of data_frame.
#tibble is used instead.

custom_stop_words

## word clouds

library(wordcloud)

tidy_books %>% 
  anti_join(stop_words) %>% 
  count(word) %>% 
  with(wordcloud(word, n, max.words = 100))

## getting the books of the Bronte sisters and H.G. Wells from Chapter 1 to do similar word clouds. 
## these are not in the Chapter code

bronte <- read_csv("data/Bronte.csv")
bronte$text <- ifelse(is.na(bronte$text), " ", bronte$text)

tidy_bronte <- bronte %>% 
  unnest_tokens(word, text) %>% 
  anti_join(stop_words)

hgwells <- read_csv("data/HGWells.csv")
hgwells$text <- ifelse(is.na(hgwells$text), " ", hgwells$text)

tidy_hgwells <- hgwells %>% 
  unnest_tokens(word, text) %>% 
  anti_join(stop_words)


tidy_bronte %>% 
  anti_join(stop_words) %>%
  count(word) %>% 
  with(wordcloud(word, n, max.words = 100))

tidy_hgwells %>% 
  anti_join(stop_words) %>%
  count(word) %>% 
  with(wordcloud(word, n, max.words = 100))

library(reshape2)

tidy_books %>% 
  inner_join(get_sentiments("bing")) %>% 
  count(word, sentiment, sort = TRUE) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>% 
  comparison.cloud(colors = c("gray20", "gray80"),
                   max.words = 100)

## Looking at units beyond just words

#Attempting to tokenise into sentences
PandP_sentences <- tibble(text = prideprejudice) %>% 
  unnest_tokens(sentence, text, token = "sentences")

#splitting the text by chapter
austen_chapters <- austen_books() %>% 
  group_by(book) %>% 
  unnest_tokens(chapter, text, token = "regex",
                pattern = "Chapter|CHAPTER [\\dIVXLC]") %>% 
  ungroup()

austen_chapters %>% 
  group_by(book) %>% 
  summarise(chapters = n())

#identifying the most negative chapters in each Jane Austen book

bingnegative <- get_sentiments("bing") %>% 
  filter(sentiment == "negative") 

wordcounts <- tidy_books %>% 
  group_by(book, chapter) %>% 
  summarize(words = n())

tidy_books %>% 
  semi_join(bingnegative) %>% 
  group_by(book, chapter) %>% 
  summarize(negativewords = n()) %>% 
  left_join(wordcounts, by = c("book", "chapter")) %>% 
  mutate(ratio = negativewords/words) %>%
  filter(chapter != 0) %>% 
  top_n(1) %>%
  ungroup()
