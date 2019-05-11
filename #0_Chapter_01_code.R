text <- c("Because I could not stop for Death -",
          "He kindly stopped for me - ",
          "The Carriage held but just Ourselves -",
          "and Immortality")

library(dplyr)
text_df <- data_frame(line = 1:4, text = text)

text_df

library(tidytext)
library(dplyr)
library(stringr)

text_df %>% 
  unnest_tokens(word, text)

#this is the same as unnest_tokens(text_df, word, text)
#the 2nd argument is the name to give the tokens column and the 3rd is the column name to look for tokens.
#tokenising using single words is the default behaviour of the function.

library(janeaustenr)

#creating an object with the line and chapter numbers to accompany each line of text
original_books <- austen_books() %>% 
                  group_by(book) %>%
                  mutate(line_number = row_number(),
                         chapter = cumsum(str_detect(text, regex("^chapter [\\divxlc]",
                                                                 ignore_case = TRUE)))) %>% 
                  ungroup()

#getting one token per row
tidy_books <- original_books %>% 
  unnest_tokens(word, text)

#removing stop words (words that are very common)

data("stop_words")

tidy_books <- tidy_books %>% 
  anti_join(stop_words)


library(ggplot2)

tidy_books %>% 
  count(word, sort = TRUE) %>% 
  filter(n > 600) %>% 
  mutate(word = reorder(word, n)) %>% 
  ggplot(aes(word, n)) + 
  geom_col() + 
  xlab(NULL) + 
  coord_flip()


austen_counts <- tidy_books %>% 
  count(word, sort = TRUE)
### getting H.G. Wells and Bronte sisters works as well

library(gutenbergr)
library(readr)
#hgwells <- gutenberg_download(c(35, 36, 5230, 159))
#write_csv(hgwells, "data/HGWells.csv")

hgwells <- read_csv("data/HGWells.csv")
hgwells$text <- ifelse(is.na(hgwells$text), " ", hgwells$text)

tidy_hgwells <- hgwells %>% 
  unnest_tokens(word, text) %>% 
  anti_join(stop_words)

hgwells_counts <- tidy_hgwells %>%
  count(word, sort = TRUE)


#bronte <- gutenberg_download(c(1260, 768, 969, 9182, 767))
#write_csv(bronte, "data/Bronte.csv")

bronte <- read_csv("data/Bronte.csv")
bronte$text <- ifelse(is.na(bronte$text), " ", bronte$text)

tidy_bronte <- bronte %>% 
  unnest_tokens(word, text) %>% 
  anti_join(stop_words)

bronte_counts <- tidy_bronte %>% 
  count(word, sort = TRUE)


## binding all 3 data frames together

library(tidyr)

frequency <- bind_rows(mutate(tidy_bronte, author = "Bronte Sisters"),
                       mutate(tidy_hgwells, author = "H.G. Wells"),
                       mutate(tidy_books, author = "Jane Austen")) %>% 
  mutate(word = str_extract(word, "[a-z']+")) %>%
  count(author, word) %>%
  group_by(author) %>%
  mutate(proportion = n / sum(n)) %>%
  select(-n) %>%
  spread(author, proportion) %>%
  gather(author, proportion, "Bronte Sisters":"H.G. Wells")

library(scales)

ggplot(frequency, aes(x = proportion, y = `Jane Austen`, color = abs(`Jane Austen` - proportion))) +
  geom_abline(color = "gray40", lty = 2) +
  geom_jitter(alpha = 0.1, size = 2.5, width = 0.3, height = 0.3) +
  geom_text(aes(label = word), check_overlap = TRUE, vjust = 1.5) +
  scale_x_log10(labels = percent_format()) +
  scale_y_log10(labels = percent_format()) + 
  scale_color_gradient(limits = c(0, 0.001),
                       low = "darkslategray4", high = "gray75") +
  facet_wrap(~author, ncol = 2) + 
  theme(legend.position = "none") +
  labs(y = "Jane Austen", x = NULL)


cor.test(data = frequency[frequency$author == "Bronte Sisters",],
         ~ proportion + `Jane Austen`)

cor.test(data = frequency[frequency$author == "H.G. Wells", ], 
         ~ proportion + `Jane Austen`)


  
         
         