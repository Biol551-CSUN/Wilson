### Week 12: Word Cloud Practice (we did word clouds this week and I wanted to try it, but couldn't figure out where else to put this file, so it's here)
### Created by: Emily Wilson
### Created on: 04/19/21
############################################################################################

# Load libraries ###########################################################################
library(here)
library(tidyverse)
library(tidytext)
library(wordcloud2)
library(webshot)

# Load data ###############################################################################
draft <-
  read.csv(here("Week_12", "data", "EmilyWilson_ThesisProposalDraft.csv"))


# Make word cloud #########################################################################
thesis <- draft %>%
  select(X) %>%
  unnest_tokens(output = word, input = X) %>%
  anti_join(get_stopwords()) %>%
  filter(
    !word %in% c(
      "et",
      "al",
      "o",
      "z",
      "may",
      "within",
      "also",
      "can",
      "orth",
      "though",
      "among",
      "including",
      "groner",
      "kimmerer",
      "ridlon"
    )
  ) %>%
  mutate(word = str_replace_all(
    string = word,
    pattern = "\\d",
    replacement = ""
  )) %>%
  na_if("") %>%
  drop_na()

thesis %>%
  count(word, sort = TRUE)

words <- thesis %>%
  count(word) %>% # count all the words
  arrange(desc(n)) %>% # sort the words
  slice(1:100) #take the top 100


wordcloud2( # make a wordcloud out of the top 100 words; can add size argument
  words,
  shape = "circle",
  color = calecopal::cal_palette("bigsur2", n = 40, type = "continuous"),
  backgroundColor = "mintcream"
)

