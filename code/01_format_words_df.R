source("code/00_functions.R")

library(tidyverse)

words <- read_lines("https://raw.githubusercontent.com/danielfrg/wordle.es/main/lib/words5.js")
words <- words[-c(1:12)] %>% 
  str_remove_all(',|"') %>%
  str_squish() 

words <- words[!str_detect(words, "[:punct:]")]

letter_freq <- tokenizers::tokenize_characters(words) %>% 
  unlist() %>%
  tibble(letter = .) %>% 
  group_by(letter) %>% 
  count(sort = T)

words_list <- str_split(words, "")
w <- map(words_list, word_weight) %>% unlist()

words_df <- tibble(
  word = words,
  w
) %>% 
  arrange(-w)


write_csv(words_df, "data/words_weighted.csv")
