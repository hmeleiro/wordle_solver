
tildes <- function(x) {
  x %>% 
    tolower() %>% 
    str_squish() %>% 
    str_replace_all("á|à|â|ä|ã", "a") %>% 
    str_replace_all("é|è|ê|ë", "e") %>% 
    str_replace_all("í|ì|î|ï", "i") %>% 
    str_replace_all("ó|ò|ô|ö", "o") %>% 
    str_replace_all("ú|ù|û|ü", "u") 
}

word_weight <- function(word) {
  word %>%
    tibble(letter = .) %>%
    merge(letter_freq) %>%
    filter(!duplicated(letter)) %>%
    summarise(w = sum(n)) %>% 
    pull()
}


# colors <- c("grey", "grey",   "yellow" ,"yellow" ,"green" )
wordle <- function(try, words, colors) {
  
  try_word <- str_split(try, "", simplify = T)
  green_letters <- try_word[colors == "green"]
  grey_letters <- paste(try_word[colors == "grey" & !try_word %in% green_letters], collapse = "|")
  
  if(length(try_word[colors == "yellow"]) > 0) {
    yellow_letters <- try_word[colors == "yellow"]
  } else {
    yellow_letters <- NULL
  }
  
  try_word[, colors == "grey"] <- "."
  try_word[, colors == "yellow"] <- paste0("[^", try_word[, colors == "yellow"] ,"]")
  regex <- paste0(try_word, collapse = "")
  
  if(grey_letters != "") {
    words <- words[!str_detect(words, grey_letters)]
  }
  
  if(length(yellow_letters) > 0) {
    words <- words[map_lgl(words, str_detect_all, patterns = yellow_letters)]
  }

  words[str_detect(words, regex)]
}


check_wordle <- function(res, try) {
  
  res <- str_split(res, "", simplify = T)
  try <- str_split(try, "", simplify = T)
  # print(try)
  colors <- NULL
  for (i in 1:ncol(res)) {
    
    if(try[,i] == res[,i]) {
      color <- "green"
    } else if(try[,i] %in% res) {
      color <- "yellow"
    } else {
      color <- "grey"
    }
    colors <- c(colors, color)
  }
  return(colors)
  
}


try_word <- function(res, try, words) {
  colors <- check_wordle(res, try)
  
  if(length(colors[colors == "green"]) == 5) {
    # message(paste("Éxito. La palabra es:", try) )
  } else {
    new_words <- wordle(try, words, colors)
    return(new_words)
  }
}

str_detect_all <- function(string, patterns) {
  all(str_detect(string, patterns))
}


sim_wordle <- function(x) {
  words <- read_csv("data/words_weighted.csv", show_col_types = FALSE) %>% 
    pull(word)
  res <- words[sample(1:length(words), 1)]
  try <- words[1]
  for (i in 1:6) {
    words <- try_word(res, try, words)
    try <- words[1]
    if(is.null(words)) {
      # message(paste("Se ha conseguido al", i, "intento."))
      temp <- tibble(solved_at = i)
      break
    }
    
    if(i == 6) {
      temp <- tibble(solved_at = 0)
    }
  }
  
  return(temp)
}