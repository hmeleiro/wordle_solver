source("code/00_functions.R")

library(tidyverse)

system.time({
  sims <- map_df(1:2000, sim_wordle, .id = "iteration")
})

sims %>% 
  group_by(solved_at) %>% 
  summarise(n = n()) %>% 
  mutate(pct = n/sum(n)) %>% 
  arrange(-pct)


mean(sims$solved_at[sims$solved_at != 0])



