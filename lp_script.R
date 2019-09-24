require(dplyr)
require(magrittr)
require(tibble)
source("functions.R")


players <- read.csv('Fantasy-Premier-League/data/2019-20/players_raw.csv', stringsAsFactors=FALSE) %>% as.tbl

n <- nrow(players)
budget <- 100

# optimization based on ict_index
ict <- players$ict_index
bps <- players$bps

probs <- players$chance_of_playing_next_round
probs[probs=='None'] <- 0
probs <- as.numeric(probs) / 100


# objective function
f.obj <- ict * probs

# run lp
res <- fantasy_lp(players, f.obj, budget=100)

# check the optimal team
players %<>% mutate(solution=res$solution)
players %>% filter(solution==1) %>%
            select(first_name, second_name, team, element_type, now_cost, chance_of_playing_next_round, ict_index, bps) %>%
            arrange(element_type)
