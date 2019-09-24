require(dplyr)
require(magrittr)
require(tibble)
source("functions.R")


players <- read.csv('Fantasy-Premier-League/data/2019-20/players_raw.csv', stringsAsFactors=FALSE) %>% as.tbl
budget <- 100

# optimization based on ict_index
ict <- players$ict_index
bps <- players$bps

probs <- players$chance_of_playing_next_round
probs[probs=='None'] <- 0
probs <- as.numeric(probs) / 100


# objective function
f.obj <- ict

# run lp
res <- fantasy_lp(players, f.obj, budget=100)

# select the optimal overall team
players_opt <- players %>%
                mutate(solution=res$solution) %>%
                filter(solution==1) %>%
                select(first_name, second_name, team, element_type, now_cost, chance_of_playing_next_round, ict_index, bps) %>%
                arrange(-ict_index)
players_opt

# select the optimal gameweek team based on price
team_res <- weekteam_lp(players_opt, players_opt$now_cost)
team_opt <- players_opt %>%
                mutate(team_solution=team_res$solution) %>%
                select(-bps,-chance_of_playing_next_round) %>%
                arrange(element_type)
team_opt