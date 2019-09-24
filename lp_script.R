require(dplyr)
require(magrittr)
require(tibble)
source("functions.R")


players <- read.csv('Fantasy-Premier-League/data/2019-20/players_raw.csv', stringsAsFactors=FALSE) %>% as.tbl
budget <- 100

# objective function candidates
ict <- players$ict_index
bps <- players$bps
selected_by <- players$selectted_by_percent
transfers_in <- players$transfers_in_event

probs <- players$chance_of_playing_next_round
probs[probs=='None'] <- 0
probs <- as.numeric(probs) / 100


# select the objective function
f.obj <- transfers_in

# run lp
res <- fantasy_lp(players, f.obj, budget=100)

# select the optimal overall team
players_opt <- players %>%
                mutate(solution=res$solution) %>%
                filter(solution==1) %>%
                arrange(-ict_index)
players_opt %>% select(first_name, second_name, team, element_type, now_cost, chance_of_playing_next_round, selected_by_percent,ict_index, bps)

# select the optimal gameweek team based on price
team_res <- weekteam_lp(players_opt, players_opt$now_cost)
team_opt <- players_opt %>%
                mutate(team_solution=team_res$solution)
                arrange(element_type)
team_opt %>% select(first_name, second_name, team, element_type, now_cost, transfers_in_event)