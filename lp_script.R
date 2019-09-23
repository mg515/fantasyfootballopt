require(dplyr)
require(magrittr)
require(tibble)
require(Rglpk)


players <- read.csv('Fantasy-Premier-League/data/2019-20/players_raw.csv') %>%
            select(first_name, second_name, team, element_type, now_cost,ict_index) %>%
            as.tbl

n <- nrow(players)
budget <- 100

# optimization based on ict_index
f.obj <- players %>% pull(ict_index)

f.con <- rbind(
            rep(1, n),                             # team in size of 15
            players$now_cost,                      # budget constraint
            as.numeric(players$element_type == 1), # goalkeepers
            as.numeric(players$element_type == 2), # defense
            as.numeric(players$element_type == 3), # midfield
            as.numeric(players$element_type == 4)  # attack
            )
f.dir <- c('<=', '<=', '==', '==', '==', '==')
f.rhs <- c(15, budget * 10, 2, 5, 5, 3)

# add 3-per-team constraints
for (team in unique(players$team)){

    teamBool = as.numeric(players$team == team)

    f.con <- rbind(f.con, teamBool)
    f.dir <- c(f.dir, '<=')
    f.rhs <- c(f.rhs, 3)
}

# run the LP
res <- Rglpk_solve_LP(f.obj, f.con, f.dir, f.rhs, max=TRUE, types="B")

# check the optimal team
players %<>% mutate(solution=res$solution)
players %>% filter(solution==1) %>%
            arrange(-ict_index)
