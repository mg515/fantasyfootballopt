require(dplyr)
require(magrittr)
require(tibble)
require(Rglpk)


players <- read.csv('Fantasy-Premier-League/data/2019-20/cleaned_players.csv') %>% as.tbl
n <- nrow(players)
budget <- 100

f.obj <- players %>% pull(ict_index)
f.con <- rbind(
            rep(1, n),
            players %>% pull(now_cost)
            )
f.dir <- c('<=', '<=')
f.rhs <- c(15, budget * 10)


res <- Rglpk_solve_LP(f.obj, f.con, f.dir, f.rhs, max=TRUE, types="B")
res


players %<>% mutate(solution=res$solution)
players %>% select(first_name, second_name, ict_index, solution) %>%
            filter(solution==1) %>%
            arrange(-ict_index) %>%
            print
