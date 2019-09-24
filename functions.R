require(Rglpk)
require(dplyr)


fantasy_lp <- function(
    player_table,           # players data, must include team, element_type, now_cost columns
    f.obj,                  # objective coefficients, must be same length as nrow(player_table)
    budget=100,             # budget of the team
    team_size = c(2,5,5,3)  # team size constraints (gk, def, mid, att)
){
    n_players <- nrow(player_table)

    # add the constraints based on game rules
    f.con <- rbind(
                rep(1, n_players),                                  # team in size of 15
                player_table$now_cost,                      # budget constraint
                as.numeric(player_table$element_type == 1), # goalkeepers
                as.numeric(player_table$element_type == 2), # defense
                as.numeric(player_table$element_type == 3), # midfield
                as.numeric(player_table$element_type == 4)  # attack
                )
    f.dir <- c('<=', '<=', '==', '==', '==', '==')
    f.rhs <- c(15, budget * 10, team_size)

    # add max-3-per-team constraints
    for (team in 1:20){
        teamBool = as.numeric(player_table$team == team)
        
        f.con <- rbind(f.con, teamBool)
        f.dir <- c(f.dir, '<=')
        f.rhs <- c(f.rhs, 3)
    }

    # run the LP
    res <- Rglpk_solve_LP(f.obj, f.con, f.dir, f.rhs, max=TRUE, types="B")
    return(res)
}


weekteam_lp <- function(
    team_table,
    f.obj,
    team_size=c(1,3,2,1)
){
    # 1 goalkeeper, atleast 3 def, 2 mid and 1 att
    f.con <- rbind(
                rep(1, 15),                               # team size always 15
                as.numeric(team_table$element_type == 1), # goalkeepers
                as.numeric(team_table$element_type == 2), # defense
                as.numeric(team_table$element_type == 3), # midfield
                as.numeric(team_table$element_type == 4)  # attack
                )
    f.dir <- c('<=', '==', '>=', '>=', '>=')
    f.rhs <- c(11, team_size)

    # run the LP
    res <- Rglpk_solve_LP(f.obj, f.con, f.dir, f.rhs, max=TRUE, types="B")
    return(res)
}