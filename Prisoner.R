library(ggplot2)
library(GPfit)

###############################################################
# Prisoner's Dilemma

possible_moves <- c("c", "d")
possible_outcomes <- c("cc", "cd", "dc", "dd")


#~0.1 - 0.9
r = seq(from=0, 1, length=100)
k = seq(from=0, 1, length=100)
grid = expand.grid("r"=r, "k"=k)

CONST = 80
#10 - 300
grid$TE <- grid$r **2 * CONST + grid$k * CONST

grid$S <- 1
grid$P <- 5

#10 - 300
#grid$T <- pmax(grid$R + grid$r * 100 - grid$k * 40, grid$R)
grid$R <- grid$r * (grid$r - grid$S + grid$TE - grid$P) + grid$S
min(grid$R)
#grid$P <- -grid$k * (grid$T - 1) + grid$R
selection <- grid[sample(nrow(grid), 1),]
selection
#difference between T and R should always be > 0
grid$DF = grid$TE - grid$R



#GRID 2
grid2 = expand.grid("r"=r, "k"=k)

CONST = 80
#10 - 300
grid2$TE <- grid2$r **2 * CONST + grid2$k * CONST

grid2$S <- 1
grid2$P <- 8

#10 - 300
#grid$T <- pmax(grid$R + grid$r * 100 - grid$k * 40, grid$R)
grid2$R <- grid2$r * (grid2$r - grid2$S + grid2$TE - grid2$P) + grid2$S
grid2$RSOFT <- softmax(grid$R, 0.001)
min(grid2$R)
#grid$P <- -grid$k * (grid$T - 1) + grid$R
selection <- grid2[sample(nrow(grid2), 1),]

newGrid <- rbind(grid, grid2)

ggplot(grid2, aes(r, k, color=RSOFT)) +
  scale_colour_gradientn(colors=rainbow(4)) +
  geom_point(shape=15, size=14) + theme_bw()

payouts <- data.frame("Outcome"=possible_outcomes, 
                      "P1_Payout"=c(6, 1, 15, 2), 
                      "P2_Payout"=c(6, 15, 1, 2))



get_payout <- function(game_data, r, k, P=5, S=1, CONST = 80){
  
  #10 - 300
  TE <- r **2 * CONST + k * CONST
  R <- r * (r - S + TE - P) + S
  
  if(game_data == "cc"){
    return (c(R, R))
  }
  if(game_data == "cd"){
    return (c(S, TE))
  }
  if(game_data == "dc"){
    return (c(TE, S))
  }
  if(game_data == "dd"){
    return (c(P, P))
  }
}


get_payout("cc", runif(n = 1, max = 1, min = 0), runif(n = 1, max = 1, min = 0))



decide <- function(decision_model="Random", which_player=1, game_history=c(), round=1){
  possible_moves <- c("c", "d")
  possible_outcomes <- c("cc", "cd", "dc", "dd")
  
  if(decision_model=="Random"){
    return(sample(possible_moves, 1))
  }
  if(decision_model=="TitForTat"){
    if(game_history[1]=="00"){
      game_history<-sample(possible_moves, 2)
      oppo_move <- game_history[3-which_player]
      player_move <- oppo_move
      return(player_move)
    }
    last_move <- strsplit(game_history[length(game_history)], split='')[[1]]
    oppo_move <- last_move[3-which_player]
    player_move <- oppo_move
    return(player_move)
  }
  if(decision_model=="LastMove"){
    constant <- 3
    if(game_history[1]=="00"){
      game_history<-sample(possible_moves, 2)
      game_history <- paste0(game_history[1], game_history[2])
    }
    game_history <- game_history[length(game_history)]
    nums <- data.frame(table(c(possible_outcomes,game_history))-1)
    nums$Multiplic <- c(10.5, 1, 6.1, 2) ########## change
    nums$Value <- nums$Multiplic * nums$Freq + constant
    nums$p <- (nums$Value + exp(-16))
    nums$p <- nums$p/sum(nums$p)
    player_move <- sample(c("c","d"), size=1, prob = c(sum(nums$p[1:2]), sum(nums$p[3:4])))
    return(player_move)
  }
  if(decision_model=="HistoryAverage"){
    if(game_history[1]=="00"){
      game_history<-sample(possible_moves, 2)
      game_history <- paste0(game_history[1], game_history[2])
    }
    nums <- data.frame(table(c(possible_outcomes,game_history))-1)
    nums$Multiplic <- c(10.5, 1, 6.1, 2) ########## change
    # should be gotten from get_payouts()
    nums$Value <- nums$Multiplic * nums$Freq
    nums$p <- (nums$Value + exp(-16))
    nums$p <- nums$p/sum(nums$p)
    player_move <- sample(c("c","d"), size=1, prob = c(sum(nums$p[1:2]), sum(nums$p[3:4])))
    return(player_move)
  }
}

p1_strat <- "HistoryAverage"
p2_strat <- "LastMove"

decision_model<- c(p1_strat, p2_strat)
get_payout <- function(game_data, payouts){
  p <- payouts[ payouts$Outcome==game_data, ]
  p1p2vec <- c(p$P1_Payout, p$P2_Payout)
  return(p1p2vec)
}
#get_payout("cc", payouts)
multiplier <- function (payouts, ri=0.5, ci=0.5){
  S <- payouts[payouts$Outcome=="cd",]$P1_Payout
  R <- payouts[payouts$Outcome=="cc",]$P1_Payout
  TE <- payouts[payouts$Outcome=="dc",]$P1_Payout
  P <- payouts[payouts$Outcome=="dd",]$P1_Payout
  p <- ci
  expected_coop <- p*R + (1-p) * S
  expected_defect <- p* TE + (1-p) * P
  return(expected_coop / (expected_coop + expected_defect))
  #return(k * ci / (exp(-4) + (abs (ri - r))))
}
# Let's play!
play_pd <- function(payouts, nRounds, decision_model=c("Random", "Random"), r=0.5, k=0.5, ri=0.5, ci=0.5){
  
  game <- data.frame("Round"=c(1:nRounds), 
                     "p1_decision"=0, 
                     "p2_decision"=0, 
                     "p1_payout"=0, 
                     "p2_payout"=0,
                     "decision_model1"=decision_model[1],
                     "decision_model2"=decision_model[2])
  
  
  #decision_model="Random", which_player=1, game_history=c(), round=1  
  fullgame <- game[c("p1_decision", "p2_decision")]
  game$fullgame <- paste0(fullgame$p1_decision, fullgame$p2_decision)
  
  for(round in game$Round){
    g_hist <- game$fullgame[1:round-1]
    if(round==1){
      mult<-multiplier(payouts, ci=ci)
      g_hist <- paste0(sample(possible_moves, 1, prob=c(mult, 1-mult)), sample(possible_moves, 1, prob=c(mult, 1-mult)))
    }
    game[round,]$p1_decision <- decide(decision_model[1], 1, g_hist, round)
    game[round,]$p2_decision <- decide(decision_model[2], 2, g_hist, round)
    game$fullgame[round] <- paste0(game[round,]$p1_decision, game[round,]$p2_decision)
    game_payout <- get_payout(game$fullgame[round], payouts)
    game[round,]$p1_payout <- game_payout[1]
    game[round,]$p2_payout <- game_payout[2]
  }
  return(game)
}

num_sims <- 80
data_all <- data.frame()
for(i in c(1:num_sims)){
  nRounds <- 10
  data <- play_pd(payouts, nRounds, decision_model)
  data$p1Tot <- cumsum(data$p1_payout)
  data$p2Tot <- cumsum(data$p2_payout)
  data2 <- data.frame("Round"=data$Round, "SimNum"=i,
                      "Total_Payout"=c(data$p1Tot, data$p2Tot), 
                      "Player"=c(rep("P1", nRounds), rep("P2", nRounds)), 
                      "DecisionStrat"=c(rep(decision_model[1],nRounds),rep(decision_model[2],nRounds)))
  data_all <- rbind(data_all, data2)
}
tail(data_all)

df <- summarySE(data_all, measurevar="Total_Payout", groupvars = c("Player", "Round", "DecisionStrat"))

ggplot(df, aes(Round, Total_Payout, color=DecisionStrat)) +
  geom_line(size=.5, alpha=0.7) + 
  geom_ribbon(aes(ymin=Total_Payout-sd,ymax=Total_Payout+sd, fill=DecisionStrat),alpha=0.1, color=NA) +
  geom_point(position = "dodge", width=0.2) + xlim(c(0,10)) + ylab("Cumulative Payout") + #ylim(c(0,100)) +
  ggtitle("Prisoner's Dilemma") + 
  theme_minimal()

head(df)



summarySE <- function(data=NULL, measurevar, groupvars=NULL, na.rm=FALSE, conf.interval=.95, .drop=TRUE) {
  require(plyr)
  
  # New version of length which can handle NA's: if na.rm==T, don't count them
  length2 <- function (x, na.rm=FALSE) {
    if (na.rm) sum(!is.na(x))
    else       length(x)
  }
  
  # This is does the summary; it's not easy to understand...
  datac <- ddply(data, groupvars, .drop=.drop,
                 .fun= function(xx, col, na.rm) {
                   c( N    = length2(xx[,col], na.rm=na.rm),
                      mean = mean   (xx[,col], na.rm=na.rm),
                      sd   = sd     (xx[,col], na.rm=na.rm),
                      sum  = sum    (xx[,col], na.rm=na.rm)
                   )
                 },
                 measurevar,
                 na.rm
  )
  
  # Rename the "mean" column    
  datac <- rename(datac, c("mean"=measurevar))
  
  datac$se <- datac$sd / sqrt(datac$N)  # Calculate standard error of the mean
  
  # Confidence interval multiplier for standard error
  # Calculate t-statistic for confidence interval: 
  # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
  ciMult <- qt(conf.interval/2 + .5, datac$N-1)
  datac$ci <- datac$se * ciMult
  
  return(datac)
}





