library(ggplot2)
library(GPfit)

###############################################################
# Prisoner's Dilemma

possible_moves <- c("c", "d")
possible_outcomes <- c("cc", "cd", "dc", "dd")

get_payout_table <- function(r=0.5, k=0.5, P=5, S=1, CONST = 80) {
  #10 - 300
  TE <- r * CONST + k * CONST
  R <- r * (r - S + TE - P) + S
  table <- data.frame("Outcome"=possible_outcomes, 
             "P1_Payout"=c(R, S, TE, P), 
             "P2_Payout"=c(R, TE, S, P))
  return(table)
}
get_payout <- function(game_data, r=0.5, k=0.5, P=5, S=1, CONST = 80){
  
  #10 - 300
  TE <- r * CONST + k * CONST
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
get_coop_prob <- function (payouts, p=0.5, NOISE=2){
  S <- payouts[payouts$Outcome=="cd",]$P1_Payout
  R <- payouts[payouts$Outcome=="cc",]$P1_Payout
  TE <- payouts[payouts$Outcome=="dc",]$P1_Payout
  P <- payouts[payouts$Outcome=="dd",]$P1_Payout
  expected_coop <- p*R + (1-p) * S + runif(1, -NOISE/2, NOISE/2)
  expected_defect <- p* TE + (1-p) * P
  return(expected_coop / (expected_coop + expected_defect))
}
decide <- function(payouts, p, decision_model="Random", which_player=1, game_history=c(), round=1){
  possible_moves <- c("c", "d")
  possible_outcomes <- c("cc", "cd", "dc", "dd")
  

  # model based on expected values of payouts and constant p
  if(decision_model=="Expectation"){
    prob_coop <- get_coop_prob(payouts, p)
    return(sample(possible_moves, 1, prob=c(prob_coop, 1-prob_coop)))
  }
  # calculates p based on history
  else if(decision_model=="AdaptiveExpectation"){
    # if second round or later
    if(round>2){
      game_history <- game_history[length(game_history)]
      nums <- data.frame(table(c(possible_outcomes, game_history))-1)
      nums$p <- (nums$Freq+ exp(-16))
      nums$p <- nums$p/sum(nums$p)
      # calculate historical probability of opponent cooperating
      if(which_player==1) {
        p <- sum(nums$p[c(1,3)])
      }
      else {
        p <- sum(nums$p[c(1,2)])
      }
    }
    prob_coop <- get_coop_prob(payouts, p)
    return(sample(possible_moves, 1, prob=c(prob_coop, 1-prob_coop)))
  }
  else if(decision_model=="Random"){
    return(sample(possible_moves, 1))
  }
  else if(decision_model=="TitForTat"){
    if(round==1){
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
  else if(decision_model=="LastMove"){
    constant <- 3
    if(round==1){
      game_history<-sample(possible_moves, 2)
      game_history <- paste0(game_history[1], game_history[2])
    }
    game_history <- game_history[length(game_history)]
    nums <- data.frame(table(c(possible_outcomes, game_history))-1)
    nums$Multiplic <- c(10.5, 1, 6.1, 2) ########## change
    nums$Value <- nums$Multiplic * nums$Freq + constant
    nums$p <- (nums$Value + exp(-16))
    nums$p <- nums$p/sum(nums$p)
    player_move <- sample(c("c","d"), size=1, prob = c(sum(nums$p[1:2]), sum(nums$p[3:4])))
    return(player_move)
  }
  else if(decision_model=="HistoryAverage"){
    if(round==1){
      game_history <- sample(possible_moves, 2)
      game_history <- paste0(game_history[1], game_history[2])
    }
    nums <- data.frame(table(c(possible_outcomes, game_history))-1)
    
    nums$Multiplic <- c(10.5, 1, 6.1, 2) ########## change
    # should be gotten from get_payouts()
    nums$Value <- nums$Multiplic * nums$Freq
    nums$p <- (nums$Value + exp(-16))
    nums$p <- nums$p/sum(nums$p)
    player_move <- sample(c("c","d"), size=1, prob = c(sum(nums$p[1:2]), sum(nums$p[3:4])))
    return(player_move)
  }
}

get_payout <- function(game_data, payouts){
  p <- payouts[ payouts$Outcome==game_data, ]
  p1p2vec <- c(p$P1_Payout, p$P2_Payout)
  return(p1p2vec)
}
# Let's play!
play_pd <- function(payouts, nRounds, decision_model=c("Random", "Random"), r=0.5, k=0.5, p){
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
    game[round,]$p1_decision <- decide(payouts, p, decision_model[1], 1, g_hist, round)
    game[round,]$p2_decision <- decide(payouts, p, decision_model[2], 2, g_hist, round)
    game$fullgame[round] <- paste0(game[round,]$p1_decision, game[round,]$p2_decision)
    game_payout <- get_payout(game$fullgame[round], payouts)
    game[round,]$p1_payout <- game_payout[1]
    game[round,]$p2_payout <- game_payout[2]
  }
  return(game)
}

simulate <- function(num_sims = 200, nRounds = 10, r = 0.5, k = 0.5, decision_model){
  data_all <- data.frame()
  for(p in seq(from=0.1, 0.9, length=4)){
    for(i in c(1:num_sims)){
      if(i %% 100==0){
        print(i)
      }
      payouts <- get_payout_table(r, k)
      data <- play_pd(payouts, nRounds, decision_model, r=r, k=k, p=p)
      data$p1Tot <- cumsum(data$p1_payout)
      data$p2Tot <- cumsum(data$p2_payout)
      data2 <- data.frame("Round"=data$Round, "SimNum"=i, "p"=p,
                          "Total_Payout"=c(data$p1Tot, data$p2Tot), 
                          "Player"=c(rep("P1", nRounds), rep("P2", nRounds)),
                          "FullGame"=data$fullgame,
                          "DecisionStrat"=c(rep(decision_model[1],nRounds),rep(decision_model[2],nRounds)))
      data_all <- rbind(data_all, data2)
      
    }
  }
  
  return(data_all)
}

bin_to_dec <- function(x) 
  sum(2^(which(rev(unlist(strsplit(as.character(x), "")) == 1))-1))

get_distribution <- function(data, num_sims=80, nRounds = 2){
  Outcome <- c()
  for(i in 1:num_sims){
    rows <- data[data$SimNum == i,]
    combination <- ""
    for(j in 1:nRounds){
      #print(rows[j, 6])
      #print(a)
      
      if(rows[j,6] == "cc"){
        bits <- "00"
      }
      if(rows[j,6] == "cd"){
        bits <- "01"
      }
      if(rows[j,6] == "dc"){
        bits <- "10"
      }
      if(rows[j,6] == "dd"){
        bits <- "11"
      }
      combination <- paste0(combination, bits, sep="")
      #combination <- paste0(combination, rows[j, 6], sep="")
    }
    Outcome <- c(Outcome, bin_to_dec(combination))
    
  }
  distr <- data.frame(Outcome, "Model"=rep(data$DecisionStrat[1], length(Outcome)))
  return(distr)
}
rounds = 5
sims = 140
r = 0.9
k = 0.5
results_titfortat <- simulate(r=r, k =k, num_sims = sims, nRounds = rounds, decision_model = c("TitForTat", "TitForTat"))
results_adaptive <- simulate(r=r, k =k, num_sims = sims, nRounds = rounds, decision_model = c("AdaptiveExpectation", "AdaptiveExpectation"))
results_expectation <- simulate(r=r, k =k, num_sims = sims, nRounds = rounds, decision_model = c("Expectation", "Expectation"))

sims_titfortat <- get_distribution(data=results_titfortat, num_sims=sims, nRounds=rounds)
sims_adaptive <- get_distribution(data=results_adaptive, num_sims=sims, nRounds=rounds)
sims_expectation <- get_distribution(data=results_expectation, num_sims=sims, nRounds=rounds)

sims_combined <- rbind(sims_titfortat, sims_adaptive, sims_expectation)
sims_table <- data.frame(table(sims_combined))

'%&%' <- function(x, y)paste0(x,y)

ggplot(sims_combined, aes(x=Outcome, fill=Model)) +
  geom_density(alpha=.5) +
  scale_colour_gradient2() +
  xlim(0, max(sims_combined$Outcome)) +
  labs(title="Outcomes Distribution (r = " %&% r %&% ", k = " %&% k %&% ", " %&% rounds %&% " rounds, " %&% sims %&% " simulations)") +
  labs(x="Outcomes", y="Occurences") + 
  theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          panel.background = element_blank(), 
          axis.line = element_line(colour = "black"))


ggplot(sims_table) +
  geom_bar(aes(reorder(Outcome, Freq), Freq, fill=Model),
            alpha = .5, stat="identity") +
  labs(title="Outcomes Distribution") +
  labs(x="Outcomes", y="Occurences") + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                             panel.background = element_blank(), axis.line = element_line(colour = "black"))


plotResults(simulate(r=0.5, k =0.5, num_sims = 5, nRounds = 10, decision_model = c("HistoryAverage", "AdaptiveExpectation")))
plotResults <- function(data_all){
  
  df <- summarySE(data_all, measurevar="Total_Payout", groupvars = c("Player", "Round", "DecisionStrat"))
  
  ggplot(df, aes(Round, Total_Payout, color=DecisionStrat)) +
    geom_line(size=.5, alpha=0.7) + 
    geom_ribbon(aes(ymin=Total_Payout-sd,ymax=Total_Payout+sd, fill=DecisionStrat),alpha=0.1, color=NA) +
    geom_point(position = "dodge", width=0.2) + xlim(c(0,10)) + ylab("Cumulative Payout") + #ylim(c(0,100)) +
    ggtitle("Prisoner's Dilemma") + 
    theme_minimal()
}




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





