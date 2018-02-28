
library(ggplot2)
library(GPfit)

source("GaussianProcess.r")
###############################################################
# Prisoner's Dilemma

possible_moves <- c("c", "d")
possible_outcomes <- c("cc", "cd", "dc", "dd")

# T > R > P > S
get_payout_table <- function(r=0.5, k=0.5, P=3, S=1, CONST = 25) {
  r<- r +0.6
  P<-log(P/((r*k))) 
  S<-log(S/((r*k))) 
  R <- ((k + 1) * P * r - k * S) / ((k * (r - 1) + r))
  T <- (R - P) / k + S 
  table <- data.frame("Outcome"=possible_outcomes, 
                      "P1_Payout"=c(R, S, T, P), 
                      "P2_Payout"=c(R, T, S, P))
  return(table)
}
get_payout <- function(game_data, r=0.5, k=0.5, P=3, S=1, CONST = 25){
  r<- r +0.6
  P<-log(P/((r*k))) 
  S<-log(S/((r*k))) 
  R <- ((k + 1) * P * r - k * S) / ((k * (r - 1) + r))
  T <- (R - P) / k + S 
  if(game_data == "cc"){
    return (c(R, R))
  }
  if(game_data == "cd"){
    return (c(S, T))
  }
  if(game_data == "dc"){
    return (c(T, S))
  }
  if(game_data == "dd"){
    return (c(P, P))
  }
}
get_coop_prob <- function (payouts, p=0.5){
  S <- payouts[payouts$Outcome=="cd",]$P1_Payout
  R <- payouts[payouts$Outcome=="cc",]$P1_Payout
  TE <- payouts[payouts$Outcome=="dc",]$P1_Payout
  P <- payouts[payouts$Outcome=="dd",]$P1_Payout
  expected_coop <- p*R + (1-p) * S
  expected_defect <- p* TE + (1-p) * P
  return(expected_coop / (expected_coop + expected_defect))
}
get_coop_prob_greedy <- function (payouts, p=0.5){
  S <- payouts[payouts$Outcome=="cd",]$P1_Payout
  R <- payouts[payouts$Outcome=="cc",]$P1_Payout
  TE <- payouts[payouts$Outcome=="dc",]$P1_Payout
  P <- payouts[payouts$Outcome=="dd",]$P1_Payout
  expected_coop <- p*R + (1-p) * S
  expected_defect <- p* TE + (1-p) * P
  prob_coop = expected_coop / (expected_coop + expected_defect)
  decision = -3 * (prob_coop-0.5) ** 2 + 0.9
  return(decision)
}
# hypothesis 3: expectation of cooperation's effect on cooperation is
# stronger for more risky games than less risky games
get_coop_prob_risk <- function (payouts, p=0.5, r){
  S <- payouts[payouts$Outcome=="cd",]$P1_Payout
  R <- payouts[payouts$Outcome=="cc",]$P1_Payout
  TE <- payouts[payouts$Outcome=="dc",]$P1_Payout
  P <- payouts[payouts$Outcome=="dd",]$P1_Payout
  expected_coop <- (p*R + (1-p) * S) * (r+0.5) ** 2
  expected_defect <- p* TE + (1-p) * P
  return(expected_coop / (expected_coop + expected_defect))
}

decide <- function(payouts, p, decision_model="Random", which_player=1, game_history=c(), round=1, r=0.5, k=0.5){
  possible_moves <- c("c", "d")
  possible_outcomes <- c("cc", "cd", "dc", "dd")
  
  
  # model based on expected values of payouts and constant p
  if(decision_model=="Expectation"){
    prob_coop <- get_coop_prob(payouts, p)
    return(sample(possible_moves, 1, prob=c(prob_coop, 1-prob_coop)))
  }
  # model based on maximizing tempatation outcomes when expecting other player to cooperate
  else if(decision_model=="ExpectationGreedy"){
    # if second round or later
    if(round>3){
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
    prob_coop <- get_coop_prob_greedy(payouts, p)
    return(sample(possible_moves, 1, prob=c(prob_coop, 1-prob_coop)))
  }
  # model based on expected values of payouts and constant p
  else if(decision_model=="ExpectationRisk"){
    prob_coop <- get_coop_prob_risk(payouts, p, r)
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
  pa <- payouts[ payouts$Outcome==game_data, ]
  p1p2vec <- c(pa$P1_Payout, pa$P2_Payout)
  return(p1p2vec)
}
# Let's play!
play_pd <- function(payouts, nRounds, decision_model=c("Random", "Random"), r=0.5, k=0.5, p1, p2){
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
    game[round,]$p1_decision <- decide(payouts, p1, decision_model[1], 1, g_hist, round, r=r, k=k)
    game[round,]$p2_decision <- decide(payouts, p2, decision_model[2], 2, g_hist, round, r=r, k=k)
    game$fullgame[round] <- paste0(game[round,]$p1_decision, game[round,]$p2_decision)
    game_payout <- get_payout(game$fullgame[round], payouts)
    game[round,]$p1_payout <- game_payout[1]
    game[round,]$p2_payout <- game_payout[2]
  }
  return(game)
}

simulate <- function(num_sims = 200, nRounds = 10, r = 0.5, k = 0.5, decision_model){
  data_all <- data.frame()
  for(i in c(1:num_sims)){
    if(i %% 100==0){
      print(i)
    }
    random = runif(min=0.01, max=0.99, n=2)
    p1 <- random[1]
    p2 <- random[2]
    payouts <- get_payout_table(r, k)
    data <- play_pd(payouts, nRounds, decision_model, r=r, k=k, p1=p1, p2=p2)
    data$p1Tot <- cumsum(data$p1_payout)
    data$p2Tot <- cumsum(data$p2_payout)
    
    data2 <- data.frame("Round"=data$Round, "SimNum"=i, "p"=p1,
                        "Total_Payout"=c(data$p1Tot, data$p2Tot), 
                        "Player"=c(rep("P1", nRounds), rep("P2", nRounds)),
                        "FullGame"=data$fullgame,
                        "DecisionStrat"=c(rep(decision_model[1],nRounds),rep(decision_model[2],nRounds)))
    data_all <- rbind(data_all, data2)
    
    
  }
  
  return(data_all)
}

bin_to_dec <- function(x) {
  sum(2^(which(rev(unlist(strsplit(as.character(x), "")) == 1))-1))
}

get_distribution <- function(data, num_sims=80, nRounds = 2){
  Outcome <- c()
  Payout <- c()
  for(i in 1:num_sims){
    rows <- data[data$SimNum == i,]
    payout <- max(rows$Total_Payout)
    combination <- ""
    for(j in 1:nRounds){
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
    Payout <- c(Payout, payout)
    
  }
  distr <- data.frame(Outcome, "Model"=rep(data$DecisionStrat[1], length(Outcome)), "Cost"=Payout)
  return(distr)
}

# simulates experiment models and returns the KL Divergence
search <- function(r, k, rounds = 6, sims = 500){
  #results_titfortat <- simulate(r=r, k =k, num_sims = sims, nRounds = rounds, decision_model = c("TitForTat", "TitForTat"))
  results_adaptive <- simulate(r=r, k =k, num_sims = sims, nRounds = rounds, decision_model = c("AdaptiveExpectation", "AdaptiveExpectation"))
  results_expectation_risk <- simulate(r=r, k =k, num_sims = sims, nRounds = rounds, decision_model = c("ExpectationRisk", "ExpectationRisk"))
  results_expectation <- simulate(r=r, k =k, num_sims = sims, nRounds = rounds, decision_model = c("Expectation", "Expectation"))
  results_greedy <- simulate(r=r, k =k, num_sims = sims, nRounds = rounds, decision_model = c("ExpectationGreedy", "ExpectationGreedy"))
  
  #sims_titfortat <- get_distribution(data=results_titfortat, num_sims=sims, nRounds=rounds)
  sims_adaptive <- get_distribution(data=results_adaptive, num_sims=sims, nRounds=rounds)
  sims_expectation_risk <- get_distribution(data=results_expectation_risk, num_sims=sims, nRounds=rounds)
  sims_expectation <- get_distribution(data=results_expectation, num_sims=sims, nRounds=rounds)
  sims_greedy <- get_distribution(data=results_greedy, num_sims=sims, nRounds=rounds)
  sims_combined <- rbind(sims_adaptive, sims_greedy, sims_expectation, sims_expectation_risk)
  
  all_outcomes <- c(sims_adaptive$Outcome, sims_expectation_risk$Outcome, 
                    sims_expectation$Outcome, sims_greedy$Outcome)
  all_payouts <- c(sims_adaptive$Cost, sims_expectation_risk$Cost, 
                    sims_expectation$Cost, sims_greedy$Cost)
  game_data <- data.frame("Outcome"=all_outcomes, "Cost"=all_payouts)
  game_data <- aggregate(game_data, by=list(game_data$Outcome, game_data$Cost), FUN=mean)[c("Outcome", "Cost")]
  
  outcomes <- as.integer(unique(c(sims_adaptive$Outcome, sims_expectation_risk$Outcome, 
                     sims_expectation$Outcome, sims_greedy$Outcome)))
  
  p0 <- exp(-12)
  tab_adapt <- data.frame(table( c(sims_adaptive$Outcome, outcomes) )-1)
  tab_adapt$Model <- "Adapt"
  tab_adapt$Freq <- (tab_adapt$Freq + p0) / sum(tab_adapt$Freq+p0)
  tab_expec <- data.frame(table( c(sims_expectation$Outcome, outcomes) )-1)
  tab_expec$Model <- "Expec"
  tab_expec$Freq <- (tab_expec$Freq + p0) / sum(tab_expec$Freq+p0)
  tab_exris <- data.frame(table( c(sims_expectation_risk$Outcome, outcomes) )-1)
  tab_exris$Model <- "Exris"
  tab_exris$Freq <- (tab_exris$Freq + p0) / sum(tab_exris$Freq+p0)
  tab_greed <- data.frame(table( c(sims_greedy$Outcome, outcomes) )-1)
  tab_greed$Model <- "Greed"
  tab_greed$Freq <- (tab_greed$Freq + p0) / sum(tab_greed$Freq+p0)
  tab_all <- rbind(tab_adapt, tab_expec, tab_exris, tab_greed)  
  
  tab_all$Cost<-c(1:dim(tab_all)[1])
  
  for(i in 1:nrow(tab_all)){
    tab_all$Cost[i] <- game_data[game_data$Outcome==tab_all$Var1[i],]$Cost
  }
  
  #for(i in tab_all$Cost){
   # outcome_i <- tab_all[tab_all$Cost==i,]$Var1
   # tab_all[tab_all$Cost==i,]$Cost <- game_data[game_data$Outcome==outcome_i,]$Cost
    #tab_all$Cost[tab_all$Cost==i] <- sum(game_data[game_data$Outcome==outcome_i,]$Cost)
  #}
  
  tab_all$ModFreq <- tab_all$Freq / tab_all$Cost
  #tab_all[tab_all$Model=="Adapt",]$ModFreq <- tab_all[tab_all$Model=="Adapt",]$ModFreq / sum(tab_all[tab_all$Model=="Adapt",]$ModFreq)
  #tab_all[tab_all$Model=="Expec",]$ModFreq <- tab_all[tab_all$Model=="Expec",]$ModFreq / sum(tab_all[tab_all$Model=="Expec",]$ModFreq)
  #tab_all[tab_all$Model=="Exris",]$ModFreq <- tab_all[tab_all$Model=="Exris",]$ModFreq / sum(tab_all[tab_all$Model=="Exris",]$ModFreq)
  #tab_all[tab_all$Model=="Greed",]$ModFreq <- tab_all[tab_all$Model=="Greed",]$ModFreq / sum(tab_all[tab_all$Model=="Greed",]$ModFreq)
  model_names <- unique(tab_all$Model)
  
  return (c(kl_divergence(tab_all, model_names, col="Freq", asymmetric=F), 
            kl_divergence(tab_all, model_names, col="ModFreq", asymmetric=F)))
}


nPoints = 4
r_s = seq(from=0.1, to=0.9, length.out = nPoints)
k_s = seq(from=0.1, to=0.9, length.out = nPoints)
found_points_I <- expand.grid(r=r_s, k=k_s)
found_points_IE <- data.frame(found_points_I)
for(i in 1:nrow(found_points_I)){
  print(i)
  search_results = search(found_points_I$r[i], found_points_I$k[i], sims=500)
  found_points_I$I[i] <- search_results[1]
  found_points_IE$I[i] <- search_results[2]
}
#found_points_IE$I <- sum(found_points_IE$I)
plotSurface(found_points_I, n=100, title="Information Surface (16 uniform samples, 500 sims)")
plotSurface(found_points_IE, n=100, title="Information Efficiency ($ cost weighted) Surface (16 uniform samples, 500 sims)")

r=0.6
k=0.5
sims=100
rounds = 6
results_adaptive <- simulate(r=r, k =k, num_sims = sims, nRounds = rounds, decision_model = c("AdaptiveExpectation", "AdaptiveExpectation"))
results_expectation_risk <- simulate(r=r, k =k, num_sims = sims, nRounds = rounds, decision_model = c("ExpectationRisk", "ExpectationRisk"))
results_expectation <- simulate(r=r, k =k, num_sims = sims, nRounds = rounds, decision_model = c("Expectation", "Expectation"))
results_greedy <- simulate(r=r, k =k, num_sims = sims, nRounds = rounds, decision_model = c("ExpectationGreedy", "ExpectationGreedy"))

#sims_titfortat <- get_distribution(data=results_titfortat, num_sims=sims, nRounds=rounds)
sims_adaptive <- get_distribution(data=results_adaptive, num_sims=sims, nRounds=rounds)
sims_expectation_risk <- get_distribution(data=results_expectation_risk, num_sims=sims, nRounds=rounds)
sims_expectation <- get_distribution(data=results_expectation, num_sims=sims, nRounds=rounds)
sims_greedy <- get_distribution(data=results_greedy, num_sims=sims, nRounds=rounds)
sims_combined <- rbind(sims_adaptive, sims_greedy, sims_expectation, sims_expectation_risk)
ggplot(sims_combined, aes(x=Outcome, fill=Model)) +
  geom_density(alpha=.35) +
  scale_colour_gradient2() +
  xlim(0, max(sims_combined$Outcome)) +
  labs(title="Outcomes Distribution (r = " %&% r %&% ", k = " %&% k %&% ", " %&% rounds %&% " rounds, " %&% sims %&% " simulations)") +
  labs(x="Outcomes", y="Occurences") + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))




num_sims = 400
plotResults(simulate(r=0.5, k =0.5, num_sims = num_sims, nRounds = 6, decision_model = c("AdaptiveExpectation", "ExpectationGreedy")))



plotResults <- function(data_all){
  
  df <- summarySE(data_all, measurevar="Total_Payout", groupvars = c("Player", "Round", "DecisionStrat"))
  
  ggplot(df, aes(Round, Total_Payout, color=DecisionStrat)) +
    geom_line(size=.5, alpha=0.7) + 
    geom_ribbon(aes(ymin=Total_Payout-sd,ymax=Total_Payout+sd, fill=DecisionStrat),alpha=0.1, color=NA) +
    geom_point(position = "dodge", width=0.2) + xlim(c(1,6)) + ylab("Cumulative Payout") + #ylim(c(0,100)) +
    ggtitle("Prisoner's Dilemma (" %&%num_sims%&% " simulations, " %&% "6 rounds)") + 
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





