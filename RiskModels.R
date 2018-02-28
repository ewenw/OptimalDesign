# 1. variables: riskiness (ri) and cooperativeness (ci) (pro-social value orientation / trust) of individual
# 2. multiplier: based on r and k values, calculate multiplier for each decision
# 3. higher risk-taking + higher r -> more cooperation
# 4. lower risk-taking + lower r -> more cooperation
# 5. higher k -> more cooperation

# returns a multiplier for probability of cooperation (between 0.1-10)
multiplier <- function (r=0.5, k=0.5, ri=0.5, ci=0.5){
  return(k * ci / (exp(-4) + (abs (ri - r))))
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

# returns the expected value of defection - expected value of cooperation
get_expected_diff <- function (payouts, p=0.5){
  S <- payouts[payouts$Outcome=="cd",]$P1_Payout
  R <- payouts[payouts$Outcome=="cc",]$P1_Payout
  TE <- payouts[payouts$Outcome=="dc",]$P1_Payout
  P <- payouts[payouts$Outcome=="dd",]$P1_Payout
  expected_coop <- p*R + (1-p) * S
  expected_defect <- p* TE + (1-p) * P
  return(expected_defect - expected_coop)
}


r_s = seq(from=0.1, to=0.9, length.out = 20)
k_s = seq(from=0.1, to=0.9, length.out = 20)
v = rep(0, 20)
payoffs <- data.frame(expand.grid(r=r_s, k=k_s), T=c(v), R=c(v), P=c(v), S=c(v))
for(i in 1:nrow(payoffs)){
  pay_tab <- get_payout_table(r=payoffs$r[i], k=payoffs$k[i])
  payoffs[i,]$T <- pay_tab$P1_Payout[3]
  payoffs[i,]$R <- pay_tab$P1_Payout[1]
  payoffs[i,]$P <- pay_tab$P1_Payout[4]
  payoffs[i,]$S <- pay_tab$P1_Payout[2]
}

payoffs_clean <- payoffs[payoffs$T < 5 & payoffs$T> 0,]

ggplot(payoffs_clean, aes(r, k, color=R)) +	
  scale_colour_gradientn(colors=rainbow(4)) +	
  geom_point(shape=15, size=14) + theme_bw()

