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