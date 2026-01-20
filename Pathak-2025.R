library("rsprite2")
library("scrutinity")
library("esc")
library("xlsx")

options(scipen = 999)

setwd("/media/ploederl/ssdata/data/artikel/meine/diverses-spielereien/Pathak-2021-ketamine")

d = read.xlsx(file="Table_2b_MSSI_Pathak_2021.xlsx", sheetIndex=1)

# https://jamesheathers.curve.space/#assessing-p-values-with-stalt

pfromt = function(mean1,sd1,n1,mean2,sd2,n2){
  t_stat <- (mean1 - mean2) / sqrt((sd1^2 / n1) + (sd2^2 / n2))
  df <- n1 + n2 - 2
  p_value <- 2 * pt(-abs(t_stat), df)
  p = paste0("p = ", p_value)
  return(p)
}

cohendci = function(mean1,sd1,n1,mean2,sd2,n2){
  cd = esc_mean_sd(grp1m = mean1, grp1sd = sd1, grp1n = n1,
                          grp2m = mean2, grp2sd = sd2, grp2n = n2, 
                          es.type="d")
  cdci=paste0(round(cd$es,2), " [", 
              round(cd$ci.lo,2)," to ",
              round(cd$ci.hi,2),"]")
  return(cdci)
}

effsizes = pvalues = rep(NA,nrow(d))

for(i in 1:nrow(d)){
  mean1 = d$mean1[i]; sd1 = d$sd1[i]; n1=d$n1[i]
  mean2 = d$mean2[i]; sd2 = d$sd2[i]; n2=d$n2[i]
  pvalues[i] = pfromt(mean1=mean1, sd1=sd1, n1=n1, 
                      mean2=mean2, sd2=sd2, n2=n2)
  
  effsizes[i] = cohendci(mean1=mean1, sd1=sd1, n1=n1, 
                       mean2=mean2, sd2=sd2, n2=n2)
}
  
d$Cohen_d = effsizes
d$p = pvalues

write.xlsx2(d, "results1.xlsx")


# Now assuming the "standard-error" (conflating SE and SD)

d$sd1=d$sd1*sqrt(d$n1)
d$sd2=d$sd2*sqrt(d$n2)

effsizes = pvalues = rep(NA,nrow(d))

for(i in 1:nrow(d)){
  mean1 = d$mean1[i]; sd1 = d$sd1[i]; n1=d$n1[i]
  mean2 = d$mean2[i]; sd2 = d$sd2[i]; n2=d$n2[i]
  pvalues[i] = pfromt(mean1=mean1, sd1=sd1, n1=n1, 
                      mean2=mean2, sd2=sd2, n2=n2)
  
  effsizes[i] = cohendci(mean1=mean1, sd1=sd1, n1=n1, 
                         mean2=mean2, sd2=sd2, n2=n2)
}

d$Cohen_d = effsizes
d$p = pvalues

write.xlsx2(d, "results2_correcedSDs.xlsx")




d = read.xlsx(file="Table_2b_MSSI_Pathak_2021.xlsx", sheetIndex=1)


for(i in 1:nrow(d)){
  mean = d$mean1[i]; sd = d$sd1[i]; n = d$n1[i]
  print(GRIM_test(mean = mean, n_obs=n))
  mean = d$mean2[i]; sd = d$sd2[i]; n=d$n2[i]
  print(GRIM_test(mean = mean, n_obs=n)) 
}


for(i in 1:nrow(d)){
  mean = d$mean1[i]; sd = d$sd1[i]; n = d$n1[i]
  print(GRIMMER_test(mean = mean, sd = sd, n_obs = n))
  mean = d$mean2[i]; sd = d$sd2[i]; n=d$n2[i]
  print(GRIMMER_test(mean = mean, sd = sd, n_obs = n))  
}



# All tests are GRIMMER-consistent

# Table 2, HDRS
# Baseline
GRIMMER_test(mean = 21.68, sd = 2.90, n_obs = 75)
GRIMMER_test(mean = 21.68, sd = 2.90, n_obs = 80) 

GRIMMER_test(mean = 21.47, sd = 3.33, n_obs = 75)
GRIMMER_test(mean = 21.47, sd = 3.33, n_obs = 80)

GRIMMER_test(mean = 22.47, sd = 2.84, n_obs = 75)
GRIMMER_test(mean = 22.47, sd = 2.84, n_obs = 80)

# Week 8
GRIMMER_test(mean = 18.81, sd = 2.83, n_obs = 75)
GRIMMER_test(mean = 18.81, sd = 2.83, n_obs = 80)

# Week 16
GRIMMER_test(mean = 13.45, sd = 3.08, n_obs = 75)
GRIMMER_test(mean = 13.45, sd = 3.08, n_obs = 80)

# 6 Mo FU
GRIMMER_test(mean = 12.36, sd = 3.16, n_obs = 75)

# 12 Mo FU
GRIMMER_test(mean = 12.39, sd = 2.28, n_obs = 75)



# Table 2, PSQI
# Baseline
GRIMMER_test(mean = 18.07, sd = 3.24, n_obs = 75)
GRIMMER_test(mean = 18.07, sd = 3.24, n_obs = 80)



##############################

# Week 8 effect sizes

# comparison bw the two PT groups 
mean1=18.81; mean2=17.73; sd1=2.83; sd2=3.03; n1=n2=75
esc_mean_sd(grp1m = mean1, grp1sd = sd1, grp1n = n1, 
            grp2m = mean2, grp2sd = sd2, grp2n = n2)
t_stat <- (mean1 - mean2) / sqrt((sd1^2 / n1) + (sd2^2 / n2))
df <- n1 + n2 - 2
p_value <- 2 * pt(-abs(t_stat), df)
p_value



# DIT vs pharma
esc_mean_sd(grp1m = 18.81, grp1sd = 2.83, grp1n = 80, 
            grp2m = 15.72, grp2sd = 2.92, grp2n = 80)


# CBT vs pharma (for paper)
mean1=17.73; mean2=15.72; sd1=3.03; sd2=2.92; n1=n2=80

esc_mean_sd(grp1m = mean1, grp1sd = sd1, grp1n = n1, 
            grp2m = mean2, grp2sd = sd2, grp2n = n2)



# now with the meta-analytic SD by Cipriani
sd1 = sd2 = 7.31
esc_mean_sd(grp1m = mean1, grp1sd = sd1, grp1n = n1, 
            grp2m = mean2, grp2sd = sd2, grp2n = n2)

t_stat <- (mean1 - mean2) / sqrt((sd1^2 / n1) + (sd2^2 / n2))
df <- n1 + n2 - 2
p_value <- 2 * pt(-abs(t_stat), df)
p_value





# CBT vs. DIT





# 12mo FU, two different PT arms
mean1=13.39; mean2=15.75; sd1=2.28; sd2=2.79; n1=n2=75
esc_mean_sd(grp1m = mean1, grp1sd = sd1, grp1n = n1, 
            grp2m = mean2, grp2sd = sd2, grp2n = n2)
t_stat <- (mean1 - mean2) / sqrt((sd1^2 / n1) + (sd2^2 / n2))
df <- n1 + n2 - 2
p_value <- 2 * pt(-abs(t_stat), df)
p_value



### Probability of observing the Symmetries
### With the help of Chaptgpt, with my modifications


# Arm sizes
N0 <- c(80, 80, 80)

# Numbers remaining at each stage. 
stages <- list(
  stage1 = c(80, 80, 80),
  stage2 = c(78, 79, 79),
  stage3 = c(76, 77, 76),
  stage4 = c(75, 75, 75)
)

# Compute dropouts per stage
dropouts <- lapply(seq_along(stages), function(i) {
  if (i == 1) N0 - stages[[i]]
  else stages[[i-1]] - stages[[i]]
})

names(dropouts) <- names(stages)
dropouts



multinom_prob <- function(d) {
  D <- sum(d)
  if (D == 0) return(1)
  dmultinom(d, prob = rep(1/3, 3))
}

stage_probs <- sapply(dropouts, multinom_prob)
stage_probs

joint_prob <- prod(stage_probs)
joint_prob






# Note: this function simplifies this: 
# Multinomial probabilities for each stage
stage1 = c(0, 0, 0)
stage2 = c(2, 1, 2)
stage3 = c(2, 2, 3)
stage4 = c(1, 2, 1)


p <- rep(1/3, 3)
prob_stage1 <- dmultinom(stage1, prob = p)
prob_stage2 <- dmultinom(stage2, prob = p)
prob_stage3 <- dmultinom(stage3, prob = p)
prob_stage4 <- dmultinom(stage4, prob = p)

prob_stage1
prob_stage2
prob_stage3
prob_stage4

joint_prob = prob_stage1*prob_stage2*prob_stage3*prob_stage4
joint_prob


  x^near_sym_prob <- function(D) {
  if (D == 0) return(1)
  
  # all possible nonnegative integer triplets summing to D
  configs <- expand.grid(d1 = 0:D, d2 = 0:D)
  configs$d3 <- D - configs$d1 - configs$d2
  configs <- subset(configs, d3 >= 0)
  
  symmetric <- apply(configs, 1, function(x) max(x) - min(x) <= 1)
  
  sum(dmultinom(configs[symmetric, ], prob = rep(1/3, 3)))
}

near_probs <- sapply(dropouts, function(d) near_sym_prob(sum(d)))
near_probs



all_numbers <- unlist(stages)
table(all_numbers %% 10) / length(all_numbers)






# Simulation of the observed symmetry (is only one)
# observed dropouts per stage
obs_stage2 <- c(2, 1, 1)
obs_stage3 <- c(2, 2, 3)
obs_stage4 <- c(1, 2, 1)

# observed symmetry (range)
obs_range2 <- max(obs_stage2) - min(obs_stage2)  # = 1
obs_range3 <- max(obs_stage3) - min(obs_stage3)  # = 1
obs_range4 <- max(obs_stage4) - min(obs_stage4)  # = 1


set.seed(123)
n_sim <- 100000       # number of simulated trials
p_dropout <- 0.0208     # (5/80 / 3) dropout probability per stage
n_total <- 240        # total participants

count_extreme <- 0

for (i in 1:n_sim) {
  
  all_stages_ok <- TRUE
  
  for (stage in 1:3) {
    
    # simulate total dropouts at this stage
    D <- rbinom(1, n_total, p_dropout)
    
    # if nobody drops out, symmetry is perfect
    if (D == 0) {
      stage_range <- 0
    } else {
      # assign dropouts randomly to 3 arms
      d <- rmultinom(1, size = D, prob = c(1/3, 1/3, 1/3))
      stage_range <- max(d) - min(d)
    }
    
    # check if symmetry is as good as observed
    if (stage_range > 1) {
      all_stages_ok <- FALSE
      break
    }
  }
  
  if (all_stages_ok) {
    count_extreme <- count_extreme + 1
  }
}

# Estimate probabillity
# The probability that all three later stages are as symmetric as observed,
# given very low attrition and random dropout.
count_extreme / n_sim
  




### General Simulation
# observed dropout patterns
obs_stage2 <- c(2, 1, 1)
obs_stage3 <- c(2, 2, 3)
obs_stage4 <- c(1, 2, 1)

# observed ranges
obs_range2 <- max(obs_stage2) - min(obs_stage2)  # 1
obs_range3 <- max(obs_stage3) - min(obs_stage3)  # 1
obs_range4 <- max(obs_stage4) - min(obs_stage4)  # 1
set.seed(123)

n_sim <- 100000     # number of simulated trials
count_match <- 0   # counts trials as symmetric as observed

for (i in 1:n_sim) {
  
  ok <- TRUE
  
  # ----- Stage 2: D = 4 -----
  d2 <- rmultinom(1, size = 4, prob = c(1/3, 1/3, 1/3))
  if (max(d2) - min(d2) > obs_range2) ok <- FALSE
  
  # ----- Stage 3: D = 7 -----
  d3 <- rmultinom(1, size = 7, prob = c(1/3, 1/3, 1/3))
  if (max(d3) - min(d3) > obs_range3) ok <- FALSE
  
  # ----- Stage 4: D = 4 -----
  d4 <- rmultinom(1, size = 4, prob = c(1/3, 1/3, 1/3))
  if (max(d4) - min(d4) > obs_range4) ok <- FALSE
  
  if (ok) count_match <- count_match + 1
}
count_match / n_sim




# No do a simulation which requires a that the end is 75/75/75
set.seed(123)

n_sim <- 100000
count_match <- 0

for (i in 1:n_sim) {
  
  # cumulative dropouts per arm
  cum <- c(0, 0, 0)
  
  # ---- Stage 1: 8 weeks, D = 0 ----
  # nothing to do
  
  # ---- Stage 2: 16 weeks, D = 4 ----
  d2 <- rmultinom(1, size = 4, prob = c(1/3, 1/3, 1/3))
  cum <- cum + d2
  
  # symmetry check at 16 weeks
  if (max(cum) - min(cum) > 1) next
  
  # ---- Stage 3: 6 months, D = 7 ----
  d3 <- rmultinom(1, size = 7, prob = c(1/3, 1/3, 1/3))
  cum <- cum + d3
  
  # symmetry check at 6 months
  if (max(cum) - min(cum) > 1) next
  
  # ---- Stage 4: 12 months, D = 4 ----
  d4 <- rmultinom(1, size = 4, prob = c(1/3, 1/3, 1/3))
  cum <- cum + d4
  
  # final exact equality check
  if (cum[1] == cum[2] && cum[2] == cum[3]) {
    count_match <- count_match + 1
  }
}

count_match / n_sim




https://chatgpt.com/c/6966406c-ec74-832c-8964-d792127a0e9c
############################################################
# Digit-preference and heaping checks for CONSORT numbers
# Design-fixed numbers (e.g. 80/80/80 at randomisation)
# are EXCLUDED to avoid inflating evidence.
#
# These tests are SCREENING TOOLS, not proof of misconduct.
# They are interpreted in combination with symmetry and
# attrition-pattern analyses.
############################################################

## -----------------------------
## 1. Enter DATA-DRIVEN numbers
## -----------------------------
# Included:
# - Screening and selection counts (not fixed by design)
# - Follow-up Ns after randomisation
#
# Excluded:
# - 80/80/80 at randomisation
# - 80/80/80 at first follow-up (zero attrition, protocol-driven)

numbers <- c(
  300, 240,          # screened and randomised
  78, 79, 79,        # 16 weeks
  76, 77, 76,        # 6 months
  75, 75, 75         # 12 months
)

numbers


## -----------------------------
## 2. Extract terminal digits
## -----------------------------
# Terminal digits (0–9) are used to detect rounding or
# digit preference (e.g. excess of 0 and 5)

last_digits <- numbers %% 10
last_digits


## -----------------------------
## 3. Descriptive digit table
## -----------------------------
digit_table <- table(last_digits)
digit_table

# Proportion of each terminal digit
digit_table / length(last_digits)


## -----------------------------
## 4. Chi-square test
## -----------------------------
# Null hypothesis:
# Terminal digits are uniformly distributed (0–9).
#
# Caveat:
# - Low power due to small sample size
# - Used mainly as a descriptive check

chisq.test(digit_table)


## -----------------------------
## 5. Focused test: excess of 0 and 5
## -----------------------------
# Rationale:
# Human rounding strongly favours digits 0 and 5.
# This test has higher power and is more appropriate here.

is_0_or_5 <- last_digits %in% c(0, 5)
table(is_0_or_5)

# Under uniform digits, probability of 0 or 5 = 2/10 = 0.20
binom.test(
  sum(is_0_or_5),
  length(last_digits),
  p = 0.2
)


## -----------------------------
## 6. Heaping at multiples of 5
## -----------------------------
# Counts that are exact multiples of 5 are examined.
# This is common in rounded or estimated reporting.

is_mult_5 <- numbers %% 5 == 0
table(is_mult_5)

# Expected proportion under no rounding ≈ 0.20
binom.test(
  sum(is_mult_5),
  length(numbers),
  p = 0.2
)


## -----------------------------
## 7. Heaping at multiples of 10
## -----------------------------
# Very conservative check.
# Excess here is particularly suspicious.

is_mult_10 <- numbers %% 10 == 0
table(is_mult_10)

# Expected proportion under no rounding ≈ 0.10
binom.test(
  sum(is_mult_10),
  length(numbers),
  p = 0.1
)


############################################################
# INTERPRETATION NOTES (for reports / reviews):
#
# - Design-fixed numbers were excluded to avoid bias.
# - Small sample size limits power of digit tests.
# - An excess of digits 0 and 5 suggests round-number
#   preference but is NOT diagnostic on its own.
# - When combined with:
#     * repeated symmetry across three arms,
#     * exact equality of cumulative attrition (5/5/5),
#     * zero early attrition,
#   th

