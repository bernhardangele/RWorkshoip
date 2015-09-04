# Generate simulated data for a simple reaction time experiment.

# Assuming a 2x2 design

# Parameters

# Reaction time (in s)

n_subjects <- 20

n_trials <- 40

time_baseline <- 1.5 # Baseline reaction time (intercept)

time_effectA <- .2

time_effectB <- -.3

time_interactionAB <- .7

time_error_sd <- .25

# Errors

# Proportion correct

response_baseline <- .6

response_effectA <- -.1

response_effectB <- .25

response_interactionAB <- .15


generate_participant <- function(subject, n_trials, time_baseline, time_effectA, time_effectB, response_baseline, time_error_sd, response_effectA, response_effectB, response_interactionAB){
  if(n_trials %% 4 != 0){
    warning("Number of trials is not a multiple of number of conditions. Unbalanced design generated.")
  }
  trials <- 1:n_trials
  conditions <- rep(1:4, n_trials/4)
  # set up contrasts for factors
  factorA <- ifelse(conditions < 3, 1, -1)
  factorB <- ifelse(conditions %% 2, 1, -1)
  times <- time_baseline + factorA*time_effectA + factorB * time_effectB + factorA * factorB * time_interactionAB + rnorm(n_trials, mean = 0, sd = time_error_sd)
  probs <- response_baseline + factorA*response_effectA + factorB * response_effectB + factorA * factorB * response_interactionAB
  responses <- rbinom(probs, 1, probs)
  data.frame(subject = subject, trial = trials, condition = conditions, RT = times, correct = responses)
}

results_list <- lapply(1:n_subjects, generate_participant, n_trials, time_baseline, time_effectA, time_effectB, response_baseline, time_error_sd, response_effectA, response_effectB, response_interactionAB)

library(data.table)

results <- rbindlist(results_list)

# quick check of the results:

rdf <- results

rdf$factorA <- with(rdf,factor(ifelse(condition < 3, 'yes', 'no')))
rdf$factorB <- with(rdf,factor(ifelse(condition %% 2, 'yes', 'no')))

library(ez)

ezANOVA(rdf, RT, subject, within = .(factorA, factorB))

library(lme4)
summary(glmer(data = rdf, correct ~ factorA*factorB + (1|subject), family = binomial(link = "logit")))

# write data files

write_subjects <- function(subject_df){
  subject <- subject_df$subject[1]
  filename <- paste0("Subj", subject, ".txt")
  output_df <- with(subject_df, data.frame(trial = trial, condition = condition, RT = round(RT,4) , correct = correct))
  write.table(output_df, file = filename, sep = "\t", quote = FALSE, row.names = FALSE)
}

library(plyr)
d_ply(results, .(subject), write_subjects)
