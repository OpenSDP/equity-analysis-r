###############################################################################
# Generate Synthetic Data In Texas Assessment Format
# Date: 06/08/2018
# Author: Jared E. Knowles
# Prepared for OpenSDP
################################################################################

## Identify the data structure needed 
## File - student grade 3-8
## grade_level 
## school_code
## sid
## male
## race_ethnicity
## eco_dis
## title_1
## migrant
## lep
## iep
## rdg_ss
## math_ss
## wrtg_ss
## composition
## wrtg_perc

## Install the synthesizer to generate the data
## This step is optional if you already have it installed
## Current package depends on this fork of the simglm package
## Uncomment to use
#devtools::install_github("jknowles/simglm")
## Install the OpenSDP data synthesizer
## Uncomment to use
# devtools::install_github("OpenSDP/OpenSDPsynthR")

## 

library(OpenSDPsynthR)
set.seed(0525212) # set the seed
library(magrittr)
library(stringr)
# The synthesizer needs some input paramaters
# As it is the defaults are not sufficient to give realistic assessment data
# These change those defaults to make the scores less deterministic

assess_adj <- sim_control()$assessment_adjustment

# Make scores spread out more
assess_adj$perturb_base <- function(x, sd) 
  {
    mean_shift <- rnorm(1, sd = 3)
    y <- x + rnorm(1, mean_shift, sd * 0.8)
    return(y)
  }

assess_adj$gender_list <- list("Male" = 3, 
                    "Female" = -3)

assess_adj$frl_list <- list("0" = 0.05, 
                            "1" = -0.1)

# Downadjust the IEP difference


# Get defaults
assess_sim_par <- OpenSDPsynthR::sim_control()$assess_sim_par
# Increase score variance
assess_sim_par$error_var <- 15
# Increase coefficient effects
assess_sim_par$fixed_param <- assess_sim_par$fixed_param * 10
# Downgrade IEP difference
assess_sim_par$fixed_param[4] <- -0.75
# Downgrade LEP difference
assess_sim_par$fixed_param[5] <- -1
assess_sim_par$fixed_param[6] <- 0
assess_sim_par$fixed_param[7] <- 1
assess_sim_par$lvl1_err_params$mean <- 1
assess_sim_par$lvl1_err_params$sd <- 10
# Set group level variances
assess_sim_par$random_param$random_var <- c(0.4, 0.75)
# Set the school-grade size ranges
assess_sim_par$unbalanceRange <- c(100, 420)

# Conduct the simulation
stu_pop <- simpop(5000L, seed = 0525212, 
                  control = sim_control(nschls = 12L, n_cohorts = 4L, 
                                        assessment_adjustment = assess_adj,
                                        assess_sim_par = assess_sim_par))

# Build analysis file from the datasets in the list produced by simpop
#
out_data <- dplyr::left_join(stu_pop$stu_assess, stu_pop$stu_year)
#
out_data <- out_data %>% select(-exit_type, -cohort_grad_year, -cohort_year, -enrollment_status, 
                                -grade_enrolled, -grade_advance, -ndays_attend, 
                                -ndays_possible)
out_data <- left_join(out_data, stu_pop$demog_master %>% 
                        select(sid, Sex, Race))
# Conver back to dataframe
out_data <- as.data.frame(out_data)

# Get sample model output to check relationships among variables
summary(lm(math_ss~rdg_ss + grade + frpl + ell + iep + Sex +  gifted, data = out_data))

# Perturb by student to give student scores dependence on an unobserved student 
# talent - too deterministic still
out_data <- out_data %>% group_by(sid) %>% 
  mutate(talent = rnorm(1, 10, 20), 
         coef_t = rnorm(1, 10, 0.5),
         coef_a = rnorm(1, 5, 2),
         coef_z = rnorm(1, 1.5, 1)) %>% 
  mutate(coef_tb = coef_t + rnorm(1, 0, 1)) %>%
  ungroup %>% 
  mutate(rdg_ss = rdg_ss + coef_t*talent + coef_a * age + 3*age + rnorm(1, 25, 10), 
         math_ss = coef_z*rdg_ss + coef_tb*talent + 3*age + coef_a*age + rnorm(1, 25, 10)) %>% 
  select(-coef_t, -coef_tb, -coef_z, -coef_a,-talent) %>% ungroup %>% as.data.frame

# Fill in missing variables
out_data$migrant <- NA
out_data$wrtg_ss <- NA
out_data$composition <- NA
out_data$title_1 <- NA

# Define an export
export <- out_data %>% 
  select(sid, schid, grade, year, Sex, Race, frpl, title_1, 
         migrant, ell, iep, rdg_ss, math_ss, wrtg_ss, composition)

## Reshape Assessment scores to map to TASS
assess_distr <- read.csv("man/texas_score_distributions.csv", 
                         stringsAsFactors = FALSE)


match_distr <- function(source, target){
  # source is a numeric vector that can be converted to percentiles
  # target is the assess_distr data from Texas above that can be 
  # subset by subject and grade
  out <- ntile(source, n = 100) # generate percentiles
  mod1 <- loess(scale_score ~ perc, data = target) # map percentiles to scores
  newdata <- data.frame("perc" = out) # use the new data to make predictions
  yhat <- predict(mod1, newdata = newdata) # predict from the loess model
  min_score <- min(target$scale_score) # find LOSS
  max_score <- max(target$scale_score) # find HOSS
  yhat[yhat > max_score] <- max_score # truncate
  yhat[yhat < min_score] <- min_score
  return(yhat)
}

###################################################################
# Uncomment code to step through the match_distr function above
####################################################################
# plotdf <- assess_distr[assess_distr$grade == 3 & 
#                          assess_distr$subject == "math", ]
# ggplot(plotdf, aes(x = scale_score, y = cum_perc)) + 
#   geom_bar(stat="identity", fill = "white", color = "black") + theme_bw() + 
#   geom_smooth()
# mod1 <- loess(scale_score ~ perc, data = plotdf)
# source <- stu_pop$stu_assess %>% filter(grade == 3) %>% ungroup %>%
#   select(math_ss) %>% pull
# zz <- match_distr(source = source, target = plotdf)

for (j in c("math", "rdg")) {
  for (i in c(3:8)) {
    if (j == "math") {
      export$math_ss[out_data$grade == i] <- 
        match_distr(source = out_data$math_ss[export$grade == i], 
                    target = assess_distr[assess_distr$subject == j & 
                                            assess_distr$grade == i,])
    } else if (j == "rdg") {
      export$rdg_ss[out_data$grade == i] <- 
        match_distr(source = out_data$math_ss[export$grade == i], 
                    target = assess_distr[assess_distr$subject == j & 
                                            assess_distr$grade == i,])
    } else if (j == "wrtg") {
      export$wrtg_ss[out_data$grade == i] <- 
        match_distr(source = out_data$math_ss[export$grade == i], 
                    target = assess_distr[assess_distr$subject == j & 
                                            assess_distr$grade == i,])
    }
  }
}


# Where is year?
## Assign variable names
names(export) <- c("sid", "school_code", "grade_level", "year", "male", 
                   "race_ethnicity", 
                   "eco_dis", "title_1", "migrant", "lep", "iep", "rdg_ss", 
                   "math_ss", "wrtg_ss", "composition")


# Final tweaks for Texas
export %<>% filter(grade_level %in% c("3", "4", "5", "6", "7", "8"))

# 9 digit sid
export$sid <- stringr::str_pad(as.numeric(export$sid), 9, pad = "0")
# 9 digit school_code
export$school_code <- stringr::str_pad(as.numeric(export$school_code), 
                                       9, pad = "0")
# Race = first letter
export$race_ethnicity %<>% as.character %>% substr(1, 1)
# sex = first letter
export$male %<>% as.character %>% substr(1, 1)
# lep = 1/0
export$lep %<>% as.numeric
# econ_dis = 1/0
export$eco_dis %<>% as.numeric

### Better match Texas data
# Reform SES variable with additional levels
table(export$eco_dis)
export$eco_dis <- sapply(export$eco_dis, 
                         function(x) ifelse(runif(1) > 0.95, 9, x))
table(export$eco_dis)
export$eco_dis <- sapply(export$eco_dis, 
                         function(x) ifelse(x == 1 & runif(1) > 0.75, 2, x))
table(export$eco_dis)

## Brute force gender gap

export %<>% group_by(grade_level) %>% 
  mutate(math_ss = ifelse(male == "M", math_ss + (0.2 * sd(math_ss)), 
                          math_ss))


# Check final model 
summary(lm(math_ss ~ grade_level +  factor(eco_dis) + lep + male + iep, data = export))


# Save
save(export, file = "data/synth_texas.rda")
write.csv(export, file = "data/synth_texas.csv", 
            row.names = FALSE)


## Tests to evaluate the synthetic data
# summary({m1 <- lm(math_ss ~ rdg_ss, data = out_data)})
# summary({m2 <- lm(rdg_ss ~ math_ss, data = out_data)})
# 
# m1_b <- lm(math_ss_b ~ rdg_ss_b, data = out_data)
# m1 <- lm(math_ss ~ . , data = out_data[, -1])
