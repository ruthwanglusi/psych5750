library(haven)
library(emmeans)
library(readr)
library(labelled)
library(rstanarm)
library(BayesFactor)
library(easystats)
library(tidyverse)
library(BFpack)
library(dplyr)
library(tidyr)
library(knitr)
library(ggplot2)
library(ggdist)
library(broom)
library(scales)
library(pwr)
library(patchwork)
library(lme4)
library(lmerTest)
library(pbkrtest)
library(brms)

####### Question 1

## 1.1 Count Table

# Import the data from the URL
url <- "https://raw.githubusercontent.com/felixthoemmes/hwdatasets/master/oscarwinners.csv"
oscar_data <- read.csv(url)

# Convert Gender and US to factors
oscar_data$Gender <- as.factor(oscar_data$Gender)
oscar_data$US <- as.factor(oscar_data$US)

# Generate a table for counts and percentages
count_output <- oscar_data |>
  group_by(Gender, US) |>
  summarise(Count = n(), .groups = 'drop') |>
  mutate(Percentage = sprintf("%.2f", Count / sum(Count) * 100)) |>
  tibble()

count_table <- kable(count_output, 
      caption = "Oscar Winners by Gender and US Nationality")

# print(count_table)

## 1.2 Statistics Summary Table
#  The total sample
total_stats <- oscar_data |>
  summarise(
    Group = "Total Sample",
    N = n(), 
    M = mean(Age, na.rm = TRUE), 
    SD = sd(Age, na.rm = TRUE)
  ) |>
  tibble()

# All US males
us_males_stats <- oscar_data |>
  filter(US == "1" & Gender == "Male") |>
  summarise(
    Group = "All US Males",
    N = n(), 
    M = mean(Age, na.rm = TRUE), 
    SD = sd(Age, na.rm = TRUE)
  ) |>
  tibble()

# All US females
us_females_stats <- oscar_data |>
  filter(US == "1" & Gender == "Female") |>
  summarise(
    Group = "All US Females",
    N = n(), 
    M = mean(Age, na.rm = TRUE), 
    SD = sd(Age, na.rm = TRUE)
  ) |>
  tibble()

# All non-US males
non_us_males_stats <- oscar_data |>
  filter(US == "0" & Gender == "Male") |>
  summarise(
    Group = "All non-US Males",
    N = n(), 
    M = mean(Age, na.rm = TRUE), 
    SD = sd(Age, na.rm = TRUE)
  ) |>
  tibble()

# All non-US females
non_us_females_stats <- oscar_data |>
  filter(US == "0" & Gender == "Female") |>
  summarise(
    Group = "All non-US Females",
    N = n(), 
    M = mean(Age, na.rm = TRUE), 
    SD = sd(Age, na.rm = TRUE)
  ) |>
  tibble()

combined_stats <- bind_rows(total_stats, us_males_stats, us_females_stats, 
                            non_us_males_stats, non_us_females_stats)

combined_table <- kable(combined_stats,
                        caption = "Descriptive Statistics by Groups",
                        digits = 2)

# print(combined_table)

## 1.3 Kernel Density Plots
# Generate an overlaid kernel density plot for age split by gender
age_density_plot <- ggplot(oscar_data, aes(x = Age, fill = Gender)) +
  geom_density(alpha = 0.5) +
  labs(title = "Kernel Density Estimate of Age by Gender",
       x = "Age",
       y = "Density",
       fill = "Gender") +
  theme_minimal()

# Display the plot
# print(age_density_plot)

## 1.4 Line Plot
age_line_plot <- ggplot(oscar_data, aes(x = Year, y = Age, group = Gender, color = Gender)) +
  geom_line() +
  labs(title = "Age of Oscar Winners Over Time by Gender",
       x = "Year of Award",
       y = "Age of Winner",
       color = "Gender") +
  theme_minimal()

# Display the plot
# print(age_line_plot)

####### Question 2
# Frequentist
lm1 <- lm(Age ~ Gender, data = oscar_data)

em1<- emmeans(lm1, ~ Gender, contr = "pairwise")

em1_output <- summary(em1$contrasts, infer = T)

freq_q2 <- em1_output |>
  mutate(p.value = pvalue(p.value)) |>
  kable(caption = "Frequentist Mean Age Difference Between Genders",
        digits = 2)

# print(freq_q2)

####### Question 3
set.seed(5750)
#3.1 Bayes Factor
bf1 = anovaBF(Age ~ Gender, data = oscar_data)
# print(bf1)

#3.2 Estimate
# Compute Bayesian estimates
if(FALSE){
set.seed(5750)
blm1 <- stan_glm(Age ~ Gender, data = oscar_data,
                 prior_intercept = normal(35, 3, autoscale = TRUE),
                 prior = normal(0, 3, autoscale = TRUE),
                 iter = 10000,
                 diagnostic_file = file.path(tempdir(), "df.csv"))
}
bem1 <- emmeans(blm1, ~ Gender, contr = "pairwise")

blm1_post <- describe_posterior(bem1$contrasts,
                                ci = .95,
                                test = c("bayesfactor"),
                                bf_prior = blm1)

blm1_post$BF <- exp(blm1_post$log_BF)

bayes_q3 <- blm1_post |>
  tibble() |>
  kable(caption = "Bayesian Mean Age Difference Between Genders",
        digits = 2)

# print(bayes_q3)

####### Question 4
#4.1 Main Effects & 4.2 Interaction Effect
lm2 <- lm(Age ~ Gender * US, data = oscar_data)

# Estimate main effects using emmeans with sample-size weighting
em2 <- emmeans(lm2, specs = c("Gender", "US"), weights = "proportional")

# Joint tests of main effects
em2_joint <- joint_tests(em2)

em2_table <- em2_joint |>
  tibble() |>
  mutate(p.value = pvalue(p.value)) |>
  kable(caption = "Frequentist Main Effects and Interaction Effect",
        digits = 2)

# print(em2_table)

# 4.3 Differences in Marginal Means
# 4.3.1 Estimate marginal means for Gender with pairwise comparisons
em2_gender <- emmeans(lm2, specs = pairwise ~ Gender, adjust = "tukey")
em2_gender_contrast <- summary(em2_gender$contrasts, infer = c(TRUE, TRUE))

em2_gender_contrast_table <- em2_gender_contrast |>
  tibble() |>
  mutate(p.value = pvalue(p.value)) |>
  kable(caption = "Frequentist Gender Contrast",
        digits = 2)

# print(em2_gender_contrast_table)

# 4.3.2 Estimate marginal means for US with pairwise comparisons
em2_us <- emmeans(lm2, specs = pairwise ~ US, adjust = "tukey")
em2_us_contrast <- summary(em2_us$contrasts, infer = c(TRUE, TRUE))

em2_us_contrast_table <- em2_us_contrast |>
  tibble() |>
  mutate(p.value = pvalue(p.value)) |>
  kable(caption = "Frequentist Nationality Contrast",
        digits = 2)

# print(em2_us_contrast_table)

# 4.4 Conditional Effects

# 4.4.1 Gender Effects for US Winners
em2_cond_US <- emmeans(lm2, specs = ~ Gender * US, 
                       contr = list("Female - Male (US)" = c(0, 0, 1, -1)))$contrasts

em2_cond_US_sum <- summary(em2_cond_US, infer = c(TRUE, TRUE))

em2_cond_US_table <- em2_cond_US_sum |>
  tibble() |>
  mutate(p.value = pvalue(p.value)) |>
  kable(caption = "Frequentist Gender Effect for US Winners", digits = 2)

# print(em2_cond_US_table)

# 4.4.2 Gender Effects for non-US Winners
em2_cond_nonUS <- emmeans(lm2, specs = ~ Gender * US, 
                  contr = list("Female - Male (non-US)" = c(1, -1, 0, 0)))$contrasts

em2_cond_nonUS_sum <- summary(em2_cond_nonUS, infer = c(TRUE, TRUE))

em2_cond_nonUS_table <- em2_cond_nonUS_sum |>
  tibble() |>
  mutate(p.value = pvalue(p.value)) |>
  kable(caption = "Frequentist Gender Effect for Non-US Winners", digits = 2)

# print(em2_cond_nonUS_table)

# 4.5 Differences in Conditional Means
em2_cond_diff <- emmeans(lm2, specs = ~ Gender * US, 
                         contr = list("US - non-US" = c(-1, 1, 1, -1)))$contrasts

em2_cond_diff_sum <- summary(em2_cond_diff, infer = c(TRUE, TRUE))

em2_cond_diff_table <- em2_cond_diff_sum|>
  tibble() |>
  mutate(p.value = pvalue(p.value)) |>
  kable(caption = "Frequentist Difference in Gender Effect by Nationality", 
        digits = 2)

# print(em2_cond_diff_table)


####### Question 5
# 5.1 Bayes Factor

# 5.1.1 A null model
set.seed(5750)
bf2 <- anovaBF(Age ~ Gender * US, data = oscar_data)

# print(bf2)

# 5.1.2 A full model
# Extract Bayes Factors
bf3 <- anovaBF(Age ~ Gender * US, data = oscar_data, whichModels = "top")
# print(bf3)

# 5.2 Estimate
if(FALSE){
set.seed(5750)
blm2 <- stan_glm(Age ~ Gender * US, data = oscar_data,
                 prior_intercept = normal(0, 3, autoscale = TRUE),
                 prior = normal(0, 3, autoscale = TRUE),                 
                 iter = 10000,
                 diagnostic_file = file.path(tempdir(), "df2.csv"))
}

# 5.2.1 Gender Marginal Means and Difference

# gender marginal means
set.seed(5750)
bem2_gender <- emmeans(blm2, ~ Gender)

bem2_gender_post <- describe_posterior(bem2_gender,
                                       test = c("bayesfactor"), 
                                       bf_prior = blm2)

bem2_gender_table <- bem2_gender_post |>
  kable(caption = "Bayesian Gender Marginal Means", digits = 2)

# print(bem2_gender_table)

# difference in gender marginal means
set.seed(5750)
bem2_gender_contr <- emmeans(blm2, ~ Gender, contr = "pairwise")$contrasts

bem2_gender_contr_post <- describe_posterior(bem2_gender_contr,
                                       test = c("bayesfactor"), 
                                       bf_prior = blm2)

bem2_gender_contr_post_table <- bem2_gender_contr_post |>
  kable(caption = "Bayesian Gender Difference", digits = 2)

# print(bem2_gender_contr_post_table)

# 5.2.2 Nationality Marginal Means and Difference
set.seed(5750)

# nationality marginal means
set.seed(5750)
bem2_US <- emmeans(blm2, ~ US)

bem2_US_post <- describe_posterior(bem2_US,
                                   test = c("bayesfactor"), 
                                   bf_prior = blm2)

bem2_US_post_table <- bem2_US_post |>
  kable(caption = "Bayesian Nationality Marginal Means", digits = 2)

# print(bem2_US_post_table)

# difference in nationality marginal means
set.seed(5750)
bem2_US_contr <- emmeans(blm2, ~ US, contr = "pairwise")$contrasts

bem2_US_contr_post <- describe_posterior(bem2_US_contr,
                                         test = c("bayesfactor"), 
                                         bf_prior = blm2)

bem2_US_contr_post_table <- bem2_US_contr_post |>
  kable(caption = "Bayesian Nationality Difference", digits = 2)

# print(bem2_US_contr_post_table)

# 5.2.3 Conditional Effects
# Gender Effect for US Winners
set.seed(5750)
bem2_cond_US <- emmeans(blm2, specs = ~ Gender * US, 
                        contr = list("Female - Male (US)" = c(0, 0, 1, -1)))$contrasts
                        

bem2_cond_US_post <- describe_posterior(bem2_cond_US,
                                     test = c("bayesfactor"), 
                                     bf_prior = blm2)

bem2_cond_US_post_table <- bem2_cond_US_post|>
  kable(caption = "Bayesian Gender Effect for US Winners", digits = 2)

# print(bem2_cond_US_post_table)

# Gender Effect for non-US Winners
set.seed(5750)
bem2_cond_nonUS <- emmeans(blm2, specs = ~ Gender * US, 
                           contr = list("Female - Male (non-US)" = c(1, -1, 0, 0)))$contrasts

bem2_cond_nonUS_post <- describe_posterior(bem2_cond_nonUS,
                                        test = c("bayesfactor"), 
                                        bf_prior = blm2)

bem2_cond_nonUS_post_table <- bem2_cond_nonUS_post |>
  kable(caption = "Bayesian Gender Effect for Non-US Winners", digits = 2)

# print(bem2_cond_nonUS_post_table)

# 5.2.4 Gender Effect Difference (US vs. non-US)
set.seed(5750)
# Compute the contrasts for the difference in conditional effects
bem2_cond_diff <- emmeans(blm2, specs = ~ Gender * US, 
                          contr = list("US - non-US" = c(-1, 1, 1, -1)))$contrasts

# Describe the posterior for the contrast
bem2_cond_diff_post <- describe_posterior(bem2_cond_diff,
                                          test = c("bayesfactor"), 
                                          bf_prior = blm2)

# Create the table
bem2_cond_diff_post_table <- bem2_cond_diff_post |>
  kable(caption = "Bayesian Difference in Gender Effect by Nationality", digits = 2)

# print(bem2_cond_diff_post_table)


###### Question 6
# 6.1 Frequentist
em2_eqv <- emmeans(lm2, ~ US, 
                  weights = "proportional", 
                  contr = "pairwise")

freq_q6 <- summary(em2_eqv$contrasts, 
                   infer = TRUE,
                   null = 0, 
                   delta = 3, 
                   side = "equivalence")

# print(freq_q6)

# 6.2 Bayesian
# Old Answer:
set.seed(5750)
bem2_eqv <- emmeans(blm2, ~ US, 
                    weights = "proportional", 
                    contr = "pairwise")

bayes_q6 <- describe_posterior(bem2_eqv$contrasts,
                               ci = .95,
                               test = c("rope"),
                               rope_range = c(-3, 3))

# print(bayes_q6)

####### Question 7
set.seed(5750)
if(FALSE){
model1 <- brm(Age ~ Gender, data = oscar_data, 
              prior = c(prior(normal(0, 3), class = "b"), 
                        prior(normal(0, 3), class = "Intercept")),
              family = gaussian(),
              save_pars=save_pars(all=TRUE))

model2 <- brm(Age ~ Gender, data = oscar_data, 
              prior = c(prior(normal(0, 0.01), class = "b"), 
                        prior(normal(0, 3), class = "Intercept")),
              family = gaussian(),
              save_pars=save_pars(all=TRUE))

model3 <- brm(Age ~ Gender, data = oscar_data, 
              prior = c(prior(uniform(-3, 3), class = "b"), 
                        prior(normal(0, 3), class = "Intercept")),
              family = gaussian(),
              save_pars=save_pars(all=TRUE))

model4 <- brm(Age ~ Gender, data = oscar_data, 
              prior = c(prior(normal(0, 500), class = "b"), 
                        prior(normal(0, 3), class = "Intercept")),
              family = gaussian(),
              save_pars=save_pars(all=TRUE))

# Comparing model1 with model2, model3, and model4
bf_12 <- bayes_factor(model1, model2)
bf_13 <- bayes_factor(model1, model3)
bf_14 <- bayes_factor(model1, model4)

# Comparing model2 with model3 and model4
bf_23 <- bayes_factor(model2, model3)
bf_24 <- bayes_factor(model2, model4)

# Comparing model3 with model4
bf_34 <- bayes_factor(model3, model4)

# Print Bayes Factors
print(bf_12)
print(bf_13)
print(bf_14)
print(bf_23)
print(bf_24)
print(bf_34)
}

####### Question 8
# Create contrast matrix
contrast_matrix_1 <- matrix(0, nrow = 3, ncol = 5)
rownames(contrast_matrix_1) <- c("A1", "A2", "A3")
colnames(contrast_matrix_1) <- c("B1", "B2", "B3", "B4", "B5")

# Define the contrast for A1 vs. (B2 + B3 + B4 + B5)
contrast_matrix_1["A1", "B1"] <- 1
contrast_matrix_1["A1", c("B2", "B3", "B4", "B5")] <- -0.25

kable(contrast_matrix_1, format = "markdown")