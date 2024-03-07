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
library(brms)
library(broom)
library(scales)
library(pwr)
library(patchwork)
library(lme4)
library(lmerTest)
library(pbkrtest)



# Question 1

# Import the data from the URL
url <- "https://github.com/felixthoemmes/hwdatasets/blob/master/rt.csv?raw=true"
data <- read.csv(url)

data$ID <- as.factor(data$ID)
data$cond <- as.factor(data$cond)

data$cond <- factor(data$cond, levels = c("low", "med", "hi"))

# Group by Condition
cond_summary <- data |>
  group_by(cond) |>
  summarize(
    mean = mean(y, na.rm = TRUE),
    sd = sd(y, na.rm = TRUE),
    n = n()
  ) 

cond_summary_table <- kable(cond_summary,
        caption = "Summary table for Reaction Time by Intensity",
        digits = 2,
        col.names = c("Intensity", "Mean", "SD", "Count")
        )

# print(cond_summary_table)

# Question 2

## 2.1 Boxplot 

# Boxplot for Reaction Time by Distraction Intensity Levels
box_plot <- ggplot(data, aes(x = cond, y = y, fill = cond)) +
  geom_boxplot() +
  labs(title = "Boxplot of Reaction Time by Intensity",
       x = "Distraction Intensity",
       y = "Reaction Time") +
  theme_minimal()

# print(box_plot)

## 2.2 Spaghetti plot
# Calculate average reaction time for each person in each condition
average_data <- data |>
  group_by(ID, cond) |>
  summarize(avg_rt = mean(y, na.rm = TRUE))

# Create spaghetti plot
average_plot <- ggplot(average_data, aes(x = cond, y = avg_rt, 
                                       group = ID, 
                                       color = as.factor(ID))) +
  geom_line(alpha = 0.5) + 
  labs(title = "Spaghetti Plot of Average Reaction Time by Intensity",
       x = "Distraction Intensity",
       y = "Average Reaction Time") +
  theme_minimal() + 
  ylim(0, 13) +
  theme(legend.position = "none") # Hide the legend

# print(average_plot)

# Question 3
# Fit linear model with ID and condition as predictors and their interaction
lm1 <- lm(y ~ ID * cond, data = data)

## 3.1 Means of the conditions
means_cond <- summary(emmeans(lm1, ~ cond), infer = T)

means_cond_table <- means_cond |> 
  mutate(p.value = pvalue(p.value)) |>
  kable(caption = "Frequentist Means of Conditions",
        digits = 2)

# print(means_cond_table)

## 3.2 Main effect
main_effect <- joint_tests(lm1)

main_effect_table <- main_effect |>
  tibble() |>
  filter(`model term` == "cond") |>
  mutate(p.value = pvalue(p.value)) |>
  kable(caption = "Main Effect of Condition",
        digits = 2)

# print(main_effect_table)

## 3.3 Pairwise differences between the marginal means
pairwise_comparisons <- summary(emmeans(lm1, ~ cond, contr="pairwise"), infer=T)

pairwise_comparisons_table <- pairwise_comparisons$contrasts |>
  mutate(p.value = pvalue(p.value)) |>
  kable(caption = "Pairwise Comparisons between Conditions",
        digits = 2)

# print(pairwise_comparisons_table)

# Question 4
lm2 <- lmer(y ~ 1 + cond + (1 + cond|ID), data = data)

## 4.1 Means of the conditions
means_cond_2 <- summary(emmeans(lm2, ~ cond),infer=T)

means_cond_table_2 <- means_cond_2 |> 
  mutate(p.value = pvalue(p.value)) |>
  kable(caption = "Frequentist Means of Conditions 2",
        digits = 2)

# print(means_cond_table_2)

## 4.2 Main effect
main_effect_2 <- joint_tests(lm2)

main_effect_table_2 <- main_effect_2 |> 
  mutate(p.value = pvalue(p.value)) |>
  kable(caption = "Frequentist Main Effect of Condition 2",
        digits = 2)

# print(main_effect_table_2)

## 4.3 Pairwise differences between the marginal means
pairwise_comparisons_2 <- summary(emmeans(lm2, ~ cond, contr="pairwise"), infer=T)

pairwise_comparisons_table_2 <- pairwise_comparisons_2$contrasts |> 
  mutate(p.value = pvalue(p.value)) |>
  kable(caption = "Frequentist Pairwise Comparisons between Conditions 2",
        digits = 2)

# print(pairwise_comparisons_table_2)

## 4.4 Summarize the model to get the random effects variability
sum_lm2 <- summary(lm2)
# print(sum_lm2)

if (FALSE) {
# Question 5
## 5.1 A null model against all models
set.seed(5750)
bf_null <- anovaBF(y ~ cond * ID, data = data, whichModels = "bottom")
# print(bf_null)

# 5.2 Full model against all models
bf_full <- anovaBF(y ~ cond * ID, data = data, whichModels = "top")
# print(bf_full)

## 5.3 Bayesian estimation
set.seed(5750)
if (FALSE) {
blm1 <- stan_glm(y ~ cond * ID, data = data,
                   family = gaussian(link = "identity"), 
                   prior = normal(0,3, autoscale=TRUE),
                   prior_intercept = normal(0,3, autoscale=TRUE))
}

# group means posterior
means_cond <- summary(emmeans(blm1, "cond"), infer = c(TRUE, TRUE))

means_cond_table <- means_cond|>
  tibble() |>
  kable(caption = "Bayesian Means of All Conditions",
        digits = 2)

# print(means_cond_table)

# pairwise comparison
pair_comp <- summary(emmeans(blm1,"cond", contr = "pairwise")$contrasts, 
                     infer = c(TRUE, TRUE))

pair_comp_table <- pair_comp |>
  tibble() |>
  kable(caption = "Bayesian Pairwise Comparisons",
        digits = 2)
  
# pair_comp_table
}

if (FALSE) {
# Question 6
## 6.1 A null model against all models
set.seed(5750)

## 6.2 All models against a null model 
bf_null_2 <- anovaBF(y ~ cond * ID, data = data,
                         whichRandom = "ID", 
                         whichModels = "bottom")
# print(bf_null_2)

# Full model against all models
bf_full_2 <- anovaBF(y ~ cond * ID, data = data,
                         whichRandom = "ID", 
                         whichModels = "top")
print(bf_full_2)

## 6.3 Bayesian estimation
if (FALSE) {
blm2 <- stan_glmer(y ~ 1 + cond + (1 + cond|ID), data = data,
                   family = gaussian(link = "identity"), 
                   prior = normal(0,3, autoscale=TRUE),
                   prior_intercept = normal(0,3, autoscale=TRUE))

}
# group means posterior
means_cond_2 <- summary(emmeans(blm2, "cond"), infer = c(TRUE, TRUE))

means_cond_table_2 <- means_cond_2 |>
  tibble() |>
  kable(caption = "Bayesian Means of All Conditions 2",
        digits = 2)

# print(means_cond_table_2)

set.seed(5750)
pair_comp_2 <- summary(emmeans(blm2,"cond", contr = "pairwise")$contrasts, 
                     infer = c(TRUE, TRUE))

# pair_comp_table_2 <- pair_comp_2 |>
  tibble() |>
  kable(caption = "Bayesian Pairwise Comparisons 2",
        digits = 2)
  
# print(pair_comp_table_2)
}

# Question 7
# Fixed Effects Prediction Model
data$pred_rt_1 <- predict(lm1, newdata = data)

plot_fixed <- ggplot(data, aes(x = cond, y = pred_rt_1, 
                                   group = ID, 
                                   color = as.factor(ID))) +
                geom_line(alpha = 0.5) +
                labs(title = "Fixed Effects",
                x = "Distraction Intensity",
                y = "Predicted Reaction Time") +
                theme_minimal() +
                ylim(0, 13) +
                theme(legend.position = "none") # Hide the legend

# Random Effects Prediction Model
data$pred_rt_2 <- predict(lm2, newdata = data)

plot_random <- ggplot(data, aes(x = cond, y = pred_rt_2, 
                                   group = ID, 
                                   color = as.factor(ID))) +
                geom_line(alpha = 0.5) +
                labs(title = "Random Effects",
                x = "Distraction Intensity",
                y = "Predicted Reaction Time") +
                theme_minimal() +
                ylim(0, 13) +
                theme(legend.position = "none") # Hide the legend

average_plot <- average_plot + 
                labs(title = "Average")

plots_comp <- average_plot | plot_fixed | plot_random

print(plots_comp)