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


# Question 1

# import data
data_url <- "https://github.com/felixthoemmes/hwdatasets/blob/master/arguments.sav?raw=true"
data <- read_sav(data_url)

# convert to factors
data$monitors <- labelled::to_factor(data$monitors)
data$argument <- labelled::to_factor(data$argument)
data$source <- labelled::to_factor(data$source)

value_summary <- data |>
  group_by(factor, monitors, argument, source) |>
  summarise(
    n = n(),
    mean = mean(value, na.rm = TRUE),
    SD = sd(value, na.rm = TRUE)
  ) |>
  ungroup()

value_summary_table <- value_summary |>
  kable(caption = "Mean and Standard Deviation of Value Across Groups",
        digits = 2)

# print(value_summary_table)

# Question 2

value_summary_plot <- ggplot(data, aes(x=value)) +
  geom_density(fill="blue", alpha=0.5) +
  facet_grid(monitors ~ argument + source, scales = "free") +
  theme_minimal() +
  labs(title="Kernel Density Estimates for Value Across Groups",
       x="Value",
       y="Density") +
  xlim(0, 8) +
  ylim(0,0.6)

# print(value_summary_plot)

# Question 3

# f-test
lm1 <- lm(value ~ monitors * argument * source, data = data)

joint_summary <- joint_tests(lm1) |>
  tidy() |>
  mutate(p.value = pvalue(p.value)) |>
  kable(caption = "Joint Tests for Main Effects and Interactions",
        col.names = c("Term", "df1", "df2", "F-statistic", "p-value"),
        digits = c(0,2,2,2,3))

# print(joint_summary)

# Question 4

contrast_results <- emmeans(lm1, ~ monitors * argument | source, 
                     contr = "pairwise", adjust = "tukey")

contrast_summary <- summary(contrast_results, 
                    infer = c(TRUE, TRUE), level = 0.95)

contrast_df <- contrast_summary$contrasts |>
               mutate(p.value = pvalue(p.value)) 

freq_q4 <- kable(contrast_df, 
      digits = c(0, 0, 2, 2, 0, 2, 2, 2, 3),
      caption = "Three-Way Interactions Between Monitors, Argument, and Source") 
      

# print(freq_q4)


if (FALSE) {
# Question 5

# 5.1
#all models against a null model 
set.seed(5750)

data_processed <- data |>
  select(value, a = argument, s = source, m = monitors)

blm1 <- anovaBF(value ~ m * s * a, data = data_processed)

# print(blm1)

# 5.2
set.seed(5750)
#all models against full model
blm2 <- anovaBF(value ~ m * s * a, 
                data = data_processed, whichModel = "top") 
                #  "top" refers to the most complex or "full" model 
#print(blm2)
}

# Question 6
## 6.1 Frequentist Test
em1 <- emmeans(lm1, specs = c("source","argument","monitors"),
               contr = list(c1 = c(0.5, -0.5, -0.5, 0.5, 0.5, -0.5, -0.5, 0.5)))


#print(em1) # to see the coefficient assignment order

freq_q6 <- kable(em1$contrasts, 
      digits = 2,
      caption = "Frequentist Custom Contrast")

# print(freq_q6)

# 6.2 Bayesian Test
if(FALSE) {
set.seed(5750)

blm3 <- stan_glm (value ~ 1 + monitors * argument * source, data = data,
                  prior_intercept = normal(0,3, autoscale = TRUE), 
                  prior = normal(0,3, autoscale = TRUE),
                  iter = 10000)

}

bem3 <- emmeans(blm3, specs = c("source", "argument", "monitors"),
                contr = list(c1 = c(0.5, -0.5, -0.5, 0.5, 0.5, -0.5, -0.5, 0.5)))
                # see the photo for coefficient assignment
# print(bem3) 

bayes_q6 <- kable(bem3$contrasts, 
      digits = 2,
      caption = "Bayesian Custom Contrast")

# print(bayes_q6)


# Question 7
## 7.1 Frequentist Equivalence Test
em2 <- emmeans(lm1, specs = c("argument", "monitors", "source"))
# print(em2)

# Difference between an attractive source and an expert source when presented with a weak argument, 
# and averaged over high and low self-monitors equivalent 

lm1_eqv <- summary((emmeans(lm1,c("argument","monitors","source"),
                            contr=list(c1 = c(0, 0.5, 0, 0.5, 0, -0.5, 0, -0.5)),
                            side = "equivalence",
                            null = 0,
                            delta = .4)$contrasts),
                    infer=TRUE)


freq_q7 <- kable(lm1_eqv, 
      digits = c(0, 2, 2, 2, 2, 2, 2, 3),
      caption = "Frequentist Equivalence Test")

# print(freq_q7)

if(FALSE) {
## 7.2 Bayesian Equivalence Test

                  

blm3_eqv <- bayesfactor_parameters((emmeans(blm3,c("argument","monitors","source"),
                                    contr=list(c1 = c(0, 0.5, 0, 0.5, 0, -0.5, 0, -0.5)))$contrasts),
                                    prior = blm3,
                                    null = c(-0.4,0.4))

print(blm3_eqv)
}

# Question 10
# construct individual data from the table
party <- factor(c(rep("D",212+266),rep("R",209+201),rep("I",22+90)))
y0 <- c(rep(1.0,212+266),rep(0.9,209+201),rep(0.9,22+90))
y1 <- c(rep(1.5,212+266),rep(1.3,209+201),rep(0.1,22+90))
treat <- c(rep(0,212),rep(1,266),rep(0,209),rep(1,201),rep(0,22),rep(1,90))
yobs <- ifelse(treat == 0,y0,y1)
df1 <- tibble(party,y0,y1,treat,yobs)

#true ATE
ATE <- mean(y1 - y0)

#prima facie (unadjusted)
PTE <- lm(yobs ~ treat)
pte_effect <- coef(summary(PTE))["treat", "Estimate"]

#main effect Type III SS
Unweighted_MM <- emmeans(lm(yobs~treat*party),~treat, contr = "revpairwise",weights = "equal")
unweighted_effect <- summary(Unweighted_MM)$contrasts[, "estimate"]

#marginal sample-size weighted effect
Weighted_MM <- emmeans(lm(yobs~treat*party),~treat, contr = "revpairwise",weights = "proportional")
weighted_effect <- summary(Weighted_MM)$contrasts[, "estimate"]

# Create a data frame for the results
results <- data.frame(
  Effect = c("ATE", "PTE", "Unweighted_MM", "Weighted_MM"),
  Value = round(c(ATE, pte_effect, unweighted_effect, weighted_effect), 2)
)

# Print the results using kable()
q10_table <- kable(results, col.names = c("Effect", "Value"))

print(q10_table)

###### below is old False code
if(FALSE) {
# Given data from the table
Y_0 <- c(D=1.0, R=0.9, I=0.9)
Y_1 <- c(D=1.5, R=1.3, I=0.1)
N_T_0 <- c(D=212, R=209, I=22)
N_T_1 <- c(D=266, R=201, I=90)

# 1. Average Treatment Effect (ATE)
ATE <- sum(N_T_1 * Y_1 + N_T_0 * Y_0) / sum(N_T_1 + N_T_0)

# 2. Prima-Facie Treatment Effect (PTE)
PTE <- (sum(N_T_1 * Y_1) / sum(N_T_1)) - (sum(N_T_0 * Y_0) / sum(N_T_0))

# 3. Main Effect of Treatment using Unweighted Marginal Means
Unweighted_MM <- mean(Y_1) - mean(Y_0)

# 4. Main Effect of Treatment using Sample-size Weighted Marginal Means
Weighted_MM_Y_1 <- sum(N_T_1 * Y_1) / sum(N_T_1)
Weighted_MM_Y_0 <- sum(N_T_0 * Y_0) / sum(N_T_0)
Weighted_MM <- Weighted_MM_Y_1 - Weighted_MM_Y_0


# Create a data frame for the results
results <- data.frame(
  Effect = c("ATE", "PTE", "Unweighted_MM", "Weighted_MM"),
  Value = round(c(ATE, PTE, Unweighted_MM, Weighted_MM), 2)
)

# Print the results using kable()
q10_table <- kable(results, col.names = c("Effect", "Value"))

# print(q10_table)
}