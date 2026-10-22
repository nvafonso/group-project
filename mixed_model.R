# MIXED-EFFECTS MODELS for HEXACO, TWQ and TWQ_overall
library(lme4)

df <- read.csv("processed_data.csv")

# Focused Mixed-Effects Model
# Reason for choosing these variables: 
# Agreeableness and Support showed the strongest correlation (r = 0.38).

model_focused <- lmer(
  support ~ agreeableness + (1 | Team),
  data = df
)

summary(model_focused)
confint(model_focused)

# This function allows the app to predict support scores based 
# on a user-adjusted agreeableness slider.
predict_support <- function(agreeableness_value) {
  new_data <- data.frame(
    agreeableness = agreeableness_value,
    Team = NA 
  )
  
  pred_value <- predict(
    model_focused, 
    newdata = new_data, 
    re.form = NA
  )
  
  return(pred_value)
}
predict_support(4)

# Predicting overall teamwork quality (twq_overall) using all six HEXACO traits.
# (1 | Team) accounts for the nested structure of participants within teams.
model_multi <- lmer(
  twq_overall ~ honesty_humility + emotionality + extraversion + 
    agreeableness + conscientiousness + openness + 
    (1 | Team),
  data = df
)

# Result: Only agreeableness significantly predicts twq_overall 
# (95% CI [0.06, 0.59]). Emotionality shows a non-significant negative trend.
summary(model_multi)
confint(model_multi)

# Multiple traits prediction for the App's 6 sliders
predict_overall_multi <- function(hon, emo, ext, agr, con, ope) {
  new_data <- data.frame(
    honesty_humility = hon,
    emotionality = emo,
    extraversion = ext,
    agreeableness = agr,
    conscientiousness = con,
    openness = ope,
    Team = NA # Team is NA for population-level prediction
  )
  
  predict(
    model_multi, 
    newdata = new_data, 
    re.form = NA
  )
}

predict_overall_multi(1.2, 3, 2.5, 4.1, 2, 3.3)

library(ggplot2)
library(dplyr)
# install.packages("broom.mixed")
library(broom.mixed)
# Forest plot: Extract coefficients and 95% CIs from the multivariate model.
# Dots = point estimates, Error bars = 95% CIs.
# Dashed line = zero effect. Only Agreeableness's CI does not cross zero,
# indicating it is the only significant positive predictor.

tidy(model_multi, conf.int = TRUE) %>%
  filter(term != "(Intercept)") %>%
  ggplot(aes(x = estimate, y = reorder(term, estimate))) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.2, color = "#28778D", linewidth = 1) +
  geom_point(color = "#D18A35", size = 3) +
  labs(
    x = "Regression Coefficient (b) with 95% CI",
    y = "HEXACO Trait",
    title = "Predicting Overall Teamwork Quality",
    subtitle = "Only Agreeableness is significantly positive"
  ) +
  theme_classic(base_size = 14)

ggsave("forest_plot.png")

saveRDS(model_focused, "model_focused.rds")
saveRDS(model_multi, "model_multi.rds")

round(fixef(model_focused), 3)
# Intercept = 1.967
# Support = 1.967 + 0.550 * Agreeableness

round(fixef(model_multi), 3)
# Intercept = 2.090
# Overall_TWQ = 2.090 + (-0.006 * H) + (-0.119 * E) + 
# (0.130 * X) + (0.321 * A) + (0.087 * C) + (0.030 * O)

# MODEL DIAGNOSTICS
library(performance)
library(see)

# Check for Multicollinearity (VIF) in the multiple regression
# (We want VIF < 5 to show predictors are not too highly correlated)
cat("\n--- Check Multicollinearity (VIF) ---\n")
print(check_collinearity(model_multi))
# All VIF values are between 1.17 and 1.48, well below the threshold 
# of 5. No multicollinearity issues.

# Check for Singular Fit (Convergence warnings)
# Checks if the random effect variance is too close to zero
check_singularity(model_focused)
check_singularity(model_multi)
# Both models passed the singularity checks (FALSE for singular fit),
# so the random effects are estimated well.

# Check Normality and Homoscedasticity of residuals for the focused model
check_normality(model_focused)
check_heteroscedasticity(model_focused)
# The residuals are normally distributed (Shapiro-Wilk p = .607)
# and the error variance is homoscedastic (p = .830).

# Generate and save Diagnostic Plots (Q-Q plot, Residuals vs Fitted, etc.)
png("diagnostic_plots.png", width = 800, height = 800)
par(mfrow = c(2, 2))
plot(model_focused)
dev.off()

