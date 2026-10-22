# This file contains:
# Descriptive statistics(SD, Mean)
# Distribution plots for HEXACO and TWQ dimensions
# Correlation analysis (matrix and heatmap)
# Scatterplots with regression lines

df <- read.csv("processed_data.csv")

# Summary table: mean and SD for HEXACO and TWQ
vars <- c("honesty_humility", "emotionality", "extraversion", 
          "agreeableness", "conscientiousness", "openness",
          "communication", "coordination", "balance",
          "support", "commitment", "cohesion", "twq_overall")

summary_table <- data.frame(
  variable = vars,
  mean = sapply(df[, vars], mean, na.rm = TRUE),
  sd = sapply(df[, vars], sd, na.rm = TRUE)
)

summary_table$type <- c(rep("HEXACO", 6), rep("TWQ", 6), "twq_overall")
summary_table$mean <- round(summary_table$mean, 2)
summary_table$sd <- round(summary_table$sd, 2)

# distributions of HEXACO and TWQ
library(dplyr)
library(tidyr)

df %>%
  pivot_longer(cols = all_of(vars),
               names_to = "variable",
               values_to = "value") %>%
  ggplot(aes(x = value)) +
  geom_histogram(bins = 15, fill = "#28778D", color = "white") +
  facet_wrap(~ variable, scales = "free") +
  theme_minimal() +
  labs(title = "Distributions of HEXACO and TWQ scores")

ggsave("Distributions of HEXACO and TWQ scores.png")

# correlation analysis for every dimension
cor_vars <- c("honesty_humility", "emotionality", "extraversion",
              "agreeableness", "conscientiousness", "openness",
              "communication", "coordination", "balance",
              "support", "commitment", "cohesion")

cor_mat <- cor(df[, cor_vars], use = "pairwise.complete.obs")
round(cor_mat, 2)

# correlation analysis for TWQ_OVERALL
hexaco_vars <- c("honesty_humility", "emotionality", "extraversion",
                 "agreeableness", "conscientiousness", "openness")

cor_hexaco_overall <- cor(df[, hexaco_vars],
                          df$twq_overall,
                          use = "pairwise.complete.obs")

round(cor_hexaco_overall, 2)

cor_hexaco_overall_df <- data.frame(
  HEXACO_trait = rownames(cor_hexaco_overall),
  twq_overall = as.numeric(cor_hexaco_overall),
  row.names = NULL
)

# correlation heatmap
library(reshape2)

melt(cor_mat) %>%
  ggplot(aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() +
  geom_text(aes(label = round(value, 2)), size = 3) +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red",
                       midpoint = 0) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "", y = "", fill = "r")

ggsave("plot_heatmap.png")

# "agreeableness" (HEXACO) with "support" and "communication" (TWQ)
# showed the strongest correlation (r = 0.38 / 0.36).
library(ggplot2)

df_long <- df %>%
  pivot_longer(cols = c(support, communication),
               names_to = "twq_dim",
               values_to = "twq_score")

ggplot(df_long, aes(x = agreeableness, y = twq_score)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = TRUE, color = "blue") +
  facet_wrap(~ twq_dim, scales = "free_y") +
  labs(x = "Agreeableness", y = "TWQ score",
       title = "Agreeableness vs TWQ dimensions") +
  theme_classic()

ggsave("plot_scatter_agreeableness.png")

# Each HEXACO trait vs overall teamwork quality.
# Emotionality shows a negative association with teamwork quality.
df_long_overall <- df %>%
  pivot_longer(
    cols = c(honesty_humility, emotionality, extraversion,
             agreeableness, conscientiousness, openness),
    names_to = "trait",
    values_to = "trait_score"
  )

ggplot(df_long_overall, aes(x = trait_score, y = twq_overall)) +
  geom_point(alpha = 0.6, color = "#28778D") +
  geom_smooth(method = "lm", se = TRUE, color = "#D18A35") +
  facet_wrap(~ trait, scales = "free_x") +
  labs(
    x = "HEXACO trait score",
    y = "Overall Teamwork Quality",
    title = "HEXACO traits vs Overall Teamwork Quality"
  ) +
  theme_classic()

ggsave("plot_scatter_overall.png")