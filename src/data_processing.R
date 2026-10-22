df <- read.csv2("https://osf.io/download/k4syv/")

library(tidyr)

# identify and reverse-score the reverse items of HEXACO
reverse_items <- c(
  # Honesty-Humility
  "F12r", "F24r", "F30r", "F42r", "F48r", "F60r",
  # Emotionality
  "F35r", "F41r", "F53r", "F59r",
  # Extraversion
  "F10r", "F28r", "F46r", "F52r",
  # Agreeableness
  "F09r", "F15r", "F21r", "F57r",
  # Conscientiousness
  "F14r", "F20r", "F26r", "F32r", "F44r", "F56r",
  # Openness
  "F01r", "F31r", "F49r"
)

reverse_items <- intersect(reverse_items, names(df))

for (item in reverse_items){
  df[[item]] <- 6 - df[[item]]
}

# calculate scores for each HEXACO dimension
H_items <- c("F06", "F30r", "F54", "F12r", "F36", "F60r",
             "F18", "F42r", "F24r", "F48r")

E_items <- c("F05", "F29", "F53r", "F11", "F35r",
             "F17", "F41r", "F23", "F47", "F59r")

X_items <- c("F04", "F28r", "F52r", "F10r", "F34", "F58",
             "F16", "F40", "F22", "F46r")

A_items <- c("F03", "F27", "F09r", "F33", "F51",
             "F15r", "F39", "F57r", "F21r", "F45")

C_items <- c("F02", "F26r", "F08", "F32r", "F14r", "F38", "F50",
             "F20r", "F44r", "F56r")

O_items <- c("F01r", "F25", "F07", "F31r", "F13", "F37",
             "F49r", "F19", "F43", "F55")

df$honesty_humility <- rowMeans(df[, H_items], na.rm = TRUE)
df$emotionality <- rowMeans(df[, E_items], na.rm = TRUE)
df$extraversion <- rowMeans(df[, X_items], na.rm = TRUE)
df$agreeableness <- rowMeans(df[, A_items], na.rm = TRUE)
df$conscientiousness <- rowMeans(df[, C_items], na.rm = TRUE)
df$openness <- rowMeans(df[, O_items], na.rm = TRUE)

# TWQ has six dimensions: Communication (CMNC), Coordination (CRDN),
# Balance (BLNC), Support (SPRT), Commitment (COMT), Cohesion (COHS)

# identify and reverse-score the reverse items of TWQ
twq_reverse <- c("CMNC04r", "CMNC06r", "CMNC07r", "CRDN4r", "BLNC3r",
                 "COMT4r", "COHS02r", "COHS06r")

for (item in intersect(twq_reverse, names(df))) {
  df[[item]] <- 6 - df[[item]]
}

CMNC_items <- c("CMNC01", "CMNC02", "CMNC03", "CMNC04r", "CMNC05", 
                "CMNC06r", "CMNC07r", "CMNC08", "CMNC09", "CMNC10")
CRDN_items <- c("CRDN1", "CRDN2", "CRDN3", "CRDN4r")
BLNC_items <- c("BLNC1", "BLNC2", "BLNC3r")
SPRT_items <- c("SPRT1", "SPRT2", "SPRT3", "SPRT4", "SPRT5", "SPRT6")
COMT_items <- c("COMT1", "COMT2", "COMT3", "COMT4r")
COHS_items <- c("COHS01", "COHS02r", "COHS03", "COHS04", "COHS05", 
                "COHS06r", "COHS07", "COHS08", "COHS09", "COHS10")

# calculate scores for each TWQ dimension
df$communication <- rowMeans(df[, CMNC_items], na.rm = TRUE)
df$coordination <- rowMeans(df[, CRDN_items], na.rm = TRUE)
df$balance <- rowMeans(df[, BLNC_items], na.rm = TRUE)
df$support <- rowMeans(df[, SPRT_items], na.rm = TRUE)
df$commitment <- rowMeans(df[, COMT_items], na.rm = TRUE)
df$cohesion <- rowMeans(df[, COHS_items], na.rm = TRUE)

df$balance <- round(df$balance, 2)
df$support <- round(df$support, 2)

# The six TWQ dimensions were strongly correlated (r = .46–.86),
# and showed high internal consistency (Cronbach's alpha = .92).
# Therefore, an overall TWQ score can be computed as their average.
cor(df[, c("communication", "coordination", "balance",
           "support", "commitment", "cohesion")],
    use = "pairwise.complete.obs")

psych::alpha(df[, c("communication", "coordination", "balance",
                    "support", "commitment", "cohesion")])

df$twq_overall <- rowMeans(
  df[, c("communication", "coordination", "balance",
         "support", "commitment", "cohesion")],
  na.rm = TRUE
)
df$twq_overall <- round(df$twq_overall, 2)

# Save the cleaned dataset
keep_vars <- c(
  "id", "Team", "TeamDuration", "TeamHours",
  "Gender", "Age", "Degree.Program",
  "honesty_humility", "emotionality", "extraversion",
  "agreeableness", "conscientiousness", "openness",
  "communication", "coordination", "balance",
  "support", "commitment", "cohesion",
  "twq_overall"
)

df_clean <- df[, keep_vars]

write.csv(df_clean, "processed_data.csv", row.names = FALSE)
write.csv(df, "processed_data_raw.csv", row.names = FALSE)
