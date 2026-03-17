load("housing.rdata")

####EDA
#loading package and dataset
library(tidyverse)
library(knitr)
library(kableExtra)
housing <- read.csv("housing.csv")

#choose some key variables to explore
eda_vars <- housing %>%
  select(cmedv, crim, rm, lstat, ptratio, tax, dis, nox)
#summary data
summary_table <- eda_vars %>%
  summarise(
    across(
      everything(),
      list(
        N = ~sum(!is.na(.)),
        Mean = ~mean(., na.rm = TRUE),
        SD = ~sd(., na.rm = TRUE),
        Min = ~min(., na.rm = TRUE),
        Median = ~median(., na.rm = TRUE),
        Max = ~max(., na.rm = TRUE)
      )
    )
  ) %>%
  pivot_longer(
    cols = everything(),
    names_to = c("Variable", ".value"),
    names_sep = "_"
  ) %>%
  mutate(
    across(where(is.numeric), ~round(., 2))
  )

summary_table
kable(summary_table, caption = "Summary Statistics for Key Variables") %>%
kable_styling(full_width = FALSE)
write.csv(summary_table, "eda_summary_table.csv", row.names = FALSE)

#correlations
cor_table <- housing %>%
  select(cmedv, crim, rm, lstat, ptratio, tax, dis, nox) %>%
  cor(use = "pairwise.complete.obs")

cor_with_cmedv <- data.frame(
  Variable = rownames(cor_table),
  Correlation_with_cmedv = cor_table[, "cmedv"]
) %>%
  filter(Variable != "cmedv") %>%
  arrange(desc(abs(Correlation_with_cmedv))) %>%
  mutate(Correlation_with_cmedv = round(Correlation_with_cmedv, 3))

cor_with_cmedv

kable(cor_with_cmedv, caption = "Correlation of Key Variables with cmedv") %>%
  kable_styling(full_width = FALSE)

write.csv(cor_with_cmedv, "correlation_with_cmedv.csv", row.names = FALSE)

#set theme
my_theme <- theme_minimal() +
  theme(
    plot.title = element_text(size = 15, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10)
  )

#cmedv plotting

p1 <- ggplot(housing, aes(x = cmedv)) +
  geom_histogram(bins = 20, fill = "skyblue", color = "black", na.rm = TRUE) +
  labs(
    title = "Distribution of Corrected Median Home Value",
    x = "cmedv",
    y = "Frequency"
  ) +
  my_theme

p1

ggsave("eda_hist_cmedv.png", plot = p1, width = 7, height = 5, dpi = 300)

#cmedv vs rm plotttng
p2 <- ggplot(housing, aes(x = rm, y = cmedv)) +
  geom_point(color = "steelblue", alpha = 0.7, na.rm = TRUE) +
  geom_smooth(method = "lm", se = FALSE, color = "red", na.rm = TRUE) +
  labs(
    title = "Rooms vs Corrected Median Home Value",
    x = "Average Number of Rooms (rm)",
    y = "cmedv"
  ) +
  my_theme

p2

ggsave("eda_rm_cmedv.png", plot = p2, width = 7, height = 5, dpi = 300)

#lstat vs cmedv plotting
p3 <- ggplot(housing, aes(x = lstat, y = cmedv)) +
  geom_point(color = "steelblue", alpha = 0.7, na.rm = TRUE) +
  geom_smooth(method = "lm", se = FALSE, color = "red", na.rm = TRUE) +
  labs(
    title = "LSTAT vs Corrected Median Home Value",
    x = "Lower-Status Population (%) (lstat)",
    y = "cmedv"
  ) +
  my_theme

p3

ggsave("eda_lstat_cmedv.png", plot = p3, width = 7, height = 5, dpi = 300)

#crime vs cmedv plotting

p <- ggplot(housing, aes(x = log(crim + 1), y = cmedv)) +
  geom_point(color = "steelblue", alpha = 0.7, na.rm = TRUE) +
  geom_smooth(method = "lm", color = "red", se = FALSE, na.rm = TRUE) +
  labs(
    title = "Log Crime Rate vs Corrected Median Home Value",
    x = "log(Crime Rate + 1)",
    y = "Corrected Median Home Value (cmedv)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10)
  )

#save the plot
ggsave(
  filename = "eda_log_crim_cmedv.png",
  plot = p,
  width = 7,
  height = 5,
  dpi = 300
)