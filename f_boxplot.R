progname <- "f_boxplot"
################################################################################
# Initial set up
# Purpose: Create a boxplot to show analysis value and change from baseline over time
################################################################################

################################################################################
# Generate a dummy dataset
################################################################################
# Load necessary libraries
library(dplyr)
library(tidyr)

# Set seed for reproducibility
set.seed(123)

# Number of subjects in each group
n_A <- 249
n_B <- 250

# Generate baseline ALT levels for each group
baseline_A <- rnorm(n_A, mean = 35, sd = 10)
baseline_B <- rnorm(n_B, mean = 36, sd = 10)

# Generate follow-up ALT levels for each group
generate_followup <- function(baseline, group) {
  if (group == "A") {
    baseline + rnorm(1, mean = 5, sd = 5)
  } else {
    baseline + rnorm(1, mean = 0, sd = 5)
  }
}

# Define time points in weeks
time_points <- c("4", "8", "12", "16", "20", "24", "36")

# Decreasing sample sizes over time with some randomness
sample_sizes_A <- c(249, 230, 200, 180, 160, 140, 120)
sample_sizes_B <- c(250, 225, 210, 190, 170, 150, 130)

# Create data frames for each group and time point with decreasing sample sizes
data_A <- data.frame(Subject = integer(), Group = character(), BASE = numeric(), Time = integer(), AVAL = numeric())
data_B <- data.frame(Subject = integer(), Group = character(), BASE = numeric(), Time = integer(), AVAL = numeric())

for (i in seq_along(time_points)) {
  time <- time_points[i]
  size_A <- sample_sizes_A[i]
  size_B <- sample_sizes_B[i]
  
  temp_A <- data.frame(
    Subject = 1:size_A,
    Group = "A",
    BASE = baseline_A[1:size_A],
    Time = as.numeric(time),
    AVAL = sapply(baseline_A[1:size_A], generate_followup, group = "A")
  )
  
  temp_B <- data.frame(
    Subject = 1:size_B,
    Group = "B",
    BASE = baseline_B[1:size_B],
    Time = as.numeric(time),
    AVAL = sapply(baseline_B[1:size_B], generate_followup, group = "B")
  )
  
  data_A <- bind_rows(data_A, temp_A)
  data_B <- bind_rows(data_B, temp_B)
}

# Combine data for both groups
data <- bind_rows(data_A, data_B)

# Add baseline records with Time = 0
baseline_long_A <- data.frame(
  Subject = 1:n_A,
  Group = "A",
  BASE = baseline_A,
  Time = 0,
  AVAL = baseline_A
)

baseline_long_B <- data.frame(
  Subject = 1:n_B,
  Group = "B",
  BASE = baseline_B,
  Time = 0,
  AVAL = baseline_B
)

baseline_long <- bind_rows(baseline_long_A, baseline_long_B)

# Combine baseline and follow-up data
data_long <- bind_rows(baseline_long, data) %>%
  arrange(Subject, Time) %>%
  mutate(CHG = ifelse(Time == 0, NA, AVAL - BASE))




################################################################################
# Create boxplots
################################################################################
# Load necessary libraries
library(ggplot2)
library(gridExtra)

# Define colors for two groups
colors <- c("A" = "navy", "B" = "orange")

# Create a summary of sample sizes at each time point for each group
sample_sizes_AVAL <- data_long %>%
  group_by(Time, Group) %>%
  summarise(n = n(), .groups = 'drop') %>%
  pivot_wider(names_from = Group, values_from = n, names_prefix = "n_") %>%
  replace(is.na(.), 0) %>%
  mutate(label = if_else(Time==0, paste0("BL", "\n\n Group A: ", n_A, "\n Group B: ", n_B),
                         paste0("W", Time, "\n\n ", n_A, "\n ", n_B)))

sample_sizes_CHG <- data_long %>%
  group_by(Time, Group) %>%
  summarise(n = n(), .groups = 'drop') %>%
  pivot_wider(names_from = Group, values_from = n, names_prefix = "n_") %>%
  replace(is.na(.), 0) %>%
  mutate(label = if_else(Time==0, paste0("BL", "\n\n Group A:   ", "\n Group B:   "),
                         paste0("W",Time, "\n\n ", n_A, "\n ", n_B)))

# Generate the boxplot for ALT values (AVAL) over time
p1 <- ggplot(data_long, aes(x = factor(Time), y = AVAL, fill = Group)) +
  geom_boxplot() +
  scale_fill_manual(values = colors) +
  labs(#title = "ALT Values Over Time",
       x = "",
       y = "ALT value (U/L)") +
  scale_x_discrete(labels = sample_sizes_AVAL$label) +
  theme_minimal() + 
  theme(legend.position = "top")

# Generate the boxplot for changes from baseline (CHG) over time
p2 <- ggplot(data_long, aes(x = factor(Time), y = CHG, fill = Group)) +
  geom_boxplot() +
  scale_fill_manual(values = colors) +
  labs(x = "Weeks from baseline",
       y = "ALT change from baseline (U/L)") +
  scale_x_discrete(labels = sample_sizes_CHG$label) +
  theme_minimal() +
  theme(legend.position = "none")  # Remove legend from p2

# Arrange the two plots in a vertical layout
combined_plot <- grid.arrange(p1, p2, ncol = 1)

footnotes <- ggplot() + 
  annotate("text", x = 0, y = 0, label = "Randomization cutoff: YYYY-MM-DD; CRF data cutoff: YYYY-MM-DD \nPROGRAM/FILE FOOTPATH", 
           hjust = 0, vjust = 1, size = 2) +
  theme_void() +
  theme(plot.margin = unit(c(1, 1, 0, 1), "cm"))

# Arrange the plots with the footnotes
final_plot <- grid.arrange(combined_plot, footnotes, heights = c(8, 1))

ggsave(paste0(progname,"_ALT.png"), final_plot, width = 6, height = 8, dpi = 500)
