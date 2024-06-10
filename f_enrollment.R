progname <- "f_enrollment"
################################################################################
# Initial set up
# Purpose: Create cumulative enrollment overtime, targeted and actual enrollment
################################################################################

################################################################################
# Generate a dummy dataset
################################################################################

# Load necessary library
library(dplyr)
library(ggplot2)

# Initialize parameters
start_month <- as.Date("2022-01-01")
end_enrollment <- 500
initial_targeted_enrollment <- 10
initial_actual_enrollment <- 7
monthly_growth_rate <- 0.05
periods <- 24

# Generate date range
dates <- seq(start_month, by = "month", length.out = periods + 1)

# Generate targeted enrollment
targeted_enrollment <- numeric(periods + 1)
targeted_enrollment[2] <- initial_targeted_enrollment
for (i in 3:(periods + 1)) {
  targeted_enrollment[i] <- targeted_enrollment[i-1] * (1 + monthly_growth_rate)
}

# Adjust to meet the total targeted enrollment
targeted_enrollment <- pmin(targeted_enrollment, end_enrollment)
cumulative_targeted_enrollment <- cumsum(targeted_enrollment)

# Generate actual enrollment slightly smaller than targeted
actual_enrollment <- numeric(periods + 1)
actual_enrollment[1] <- 0  # Starting with 0
actual_enrollment[2] <- initial_actual_enrollment

set.seed(123)  # for reproducibility
for (i in 3:(periods + 1)) {
  actual_enrollment[i] <- targeted_enrollment[i] * (1 - runif(1, 0.01, 0.2))
}

cumulative_actual_enrollment <- cumsum(actual_enrollment)

# Create DataFrame
df <- data.frame(
  Month_Year = dates,
  Targeted_Enrollment = round(cumulative_targeted_enrollment),
  Actual_Enrollment = round(cumulative_actual_enrollment)
)

# Subset the data to include every 3 months for label display
df_subset <- df[seq(1, nrow(df), by = 3), ]

################################################################################
# Create cumulative enrollment figure
################################################################################
# Plot cumulative enrollment figure
p <- ggplot(df, aes(x = Month_Year)) +
  geom_line(aes(y = Targeted_Enrollment, color = "Targeted enrollment"), 
            size = 1, linetype = "dotted") +
  geom_line(aes(y = Actual_Enrollment, color = "Actual enrollment"), 
            size = 1, linetype = "solid") +
  labs(#title = "Cumulative Enrollment Over Time",
       x = "Year-Month",
       y = "Cumulative enrollment",
       color = "Legend") +
  scale_color_manual(values = c("Targeted enrollment" = "darkgrey", 
                                "Actual enrollment" = "black")) +
  scale_x_date(date_labels = "%Y-%m", date_breaks = "3 months", expand = c(0, 0)) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 0, hjust = 1, size = 12),
    axis.text.y = element_text(size = 12),
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    legend.position = c(0.05, 0.95),
    legend.justification = c(0, 1),
    legend.text = element_text(size = 12),
    plot.margin = margin(10, 10, 1, 10)  # Add margin at the bottom for footnotes
  )+
  annotate("text", x = df_subset$Month_Year[2], y = -30, label = "Actual #",
           angle = 0, hjust = 1.6, vjust = 1, size = 4) +
  geom_text(data = df_subset, aes(label = Actual_Enrollment, y = -50), 
            angle = 0, hjust = 1, vjust = 1, size = 4) +
  annotate("text", x = max(df$Month_Year)-90, y = max(df$Targeted_Enrollment), 
           label = paste("Targeted #:", max(df$Targeted_Enrollment), sep = ""), size = 4, hjust = 0)


# Add footnotes
footnotes <- grid::grobTree(
  grid::textGrob("Randomization cutoff: YYYY-MM-DD; CRF data cutoff: YYYY-MM-DD", 
                 x = 0.01, y = 0.82, hjust = 0, gp = gpar(fontsize = 10)),
  grid::textGrob("PROGRAM/FILE FOOTPATH", 
                 x = 0.01, y = 0.70, hjust = 0, gp = gpar(fontsize = 10))
)

# Combine plot and footnotes
# gridExtra::grid.arrange(p, footnotes, ncol = 1, heights = c(4, 1))
final_plot <- arrangeGrob(p, footnotes, ncol = 1, heights = c(4, 1))

# Output png file
# Save the plot as a PNG file
ggsave(paste0(progname,".png"), final_plot, width = 10, height = 7, dpi = 500)


