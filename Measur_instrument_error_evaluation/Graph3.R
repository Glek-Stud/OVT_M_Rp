# Load ggplot2 package
library(ggplot2)

# Create a data frame with your data
data <- data.frame(
  frequency = c(50, 100, 160, 200, 300, 380, 420, 480),
  reading = c(11.2, 11.2, 11.1, 11.2, 11.1, 11.2, 11, 11),
  error = c(0.075, 0.075, 0.225, 0.225, 0.225, 0.225, 0.375, 0.375)
)

# Set the threshold for the distinction between normal and side frequency areas
threshold <- 130
threshold2 <- 500

# Create the plot
plot <- ggplot(data, aes(x = frequency, y = reading)) +
  geom_point() +
  geom_errorbar(aes(ymin = reading - error, ymax = reading + error), width = 5) +
  geom_vline(aes(xintercept = threshold), linetype = "dashed", color = "blue") +
  geom_vline(aes(xintercept = threshold2), linetype = "dashed", color = "blue") +
  geom_segment(aes(x = 0, y = 10.5, xend = 0, yend = 11.5), arrow = arrow(length = unit(0.3, "cm"))) +
  geom_segment(aes(x = 0, y = 10.5, xend = 510, yend = 10.5), arrow = arrow(length = unit(0.3, "cm"))) +
  geom_text(aes(x = 510, y = 10.5), label = "V, Гц", hjust = -0.2, vjust = 0, size = 4) +
  geom_text(aes(x = 0, y = 11.5), label = "U, В", hjust = 0., vjust = -0.3, size = 4) +
  labs(x = "", y = "") +
  scale_x_continuous(limits = c(0, 515)) +
  scale_y_continuous(limits = c(10.5, 11.5)) +
  theme_minimal() +
  theme(
    axis.ticks = element_line(color = "black", size = 0.5),
    axis.ticks.length = unit(-0.2, "cm"),
    axis.text.x = element_text(margin = margin(t = 0.2, unit = "cm")),
    axis.text.y = element_text(margin = margin(r = 0.2, unit = "cm"))
  )

# Print the plot
print(plot)
