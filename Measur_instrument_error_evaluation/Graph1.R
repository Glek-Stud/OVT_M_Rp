# Load required packages
library(ggplot2)
library(grid)

# Create a data frame with the points and labels
points_data <- data.frame(
  U_B = c(3, 4, 5, 6, 8, 10, 12, 14, 15, 0, 0),
  V = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0.075, -0.075),
  label = c("3", "4", "5", "6", "8", "10", "12", "14", "15", "0.075", "-0.075")
)

# Create a data frame with the lines data
lines_data <- data.frame(
  U_B = c(0, 0),
  V = c(0.075, -0.075),
  color = c("green", "blue")
)

# Create the ggplot object with the points and labels
p <- ggplot() +
  geom_point(data = points_data, aes(x = U_B, y = V), size = 3) +
  geom_text(data = points_data, aes(x = U_B, y = V, label = label), hjust = -0.1, vjust = -0.6, size = 3.5) +
  geom_segment(data = lines_data, aes(x = U_B, y = V, xend = 18, yend = V, color = color), linetype = "solid") +
  scale_color_manual(values = lines_data$color) +
  labs(x = "U, B", y = "") +
  theme_bw() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )

# Add arrow tips to the axes
p <- p + coord_cartesian(clip = "off", xlim = c(-2, max(points_data$U_B) + 2), ylim = c(-0.15, 0.15)) +
  annotate("segment", x = 0, y = 0, xend = max(points_data$U_B) + 2, yend = 0, arrow = arrow(type = "closed", length = unit(0.15, "inches"))) +
  annotate("segment", x = 0, y = -0.15, xend = 0, yend = 0.15, arrow = arrow(type = "closed", length = unit(0.15, "inches"))) +
  geom_text(aes(x = 17, y = 0), label = "U, B", hjust = -0.1, vjust = -0.2, size = 4) 

# Print the plot
print(p)
