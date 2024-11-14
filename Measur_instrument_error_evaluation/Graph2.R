library(ggplot2)

# Create a data frame for the first line
first_line <- data.frame(
  U_B = c(3, 4, 5, 6, 8, 10, 12, 14, 15),
  b_percent = c(-0.025, -0.019, -0.015, -0.013, -0.009, -0.008, -0.006, -0.0053, -0.005),
  line = "first_line"
)

# Create a data frame for the second line
second_line <- data.frame(
  U_B = c(3, 4, 5, 6, 8, 10, 12, 14, 15),
  b_percent = c(0.025, 0.019, 0.015, 0.013, 0.009, 0.008, 0.006, 0.0053, 0.005),
  line = "second_line"
)

# Combine the data frames
data <- rbind(first_line, second_line)

# Create the graph
plot <- ggplot(data, aes(x = U_B, y = b_percent, group = line)) +
  geom_line(aes(color = line), size = 1) +
  geom_point(aes(color = line), size = 3) +
  scale_color_manual(values = c("first_line" = "black", "second_line" = "black")) +
  labs(x = "", y = "") +
  theme_bw() +
  theme(legend.position = "none",
        axis.ticks = element_line(color = "black", size = 0.5),
        axis.ticks.length = unit(-0.25, "cm"),
        axis.text.x = element_text(angle = 0, vjust = 0.5, hjust = 1, margin = margin(t = 0.2, unit = "cm")),
        axis.text.y = element_text(angle = 0, vjust = 0.5, hjust = 1, margin = margin(r = 0.2, unit = "cm")),
        panel.border = element_blank()) + 
  geom_text(aes(label = paste0("(", U_B, "; ", b_percent * 100, ")")), vjust = -2, hjust = 0.4, size = 3, color = "blue") +
  scale_x_continuous(limits = c(2, 15.1)) +
  scale_y_continuous(limits = c(-0.0270, 0.0270), labels = function(x) x * 100) +
  geom_segment(aes(x = 2, y = -0.0270, xend = 2, yend = 0.0270), arrow = arrow(length = unit(0.3, "cm"))) +
  geom_segment(aes(x = 2, y = -0.0270, xend = 15.1, yend = -0.0270), arrow = arrow(length = unit(0.3, "cm"))) +
  geom_text(aes(x = 15.1, y = -0.0270), label = "U, B", hjust = -0.1, vjust = 0, size = 4) +
  geom_text(aes(x = 2, y = 0.0270), label = "b%", hjust = 0, vjust = -0.3, size = 4)

# Print the plot
print(plot)


