# source: https://en.wikipedia.org/wiki/Interpacket_gap

# 9.6 µs for 10 Mbit/s Ethernet,
# 0.96 µs for 100 Mbit/s (Fast) Ethernet,
# 96 ns for Gigabit Ethernet,
# 38.4 ns for 2.5 Gigabit Ethernet,
# 19.2 ns for 5 Gigabit Ethernet,
# 9.6 ns for 10 Gigabit Ethernet,
# 3.84 ns for 25 Gigabit Ethernet,
# 2.4 ns for 40 Gigabit Ethernet,
# 1.92 ns for 50 Gigabit Ethernet,
# 0.96 ns for 100 Gigabit Ethernet,
# 0.48 ns for 200 Gigabit Ethernet, and
# 0.24 ns for 400 Gigabit Ethernet.[1]

library(tidyverse)
library(scales)

# interpacket_gaps <- c(9.6 µs, 0.96 µs, 96 ns, 38.4 ns, 19.2 ns, 9.6 ns, 3.84 ns, 2.4 ns, 1.92 ns, 0.96 ns, 0.48 ns, 0.24 ns)
# ethernet_speed <- c(10 Mbit/s, 100 Mbit/s, 1 Gigabit, 2.5 Gigabit, 5 Gigabit, 10 Gigabit, 25 Gigabit, 40 Gigabit, 50 Gigabit, 100 Gigabit, 200 Gigabit, 400 Gigabit)

# define inputs assuming that 1 Gigabit =  1 Gbit/s

interpacket_gaps_ns <- c(9600, 960, 96, 38.4, 19.2, 9.6, 3.84, 2.4, 1.92, 0.96, 0.48, 0.24)
ethernet_speed_gbps <- c(0.01, 0.1, 1, 2.5, 5, 10, 25, 40, 50, 100, 200, 400)

data <- tibble(interpacket_gaps_ns, ethernet_speed_gbps)
# write_csv(data,"~/Projects/ethernet/data.csv")

# power function

# define the variables
x <- data$ethernet_speed_gbps
y <- data$interpacket_gaps_ns

# define the model 
model <- lm(log(x) ~ log(y), data = data)

# power function so f(x) = k*x^p
# k and p are real numbers 
# k is known as the coefficient

coefficient <- coef(model)["(Intercept)"]
p <- coef(model)["log(x)"]

coefficient <- 4.564
p <- -1

# finding the x value for the y values from the Moritz paper

ram_y_gap <- 61.5
l3_y_gap <- 10.5
l2_y_gap <- 3.5
l1_y_gap <- 1

ram_x_speed <- exp(coefficient)*(ram_y_gap^p)
l3_x_speed <- exp(coefficient)*(l3_y_gap^p)
l2_x_speed <- exp(coefficient)*(l2_y_gap^p)
l1_x_speed <- exp(coefficient)*(l1_y_gap^p)

# plotting the data

data %>%
  ggplot(aes(x = ethernet_speed_gbps, y = interpacket_gaps_ns)) +
  geom_rect(mapping=aes(xmin=ram_x_speed, xmax=1000, ymin=0, ymax=ram_y_gap), fill = "#c7e8f5", alpha=0.3) +
  geom_rect(mapping=aes(xmin=l3_x_speed, xmax=1000, ymin=0, ymax=l3_y_gap), fill = "#85c8f5", alpha=0.3) +
  geom_rect(mapping=aes(xmin=l2_x_speed, xmax=1000, ymin=0, ymax=l2_y_gap), fill = "#5ca1fa", alpha=0.3) +
  geom_rect(mapping=aes(xmin=l1_x_speed, xmax=1000, ymin=0, ymax=l1_y_gap), fill = "#007bff", alpha=0.3) +
  geom_point(size = 2) + 
  geom_smooth(color = "black", size = 0.4) +
  scale_x_continuous(
    trans = "log10", 
    labels = comma, 
    limits = c(NA, 1000),
    breaks = c(0, 0.01, 0.1, 1, 10, 100, 1000),
    ) +
  annotation_logticks(base = 10) +
  scale_y_continuous(trans = "log10") +
  labs(
    title = "Interpacket gap versus Ethernet speed",
    x = "Ethernet speed (Gbit/s or Gigabit)",
    y = "Interpacket gap (ns)"
  ) + 
  theme_minimal() + 
  theme(
    plot.title = element_text(size=18, face="bold", margin=margin(20,0,20,0), hjust = 0.5, vjust = 0),
    axis.title.x = element_text(size=16, margin=margin(10,0,10,0)),
    axis.title.y = element_text(size=16, margin=margin(0,10,0,10)),
    axis.text.x = element_text(size=12),
    axis.text.y = element_text(size=12),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 12)
  ) +
  annotate("text", x=900, y=40, label= "RAM (61.5 ns)", hjust = 1) +
  annotate("text", x=900, y=7, label= "L3 cache (10.5 ns)", hjust = 1) +
  annotate("text", x=900, y=2.5, label= "L2 cache (3.5 ns)", hjust = 1) +
  annotate("text", x=900, y=0.7, label= "L1 cache (1 ns)", hjust = 1)