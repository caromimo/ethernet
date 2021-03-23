#########################################################
## Plotting interpacket gap versus Ethernet speed
#########################################################

# setup
rm(list = ls())

# load libraries
library(tidyverse)
library(scales)

# source for the ethernet speeds: 
# https://en.wikipedia.org/wiki/Interpacket_gap

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

# interpacket_gaps <- c(9.6 µs, 0.96 µs, 96 ns, 38.4 ns, 19.2 ns, 9.6 ns, 3.84 ns, 2.4 ns, 1.92 ns, 0.96 ns, 0.48 ns, 0.24 ns)
# ethernet_speed <- c(10 Mbit/s, 100 Mbit/s, 1 Gigabit, 2.5 Gigabit, 5 Gigabit, 10 Gigabit, 25 Gigabit, 40 Gigabit, 50 Gigabit, 100 Gigabit, 200 Gigabit, 400 Gigabit)

# define inputs assuming that 1 Gigabit =  1 Gbit/s
interpacket_gaps_ns <- c(9600, 960, 96, 38.4, 19.2, 9.6, 3.84, 2.4, 1.92, 0.96, 0.48, 0.24)
ethernet_speed_gbps <- c(0.01, 0.1, 1, 2.5, 5, 10, 25, 40, 50, 100, 200, 400)

data <- tibble(interpacket_gaps_ns, ethernet_speed_gbps)
# write_csv(data,"~/Projects/ethernet/data.csv")

# FOR THE PLOT:
# the interpacket gap as the response variable, and
# the ethernet speed as the predictor variable
# FOR THE FUNCTION: 
# the ethernet speed as the predictor variable, and
# the interpacket gap as the response variable

# power function

# define the variables
x <- data$ethernet_speed_gbps
y <- data$interpacket_gaps_ns

# define the model 
model <- lm(log(x) ~ log(y), data = data)

my_model <- lm(log(y)~ log(x))
summary(my_model)

attach(data)
nmodel <- lm(log(ethernet_speed_gbps)~ log(interpacket_gaps_ns))
summary(nmodel)

mmodel <- lm(log(interpacket_gaps_ns)~ log(ethernet_speed_gbps))

mmodel_coefficients <- mmodel$coefficients
new <- data.frame(interpacket_gaps_ns = 1)
predict(nmodel, newdata = new)
new <- data.frame(y = 1)
predict(mmodel, new, se.fit = TRUE)


# model returns:
# Call: lm(formula = log(x) ~ log(y), data = data)
# Coefficients: (Intercept): 4.564
# Coefficients: log(y): -1.000 

# power function so f(x) = k*x^p
# k and p are real numbers 
# k is known as the coefficient

coefficient <- 4.564
p <- -1.000

y <- data$ethernet_speed_gbps
x <- data$interpacket_gaps_ns
xmodel <- lm(log(y) ~ log(x))
summary(xmodel)
gaps <- data.frame(x = c(61.5, 10.5, 3.5, 1))
test <- predict(xmodel, newdata = gaps)

# finding the x value for the y values from the Moritz paper

ram_y_gap <- 61.5
l3_y_gap <- 10.5
l2_y_gap <- 3.5
l1_y_gap <- 1

ram_x_speed <- exp(coefficient)*(ram_y_gap^p)
l3_x_speed <- exp(coefficient)*(l3_y_gap^p)
l2_x_speed <- exp(coefficient)*(l2_y_gap^p)
l1_x_speed <- exp(coefficient)*(l1_y_gap^p)

ram_x_speed <- 1.560976
l3_x_speed <- 9.142857
l2_x_speed <- 27.428571
l1_x_speed <- 96.000000

# reference for the speed of cycles to access the caches and the RAM
# Intel i7-6700 processor using DDR4-2400 RAM
# Mundhenke, M. (2019). Spectre and Cloud: An evaluation of threats in shared computation environments.

# Source: Mundhenke, M. (2019). Spectre and Cloud: An evaluation of threats in shared computation environments. Timing are calculated assuming a clock speed of 4 GHz for an Intel i7-6700 processor using DDR4-2400 RAM.

# plotting the data

data %>%
  ggplot(aes(x = ethernet_speed_gbps, y = interpacket_gaps_ns)) +
  geom_rect(mapping=aes(xmin=ram_x_speed, xmax=1000, ymin=0, ymax=ram_y_gap), fill = "#eeeee4", alpha=0.3) +
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
    title = "Increased network speed reduces packet processing time",
    x = "Ethernet speed (Gbit/s or Gigabit)",
    y = "Time between packets (interpacket gap) (ns)",
    caption = "Sources: Ethernet speeds from IEEE (on Wikipedia) and speeds to access RAM and caches from Mundhenke, M. 2019 (Spectre and Cloud: An evaluation of threats in shared computation environments).\nNote that the axes are on a logarithmic scale."
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
#  annotate("text", x=900, y=140, label= "Intel i7-6700 processor", hjust = 1) +
  annotate("text", x=900, y=50, label= "DDR4-2400 RAM (61.5 ns)", hjust = 1) +
  annotate("text", x=900, y=7, label= "Intel i7-6700 processor L3 cache (shared)\n (42 cycles ~ 10.5 ns)", hjust = 1) +
  annotate("text", x=900, y=2.5, label= "L2 cache (per core)\n (14 cylces ~ 3.5 ns)", hjust = 1) +
  annotate("text", x=900, y=0.7, label= "L1 cache (per core)\n (4 cycles ~ 1 ns)", hjust = 1)