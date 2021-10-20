#' Create a single plot of the 32-run test case.

# Packages
library(ggplot2)
library(tidyverse)

# Find and load the data file
case <- "N32_m1_r3.xlsx"
data <- readxl::read_excel(case)

# Find values of m and N
test.case <- gsub(".xlsx","",case)
N = as.integer(sub("N","",sub("_.*","",test.case)))
m = as.integer(sub("m","",sapply(strsplit(test.case, "_"), "[", 2)))

# Remove full-factorial designs
min_n = log2(N) - 2*m
tidy_data <- dplyr::filter(data, n > min_n)
max_n = tidy_data %>% 
  group_by(method) %>% 
  summarise(n = max(n)) %>% 
  summarise(min(n)) %>% 
  unlist() %>%
  as.numeric()
tidy_data <- dplyr::filter(tidy_data, n < max_n +1)

# Plot time vs. $n$ for each method
p <- ggplot(tidy_data,
            aes(x = n, y = time, linetype = method, shape = method, color = method)) +
  geom_point(size = 2) +
  geom_line() +
  labs(
    x = "Number of two-level factors",
    y = "Time (seconds)",
    title = "Computing times for 32-run designs",
    subtitle = "with 1 four-level factor"
  ) +
  scale_linetype_discrete("Method") +
  scale_shape_discrete("Method") +
  scale_color_discrete("Method") +
  scale_x_continuous(breaks = seq(min_n+1, max_n, 1)) +
  theme_bw(base_size = 15) +
  theme(panel.grid.minor.x = element_blank())

# Save the figure in writing folder to access in Latex
ggsave(paste0("plot.pdf"), p, dpi = 300, height = 5, width = 7)

