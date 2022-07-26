library(tidyverse)
library(ggsci)
library(patchwork)

data1 <- read_csv("data2_drug_time.csv")

data1.long <- data1 %>% 
  pivot_longer(colnames(data1)[3:ncol(data1)],
               names_to = "sample",
               values_to = "concentration")

p1 <- ggplot(
  data = data1.long,
  aes(
    `time(h)`, concentration,
    color = day
  )
) +
  # geom_jitter(
  #   size = 3,
  #   color = "grey70",
  #   alpha = .5
  # ) +
  # geom_line(
  #   aes(group = sample),
  #   color = "grey70",
  #   alpha = .5
  # ) +
  stat_summary(
    fun = mean,
    geom = "line",
    # size = 3.4,
    color = "grey70",
    size = 1.6,
    alpha = .7,
  ) +
  stat_summary(
    fun.data = mean_se,
    geom = "errorbar",
    size = 1.35,
    width = 2.55,
  ) +
  stat_summary(
    fun = mean,
    geom = "point",
    # size = 3.4,
    size = 3.8,
  ) +
  scale_x_continuous(
    breaks = c(seq(0, 180, by = 24))
  ) +
  scale_y_continuous(
    limits = c(0, 17000),
    breaks = c(0, 5000, 10000, 15000),
    labels = c(0, 5000, 10000, 15000)
  ) +
  scale_color_gsea() +
  # scale_y_log10(
  #   limits = c(1, 17000),
  #   breaks = c(1, 10, 100, 1000, 10000),
  #   labels = c(1, 10, 100, 1000, 10000)
  # ) +
  labs(
    title = "",
    y = "EH Conc.(ng/ml)"
  ) +
  theme_bw(base_size = 15) +
  theme(
    panel.grid = element_blank(),
    legend.position = c(.95, .6),
    legend.margin = margin(rep(0, 4), unit = "pt"),
    legend.title = element_text(size = 18),
    legend.text = element_text(size = 18),
    axis.title = element_text(size = 24),
    axis.text = element_text(size = 20)
  )

data2.long <- filter(data1.long, day %in% c(1, 7))
data2.long$`time(h)`[which(data2.long$day == 7)] <- data2.long$`time(h)`[which(data2.long$day == 7)] - 145
data2.long$day <- as.factor(data2.long$day)

p2 <- ggplot(
  data = data2.long,
  aes(
    `time(h)`, concentration,
    color = day,
    shape = day
  )
) +
  stat_summary(
    fun = mean,
    geom = "line",
    # size = 3.4,
    color = "grey70",
    size = 1.6,
    alpha = .7,
  ) +
  stat_summary(
    fun.data = mean_se,
    geom = "errorbar",
    size = .55,
    width = .75,
  ) +
  stat_summary(
    fun = mean,
    geom = "point",
    # size = 3.4,
    size = 3.8,
  ) +
  scale_x_continuous(
    breaks = c(seq(0, 24, by = 4))
  ) +
  scale_y_log10(
    limits = c(1, 12000),
    breaks = c(1, 10, 100, 1000, 10000),
    labels = c(1, 10, 100, 1000, 10000)
  ) +
  scale_color_manual(
    values = c('1' = "#4500AC", '7' = "#D60C00")
  ) +
  labs(
    title = "",
    y = "EH Conc.(ng/ml)"
  ) +
  theme_bw(base_size = 15) +
  theme(
    panel.grid = element_blank(),
    legend.position = c(.95, .8),
    legend.margin = margin(rep(0, 4), unit = "pt"),
    legend.title = element_text(size = 18),
    legend.text = element_text(size = 18),
    axis.title = element_text(size = 24),
    axis.text = element_text(size = 20)
  )

p2 / p1 +
  plot_annotation(
    tag_levels = "A"
  ) &
  theme(plot.tag = element_text(size = 24))

ggsave("fig2.pdf",
       width = 8, height = 8)
ggsave("fig2.png",
       width = 8, height = 8)
ggsave("fig2.tiff",
       width = 8, height = 8)
