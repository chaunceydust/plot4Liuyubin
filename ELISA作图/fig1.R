library(tidyverse)
library(ggsci)
library(patchwork)

data1 <- read_csv("data1_drug_time.csv")

grp.info <- tribble(
  ~sample, ~group,
  #------|-------,
  "9#",   "1.0 mg/kg(EH)",
  "10#",  "1.0 mg/kg(EH)",
  "11#",  "1.0 mg/kg(EH)",
  "12#",  "1.0 mg/kg(EH)",
  "1#",   "3.0 mg/kg(EH)",
  "2#",   "3.0 mg/kg(EH)",
  "3#",   "3.0 mg/kg(EH)",
  "4#",   "3.0 mg/kg(EH)",
  "5#",   "6.0 mg/kg(EH)",
  "6#",   "6.0 mg/kg(EH)",
  "7#",   "6.0 mg/kg(EH)",
  "8#",    "6.0 mg/kg(EH)"
)

data1.long <- data1 %>% 
  pivot_longer(colnames(data1)[2:ncol(data1)],
               names_to = "sample",
               values_to = "concentration") %>% 
  left_join(grp.info)

p1 <- ggplot(
  data = data1.long,
  aes(
    `time(h)`, concentration,
    color = group,
    group = group,
    shape = group
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
    size = 1.6,
    alpha = .7,
    aes(colour = group)
  ) +
  stat_summary(
    fun.data = mean_se,
    geom = "errorbar",
    size = 1.15,
    width = .55,
    aes(color = group)
  ) +
  stat_summary(
    fun = mean,
    geom = "point",
    # size = 3.4,
    size = 4.8,
    aes(fill = group, colour = group,
        shape = group)
  ) +
  scale_x_continuous(
    breaks = c(seq(0, 25, by = 5))
  ) +
  scale_y_log10(
    limits = c(1, 40000),
    breaks = c(1, 10, 100, 1000, 10000, 40000),
    labels = c(1, 10, 100, 1000, 10000, 40000)
  ) +
  scale_color_npg() +
  labs(
    title = "",
    y = "EH Conc.(ng/ml)"
  ) +
  theme_bw(base_size = 15) +
  theme(
    legend.position = c(.78, .8),
    legend.margin = margin(rep(0, 4), unit = "pt"),
    legend.key.width = unit(.4, "pt"),
    legend.key.height = unit(3, "pt"),
    legend.spacing.y = unit(3, "pt"),
    legend.title = element_text(size = 18),
    legend.text = element_text(size = 18),
    axis.title = element_text(size = 24),
    axis.text = element_text(size = 20)
  )

p2 <- ggplot(
  data = filter(data1.long, group == "1.0 mg/kg(EH)"),
       aes(`time(h)`, concentration,
           # alpha = sample,
           group = sample)) +
  geom_line(color = "grey",
            # alpha = .6,
            size = 1) +
  geom_point(
    aes(shape = sample),
    color = "#E85D4A",
    size = 2.4,
    alpha = .75
  ) +
  scale_y_log10(
    limits = c(1, 40000),
    breaks = c(1, 10, 100, 1000, 40000),
    labels = c(1, 10, 100, 1000, 40000)
  ) +
  scale_shape_manual(
    values = c(19, 15, 17, 18)
  ) +
  labs(
    title = "1.0 mg/kg(EH)",
    y = ""
  ) +
  theme_bw(base_size = 15) +
  theme(
    panel.grid = element_blank(),
    plot.title = element_text(hjust = .5),
    plot.margin = unit(rep(0, 4), "lines"),
    legend.position = "top",
    legend.direction = "horizontal",
    legend.margin = margin(rep(0, 4), unit = "pt"),
    legend.key.width = unit(.2, "pt"),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 14)
  )

p3 <- ggplot(
  data = filter(data1.long, group == "3.0 mg/kg(EH)"),
  aes(`time(h)`, concentration,
      # alpha = sample,
      group = sample)) +
  geom_line(color = "grey",
            # alpha = .6,
            size = 1) +
  geom_point(
    aes(shape = sample),
    color = "#54BDD6",
    size = 2.4,
    alpha = .6
  ) +
  scale_y_log10(
    limits = c(1, 40000),
    breaks = c(1, 10, 100, 1000, 40000),
    labels = c(1, 10, 100, 1000, 40000)
  ) +
  scale_shape_manual(
    values = c(19, 15, 17, 18)
  ) +
  labs(
    title = "3.0 mg/kg(EH)",
    y = ""
  ) +
  theme_bw(base_size = 15) +
  theme(
    panel.grid = element_blank(),
    plot.title = element_text(hjust = .5),
    plot.margin = unit(rep(0, 4), "lines"),
    legend.position = "top",
    legend.direction = "horizontal",
    legend.margin = margin(rep(0, 4), unit = "pt"),
    legend.key.width = unit(.2, "pt"),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 14)
  )




p4 <- ggplot(
  data = filter(data1.long, group == "6.0 mg/kg(EH)"),
  aes(`time(h)`, concentration,
      # alpha = sample,
      group = sample)) +
  geom_line(color = "grey",
            # alpha = .6,
            size = 1) +
  geom_point(
    aes(shape = sample),
    color = "#00A087",
    size = 2.4,
    alpha = .6
  ) +
  scale_y_log10(
    limits = c(1, 40000),
    breaks = c(1, 10, 100, 1000, 40000),
    labels = c(1, 10, 100, 1000, 40000)
  ) +
  scale_shape_manual(
    values = c(19, 15, 17, 18)
  ) +
  labs(
    title = "6.0 mg/kg(EH)",
    y = ""
  ) +
  theme_bw(base_size = 15) +
  theme(
    panel.grid = element_blank(),
    plot.title = element_text(hjust = .5),
    plot.margin = unit(rep(0, 4), "lines"),
    legend.position = "top",
    legend.direction = "horizontal",
    legend.margin = margin(rep(0, 4), unit = "pt"),
    legend.key.width = unit(.2, "pt"),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 14)
  )

mydesign <- "
AAB
AAC
AAD"

p1 + p2 + p3 + p4 +
  plot_layout(design = mydesign) +
  plot_annotation(
    title = "",
    tag_levels = "A"
  ) &
  theme(plot.tag = element_text(size = 24))

ggsave("fig1.pdf",
       width = 12, height = 8)
ggsave("fig1.tiff",
       width = 12, height = 8)
ggsave("fig1.png",
       width = 12, height = 8)
