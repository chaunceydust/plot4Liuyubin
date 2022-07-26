library(tidyverse)
library(ggsci)
library(patchwork)
library(ggbreak)

data1 <- read_csv("data1_drug_time.csv")

# grp.info <- tribble(
#   ~sample, ~group,
#   #------|-------,
#   "9#",   "1.0 mg/kg(EH)",
#   "10#",  "1.0 mg/kg(EH)",
#   "11#",  "1.0 mg/kg(EH)",
#   "12#",  "1.0 mg/kg(EH)",
#   "1#",   "3.0 mg/kg(EH)",
#   "2#",   "3.0 mg/kg(EH)",
#   "3#",   "3.0 mg/kg(EH)",
#   "4#",   "3.0 mg/kg(EH)",
#   "5#",   "6.0 mg/kg(EH)",
#   "6#",   "6.0 mg/kg(EH)",
#   "7#",   "6.0 mg/kg(EH)",
#   "8#",    "6.0 mg/kg(EH)"
# )

data1.long <- data1 %>% 
  pivot_longer(colnames(data1)[2:ncol(data1)],
               names_to = "sample",
               values_to = "concentration") 
data1.long$group <- NA
data1.long$group[grep("^1",data1.long$sample)] <- "low dose group"
data1.long$group[grep("^2",data1.long$sample)] <- "middle dose group"
data1.long$group[grep("^3",data1.long$sample)] <- "high dose group"

data1.long$group <- factor(data1.long$group, levels = c("high dose group", "middle dose group", "low dose group"))

data1.long <- filter(data1.long, `time(h)` != 0) %>% 
  mutate(
    `time(h)` = if_else(
      `time(h)` == 1,
      3,
      if_else(
        `time(h)` == 2,
        6,
        `time(h)`
      )
    )
  )

# p1 -----------------------------------------------------------------------

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
    size = 1.2,
    alpha = .5,
    aes(colour = group)
  ) +
  stat_summary(
    fun.data = mean_se,
    geom = "errorbar",
    size = .95,   # 1.35
    width = .75,
    aes(color = group)
  ) +
  stat_summary(
    fun = mean,
    geom = "point",
    # size = 3.4,
    size = 3.4,
    aes(fill = group, colour = group,
        shape = group)
  ) +
  scale_x_continuous(
    # breaks = c(seq(0, 25, by = 5))
    breaks = c(0.0167, 3, 6, 24, 72, 74, 78),
    labels = c(0.0167, 1, 2, 24, 72, 74, 78),
    position = "bottom"
  ) +
  scale_y_continuous(
    limits = c(1, 10000),
    breaks = seq(0, 10000, length.out = 5)
  ) +
  scale_x_break(
    c(25, 71),
    space = .1,
    scales = .4,
  ) +
  # scale_y_log10(
  #   limits = c(1, 40000),
  #   breaks = c(1, 10, 100, 1000, 10000, 40000),
  #   labels = c(1, 10, 100, 1000, 10000, 40000)
  # ) +
  scale_color_npg() +
  scale_shape_manual(
    values = c(19, 15, 17)
  ) +
  labs(
    title = "",
    y = "Conc.(ng/ml)"
  ) +
  theme_bw(base_size = 15) +
  theme(
    legend.position = "none",
    panel.grid.minor = element_blank(),
    plot.margin = unit(c(0, 0, 1, 1), "cm")
    # legend.position = c(.25, .9),
    # legend.margin = margin(rep(0, 4), unit = "pt"),
    # legend.key.width = unit(.4, "pt"),
    # legend.key.height = unit(3, "pt"),
    # legend.spacing.y = unit(3, "pt"),
    # legend.title = element_text(size = 15),
    # legend.text = element_text(size = 15),
    # axis.title = element_text(size = 24),
    # axis.text = element_text(size = 20)
  )
p1
ggsave("P1.png",
       width = 6, height = 6)
# p2 ----------------------------------------------------------------------

p2 <- ggplot(
  data = filter(data1.long, group == "high dose group"),
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
    alpha = .99
  ) +
  scale_x_continuous(
    # breaks = c(seq(0, 25, by = 5))
    breaks = c(0.0167, 6, 24, 72, 74, 78),
    labels = c(0.0167, 2, 24, 72, 74, 78),
    position = "bottom"
  ) +
  scale_y_continuous(
    limits = c(1, 10000),
    breaks = seq(0, 10000, length.out = 3)
  ) +
  scale_x_break(
    c(25, 71),
    space = .1,
    scales = .4,
  ) +
  # scale_y_log10(
  #   limits = c(1, 40000),
  #   breaks = c(1, 10, 100, 1000, 40000),
  #   labels = c(1, 10, 100, 1000, 40000)
  # ) +
  scale_shape_manual(
    values = rep(19, 8)
  ) +
  labs(
    title = "high dose group",
    subtitle = "(0.4mg/kg+0.45mg/kg/h EH)",
    y = "",
    x = ""
  ) +
  theme_bw(base_size = 15) +
  theme(
    panel.grid = element_blank(),
    plot.title = element_text(hjust = .5,
                              face = "bold",
                              colour = "#E85D4A"),
    plot.subtitle = element_text(hjust = .5,
                                 colour = "#E85D4A"),
    # plot.margin = unit(rep(0, 4), "lines"),
    legend.position = "none",  #  "top"
    legend.direction = "horizontal",
    # legend.margin = margin(rep(0, 4), unit = "pt"),
    # legend.key.width = unit(.2, "pt"),
    # legend.title = element_text(size = 14),
    # legend.text = element_text(size = 14)
  )
p2
ggsave("P2.png",
       width = 3.8, height = 2.5)

# p3 ----------------------------------------------------------------------

p3 <- ggplot(
  data = filter(data1.long, group == "middle dose group"),
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
    alpha = .99
  ) +
  scale_x_continuous(
    # breaks = c(seq(0, 25, by = 5))
    breaks = c(0.0167, 6, 24, 72, 74, 78),
    labels = c(0.0167, 2, 24, 72, 74, 78),
    position = "bottom"
  ) +
  scale_y_continuous(
    limits = c(1, 10000),
    breaks = seq(0, 10000, length.out = 3)
  ) +
  scale_x_break(
    c(25, 71),
    space = .1,
    scales = .4,
  ) +
  # scale_y_log10(
  #   limits = c(1, 40000),
  #   breaks = c(1, 10, 100, 1000, 40000),
  #   labels = c(1, 10, 100, 1000, 40000)
  # ) +
  scale_shape_manual(
    values = rep(15, 8)
  ) +
  labs(
    title = "middle dose group",
    subtitle = "(0.4mg/kg+0.30mg/kg/h EH)",
    y = "",
    x = ""
  ) +
  theme_bw(base_size = 15) +
  theme(
    panel.grid = element_blank(),
    plot.title = element_text(hjust = .5,
                              colour = "#54BDD6",
                              face = "bold"),
    plot.subtitle = element_text(hjust = .5,
                                 colour = "#54BDD6"),
    # plot.margin = unit(rep(0, 4), "lines"),
    legend.position = "none",
    legend.direction = "horizontal",
    # legend.margin = margin(rep(0, 4), unit = "pt"),
    # legend.key.width = unit(.2, "pt"),
    # legend.title = element_text(size = 14),
    # legend.text = element_text(size = 14)
  )
p3
ggsave("P3.png",
       width = 3.8, height = 2.5)

# p4 ----------------------------------------------------------------------

p4 <- ggplot(
  data = filter(data1.long, group == "low dose group"),
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
    alpha = .99
  ) +
  scale_x_continuous(
    # breaks = c(seq(0, 25, by = 5))
    breaks = c(0.0167, 6, 24, 72, 74, 78),
    labels = c(0.0167, 2, 24, 72, 74, 78),
    position = "bottom"
  ) +
  scale_y_continuous(
    limits = c(1, 10000),
    breaks = seq(0, 10000, length.out = 3)
  ) +
  scale_x_break(
    c(25, 71),
    space = .1,
    scales = .4,
  ) +
  # scale_y_log10(
  #   limits = c(1, 40000),
  #   breaks = c(1, 10, 100, 1000, 40000),
  #   labels = c(1, 10, 100, 1000, 40000)
  # ) +
  scale_shape_manual(
    values = rep(17, 8)
  ) +
  labs(
    title = "low dose group",
    subtitle = "(0.4mg/kg+0.15mg/kg/h EH)",
    y = "",
    x = ""
  ) +
  theme_bw(base_size = 15) +
  theme(
    panel.grid = element_blank(),
    plot.title = element_text(hjust = .5,
                              color = "#00A087",
                              face = "bold"),
    plot.subtitle = element_text(hjust = .5,
                                 color = "#00A087"),
    # plot.margin = unit(rep(0, 4), "lines"),
    legend.position = "none",
    legend.direction = "horizontal",
    # legend.margin = margin(rep(0, 4), unit = "pt"),
    # legend.key.width = unit(.2, "pt"),
    # legend.title = element_text(size = 14),
    # legend.text = element_text(size = 14)
  )
p4
ggsave("P4.png",
       width = 3.8, height = 2.5)


# fig ---------------------------------------------------------------------


q1 <- png::readPNG("P1.png", native = TRUE)
q2 <- png::readPNG("P2.png", native = TRUE)
q3 <- png::readPNG("P3.png", native = TRUE)
q4 <- png::readPNG("P4.png", native = TRUE)
mydesign <- "
ABBBBBBCCC
ABBBBBBDDD
ABBBBBBEEE"

# p1 + p2 + p2 + p2 +
#   plot_layout(design = mydesign)

ggplot() + theme_minimal() + q1 + q2 + q3 + q4 +
  plot_layout(
    design = mydesign,
    widths = c(0.0001, 4, 1)
  ) +
  plot_annotation(
    title = "",
    # tag_levels = c("A", "B", "A", "A", "A")
  ) &
  theme(plot.tag = element_text(size = 24))
ggsave("fig.pdf",
       width = 7, height = 5)
ggsave("fig.png",
       width = 7, height = 5)
ggsave("fig.tiff",
       width = 7, height = 5)

p1
ggsave("fig1.pdf",
       width = 9, height = 5)
ggsave("fig1.tiff",
       width = 9, height = 5)
ggsave("fig1.png",
       width = 9, height = 5)
