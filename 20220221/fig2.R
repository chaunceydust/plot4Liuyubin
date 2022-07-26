library(tidyverse)
library(ggsci)
library(patchwork)
library(ggbreak)

myPos <- position_dodge(.7)

data1 <- xlsx::read.xlsx(
  "rawdata0524.xlsx", sheetIndex = 1,
  startRow = 2
)

data1$Day <- factor(
  data1$Day,
  levels = c("D -1", "D 1", "D 2", "D 3", "D 4")
)
data1$Group <- factor(
  data1$Group,
  levels = c("High dose group", "Middle dose group", "Low dose group")
)
colnames(data1)[2] <- "group"


# p1 -----------------------------------------------------------------------

p1 <- ggplot(
  data = data1,
  aes(
    Day, Mean,
    color = group,
    group = group,
    shape = group
  )
) +
  geom_line(
    size = 1.2,
    alpha = .5,
    position = myPos
  ) +
  geom_errorbar(
    aes(Day, ymin = Mean - SD, ymax = Mean + SD),
    size = .95,   # 1.35
    width = .45,
    position = myPos
  ) +
  geom_point(
    size = 3.4,
    aes(fill = group, colour = group,
        shape = group),
    position = myPos
  ) +
  scale_y_continuous(
    limits = c(8, 20),
    breaks = seq(8, 20, length.out = 3)
  ) +
  scale_color_npg() +
  scale_shape_manual(
    values = c(19, 15, 17)
  ) +
  labs(
    title = "Respiration rate (times/min)",
    y = ""
  ) +
  theme_bw(base_size = 12) +
  theme(
    legend.position = "top",
    panel.grid.minor = element_blank(),
    plot.margin = unit(c(.3, .3, .3, .3), "cm")
  )
p1
# ggsave("Fig20220524/fg1.pdf",
#        width = 5.5, height = 4)
# ggsave("Fig20220524/fg1.png",
#        width = 5.5, height = 4)
# ggsave("Fig20220524/fg1.tiff",
#        width = 5.5, height = 4)



# fig2 --------------------------------------------------------------------

data2 <- xlsx::read.xlsx(
  "rawdata0524.xlsx", sheetIndex = 2,
  startRow = 1
)

data2$Day <- factor(
  data2$Day,
  levels = c("D -1", "D 1", "D 2", "D 3", "D 4")
)
data2$group <- factor(
  data2$group,
  levels = c("High dose group", "Middle dose group", "Low dose group")
)

data2$Index <- factor(
  data2$Index,
  levels = c("Diastolic blood pressure", "Systolic blood pressure")
)

p2 <- ggplot(
  data = data2,
  aes(
    Day, Mean,
    color = group,
    group = group,
    shape = group
  )
) +
  geom_line(
    size = 1.2,
    alpha = .5,
    position = myPos
  ) +
  geom_errorbar(
    aes(Day, ymin = Mean - SD, ymax = Mean + SD),
    size = .95,   # 1.35
    width = .45,
    position = myPos
  ) +
  geom_point(
    size = 3.4,
    aes(fill = group, colour = group,
        shape = group),
    position = myPos
  ) +
  facet_wrap(.~Index, ncol = 1, scales = "free") +
  # scale_y_continuous(
  #   limits = c(8, 20),
  #   breaks = seq(8, 20, length.out = 3)
  # ) +
  scale_color_npg() +
  scale_shape_manual(
    values = c(19, 15, 17)
  ) +
  labs(
    title = "Blood pressure (mmHg)",
    y = ""
  ) +
  theme_bw(base_size = 12) +
  theme(
    legend.position = "top",
    panel.grid.minor = element_blank(),
    plot.margin = unit(c(.3, .3, .3, .3), "cm")
  )
p2
# ggsave("Fig20220524/fg2.pdf",
#        width = 5.5, height = 6)
# ggsave("Fig20220524/fg2.png",
#        width = 5.5, height = 6)
# ggsave("Fig20220524/fg2.tiff",
#        width = 5.5, height = 6)


# fig3 --------------------------------------------------------------------

data3 <- xlsx::read.xlsx(
  "rawdata0524.xlsx", sheetIndex = 3,
  startRow = 2
)

data3$Day <- factor(
  data3$Day,
  levels = c("D -1", "D 1", "D 2", "D 3", "D 4")
)
data3$Group <- factor(
  data3$Group,
  levels = c("High dose group", "Middle dose group", "Low dose group")
)
colnames(data3)[2] <- "group"


# p1 -----------------------------------------------------------------------

p3 <- ggplot(
  data = data3,
  aes(
    Day, Mean,
    color = group,
    group = group,
    shape = group
  )
) +
  geom_line(
    size = 1.2,
    alpha = .5,
    position = myPos
  ) +
  geom_errorbar(
    aes(Day, ymin = Mean - SD, ymax = Mean + SD),
    size = .95,   # 1.35
    width = .45,
    position = myPos
  ) +
  geom_point(
    size = 3.4,
    aes(fill = group, colour = group,
        shape = group),
    position = myPos
  ) +
  scale_y_continuous(
    limits = c(35, 37.2),
    breaks = seq(35, 37, length.out = 3)
  ) +
  scale_color_npg() +
  scale_shape_manual(
    values = c(19, 15, 17)
  ) +
  labs(
    title = "Temperature (℃)",
    y = ""
  ) +
  theme_bw(base_size = 12) +
  theme(
    legend.position = "top",
    panel.grid.minor = element_blank(),
    plot.margin = unit(c(.3, .3, .3, .3), "cm")
  )
p3
# ggsave("Fig20220524/fg3.pdf",
#        width = 5.5, height = 4)
# ggsave("Fig20220524/fg3.png",
#        width = 5.5, height = 4)
# ggsave("Fig20220524/fg3.tiff",
#        width = 5.5, height = 4)



# fig4 --------------------------------------------------------------------

data4 <- xlsx::read.xlsx(
  "rawdata0524.xlsx", sheetIndex = 4,
  startRow = 2
)

data4$Day <- factor(
  data4$Day,
  levels = c("D -1", "D 1", "D 2", "D 3", "D 4")
)
data4$Group <- factor(
  data4$Group,
  levels = c("High dose group", "Middle dose group", "Low dose group")
)
colnames(data4)[2] <- "group"


# p1 -----------------------------------------------------------------------

p4 <- ggplot(
  data = data4,
  aes(
    Day, Mean,
    color = group,
    group = group,
    shape = group
  )
) +
  geom_line(
    size = 1.2,
    alpha = .5,
    position = myPos
  ) +
  geom_errorbar(
    aes(Day, ymin = Mean - SD, ymax = Mean + SD),
    size = .95,   # 1.35
    width = .45,
    position = myPos
  ) +
  geom_point(
    size = 3.4,
    aes(fill = group, colour = group,
        shape = group),
    position = myPos
  ) +
  scale_y_continuous(
    limits = c(55, 83),
    breaks = seq(55, 83, by = 5)
  ) +
  scale_color_npg() +
  scale_shape_manual(
    values = c(19, 15, 17)
  ) +
  labs(
    title = "Heart rate (times/min)",
    y = ""
  ) +
  theme_bw(base_size = 12) +
  theme(
    legend.position = "top",
    panel.grid.minor = element_blank(),
    plot.margin = unit(c(.3, .3, .3, .3), "cm")
  )
p4
# ggsave("Fig20220524/fg4.pdf",
#        width = 5.5, height = 4)
# ggsave("Fig20220524/fg4.png",
#        width = 5.5, height = 4)
# ggsave("Fig20220524/fg4.tiff",
#        width = 5.5, height = 4)


myLayot <- "
AB
#B
CD"

p1 + p2 + p3 + p4 +
  plot_annotation(
    tag_levels = "A"
  ) +
  plot_layout(
    design = myLayot,
    guides = "collect"
  ) &
  theme(
    legend.position = "bottom",
    plot.tag = element_text(
      face = "bold",
      size = 18
    )
  )

ggsave("Total.pdf",
       height = 8, width = 8)
ggsave("Total.png",
       height = 8, width = 8)
ggsave("Total.tiff",
       height = 8, width = 8)
