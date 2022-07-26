setwd("D:/Work/others/LiuYuBin/2")
library(tidyverse)
library(ggbreak)
library(gggap)

# table8 ------------------------------------------------------------------
data <- readxl::read_excel("rawData.xlsx",
                           sheet = "Table8",
                           col_names = TRUE)

data2 <- data %>% 
  filter(Group != "Sham operation") %>% 
  pivot_longer(
    c("wet weight", "dry weight"),
    names_to = "g2",
    values_to = "Weight"
  ) 
data2$Weight <- as.numeric(data2$Weight)

data3 <- data2 %>% 
  group_by(
    Group, g2
  ) %>% 
  summarise(
    MEAN = mean(Weight),
    SD = sd(Weight)
  )

data4 <- data[c(1, 4, 6)] 
colnames(data4)[2:3] <- c("wet weight", "dry weight")
data4 <- data4[which(!is.na(data4$`wet weight`) & !is.na(data4$`dry weight`)),] %>% 
  pivot_longer(
    c("wet weight", "dry weight"),
    names_to = "g2",
    values_to = "tick"
  )

data3 <- full_join(data3, data4)

data3$Group <- factor(
  data3$Group,
  levels = c(
    "Normal saline",
    "EH(0.3 mg/kg)",
    "EH(1 mg/kg)",
    "EH(3mg/kg)",
    "LMWH(440I.U.aXa /kg)"
  ),
  labels = c(
    "Normal saline",
    "EH\n(0.3 mg/kg)",
    "EH\n(1 mg/kg)",
    "EH\n(3 mg/kg)",
    "LMWH(440 \nI.U.aXa/kg)"
  ),
)
data3$g2 <- factor(
  data3$g2,
  levels = c("wet weight", "dry weight"),
  labels = c("wet weight", "dry weight")
)
data3$LG <- 1.2*(data3$MEAN + data3$SD)




# fig3a -------------------------------------------------------------------
fig3a1 <- ggplot(
  filter(data3, g2 == "wet weight"),
  aes(x = Group, y = MEAN,
      fill = g2)
) +
  geom_col(position = "dodge") +
  geom_errorbar(
    aes(ymin = MEAN - SD, ymax = MEAN + SD,
        color = g2),
    position = position_dodge(.9),
    width = .35, size = 1.4
  ) +
  geom_text(
    aes(Group, LG, label = tick),
    position = position_dodge(.9),
    size = 8
  ) +
  facet_wrap(.~g2, ncol = 2, scales = "free_y") +
  xlab("") +
  ylab("Weight(mg)") +
  ggtitle("") +
  theme_bw(base_size = 13) +
  theme(
    legend.position = "none",
    legend.title = element_blank(),
    # legend.key.width = unit(0.9,"cm")
    # axis.title.y = element_text(colour='black',size=18),
    # axis.text.y = element_text(colour = 'black', size = 18),
    axis.text.x=element_text(
      colour = "black",size = 13,face = "bold",
      angle = 40,hjust = .5,vjust = 0.5
    ),
    strip.background = element_rect(
      colour = "black",
      fill = "white"
    ),
    strip.text = element_text(
      face = "bold", size = rel(1.2)
    ),
    plot.margin = unit(c(0,0,0,0), "lines")
  ) +
  scale_y_break(
    c(15, 25), 
    scales = .4
  ) +
  scale_y_continuous(
    breaks = c(-7, 0, 7, 14, 30, 50)
  )

pdf("fig3a1.pdf", width = 7, height = 4.5)
fig3a1
dev.off()
  
data3$tick[1] <- "***"
fig3a2 <- ggplot(
  filter(data3, g2 == "dry weight"),
  aes(x = Group, y = MEAN)
) +
  geom_col(position = "dodge",
           fill = "#00BFC4",
           color = "#00BFC4") +
  geom_errorbar(
    aes(ymin = MEAN - SD, ymax = MEAN + SD),
    color = "#00BFC4",
    position = position_dodge(.9),
    width = .35, size = 1.4
  ) +
  geom_text(
    aes(Group, LG, label = tick),
    position = position_dodge(.9),
    size = 8
  ) +
  facet_wrap(.~g2, ncol = 2, scales = "free_y") +
  xlab("") +
  ylab("Weight(mg)") +
  ggtitle("") +
  theme_bw(base_size = 13) +
  theme(
    legend.position = "none",
    legend.title = element_blank(),
    # legend.key.width = unit(0.9,"cm")
    # axis.title.y = element_text(colour='black',size=18),
    # axis.text.y = element_text(colour = 'black', size = 18),
    axis.text.x=element_text(
      colour = "black",size = 13,face = "bold",
      angle = 40,hjust = .5,vjust = 0.5
    ),
    strip.background = element_rect(
      colour = "black",
      fill = "white"
    ),
    strip.text = element_text(
      face = "bold", size = rel(1.1)
    ),
    plot.margin = unit(c(0,0.2,0,0), "lines")
  ) +
  scale_y_continuous(
    breaks = c(-7, 0, 7, 14, 30, 50)
  )
fig3a2
pdf("fig3a2.pdf", width = 7, height = 4.5)
fig3a2
dev.off()


# table9 ------------------------------------------------------------------
data <- readxl::read_excel("rawData.xlsx",
                           sheet = "Table9",
                           col_names = TRUE)

data2 <- data %>% 
  pivot_longer(
    c("3d (Score)", "7d (Score)"),
    names_to = "g2",
    values_to = "Weight"
  ) 
data2$Weight <- as.numeric(data2$Weight)

data3 <- data2 %>% 
  group_by(
    Group, g2
  ) %>% 
  summarise(
    MEAN = mean(Weight),
    SD = sd(Weight)
  )

data4 <- data[c(1, 4, 6)] 
colnames(data4)[2:3] <- c("3d (Score)", "7d (Score)")
data4 <- data4[which(!is.na(data4$`3d (Score)`) & !is.na(data4$`7d (Score)`)),] %>% 
  pivot_longer(
    c("3d (Score)", "7d (Score)"),
    names_to = "g2",
    values_to = "tick"
  )

data3 <- full_join(data3, data4)

data3$Group <- factor(
  data3$Group,
  levels = c(
    "Sham operation",
    "Normal saline",
    "EH(0.3 mg/kg)",
    "EH(1 mg/kg)",
    "EH(3 mg/kg)",
    "LMWH(440 I.U.aXa/kg)"
  ),
  labels = c(
    "Sham operation",
    "Normal saline",
    "EH\n(0.3 mg/kg)",
    "EH\n(1 mg/kg)",
    "EH\n(3 mg/kg)",
    "LMWH(440 \nI.U.aXa/kg)"
  ),
)
# data3$g2 <- factor(
#   data3$g2,
#   levels = c("wet weight", "dry weight"),
#   labels = c("wet weight", "dry weight")
# )
data3$LG <- 1.4*(data3$MEAN + data3$SD)




# fig3b -------------------------------------------------------------------
fig3b <- ggplot(
  data3,
  aes(x = Group, y = MEAN,
      fill = g2)
) +
  geom_col(position = "dodge") +
  geom_errorbar(
    aes(ymin = MEAN - SD, ymax = MEAN + SD,
        color = g2),
    position = position_dodge(.9),
    width = .35, size = 1.4
  ) +
  geom_text(
    aes(Group, y = 19.5, label = tick),
    position = position_dodge(.9),
    size = 8,
    color = "white"
  ) +
  xlab("") +
  ylab("BBB value") +
  ggtitle("") +
  theme_bw(base_size = 13) +
  theme(
    # legend.position = c(0.6,0.8),
    legend.position = "top",
    legend.title = element_blank(),
    # legend.key.width = unit(0.9,"cm")
    # axis.text.y = element_text(colour='black',size=14),
    # axis.title.y = element_text(colour='black',size=18),
    axis.text.x=element_text(
      colour = "black",size = 13,
      # face = "bold",
      angle = 30,hjust = .5,vjust = 0.5
    ),
    strip.background = element_rect(
      colour = "black",
      fill = "white"
    ),
    strip.text = element_text(
      face = "bold", size = rel(1.1)
    )
  ) +
  scale_y_break(c(4, 18), scales = .35) +
  scale_y_continuous(limits = c(-0.5, 20),
                     breaks = c(0, 2, 4, 18,19),
                     labels = c(0, 2, 4, 18,21)) 
  
  
fig3b

pdf("fig3b-long.pdf", width = 12.5, height = 5.4)
fig3b
dev.off()


# table10 -----------------------------------------------------------------
data <- readxl::read_excel("rawData.xlsx",
                           sheet = "Table10",
                           col_names = TRUE)

data$Group <- factor(
  data$Group,
  levels = c(
    "Sham operation",
    "Normal saline",
    "EH(0.3 mg/kg)",
    "EH(1 mg/kg)",
    "EH(3 mg/kg)",
    "LMWH(440 I.U.aXa/kg)"
  ),
  labels = c(
    "Sham operation",
    "Normal saline",
    "EH\n(0.3 mg/kg)",
    "EH\n(1 mg/kg)",
    "EH\n(3 mg/kg)",
    "LMWH(440 \nI.U.aXa/kg)"
  ),
)

# fig3c -------------------------------------------------------------------



fig3c <- ggplot(data,
                aes(Group, `Extent of disease (%)`)) +
  geom_violin(
    color = "grey",
    alpha = .3,
    size = 1.2
  ) +
  geom_jitter(
    data = data,
    aes(Group, `Extent of disease (%)`),
    color = "grey",
    size = 4,
    # position = position_jitterdodge(0.6),
    alpha = .25,
    width = .4
  ) +
  stat_summary(
    fun.data = mean_sdl,
    fun.args = list(mult = 1),
    geom = "errorbar",
    color = "#9A3254",
    size = 1.2,
    width = .25,
    aes()
  ) +
  stat_summary(
    fun.y = mean,
    geom = "point",
    color ="#9A3254",
    size = 5.5,
    aes()
  ) +
  # geom_text(
  #   data = data3,
  #   aes(Group, LG, label = ticks),
  #   color = "#9A3254",
  #   size = 12
  # ) +
  xlab("") +
  ylab("Range of lesions(%)") +
  theme_bw(base_size = 13) +
  theme(
    # axis.text.y = element_text(colour='black',size=18),
    axis.text.x=element_text(
      colour = "black",size = 13,face = "bold",
      angle = 40,hjust = .5,vjust = 0.5
    )
  )
pdf("fig3c.pdf", width = 6, height = 5.4)
fig3c
dev.off()


# table11 -----------------------------------------------------------------

data <- readxl::read_excel("rawData.xlsx",
                           sheet = "Table11",
                           col_names = TRUE)

data$Group <- factor(
  data$Group,
  levels = c(
    "Sham operation",
    "Normal saline",
    "EH(0.3 mg/kg)",
    "EH(1 mg/kg)",
    "EH(3 mg/kg)",
    "LMWH(440 I.U.aXa/kg)"
  ),
  labels = c(
    "Sham operation",
    "Normal saline",
    "EH\n(0.3 mg/kg)",
    "EH\n(1 mg/kg)",
    "EH\n(3 mg/kg)",
    "LMWH(440 \nI.U.aXa/kg)"
  ),
)

data2 <- data %>% 
  group_by(Group) %>% 
  summarise(
    LG = 1.1*max(`Red blood cell count`)
  )

ticks <- select(data, c(Group, ticks)) %>% 
  filter(!is.na(ticks))

data3 <- full_join(data2, ticks)


# fig3d -------------------------------------------------------------------

fig3d <- ggplot(data,
                aes(Group, `Red blood cell count`)) +
  geom_violin(
    color = "grey",
    alpha = .3,
    size = 1.2
  ) +
  geom_jitter(
    data = data,
    aes(Group, `Red blood cell count`),
    color = "grey",
    size = 4,
    # position = position_jitterdodge(0.6),
    alpha = .25,
    width = .4
  ) +
  stat_summary(
    fun.data = mean_sdl,
    fun.args = list(mult = 1),
    geom = "errorbar",
    color = "#9A3254",
    size = 1.2,
    width = .25,
    aes()
  ) +
  stat_summary(
    fun.y = mean,
    geom = "point",
    color ="#9A3254",
    size = 5.5,
    aes()
  ) +
  geom_text(
    data = data3,
    aes(Group, LG, label = ticks),
    color = "#9A3254",
    size = 12
  ) +
  xlab("") +
  theme_bw(base_size = 13) +
  theme(
    # axis.text.y = element_text(colour='black',size=18),
    axis.text.x=element_text(
      colour = "black",size = 13,
      # face = "bold",
      angle = 40,hjust = .5,vjust = 0.5
    )
  )
pdf("fig3d.pdf", width = 6, height = 5.4)
fig3d
dev.off()

# table12 -----------------------------------------------------------------

data <- readxl::read_excel("rawData.xlsx",
                           sheet = "Table12",
                           col_names = TRUE)

data$Group <- factor(
  data$Group,
  levels = c(
    "Sham operation",
    "Normal saline",
    "EH(0.3 mg/kg)",
    "EH(1 mg/kg)",
    "EH(3 mg/kg)",
    "LMWH(440 I.U.aXa/kg)"
  ),
  labels = c(
    "Sham operation",
    "Normal saline",
    "EH\n(0.3 mg/kg)",
    "EH\n(1 mg/kg)",
    "EH\n(3 mg/kg)",
    "LMWH(440 \nI.U.aXa/kg)"
  ),
)

data2 <- data %>% 
  group_by(Group) %>% 
  summarise(
    LG = 1.1*max(`Bleeding time(s)`)
  )

ticks <- select(data, c(Group, ticks)) %>% 
  filter(!is.na(ticks))

data3 <- full_join(data2, ticks)

# fig3e -------------------------------------------------------------------

fig3e <- ggplot(data,
                aes(Group, `Bleeding time(s)`)) +
  geom_violin(
    color = "grey",
    alpha = .3,
    size = 1.2
  ) +
  geom_jitter(
    data = data,
    aes(Group, `Bleeding time(s)`),
    color = "grey",
    size = 4,
    # position = position_jitterdodge(0.6),
    alpha = .25,
    width = .4
  ) +
  stat_summary(
    fun.data = mean_sdl,
    fun.args = list(mult = 1),
    geom = "errorbar",
    color = "#9A3254",
    size = 1.2,
    width = .25,
    aes()
  ) +
  stat_summary(
    fun.y = mean,
    geom = "point",
    color ="#9A3254",
    size = 5.5,
    aes()
  ) +
  geom_text(
    data = data3,
    aes(Group, LG, label = ticks),
    color = "#9A3254",
    size = 8
  ) +
  xlab("") +
  theme_bw(base_size = 13) +
  theme(
    # axis.text.y = element_text(colour='black',size=18),
    axis.text.x=element_text(
      colour = "black",size = 13,
      # face = "bold",
      angle = 40,hjust = .5,vjust = 0.5
    )
  )

# table13 -----------------------------------------------------------------

data <- readxl::read_excel("rawData.xlsx",
                           sheet = "Table13",
                           col_names = TRUE)

data2 <- data %>% 
  select(
    c(Group, `PT(s)`, `APTT(s)`, `TT(s)`, `FIB(g/L)`)
  ) %>% 
  pivot_longer(
    c(`PT(s)`, `APTT(s)`, `TT(s)`, `FIB(g/L)`),
    names_to = "index",
    values_to = "value"
  )

data2$Group <- factor(
  data2$Group,
  levels = c(
    "Sham operation",
    "Normal saline",
    "EH(0.3 mg/kg)",
    "EH(1 mg/kg)",
    "EH(3 mg/kg)",
    "LMWH(440 I.U.aXa/kg)"
  ),
  labels = c(
    "Sham operation",
    "Normal saline",
    "EH(0.3 mg/kg)",
    "EH(1 mg/kg)",
    "EH(3 mg/kg)",
    "LMWH(440 I.U.aXa/kg)"
  ),
)

data2$index <- factor(
  data2$index,
  levels = c(
    "APTT(s)", "PT(s)", "TT(s)", "FIB(g/L)"
  )
)

data3 <- data2 %>% 
  group_by(Group, index) %>% 
  summarise(
    MEAN = mean(value),
    SD = sd(value),
    LG = 1.1*max(value)
  )

ticks <- data[c(1, 5, 7, 9)]
colnames(ticks)[2:4] <- colnames(data)[c(4, 6, 8)]

ticks2 <- ticks %>% 
  pivot_longer(
    c(`APTT(s)`, `TT(s)`, `FIB(g/L)`),
    names_to = "index",
    values_to = "ticks"
  ) %>% 
  filter(!is.na(ticks))


data4 <- full_join(data3, ticks2)
data4$Group <- factor(
  data4$Group,
  levels = c(
    "Sham operation",
    "Normal saline",
    "EH(0.3 mg/kg)",
    "EH(1 mg/kg)",
    "EH(3 mg/kg)",
    "LMWH(440 I.U.aXa/kg)"
  ),
  labels = c(
    "Sham operation",
    "Normal saline",
    "EH\n(0.3 mg/kg)",
    "EH\n(1 mg/kg)",
    "EH\n(3 mg/kg)",
    "LMWH(440 \nI.U.aXa/kg)"
  ),
)

data4$index <- factor(
  data4$index,
  levels = c(
    "APTT(s)", "PT(s)", "TT(s)", "FIB(g/L)"
  )
)

# data4a <- filter(data4, index != "FIB(g/L)")
# data4a$index <- factor(
#   data4a$index,
#   levels = c(
#     "PT(s)", "APTT(s)", "TT(s)"
#   )
# )
# fig3f -------------------------------------------------------------------
# pd <- position_dodge(.9)
data4$LG[21] <- 68
data4$LG[23] <- 128
data4$LG[8] <- 6
data4$LG[20] <- 6
fig3fa <- ggplot(
  data4,
  aes(x = Group, y = MEAN,
      color = index, group = index,
      fill = index)
) +
  geom_bar(
    stat = "identity"
  ) +
  geom_errorbar(
    aes(ymin = MEAN - SD, ymax = MEAN + SD,
        color = index),
    # position = pd,
    width = .35, size = 1.4
  ) +
  # geom_line(position = pd) +
  geom_text(
    aes(Group, LG, label = ticks),
    # position = pd,
    size = 8
  ) +
  xlab("") +
  ylab("") +
  ggtitle("") +
  facet_wrap(~index, ncol = 2,
             scales = "free_y") +
  theme_bw(base_size = 13) +
  ggsci::scale_color_npg() +
  ggsci::scale_fill_npg() +
  theme(
    legend.position = "none",
    # legend.title = element_blank(),
    # legend.direction = "horizontal"
    # legend.key.width = unit(0.9,"cm")
    axis.text.x=element_text(
      colour = "black",size = 13,
      # face = "bold",
      angle = 40,hjust = .5,vjust = 0.5
    ),
    strip.background = element_rect(
      colour = "black",
      fill = "white"
    ),
    strip.text = element_text(
      face = "bold", size = rel(1.1)
    )
  )
fig3fa

# data4b <- filter(data4, index == "FIB(g/L)")
# data4b$index <- factor(
#   data4b$index,
#   levels = c(
#     "FIB(g/L)"
#   )
# )
# # fig3fb -------------------------------------------------------------------
# pd <- position_dodge(.9)
# fig3fb <- ggplot(
#   data4b,
#   aes(x = Group, y = MEAN,
#       color = index, group = index,
#       fill = index)
# ) +
#   geom_col(position = pd, width = .27) +
#   geom_errorbar(
#     aes(ymin = MEAN - SD, ymax = MEAN + SD,
#         color = index),
#     position = pd,
#     width = .1, size = 1.4
#   ) +
#   # geom_line(position = pd) +
#   geom_text(
#     aes(Group, LG, label = ticks),
#     position = pd,
#     size = 15
#   ) +
#   xlab("") +
#   ylab("") +
#   ggtitle("") +
#   theme_bw(base_size = 23) +
#   theme(
#     legend.position = c(0.45, 0.8),
#     legend.title = element_blank(),
#     legend.direction = "horizontal"
#     # legend.key.width = unit(0.9,"cm")
#   )
# fig3fb

library(patchwork)
myDesign <- "AA
BC
DD
EE"
# fig3a_2 <- barplot(1:10)
# pp <- as.ggplot(fig3a_2)

# pdf("fig3_220427.pdf", width = 13.5, height = 15)
pdf("fig3_220427.pdf", width = 10, height = 12)
# fig3a1 + fig3b + fig3c + fig3d + 
#   fig3e + fig3fa + fig3fb + fig3a2 +
#   plot_layout(design = myDesign) +
#   plot_annotation(tag_levels = "A")
# fig3a1 + fig3b + fig3d + 
#   fig3e + fig3fa  + fig3a2 +
#   plot_layout(design = myDesign) +
#   plot_annotation(tag_levels = "A")


myfig3a1 <- png::readPNG(
  "fig3a1.png",
  native = TRUE
)
myfig3a2 <- png::readPNG(
  "fig3a2.png",
  native = TRUE
)
myblank <- ggplot() + theme_minimal()

myblank +
  myfig3a1 + myfig3a2 + 
  fig3e + fig3fa   +
  plot_layout(design = myDesign,
              heights = c(1, 12, 7, 8.5)) +
  plot_annotation(tag_levels = "A")
dev.off()
