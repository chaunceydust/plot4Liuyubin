setwd("D:/Work/others/LiuYuBin/2")
library(tidyverse)
library(ggsci)


# table1 ------------------------------------------------------------------
data <- readxl::read_excel("rawData.xlsx",
                           sheet = "Table1",
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
    "Negative control",
    "EH(0.3 mg/kg)",
    "EH(1 mg/kg)",
    "EH(3mg/kg)",
    "LMWH(440I.U.aXa /kg)"
    ),
  labels = c(
    "Negative control",
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




# fig1a -------------------------------------------------------------------
fig1a <- ggplot(
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
    aes(Group, LG, label = tick),
    position = position_dodge(.9),
    size = 8
  ) +
  scale_fill_npg() +
  scale_color_npg() +
  xlab("") +
  ylab("Weight(mg)") +
  ggtitle("") +
  theme_bw(base_size = 23) +
  facet_wrap(.~g2, ncol = 2, scales = "free_y") +
  theme(
    panel.grid = element_blank(),
    legend.position = "none",
    # legend.title = element_blank(),
    # legend.key.width = unit(0.9,"cm")
    axis.text.y = element_text(colour='black',size=18),
    axis.text.x=element_text(
      colour = "black",size = 15,face = "bold",
      angle = 30,hjust = 1,vjust = .85
    ),
    strip.background = element_rect(
      colour = "black",
      fill = "white"
    ),
    strip.text = element_text(
      face = "bold", size = rel(1.1)
    )
  )
fig1a


# table2 ------------------------------------------------------------------
list = ls()
remove(list[which(list != "fig1a")])
data <- readxl::read_excel("rawData.xlsx",
                           sheet = "Table2",
                           col_names = TRUE)

data$Group <- factor(
  data$Group,
  levels = c(
    "Sham operation",
    "Negative control",
    "EH(0.3 mg/kg)",
    "EH(1 mg/kg)",
    "EH(3 mg/kg)",
    "LMWH(440 I.U.aXa/kg)"
  ),
  labels = c(
    "Sham operation",
    "Negative control",
    "EH\n(0.3 mg/kg)",
    "EH\n(1 mg/kg)",
    "EH\n(3 mg/kg)",
    "LMWH(440 \nI.U.aXa/kg)"
  ),
)

data2 <- data %>% 
  group_by(Group) %>% 
  summarise(
    LG = 1.1*max(`Hemoglobin(mg/L)`)
  )

ticks <- select(data, c(Group, ticks)) %>% 
  filter(!is.na(ticks))

data3 <- full_join(data2, ticks)

# scales::show_col()
figb_col <- pal_npg("nrc")(3)[3]
fig1b <- ggplot(data,
       aes(Group, `Hemoglobin(mg/L)`)) +
  geom_violin(
    color = "grey",
    alpha = .8,
    size = 1.2
  ) +
  geom_jitter(
    data = data,
    aes(Group, `Hemoglobin(mg/L)`),
    color = "grey",
    size = 4,
    # position = position_jitterdodge(0.6),
    alpha = .5,
    width = .4
  ) +
  stat_summary(
    fun.data = mean_sdl,
    fun.args = list(mult = 1),
    geom = "errorbar",
    color = figb_col,#"#9A3254",
    size = 1.2,
    width = .25,
    aes()
  ) +
  stat_summary(
    fun.y = mean,
    geom = "point",
    color = figb_col, #"#9A3254",
    size = 6.5,
    aes()
  ) +
  geom_text(
    data = data3,
    aes(Group, LG, label = ticks),
    color = figb_col, #"#9A3254",
    size = 12
  ) +
  xlab("") +
  theme_bw(base_size = 23) +
  theme(
    panel.grid = element_blank(),
    axis.text.y = element_text(colour='black',size=18),
    axis.text.x=element_text(
      colour = "black",size = 15,face = "bold",
      angle = 30,hjust = 1,vjust = 0.85
    )
  )
fig1b

# table3 ------------------------------------------------------------------
# list = ls()
# remove(list[which(list != "fig1b")])
data <- readxl::read_excel("rawData.xlsx",
                           sheet = "Table3",
                           col_names = TRUE)

data$Group <- factor(
  data$Group,
  levels = c(
    "Sham operation",
    "Negative control",
    "EH(0.3 mg/kg)",
    "EH(1 mg/kg)",
    "EH(3 mg/kg)",
    "LMWH(440 I.U.aXa/kg)"
  ),
  labels = c(
    "Sham operation",
    "Negative control",
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

fig1c <- ggplot(data,
                aes(Group, `Bleeding time(s)`)) +
  geom_violin(
    color = "grey",
    alpha = .8,
    size = 1.2
  ) +
  geom_jitter(
    data = data,
    aes(Group, `Bleeding time(s)`),
    color = "grey",
    size = 4,
    # position = position_jitterdodge(0.6),
    alpha = .5,
    width = .4
  ) +
  stat_summary(
    fun.data = mean_sdl,
    fun.args = list(mult = 1),
    geom = "errorbar",
    color = figb_col,
    size = 1.2,
    width = .25,
    aes()
  ) +
  stat_summary(
    fun.y = mean,
    geom = "point",
    color = figb_col,
    size = 5.5,
    aes()
  ) +
  geom_text(
    data = data3,
    aes(Group, LG, label = ticks),
    color = figb_col,
    size = 12
  ) +
  xlab("") +
  theme_bw(base_size = 23) +
  theme(
    panel.grid = element_blank(),
    axis.text.y = element_text(colour='black',size=18),
    axis.text.x=element_text(
      colour = "black",size = 15,face = "bold",
      angle = 30,hjust = 1,vjust = 0.85
    )
  )
fig1c

# table4 ------------------------------------------------------------------
# list = ls()
# remove(list[which(list != "fig1c")])
data <- readxl::read_excel("rawData.xlsx",
                           sheet = "Table4",
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
    "Negative control",
    "EH(0.3 mg/kg)",
    "EH(1 mg/kg)",
    "EH(3 mg/kg)",
    "LMWH(440 I.U.aXa/kg)"
  ),
  labels = c(
    "Sham operation",
    "Negative control",
    "EH(0.3 mg/kg)",
    "EH(1 mg/kg)",
    "EH(3 mg/kg)",
    "LMWH(440 I.U.aXa/kg)"
  ),
)

data2$index <- factor(
  data2$index,
  levels = c(
    "PT(s)", "APTT(s)", "TT(s)", "FIB(g/L)"
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
    "Negative control",
    "EH(0.3 mg/kg)",
    "EH(1 mg/kg)",
    "EH(3 mg/kg)",
    "LMWH(440 I.U.aXa/kg)"
  ),
  labels = c(
    "Sham operation",
    "Negative control",
    "EH\n(0.3 mg/kg)",
    "EH\n(1 mg/kg)",
    "EH\n(3 mg/kg)",
    "LMWH(440 \nI.U.aXa/kg)"
  ),
)

data4$index <- factor(
  data4$index,
  levels = c(
    "PT(s)", "APTT(s)", "TT(s)", "FIB(g/L)"
  )
)

data4a <- filter(data4, index != "FIB(g/L)")
data4a$index <- factor(
  data4a$index,
  levels = c(
    "PT(s)", "APTT(s)", "TT(s)"
  )
)

# fig1d -------------------------------------------------------------------
pd <- position_dodge(.9)
fig1da <- ggplot(
  data4a,
  aes(x = Group, y = MEAN,
      color = index, group = index,
      fill = index)
) +
  geom_col(position = pd) +
  geom_errorbar(
    aes(ymin = MEAN - SD, ymax = MEAN + SD,
        color = index),
    position = pd,
    width = .35, size = 1.4
  ) +
  # geom_line(position = pd) +
  geom_text(
    aes(Group, LG, label = ticks),
    position = pd,
    size = 15
  ) +
  scale_color_npg() +
  scale_fill_npg() +
  xlab("") +
  ylab("") +
  ggtitle("") +
  theme_bw(base_size = 23) +
  theme(
    legend.position = c(0.45, 0.8),
    legend.title = element_blank(),
    legend.direction = "horizontal"
    # legend.key.width = unit(0.9,"cm")
  )
fig1da

data4b <- filter(data4, index == "FIB(g/L)")
data4b$index <- factor(
  data4b$index,
  levels = c(
    "FIB(g/L)"
  )
)

# fig1d -------------------------------------------------------------------
pd <- position_dodge(.9)
fig1db <- ggplot(
  data4b,
  aes(x = Group, y = MEAN,
      color = index, group = index,
      fill = index)
) +
  geom_col(position = pd, width = .5) +
  geom_errorbar(
    aes(ymin = MEAN - SD, ymax = MEAN + SD),
    position = pd,
    width = .35, size = 1.4
  ) +
  # geom_line(position = pd) +
  geom_text(
    aes(Group, LG, label = ticks),
    position = pd,
    size = 15
  ) +
  scale_color_manual(values = pal_npg("nrc")(4)[4]) +
  scale_fill_manual(values = pal_npg("nrc")(4)[4]) +
  xlab("") +
  ylab("") +
  ggtitle("") +
  theme_bw(base_size = 23) +
  theme(
    panel.grid = element_blank(),
    legend.position = c(0.45, 0.8),
    legend.title = element_blank(),
    legend.direction = "horizontal"
    # legend.key.width = unit(0.9,"cm")
  )
fig1db

library(patchwork)
myDesign <- "AAAAAA
BBBCCC
DDDDDD
EEEEEE"

pdf("Fig1-220130.pdf", width = 13.5, height = 18)
fig1a + fig1b + fig1c + fig1da + fig1db +
  plot_layout(design = myDesign) +
  plot_annotation(tag_levels = "A")
dev.off()
