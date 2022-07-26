setwd("D:/Work/others/LiuYuBin/2")
library(tidyverse)

# table5 ------------------------------------------------------------------
data <- readxl::read_excel("rawData.xlsx",
                           sheet = "Table5",
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
    Group, Time, g2
  ) %>% 
  summarise(
    MEAN = mean(Weight),
    SD = sd(Weight)
  )

data4 <- data[c(1, 2, 5, 7)] 
colnames(data4)[3:4] <- c("wet weight", "dry weight")
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
    "EH(1 mg/kg)",
    "LMWH(440 I.U.aXa/kg)"
  ),
  labels = c(
    "Negative control",
    "EH(1 mg/kg)",
    "LMWH(440 I.U.aXa/kg)"
  ),
)
data3$g2 <- factor(
  data3$g2,
  levels = c("wet weight", "dry weight"),
  labels = c("wet weight", "dry weight")
)
data3$Time <- factor(
  data3$Time,
  levels = c("1h", "2h", "4h", "6h", "24h")
)
data3$LG <- 1.2*(data3$MEAN + data3$SD)

# fig2a -------------------------------------------------------------------
fig2a <- ggplot(
  data3,
  aes(x = Group, y = MEAN,
      fill = Time)
) +
  geom_col(position = "dodge") +
  geom_errorbar(
    aes(ymin = MEAN - SD, ymax = MEAN + SD,
        color = Time),
    position = position_dodge(.9),
    width = .35, size = 1.4
  ) +
  geom_text(
    aes(Group, LG, label = tick),
    position = position_dodge(.9),
    size = 8
  ) +
  facet_wrap(~g2, ncol = 1, scales = "free_y") +
  xlab("") +
  ylab("Weight(mg)") +
  ggtitle("") +
  theme_bw(base_size = 23) +
  theme(
    legend.position = "top",
    # legend.title = element_blank(),
    # legend.key.width = unit(0.9,"cm")
    axis.text.y = element_text(colour='black',size=18),
    axis.text.x=element_text(
      colour = "black",size = 15,face = "bold",
      angle = 0,hjust = .5,vjust = 0.5
    )
  ) +
  theme(
    strip.text.x = element_text(face="bold", size=rel(.7)),
    strip.text.y = element_text(face="bold", size=rel(.5)),
    # strip.text.y = element_blank(),
    strip.background = element_rect(
      fill="lightblue", colour="black", size=1
    ),
    # strip.background.y = element_blank()
  )


# table6 ------------------------------------------------------------------

data <- readxl::read_excel("rawData.xlsx",
                           sheet = "Table6",
                           col_names = TRUE)

data$Group <- factor(
  data$Group,
  levels = c(
    "Sham operation",
    "Negative control",
    "EH(1 mg/kg)",
    "LMWH(440 I.U.aXa/kg)"
  ),
  labels = c(
    "Sham operation",
    "Negative control",
    "EH(1 mg/kg)",
    "LMWH(440 I.U.aXa/kg)"
  ),
)

data2 <- data %>% 
  group_by(Group, Time) %>% 
  summarise(
    LG = 1.1*max(`Bleeding volume (mg/L)`)
  )

ticks <- select(data, c(Group, Time, ticks)) %>% 
  filter(!is.na(ticks))

data3 <- full_join(data2, ticks)

data$Time <- factor(
  data$Time,
  levels = c("1h", "2h", "4h", "6h", "24h")
)
data3$Time <- factor(
  data3$Time,
  levels = c("1h", "2h", "4h", "6h", "24h")
)

pd <- position_dodge(.5)
fig2b <- ggplot(data,
                aes(Group, `Bleeding volume (mg/L)`,
                    color = Time,
                    group = Time)) +
  # geom_violin(,
  #   alpha = .3,
  #   size = 1.2,
  #   position = pd
  # ) +
  # geom_jitter(
  #   data = data,
  #   aes(Group, `Bleeding volume (mg/L)`,
  #       color = Time),
  #   # color = "grey",
  #   size = 4,
  #   # position = pd,
  #   alpha = .25,
  #   width = .4
  # ) +
  stat_summary(
    fun.data = mean_sdl,
    fun.args = list(mult = 1),
    geom = "errorbar",
    # color = "#9A3254",
    size = 1.2,
    width = .25,
    aes(),
    position = pd
  ) +
  stat_summary(
    fun.y = mean,
    geom = "point",
    # color ="#9A3254",
    size = 5.5,
    aes(),
    position = pd
  ) +
  geom_text(
    data = data3,
    aes(Group, LG, label = ticks),
    # color = "#9A3254",
    size = 5,
    position = pd
  ) +
  xlab("") +
  theme_bw(base_size = 23) +
  theme(
    axis.text.y = element_text(colour='black',size=18),
    axis.title.y = element_text(colour='black',size=18),
    axis.text.x=element_text(
      colour = "black",size = 15,face = "bold",
      angle = 0,hjust = .5,vjust = 0.5
    ),
    legend.position = "none"
  )

# table7 ------------------------------------------------------------------

data <- readxl::read_excel("rawData.xlsx",
                           sheet = "Table7",
                           col_names = TRUE)

data$Group <- factor(
  data$Group,
  levels = c(
    "Sham operation",
    "Negative control",
    "EH(1 mg/kg)",
    "LMWH(440 I.U.aXa/kg)"
  ),
  labels = c(
    "Sham operation",
    "Negative control",
    "EH(1 mg/kg)",
    "LMWH(440 I.U.aXa/kg)"
  ),
)

data2 <- data %>% 
  group_by(Group, Time) %>% 
  summarise(
    LG = 1.1*max(`Bleeding time (s)`)
  )

ticks <- select(data, c(Group, Time, ticks)) %>% 
  filter(!is.na(ticks))

data3 <- full_join(data2, ticks)

data$Time <- factor(
  data$Time,
  levels = c("1h", "2h", "4h", "6h", "24h")
)
data3$Time <- factor(
  data3$Time,
  levels = c("1h", "2h", "4h", "6h", "24h")
)

pd <- position_dodge(.5)
fig2c <- ggplot(data,
                aes(Group, `Bleeding time (s)`,
                    color = Time,
                    group = Time)) +
  # geom_violin(,
  #   alpha = .3,
  #   size = 1.2,
  #   position = pd
  # ) +
  # geom_jitter(
  #   data = data,
  #   aes(Group, `Bleeding volume (mg/L)`,
  #       color = Time),
  #   # color = "grey",
  #   size = 4,
#   # position = pd,
#   alpha = .25,
#   width = .4
# ) +
stat_summary(
  fun.data = mean_sdl,
  fun.args = list(mult = 1),
  geom = "errorbar",
  # color = "#9A3254",
  size = 1.2,
  width = .25,
  aes(),
  position = pd
) +
  stat_summary(
    fun.y = mean,
    geom = "point",
    # color ="#9A3254",
    size = 5.5,
    aes(),
    position = pd
  ) +
  geom_text(
    data = data3,
    aes(Group, LG, label = ticks),
    # color = "#9A3254",
    size = 9,
    position = pd
  ) +
  xlab("") +
  theme_bw(base_size = 23) +
  theme(
    axis.text.y = element_text(colour='black',size=18),
    axis.text.x=element_text(
      colour = "black",size = 15,face = "bold",
      angle = 0,hjust = .5,vjust = 0.5
    ),
    legend.position = "none"
  )

library(patchwork)
pdf("fig2.pdf", width = 13.5, height = 15.5)
fig2a + fig2b + fig2c + 
  plot_layout(ncol = 1, heights = c(1.8,1,1)) +
  plot_annotation(tag_levels = "A")
dev.off()
