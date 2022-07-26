setwd("D:/Work/others/LiuYuBin/20220509")

library(tidyverse)
library(ggprism)
library(ggsci)

myCol <- c("#2c3e50", "#e31a1c", "#18bc9c", "#ccbe93", "#a6cee3")
# fig1 --------------------------------------------------------------------

data1 <- xlsx::read.xlsx("rawData.xlsx", sheetIndex = 1)

data1$Index <- factor(
  data1$Index,
  levels = c("Respiratory Rate (times/minute)", "Respiration Intensity (%)")
)
data1$Time <- factor(
  data1$Time,
  levels = c("-15", "15", "10", "30", "60", "120")
)
data1$Group <- factor(
  data1$Group,
  levels = c("Normal saline", "3 mg/kg EH", "30 mg/kg EH")
)


ggplot(data1) +
  geom_line(
    aes(Time, mean,
        color = Group,
        group = Group),
    size = 1,
    alpha = .35
  ) +
  geom_errorbar(
    aes(Time, ymin = mean -SD, ymax = mean + SD,
        color = Group,
        group = Group),
    size = 1,
    alpha = .75,
    width = .15
  ) +
  geom_point(
    aes(Time, mean,
        color = Group, shape = Group,
        group = Group),
    size = 4.8
  ) +
  labs(
    x = "time (min)",
    y = "",
  ) +
  # legend(title = "dose") +
  # guides(color = guide_legend(title.vjust = c(.5))) +
  scale_color_manual(values = myCol) +
  facet_wrap(.~Index, ncol = 1, scales = "free") +
  theme_prism(base_size = 16) +
  theme(
    legend.position = "bottom",
    strip.text = element_text(size = 22)
  )
ggsave(filename = "fig1.pdf", width = 5.7, height = 6)
ggsave(filename = "fig1.png", width = 5.7, height = 6)
ggsave(filename = "fig1.tiff", width = 5.7, height = 6)


# fig2 --------------------------------------------------------------------

data2 <- xlsx::read.xlsx("rawData.xlsx", sheetIndex = 2)

data2$Index <- factor(
  data2$Index,
  levels = c("Systolic Pressure (mmHg)", "Diastolic Pressure (mmHg)", "Mean Arterial Pressure (mmHg)")
)
data2$Time <- factor(
  data2$Time,
  levels = c("-15", "15", "10", "30", "60", "120")
)
data2$Group <- factor(
  data2$Group,
  levels = c("Normal saline", "3 mg/kg EH", "30 mg/kg EH")
)


ggplot(data2) +
  geom_line(
    aes(Time, mean,
        color = Group,
        group = Group),
    size = 1,
    alpha = .35
  ) +
  geom_errorbar(
    aes(Time, ymin = mean -SD, ymax = mean + SD,
        color = Group,
        group = Group),
    size = 1,
    alpha = .75,
    width = .15
  ) +
  geom_point(
    aes(Time, mean,
        color = Group, shape = Group,
        group = Group),
    size = 4.8
  ) +
  labs(
    x = "time (min)",
    y = "",
  ) +
  # legend(title = "dose") +
  # guides(color = guide_legend(title.vjust = c(.5))) +
  scale_color_manual(values = myCol) +
  facet_wrap(.~Index, ncol = 1, scales = "free") +
  theme_prism(base_size = 16) +
  theme(
    legend.position = "bottom",
    strip.text = element_text(size = 22)
  )
ggsave(filename = "fig2.pdf", width = 5.8, height = 9)
ggsave(filename = "fig2.png", width = 5.8, height = 9)
ggsave(filename = "fig2.tiff", width = 5.8, height = 9)


# fig3 --------------------------------------------------------------------

data3 <- xlsx::read.xlsx("rawData.xlsx", sheetIndex = 3)

data3$Index <- factor(
  data3$Index,
  levels = c(
    "RR interval (ms)", "Heart Rate (times/minute)",
    "PR interval (ms)", "P wave (ms)",
    "QRS (ms)", "QT interval (ms)", "QTcF"
  )
)
data3$Time <- factor(
  data3$Time,
  levels = c("-15", "15", "10", "30", "60", "120")
)
data3$Group <- factor(
  data3$Group,
  levels = c("Normal saline", "3 mg/kg EH", "30 mg/kg EH")
)


ggplot(data3) +
  geom_line(
    aes(Time, mean,
        color = Group,
        group = Group),
    size = 1,
    alpha = .35
  ) +
  geom_errorbar(
    aes(Time, ymin = mean -SD, ymax = mean + SD,
        color = Group,
        group = Group),
    size = 1,
    alpha = .75,
    width = .15
  ) +
  geom_point(
    aes(Time, mean,
        color = Group, shape = Group,
        group = Group),
    size = 4.8
  ) +
  labs(
    x = "time (min)",
    y = "",
  ) +
  # legend(title = "dose") +
  # guides(color = guide_legend(title.vjust = c(.5))) +
  scale_color_manual(values = myCol) +
  facet_wrap(.~Index, ncol = 3, scales = "free") +
  theme_prism(base_size = 16) +
  theme(
    legend.position = "none",
    strip.text = element_text(size = 22)
  )
ggsave(filename = "fig3-2.pdf", width = 9, height = 9.5)
ggsave(filename = "fig3-2.png", width = 9, height = 9.5)
ggsave(filename = "fig3-2.tiff", width = 9, height = 9.5)

ggsave(filename = "fig3-3.pdf", width = 13.5, height = 8.5)
ggsave(filename = "fig3-3.png", width = 13.5, height = 8.5)
ggsave(filename = "fig3-3.tiff", width = 13.5, height = 8.5)


# fig4 --------------------------------------------------------------------

data4 <- xlsx::read.xlsx("rawData.xlsx", sheetIndex = 4)

data4$Index <- factor(
  data4$Index,
  levels = c(
    "Weight (kg)", "Temperature (℃)"
  )
)
data4$Time <- factor(
  data4$Time,
  levels = c("Day -7", "Day -1", "Day 7", "Day 14")
)
data4$NO <- factor(
  data4$NO,
  levels = c("1", "2", "3", "4", "5", "6")
)

library(ggtext)
ggplot(data4) +
  geom_line(
    aes(Time, value,
        color = Label2,
        group = Label2),
    size = 1,
    alpha = .35
  ) +
  # geom_text(
  #   aes(x = 0.6, y = label.y, label = Label2),
  #   hjust = 0,
  #   # size = 2.4
  # ) +
  geom_point(
    aes(Time, value,
        color = Label2,
        group = Label2),
    size = 4.8
  ) +
  labs(
    x = "",
    y = "",
  ) +
  # legend(title = "dose") +
  guides(color = guide_legend(nrow = 1)) +
  scale_color_npg() +
  # scale_x_(limits = c(0, 100)) +
  facet_wrap(.~Index, ncol = 2, scales = "free") +
  theme_prism(base_size = 16) +
  theme(
    legend.position = "bottom",
    strip.text = element_text(size = 22)
  )
ggsave("fig4.pdf", width = 8, height = 6.5)
ggsave("fig4.png", width = 8, height = 6.5)
ggsave("fig4.tiff", width = 8, height = 6.5)


# fig5 ---------------------------------------------------------------------

data5 <- xlsx::read.xlsx("rawData.xlsx", sheetIndex = 5)

data5$Time <- factor(
  data5$Time,
  levels = c("Before dosing", "1h after dosing", "Day 14")
)
data5$Index <- factor(
  data5$Index,
  levels = c(
    "TT (s)", "APTT (s)", "PT (s)", "Fib (g/l)"
  )
)
data5$Label <- factor(
  data5$Label,
  levels = c("No. 1", "No. 2", "No. 3", "No. 4", "No. 5", "No. 6")
)

ggplot(data5,
       aes(group = Label)) +
  geom_line(
    aes(Time, value,
        color = Label),
    size = 1,
    alpha = .35
  ) +
  geom_point(
    aes(Time, value,
        color = Label),
    size = 4.8
  ) +
  labs(
    x = "",
    y = "",
  ) +
  guides(color = guide_legend(nrow = 1)) +
  scale_color_npg() +
  facet_wrap(.~Index, ncol = 2, scales = "free") +
  theme_prism(base_size = 16) +
  theme(
    legend.position = "bottom",
    strip.text = element_text(size = 22),
    axis.text.x.bottom = element_text(
      angle = 15, hjust = .5, vjust = .85,
      size = 12
    )
  )
ggsave("fig5.pdf", width = 8, height = 8)
ggsave("fig5.png", width = 8, height = 8)
ggsave("fig5.tiff", width = 8, height = 8)


# fig6 --------------------------------------------------------------------

data6 <- xlsx::read.xlsx("rawData.xlsx", sheetIndex = 6)

data6$Time <- factor(
  data6$Time,
  levels = c("Before dosing", "Day 14")
)
data6$Index[which(data6$Index == "Heart Rate \\n(times/minute)")] <- "Heart Rate\n(times/minute)"
data6$Index <- factor(
  data6$Index,
  levels = c(
    "Heart Rate\n(times/minute)", "PR interval (s)", "QRS (s)", "QTinterval (s)"
  )
)
data6$Label <- factor(
  data6$Label,
  levels = c("No. 1", "No. 2", "No. 3", "No. 4", "No. 5", "No. 6")
)

ggplot(data6,
       aes(group = Label)) +
  geom_line(
    aes(Time, value,
        color = Label),
    size = 1,
    alpha = .35
  ) +
  geom_point(
    aes(Time, value,
        color = Label),
    size = 4.8
  ) +
  labs(
    x = "",
    y = "",
  ) +
  guides(color = guide_legend(nrow = 1)) +
  scale_color_npg() +
  facet_wrap(.~Index, ncol = 4, scales = "free") +
  theme_prism(base_size = 16) +
  theme(
    legend.position = "bottom",
    strip.text = element_text(size = 22),
    axis.text.x.bottom = element_text(
      # angle = 15, hjust = .5, vjust = .85,
      size = 12
    )
  )
ggsave("fig6-4.pdf", width = 12, height = 4.5)
ggsave("fig6-4.png", width = 12, height = 4.5)
ggsave("fig6-4.tiff", width = 12, height = 4.5)

ggplot(data6,
       aes(group = Label)) +
  geom_line(
    aes(Time, value,
        color = Label),
    size = 1,
    alpha = .35
  ) +
  geom_point(
    aes(Time, value,
        color = Label),
    size = 4.8
  ) +
  labs(
    x = "",
    y = "",
  ) +
  guides(color = guide_legend(nrow = 2)) +
  scale_color_npg() +
  facet_wrap(.~Index, ncol = 2, scales = "free") +
  theme_prism(base_size = 16) +
  theme(
    legend.position = "bottom",
    strip.text = element_text(size = 22),
    axis.text.x.bottom = element_text(
      # angle = 15, hjust = .5, vjust = .85,
      size = 12
    )
  )
ggsave("fig6-2.pdf", width = 6.5, height = 7)
ggsave("fig6-2.png", width = 6.5, height = 7)
ggsave("fig6-2.tiff", width = 6.5, height = 7)
