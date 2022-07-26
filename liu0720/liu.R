setwd("D:/Work/others/LiuYuBin")

data <- readxl::read_excel("liuyubin.xlsx", sheet = 1,
                           col_names = TRUE)
library(ggplot2)
library(tidyverse)

data$LG <- 1.1*(data$Mean + data$SD)

data$dosing <- factor(data$dosing,
                      levels = c("Base value", "30",
                                 "60", "120"),
                      labels = c("Base value", "30 min",
                                 "60 min", "120 min"))

# data$Changes <- factor(data$Changes,
# levels = c("Changes in TT",
#            "Changes in PT",
#            "Changes in APTT",
#            "Changes in FG"))
# colnames(data)[3] <- "Changes"

# Changes in TT -----------------------------------------------------------
p_TT <- ggplot(filter(data, Changes == "Changes in TT"),
       aes(dosing, Mean,
           group = Group,
           color = Group, fill = Group,
           shape = Group)) + 
  geom_line(size = 2, color = "grey", alpha = 0.7) +
  geom_point(size = 5) +
  geom_errorbar( 
    aes(x=dosing, ymin=Mean-SD, ymax=Mean+SD),
    width=0.25, alpha=0.9, size=1.5
  ) +
  geom_text(aes(dosing, LG, label = ticks),
            size = 13) +
  guides(label = "none") +
  scale_shape_manual(values = c(21:25)) +
  scale_y_continuous(
    breaks = c(10, 15, 20),
    labels = c(10.0, 15.0, 20.0)
  ) +
  ylab(
    ""
  ) +
  xlab("") +
  ggtitle("Changes in TT(s)") +
  facet_wrap(~Group, ncol = 1) +
  theme_bw()+
  theme(
    axis.ticks.length = unit(0.4,"lines"), 
    axis.ticks = element_line(color='black'),
    axis.line = element_line(colour = "black"),
    axis.title.x = element_blank(),
    axis.title.y = element_text(
      colour = 'black', size = 28,face = "bold", vjust = 3
    ),
    axis.text.y = element_text(colour='black',size=18),
    axis.text.x=element_text(
      colour = "black",size = 15,face = "bold",
      angle = 90,hjust = 1,vjust = 0.5
    ),
    plot.margin = margin(
      t = 5,r = 20,b = 5, l = 5, unit = "pt"
    ),
    text = element_text(
      colour = "black",size = 28,face = "bold"
    ),
    legend.position = "none",
    # legend.title = element_blank(),
    # legend.key.width = unit(0.9,"cm")
  ) +
  theme(
    plot.title = element_text(
      size=28,colour = "black",face = "bold",hjust = 0.5
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

# changes in PT -----------------------------------------

p_PT <- ggplot(filter(data, Changes == "Changes in PT"),
               aes(dosing, Mean,
                   group = Group,
                   color = Group, fill = Group,
                   shape = Group)) + 
  geom_line(size = 2, color = "grey", alpha = 0.7) +
  geom_point(size = 5) +
  geom_errorbar( 
    aes(x=dosing, ymin=Mean-SD, ymax=Mean+SD),
    width=0.25, alpha=0.9, size=1.5
  ) +
  geom_text(aes(dosing, LG, label = ticks),
            size = 13) +
  guides(label = "none") +
  scale_shape_manual(values = c(21:25)) +
  ylab(
    ""
  ) +
  xlab("") +
  ggtitle("Changes in PT(s)") +
  facet_wrap(~Group, ncol = 1) +
  theme_bw()+
  theme(
    axis.ticks.length = unit(0.4,"lines"), 
    axis.ticks = element_line(color='black'),
    axis.line = element_line(colour = "black"),
    axis.title.x = element_blank(),
    axis.title.y = element_text(
      colour = 'black', size = 28,face = "bold", vjust = 3
    ),
    axis.text.y = element_text(colour='black',size=18),
    axis.text.x=element_text(
      colour = "black",size = 15,face = "bold",
      angle = 90,hjust = 1,vjust = 0.5
    ),
    plot.margin = margin(
      t = 5,r = 20,b = 5, l = 5, unit = "pt"
    ),
    text = element_text(
      colour = "black",size = 28,face = "bold"
    ),
    legend.position = "none",
    # legend.title = element_blank(),
    # legend.key.width = unit(0.9,"cm")
  ) +
  theme(
    plot.title = element_text(
      size=28,colour = "black",face = "bold",hjust = 0.5
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



# changes in APTT---------------------------------------

p_APTT <- ggplot(filter(data, Changes == "Changes in APTT"),
               aes(dosing, Mean,
                   group = Group,
                   color = Group, fill = Group,
                   shape = Group)) + 
  geom_line(size = 2, color = "grey", alpha = 0.7) +
  geom_point(size = 5) +
  geom_errorbar( 
    aes(x=dosing, ymin=Mean-SD, ymax=Mean+SD),
    width=0.25, alpha=0.9, size=1.5
  ) +
  geom_text(aes(dosing, LG, label = ticks),
            size = 13) +
  guides(label = "none") +
  scale_shape_manual(values = c(21:25)) +
  ylab(
    ""
  ) +
  xlab("") +
  ggtitle("Changes in APTT(s)") +
  facet_wrap(~Group, ncol = 1) +
  theme_bw()+
  theme(
    axis.ticks.length = unit(0.4,"lines"), 
    axis.ticks = element_line(color='black'),
    axis.line = element_line(colour = "black"),
    axis.title.x = element_blank(),
    axis.title.y = element_text(
      colour = 'black', size = 28,face = "bold", vjust = 3
    ),
    axis.text.y = element_text(colour='black',size=18),
    axis.text.x=element_text(
      colour = "black",size = 15,face = "bold",
      angle = 90,hjust = 1,vjust = 0.5
    ),
    plot.margin = margin(
      t = 5,r = 20,b = 5, l = 5, unit = "pt"
    ),
    text = element_text(
      colour = "black",size = 28,face = "bold"
    ),
    legend.position = "none",
    # legend.title = element_blank(),
    # legend.key.width = unit(0.9,"cm")
  ) +
  theme(
    plot.title = element_text(
      size=28,colour = "black",face = "bold",hjust = 0.5
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


# changes in FG -----------------------------------------

p_FG <- ggplot(filter(data, Changes == "Changes in FG"),
               aes(dosing, Mean,
                   group = Group,
                   color = Group, fill = Group,
                   shape = Group)) + 
  geom_line(size = 2, color = "grey", alpha = 0.7) +
  geom_point(size = 5) +
  geom_errorbar( 
    aes(x=dosing, ymin=Mean-SD, ymax=Mean+SD),
    width=0.25, alpha=0.9, size=1.5
  ) +
  geom_text(aes(dosing, LG, label = ticks),
            size = 13) +
  guides(label = "none") +
  scale_shape_manual(values = c(21:25)) +
  scale_y_continuous(
    breaks = c(3, 5, 7)
  ) +
  ylab(
    ""
  ) +
  xlab("") +
  ggtitle("Changes in FG(g/L)") +
  facet_wrap(~Group, ncol = 1) +
  theme_bw()+
  theme(
    axis.ticks.length = unit(0.4,"lines"), 
    axis.ticks = element_line(color='black'),
    axis.line = element_line(colour = "black"),
    axis.title.x = element_blank(),
    axis.title.y = element_text(
      colour = 'black', size = 28,face = "bold", vjust = 3
    ),
    axis.text.y = element_text(colour='black',size=18),
    axis.text.x=element_text(
      colour = "black",size = 15,face = "bold",
      angle = 90,hjust = 1,vjust = 0.5
    ),
    plot.margin = margin(
      t = 5,r = 20,b = 5, l = 5, unit = "pt"
    ),
    text = element_text(
      colour = "black",size = 28,face = "bold"
    ),
    legend.position = "none",
    # legend.title = element_blank(),
    # legend.key.width = unit(0.9,"cm")
  ) +
  theme(
    plot.title = element_text(
      size=28,colour = "black",face = "bold",hjust = 0.5
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



library(patchwork)


p1 <- p_TT + p_PT + p_APTT + p_FG + plot_layout(widths = c(1,1,1,1)) +
  plot_annotation(tag_levels = "A")
pdf("ChangesTT.pdf",height=9.5,width=17)
p1
dev.off()
# data2 -------------------------------------------------------------------

data <- readxl::read_excel("liuyubin -出血量+出血时间.xlsx", sheet = 1,
                           col_names = TRUE)


data$LG <- 1.1*(data$Mean + data$SD)

data$dosing <- factor(data$dosing,
                      levels = c("Base value", "30",
                                 "60", "120"),
                      labels = c("Base value", "30 min",
                                 "60 min", "120 min"))

# data$Changes <- factor(data$Changes,
#                        levels = c("Mucosal bleeding time",
#                                   "Wound blood loss"),
#                        labels = c("Mucosal\nbleeding time",
#                                   "Wound blood loss"))

# p_time ------------------------------------------------------------------
p_time <- ggplot(filter(data, Changes == "Mucosal bleeding time"),
            aes(dosing, Mean,
                group = Group,
                color = Group, fill = Group,
                shape = Group)) + 
  geom_line(size = 2, color = "grey", alpha = 0.7) +
  geom_point(size = 5) +
  geom_errorbar( 
    aes(x=dosing, ymin=Mean-SD, ymax=Mean+SD),
    width=0.25, alpha=0.9, size=1.5
  ) +
  geom_text(aes(dosing, LG, label = ticks),
            size = 9) +
  guides(label = "none") +
  scale_shape_manual(values = c(21:25)) +
  ggtitle("Mucosal bleeding time(s)") +
  ylab(
    ""
  ) +
  xlab("") +
  facet_wrap(~Group, ncol = 1) +
  theme_bw()+
  theme(
    axis.ticks.length = unit(0.4,"lines"), 
    axis.ticks = element_line(color='black'),
    axis.line = element_line(colour = "black"),
    axis.title.x = element_blank(),
    axis.title.y = element_text(
      colour = 'black', size = 28,face = "bold", vjust = 3
    ),
    axis.text.y = element_text(colour='black',size=18),
    axis.text.x=element_text(
      colour = "black",size = 15,face = "bold",
      angle = 90,hjust = 1,vjust = 0.5
    ),
    plot.margin = margin(
      t = 5,r = 20,b = 5, l = 5, unit = "pt"
    ),
    text = element_text(
      colour = "black",size = 28,face = "bold"
    ),
    legend.position = "none"
    # legend.position = "top",
    # legend.title = element_blank(),
    # legend.key.width = unit(0.9,"cm")
  ) +
  theme(
    plot.title = element_text(
      size=28,colour = "black",face = "bold",hjust = 0.5
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
# p_blood ------------------------------------------------------------------
p_loss <- ggplot(filter(data, Changes == "Wound blood loss"),
                 aes(dosing, Mean,
                     group = Group,
                     color = Group, fill = Group,
                     shape = Group)) + 
  geom_line(size = 2, color = "grey", alpha = 0.7) +
  geom_point(size = 5) +
  geom_errorbar( 
    aes(x=dosing, ymin=Mean-SD, ymax=Mean+SD),
    width=0.25, alpha=0.9, size=1.5
  ) +
  geom_text(aes(dosing, LG, label = ticks),
            size = 9) +
  guides(label = "none") +
  scale_shape_manual(values = c(21:25)) +
  ggtitle("Wound blood loss(mg)") +
  ylab(
    ""
  ) +
  xlab("") +
  facet_wrap(~Group, ncol = 1) +
  theme_bw()+
  theme(
    axis.ticks.length = unit(0.4,"lines"), 
    axis.ticks = element_line(color='black'),
    axis.line = element_line(colour = "black"),
    axis.title.x = element_blank(),
    axis.title.y = element_text(
      colour = 'black', size = 28,face = "bold", vjust = 3
    ),
    axis.text.y = element_text(colour='black',size=18),
    axis.text.x=element_text(
      colour = "black",size = 15,face = "bold",
      angle = 90,hjust = 1,vjust = 0.5
    ),
    plot.margin = margin(
      t = 5,r = 20,b = 5, l = 5, unit = "pt"
    ),
    text = element_text(
      colour = "black",size = 28,face = "bold"
    ),
    legend.position = "none"
    # legend.position = "top",
    # legend.title = element_blank(),
    # legend.key.width = unit(0.9,"cm")
  ) +
  theme(
    plot.title = element_text(
      size=28,colour = "black",face = "bold",hjust = 0.5
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

library(patchwork)

# p_time + p_loss + plot_layout(widths = c(4,2)) +
#   plot_annotation(tag_levels = "A")



# png(filename="liu.png",res=600,height=6600,width=12600)
# p + p2 + plot_layout(widths = c(4,2)) +
#   plot_annotation(tag_levels = "A")
# dev.off()

pdf("blood.pdf",height=9.5,width=10)
p_time + p_loss + plot_layout(widths = c(2,2)) +
  plot_annotation(tag_levels = "A")
dev.off()


# R3 ----------------------------------------------------------------------


data <- readxl::read_excel("liuyubin -动脉血栓形成频率的影响.xlsx", sheet = 1,
                           skip = 1,
                           col_names = TRUE)


data$LG <- 1.1*(data$Mean + data$SD)

data$dosing <- factor(data$dosing,
                      levels = c("Base value", "30",
                                 "60", "90", "120"),
                      labels = c("Base value", "30 min",
                                 "60 min", "90 min", "120 min"))

# data$Changes <- factor(data$Changes,
#                        levels = c("Mucosal bleeding time",
#                                   "Wound blood loss"),
#                        labels = c("Mucosal\nbleeding time",
#                                   "Wound blood loss"))

p3 <- ggplot(data,
             aes(dosing, Mean,
                 group = Group,
                 color = Group, fill = Group,
                 shape = Group)) + 
  geom_line(size = 2, color = "grey", alpha = 0.7) +
  geom_point(size = 5) +
  geom_errorbar( 
    aes(x=dosing, ymin=Mean-SD, ymax=Mean+SD),
    width=0.25, alpha=0.9, size=1.5
  ) +
  geom_text(aes(dosing, LG, label = ticks),
            size = 9) +
  guides(label = "none") +
  scale_shape_manual(values = c(21:25)) +
  scale_y_continuous(limits = c(0,15))+
  ylab(
    ""
  ) +
  xlab("") +
  ggtitle("Effect of EH\non the frequency (times /h) of\n coronary thrombosis\n in anesthetized canine") +
  facet_wrap(~Group, scales = "free_y", ncol = 1) +
  theme_bw()+
  theme(
    axis.ticks.length = unit(0.4,"lines"), 
    axis.ticks = element_line(color='black'),
    axis.line = element_line(colour = "black"),
    axis.title.x = element_blank(),
    axis.title.y = element_text(
      colour = 'black', size = 28,face = "bold", vjust = 3
    ),
    axis.text.y = element_text(colour='black',size=15),
    axis.text.x=element_text(
      colour = "black",size = 15,face = "bold",
      angle = 90,hjust = 1,vjust = 0.5
    ),
    plot.margin = margin(
      t = 5,r = 20,b = 5, l = 5, unit = "pt"
    ),
    text = element_text(
      colour = "black",size = 28,face = "bold"
    ),
    legend.position = "none"
    # legend.position = "top",
    # legend.title = element_blank(),
    # legend.key.width = unit(0.9,"cm")
  ) +
  theme(
    plot.title = element_text(
      size=28,colour = "black",face = "bold",hjust = 0.5
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

pdf("liu2.pdf",height=10,width=6)
p3
dev.off()
