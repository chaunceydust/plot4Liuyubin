library(tidyverse)
library(ggsci)
library(patchwork)

data <- readxl::read_excel("20220811/24例受试者心电图数据.xlsx")

data$组别 <- factor(
  data$组别,
  levels = c(
    "High dose group", "Medium dose group", "Low dose group"
  )
)
data$访视 <- factor(
  data$访视,
  levels = c(
    "D-1","D1","D2","D3","D4"
  )
)

plot.data <- data |> 
  pivot_longer(
    c("PR期间","QRS波","Qt间期","QTcF"),
    names_to = "Index",
    values_to = "value"
  )
plot.data$Index <- factor(
  plot.data$Index,
  levels = c("PR期间","QRS波","Qt间期","QTcF")
)

plot.data$value <- as.numeric(plot.data$value)
plot.data2 <- plot.data |> 
  group_by(
    组别, 访视, Index
  ) |> 
  summarise(
    Mean = mean(value),
    SD = sd(value)
  ) |> 
  ungroup()


#### 1.3 statistical test ####
# t test
stat.t.test.data_baseline <- plot.data %>% 
  group_by(组别, Index) %>% 
  rstatix::t_test(value ~ 访视) %>% 
  ungroup() %>%
  filter(group1 == "D-1") |> 
  select("组别", "Index", "group2", "p") |> 
  pivot_wider(
    names_from = 组别,
    values_from = "p"
  )
stat.t.test.data_baseline$High.mark <- ""
stat.t.test.data_baseline$High.mark[stat.t.test.data_baseline$`High dose group` <= 0.05] <- "*"
stat.t.test.data_baseline$High.mark[stat.t.test.data_baseline$`High dose group` <= 0.01] <- "**"
stat.t.test.data_baseline$High.mark[stat.t.test.data_baseline$`High dose group` <= 0.001] <- "***"

stat.t.test.data_baseline$Middle.mark <- ""
stat.t.test.data_baseline$Middle.mark[stat.t.test.data_baseline$`Medium dose group` <= 0.05] <- "#"
stat.t.test.data_baseline$Middle.mark[stat.t.test.data_baseline$`Medium dose group` <= 0.01] <- "##"
stat.t.test.data_baseline$Middle.mark[stat.t.test.data_baseline$`Medium dose group` <= 0.001] <- "###"

stat.t.test.data_baseline$Low.mark <- ""
stat.t.test.data_baseline$Low.mark[stat.t.test.data_baseline$`Low dose group` <= 0.05] <- "+"
stat.t.test.data_baseline$Low.mark[stat.t.test.data_baseline$`Low dose group` <= 0.01] <- "++"
stat.t.test.data_baseline$Low.mark[stat.t.test.data_baseline$`Low dose group` <= 0.001] <- "+++"

stat.t.test.data_baseline$P.mark <- paste0(
  stat.t.test.data_baseline$High.mark,
  stat.t.test.data_baseline$Middle.mark,
  stat.t.test.data_baseline$Low.mark
)


write_csv(stat.t.test.data_baseline, "stat.vs.baseline_T_test.csv")
stat.t.test.data_baseline <- select(
  stat.t.test.data_baseline,
  c(Index, group2, P.mark)
)

# wilcox test
stat.wilcox.test.data_baseline <- plot.data %>% 
  group_by(组别, Index) %>% 
  rstatix::wilcox_test(value ~ 访视) %>% 
  ungroup() %>%
  filter(group1 == "D-1") |> 
  select("组别", "Index", "group2", "p") |> 
  pivot_wider(
    names_from = 组别,
    values_from = "p"
  )
stat.wilcox.test.data_baseline$High.mark <- ""
stat.wilcox.test.data_baseline$High.mark[stat.wilcox.test.data_baseline$`High dose group` <= 0.05] <- "*"
stat.wilcox.test.data_baseline$High.mark[stat.wilcox.test.data_baseline$`High dose group` <= 0.01] <- "**"
stat.wilcox.test.data_baseline$High.mark[stat.wilcox.test.data_baseline$`High dose group` <= 0.001] <- "***"

stat.wilcox.test.data_baseline$Middle.mark <- ""
stat.wilcox.test.data_baseline$Middle.mark[stat.wilcox.test.data_baseline$`Medium dose group` <= 0.05] <- "#"
stat.wilcox.test.data_baseline$Middle.mark[stat.wilcox.test.data_baseline$`Medium dose group` <= 0.01] <- "##"
stat.wilcox.test.data_baseline$Middle.mark[stat.wilcox.test.data_baseline$`Medium dose group` <= 0.001] <- "###"

stat.wilcox.test.data_baseline$Low.mark <- ""
stat.wilcox.test.data_baseline$Low.mark[stat.wilcox.test.data_baseline$`Low dose group` <= 0.05] <- "+"
stat.wilcox.test.data_baseline$Low.mark[stat.wilcox.test.data_baseline$`Low dose group` <= 0.01] <- "++"
stat.wilcox.test.data_baseline$Low.mark[stat.wilcox.test.data_baseline$`Low dose group` <= 0.001] <- "+++"

stat.wilcox.test.data_baseline$P.mark <- paste0(
  stat.wilcox.test.data_baseline$High.mark,
  stat.wilcox.test.data_baseline$Middle.mark,
  stat.wilcox.test.data_baseline$Low.mark
)


write_csv(stat.wilcox.test.data_baseline, "stat.vs.baseline_wilcoxon_test.csv")
stat.wilcox.test.data_baseline <- select(
  stat.wilcox.test.data_baseline,
  c(Index, group2, P.mark)
)

#### vs group
# t test
stat.t.test.data_group <- plot.data %>% 
  group_by(访视, Index) %>% 
  rstatix::t_test(value ~ 组别) %>% 
  ungroup() %>%
  select("访视", "Index", "group1","group2", "p")
  
write_csv(stat.t.test.data_group, "stat.vs.group_T_test.csv")

# wilco test
stat.wilco.test.data_group <- plot.data %>% 
  group_by(访视, Index) %>% 
  rstatix::wilcox_test(value ~ 组别) %>% 
  ungroup() %>%
  select("访视", "Index", "group1","group2", "p")

write_csv(stat.wilco.test.data_group, "stat.vs.group_wilcon_test.csv")

#### flag
tmp <- plot.data2
tmp$top <- tmp$Mean + tmp$SD

tmp2 <- tmp |> 
  group_by(Index) |> 
  arrange(desc(top)) |> 
  slice_head(n = 1) |> 
  select(Index, top)

colnames(stat.t.test.data_baseline)[2] <- "访视"
plot.data3 <- plot.data2 |> 
  left_join(stat.t.test.data_baseline) |> 
  left_join(tmp2)

myPos <- position_dodge(.7)
# plot --------------------------------------------------------------------

p1 <- ggplot(
  data = filter(
    plot.data2,
    Index == "PR期间"
  ),
  aes(
    访视, Mean,
    color = 组别,
    group = 组别,
    shape = 组别
  )
) +
  geom_line(
    size = 1.2,
    alpha = .5,
    position = myPos
  ) +
  geom_errorbar(
    aes(访视, ymin = Mean - SD, ymax = Mean + SD),
    size = .95,   # 1.35
    width = .45,
    position = myPos
  ) +
  geom_point(
    size = 3.4,
    aes(fill = 组别, colour = 组别,
        shape = 组别),
    position = myPos
  ) +
  scale_y_continuous(
    limits = c(135, 220),
    breaks = seq(140, 220, length.out = 5)
  ) +
  scale_color_npg() +
  scale_shape_manual(
    values = c(19, 15, 17)
  ) +
  labs(
    title = "PR期间",
    x = "Day",
    y = ""
  ) +
  theme_bw(base_size = 12) +
  theme(
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    plot.margin = unit(c(.3, .3, .3, .3), "cm")
  )
p1


# 2 -----------------------------------------------------------------------

p2 <- ggplot(
  data = filter(
    plot.data3,
    Index == "QRS波"
  ),
  aes(
    访视, Mean,
    color = 组别,
    group = 组别,
    shape = 组别
  )
) +
  geom_blank(
    aes(访视, top*1.1)
  ) +
  geom_line(
    size = 1.2,
    alpha = .5,
    position = myPos
  ) +
  geom_errorbar(
    aes(访视, ymin = Mean - SD, ymax = Mean + SD),
    size = .95,   # 1.35
    width = .45,
    position = myPos
  ) +
  geom_point(
    size = 3.4,
    aes(fill = 组别, colour = 组别,
        shape = 组别),
    position = myPos
  ) +
  geom_text(
    aes(访视, top*1.01, label = P.mark),
    size = 12,
    color = "#E64B35",
    show.legend = FALSE,
    position = position_nudge(x = -0.25)
    
  ) +
  scale_y_continuous(
    limits = c(80, 120),
    breaks = seq(80, 120, length.out = 5)
  ) +
  scale_color_npg() +
  scale_shape_manual(
    values = c(19, 15, 17)
  ) +
  labs(
    title = "QRS波",
    x = "Day",
    y = ""
  ) +
  theme_bw(base_size = 12) +
  theme(
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    plot.margin = unit(c(.3, .3, .3, .3), "cm")
  )

p2

# 3 -----------------------------------------------------------------------

p3 <- ggplot(
  data = filter(
    plot.data2,
    Index == "Qt间期"
  ),
  aes(
    访视, Mean,
    color = 组别,
    group = 组别,
    shape = 组别
  )
) +
  geom_line(
    size = 1.2,
    alpha = .5,
    position = myPos
  ) +
  geom_errorbar(
    aes(访视, ymin = Mean - SD, ymax = Mean + SD),
    size = .95,   # 1.35
    width = .45,
    position = myPos
  ) +
  geom_point(
    size = 3.4,
    aes(fill = 组别, colour = 组别,
        shape = 组别),
    position = myPos
  ) +
  scale_y_continuous(
    limits = c(350, 450),
    breaks = seq(350, 450, length.out = 5)
  ) +
  scale_color_npg() +
  scale_shape_manual(
    values = c(19, 15, 17)
  ) +
  labs(
    title = "Qt间期",
    x = "Day",
    y = ""
  ) +
  theme_bw(base_size = 12) +
  theme(
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    plot.margin = unit(c(.3, .3, .3, .3), "cm")
  )
p3


# 4 -----------------------------------------------------------------------

p4 <- ggplot(
  data = filter(
    plot.data2,
    Index == "QTcF"
  ),
  aes(
    访视, Mean,
    color = 组别,
    group = 组别,
    shape = 组别
  )
) +
  geom_line(
    size = 1.2,
    alpha = .5,
    position = myPos
  ) +
  geom_errorbar(
    aes(访视, ymin = Mean - SD, ymax = Mean + SD),
    size = .95,   # 1.35
    width = .45,
    position = myPos
  ) +
  geom_point(
    size = 3.4,
    aes(fill = 组别, colour = 组别,
        shape = 组别),
    position = myPos
  ) +
  scale_y_continuous(
    limits = c(370, 460),
    breaks = seq(370, 460, length.out = 5)
  ) +
  scale_color_npg() +
  scale_shape_manual(
    values = c(19, 15, 17)
  ) +
  labs(
    title = "QTcF",
    x = "Day",
    y = ""
  ) +
  theme_bw(base_size = 12) +
  theme(
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    plot.margin = unit(c(.3, .3, .3, .3), "cm")
  )
p4


# total -------------------------------------------------------------------


myLayot <- "
AB
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

ggsave("20220811/220811.pdf",
       height = 6, width = 8)
ggsave("20220811/220811.png",
       height = 6, width = 8)
ggsave("20220811/220811.tiff",
       height = 6, width = 8)
