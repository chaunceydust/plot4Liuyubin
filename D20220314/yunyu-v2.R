setwd("D:/Work/others/LiuYuBin/D20220314")

library(tidyverse)
library(rstatix)


# 1.data wrap -------------------------------------------------------------
#### 1.1 reading raw data #### 
rawdata <- xlsx::read.xlsx(
  "rawdata.xlsx",
  sheetIndex = 1
)
rawdata$Result <- as.numeric(rawdata$Result)

rawdata$Group <- NA
rawdata$Group[which(str_detect(rawdata$subject, pattern = "^1"))] <- "low dose group"
rawdata$Group[which(str_detect(rawdata$subject, pattern = "^2"))] <- "middle dose group"
rawdata$Group[which(str_detect(rawdata$subject, pattern = "^3"))] <- "high dose group"

rawdata$Group <- factor(
  rawdata$Group,
  levels = c("low dose group",
             "middle dose group",
             "high dose group")
)
rawdata$Time <- factor(
  rawdata$Time,
  levels = c("30min before ivdrip", 
             "1h after ivdrip", 
             "2h after ivdrip", 
             "24h after ivdrip", 
             "72h after ivdrip", 
             "2h after the end of ivdrip", 
             "6h after the end of ivdrip")
)
rawdata$INDEX <- factor(
  rawdata$INDEX
)

#### 1.2 summary raw data ####
summ_data <- rawdata %>% 
  group_by(
    Group, INDEX, Time
  ) %>% 
  summarise(
    n = n(),
    MEAN = mean(Result),
    SD = sd(Result),
    SE = SD/sqrt(n)
  )


# check_statistic <- tibble(
#   Time = character(),
#   INDEX = character(),
#   groupA = character(),
#   groupB = character()
# )
# for (UNIa in 1:length(levels(rawdata$Time))) {
#   for (UNIb in 1:length(levels(rawdata$INDEX))) {
#     for (Ga in 1:length(levels(rawdata$Group))) {
#       for (Gb in (Ga+1):length(levels(rawdata$Group))) {
#         if (Ga != Gb) {
#           tmp_data <- rawdata %>% 
#             filter(
#               Time == levels(rawdata$Time)[UNIa] &
#                 INDEX == levels(rawdata$INDEX)[UNIb]
#             )
#           
#           tmp_a <- tmp_data %>% 
#             filter(Group == levels(rawdata$Group)[Ga]) %>% 
#             arrange(Result) %>% 
#             select(Result) %>% 
#             as_vector()
#           tmp_b <- tmp_data %>% 
#             filter(Group == levels(rawdata$Group)[Gb]) %>% 
#             arrange(Result) %>% 
#             select(Result) %>% 
#             as_vector()
#           
#           if (identical(tmp_a, tmp_b)) {
#             check_statistic[nrow(check_statistic) + 1, 1] <- levels(rawdata$Time)[UNIa] 
#             check_statistic[nrow(check_statistic), 2] <- levels(rawdata$INDEX)[UNIb] 
#             check_statistic[nrow(check_statistic), 3] <- levels(rawdata$Group)[Ga]
#             check_statistic[nrow(check_statistic), 4] <- levels(rawdata$Group)[Gb]
#           }
#         }
#       }
#     }
#   }
# }
# check_statistic$flag <- "identity"
# write_csv(check_statistic, "identity_group_info.csv")
# 
# formated_to_stat_data <- check_statistic %>% 
#   rename(Group = groupB) %>% 
#   select(-groupA) %>%
#   .[-5,] %>% 
#   right_join(rawdata) %>% 
#   filter(is.na(flag)) %>% 
#   select(-flag)

#### 1.3 statistical test ####
# t test
stat.test.data <- rawdata %>% 
  filter(INDEX != "FDP") %>% 
  group_by(Time, INDEX) %>% 
  rstatix::t_test(Result ~ Group) %>% 
  ungroup() %>%
  group_by(Time, INDEX) %>%
  mutate(
    batch = row_number(),
    p.scient = format(p, scientific = TRUE, digits = 2)
  ) %>% 
  ungroup() %>% 
  select(INDEX, everything())

write_csv(stat.test.data, "T_test.csv")

# wilcox test
stat.test.data2 <- rawdata %>% 
  filter(INDEX != "FDP") %>% 
  group_by(Time, INDEX) %>% 
  rstatix::wilcox_test(Result ~ Group) %>% 
  ungroup() %>%
  group_by(Time, INDEX) %>%
  mutate(
    batch = row_number(),
    p.scient = format(p, scientific = TRUE, digits = 2)
  ) %>% 
  ungroup() %>% 
  select(INDEX, everything())

write_csv(stat.test.data2, "wilcox_test.csv")

#### vs baseline
stat.test.data3 <- rawdata %>% 
  filter(INDEX != "FDP") %>%
  group_by(Group, INDEX) %>% 
  rstatix::t_test(Result ~ Time) %>% 
  ungroup() %>%
  group_by(Group, INDEX) %>%
  mutate(
    batch = row_number(),
    p.scient = format(p, scientific = TRUE, digits = 2)
  ) %>% 
  ungroup() %>% 
  select(INDEX, everything()) %>% 
  filter(group1 == "30min before ivdrip")

write_csv(stat.test.data3, "T_test2.csv")

stat.test.data4 <- rawdata %>% 
  filter(INDEX != "FDP") %>%
  group_by(Group, INDEX) %>% 
  rstatix::t_test(Result ~ Time) %>% 
  ungroup() %>%
  group_by(Group, INDEX) %>%
  mutate(
    batch = row_number(),
    p.scient = format(p, scientific = TRUE, digits = 2)
  ) %>% 
  ungroup() %>% 
  select(INDEX, everything()) %>% 
  filter(group1 == "30min before ivdrip")

write_csv(stat.test.data4, "wilcox_test2.csv")

#### flag
stat.flag2 <- stat.test.data3 %>% 
  select(INDEX, Group, group2, p) %>% 
  group_by(INDEX, group2) %>% 
  mutate(batch= row_number()) %>% 
  ungroup() %>% 
  select(-Group) %>% 
  pivot_wider(
    names_from = "batch",
    values_from = "p"
  ) %>% 
  bind_rows(
    tibble(
      INDEX = unique(stat.test.data3$INDEX),
      group2 = "30min before ivdrip",
      `1` = 1,
      `2` = 1,
      `3` = 1
    )
  ) %>% 
  mutate(
    Time = factor(
      group2,
      levels = c("30min before ivdrip", 
                 "1h after ivdrip", 
                 "2h after ivdrip", 
                 "24h after ivdrip", 
                 "72h after ivdrip", 
                 "2h after the end of ivdrip", 
                 "6h after the end of ivdrip")
    )
  ) %>% 
  select(INDEX, Time, `1`, `2`, `3`)

stat.flag2$f1 <- ""
stat.flag2$f1[which(stat.flag2$`1` <= .05)] <- "*"
stat.flag2$f1[which(stat.flag2$`1` <= .01)] <- "**"
stat.flag2$f1[which(stat.flag2$`1` <= .001)] <- "***"

stat.flag2$f2 <- ""
stat.flag2$f2[which(stat.flag2$`2` <= .05)] <- "#"
stat.flag2$f2[which(stat.flag2$`2` <= .01)] <- "##"
stat.flag2$f2[which(stat.flag2$`2` <= .001)] <- "###"

stat.flag2$f3 <- ""
stat.flag2$f3[which(stat.flag2$`3` <= .05)] <- "+"
stat.flag2$f3[which(stat.flag2$`3` <= .01)] <- "++"
stat.flag2$f3[which(stat.flag2$`3` <= .001)] <- "+++"

stat.flag2$flag2 <- paste0(stat.flag2$f1, stat.flag2$f2, stat.flag2$f3)
stat.flag2 <- stat.flag2 %>% 
  select(INDEX, Time, flag2)

#### flag1
stat.flag <- stat.test.data %>% 
  select(INDEX, Time, p, batch) %>% 
  pivot_wider(
    names_from = "batch",
    values_from = "p"
  )
stat.flag$f1 <- ""
stat.flag$f1[which(stat.flag$`1` <= .05)] <- "*"
stat.flag$f1[which(stat.flag$`1` <= .01)] <- "**"
stat.flag$f1[which(stat.flag$`1` <= .001)] <- "***"

stat.flag$f2 <- ""
stat.flag$f2[which(stat.flag$`2` <= .05)] <- "#"
stat.flag$f2[which(stat.flag$`2` <= .01)] <- "##"
stat.flag$f2[which(stat.flag$`2` <= .001)] <- "###"

stat.flag$f3 <- ""
stat.flag$f3[which(stat.flag$`3` <= .05)] <- "+"
stat.flag$f3[which(stat.flag$`3` <= .01)] <- "++"
stat.flag$f3[which(stat.flag$`3` <= .001)] <- "+++"

stat.flag$flag1 <- paste0(stat.flag$f1, stat.flag$f2, stat.flag$f3)
stat.flag <- stat.flag %>% 
  select(INDEX, Time, flag1)

top.data <- rawdata %>% 
  group_by(INDEX) %>% 
  summarise(
    y_max1 = max(Result)*1.1,
    y_max2 = max(Result)*1.2
  )

flag.data <- stat.flag %>% 
  full_join(stat.flag2) %>% 
  # select(INDEX, Time, flag) %>% 
  left_join(top.data)

# ff <- list()
# ff[[1]] <- c('****','***', '**', '*', ' ')
# ff[[2]] <- c('####','###', '##', '#', ' ')
# ff[[3]] <- c('++++','+++', '++', '+', ' ')
# stat.test.data$p.signif <- " "
# for (m in 1:3) {
#   flag <- which(stat.test.data$batch == m)
#   stat.test.data$p.signif[flag] <- cut(
#     stat.test.data$p[flag],
#     c(0,0.0001, 0.001, 0.01, 0.05, 1),
#     labels = ff[[m]]
#   )
# }


# stat.test <- as_tibble(matrix(NA, ncol = 14))
# stat.test <- stat.test[-1,]
# colnames(stat.test) <- c("INDEX", "Time", ".y.", "group1", "group2", "n1", "n2", "statistic", "df", "p", "p.adj", "p.adj.signif", "batch", "p.scient")
# UIndex <- unique(rawdata$INDEX)

# for (m in 1:length(UIndex)) {
#   print(m)
#   tmp.data <- rawdata %>% 
#     filter(INDEX == UIndex[m]) %>% 
#     group_by(Time) %>% 
#     rstatix::t_test(Result ~ Group) %>% 
#     ungroup() %>% 
#     group_by(Time) %>% 
#     mutate(
#       INDEX = UIndex[m],
#       batch = row_number(),
#       p.scient = format(p, scientific = TRUE, digits = 2)
#     ) %>% 
#     ungroup() %>% 
#     select(INDEX, everything())
#   stat.test <- rbind(stat.test, tmp.data)
#   
# }



# stat.test <- cc.data %>% 
#   group_by(Time, INDEX) %>%
#   rstatix::t_test(Result ~ Group) %>%  ## t_test
#   ungroup() %>% 
#   # add_significance() %>% 
#   filter(group1 == "Control" & p <= .05) %>%
#   add_xy_position(x = "group") %>% 
#   group_by(substrate) %>% 
#   dplyr::mutate(tag = row_number()) %>% 
#   ungroup() %>% 
#   left_join(stat.top) %>% 
#   mutate(
#     y.pos = top * y.flag[tag],
#     p.scient = format(p, scientific = TRUE, digits = 2),
#     custom.label = ifelse(p <= .05, p, "ns")
#   )




# library(ggsci)

# fig1 --------------------------------------------------------------------

grp_f1 <- c("APTT", "PT", "TT", "INR", "FIB", "PTA")
plot.raw.data <- rawdata %>% 
  filter(
    INDEX %in% grp_f1
  )
plot.raw.data$INDEX <- factor(
  plot.raw.data$INDEX,
  levels = grp_f1
)

plot.summ.data <- summ_data %>% 
  filter(
    INDEX %in% grp_f1
  )
plot.summ.data$INDEX <- factor(
  plot.summ.data$INDEX,
  levels = grp_f1
)

plot.flag.data <- flag.data %>% 
  filter(
    INDEX %in% grp_f1
  )
plot.flag.data$INDEX <- factor(
  plot.flag.data$INDEX,
  levels = grp_f1
)

####
size_of_line <- 1.2

fig1 <- ggplot(
  plot.raw.data,
  aes(Time, Result)
) +
  geom_rect(
    xmin = 5.5, xmax = Inf,
    ymin = -Inf, ymax = Inf,
    fill =  "#D4D4D4E6" # "#D4D4D4E6"  "#EBEBEB"
  ) +
  geom_boxplot(
    aes(
      # alpha = .5,
      fill = Group,
      color = Group
    ),
    alpha = .5
    
  ) +
  geom_line(
    data = filter(plot.summ.data, Group == "low dose group"),
    aes(Time, MEAN, color = Group, group = Group),
    size = size_of_line,
    position = position_nudge(x = -.25, y = 0)
  ) +
  geom_point(
    data = filter(plot.summ.data, Group == "low dose group"),
    aes(
      Time, MEAN, 
      # color = con
    ),
    color = "black",
    size = 4,
    position = position_nudge(x = -.25, y = 0)
  ) +
  geom_point(
    data = filter(plot.summ.data, Group == "low dose group"),
    aes(
      Time, MEAN, 
      color = Group
    ),
    # color = "black",
    size = 1,
    position = position_nudge(x = -.25, y = 0)
  )  +
  geom_line(
    data = filter(plot.summ.data, Group == "middle dose group"),
    aes(Time, MEAN, color = Group, group = Group),
    size = size_of_line,
    # position = position_nudge(x = -.25, y = 0)
  ) +
  geom_point(
    data = filter(plot.summ.data, Group == "middle dose group"),
    aes(
      Time, MEAN, 
      # color = con
    ),
    color = "black",
    size = 4,
    # position = position_nudge(x = -.25, y = 0)
  ) +
  geom_point(
    data = filter(plot.summ.data, Group == "middle dose group"),
    aes(
      Time, MEAN, 
      color = Group
    ),
    # color = "black",
    size = 1,
    # position = position_nudge(x = -.25, y = 0)
  ) +
  geom_line(
    data = filter(plot.summ.data, Group == "high dose group"),
    aes(Time, MEAN, color = Group, group = Group),
    size = size_of_line,
    position = position_nudge(x = .25, y = 0)
  ) +
  geom_point(
    data = filter(plot.summ.data, Group == "high dose group"),
    aes(
      Time, MEAN, 
      # color = con
    ),
    color = "black",
    size = 4,
    position = position_nudge(x = .25, y = 0)
  ) +
  geom_point(
    data = filter(plot.summ.data, Group == "high dose group"),
    aes(
      Time, MEAN, 
      color = Group
    ),
    # color = "black",
    size = 1,
    position = position_nudge(x = .25, y = 0)
  ) +
  geom_text(
    data = plot.flag.data,
    aes(x = Time, y = y_max1, label = flag1),
    color = "#3019FF", #("#EE2C2C") # "#FFFFFF", "#FFFFFF")
    # face = "bold", 
    size = 4.8
  ) +
  geom_text(
    data = plot.flag.data,
    aes(x = Time, y = y_max2, label = flag2),
    color = "#947F00", #("#EE2C2C") # "#FFFFFF", "#FFFFFF")
    # face = "bold", 
    size = 4.8
  ) +
  facet_wrap(
    ~INDEX,
    ncol = 2,
    scales = "free_y"
  ) +
  # scale_color_npg() +
  # scale_fill_npg() +
  scale_fill_brewer(palette = "Set1") + #按类填充颜色
  scale_color_brewer(palette = "Set1") + #按类给边框着色
  labs(
    x = "Time Series",
    y = ""
  ) +
  theme_bw(base_size = 14) +
  theme(
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.title = element_blank(),
    panel.grid = element_blank(),
    axis.text.x.bottom = element_text(
      angle = 45,
      hjust = .66, vjust = .5
    ),
    strip.text = element_text(
      face="bold", size=rel(1.2)
    ),
    strip.background = element_rect(
      fill = NA
    )
  )
fig1
  
ggsave("Fig1-v2.pdf",
       width = 12, height = 9)
ggsave("Fig1-v2.tiff",
       width = 12, height = 9)
ggsave("Fig1-v2.png",
       width = 12, height = 9)


# fig2-1 ------------------------------------------------------------------

grp_f2 <- c("D-Dimer", "FDP")
plot.raw.data <- rawdata %>% 
  filter(
    INDEX %in% grp_f2
  )
plot.raw.data$INDEX <- factor(
  plot.raw.data$INDEX,
  levels = grp_f2
)

plot.summ.data <- summ_data %>% 
  filter(
    INDEX %in% grp_f2
  )
plot.summ.data$INDEX <- factor(
  plot.summ.data$INDEX,
  levels = grp_f2
)

plot.flag.data <- flag.data %>% 
  filter(
    INDEX %in% grp_f2
  )
plot.flag.data$INDEX <- factor(
  plot.flag.data$INDEX,
  levels = grp_f2
)

####
size_of_line <- 1.2

fig2.1 <- ggplot(
  plot.raw.data,
  aes(Time, Result)
) +
  geom_rect(
    xmin = 5.5, xmax = Inf,
    ymin = -Inf, ymax = Inf,
    fill =  "#D4D4D4E6" # "#D4D4D4E6"  "#EBEBEB"
  ) +
  geom_boxplot(
    aes(
      # alpha = .5,
      fill = Group,
      color = Group
    ),
    alpha = .5
    
  ) +
  geom_line(
    data = filter(plot.summ.data, Group == "low dose group"),
    aes(Time, MEAN, color = Group, group = Group),
    size = size_of_line,
    position = position_nudge(x = -.25, y = 0)
  ) +
  geom_point(
    data = filter(plot.summ.data, Group == "low dose group"),
    aes(
      Time, MEAN, 
      # color = con
    ),
    color = "black",
    size = 4,
    position = position_nudge(x = -.25, y = 0)
  ) +
  geom_point(
    data = filter(plot.summ.data, Group == "low dose group"),
    aes(
      Time, MEAN, 
      color = Group
    ),
    # color = "black",
    size = 1,
    position = position_nudge(x = -.25, y = 0)
  )  +
  geom_line(
    data = filter(plot.summ.data, Group == "middle dose group"),
    aes(Time, MEAN, color = Group, group = Group),
    size = size_of_line,
    # position = position_nudge(x = -.25, y = 0)
  ) +
  geom_point(
    data = filter(plot.summ.data, Group == "middle dose group"),
    aes(
      Time, MEAN, 
      # color = con
    ),
    color = "black",
    size = 4,
    # position = position_nudge(x = -.25, y = 0)
  ) +
  geom_point(
    data = filter(plot.summ.data, Group == "middle dose group"),
    aes(
      Time, MEAN, 
      color = Group
    ),
    # color = "black",
    size = 1,
    # position = position_nudge(x = -.25, y = 0)
  ) +
  geom_line(
    data = filter(plot.summ.data, Group == "high dose group"),
    aes(Time, MEAN, color = Group, group = Group),
    size = size_of_line,
    position = position_nudge(x = .25, y = 0)
  ) +
  geom_point(
    data = filter(plot.summ.data, Group == "high dose group"),
    aes(
      Time, MEAN, 
      # color = con
    ),
    color = "black",
    size = 4,
    position = position_nudge(x = .25, y = 0)
  ) +
  geom_point(
    data = filter(plot.summ.data, Group == "high dose group"),
    aes(
      Time, MEAN, 
      color = Group
    ),
    # color = "black",
    size = 1,
    position = position_nudge(x = .25, y = 0)
  ) +
  geom_text(
    data = plot.flag.data,
    aes(x = Time, y = y_max1, label = flag1),
    color = "#3019FF", #("#EE2C2C") # "#FFFFFF", "#FFFFFF")
    # face = "bold", 
    size = 4.8
  ) +
  geom_text(
    data = plot.flag.data,
    aes(x = Time, y = y_max2, label = flag2),
    color = "#947F00", #("#EE2C2C") # "#FFFFFF", "#FFFFFF")
    # face = "bold", 
    size = 4.8
  ) +
  facet_wrap(
    ~INDEX,
    ncol = 2,
    scales = "free_y"
  ) +
  # scale_color_npg() +
  # scale_fill_npg() +
  scale_fill_brewer(palette = "Set1") + #按类填充颜色
  scale_color_brewer(palette = "Set1") + #按类给边框着色
  labs(
    x = "Time Series",
    y = ""
  ) +
  theme_bw(base_size = 14) +
  theme(
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.title = element_blank(),
    panel.grid = element_blank(),
    axis.text.x.bottom = element_text(
      angle = 45,
      hjust = .66, vjust = .5
    ),
    strip.text = element_text(
      face="bold", size=rel(1.2)
    ),
    strip.background = element_rect(
      fill = NA
    )
  )
fig2.1

ggsave("Fig2-1-v2.pdf",
       width = 12, height = 6)
ggsave("Fig2-1-v2.tiff",
       width = 12, height = 6)
ggsave("Fig2-1-v2.png",
       width = 12, height = 6)


# fig2-2 ------------------------------------------------------------------

grp_f2 <- c("D-Dimer", "FDP")
plot.raw.data <- rawdata %>% 
  filter(
    INDEX %in% grp_f2
  )

intersect(which(plot.raw.data$INDEX == "D-Dimer"), which(plot.raw.data$Result > 1))
intersect(which(plot.raw.data$INDEX == "FDP"), which(plot.raw.data$Result > 5))

plot.raw.data <- plot.raw.data[-c(119:120),]
plot.raw.data$INDEX <- factor(
  plot.raw.data$INDEX,
  levels = grp_f2
)

plot.summ.data <- summ_data %>% 
  filter(
    INDEX %in% grp_f2
  )
plot.summ.data$INDEX <- factor(
  plot.summ.data$INDEX,
  levels = grp_f2
)

plot.flag.data <- flag.data %>% 
  filter(
    INDEX %in% grp_f2
  )
plot.flag.data$INDEX <- factor(
  plot.flag.data$INDEX,
  levels = grp_f2
)

fig2.2 <- ggplot(
  plot.raw.data,
  aes(Time, Result)
) +
  geom_rect(
    xmin = 5.5, xmax = Inf,
    ymin = -Inf, ymax = Inf,
    fill =  "#D4D4D4E6" # "#D4D4D4E6"  "#EBEBEB"
  ) +
  geom_boxplot(
    aes(
      # alpha = .5,
      fill = Group,
      color = Group
    ),
    outlier.shape = NA,
    alpha = .5
    
  ) +
  geom_line(
    data = filter(plot.summ.data, Group == "low dose group"),
    aes(Time, MEAN, color = Group, group = Group),
    size = size_of_line,
    position = position_nudge(x = -.25, y = 0)
  ) +
  geom_point(
    data = filter(plot.summ.data, Group == "low dose group"),
    aes(
      Time, MEAN, 
      # color = con
    ),
    color = "black",
    size = 4,
    position = position_nudge(x = -.25, y = 0)
  ) +
  geom_point(
    data = filter(plot.summ.data, Group == "low dose group"),
    aes(
      Time, MEAN, 
      color = Group
    ),
    # color = "black",
    size = 1,
    position = position_nudge(x = -.25, y = 0)
  )  +
  geom_line(
    data = filter(plot.summ.data, Group == "middle dose group"),
    aes(Time, MEAN, color = Group, group = Group),
    size = size_of_line,
    # position = position_nudge(x = -.25, y = 0)
  ) +
  geom_point(
    data = filter(plot.summ.data, Group == "middle dose group"),
    aes(
      Time, MEAN, 
      # color = con
    ),
    color = "black",
    size = 4,
    # position = position_nudge(x = -.25, y = 0)
  ) +
  geom_point(
    data = filter(plot.summ.data, Group == "middle dose group"),
    aes(
      Time, MEAN, 
      color = Group
    ),
    # color = "black",
    size = 1,
    # position = position_nudge(x = -.25, y = 0)
  ) +
  geom_line(
    data = filter(plot.summ.data, Group == "high dose group"),
    aes(Time, MEAN, color = Group, group = Group),
    size = size_of_line,
    position = position_nudge(x = .25, y = 0)
  ) +
  geom_point(
    data = filter(plot.summ.data, Group == "high dose group"),
    aes(
      Time, MEAN, 
      # color = con
    ),
    color = "black",
    size = 4,
    position = position_nudge(x = .25, y = 0)
  ) +
  geom_point(
    data = filter(plot.summ.data, Group == "high dose group"),
    aes(
      Time, MEAN, 
      color = Group
    ),
    # color = "black",
    size = 1,
    position = position_nudge(x = .25, y = 0)
  ) +
  # geom_text(
  #   data = plot.flag.data,
  #   aes(x = Time, y = y_max, label = flag),
  #   # color = c("#EE2C2C") # "#FFFFFF", "#FFFFFF")
  #   # face = "bold", 
  #   size = 4.8
  # ) +
  facet_wrap(
    ~INDEX,
    ncol = 2,
    scales = "free_y"
  ) +
  # scale_color_npg() +
  # scale_fill_npg() +
  scale_fill_brewer(palette = "Set1") + #按类填充颜色
  scale_color_brewer(palette = "Set1") + #按类给边框着色
  labs(
    x = "Time Series",
    y = ""
  ) +
  theme_bw(base_size = 14) +
  theme(
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.title = element_blank(),
    panel.grid = element_blank(),
    axis.text.x.bottom = element_text(
      angle = 45,
      hjust = .66, vjust = .5
    ),
    strip.text = element_text(
      face="bold", size=rel(1.2)
    ),
    strip.background = element_rect(
      fill = NA
    )
  )
fig2.2

ggsave("Fig2-2-v2.pdf",
       width = 12, height = 6)
ggsave("Fig2-2-v2.tiff",
       width = 12, height = 6)
ggsave("Fig2-2-v2.png",
       width = 12, height = 6)



# fig3 --------------------------------------------------------------------

grp_f3 <- c("FIIa", "FXa", "FXIa")
plot.raw.data <- rawdata %>% 
  filter(
    INDEX %in% grp_f3
  )
plot.raw.data$INDEX <- factor(
  plot.raw.data$INDEX,
  levels = grp_f3
)

plot.summ.data <- summ_data %>% 
  filter(
    INDEX %in% grp_f3
  )
plot.summ.data$INDEX <- factor(
  plot.summ.data$INDEX,
  levels = grp_f3
)

plot.flag.data <- flag.data %>% 
  filter(
    INDEX %in% grp_f3
  )
plot.flag.data$INDEX <- factor(
  plot.flag.data$INDEX,
  levels = grp_f3
)

####
size_of_line <- 1.2

fig3 <- ggplot(
  plot.raw.data,
  aes(Time, Result)
) +
  geom_rect(
    xmin = 5.5, xmax = Inf,
    ymin = -Inf, ymax = Inf,
    fill =  "#D4D4D4E6" # "#D4D4D4E6"  "#EBEBEB"
  ) +
  geom_boxplot(
    aes(
      # alpha = .5,
      fill = Group,
      color = Group
    ),
    alpha = .5
    
  ) +
  geom_line(
    data = filter(plot.summ.data, Group == "low dose group"),
    aes(Time, MEAN, color = Group, group = Group),
    size = size_of_line,
    position = position_nudge(x = -.25, y = 0)
  ) +
  geom_point(
    data = filter(plot.summ.data, Group == "low dose group"),
    aes(
      Time, MEAN, 
      # color = con
    ),
    color = "black",
    size = 4,
    position = position_nudge(x = -.25, y = 0)
  ) +
  geom_point(
    data = filter(plot.summ.data, Group == "low dose group"),
    aes(
      Time, MEAN, 
      color = Group
    ),
    # color = "black",
    size = 1,
    position = position_nudge(x = -.25, y = 0)
  )  +
  geom_line(
    data = filter(plot.summ.data, Group == "middle dose group"),
    aes(Time, MEAN, color = Group, group = Group),
    size = size_of_line,
    # position = position_nudge(x = -.25, y = 0)
  ) +
  geom_point(
    data = filter(plot.summ.data, Group == "middle dose group"),
    aes(
      Time, MEAN, 
      # color = con
    ),
    color = "black",
    size = 4,
    # position = position_nudge(x = -.25, y = 0)
  ) +
  geom_point(
    data = filter(plot.summ.data, Group == "middle dose group"),
    aes(
      Time, MEAN, 
      color = Group
    ),
    # color = "black",
    size = 1,
    # position = position_nudge(x = -.25, y = 0)
  ) +
  geom_line(
    data = filter(plot.summ.data, Group == "high dose group"),
    aes(Time, MEAN, color = Group, group = Group),
    size = size_of_line,
    position = position_nudge(x = .25, y = 0)
  ) +
  geom_point(
    data = filter(plot.summ.data, Group == "high dose group"),
    aes(
      Time, MEAN, 
      # color = con
    ),
    color = "black",
    size = 4,
    position = position_nudge(x = .25, y = 0)
  ) +
  geom_point(
    data = filter(plot.summ.data, Group == "high dose group"),
    aes(
      Time, MEAN, 
      color = Group
    ),
    # color = "black",
    size = 1,
    position = position_nudge(x = .25, y = 0)
  ) +
  geom_text(
    data = plot.flag.data,
    aes(x = Time, y = y_max1, label = flag1),
    color = "#3019FF", #("#EE2C2C") # "#FFFFFF", "#FFFFFF")
    # face = "bold", 
    size = 4.8
  ) +
  geom_text(
    data = plot.flag.data,
    aes(x = Time, y = y_max2, label = flag2),
    color = "#947F00", #("#EE2C2C") # "#FFFFFF", "#FFFFFF")
    # face = "bold", 
    size = 4.8
  ) +
  facet_wrap(
    ~INDEX,
    ncol = 1,
    scales = "free_y"
  ) +
  # scale_color_npg() +
  # scale_fill_npg() +
  scale_fill_brewer(palette = "Set1") + #按类填充颜色
  scale_color_brewer(palette = "Set1") + #按类给边框着色
  labs(
    x = "Time Series",
    y = ""
  ) +
  theme_bw(base_size = 14) +
  theme(
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.title = element_blank(),
    panel.grid = element_blank(),
    axis.text.x.bottom = element_text(
      angle = 45,
      hjust = .66, vjust = .5
    ),
    strip.text = element_text(
      face="bold", size=rel(1.2)
    ),
    strip.background = element_rect(
      fill = NA
    )
  )
fig3

ggsave("Fig3-v2.pdf",
       width = 7, height = 8)
ggsave("Fig3-v2.tiff",
       width = 7, height = 8)
ggsave("Fig3-v2.png",
       width = 7, height = 8)


  
