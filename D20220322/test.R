setwd("D:/Work/others/LiuYuBin/D20220322")

library(tidyverse)
library(rstatix)
library(ggpubr)

rawData <- xlsx::read.xlsx("rawdata.xlsx",
                           sheetIndex = 1) %>% 
  pivot_longer(
    c("D1", "D4"),
    names_to = "INDEX",
    values_to = "RESULT"
  ) %>% 
  mutate(
    Group = factor(
      Group,
      levels = c("low dose group", "middle dose group", "high dose group")
    )
  )
colnames(rawData)[2:3] <- c("batch", "project")

#### summary ####
summ_data <- rawData %>% 
  group_by(
    Group, INDEX, project
  ) %>% 
  summarise(
    n = n(),
    MEAN = mean(RESULT),
    SD = sd(RESULT),
    SE = SD/sqrt(n)
  ) %>% 
  ungroup()
write.csv(summ_data,
          "summary_data.csv")


# t test
stat.test.data.INDEX_T <- rawData %>% 
  # filter(INDEX != "FDP") %>% 
  group_by(project, INDEX) %>% 
  rstatix::t_test(RESULT ~ Group, paired = TRUE) %>% 
  ungroup()
write.csv(stat.test.data.INDEX_T, "compare_dose_T_test.csv")

stat.test.data.INDEX_W <- rawData %>% 
  # filter(INDEX != "FDP") %>% 
  group_by(project, INDEX) %>% 
  rstatix::wilcox_test(RESULT ~ Group) %>% 
  ungroup()
write.csv(stat.test.data.INDEX_W, "compare_dose_rank_test.csv")


stat.test.data.INDEX_T2 <- rawData %>% 
  # filter(INDEX != "FDP") %>% 
  group_by(project, Group) %>% 
  rstatix::t_test(RESULT ~ INDEX, paired = TRUE) %>% 
  ungroup() %>% 
  filter(p <= .05) %>% 
  add_xy_position()
stat.test.data.INDEX_T2$y.position <- c(1.8, 2.65)
write.csv(stat.test.data.INDEX_T2, "compare_D1D4_T_paired_test.csv")

stat.test.data.INDEX_W2 <- rawData %>% 
  # filter(INDEX != "FDP") %>% 
  group_by(project, Group) %>% 
  rstatix::wilcox_test(RESULT ~ INDEX) %>% 
  ungroup()
write.csv(stat.test.data.INDEX_W2, "compare_D1D4_W_test.csv")

stat.test.data.INDEX_W2 <- rawData %>% 
  # filter(INDEX != "FDP") %>% 
  group_by(project, INDEX) %>% 
  rstatix::t_test(RESULT ~ Group) %>% 
  ungroup() %>%
  group_by(Time, INDEX) %>%
  mutate(
    batch = row_number(),
    p.scient = format(p, scientific = TRUE, digits = 2)
  ) %>% 
  ungroup() %>% 
  select(INDEX, everything())

write_csv(stat.test.data, "T_test.csv")


ggplot(summ_data,
       aes(INDEX, MEAN,
           # fill = INDEX, color = INDEX
           )) +
  geom_bar(
    aes(
      fill = INDEX, color = INDEX
    ),
    stat = "identity",
    position = "dodge",
    width = 0.7
  ) +
  geom_errorbar(
    aes(x = INDEX,
        fill = INDEX, color = INDEX,
        ymin = MEAN - SE, ymax = MEAN + SE),
    width = 0.3,
    position = "dodge",
    # color = "black",
    size = 1.2
  ) +
  stat_pvalue_manual(
    stat.test.data.INDEX_T2,
    label = "p",
    label.size = 4.2
    # x = "groups",
    # xmin = "xmin", xmax = "xmax",
    # y.position = "y.position",
    # aes()
  ) +
  facet_grid(
    project~Group,
    scales = "free_y"
  ) +
  theme_bw(
    base_size = 12
  ) +
  labs(
    x = "", y = ""
  ) +
  ggsci::scale_color_npg() +
  ggsci::scale_fill_npg() +
  theme(
    legend.position = "none",
    panel.grid = element_blank(),
    strip.background = element_rect(
      fill = "white"
    ),
    strip.text = element_text(
      face = "bold",
      size = 12
    )
  )

ggsave("HDL.pdf",
       width = 6.5, height = 6)  
