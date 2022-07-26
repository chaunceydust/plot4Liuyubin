library(tidyverse)
library(rstatix)      # 统计检验
library(ggpubr)
library(ggsci)
library(patchwork)

data <- readxl::read_excel("低剂量-PD分析.xlsx") %>% 
  select(受试者, 时间, 指标, 单位, 结果) %>% 
  filter(!is.na(结果)) %>% 
  filter(!结果 %in% c("LBORRES1", "未做"))
data$结果 <- as.numeric(data$结果)

index <- c(
  "FIIa", "FXa", "FXIa",
  "激活部分凝血酶原时间(APTT)", "凝血酶时间(TT)",
  "D-二聚体(D-Dimer)", "纤维蛋白原降解产物(FDP)"
)
data <- filter(data, 指标 %in% index)

data$时间 <- factor(
  data$时间,
  levels = c(
    "筛选/基线期", "给药前30min内", "开始静滴后 1h",
    "开始静滴后 2h", "开始静滴后24h", "开始静滴后72h",
    "开始静滴后120h", "给药结束后 2h", "给药结束后 6h"
  )
)

data2 <- filter(data, 指标 == index[1])
colnames(data2) <- c("a", "time", "ii", "cc", "res")
stat.test <- data2 %>% 
  group_by(指标) %>%
  rstatix::t_test(结果 ~ 时间) %>% 
  ungroup()

p <- vector("list", length = length(index))
q <- vector("list", length = length(index))
for (m in 1:length(index)) {
  plot.data <- filter(data, 指标 == index[m])
  
  ggplot(plot.data,
         aes(时间, 结果)) +
    stat_boxplot(
      geom = "errorbar",
      aes(color = 时间),
      width = .2
    ) +
    geom_boxplot(
      aes(color = 时间)
    ) +
    geom_jitter(
      aes(color = 时间, fill = 时间),
      width = .02,
      size = 1
    ) +
    geom_line(
      aes(group = 受试者),
      linetype = "dashed",
      alpha = .8,
      size = .2
    ) +
    scale_color_npg() +
    scale_fill_npg() +
    labs(
      y = unique(plot.data$指标)
    ) +
    theme_bw(base_size = 15) +
    theme(
      axis.text.x.bottom = element_blank()
    ) +
    guides(color = guide_legend(nrow = 4)) -> p[[m]]
  
  ggplot(plot.data,
         aes(时间, 结果)) +
    stat_summary(
      fun.data = mean_se,
      geom = "errorbar",
      size = .75,
      width = .35,
      aes(color = 时间)
    ) +
    stat_summary(
      fun = mean,
      geom = "bar",
      # size = 3.4,
      width = 0.75,
      aes(fill = 时间, colour = 时间)
    ) +
    scale_color_npg() +
    scale_fill_npg() +
    labs(
      y = unique(plot.data$指标)
    ) +
    theme_bw(base_size = 15) +
    theme(
      axis.text.x.bottom = element_blank()
    ) +
    guides(color = guide_legend(nrow = 4)) -> q[[m]]
    
}


mydesn <- "
ABC
DE#
FG#"

p[[1]] + p[[2]] + p[[3]] + 
  p[[4]] + p[[5]] +
  p[[6]] + p[[7]] +
  plot_annotation(tag_levels = "A",
                  caption = "") +
  plot_layout(guides = "collect",
              design = mydesn) &
  theme(legend.position='bottom',
        legend.title = element_blank(),
        legend.direction = "horizontal",
        plot.caption = element_text(size = 16))

ggsave("test.png",
       width = 12, height = 12)

q[[1]] + q[[2]] + q[[3]] + 
  q[[4]] + q[[5]] +
  q[[6]] + q[[7]] +
  plot_annotation(tag_levels = "A",
                  caption = "") +
  plot_layout(guides = "collect",
              design = mydesn) &
  theme(legend.position='bottom',
        legend.title = element_blank(),
        legend.direction = "horizontal",
        plot.caption = element_text(size = 16))

ggsave("test2.png",
       width = 12, height = 12)
