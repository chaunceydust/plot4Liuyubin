library(tidyverse)
# library(rstatix)      # 统计检验
library(ggpubr)
library(ggsci)
library(patchwork)

data <- readxl::read_excel("低剂量-PD分析.xlsx") %>% 
  dplyr::select(受试者, 时间, 指标, 单位, 结果) %>% 
  dplyr::filter(!is.na(结果)) %>% 
  dplyr::filter(!结果 %in% c("LBORRES1", "未做"))
data$结果 <- as.numeric(data$结果)

index <- c(
  "FIIa", "FXa", "FXIa",
  "激活部分凝血酶原时间(APTT)", "凝血酶时间(TT)",
  "D-二聚体(D-Dimer)", "纤维蛋白原降解产物(FDP)"
)
data <- filter(data, 指标 %in% index)

data2 <- data %>% 
  pivot_wider(names_from = 时间,
              values_from = 结果)

data3 <- data2 %>% 
  group_by(指标) %>% 
  summarise(
    base = mean(`筛选/基线期`, na.rm = TRUE)
  )

data3$base[is.nan(data3$base)] <- 100

data4 <- full_join(data2, data3)

normralSize <- function(x) {
  return(x * 100 / data4$base)
}
data4 <- mutate_at(
  data4,
  c("给药前30min内", "开始静滴后 1h", "开始静滴后 2h",
    "开始静滴后24h", "开始静滴后72h", "给药结束后 2h",
    "给药结束后 6h"),
  normralSize
  # 给药前30min内 = 给药前30min内 / base
) %>% 
  select(
    -c(`筛选/基线期`, 开始静滴后120h, base)
  ) %>% 
  pivot_longer(
    colnames(.)[4:10],
    names_to = "时间",
    values_to = "结果"
  )
data4$时间 <- factor(
  data4$时间,
  levels = c(
    "给药前30min内", "开始静滴后 1h",
    "开始静滴后 2h", "开始静滴后24h", "开始静滴后72h",
    "给药结束后 2h", "给药结束后 6h"
  )
)


# data$时间 <- factor(
#   data$时间,
#   levels = c(
#     "筛选/基线期", "给药前30min内", "开始静滴后 1h",
#     "开始静滴后 2h", "开始静滴后24h", "开始静滴后72h",
#     "开始静滴后120h", "给药结束后 2h", "给药结束后 6h"
#   )
# )

# data2 <- filter(data, 指标 == index[1])
# colnames(data2) <- c("a", "time", "ii", "cc", "res")
stat.test <- data4 %>% 
  dplyr::group_by(指标) %>%
  rstatix::wilcox_test(结果 ~ 时间) %>% 
  dplyr::ungroup()

p <- vector("list", length = length(index))
q <- vector("list", length = length(index))
data <- data4
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

ggsave("test21.png",
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

ggsave("test22.png",
       width = 12, height = 12)
