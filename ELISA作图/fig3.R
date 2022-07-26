
library(tidyverse)
library(rstatix)
library(magrittr)
library(ggpubr)
# library(ggsignif)
# library(ggtext)
library(ggsci)
library(patchwork)

data1 <- read_csv("data3_drug_time.csv")

data1.long <- data1 %>% 
  pivot_longer(colnames(data1)[3:ncol(data1)],
               names_to = "sample",
               values_to = "concentration")

data1.long$time <- factor(data1.long$time,
                          levels = c("control",
                                     "control(7d)",
                                     "iv(0h)",
                                     "iv(1h)",
                                     "iv(2h)",
                                     "iv(4h)")
                          )

stat.top_all <- data1.long %>% 
  group_by(group, time) %>% 
  summarise(
    myMean = mean(concentration),
    mySE = sd(concentration) / sqrt(length(concentration)),
    max = myMean + mySE
  ) %>% 
  ungroup() 

stat.top <- stat.top_all %>% 
  group_by(group) %>% 
  summarise(top = max(max))

# 2. static ---------------------------------------------------------------

y.flag <- seq(1.1, 30, by = .2)[1:(length(unique(data1.long$time))+1)]

stat.test <- data1.long %>% 
  group_by(group) %>% 
  rstatix::wilcox_test(concentration ~ time) %>%  ## t_test
  ungroup() %>% 
  # add_significance() %>% 
  filter(group1 == "control" & p <= .05) %>%
  add_xy_position(x = "time") %>% 
  group_by(group) %>% 
  dplyr::mutate(tag = row_number()) %>% 
  ungroup() %>% 
  left_join(stat.top) %>% 
  mutate(
    y.pos = top * y.flag[tag],
    p.scient = format(p, scientific = TRUE, digits = 2),
    custom.label = ifelse(p <= .05, p, "ns")
  )

stat.test %>% 
  write_csv(., paste0("pvalue_significance.csv"))

# 3. plot -----------------------------------------------------------------
cc <- data1.long
cc %<>% left_join(stat.top)
p <- vector("list", length = 3)
for (n in 1:length(unique(cc$group))) {
  pp <- unique(cc$group)[n]
  plot.data <- filter(cc, group == pp)
  stat.data <- filter(stat.test, group == pp)
  # sig.data <- filter(sigdata, group == pp)
  
  ggplot(plot.data,
         aes(time, concentration)) +
    geom_point(
      aes(time, 1.5*top),
      colour = "white"
    ) +
    stat_summary(
      fun.data = mean_se,
      geom = "errorbar",
      size = .75,
      width = .35,
      aes(color = time)
    ) +
    stat_summary(
      fun = mean,
      geom = "bar",
      # size = 3.4,
      width = 0.75,
      aes(fill = time, colour = time)
    ) +
    # geom_text(
    #   data = sig.data,
    #   aes(group, 1.2*max, label = p.signif),
    #   color = "red",
    #   size = 6
    #   
    # ) +
    labs(
      title = unique(plot.data$group),
      y = ""
    ) +
    theme_classic(base_size = 16) +
    theme(
      strip.text = element_text(face="bold", size=rel(.9)),
      strip.background = element_rect(
        fill="white", colour="white", size=1
      ),
      panel.spacing.y = unit(3, 'mm'),
      plot.title = element_text(vjust = .5, hjust = .5),
      axis.title.x = element_blank(),
      axis.title.y = element_text(),
      # axis.line.x.bottom = element_blank(),
      axis.text.x.bottom = element_blank(),
      axis.ticks.x.bottom = element_blank()
    ) +
    stat_pvalue_manual(stat.data, label = "p",
                       y.position = "y.pos",
                       label.size = 3.8,
                       ) +
    scale_y_continuous(
      # n.breaks = 3,
      expand = c(0, 0)
    ) +
    scale_color_npg() +
    scale_fill_npg() +
    guides(color = guide_legend(nrow = 1)) -> p[[n]]
}


# 4. plot together --------------------------------------------------------
mydesign <- "
AB
CD"

p[[1]] + p[[2]] + p[[3]] + p[[4]] +
  plot_annotation(tag_levels = "A") +
  plot_layout(guides = "collect",
              design = mydesign) &
  theme(legend.position='bottom',
        legend.title = element_blank(),
        legend.direction = "horizontal") 

ggsave("fig3.pdf",
       width = 8, height = 6)
ggsave("fig3.png",
       width = 8, height = 6)
ggsave("fig3.tiff",
       width = 8, height = 6)
