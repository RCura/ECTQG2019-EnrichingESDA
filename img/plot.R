library(tidyverse)
library(ggalt)

bench_data <- read_csv("img/plot_updated2.csv")

bench_plot <- bench_data %>%
  gather(key = Operation, value = Time, `Load/Read`,Aggregation, Join) %>%
  # mutate(Operation = case_when(
  #   Operation == "Load/Read" ~ "Insertion de données",
  #   Operation == "Join" ~ "Jointure",
  #   Operation == "Aggregation" ~ "Aggrégation de données"
  # )) %>%
  mutate(System = if_else(`SQL?` == "yes",
                          true = paste0(System, "\n(SQL)"),
                          false = paste0(System, "\n(non SQL)"))) %>%
  # mutate(Operation = factor(Operation, levels = c("Insertion de données",
  #                                                 "Aggrégation de données",
  #                                                 "Jointure"))) %>%
  mutate(Time = Time + 1) %>%
  mutate(System = fct_reorder(System, Time, .fun = min))

ticks <- c(0.5,1,3,5,10,30,60,120,300,600)
ticks2 <- ticks - 1

isSQL <- c(rep("black", times = 5),
           rep("grey", times = 2),
           rep("black", times = 3))

ggplot(bench_plot) +
  aes(x = System, y = Time, color = Type, point.colour=Type) +
  geom_lollipop(point.size=2) +
  facet_wrap(~Operation, nrow = 1) +
  scale_x_discrete(position ="top") +
  scale_y_log10(breaks = ticks2, labels = ticks, limits = c(.9,500)) +
  labs(title= "Performance of various DataBase Management Systems",
       caption = "R. Cura (2018), updated from S. Pafka (2017)
       * Omnisci (CPU) and SQLite : Benchmark executed on a less powerfull computer",
       x = "DBMS",
       y = "Query time [seconds]\n (Log scale)") +
  coord_flip() +
  theme(legend.position="bottom",
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.text.y = element_text(colour = isSQL)) +
  guides(colour = guide_legend(title = "DBMS types",
                               override.aes = list(size=6),
                               title.position = "top",
                               title.hjust = .5,
                               label.position = "bottom"))
#ggsave(last_plot(), filename = "benchmark_results2.png", width = 30, height = 20, units = "cm", dpi = 150)
