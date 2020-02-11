

# source("scripts/data.R")

  filter(data, time != "Difference") %>%
    select(glvef, arm, time, id, starts_with("demo")) %>%
    mutate(arm = factor(arm, labels = c("Placebo", "LD-IGF1 1.5ng",
                                                "LD-IGF1 15ng"))) %>%

  ggplot(aes(y = glvef, shape = arm)) +
    geom_violin(aes(x = as.numeric(time), group = time),
                color = "grey90", fill = "grey90", width = .5) +
    geom_line(aes(group = id,
                  x = as.numeric(time)),
              alpha = 0.7, linetype = "dashed") +
    facet_wrap(~arm) +
    theme_base() +
    scale_x_continuous(breaks = c(1, 2),
                       labels = c("Baseline", "8 weeks")) +
    geom_beeswarm(aes(group = id, x = as.numeric(time))) +
    geom_smooth(aes(x = as.numeric(time)),
                method = "lm", se = FALSE, size = 2, color = "black") +
    geom_pointrange(data = filter(plot, arm == "Placebo" & time == "Baseline"),
                    aes(x = 0.5,
                        y =    mean(glvef, na.rm = TRUE),
                        ymax = mean(glvef, na.rm = TRUE) +
                               sd(glvef, na.rm = TRUE),
                        ymin = mean(glvef, na.rm = TRUE) -
                               sd(glvef, na.rm = TRUE))) +
    geom_pointrange(data = filter(plot, arm == "Placebo" & time == "8 weeks"),
                    aes(x = 2.5,
                        y =    mean(glvef, na.rm = TRUE),
                        ymax = mean(glvef, na.rm = TRUE) +
                          sd(glvef, na.rm = TRUE),
                        ymin = mean(glvef, na.rm = TRUE) -
                          sd(glvef, na.rm = TRUE))) +
    geom_pointrange(data = filter(plot, arm == "LD-IGF1 1.5ng" & time == "Baseline"),
                    aes(x = 0.5,
                        y =    mean(glvef, na.rm = TRUE),
                        ymax = mean(glvef, na.rm = TRUE) +
                          sd(glvef, na.rm = TRUE),
                        ymin = mean(glvef, na.rm = TRUE) -
                          sd(glvef, na.rm = TRUE))) +
    geom_pointrange(data = filter(plot, arm == "LD-IGF1 1.5ng" & time == "8 weeks"),
                    aes(x = 2.5,
                        y =    mean(glvef, na.rm = TRUE),
                        ymax = mean(glvef, na.rm = TRUE) +
                          sd(glvef, na.rm = TRUE),
                        ymin = mean(glvef, na.rm = TRUE) -
                          sd(glvef, na.rm = TRUE))) +
    geom_pointrange(data = filter(plot, arm == "LD-IGF1 15ng" & time == "Baseline"),
                    aes(x = 0.5,
                        y =    mean(glvef, na.rm = TRUE),
                        ymax = mean(glvef, na.rm = TRUE) +
                          sd(glvef, na.rm = TRUE),
                        ymin = mean(glvef, na.rm = TRUE) -
                          sd(glvef, na.rm = TRUE))) +
    geom_pointrange(data = filter(plot, arm == "LD-IGF1 15ng" & time == "8 weeks"),
                    aes(x = 2.5,
                        y =    mean(glvef, na.rm = TRUE),
                        ymax = mean(glvef, na.rm = TRUE) +
                          sd(glvef, na.rm = TRUE),
                        ymin = mean(glvef, na.rm = TRUE) -
                          sd(glvef, na.rm = TRUE))) +
  # scale_color_brewer(guide = FALSE, palette = "Set1") +
  # scale_fill_manual(guide  = FALSE, palette = "Set1") +
    scale_shape(guide = FALSE) +
    xlab("")+
    ylab("GLVEF (%)") +
    ylim(0, 75) +
    theme(panel.margin = unit(2, "lines"),
          panel.border = element_rect(color = "white"),
          axis.text.y = element_text(size = 16),
          strip.text = element_text(size = 16))

  ggsave("plots/figure.1.png", height = 19.05, width = 33.86, units = "cm",
         scale = 0.6)



