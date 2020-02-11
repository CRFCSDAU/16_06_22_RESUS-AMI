

  source("scripts/data.R")

# log lvmass

  d <- full_join(select(data, id, arm, time, lvmass.a) %>%
                   filter(time == "8 weeks" & arm != "Arm B (LD)") %>%
                   rename(lvmass.a_end = lvmass.a) %>%
                   select(-arm, -time) %>%
                   distinct(),
                 select(data, id, arm, time, lvmass.a) %>%
                   filter(time == "Baseline" & arm != "Arm B (LD)") %>%
                   rename(lvmass.a_bl = lvmass.a) %>%
                   select(-time) %>%
                   distinct(),
                 by = "id")

  d <- d[complete.cases(d), ]

  m1 <- lm(lvmass.a_end ~ arm + lvmass.a_bl, data = d)

  m2 <- lm(log(lvmass.a_end) ~ arm + log(lvmass.a_bl), data = d)

  df <- select(data, id, time, arm, lvmass.a) %>%
          filter(arm != "Arm B (LD)" & time != "Difference") %>%
          mutate(log_lvmass = log(lvmass.a)) %>%
          gather(type, lvmass, lvmass.a, log_lvmass) %>%
          mutate(type = factor(type, labels = c("log(LV Mass)", "LV Mass")))

  ggplot(df, aes(y = lvmass, x = time)) +
           geom_line(aes(group = id), alpha = 0.5) +
           geom_smooth(aes(x = as.numeric(time)), method = "lm", size = 2,
                       se = FALSE, color = "red") +
           facet_wrap(~type + arm, scale = "free") +
           theme_minimal() +
           ylab("LV Mass or log(LV Mass") +
           xlab("Time") +
           ggtitle("Changes in LV Mass and log(LV Mass) by treatment arm") +
           theme(panel.grid = element_blank())


  ggsave("plots/log_lines.png", width = 33.867, height = 19.05, units = "cm",
         scale = 0.6)

  ggplot(df, aes(x = lvmass, fill = time)) +
    geom_density(alpha = 0.5) +
    facet_wrap(~type + arm, scale = "free") +
    theme_minimal() +
    xlab("LV Mass or log(LV Mass)") +
    ylab("Density") +
    scale_fill_brewer("", palette = "Set1") +
    ggtitle("Pre/Post distributions of LV Mass and log(LV Mass) by treatment arm")

  ggsave("plots/log_density.png", width = 33.867, height = 19.05, units = "cm",
         scale = 0.6)



  stargazer(m1, m2, out = "tables/log.htm", rnames = FALSE, type = "html")

  ggplot(d, aes(lvmass.a_bl, lvmass.a_end, color = arm)) +
    geom_point(size = 2) +
    geom_smooth(method = "lm", se = FALSE) +
    theme_minimal() +
    xlab("LV Mass at baseline") +
    ylab("LV Mass at 8 weeks") +
    scale_color_brewer("", palette = "Set1") +
    ggtitle("ANCOVA")

  ggsave("plots/ancova.png", width = 33.867, height = 19.05, units = "cm",
         scale = 0.6)



  d$change <- d$lvmass.a_end - d$lvmass.a_bl

  ggplot(d, aes(lvmass.a_bl, change, color = arm)) +
    geom_point(size = 2) +
    geom_smooth(method = "lm", se = FALSE) +
    theme_minimal() +
    xlab("LV Mass at baseline") +
    ylab("Change in LV Mass") +
    scale_color_brewer("", palette = "Set1") +
    ggtitle("Change scores")

  ggsave("plots/change.png", width = 33.867, height = 19.05, units = "cm",
         scale = 0.6)

  m1 <- lm(lvmass.a_end ~ arm, data = d)
  m2 <- lm(change ~       arm, data = d)
  m3 <- lm(lvmass.a_end ~ arm + scale(lvmass.a_bl, scale = FALSE), data = d)
  m4 <- lm(change ~       arm + scale(lvmass.a_bl, sale = FALSE), data = d)

  stargazer(m1,  m2, m3, m4,
            out = "tables/ancova.htm",
            type = "html",
            ci = TRUE, report = "vcsp",
            digits = 2,
            covariate.labels = c("Active (vs. Placebo)",
                                 "Baseline LV Mass",
                                 "Constant"),
            column.labels = c("LV Mass", "Change score", "LV Mass + baseline", "Change + baseline"),
            dep.var.labels = rep("", 4),
            single.row = TRUE)

  ggplot(d, aes(x = lvmass.a_end, fill = arm)) +
    geom_density(alpha = 0.5) +
    theme_minimal() +
    xlab("LV Mass at 8 weeks") +
    ylab("Density") +
    scale_fill_brewer("", palette = "Set1") +
    ggtitle("Distribution of LV Mass at 8 weeks by treatment arm") +
    geom_vline(xintercept = mean(d$lvmass.a_end[d$arm == "Arm A (P)"]),
               color = brewer.pal(3, "Set1")[1], size = 2) +
    geom_vline(xintercept = mean(d$lvmass.a_end[d$arm == "Arm C (HD)"]),
               color = brewer.pal(3, "Set1")[2], size = 2)

  ggsave("plots/byarm.png", width = 33.867, height = 19.05, units = "cm",
         scale = 0.6)



