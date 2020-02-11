

  events <- select(data, id, arm, any_event, event, timetoevent2) %>%
              distinct()

  table(events$arm, events$any_event)

  filter(events, event == "Arrhythmia") %>% View()


  eventModel <- glm(any_event ~ arm, data = events, family = binomial(logit))

  or.vector.1 <- exp(eventModel$coef)
  ci.vector.1 <- exp(confint(eventModel))

  stargazer(eventModel, ci = T, digits = 2,
            out = "tables/anyevent.htm",
            single.row = T, type = "latex",
            column.labels = c(""),
            dep.var.labels = c("Any Event (Yes or No)"),
            covariate.labels = c("Arm B (low dose)",
                                 "Arm c (high dose)",
                                 "Intercept"))

  library(binom)

  x <- select(data, id, event) %>% distinct() %>%
         filter(event == "Arrhythmia") %>%
         nrow()

  n <- select(data, id) %>% distinct() %>%
         nrow()

  binom.confint(15, 47, conf.level = 0.95, methods = "exact")

  binom.confint(1, n, conf.level = 0.95, methods = "all")
  binom.confint(0, n, conf.level = 0.95, methods = "all")
  binom.confint(0, 15, conf.level = 0.95, methods = "all")
