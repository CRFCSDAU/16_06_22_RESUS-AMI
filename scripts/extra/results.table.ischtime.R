
# This analysis adds isch.to.pci.2 to the adjusted covaraites

  source("scripts/data.R")

  x <- select(data, id,
                    arm,
                    time,
                    lvedv,
                    lvesv,
                    glvef,
                    lvmass.a,
                    latece,
                    sv.main,
                    inf.size,
                    lv.mass.ed,
                    lv.mass.es,
                    avg.thickening,
                    bsa.m2,
                    any.diabetes,
                    new.time) %>%
       mutate(lvedv.index = lvedv / bsa.m2) %>%
       mutate(lvesv.index = lvesv / bsa.m2) %>%
       mutate(lvmass.a.index = lvmass.a / bsa.m2) %>%
       mutate(lv.mass.ed.index = lv.mass.ed / bsa.m2) %>%
       mutate(lv.mass.es.index = lv.mass.es / bsa.m2) %>%
       filter(time != "Difference") %>%
       select(id, arm, time, bsa.m2, any.diabetes, new.time,
              everything())

  baseline <- filter(x, time == "Baseline") %>% distinct() %>%
              select(-time)
  names(baseline)[6:20] <- paste0(names(baseline)[6:20], "_BL")

  followup <- filter(x, time == "8 weeks") %>% distinct() %>%
              select(-time)

  names(followup)[6:20] <- paste0(names(followup)[6:20], "_FU")

  followup <- followup[, c(1, 2, 6:20)]

  x <- full_join(baseline, followup, by = c("id", "arm")) %>%
       mutate(lvedv_DF            = lvedv_FU - lvedv_BL) %>%
       mutate(lvesv_DF            = lvesv_FU - lvesv_BL) %>%
       mutate(glvef_DF            = glvef_FU - glvef_BL) %>%
       mutate(lvmass.a_DF         = lvmass.a_FU - lvmass.a_BL) %>%
       mutate(latece_DF           = latece_FU - latece_BL) %>%
       mutate(sv.main_DF          = sv.main_FU - sv.main_BL) %>%
       mutate(inf.size_DF         = inf.size_FU - inf.size_BL) %>%
       mutate(lv.mass.ed_DF       = lv.mass.ed_FU - lv.mass.ed_BL) %>%
       mutate(lv.mass.es_DF       = lv.mass.es_FU - lv.mass.es_BL) %>%
       mutate(lvedv.index_DF      = lvedv.index_FU - lvedv.index_BL) %>%
       mutate(lvesv.index_DF      = lvesv.index_FU - lvesv.index_BL) %>%
       mutate(lvmass.a.index_DF   = lvmass.a.index_FU - lvmass.a.index_BL) %>%
       mutate(lv.mass.ed.index_DF = lv.mass.ed.index_FU - lv.mass.ed.index_BL) %>%
       mutate(lv.mass.es.index_DF = lv.mass.es.index_FU - lv.mass.es.index_BL) %>%
       mutate(avg.thickening_DF   = avg.thickening_FU - avg.thickening_BL)



  dflist <- list()
  k <- 1
  for(i in levels(x$arm)){
    for(j in c("_BL", "_FU", "_DF")){
      dflist[[k]] <- filter(x, arm == i) %>%
                     select(ends_with(j)) %>%
                     mutate(arm = i) %>%
                     select(-starts_with("time")) %>%
                     mutate(time = j)

      k <- k + 1
    }
  }


# models

  lvedv      <- lm(lvedv_FU          ~ arm + bsa.m2 + any.diabetes + lvedv_BL + new.time,
                   data = x) %>% tidy()
  lvesv      <- lm(lvesv_FU          ~ arm + bsa.m2 + any.diabetes + lvesv_BL + new.time,
                   data = x) %>% tidy()
  glvef      <- lm(glvef_FU          ~ arm +          any.diabetes + glvef_BL + new.time,
                   data = x) %>% tidy()
  lvmass.a   <- lm(lvmass.a_FU       ~ arm + bsa.m2 + any.diabetes + lvmass.a_BL + new.time,
                   data = x) %>% tidy()
  latece     <- lm(latece_FU         ~ arm +          any.diabetes + latece_BL + new.time,
                   data = x) %>% tidy()
  sv.main    <- lm(sv.main_FU        ~ arm +          any.diabetes + sv.main_BL + new.time,
                   data = x) %>% tidy()
  inf.size   <- lm(inf.size_FU       ~ arm +          any.diabetes + inf.size_BL + new.time,
                   data = x) %>% tidy()
  lv.mass.ed <- lm(lv.mass.ed_FU     ~ arm + bsa.m2 + any.diabetes + lv.mass.ed_BL + new.time,
                   data = x) %>% tidy()
  lv.mass.es <- lm(lv.mass.es_FU     ~ arm + bsa.m2 + any.diabetes + lv.mass.es_BL + new.time,
                   data = x) %>% tidy()
  thickening <- lm(avg.thickening_FU ~ arm +     + any.diabetes + avg.thickening_BL + new.time,
                   data = x) %>% tidy()


# indexed
  lvedv.index      <- lm(lvedv.index_FU      ~ arm + any.diabetes + lvedv.index_BL + new.time,
                         data = x) %>% tidy()
  lvesv.index      <- lm(lvesv.index_FU      ~ arm + any.diabetes + lvesv.index_BL + new.time,
                         data = x) %>% tidy()
  lvmass.a.index   <- lm(lvmass.a.index_FU   ~ arm + any.diabetes + lvmass.a.index_BL + new.time,
                         data = x) %>% tidy()
  lv.mass.ed.index <- lm(lv.mass.ed.index_FU ~ arm + any.diabetes + lv.mass.ed.index_BL + new.time,
                         data = x) %>% tidy()
  lv.mass.es.index <- lm(lv.mass.es.index_FU ~ arm + any.diabetes + lv.mass.es.index_BL + new.time,
                          data = x) %>% tidy()

# model results

  b.p <- c(lvedv$p.value[2],
           lvesv$p.value[2],
           glvef$p.value[2],
           lvmass.a$p.value[2],
           latece$p.value[2],
           sv.main$p.value[2],
           inf.size$p.value[2],
           lv.mass.ed$p.value[2],
           lv.mass.es$p.value[2],
           thickening$p.value[2],
           lvedv.index$p.value[2],
           lvesv.index$p.value[2],
           lvmass.a.index$p.value[2],
           lv.mass.ed.index$p.value[2],
           lv.mass.es.index$p.value[2]) %>% round(3)

  b.est <- c(lvedv$estimate[2],
             lvesv$estimate[2],
             glvef$estimate[2],
             lvmass.a$estimate[2],
             latece$estimate[2],
             sv.main$estimate[2],
             inf.size$estimate[2],
             lv.mass.ed$estimate[2],
             lv.mass.es$estimate[2],
             thickening$estimate[2],
             lvedv.index$estimate[2],
             lvesv.index$estimate[2],
             lvmass.a.index$estimate[2],
             lv.mass.ed.index$estimate[2],
             lv.mass.es.index$estimate[2]) %>% round(3)

  b.ll <- c(lvedv$estimate[2] - (1.96 * lvedv$std.error[2]),
            lvesv$estimate[2] - (1.96 * lvesv$std.error[2]),
            glvef$estimate[2] - (1.96 * glvef$std.error[2]),
            lvmass.a$estimate[2] - (1.96 * lvmass.a$std.error[2]),
            latece$estimate[2] - (1.96 * latece$std.error[2]),
            sv.main$estimate[2] - (1.96 * sv.main$std.error[2]),
            inf.size$estimate[2] - (1.96 * inf.size$std.error[2]),
            lv.mass.ed$estimate[2] - (1.96 * lv.mass.ed$std.error[2]),
            lv.mass.es$estimate[2] - (1.96 * lv.mass.es$std.error[2]),
            thickening$estimate[2] - (1.96 * thickening$std.error[2]),
            lvedv.index$estimate[2] - (1.96 * lvedv.index$std.error[2]),
            lvesv.index$estimate[2] - (1.96 * lvesv.index$std.error[2]),
            lvmass.a.index$estimate[2] - (1.96 * lvmass.a.index$std.error[2]),
            lv.mass.ed.index$estimate[2] - (1.96 * lv.mass.ed.index$std.error[2]),
            lv.mass.es.index$estimate[2] - (1.96 * lv.mass.es.index$std.error[2])) %>%
          round(3)

  b.ul <- c(lvedv$estimate[2] + (1.96 * lvedv$std.error[2]),
            lvesv$estimate[2] + (1.96 * lvesv$std.error[2]),
            glvef$estimate[2] + (1.96 * glvef$std.error[2]),
            lvmass.a$estimate[2] + (1.96 * lvmass.a$std.error[2]),
            latece$estimate[2] + (1.96 * latece$std.error[2]),
            sv.main$estimate[2] + (1.96 * sv.main$std.error[2]),
            inf.size$estimate[2] + (1.96 * inf.size$std.error[2]),
            lv.mass.ed$estimate[2] + (1.96 * lv.mass.ed$std.error[2]),
            lv.mass.es$estimate[2] + (1.96 * lv.mass.es$std.error[2]),
            thickening$estimate[2] + (1.96 * thickening$std.error[2]),
            lvedv.index$estimate[2] + (1.96 * lvedv.index$std.error[2]),
            lvesv.index$estimate[2] + (1.96 * lvesv.index$std.error[2]),
            lvmass.a.index$estimate[2] + (1.96 * lvmass.a.index$std.error[2]),
            lv.mass.ed.index$estimate[2] + (1.96 * lv.mass.ed.index$std.error[2]),
            lv.mass.es.index$estimate[2] + (1.96 * lv.mass.es.index$std.error[2])) %>%
    round(3)

  b.ci <- paste0("(", b.ll, " to ", b.ul, ")")

  c.p <- c(lvedv$p.value[3],
           lvesv$p.value[3],
           glvef$p.value[3],
           lvmass.a$p.value[3],
           latece$p.value[3],
           sv.main$p.value[3],
           inf.size$p.value[3],
           lv.mass.ed$p.value[3],
           lv.mass.es$p.value[3],
           thickening$p.value[3],
           lvedv.index$p.value[3],
           lvesv.index$p.value[3],
           lvmass.a.index$p.value[3],
           lv.mass.ed.index$p.value[3],
           lv.mass.es.index$p.value[3]) %>% round(3)

  c.est <- c(lvedv$estimate[3],
             lvesv$estimate[3],
             glvef$estimate[3],
             lvmass.a$estimate[3],
             latece$estimate[3],
             sv.main$estimate[3],
             inf.size$estimate[3],
             lv.mass.ed$estimate[3],
             lv.mass.es$estimate[3],
             thickening$estimate[3],
             lvedv.index$estimate[3],
             lvesv.index$estimate[3],
             lvmass.a.index$estimate[3],
             lv.mass.ed.index$estimate[3],
             lv.mass.es.index$estimate[3]) %>% round(3)


  c.ll <- c(lvedv$estimate[3] - (1.96 * lvedv$std.error[3]),
            lvesv$estimate[3] - (1.96 * lvesv$std.error[3]),
            glvef$estimate[3] - (1.96 * glvef$std.error[3]),
            lvmass.a$estimate[3] - (1.96 * lvmass.a$std.error[3]),
            latece$estimate[3] - (1.96 * latece$std.error[3]),
            sv.main$estimate[3] - (1.96 * sv.main$std.error[3]),
            inf.size$estimate[3] - (1.96 * inf.size$std.error[3]),
            lv.mass.ed$estimate[3] - (1.96 * lv.mass.ed$std.error[3]),
            lv.mass.es$estimate[3] - (1.96 * lv.mass.es$std.error[3]),
            thickening$estimate[3] - (1.96 * thickening$std.error[3]),
            lvedv.index$estimate[3] - (1.96 * lvedv.index$std.error[3]),
            lvesv.index$estimate[3] - (1.96 * lvesv.index$std.error[3]),
            lvmass.a.index$estimate[3] - (1.96 * lvmass.a.index$std.error[3]),
            lv.mass.ed.index$estimate[3] - (1.96 * lv.mass.ed.index$std.error[3]),
            lv.mass.es.index$estimate[3] - (1.96 * lv.mass.es.index$std.error[3])) %>%
    round(3)

  c.ul <- c(lvedv$estimate[3] + (1.96 * lvedv$std.error[3]),
            lvesv$estimate[3] + (1.96 * lvesv$std.error[3]),
            glvef$estimate[3] + (1.96 * glvef$std.error[3]),
            lvmass.a$estimate[3] + (1.96 * lvmass.a$std.error[3]),
            latece$estimate[3] + (1.96 * latece$std.error[3]),
            sv.main$estimate[3] + (1.96 * sv.main$std.error[3]),
            inf.size$estimate[3] + (1.96 * inf.size$std.error[3]),
            lv.mass.ed$estimate[3] + (1.96 * lv.mass.ed$std.error[3]),
            lv.mass.es$estimate[3] + (1.96 * lv.mass.es$std.error[3]),
            thickening$estimate[3] + (1.96 * thickening$std.error[3]),
            lvedv.index$estimate[3] + (1.96 * lvedv.index$std.error[3]),
            lvesv.index$estimate[3] + (1.96 * lvesv.index$std.error[3]),
            lvmass.a.index$estimate[3] + (1.96 * lvmass.a.index$std.error[3]),
            lv.mass.ed.index$estimate[3] + (1.96 * lv.mass.ed.index$std.error[3]),
            lv.mass.es.index$estimate[3] + (1.96 * lv.mass.es.index$std.error[3])) %>%
    round(3)

  c.ci <- paste0("(", c.ll, " to ", c.ul, ")")

#
# y <- data_frame(a = names(dflist[[1]][1:15]),
#                 b = n.miss(dflist[[1]]),
#                 c = summary.1(dflist[[1]]),
#
#                 d = n.miss(dflist[[2]]),
#                 e = summary.1(dflist[[2]]),
#
#                 g = n.miss(dflist[[3]]),
#                 h = summary.1(dflist[[3]]),
#
#                 i = n.miss(dflist[[4]]),
#                 j = summary.1(dflist[[4]]),
#
#                 k = n.miss(dflist[[5]]),
#                 l = summary.1(dflist[[5]]),
#
#                 m = n.miss(dflist[[6]]),
#                 n = summary.1(dflist[[6]]),
#
#                 o = n.miss(dflist[[7]]),
#                 p = summary.1(dflist[[7]]),
#
#                 q = n.miss(dflist[[8]]),
#                 r = summary.1(dflist[[8]]),
#
#                 s = n.miss(dflist[[9]]),
#                 t = summary.1(dflist[[9]]),
#
#                 u = b.est,
#                 v = b.ci,
#                 w = b.p,
#
#                 x = c.est,
#                 y = c.ci,
#                 z = c.p)
#
#
#
#
#
#   y <- data_frame(a = names(dflist[[1]][1:15]),
#                   b = n.miss(dflist[[1]]),
#                   c = summary.1(dflist[[1]]),
#
#                   d = n.miss(dflist[[2]]),
#                   e = summary.1(dflist[[2]]),
#
#                   g = n.miss(dflist[[3]]),
#                   h = summary.1(dflist[[3]]),
#
#                   o = n.miss(dflist[[7]]),
#                   p = summary.1(dflist[[7]]),
#
#                   q = n.miss(dflist[[8]]),
#                   r = summary.1(dflist[[8]]),
#
#                   s = n.miss(dflist[[9]]),
#                   t = summary.1(dflist[[9]]),
#
#                   x = c.est,
#                   y = c.ci,
#                   z = c.p)
#
#   htmlTable(y, rnames = FALSE)


  control <- data_frame(a = names(dflist[[1]][1:15]),
                        b = n.miss(dflist[[1]]),
                        c = summary.1(dflist[[1]]),

                        d = n.miss(dflist[[2]]),
                        e = summary.1(dflist[[2]]),

                        g = n.miss(dflist[[3]]),
                        h = summary.1(dflist[[3]]),

                        i = NA,
                        j = NA,
                        k = NA)

  lowdose <- data_frame(a = names(dflist[[4]][1:15]),
                        b = n.miss(dflist[[4]]),
                        c = summary.1(dflist[[4]]),

                        d = n.miss(dflist[[5]]),
                        e = summary.1(dflist[[5]]),

                        g = n.miss(dflist[[6]]),
                        h = summary.1(dflist[[6]]),

                        i = b.est,
                        j = b.ci,
                        k = b.p)

  highdose <- data_frame(a = names(dflist[[7]][1:15]),
                         b = n.miss(dflist[[7]]),
                         c = summary.1(dflist[[7]]),

                         d = n.miss(dflist[[8]]),
                         e = summary.1(dflist[[8]]),

                         g = n.miss(dflist[[9]]),
                         h = summary.1(dflist[[9]]),

                         i = c.est,
                         j = c.ci,
                         k = c.p)

  y <- rbind(control, lowdose, highdose)


  y <- y[c(11, 26, 41,
           12, 27, 42,
           3,  18, 33,
           13, 28, 43,
           5,  20, 35,
           6,  21, 36,
           7,  22, 37,
           14, 29, 44,
           15, 30, 45,
           10, 25, 40), ]

  htmlTable(y, rnames = FALSE)



# dflist <- list()
# k <- 1
# for(i in levels(x$arm)){
#   for(j in levels(x$time)){
#     dflist[[k]] <- filter(x, arm == i & time == j, data = x)
#                    select(-id, -arm, -time, -bsa.m2, -any.diabetes)
#
#     k <- k + 1
#   }
# }
#
#
# names(dflist[[1]])
# n.miss(dflist[[1]])
# summary.1(dflist[[1]])
#
# names(dflist[[2]])
# n.miss(dflist[[2]])
# summary.1(dflist[[2]])
#
# names(dflist[[3]])
# n.miss(dflist[[3]])
# summary.1(dflist[[3]])
