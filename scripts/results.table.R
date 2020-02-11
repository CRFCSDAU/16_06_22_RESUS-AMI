
  source("scripts/data.R")

  source("scripts/functions.R")

# Select the outcomes and covariates to adjust for

  d <- select(data, id, arm, time,  bsa.m2, any.diabetes,
              lvedv, lvesv, glvef, lvmass.a, latece,
              sv.main, inf.size, lv.mass.ed, lv.mass.es) %>%
       mutate(lvedv.index = lvedv / bsa.m2, # Create indexed outcomes
              lvesv.index = lvesv / bsa.m2,
              lvmass.a.index = lvmass.a / bsa.m2,
              lv.mass.ed.index = lv.mass.ed / bsa.m2,
              lv.mass.es.index = lv.mass.es / bsa.m2) %>%
       filter(time != "Difference") %>%
       select(id, arm, time, bsa.m2, any.diabetes,
              everything())

  t <-  names(d)[6:19]

  baseline <- filter(d, time == "Baseline") %>%
              distinct() %>%
              select(-time)


  names(baseline)[5:18] <- paste0(names(baseline)[5:18], "_BL")

  followup <- filter(d, time == "8 weeks") %>%
              distinct() %>%
              select(-time)

  names(followup)[5:18] <- paste0(names(followup)[5:18], "_FU")

  followup <- followup[, c(1, 2, 5:18)] # Lose the repeat variables

# Make differences variables for the descriptive tables

  d <- full_join(baseline, followup, by = c("id", "arm"))

  for(i in t){
    d[, paste0(i, "_DF")] <- d[, paste0(i, "_FU")] - d[, paste0(i, "_BL")]
  }


# Any missing follow up data
# miss <- select(d, id, arm, ends_with("FU"))
# miss <- miss[!(complete.cases(miss)), ]
# data <- mutate(miss, missing = "Yes") %>% select(id, missing) %>% full_join(data, by = "id")
# data$missing[is.na(data$missing)] <- "No"
# data$missing <- factor(data$missing)

# models

  models <- list(); k <- 1

  for(i in t){
    if(i %in% t[c(1, 2, 4, 8, 9)]){
      f <- as.formula(paste0(i, "_FU ~ arm + bsa.m2 + any.diabetes + ", i, "_BL"))
      }
    else{
      f <- as.formula(paste0(i, "_FU ~ arm + any.diabetes + ", i, "_BL"))
      }
    models[[k]] <- tidy(lm(f, data = d))
    k <- k + 1
    }

  models <- do.call(rbind, models) %>%
    mutate(ll = estimate - 1.96 * std.error,
           ul = estimate + 1.96 * std.error)

  models[sapply(models, is.numeric)] <-
    lapply(models[sapply(models, is.numeric)], round, 3)

  models$ci <- with(models, paste0("(", ll, " to ", ul, ")"))

# This is just the outcome values by arm and time, all split up into separate
# data frames so we can run the summary functions on them.
  dflist <- list()
  k <- 1
  for(i in levels(d$arm)){
    for(j in c("_BL", "_FU", "_DF")){
      dflist[[k]] <- filter(d, arm == i) %>%
        select(ends_with(j)) %>%
        mutate(arm = i) %>%
        select(-starts_with("time")) %>%
        mutate(time = j)

      k <- k + 1
    }
  }


  control <- data_frame(a = names(dflist[[1]][1:14]),
                        b = n.miss(dflist[[1]]),
                        c = summary.1(dflist[[1]]),

                        d = n.miss(dflist[[2]]),
                        e = summary.1(dflist[[2]]),

                        g = n.miss(dflist[[3]]),
                        h = summary.1(dflist[[3]]),

                        i = NA,
                        j = NA,
                        k = NA)

  lowdose <- data_frame(a = names(dflist[[4]][1:14]),
                        b = n.miss(dflist[[4]]),
                        c = summary.1(dflist[[4]]),

                        d = n.miss(dflist[[5]]),
                        e = summary.1(dflist[[5]]),

                        g = n.miss(dflist[[6]]),
                        h = summary.1(dflist[[6]]),

                        i = filter(models, term == "armArm B (LD)")$estimate,
                        j = filter(models, term == "armArm B (LD)")$ci,
                        k = filter(models, term == "armArm B (LD)")$p.value)

  highdose <- data_frame(a = names(dflist[[7]][1:14]),
                         b = n.miss(dflist[[7]]),
                         c = summary.1(dflist[[7]]),

                         d = n.miss(dflist[[8]]),
                         e = summary.1(dflist[[8]]),

                         g = n.miss(dflist[[9]]),
                         h = summary.1(dflist[[9]]),

                         i = filter(models, term == "armArm C (HD)")$estimate,
                         j = filter(models, term == "armArm C (HD)")$ci,
                         k = filter(models, term == "armArm C (HD)")$p.value)


  y <- rbind(control, lowdose, highdose)

  y <- y[c(1,  15, 29,
           2,  16, 30,
           3,  17, 31,
           4,  18, 32,
           5,  19, 33,
           6,  20, 34,
           7,  21, 35,
           8,  22, 36,
           9,  23, 37,
           10, 24, 38,
           11, 25, 39,
           12, 26, 40,
           13, 27, 41,
           14, 28, 42), ]

  library(stargazer)

  stargazer(y, summary = FALSE, out = "tables/table_2.htm",
            rnames = FALSE, type = "html")




