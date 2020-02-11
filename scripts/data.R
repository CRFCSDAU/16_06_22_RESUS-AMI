
# source("scripts/functions.R")

  library(readxl)
  library(tidyverse)

# Data ####

# Outcomes ####

# Data were recived from Noel Caplice on June 22, 2016 via thumb drive.

# 17Jun2016_CMRI_ECHO DATABASE_UNBLINDED.xlsx - the study data
# Baseline Characteristics RESUS AMI-UNBLINDED.xls - baseline data

# Read in the study results data. These are the outcomes, in long format.

  study <- read_excel("data/17Jun2016_CMRI_ECHO DATABASE_UNBLINDED.xlsx",
                      na = "_")

  study <- study[rowSums(is.na(study)) != ncol(study),] # Remove blank rows

  study.labels <- names(study)

  names(study)[1:3] <- c("id", "arm", "time")
  names(study)[9] <- "LV Mass (g) B"

  hard.names <- names(study)

  easy.names <- c("id", "arm", "time", "glvef", "latece", "lvmass.a",
                  "inf.size", "v.edema", "lvmass.b", "atrisk", "lvedv", "lvesv",
                  "msi.a", "msi.b")

  names(study) <- easy.names

# Better names for time

  study$time[study$time == "BL"] <- "Baseline"
  study$time[study$time == "8" ] <- "8 weeks"
  study$time[study$time == "DIFF" ] <- "Difference"

  study$time <- factor(study$time)
  study$time <- relevel(study$time, "Baseline")


# Study arm

# The atudy arm was unblinded to be July 7, 2016 in the presense of Noel and Joe

  study$arm <- factor(study$arm, labels = c("Arm A (P)",
                                            "Arm B (LD)",
                                            "Arm C (HD)"))

# Baseline ####

# These data were originally saved as xls (see above). I opened the file and
# saved as xlsx and named the file file "baseline".

  base <- read_excel("data/baseline.xlsx", na = "-99")

  names(base) <- tolower(names(base))
  names(base) <- make.names(names(base), unique = TRUE)

# plot(base[[1]], base[[2]])

  base <- base[, -2]

  base <- base[rowSums(is.na(base)) != ncol(base),] # Remove blank rows


# data <- select(base, id = subjectid, starts_with("demo"), starts_with("pre"),
#                contains("hx"), starts_with("bl")) %>%
#         full_join(study, by = "id")

# Combine them

  data <- select(base, id = subjectid, starts_with("demo"), starts_with("pre"),
                 contains("hx"), starts_with("bl"), everything()) %>%
          full_join(study, by = "id")

  rm(study, base)



# Additional data ####

# LV mass and surface area ####

  lvdata <- read_excel("data/LV Mass (ED&ES) BSA.xlsx")

  lvdata <- lvdata[rowSums(is.na(lvdata)) != ncol(lvdata),] # Remove blank rows

  lvdata <- lvdata[, unlist(lapply(lvdata, function(x) !all(is.na(x))))]

  names(lvdata) <- c("id", "time", "lv.mass.ed", "lv.mass.es", "bsa.m2")

  lvdata$time[lvdata$time == "BL"] <- "Baseline"
  lvdata$time[lvdata$time == "8" ] <- "8 weeks"
  lvdata$time[lvdata$time == "DIFF" ] <- "Difference"

  lvdata$time <- factor(lvdata$time)
  lvdata$time <- relevel(lvdata$time, "Baseline")

  lvdata <- filter(lvdata, !(is.na(lv.mass.ed) &  is.na(lv.mass.es) &
                           is.na(bsa.m2)))

# lvdata$time <- droplevels(lvdata$time)

  data <- full_join(data, select(lvdata, id, time, lv.mass.ed, lv.mass.es),
                    by = c("id", "time"))

  data <- full_join(data, select(lvdata, id, bsa.m2) %>% filter(!is.na(bsa.m2)),
                    by = c("id"))

  rm(lvdata)

# Stroke volume data ####

  svdata <- read_excel("data/Jul72016StrokeVolRESUSAMI.xlsx",
                       sheet = 2, skip = 1 )

  svdata <- svdata[, unlist(lapply(svdata, function(x) !all(is.na(x))))] # blank cols
  svdata <- svdata[rowSums(is.na(svdata)) != ncol(svdata),] # Remove blank rows

  names(svdata) <- c("id", "time", "sv.main")

# svdata <- filter(svdata, time != "DIFF")

  svdata$sv.main <-  as.numeric(svdata$sv.main) %>% round(2)

  svdata$time[svdata$time == "BL"] <- "Baseline"
  svdata$time[svdata$time == "8" ] <- "8 weeks"
  svdata$time[svdata$time == "DIFF" ] <- "Difference"


  svdata$time <- factor(svdata$time)
  svdata$time <- relevel(svdata$time, "Baseline")

  data <- full_join(data, svdata, by = c("id", "time")) %>%
    mutate(sv.main = as.numeric(sv.main))

  rm(svdata)

# Thickening ####

#   thicg <- read_excel("data/09_08_2016_CMRI_ECHO DATABASE.xlsx", sheet = 2)
#
#   thicg <- thicg[rowSums(is.na(thicg)) != ncol(thicg),] # Remove blank rows
#
#   thicg <- thicg[, unlist(lapply(thicg, function(x) !all(is.na(x))))] # blank cols
#
#   names(thicg) <- c("id", "time", "thicg01", "thicg02", "thicg05", "thicg06",
#                     "thicg07", "thicg08", "thicg11", "thicg12", "thicg13",
#                     "thicg14", "thicg16",
#                     "sum.thickening", "mean.thickening" )
#
# # thicg <- filter(thicg, time != "DIFF")
#
#   thicg$time[thicg$time == "BL"] <- "Baseline"
#   thicg$time[thicg$time == "8" ] <- "8 weeks"
#   thicg$time[thicg$time == "DIFF" ] <- "Difference"
#
#
#   thicg$time <- factor(thicg$time)
#   thicg$time <- relevel(thicg$time, "Baseline")
#
#   thicg$avg.thickening <- (thicg$thicg01 + thicg$thicg02 + thicg$thicg07 +
#                            thicg$thicg08 + thicg$thicg13 + thicg$thicg14) / 6
#
# # Checking calculations in spreadsheet
# # plot(rowSums(select(thicg, starts_with("seg")), na.rm = TRUE),
# #      thicg$sum.thickening)
#
# # plot(rowMeans(select(thicg, starts_with("seg")), na.rm = TRUE),
# #      thicg$mean.thickening)
#
#   data <- full_join(data, thicg, by = c("id", "time"))
#
#   rm(thicg)

# select(thicg, id, time, seg01, seg02, seg07, seg08, seg13, seg14) %>%
# gather(seg, thickening, starts_with("seg")) %>%
# ggplot(aes(x = thickening, fill = time, color = time)) +
#   geom_bkde(alpha = 0.5) +
#   geom_rug() +
#   facet_wrap(~ seg, ncol = 2) +
#   theme_base() +
#   scale_fill_viridis(discrete = TRUE) +
#   scale_color_viridis(discrete = TRUE) +
#   xlab("Thickening (%)") +
#   ylab("Probability Density")
#
# ggsave("plots/thickening.png", height = 6, width = 6 * 1.67, units = "in")


# Thickness ####
#
#   thick <- read_excel("data/09_08_2016_CMRI_ECHO DATABASE.xlsx", sheet = 4)
#
#   thick <- thick[rowSums(is.na(thick)) != ncol(thick),] # Remove blank rows
#
#   thick <- thick[, unlist(lapply(thick, function(x) !all(is.na(x))))] # blank cols
#
#   names(thick) <- c("id", "time", "thick01", "thick02", "thick07", "thick08",
#                     "thick13", "thick14")
#
# # thick <- filter(thick, time != "DIFF")
#
#   thick$time[thick$time == "BL"] <- "Baseline"
#   thick$time[thick$time == "8" ] <- "8 weeks"
#   thick$time[thick$time == "DIFF" ] <- "Difference"
#
#
#   thick$time <- factor(thick$time)
#   thick$time <- relevel(thick$time, "Baseline")
#
#   data <- full_join(data, thick, by = c("id", "time"))
#
#   rm(thick)

# Isch to PCI ####

# PCI data ####

  pci <- read_excel("data/PCI to end drug Admin.xls")[, -c(4:6)][-48, ]

  names(pci) <- c("id", "pcistarttodrugstart.min", "drugadminend.min",
                  "icshtoimp.min")

  data <- full_join(data, pci, by = "id")

  rm(pci)

# table(is.na(data$date_onset_ischemia))
  data$ischemia.time <-
    paste(data$date_onset_ischemia,
          gsub("1899-12-31 ", "", data$time_onset_ischemia)) %>%
    as.POSIXct(format="%Y-%m-%d %H:%M:%S")

# person 114 had their missing time entered by hand, based on a file sent to
# me on March 3, 2017

data$pci.time <-
  paste(data$date_procedure,
        gsub("1899-12-31 ", "", data$time_pci_start)) %>%
  as.POSIXct(format="%Y-%m-%d %H:%M:%S")

  data$isch.to.pci <- data$pci.time - data$ischemia.time
  data$isch.to.pci.A <- as.numeric(difftime(data$pci.time, data$ischemia.time,
                                            units = "mins"))

# table(is.na(data$isch.to.pci.A ))

# with(data, plot(isch.to.pci, isch.to.pci.A)) #Perfect

  data$isch.to.pci.2 <- data$icshtoimp.min - data$pcistarttodrugstart.min

# with(data, plot(isch.to.pci.2, isch.to.pci.A)) # NO

# isch.to.pci.A # minutes - This is in the table 1. From main data
# isch.to.pci.2 # ??? From different data, SHould be minutes, but it isn't in
# the arm 2

# ggplot(data, aes(y = isch.to.pci.A, x = arm)) +
#   geom_violin() +
#   geom_beeswarm() # Arm 2 has come out there values
#
# ggplot(data, aes(y = isch.to.pci.2, x = arm)) +
#   geom_violin() +
#   geom_beeswarm() # Arm 2 has come out there values

  new.time <- read_excel("data/new.time.xls")[, c(2, 8)]
  names(new.time) <- c("id", "new.time")

  data <- full_join(data, new.time, by = "id")

  rm(new.time)

# ggplot(data, aes(y = new.time, x = arm)) +
#   geom_violin() +
#   geom_beeswarm() # matches isch.to.pci.2, explanation for extreme times

# test <-  select(data, id, arm, isch.to.pci, isch.to.pci.2, new.time) %>%
#          distinct() %>%
#          mutate(isch.to.pci = as.numeric(isch.to.pci)) %>%
#          mutate(isch.to.pci.2 = isch.to.pci.2 / 60,
#                 diff = isch.to.pci - isch.to.pci.2)
#
# group_by(test, arm) %>%
#   summarise(mean1 = mean(isch.to.pci, na.rm = TRUE),
#             min1  = min (isch.to.pci, na.rm = TRUE),
#             max1  = max (isch.to.pci, na.rm = TRUE),
#             mean2 = mean(isch.to.pci.2, na.rm = TRUE),
#             min2  = min (isch.to.pci.2, na.rm = TRUE),
#             max2  = max (isch.to.pci.2, na.rm = TRUE),
#             mean3 = mean(new.time, na.rm = TRUE),
#             min3  = min (new.time, na.rm = TRUE),
#             max3  = max (new.time, na.rm = TRUE)) %>%
#   View() # Are the unis all equal to ~ 60? NO

#
#
#   test2 <- gather(test, type, val, -arm, -id, - diff) %>%
#            filter(!is.na(id))
#
#
#   ggplot(test2, aes(y = val, x = arm, label = id)) +
#     facet_wrap(~type, scales = "free_y") +
#     geom_violin() +
#     geom_label_repel(data = filter(test2, id %in% c(117, 204, 106, 144, 114))) +
#     geom_beeswarm() +
#     theme_minimal()
#
#   ggsave("plots/ischtopci_arm.png") # The outliers are different people
#
#
#
#   ggplot(test2, aes(y = val, x = type, label = id, group = id, color = diff)) +
#     facet_wrap(~ arm, scales = "free_y", nrow = 1) +
#     geom_line() +
#     scale_color_viridis(guide = FALSE) +
#     geom_label_repel(data = filter(test2, id %in% c(117, 204, 106, 144, 114))) +
#     theme_minimal()
#
#   ggsave("plots/ischtopci_arm.png") # The outliers are different people
#
#
#
#   ggplot(data, aes(x = as.numeric(isch.to.pci), y = isch.to.pci.2, color = arm)) +
#     geom_jitter() +
#     geom_abline(intercept = 0, slope = 60)

# Should be 60

# table(is.na(data$isch.to.pci.2))
# table(is.na(data$isch.to.pci)) # 1 missing
# table(is.na(data$pci.time)) #
# table(is.na(data$ischemia.time)) # 1 missing
#
# data$isch.to.pci.2[is.na(data$isch.to.pci)] # 2956 2956 2956
#
# data$isch.to.pci.2[data$isch.to.pci > 100] # 530 530 530

# Ok, so the person with 100 hour isch.to.pci, has isch.to.pci.2 time of
# 530 minutes (8.83 hours), so isch.to.pci.2 seems more reasonable. Plus
# isch.to.pci has 1 missing (patient 114) due to missing onset time.

# However, we still have 1 person with isch.to.pci.2 = 2956 2956 2956, and these
# happen to be those missing isch.to.pci. This is 2956/60 = 49 hours, assuming
# its minutes.



# Peak troponin ####

  troponin <- read_excel("data/troponin.xlsx")

  names(troponin) <- c("id", "peak.tn.post.pci.new")

  data <- full_join(data, troponin, by = c("id"))

  rm(troponin)

  data$peak.tn.post.pci.new[grepl("236.00", data$peak.tn.post.pci.new)] <- "236"

  data$peak.tn.post.pci.new <-
    as.numeric(gsub("[[:punct:]]", "", data$peak.tn.post.pci.new))

# with(data, plot(peak.tn.post.pci.new, peak.tn.post.pci)) # Perfect match

# ggplot(data, aes(y = peak.tn.post.pci.new)) +
#   geom_violin(aes(x = 1)) +
#   geom_beeswarm(aes(x = 1))

# Balloon times ####

  balloon <- read_excel("data/Last balloon inflation time to start IMP administration time.xlsx")

  names(balloon)[1:5] <- c("id", "date_imp", "balloon_time", "drug_time", "balloon_diff")

  balloon <- balloon[1:5]

  data <- full_join(balloon, data, by = "id")

  balloon <- distinct(select(data, id, arm, balloon_diff))

# ggplot(balloon, aes(x = balloon_diff)) +
#   geom_bar() +
#   facet_wrap(~arm, ncol = 1) +
#   theme_minimal() +
#   xlab("Time between final balloon inflation and drug administration (minutes)") +
#   ylab("Number of patients")
#
# ggsave("plots/balloon.png", height = 19.05, width = 33.86, units = "cm",
#        scale = 0.6)

# summary(lm(balloon_diff ~ arm, data = balloon))
#
# anova(lm(balloon_diff ~ arm, data = balloon))
#
# anova(lm(balloon_diff ~ arm, data = balloon), lm(balloon_diff ~ 1, data = balloon))
#
# group_by(balloon, arm) %>% summarise(mean = mean(balloon_diff))
#
# mean(balloon$balloon_diff)

  rm(balloon)


# Events ####

# events <- read_excel("data/events.xlsx")
#
# names(events) <- c("id", "group", "ce", "drugdate", "event", "event_date",
#                    "event_dead")
#
# events$any_event <- "Yes"
#
# events$event[events$event == "Arrythmia"] <- "Arrhythmia"
#
# events$timetoevent <- difftime(events$event_date, events$drugdate,
#                               units = "days")
#
# events$timetoevent2 <- difftime(events$event_date, events$drugdate,
#                                 units = "hours")
#
# data <- full_join(events, data, by = "id")
#
# data$any_event[is.na(data$any_event)] <- "No"
#
# data$any_event <- factor(data$any_event)




# Variable creation/cleaning ####

# Collapse diabetes
# table(data$hx_dm)

  data$any.diabetes <- NA
  data$any.diabetes[data$hx_dm == 0] <- "No"
  data$any.diabetes[data$hx_dm >  0] <- "Yes"

  data$any.diabetes <- factor(data$any.diabetes, levels = c("No", "Yes"))

# ALternate stroke volume
# glvef = (sv/lvedv)
# sv = glvef * lvdev
# sv =   lvedv - lvesv

  data$sv.1 <- data$glvef * data$lvedv
  data$sv.2 <- data$lvedv - data$lvesv

# ggplot(data, aes(x = sv.1, y = sv.2, label = id)) +
#          facet_wrap(~time) +
#          geom_text() # One outlier 104
#
# ggplot(data, aes(x = glvef, y = lvedv, label = id)) +
#   facet_wrap(~time) +
#   geom_text() # 104's lvedv low
#
# ggplot(data, aes(x = lvesv, y = lvedv, label = id)) +
#   facet_wrap(~time) +
#   geom_text()
#

  data$glvef.2 <- (data$lvedv - data$lvesv) / data$lvedv
#
#    ggplot(data, aes(x = glvef, y = glvef.2, label = id)) +
#      facet_wrap(~time) +
#      geom_text()


# BMI

  data$bmi <- data$demo_wt_kg / (data$demo_ht_cm / 100)^2

# Clean

  data$famhx_cad[data$famhx_cad == 99] <- NA
  data$pre_rand_killip[data$pre_rand_killip == 99] <- NA

  data$demo_gender <- factor(data$demo_gender, labels = c("Male", "Female"))

  data$discharge_nyha <- factor(data$discharge_nyha)

  data$dsch_med_asa <- factor(data$dsch_med_asa, levels = c(0, 1),
                              labels = c("No", "Yes"))

  data$dsch_med_statin <- factor(data$dsch_med_statin, levels = c(0, 1),
                                 labels = c("No", "Yes"))

  data$dsch_med_betablk <- factor(data$dsch_med_betablk, levels = c(0, 1),
                                  labels = c("No", "Yes"))

  data$hx_smkg <- factor(data$hx_smkg)

  data$culprit <- factor(data$culprit)

  data$stent1_type <- factor(data$stent1_type)

  data$pre_rand_killip <- factor(data$pre_rand_killip)

  data$pre_timi[data$id == 109] <- 0 # Corrected error from JC

  data$pre_timi <- factor(data$pre_timi)

  data$disc_med_ticagr[data$id == 110] <- 0 # Corrected error from JC



# Missing values ####

  mcebl <- filter(data, time == "Baseline") %>%
    select(id, arm, latece, everything()) %>%
    arrange(id, arm)

  mcebl$latece_bl_miss <- "No"
  mcebl$latece_bl_miss[is.na(mcebl$latece)] <- "Yes"

  data <- full_join(data, select(mcebl, id, latece_bl_miss), by = "id")

  mcebl <- filter(data, time == "8 weeks") %>%
    select(id, arm, latece, everything()) %>%
    arrange(id, arm)

  mcebl$latece_8_miss <- "No"
  mcebl$latece_8_miss[is.na(mcebl$latece)] <- "Yes"

  data <- full_join(data, select(mcebl, id, latece_8_miss), by = "id")

  rm(mcebl)

  table(data$latece_8_miss, data$latece_bl_miss)

# plot(as.numeric(difftime(balloon$drug_time, balloon$balloon_time, units = "mins")),
# balloon$diff) # Perfect



# Follow-up outcomes ####

  follow <- read_excel("data/RESUS AMI _Full Echo Data06112017.xlsx")

  follow <- follow[rowSums(is.na(follow)) != ncol(follow),] # Remove blank rows

  names(follow) <- c("id", "time", "edv", "esv", "lvef_biplane")

  target <- c("edv", "esv", "lvef_biplane")
  follow[target] <- lapply(follow[target], as.numeric); rm(target)

  follow <- filter(follow, time != "DIFF")

  follow$lvef_biplane[follow$lvef_biplane < 0] <- NA

# table(follow$time)

  follow$time[grepl("BL", follow$time)] <- 0
  follow$time <- as.numeric(gsub("4",  "", follow$time))
  follow$time <- as.numeric(gsub("\\D",  "", follow$time))
  follow$time[follow$time == 8] <- 2

  d <- select(data, id, arm, glvef, lvedv, lvesv, time) %>%
    filter(time != "Difference") %>%
    mutate(time = as.character(time))

  d$time[d$time == "Baseline"] <- 0
  d$time[d$time == "8 weeks"] <- 2
  d$time <- as.numeric(d$time)

  follow <- full_join(follow, select(d, -arm), by = c("id", "time")) %>%
    distinct()

  follow <- full_join(follow, select(d, id, arm), by = c("id")) %>%
    distinct(); rm(d)

# corrplot <- function(data, var1, var2, ...){
#   return(
#     ggplot(data, aes_string(x = var1, y = var2)) +
#       facet_wrap(~time) +
#       geom_point() +
#       geom_smooth(method = "lm", se = FALSE) +
#       ggtitle(var1) +
#       xlab("Main data value") +
#       ylab("Follow up data value")
#   )
# }
#
# corrplot(filter(follow, time < 3), "lvedv", "edv")
#   ggsave("plots/corr_edv.png")
# corrplot(filter(follow, time < 3), "lvesv", "esv")
#   ggsave("plots/corr_esv.png")
# corrplot(filter(follow, time < 3), "glvef", "lvef_biplane")
#   ggsave("plots/corr_lvef.png")
#
# changeplot <- function(data, y, ...){
#   return(
#     ggplot(follow, aes_string(x = "time", y = y, color = "arm", group = "id")) +
#       facet_wrap(~arm, nrow = 1) +
#       geom_point() +
#       geom_line() +
#       scale_color_brewer(guide = FALSE, palette = "Set1") +
#       scale_x_continuous(breaks = c(0, 2, 6, 12)) +
#       theme(panel.grid.minor = element_blank()) +
#       xlab("Months") +
#       ylab(y) +
#       ggtitle(y)
#   )
# }
#
# changeplot(follow, "edv")
#   ggsave("plots/change_edv.png")
# changeplot(follow, "esv")
#   ggsave("plots/change_esv.png")
# changeplot(follow, "lvef_biplane")
#   ggsave("plots/change_lvef.png")



# EXTRA ####

# New regional data ####

#   regional <- read_excel("data/Regionaldata417.xlsx", col_names = FALSE)
#
#   names(regional) <- c("id",
#                        "thick_ed_bl", "thick_es_bl", "thickening_bl", "motion_bl",
#                        "thick_ed_8w", "thick_es_8w", "thickening_8w", "motion_8w")
#
#   regional$zone <- gsub("[[:digit:]]", NA, regional$id)
#   regional$id <-   gsub("[[:upper:]]", NA, regional$id)
#
#   regional <- regional[rowSums(is.na(regional)) != ncol(regional),] # Remove blank rows
#
#
#   regional$id   <- repeat.before(regional$id)
#
#   x <- data_frame()
#   for(i in unique(regional$id)){
#     x <- rbind(x, filter(regional, id == i)[-c(1:4), ])
#   }
#
#   regional <- x
#
#   regional[1:9] <- lapply(regional[1:9], as.numeric)
#
#   regional <- left_join(regional, select(data, id, arm, bsa.m2, any.diabetes) %>%
#                                   distinct(id, arm, bsa.m2, any.diabetes),
#                         by = "id")
#
# # ERROR in motion, value of ~120 at baseline in the RZ, arm B
#
#   regional$motion_bl[regional$motion_bl > 30] <- NA
#
#
# # Long version
#
#   longnames <- c("id", "thick_ed", "thick_es", "thickening", "motion", "zone",
#                  "arm", "bsa.m2", "any.diabetes", "time")
#
#   x <- data_frame()
#   for(i in unique(regional$id)){
#     y1 <- filter(regional, id == i) %>%
#             select(id,
#                    thick_ed_bl, thick_es_bl, thickening_bl, motion_bl, zone,
#                    arm, bsa.m2, any.diabetes) %>%
#             mutate(time = "Baseline")
#     y2 <- filter(regional, id == i) %>%
#             select(id,
#                    thick_ed_8w, thick_es_8w, thickening_8w, motion_8w, zone,
#                    arm, bsa.m2, any.diabetes) %>%
#             mutate(time = "8 weeks")
#
#     names(y1) <- longnames
#     names(y2) <- longnames
#
#     x <- rbind(x, rbind(y1, y2))
#   }
#
#   regional_long <- x
#
#   rm(longnames, y1, y2, x)
#
#   regional_long$time <- factor(regional_long$time, levels = c("Baseline", "8 weeks"))
#
# # for(i in c("thick_ed", "thick_es", "thickening", "motion")){
# #
# #   ggplot(regional_long, aes_string(y = i, x = "time")) +
# #     geom_line(aes(group = id), alpha = 0.5, color = "black", linetype = "dashed") +
# #     geom_smooth(aes(x = as.numeric(time)), method = "lm", se = FALSE) +
# #     facet_wrap(~zone + arm, ncol = 3) +
# #     theme_minimal() +
# #     ylab("Thickness ED") +
# #     xlab("")
# #
# #   ggsave(paste0("plots/", i, ".png"), width = 6 * 1.67, height = 6, units = "in")
# #
# # }
#
#   x <- data_frame()
#   for(i in unique(regional$zone)){
#     for(j in c("thick_ed", "thick_es", "thickening", "motion")){
#       data <- filter(regional, zone == i) %>%
#                 select(starts_with(j), arm, bsa.m2, any.diabetes)
#       form <- as.formula(paste0(j, "_8w ~ ", j, "_bl + arm + any.diabetes"))
#       x   <- rbind(x, lm(form, data) %>%
#                       tidy() %>%
#                       mutate(outcome = j, zone = i))
#     }
#   }
#
#   filter(x, term == "armArm B (LD)" | term == "armArm C (HD)") %>%
#     mutate(term = factor(term, levels = c("armArm B (LD)", "armArm C (HD)"),
#                          labels = c("LD-IGF1 1.5ng", "LD-IGF1 15ng"))) %>%
#   ggplot(aes(y = estimate, x = term)) +
#     geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
#     geom_pointrange(aes(ymax = estimate + (1.96 * std.error),
#                         ymin = estimate - (1.96 * std.error))) +
#     facet_wrap(~zone + outcome, ncol = 4, scales = "free_y") +
#     theme_minimal()
#
#   ggsave("plots/regional_models.png", height = 9, width = 8, units = "in",
#          scale = 1.2)
#
#
#



# with(distinct(select(data, id, arm, event)), table(arm, event))

# with(distinct(select(data, id, arm, any_event)), chisq.test(arm, any_event))


# Mean thickening ####



#
# # Checking calculations in spreadsheet
# # plot(rowSums(select(thicn, starts_with("seg")), na.rm = TRUE),
# #      thicn$sum.thickening)
#
# # plot(rowMeans(select(thicn, starts_with("seg")), na.rm = TRUE),
# #      thicn$mean.thickening)
#
#
#   select(thicn, id, time, seg01, seg02, seg07, seg08, seg13, seg14) %>%
#     gather(seg, thickening, starts_with("seg")) %>%
#     ggplot(aes(x = thickening, fill = time, color = time)) +
#     geom_bkde(alpha = 0.5) +
#     geom_rug() +
#     facet_wrap(~ seg, ncol = 2) +
#     theme_base() +
#     scale_fill_viridis(discrete = TRUE) +
#     scale_color_viridis(discrete = TRUE) +
#     xlab("Thickness (mm)") +
#     ylab("Probability Density")
#
#   ggsave("plots/thickness.png", height = 6, width = 6 * 1.67, units = "in")

#  names(base)

# radar.thick <- filter(data, time == "8 weeks") %>%
#                 select(id, arm, starts_with("thick")) %>%
#                 group_by(arm) %>%
#                 summarise(thick01 = mean(thick01, na.rm = TRUE),
#                           thick02 = mean(thick02, na.rm = TRUE),
#                           thick07 = mean(thick07, na.rm = TRUE),
#                           thick08 = mean(thick08, na.rm = TRUE),
#                           thick13 = mean(thick13, na.rm = TRUE),
#                           thick14 = mean(thick14, na.rm = TRUE))
#
# radar.thick <- data_frame(label = names(radar.thick)[2:7],
#                           "Arm A (P)"  = as.numeric(radar.thick[1, 2:7]),
#                           "Arm B (LD)" = as.numeric(radar.thick[2, 2:7]),
#                           "Arm C (HD)" = as.numeric(radar.thick[3, 2:7]))
#
#
# chartJSRadar(radar.thick)
#


# select(data, id, time, arm, starts_with("thick")) %>%
# group_by(arm, time) %>%
# summarise(seg01 = mean(thick01, na.rm = TRUE),
#           seg02 = mean(thick02, na.rm = TRUE),
#           seg07 = mean(thick07, na.rm = TRUE),
#           seg08 = mean(thick08, na.rm = TRUE),
#           seg13 = mean(thick13, na.rm = TRUE),
#           seg14 = mean(thick14, na.rm = TRUE)) %>%
# gather(segment, value, 3:8) %>%
# ggplot(aes(x = segment, y = value, fill = arm)) +
#   geom_bar(stat = "identity", position = "dodge") +
#   theme_base() +
#   facet_wrap(~time) +
#   coord_flip() +
#   scale_fill_viridis(discrete = TRUE) +
#   ggtitle("Thickness")
#
# ggsave("plots/thickness.png", height = 6, width = 6 * 1.67, units = "in")
#
#
#
# select(data, id, time, arm, starts_with("thicg")) %>%
#   group_by(arm, time) %>%
#   summarise(seg01 = mean(thicg01, na.rm = TRUE),
#             seg02 = mean(thicg02, na.rm = TRUE),
#             seg07 = mean(thicg07, na.rm = TRUE),
#             seg08 = mean(thicg08, na.rm = TRUE),
#             seg13 = mean(thicg13, na.rm = TRUE),
#             seg14 = mean(thicg14, na.rm = TRUE)) %>%
#   gather(segment, value, 3:8) %>%
#   ggplot(aes(x = segment, y = value, fill = arm)) +
#   geom_bar(stat = "identity", position = "dodge") +
#   theme_base() +
#   facet_wrap(~time) +
#   coord_flip() +
#   scale_fill_viridis(discrete = TRUE) +
#   ggtitle("Thickening")
#
# ggsave("plots/thickening.png", height = 6, width = 6 * 1.67, units = "in")
#
#
#

# Thousand segment

#   thou <- read_excel("data/THEOS ANALYSIS.xlsx")[c(1:31), c(1:21)]
#
# # So this is a oddly formatted sheet. It needs to be split into sets of rows,
# # those need to be transposed and renamed, and then it all needs to go back
# # together.
#
#   thou <- rbind(
#
#   t(thou[c(1:7), ])[-1, ] %>%    # Transpose set of rows 1
#     tbl_df() %>%                 # Convert to tibble
#     rename(time = V1,            # Rename
#            id   = V2,
#            cat1 = V3,
#            cat2 = V4,
#            cat3 = V5,
#            cat4 = V6,
#            cat5 = V7),
#
#   t(thou[c(9:15), ])[-1, ] %>%   # Transpose set of rows 2
#     tbl_df() %>%
#     rename(time = V1,
#            id   = V2,
#            cat1 = V3,
#            cat2 = V4,
#            cat3 = V5,
#            cat4 = V6,
#            cat5 = V7),
#
#   t(thou[c(17:23), ])[-1, ] %>%  # Transpose set of rows 3
#     tbl_df() %>%
#     rename(time = V1,
#            id   = V2,
#            cat1 = V3,
#            cat2 = V4,
#            cat3 = V5,
#            cat4 = V6,
#            cat5 = V7),
#
#   t(thou[c(25:31), ])[-1, ] %>%  # Transpose set of rows 4
#     tbl_df() %>%
#     rename(time = V1,
#            id   = V2,
#            cat1 = V3,
#            cat2 = V4,
#            cat3 = V5,
#            cat4 = V6,
#            cat5 = V7))
#
#   thou <- thou[rowSums(is.na(thou)) != ncol(thou),] # Remove blank rows
#
#
#   thou$time[thou$time == "Follow Up" & !is.na(thou$time)] <- "8 weeks"
#   thou$time <- factor(thou$time, levels = c("Baseline", "8 weeks"))
#
#   thou[2:7] <- lapply(thou[2:7], function(x){gsub("[[:alpha:]]", NA, x)})
#
#   thou[2:7] <- lapply(thou[2:7], as.numeric)
#
#   thou$cat1[thou$cat1 == 0 & !is.na(thou$cat1)] <- 1
#   thou$cat2[thou$cat2 == 0 & !is.na(thou$cat2)] <- 1
#   thou$cat3[thou$cat3 == 0 & !is.na(thou$cat3)] <- 1
#   thou$cat4[thou$cat4 == 0 & !is.na(thou$cat4)] <- 1
#   thou$cat5[thou$cat5 == 0 & !is.na(thou$cat5)] <- 1
#
#
#   thou <- mutate(thou, total.segs = cat1 + cat2 + cat3 + cat4 + cat5)
#   thou <- full_join(thou, select(data, id, arm) %>% distinct(), by = "id")
#
#
#   comp <- t(apply(select(thou, cat1:cat5), 1, function(x){x/sum(x)})) %>%
#           as_data_frame() %>%
#           rename(p.cat1 = cat1,
#                  p.cat2 = cat2,
#                  p.cat3 = cat3,
#                  p.cat4 = cat4,
#                  p.cat5 = cat5)
#
#   clr <- t(apply(select(thou, cat1:cat5), 1,
#                  function(x){log(x) - mean(log(x))})) %>%
#          as_data_frame() %>%
#          rename(clr1 = cat1,
#                 clr2 = cat2,
#                 clr3 = cat3,
#                 clr4 = cat4,
#                 clr5 = cat5)
#
#  thou <- cbind(thou, clr, comp)
#
#  comp.long <- select(thou, id, time, arm, starts_with("p.cat")) %>%
#               gather(cat, value, starts_with("p.cat"))
#
#  comp.long <- filter(comp.long, cat == "p.cat5" & time == "Baseline") %>%
#               group_by(id) %>%
#               summarise(maxcat5 = mean(value, na.rm = TRUE)) %>%
#               full_join(comp.long, by = "id")
#
#  comp.long$cat <- factor(comp.long$cat, levels = c("p.cat5", "p.cat4", "p.cat3",
#                                                    "p.cat2", "p.cat1"))
#
#  comp.long$id <- reorder(factor(comp.long$id), comp.long$maxcat5)
#
#  comp.long[complete.cases(comp.long), ] %>%
#  ggplot(aes(x = interaction(time, factor(id)), y = value, fill = cat)) +
#    geom_bar(stat = "identity") +
#    facet_wrap(~arm, nrow = 3, scales = "free_x") +
#    scale_fill_brewer("", palette = "Reds", direction = -1) +
#    theme_base() +
#    theme(axis.text.x = element_text(angle = 90, hjust = 1))
#
#  ggsave("plots/thousand.png", width = 6 * 1.67, height = 6, units = "in",
#         scale = 2)
#
# # CLRS
#
#   clrs.long <- select(thou, id, time, arm, starts_with("clr")) %>%
#                gather(cat, value, starts_with("clr"))
#
#   clrs.long$value[clrs.long$value == Inf] <- NA
#
#   ggplot(clrs.long, aes(y = value)) +
#     geom_violin(aes(x = as.numeric(time), group = time),
#                 color = "grey90", fill = "grey90", width = .5) +
#
#     theme_base() +
#     facet_wrap(~ cat + arm, ncol = 3, scales = "free_y") +
#
#     geom_line(aes(group = id,
#                   x = as.numeric(time)),
#               alpha = 0.3, linetype = "dashed") +
#     geom_point(aes(x = as.numeric(time)),
#                size = 2.5, alpha = 0.5) +
#     geom_line(data = clrs.long %>%
#                      group_by(arm, cat, time) %>%
#                      summarise(median =  median(value, na.rm = TRUE)),
#               aes(x = as.numeric(time), y = median),
#               color = "black", size = 1) +
#     scale_x_continuous(breaks = c(1, 2),
#                        labels = c("Pre", "Post")) +
#     scale_color_viridis(guide = FALSE, discrete = FALSE) +
#     xlab("") +
#     ylab("CLR") +
#     theme(strip.text = element_text(size = 10)) +
#     labs(title = "Changes in composition",
#          subtitle = "Centered log ratios")
#
#
#  ggsave("plots/change.clr.png",
#         height = 6,
#         width = 6 * 1.67,
#         units = "in", scale = 1.2)
#
#  ggplot(filter(clrs.long, !is.na(value)), aes(x = value, fill = cat)) +
#    geom_density(alpha = 0.5, color = "black") +
#    facet_wrap(~time + arm) +
#    scale_fill_viridis("", discrete = TRUE) +
#    theme_base() +
#    ylab("Density") +
#    xlab("CLR")
#
#  ggsave("plots/change.clr.dens.png",
#         height = 6,
#         width = 6 * 1.67,
#         units = "in", scale = 1.2)
#
#
#  ggplot(filter(clrs.long, !is.na(value)), aes(x = value, fill = cat)) +
#    geom_density(alpha = 0.5, color = "black") +
#    facet_wrap(~time + arm) +
#    scale_fill_brewer("", palette = "Reds") +
#    theme_base() +
#    ylab("Density") +
#    xlab("CLR")
#
#  ggsave("plots/change.clr.dens.r.png",
#         height = 6,
#         width = 6 * 1.67,
#         units = "in", scale = 1.2)
#
#  x <- select(thou, time, arm, id, clr5) %>%
#       filter(time == "Baseline") %>%
#       rename(clr5baseline  = clr5)
#
#  x$clr5baseline[x$clr5baseline == Inf | is.nan(x$clr5baseline)] <- NA
#
#
#  y <- select(thou, time, arm, id, clr5) %>%
#       filter(time == "8 weeks") %>%
#       rename(clr5_8wks  = clr5)
#
#  y$clr5_8wks[y$clr5_8wks == Inf | is.nan(y$clr5_8wks)] <- NA
#
#
#  x <- full_join(x, y, by = c("id", "arm")) %>% select(-starts_with("time"))
#
#  lm(clr5_8wks ~ arm + clr5baseline, data = x) %>% summary()
#
#
#
#  x <- select(thou, time, arm, id, clr4) %>%
#    filter(time == "Baseline") %>%
#    rename(clr4baseline  = clr4)
#
#  x$clr4baseline[x$clr4baseline == Inf | is.nan(x$clr4baseline)] <- NA
#
#
#  y <- select(thou, time, arm, id, clr4) %>%
#    filter(time == "8 weeks") %>%
#    rename(clr4_8wks  = clr4)
#
#  y$clr4_8wks[y$clr4_8wks == Inf | is.nan(y$clr4_8wks)] <- NA
#
#
#  x <- full_join(x, y, by = c("id", "arm")) %>% select(-starts_with("time"))
#
#  lm(clr4_8wks ~ arm + clr4baseline, data = x) %>% summary()

