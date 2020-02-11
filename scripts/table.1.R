

# source("scripts/data.R")

# Select the variables you want to use ####

  table.1.var <- select(data, arm,
                        demo_age,
                        demo_gender,
                        hx_smkg,
                        hx_htn,
                        hx_dyslip,
                        any.diabetes,
                        famhx_cad,
                        bl_min_sbp, bl_min_dbp,
                        bl_min_hr,
                        demo_ht_cm, demo_wt_kg, bmi,
                        pre_timi,
                        culprit,
                        thromboysed,
                        post_pci_lvef,
                        stent1_type,
                        pre_rand_killip,
                        peak.tn.post.pci.new,
                        new.time,
                        pcistarttodrugstart.min,
                        dsch_med_clopid,
                        disc_med_ticagr,
                        gpiibiiia_given,
                        dsch_med_betablk,
                        dsch_med_ace,
                        dsch_med_statin,
                        dsch_med_aldos,
                        dsch_med_prasu,
                        dsch_med_asa,
                        discharge_nyha) %>% distinct()
  x <- table.1.var[, sapply(table.1.var, is.numeric)]
  x <- x[, sapply(x, max, na.rm = TRUE) == 1 &
           sapply(x, min, na.rm = TRUE) == 0]
  x <- names(x)
  table.1.var[, x] <- lapply(table.1.var[, x], factor, labels = c("No", "Yes"))


  table.1 <-
    data_frame("Variable"           = name.1(select(table.1.var, -arm)),
               "Missing values"     = as.numeric(n.miss(select(table.1.var,
                                                               -arm))),
               "Total (n = 47)"     = summary.2(select(table.1.var, -arm)),
               "Placebo (n = 15)"   = summary.2(filter(table.1.var,
                                                       arm == "Arm A (P)") %>%
                                                  select(-arm)),
               "Low dose (n = 16)"  = summary.2(filter(table.1.var,
                                                       arm == "Arm B (LD)") %>%
                                                  select(-arm)),
               "High dose (n = 16)" = summary.2(filter(table.1.var,
                                                       arm == "Arm C (HD)") %>%
                                                  select(-arm)),
               "Test p-value"       = tests.2(table.1.var)[-c(1:4)])


  stargazer(table.1, out = "tables/table1.htm", type = "html", row.names = FALSE,
            summary = FALSE)


# Age                            demo_age
# Sex	                           demo_gender
# Current Smoker		             hx_smkg; pack_yrs
#
# _Medical History
# Hypertension		               hx_htn
# Hyperlipidemia		             hx_dyslip
# Diabetes 		                   hx_dm
#
# Family History of CAD 		     famhx_cad
#
# _Baseline Parameters
# Blood Pressure		             bl_min_sbp, bl_min_dbp
# Heart rate		                 bl_min_hr
# BMI 		                       demo_ht_cm, demo_wt_kg
#
# TIMI Flow Prior to PCI 		     pre_timi
#
# Infarct related artery 		     culprit
#
# Pre Thrombolyisis	             thromboysed
#
# LV Angiogram Ejection Fraction post_pci_lvef
#
# DES                            stent1_type
#
# KILIP >1		                   pre_rand_killip
#
# Max serum Troponin		         peak.tn.post.pci
# Time to PCI (mins)	           isch.to.pci.A - ischemia to PCI
#
# PCI to study drug infusion 		 pcitodrug.min
#
#
# _Medical Therapy
# Aspirin
# Clopidogrel		                 dsch_med_clopid
# Ticagrelor		                 disc_med_ticagr
# Heparin
# GIIbIIIa                       gpiibiiia_given
# Beta Blocker		               dsch_med_betablk
# ACEI		                       dsch_med_ace
# ARB
# Statin		                     dsch_med_statin
# Oral Anticoagulants
# ?                              dsch_med_aldos
# ?                              dsch_med_prasu
# ?                              dsch_med_asa
#
# NYHA >1		                     discharge_nyha