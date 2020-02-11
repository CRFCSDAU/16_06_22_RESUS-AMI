# source("scripts/data.R")

# Select the variables you want to use ####


table.1.var <- select(data, arm = missing, # see results.table..R for missing flag
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


table.1m <-
  data_frame("Variable"           = name.1(select(table.1.var, -arm)),

             "Total (n = 47)"     = summary.2(select(table.1.var, -arm)),
             "Missing (n = 10)"   = summary.2(filter(table.1.var,
                                                     arm == "Yes") %>%
                                                select(-arm)),
             "No missing (n = 37)"  = summary.2(filter(table.1.var,
                                                     arm == "No") %>%
                                                select(-arm)),
             "Test p-value"       = tests.2(table.1.var)[-c(1:3)])


stargazer(table.1m, out = "tables/table1miss.htm", type = "html", row.names = FALSE,
          summary = FALSE)