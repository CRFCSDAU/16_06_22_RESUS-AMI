# Missing CE tables

  table.supp <- select(data, latece_bl_miss, latece_8_miss,
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


  table.supp$arm <- "No"
  table.supp$arm[table.supp$latece_bl_miss == "Yes" |
                     table.supp$latece_8_miss == "Yes"] <- "Yes"

  table.supp$arm <- factor(table.supp$arm)


  x <- table.supp[, sapply(table.supp, is.numeric)]
  x <- x[, sapply(x, max, na.rm = TRUE) == 1 &
           sapply(x, min, na.rm = TRUE) == 0]
  x <- names(x)
  table.supp[, x] <- lapply(table.supp[, x], factor, labels = c("No", "Yes"))

  table.supp <- select(table.supp, - latece_bl_miss, - latece_8_miss)

  table.supp.1 <-
    data_frame("Variable"           = name.1(select(table.supp, -arm)),
               "Missing values"     = as.numeric(n.miss(select(table.supp,
                                                               -arm))),
               "Total (n = 47)"     = summary.2(select(table.supp, -arm)),
               "No missing Late CE (n = 31)"   = summary.2(filter(table.supp,
                                                       arm == "No") %>%
                                                  select(-arm)),
               "Missing Late CE (n = 16)"  = summary.2(filter(table.supp,
                                                       arm == "Yes") %>%
                                                  select(-arm)),
               "Test p-value"       = tests.2(table.supp)[-c(79:81)])


  stargazer(table.1, out = "tablesupp.htm", type = "html", row.names = FALSE,
            summary = FALSE)