

  vars <- c("GLVEF_l", "GLVEF_h", "LVEDVIndex_l", "LVEDVIndex_h",
            "LVESVIndex_l", "LVESVIndex_h", "LVMassIndex_l", "LVMassIndex_h",
            "StrokeVolume_l", "StrokeVolume_h", "LateCE_l", "LateCE_h")

  data <- data_frame(p = c(0.505, 0.736, 0.138, 0.018, 0.216, 0.313, 0.827,
                           0.001, 0.223, 0.016, 0.87, 0.095),
                     var = vars)

  data <- arrange(data, p)

  data$m    <- nrow(data)
  data$i    <- c(1:nrow(data))
  data$q    <- 0.05
  data$fdrp <- with(data, i / m * q)
  data$sig  <- data$p <= data$fdrp

# Result: Only LV mass

# High dose only

  vars <- c("GLVEF_h", "LVEDVIndex_h",
            "LVESVIndex_h", "LVMassIndex_h",
            "StrokeVolume_h","LateCE_h")

  data <- data_frame(p = c(0.736, 0.018, 0.313, 0.001, 0.016, 0.095),
                     var = vars)

  data <- arrange(data, p)

  data$m    <- nrow(data)
  data$i    <- c(1:nrow(data))
  data$q    <- 0.05
  data$fdrp <- with(data, i / m * q)
  data$sig  <- data$p <= data$fdrp

# Result: All three only









