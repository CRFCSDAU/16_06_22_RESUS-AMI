

# Useful libraries ####

# library(readxl)         # excel
#
# library(dplyr)
# library(tidyr)
# library(broom)
#
# library(stringr)        # Tidy characters
# library(stringi)
#
# library(ggplot2)        # Plot data
# library(RColorBrewer)
# library(ggrepel)
# library(ggthemes)
# library(viridis)
# library(ggalt)
# library(ggbeeswarm)
#
# library(gmodels)        # Describe data
# library(xtable)
# library(stargazer)
# library(pander)
# library(htmlTable)
#
#
# library("utils")        # Read scripts from github
# library(devtools)
#
# library(car)
# library(rms)
# library(Hmisc)
# library(lme4)
#


# sumTable ####


  sumTable <- function(data, var, g1, g2, ...){

    require(dplyr)

    x <- substitute(
           group_by(data, g1, g2) %>%
           summarise(n    = sum(!is.na(var)),
                     mean = mean(  var, na.rm = TRUE),
                     med  = median(var, na.rm = TRUE),
                     sd   = mean(  var, na.rm = TRUE),
                     min  = min(   var, na.rm = TRUE),
                     max  = max(   var, na.rm = TRUE)),
         list(g1 = as.name(g1),
              g2 = as.name(g2),
              var = as.name(var)))

    eval(x)
  }

#  sumTable(data, "glvef", "arm", "time")

# # trm ####
# # Remove trailing and leading white space from characters
#   trim <- function(x) {
#
#     # user defined for removing trail/lead white space
#
#     if (is.character(x) == TRUE) {
#       x <- as.character(gsub("^\\s+|\\s+$", "", x))
#     }
#     else {
#       x <- x
#     }
#   }
#
# # Example
# # data <- as.data.frame(lapply(data, trim), stringsAsFactors = FALSE)
#






# Table functions

# Generate the list of names for the table ####

  name.1 <- function(x, ...) {

    var.names <- list()

    for (i in seq_along(x)) {

      if(is.numeric(x[[i]])){
        var.names[[i]] <- names(x[i])
      }

      if(is.factor(x[[i]])){
        var.names[[i]] <- c(names(x[i]), levels(x[[i]]))
      }
    }

    unlist(var.names)
  }



# Means(sds) or counts(%) ####

  summary.1 <- function(x, ...) {

    summary.list <- list()

    for (i in seq_along(x)) {

      if(is.numeric(x[[i]])){
        summary.list[[i]] <- paste0(round(mean(x[[i]], na.rm = TRUE), 1),
                                    " (",
                                    round(sd(x[[i]],   na.rm = TRUE), 1),
                                    ")")
      }

      if(is.factor(x[[i]])){
        summary.list[[i]] <- c("", paste0(table(x[[i]]),
                                          " (",
                                          round(table(x[[i]]) /
                                                  sum(table(x[[i]])), 3) * 100,
                                          "%)"))
      }

    }
    unlist(summary.list)
  }

# Median(iqr) or counts(%) ####
  summary.2 <- function(x, ...) {

    summary.list <- list()

    for (i in seq_along(x)) {

      if(is.numeric(x[[i]])){
        summary.list[[i]] <- paste0(round(quantile(x[[i]], probs = c(0.50),
                                                   na.rm = TRUE), 0),
                                    " [",
                                    round(quantile(x[[i]], probs = c(0.25),
                                                   na.rm = TRUE), 0),
                                    ", ",
                                    round(quantile(x[[i]], probs = c(0.75),
                                                   na.rm = TRUE), 0),
                                    "]")
      }

      if(is.factor(x[[i]])){
        summary.list[[i]] <- c("", paste0(table(x[[i]]),
                                          " (",
                                          round(table(x[[i]]) /
                                                  sum(table(x[[i]])), 3) * 100,
                                          "%)"))
      }

    }
    unlist(summary.list)
  }

# Missing observations

  n.miss <- function(x, ...) {

    miss.list <- list()

    for (i in seq_along(x)) {

      if(is.numeric(x[[i]])){
        miss.list[[i]] <- length(x[[i]][!is.na(x[[i]])])
      }

      if(is.factor(x[[i]])){
        miss.list[[i]] <- c(length(x[[i]][!is.na(x[[i]])]),
                            rep("", length(levels(x[[i]]))))
      }

    }
    unlist(miss.list)
  }


# Min and max

  min.max <- function(x, ...) {

    min.max.list <- list()

    for (i in seq_along(x)) {

      if(is.numeric(x[[i]])){
        min.max.list[[i]] <- paste0("(",
                                    round(min(x[[i]], na.rm = TRUE), 1),
                                    ", ",
                                    round(max(x[[i]], na.rm = TRUE), 1),
                                    ")")
      }

      if(is.factor(x[[i]])){
        min.max.list[[i]] <- c("", rep("", length(levels(x[[i]]))))
      }

    }
    unlist(min.max.list)
  }


# Quartiles

  tiles <- function(x, ...) {

    quantiles.list <- list()

    for (i in seq_along(x)) {

      if(is.numeric(x[[i]])){
        quantiles.list[[i]] <- paste0(round(quantile(x[[i]], probs = c(0.25),
                                                     na.rm = TRUE), 1),
                                      ", ",
                                      round(quantile(x[[i]], probs = c(0.50),
                                                     na.rm = TRUE), 1),
                                      ", ",
                                      round(quantile(x[[i]], probs = c(0.75),
                                                     na.rm = TRUE), 1))
      }

      if(is.factor(x[[i]])){
        quantiles.list[[i]] <- c("", rep("", length(levels(x[[i]]))))
      }

    }
    unlist(quantiles.list)
  }


# Tests



  tests.1 <- function(data, ...) {

    tests.list <- list()

    require(dplyr)
    require(broom)

    for (j in seq_along(data)) {

      if(is.numeric(data[[j]])){

        t <- aov(data[[j]] ~ arm, data) %>%
             tidy()

        tests.list[[j]] <- round(t$p.value[1], 2)
      }

      if(is.factor(data[[j]])){

        c <- table(data[[j]], data$arm) %>%
             chisq.test() %>%
             tidy()

        tests.list[[j]] <- c(round(c$p.value[1], 2),
                             rep("", length(levels(data[[j]]))))
      }

    }
    unlist(tests.list)
  }



  tests.2 <- function(data, ...) {

    tests.list <- list()

    require(dplyr)
    require(broom)

    for (j in seq_along(data)) {

      if(is.numeric(data[[j]])){

        k <- kruskal.test(data[[j]] ~ arm, data) %>%
             tidy()

        tests.list[[j]] <- round(k$p.value[1], 2)
      }

      if(is.factor(data[[j]])){

        c <- table(data[[j]], data$arm) %>%
             chisq.test() %>%
             tidy()

        tests.list[[j]] <- c(round(c$p.value[1], 2),
                             rep("", length(levels(data[[j]]))))

      }
    }
    unlist(tests.list)
  }

# Repeats values to fill in NAs
# http://stackoverflow.com/questions/7735647/replacing-nas-with-latest-non-na-value
  repeat.before = function(x) {   # repeats the last non NA value. Keeps leading NA
    ind = which(!is.na(x))        # get positions of nonmissing values
    if(is.na(x[1]))               # if it begins with a missing, add the
      ind = c(1,ind)              # first position to the indices
    rep(x[ind], times = diff(     # repeat the values at these indices
      c(ind, length(x) + 1) ))    # diffing the indices + length yields how often
  }
