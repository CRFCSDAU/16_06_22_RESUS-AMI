

  source("scripts/data.R")
  source("scripts/functions.R")


# Study arm ####

  select(data, arm, id) %>% distinct() %>% group_by(arm) %>% summarise(n = n())

# Outcomes ####

#          Time

  table(data$time, data$arm)


#          Global Left Ventricular Fraction

  sumTable(data, "glvef", "arm", "time")

  ggplot(study, aes(fill = time, x = glvef)) +
    geom_density() +
    facet_wrap(~arm)

  ggplot(study, aes(sample = glvef)) +
    geom_qq() +
    geom_abline(intercept = 0, slope = 1) +
    facet_wrap(~arm + time, scales = "free")

  qqnorm(study$glvef)


  x <- select(data, id, arm, time, glvef) %>% spread(time, glvef)
  names(x) <- c("id", "arm", "glvef.bl", "glvef.8")

  ggplot(x, aes(x = glvef.bl, y = glvef.8, color = arm)) +
    geom_point() +
    geom_smooth(method = "lm", se = FALSE)

  m.1 <- lm(glvef.8 ~ glvef.bl + arm, data = x)
  qqnorm(residuals(m.1))


# Covariates ####





# Tests #

# Longitudinal model

model.glvef.long <- lmer(glvef ~ arm + (1|id), data = lvef)

anova(model.glvef.long)
summary(model.glvef.long)

# Anova #

lvef.w <- spread(lvef, time, glvef)

names(lvef.w) <- c("id", "arm", "glvef.bl", "glvef.8")

lvef.w$arm <- relevel(lvef.w$arm, ref = "Arm B")

# Unadjusted

aov.glvef <- aov(glvef.8 ~ arm, data = lvef.w)
anova(aov.glvef)
Anova(aov.glvef, type="III")
Anova(aov.glvef, type="II")


lm.glvef.u <- lm(glvef.8 ~ arm, data = lvef.w)

anova(lm.glvef.u)
summary(lm.glvef.u)

# Adjusted

ancov.glvef <- aov(glvef.8 ~ arm + glvef.bl, data = lvef.w)
anova(ancov.glvef)
Anova(ancov.glvef, type="III")
Anova(ancov.glvef, type="II")

lm.glvef.a <- lm(glvef.8 ~ arm + glvef.bl, data = lvef.w)

anova(lm.glvef.a)
summary(lm.glvef.a)

# Non-parametric

library(quantreg)
library(Hmisc)
library(rms)

glvef.qr <- list()
k <- 1

for (i in c(5:95)){

  qr <- Rq(glvef.8 ~ arm + glvef.bl, se="boot", tau = i/100, lvef.w)


  glvef.qr[[k]] <- data.frame(var = attributes(qr$coefficients),
                              b   = as.numeric(qr$coefficients),
                              se  = as.numeric(sqrt(diag(vcov(qr)))),
                              tau = as.numeric(i))

  print(i)
  k <- k + 1
}

glvef.qr     <- as.data.frame(do.call(rbind, glvef.qr))
glvef.qr$out <- "GLVEF"

qrPlot <- function(dataA, ...){

  require(ggplot2)

  return(
    ggplot(dataA, aes(x = tau, y = b)) +
      geom_ribbon(aes(ymin = b - (1.96 * se), ymax = b + (1.96 * se)),
                  color = "#377EB8",
                  fill = "#377EB8", alpha = 0.9) +
      geom_line(color = "white", size = 1) +
      xlab("Centile") +
      facet_wrap(~names) +
      geom_hline(yintercept = 0, color = "#E41A1C") +
      theme(panel.grid.minor = element_blank()))

}

qrPlot(filter(glvef.qr, names == "arm=Arm A" | names == "arm=Arm C"))
qrPlot(filter(glvef.qr, names == "glvef.bl"))
















# Everything #######

ggplot(data, aes(y = value, color = factor(arm))) +
  geom_violin(aes(x = as.numeric(time), group = time),
              color = "grey90", fill = "grey90", width = .5) +
  geom_line(aes(group = interaction(id, arm),
                x = as.numeric(time)),
            alpha = 0.3, linetype = "dashed") +
  facet_wrap(~outcome + arm, scales = "free") +
  scale_x_continuous(breaks = c(1, 2),
                     labels = c("Baseline", "8 Weeks")) +
  theme_base() +
  geom_jitter(aes(group = id, x = as.numeric(time)),
              size = 2.5, alpha = 0.5, width = 0.05) +
  geom_smooth(data = group_by(data, outcome, arm),
              aes(x = as.numeric(time)),
              method = "lm", se = FALSE, size = 1) +
  scale_color_manual(guide = FALSE,
                     values = viridis(3, end = 0.8)) +
  scale_fill_manual(guide = FALSE, values = viridis(3, end = 0.8)) +
  xlab("") +
  ylab("")


# Area at risk

table(study[[10]], study[[3]])
