cuminc_dem <- fitCuminc(time = data$max,
                        risk = data$death,
                        group = data$exposure,
                        cens = "0")

plotCuminc(ci = cuminc_dem,
           cens = "0",
           ggtheme = theme_gray(),
           legendtitle = "Treatment")


# data("pbc2.id", package = "JM")
# pbc2.id$status3 <- as.numeric(pbc2.id$status != "alive")
# 
# pbc2.id$status != "alive"
# 
# fit <- survfit(Surv(years, status3) ~ 1, data = pbc2.id, etype = status)
# 
# plot(fit, fun = "event", col = c("black", "red"), lwd = 2, ylim = c(0, 0.7), 
#      xlab = "Follow-Up (years)", ylab = "Cumulative Incidence")
# legend("topleft", levels(data$death)[2:3], lty = 1, lwd = 2, col = c("black", "red"),
#        bty = "n")


data <- data %>% 
  mutate(status = as.numeric(death != 0))

fit <- survfit(Surv(max, status) ~ exposure, data = data, etype = death)

plot(fit, fun = "event", col = c("black", "red"), lwd = 2, ylim = c(0, 0.7),
     xlab = "Follow-Up (years)", ylab = "Cumulative Incidence")
