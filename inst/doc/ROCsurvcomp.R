## ----echo=FALSE, out.width="80%"----------------------------------------------
knitr::include_graphics("figures/joint_plot.png")

## -----------------------------------------------------------------------------
library(ROCsurvcomp)
library(survival)
library(ggplot2)
library(PWEXP)

# Generating right-censored data with crossing survivals
set.seed(126)
n_trt <- 50
break_trt <- c(2, 4)
rate_trt <- c(log(2)/3, log(2)/7, log(2)/20)
rate.censor_trt <- c(log(2)/55, log(2)/62, log(2)/68)
event_trt <- PWEXP::rpwexp(n_trt, rate = rate_trt, breakpoint = break_trt)
censor_trt <- PWEXP::rpwexp(n_trt, rate = rate.censor_trt, breakpoint = break_trt)

n_ctrl <- 50
rate_ctrl <- log(2)/10
rate.censor_ctrl <- log(2)/58
event_ctrl <- rexp(n_ctrl, rate = rate_ctrl)
censor_ctrl <- rexp(n_ctrl, rate = rate.censor_ctrl)

# Observed time and censoring status (0 = event, 1 = right-censored)
time_trt <- pmin(event_trt, censor_trt)
status_trt <- ifelse(event_trt <= censor_trt, 0, 1)
time_ctrl <- pmin(event_ctrl, censor_ctrl)
status_ctrl <- ifelse(event_ctrl <= censor_ctrl, 0, 1)
time <- c(time_trt, time_ctrl)
status <- c(status_trt, status_ctrl)
group <- c(rep(1, n_trt), rep(2, n_ctrl))

# Plot Kaplan-Meier survival curves
plot(survfit(Surv(time, status == 0) ~ group), col = c("red", "blue"), lwd = 2,
     xlab = "Time", ylab = "Survival Probability",
     main = "Kaplan-Meier Curves")

# Run `surv.comp` function
# Note: n_perm = 10 is used here only for illustration purposes.
# Highly recommended n_perm to be set at 50,000 or more.
surv.comp(
  time = time,
  status = status,
  group = group,
  censor_type = "right",
  method = "roc_length",
  n_perm = 10,
  progress = TRUE,
  plot = FALSE
)

