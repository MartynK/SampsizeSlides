library(ggplot2)
library(dplyr)

# simulate 2 groups of 30 observations
# 30 times

set.seed(42)
N   <- 30
REP <- 30

gen_2armstudy <- function(N, mu1, mu2, sigma1, sigma2) {
  x1 <- rnorm(N, mu1, sigma1)
  x2 <- rnorm(N, mu2, sigma2)
  data.frame(x = c(x1, x2), group = rep(c("A", "B"), each = N))
}

dat <- do.call(rbind, lapply(1:REP, function(i) {
  gen_2armstudy(N, 7, 9, 1, 1) %>% mutate(rep = i)
  }))

dat_sum <- dat %>%
  group_by(group, rep) %>%
  summarise(mean = mean(x), sd = sd(x))

# plot the 1st rep
fig_1a_1strep <-
  dat %>%
    filter(rep == 1) %>%
  ggplot(., aes(x = x, alpha = rep/30)) +
    geom_histogram( color = "black",
                   position = "identity", bins = 40,
                   fill = "red") +
    theme_minimal() +
    theme(legend.position = "none") +
    scale_y_continuous(breaks=NULL) +
    scale_x_continuous(breaks = seq(0, 15, 5), limits = c(0,15)) +
    labs(y="",x="") +
    facet_grid(~group)

# plot the 1st rep's summary data
fig_1b_1strep <-
  dat_sum %>%
    filter(rep == 1) %>%
    ggplot(., aes(x = mean)) +
     geom_histogram( color = "black",
                     position = "identity", bins = 40, fill = "blue") +
     theme_minimal() +
     theme(legend.position = "none") +
     scale_y_continuous(breaks=NULL) +
     scale_x_continuous(breaks = seq(0, 15, 5), limits = c(0,15)) +
     labs(y="",x="") +
     facet_grid(~group)

#fig_1 <- gridExtra::grid.arrange(fig_1a_1strep, fig_1b_1strep, nrow = 2)

# plot all reps
fig_2a <-
  dat %>%
    ggplot(., aes(x = x, alpha = rep/30, group = rep)) +
    geom_histogram( color = "black",
                    position = "identity", bins = 40, fill = "red") +
    theme_minimal() +
    theme(legend.position = "none") +
    scale_y_continuous(breaks=NULL) +
    scale_x_continuous(breaks = seq(0, 15, 5), limits = c(0,15)) +
    labs(y="",x="") +
    facet_grid(~group)

# plot all reps' summary data
fig_2b <-
  dat_sum %>%
    ggplot(., aes(x = mean)) +
    geom_histogram( color = "black",
                    position = "identity", bins = 100, fill = "blue") +
    theme_minimal() +
    theme(legend.position = "none") +
    scale_y_continuous(breaks=NULL) +
    scale_x_continuous(breaks = seq(0, 15, 5), limits = c(0,15)) +
    labs(y="",x="") +
    facet_grid(~group)

#fig_2 <- gridExtra::grid.arrange(fig_2a, fig_2_b, nrow = 2)

# calculate the results of the studies for each rep (difference between groups with 95% CI)

for (i in 1:REP) {

  mod <- dat %>%
    filter(rep== i) %>%
    lm(x ~ group, data = .)

  if (i == 1) {
    dat_res <- data.frame(
      rep = i,
      pe = coef(mod)[2],
      confint_low = confint(mod)[2,1],
      confint_high = confint(mod)[2,2]
    )
  } else {
    dat_res <- rbind(dat_res, data.frame(
      rep = i,
      pe = coef(mod)[2],
      confint_low = confint(mod, level = 0.975)[2,1],
      confint_high = confint(mod, level = 0.975)[2,2]
    ))
  }

}

# plot dat_res as a forest plot
fig_3_forest <-
  dat_res %>%
    arrange(confint_low) %>%
    mutate(rank = row_number()) %>%
    ggplot(., aes(x = pe, xmin = confint_low, xmax = confint_high, y = rank)) +
    geom_pointrange(color = "darkgreen") +
    theme_minimal() +
    theme(legend.position = "none") +
    geom_vline(xintercept = c(1,1.5), linetype = "dashed", color = "salmon4") +
    scale_y_continuous(breaks=NULL) +
    scale_x_continuous(breaks = seq(0, 15, 5), limits = c(0,5)) +
    labs(y="",x="")



# plot lower ci as a histogram
fig_4_hist <-
  dat_res %>%
    ggplot(., aes(x = confint_low)) +
    geom_histogram( color = "black",
                    position = "identity", bins = 150, fill = "darkgreen") +
    theme_minimal() +
    theme(legend.position = "none") +
  geom_vline(xintercept = c(1,1.5), linetype = "dashed", color = "salmon4") +
    scale_y_continuous(breaks=NULL) +
    scale_x_continuous(breaks = c(seq(0, 15, 5),1,2.5), limits = c(0,5)) +
    labs(y="",x="")


dat_powss <- data.frame(
  n = seq(1, 20, 1),
  power = NA
)


for (i in 1:nrow(dat_powss)) {
  dat_powss$power[i] <- power.t.test(n = dat_powss$n[i], delta = 2,
                                     sd = 1,
                                     sig.level = 0.05,
                                     type = "two.sample",
                                     alternative = "two.sided")$power
}


fig_5_power <- dat_powss %>%
  ggplot(., aes(x = n, y = power)) +
  geom_line() +
  geom_point() +
  theme_minimal() +
  theme(legend.position = "none") +
  geom_hline(yintercept = c(0.8,.9), linetype = "dashed", color = "salmon4") +
  scale_y_continuous(breaks=c(.8,.9)) +
  scale_x_continuous(breaks = c(seq(0, 20, 5),6,7), limits = c(0,20)) +
  labs(y="Power",x="Sample size per group")

#fig_5_power


dat_powss_ext <- expand.grid(
  n = seq(1, 30, 1),
  sd = c(0.8,1,1.2),
  delta = c(1.5,2,2.5),
  dropout = c(.1,.2,.3),
  power = NA
) %>%
  mutate(situation = factor( paste0(sd, "_", delta, "_", dropout)),
         n_adj = (n / (1 - dropout))*2)



for (i in 1:nrow(dat_powss_ext)) {
  dat_powss_ext$power[i] <- power.t.test(n = dat_powss_ext$n[i], delta = dat_powss_ext$delta[i],
                                     sd = dat_powss_ext$sd[i],
                                     sig.level = 0.05,
                                     type = "two.sample",
                                     alternative = "two.sided")$power
}



# plot
fig_6_power <- dat_powss_ext %>%
  ggplot(., aes(x = n_adj, y = power, color = factor(delta), alpha = dropout,
                group = situation)) +
  geom_line() +
  geom_point() +
  theme_minimal() +
  theme(legend.position = "bottom") +
  geom_hline(yintercept = c(0.8,.9), linetype = "dashed", color = "salmon4") +
  scale_y_continuous(breaks=c(.8,.9)) +
  scale_x_continuous(breaks = c(seq(0, 40, 10)), limits = c(0,40)) +
  labs(y="Power",x="Total Sample size", color = "Effect size") +
  facet_grid(~sd, labeller = label_both)

#fig_6_power
