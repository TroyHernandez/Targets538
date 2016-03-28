# Target538.R

dat <- read.csv("FiveThirtyEightTargets.csv")
dat <- dat[-nrow(dat), ]

future.ind <- which(is.na(dat$CLINTON.Won))

past.dat <- dat[-future.ind, ]
Clinton <- past.dat$CLINTON.Won - past.dat$CLINTON.Target
names(Clinton) <- past.dat$STATE.OR.TERRITORY
sort(Clinton)

Sanders <- past.dat$SANDERS.Won - past.dat$SANDERS.Target
names(Sanders) <- past.dat$STATE.OR.TERRITORY
sort(Sanders)

plot(x = past.dat$SANDERS.Target, y = past.dat$SANDERS.Won,
     xlab = "Target", ylab = "Won", main = "Sanders: Target vs. Won", cex = .5)
big.sanders <- which(past.dat$SANDERS.Target > 50)
text(x = past.dat$SANDERS.Target[big.sanders],
     y = past.dat$SANDERS.Won[big.sanders] + c(5, rep(0, 5)),
     labels = past.dat$STATE.OR.TERRITORY[big.sanders])
abline(0, 1)

plot(x = past.dat$SANDERS.Target, y = past.dat$SANDERS.Won,
     xlab = "Target", ylab = "Won", main = "538's Sanders' Delegates\nTarget vs. Won", cex = .5)
big.sanders <- which(past.dat$SANDERS.Target > 50)
text(x = past.dat$SANDERS.Target[big.sanders] - c(10, rep(7, 5)),
     y = past.dat$SANDERS.Won[big.sanders],
     labels = past.dat$STATE.OR.TERRITORY[big.sanders], cex = .5)
abline(0, 1)
