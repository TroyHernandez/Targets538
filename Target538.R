# Target538.R

dat <- read.csv("FiveThirtyEightTargets.csv")
dat <- dat[-nrow(dat), ]

future.ind <- which(is.na(dat$CLINTON.Won))
color.str <- as.character(dat$COLOR)
color.str <- replace(color.str, which(color.str == "R"), "red")
color.str <- replace(color.str, which(color.str == "P"), "purple")
color.str <- replace(color.str, which(color.str == "B"), "blue")
dat$COLOR <- color.str

past.dat <- dat[-future.ind, ]
Sanders <- past.dat$SANDERS.Won - past.dat$SANDERS.Target
names(Sanders) <- past.dat$STATE.OR.TERRITORY
sort(Sanders)

plot(x = past.dat$SANDERS.Target, y = past.dat$SANDERS.Won,
     xlab = "Target", ylab = "Won",
     main = "538's Sanders' Delegates\nTarget vs. Won",
     col = past.dat$COLOR, pch = 20)

big.sanders <- which(past.dat$SANDERS.Target > 50)

text(x = past.dat$SANDERS.Target[big.sanders] - c(10, rep(6, 5)),
     y = past.dat$SANDERS.Won[big.sanders],
     labels = past.dat$STATE.OR.TERRITORY[big.sanders], cex = .5)
abline(0, 1)

