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

#####################################################

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
#####################################################
# Modeling performance by partisanship
color.str2 <- replace(color.str, which(color.str == "red"), "red.purp")
color.str2 <- replace(color.str2, which(color.str2 == "purple"), "red.purp")

mod.sanders.blue <- lm(past.dat$SANDERS.Won[color.str2 == "blue"] ~ 0 +
                         past.dat$SANDERS.Target[color.str2 == "blue"])
# mod$coefficients
# Blue line is shows Bernie's average over-performance in blue states 
abline(0, mod.sanders.blue$coefficients[1], col = "blue")

mod.sanders.red <- lm(past.dat$SANDERS.Won[color.str2 == "red.purp"] ~ 0 +
                        past.dat$SANDERS.Target[color.str2 == "red.purp"])
# mod$coefficients
# Red line is shows Bernie's average under-performance in red/purple states 
abline(0, mod.sanders.red$coefficients[1], col = "red")

#####################################################
Clinton <- past.dat$CLINTON.Won - past.dat$CLINTON.Target
names(Clinton) <- past.dat$STATE.OR.TERRITORY
sort(Clinton)

plot(x = past.dat$CLINTON.Target, y = past.dat$CLINTON.Won,
     xlab = "Target", ylab = "Won",
     main = "538's Clinton's Delegates\nTarget vs. Won",
     col = past.dat$COLOR, pch = 20)

big.clinton <- which(past.dat$CLINTON.Target > 50)

text(x = past.dat$CLINTON.Target[big.clinton] - rep(8, length(big.clinton)),
     y = past.dat$CLINTON.Won[big.clinton],
     labels = past.dat$STATE.OR.TERRITORY[big.clinton], cex = .5)
abline(0, 1)

#####################################################

mod.clinton.blue <- lm(past.dat$CLINTON.Won[color.str2 == "blue"] ~ 0 +
                         past.dat$CLINTON.Target[color.str2 == "blue"])
# mod$coefficients
# Blue line is shows Hilary's average under-performance in blue states 
abline(0, mod.clinton.blue$coefficients[1], col = "blue")

mod.clinton.red <- lm(past.dat$CLINTON.Won[color.str2 == "red.purp"] ~ 0 +
                        past.dat$CLINTON.Target[color.str2 == "red.purp"])
# mod$coefficients
# Red line is shows Hillary's average over-performance in red/purple states 
abline(0, mod.clinton.red$coefficients[1], col = "red")

#####################################################
# Percent of delegates pledged:
sum(past.dat$TOTAL.DELEGATES) / sum(dat$TOTAL.DELEGATES)

# Percent of past delegates from red or purple states
sum(past.dat$TOTAL.DELEGATES[past.dat$COLOR == "red" |
                              past.dat$COLOR == "purple"]) /
  sum(past.dat$TOTAL.DELEGATES)

# Percent of past delegates from blue states
sum(past.dat$TOTAL.DELEGATES[past.dat$COLOR == "blue"]) /
  sum(past.dat$TOTAL.DELEGATES)

# Percent of future delegates from red or purple states
future.dat <- dat[future.ind, ]
sum(future.dat$TOTAL.DELEGATES[future.dat$COLOR == "red" |
                                future.dat$COLOR == "purple"]) /
  sum(future.dat$TOTAL.DELEGATES)

# Percent of future delegates from blue states
sum(future.dat$TOTAL.DELEGATES[future.dat$COLOR == "blue"]) /
  sum(future.dat$TOTAL.DELEGATES)
##########################################################
# Bernie still comes up short
sum(future.dat$SANDERS.Target[future.dat$COLOR == "blue"] *
    mod.sanders.blue$coefficients) + # 1.25) +#
  sum(future.dat$SANDERS.Target[future.dat$COLOR == "red" |
                                  future.dat$COLOR == "purple"] *
        mod.sanders.red$coefficients) + sum(dat$SANDERS.Won, na.rm = T)
# [1] 1945.444
# [1] 1959.953
# [1] 1861.727

# Hillary eeks it out
sum(future.dat$CLINTON.Target[future.dat$COLOR == "blue"] *
      mod.clinton.blue$coefficients) +
  sum(future.dat$CLINTON.Target[future.dat$COLOR == "red" |
                                  future.dat$COLOR == "purple"] *
        mod.clinton.red$coefficients) + sum(dat$CLINTON.Won, na.rm = T)
# [1] 2102.357
# [1] 2188.657

# Percent of remaining for Bernie to win
(2026 - sum(dat$SANDERS.Won, na.rm = T)) / sum(future.dat$TOTAL.DELEGATES)
# [1] 0.6587473

