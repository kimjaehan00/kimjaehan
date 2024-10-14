dbinom(0, 10, 0.01) + dbinom(1, 10, 0.01)
drink <- 0
for (x in 91:100) {
  drink <- drink + dbinom(x, 100, 0.8)
}
drink

dpois(2,1.2)
dpois(41,45)
1 - ppois(40, 45)
