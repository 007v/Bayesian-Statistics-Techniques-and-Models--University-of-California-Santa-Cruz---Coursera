# Week 1 honors quiz - Markov Chains

# Q3
Q = matrix(c(0, 1,
             0.3, 0.7),
           nrow = 2, byrow = TRUE)

Q3 = Q
for (i in 2:3) {
  Q3 = Q3 %*% Q
}
# Initial distribution
init.dist = c(1,0)
Q4.result = init.dist %*% Q3
print(Q4.result)

# Q4
# stationary distribution

Q100 = Q
for (i in 2:100){
  Q100 = Q100 %*% Q
}
stationary.dist = Q100[1,]
print(stationary.dist)

#Q5
Q5.ans = stationary.dist %*% Q
print(Q5.ans)