# Quiz lesson 3 week 1

# Q5
set.seed(1118)
a = 5
b = 3
m = 1e5

theta = rbeta(n=m, shape1=a, shape2=b)
odds = theta/(1-theta)
ans_q5 = mean(odds)

# Q6
ind = odds > 1.0
# Probability odds > 1.0
ans_q6 = mean(ind)

# Q7
x = rnorm(n = m, mean = 0, sd = 1)
ans_q7 = quantile(x=x, probs=0.3)
ans_q7_validate = qnorm(p = 0.3, mean = 0, sd = 1)

# Q8
ans_q8 = sqrt(5.2/5000)
