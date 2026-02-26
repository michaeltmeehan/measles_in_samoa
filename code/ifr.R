survival = data.frame(age = c(2, 10, 25, 45, 65), 
                      died = c(11 + 21 + 41, 1+1, 2+2+3, 3+1, 0), 
                      total = c(443+733+1791, 547+162, 533+910+362, 362+156, 27))

survival$survived = with(survival, total - died)

mod = glm(cbind(died, survived) ~ age, data=survival, family=binomial)

ifr = exp(predict(mod, survival))
