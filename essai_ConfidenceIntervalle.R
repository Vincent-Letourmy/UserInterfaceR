res <- c(100,90,80,95,85)
mean <- mean(res)

error <- qt(0.975,df=length(res)-1)*sd(res)/sqrt(length(res))

left <- mean - error
right <- mean + error
