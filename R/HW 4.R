  library("ggplot2")
  lower_point <- numeric()
  Upper_point <- numeric()
for (i in 1:50) {
  sample_data <- sample(Google_Review_Ratings_$`Average ratings on parks`,size = 50)
  sample_frame <- data.frame(Rating = sample_data)
  
  z <- 2.58
  n <- 50
  sigma <- sd(Google_Review_Ratings_$`Average ratings on parks`)
  x_bar <- mean(sample_frame$Rating)
  
  ci_low <- x_bar - (z*sigma/sqrt(n))
  lower_point <- c(lower_point, ci_low)
  ci_up <- x_bar + (z*sigma/sqrt(n))
  Upper_point <- c(Upper_point, ci_up)
}
  print("Lower : ")
  print(lower_point)
  print("Upper : ")
  print(Upper_point)
  
real_mean <- mean(Google_Review_Ratings_$`Average ratings on parks`)
print("Lower mean is : ")
lower_mean <- mean(lower_point)
print(lower_mean)
print("Upper mean is : ")
upper_mean <- mean(Upper_point)
print(upper_mean)

ggplot(sample_frame,aes(sample_frame$Rating))+geom_density()+geom_vline(aes(xintercept = lower_mean),
color = "blue", linetype ="dashed", size=1)+geom_vline(aes(xintercept=upper_mean), color="green",linetype="dashed", size=1)+
geom_vline(aes(xintercept=real_mean),color="red",size=1)



