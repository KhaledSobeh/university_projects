########################################
### Finding the number of iterations ###
########################################


# Loading the data
data0 <- read.csv("data0ready.csv",header = T)[,2:3]
data1 <- read.csv("data1ready.csv",header = T)[,2:3]
data2 <- read.csv("data2ready.csv",header = T)[,2:3]
data3 <- read.csv("data3ready.csv",header = T)[,2:3]


# Check if the alternatives meet the relative accuracy requirement n (Checking)
find_n <- function(alphA,kpi_number,gammA,KPI){
  n <- length(KPI)
  alpha_i <- alphA/kpi_number
  delta <- abs(qt(p=alpha_i/2, df=n-1))*(sd(KPI)/sqrt(n))
  Checking <- delta/mean(KPI) <= gammA/(1+gammA)
  delta_mean <- delta/mean(KPI)
  gama <- gammA/(1+gammA)
  return(c(delta_mean,gama,Checking))
}


kpi01 <- find_n(0.09,2,0.09,data0$vote_to_left_prob0)
kpi02 <- find_n(0.09,2,0.09,data0$reneged_from_left_prob0)

kpi11 <- find_n(0.09,2,0.09,data1$vote_to_left_prob1)
kpi12 <- find_n(0.09,2,0.09,data1$reneged_from_left_prob1)

kpi21 <- find_n(0.09,2,0.09,data2$vote_to_left_prob2)
kpi22 <- find_n(0.09,2,0.09,data2$reneged_from_left_prob2)

kpi31 <- find_n(0.09,2,0.09,data3$vote_to_left_prob3)
kpi32 <- find_n(0.09,2,0.09,data3$reneged_from_left_prob3)


Final_data_checking_n <- data.frame(vote_prob0 = kpi01, reneged_prob0 = kpi02,
                                    vote_prob1 = kpi11, reneged_prob1 = kpi12,
                                    vote_prob2 = kpi21, reneged_prob2 = kpi22,
                                    vote_prob3 = kpi31, reneged_prob3 = kpi32)
row.names(Final_data_checking_n) <- c('delta/mean','gama/gama+1','is delta/mean <= gama/gama+1 ?')

(Final_data_checking_n)
#as we can see , n=23 is good enough.

#export the final results:
write.csv(Final_data_checking_n, "Final_data_checking_n.csv")
