##############################
### Comparing Alternatives ###
##############################

# Loading the data
data0 <- read.csv("data0ready.csv",header = T)[,2:3]
data1 <- read.csv("data1ready.csv",header = T)[,2:3]
data2 <- read.csv("data2ready.csv",header = T)[,2:3]
data3 <- read.csv("data3ready.csv",header = T)[,2:3]


alpha_total <- 0.09
alpha_i <- 0.09/12

#------------------------------------------------------------Probability of voting to left party

# - kayam ---- hlufa 1
pairdTest1<- t.test(x=data0$vote_to_left_prob0,y=data1$vote_to_left_prob1,alternative="two.sided",paired=TRUE,var.equal=TRUE,conf.level=alpha_i)
print(pairdTest1)

# - kayam ---- hlufa 2
pairdTest2<- t.test(x=data0$vote_to_left_prob0,y=data2$vote_to_left_prob2,alternative="two.sided",paired=TRUE,var.equal=TRUE,conf.level=alpha_i)
print(pairdTest2)

# - kayam ---- hlufa 3
pairdTest3<- t.test(x=data0$vote_to_left_prob0,y=data3$vote_to_left_prob3,alternative="two.sided",paired=TRUE,var.equal=TRUE,conf.level=alpha_i)
print(pairdTest3)

# - hlufa 1 ---- hlufa 2
pairdTest4<- t.test(x=data1$vote_to_left_prob1,y=data2$vote_to_left_prob2,alternative="two.sided",paired=TRUE,var.equal=TRUE,conf.level=alpha_i)
print(pairdTest4)

# - hlufa 1 ---- hlufa 3
pairdTest5<- t.test(x=data1$vote_to_left_prob1,y=data3$vote_to_left_prob3,alternative="two.sided",paired=TRUE,var.equal=TRUE,conf.level=alpha_i)
print(pairdTest5)

# - hlufa 2 ---- hlufa 3
pairdTest6<- t.test(x=data2$vote_to_left_prob2,y=data3$vote_to_left_prob3,alternative="two.sided",paired=TRUE,var.equal=TRUE,conf.level=alpha_i)
print(pairdTest6)


#------------------------------------------------------------Probability of reneging from left do5n

# - kayam ---- hlufa 1
pairdTest7<- t.test(x=data0$reneged_from_left_prob0,y=data1$reneged_from_left_prob1,alternative="two.sided",paired=TRUE,var.equal=TRUE,conf.level=alpha_i)
print(pairdTest7)

# - kayam ---- hlufa 2
pairdTest8<- t.test(x=data0$reneged_from_left_prob0,y=data2$reneged_from_left_prob2,alternative="two.sided",paired=TRUE,var.equal=TRUE,conf.level=alpha_i)
print(pairdTest8)

# - kayam ---- hlufa 3
pairdTest9<- t.test(x=data0$reneged_from_left_prob0,y=data3$reneged_from_left_prob3,alternative="two.sided",paired=TRUE,var.equal=TRUE,conf.level=alpha_i)
print(pairdTest9)

# - hlufa 1 ---- hlufa 2
pairdTest10<- t.test(x=data1$reneged_from_left_prob1,y=data2$reneged_from_left_prob2,alternative="two.sided",paired=TRUE,var.equal=TRUE,conf.level=alpha_i)
print(pairdTest10)

# - hlufa 1 ---- hlufa 3
pairdTest11<- t.test(x=data1$reneged_from_left_prob1,y=data3$reneged_from_left_prob3,alternative="two.sided",paired=TRUE,var.equal=TRUE,conf.level=alpha_i)
print(pairdTest11)

# - hlufa 2 ---- hlufa 3
pairdTest12<- t.test(x=data2$reneged_from_left_prob,y=data3$reneged_from_left_prob3,alternative="two.sided",paired=TRUE,var.equal=TRUE,conf.level=alpha_i)
print(pairdTest12)



# one-way t-test kayam
test0<- t.test(x= data0$vote_to_left_prob0,y=NULL, alternative="two.sided",conf.level=alpha_total)
print(test0)
sd(data0$vote_to_left_prob0)
test1<- t.test(x= data0$reneged_from_left_prob0,y=NULL, alternative="two.sided",conf.level=alpha_total)
print(test1)
sd(data0$reneged_from_left_prob0)

# one-way t-test 7alufa1
test2<- t.test(x= data1$vote_to_left_prob1,y=NULL, alternative="two.sided",conf.level=alpha_total)
print(test2)
sd(data1$vote_to_left_prob1)
test3<- t.test(x= data1$reneged_from_left_prob1,y=NULL, alternative="two.sided",conf.level=alpha_total)
print(test3)
sd(data1$reneged_from_left_prob1)

# one-way t-test 7alufa2
test4<- t.test(x= data2$vote_to_left_prob2,y=NULL, alternative="two.sided",conf.level=alpha_total)
print(test4)
sd(data2$vote_to_left_prob2)
test5<- t.test(x= data2$reneged_from_left_prob2,y=NULL, alternative="two.sided",conf.level=alpha_total)
print(test5)
sd(data2$reneged_from_left_prob2)

# one-way t-test 7alufa3
test6<- t.test(x= data3$vote_to_left_prob3,y=NULL, alternative="two.sided",conf.level=alpha_total)
print(test6)
sd(data3$vote_to_left_prob3)
test7<- t.test(x= data3$reneged_from_left_prob3,y=NULL, alternative="two.sided",conf.level=alpha_total)
print(test7)
sd(data3$reneged_from_left_prob3)


