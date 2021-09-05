#------------------------- Assignment 1 -----------------------------

#Made By: Gaurav Baweja - 102097005 - CSE 4

# Qn - You have six courses in this semester (one of these is UCS410) and there are 750
# students in your class. Generate the marks of these 750 students for the six courses
# using the Linear Congruential Method (LCM), which is a type of random number
# generator. You should note that the marks will be uniformly distributed over [0,
# 100].
# Find the mean, median and standard deviation of the all the six subjects. Also, find
# the mean, median and standard deviation of the sum of marks in the six courses. You
# have also to draw six histograms for the marks in the six courses and one for the total
# marks.

lcm.random <- function(n){
  a = 3
  b = 5
  m = 101
  x = floor(runif(1,1,100))
  rnd = vector(length=n)
  for(i in 1:n){
    x = (a*x+b)%%m
    rnd[i] = x
  }
  return (rnd)
}
#------------------------- Initialize -----------------------------

students = 750
subjects = 6
sub.marks = matrix(nrow= students, ncol = subjects, byrow = TRUE)
#------------------------- Random marks for all subjects---------------------

for(i in 1:subjects)
{
  sub.marks[,i] = lcm.random(students)
}
#------------------------- Mean-Median-Standard Deviation ------------------

res.mean = vector(length=subjects)
res.median = vector(length=subjects)
res.sdev = vector(length=subjects)
total.sub = vector(length=subjects)
for(i in 1:subjects){
  res.mean[i] = mean(sub.marks[,i])
  res.median[i] = median(sub.marks[,i])
  res.sdev[i] = sd(sub.marks[,i])
  total.sub[i] = sum(sub.marks[,i])
}
total.sub.mean = mean(total.sub)
total.sub.median = median(total.sub)
total.sub.sdev = sd(total.sub)
#------------------------- Histogram of Subject Marks-----------------------

for(i in 1: subjects){
  hist(sub.marks[,i], xlab ="Marks", ylab= "Students" ,
       main=paste("Subject", as.character(i)))
}
#------------------------- histogram of sum of marks -----------------------

hist(total.sub, xlab ="Total Marks", ylab= "Subjects" ,
     main="Sum of Subjects")

#------------------------- END ---------------------------------
