# 1) Write a R program using control operators to test whether following values are prime numbers or not by providing a PRIME or NOT PRIME message as output : 
#A. 103 B. 82 C. 179
x<-c(103, 82, 179)
for(z in x){
  if((z %% 2 != 0) && (z %% 3 != 0)){
    cat(z,": PRIME NUMBER \n")
  }else{
    cat(z, ": Not PRIME NUMBER \n")
  }
}

#2) Write a R program using control operators to identify letter u and a both occur in the following words: 1. above 2. unit 3. Under
library(stringr)
library(dplyr)
words<-c("above", "unit", "Under", "argument")
for(alphabet in words){
  if((str_detect(alphabet, "a"))==TRUE && ((str_detect(alphabet, "u"))==TRUE)){
    cat(alphabet, ": both u and a appear in each words \n")
  }else{
    cat(alphabet, ": both u and a doesn't' appear in each words \n")
  }
}

#3) Write a function that to calculate BMI (Body Mass Index): BMI for a person is defined 
#as their body mass divided by the square of their height The weight is in kilograms and 
#the height in meters or (The weight can be in pounds and the height in inches)* 703
BMI <- function(weight, height){
  BMI = (weight/(height^2))*703
  # weight in Kilogram and height in inches
  return(BMI)
  # 1kilogram = 2.20462262185, so weight(pound) = weight(kilogram)/ 0.45359237
}
BMI(50, 1.75)
BMI(72, 1.65)

#4) Write a function called sum_of_cubes, that calculates the sum of cubes of the first n natural numbers : if we have two numbers : 1, 2 then sum of squares is 9 ( 1^3 + 2^3) if we have three numbers : 1, 2, 3 then sum of squares is 36 ( 1^3 + 2^3 + 3^3)

sum_of_cubes <- function(x){
  cubes <- x^3
  sum <- sum(cubes)
  return(sum)
  
}

sum_of_cubes(6)

#5) Write a function to calculate the mode (highest frequency) of the following vector: x = c(2,3,3,4,4,5,6,7,9,10)

get <- function(x){
  Mode <- unique(x)
  Mode[which.max(tabulate(match(x, Mode)))]
  
}
x = c(2,3,3,4,4,5,6,7,9,10)
get(x)


#6) Write a function to calculate the no. of prime numbers of the following vector : x = c(2,2,3,3,4,5,7,11,15,19,24,29)

install.packages("numbers")
library(numbers)
x <- c(2,2,3,3,4,5,7,11,15,19,24,29)
calculate_no_primes <- function(x){
  for(i in x){
    if(isPrime(i)){
      cat(i, ": Prime \n")
    }else{
      cat(i, ": Not Prime \n")
    }
  }
  
}
calculate_no_primes(x)