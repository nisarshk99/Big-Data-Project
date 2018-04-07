#Law of large Numbers,,,, 

# a notrmal distribution, when you approach really large sets of numbers, the mean approaches or comes very close to Expected value.


x<-0 #counter

for (i in rnorm(1000)) # as opposed to conventional programming lang, the "i" variable is not a counter but takes 
  #the value of the rnorm function, a random number. loops through A thousand times.
  {
  if (i > -1 & i < 1) # for all the values falling between -1 and +1,,, checking the number or thier percent that 
    #fall between one standard deviation on both sides of mean is almost equal to that of expected value; "68.2%" 
    {
  y <- sum(i)
  x<- x+1}
}
c <- y/x

#in you answer in the "Values" towards the right window of Rstudio, check the number x, that is hte number of vairbales falling in between,
#close to 68% of numbers are with in the limit, suggesting the law of large numbers is upheld.