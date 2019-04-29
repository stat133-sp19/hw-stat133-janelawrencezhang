<h2> Overview  

***
`"binomial"` is a minimal R package that provides functions to get measures and visualization of a binomial variable.  
  
* `bin_choose()`calculates the binomial choosing factor.  
* `bin_variable()`create a "binvar" binomial variable.  
* `bin_probability()` calculates the probability.  
* `bin_distribution()` calculates the probabilities of all possible successes, a data.frame with class "bindis".   
* `bin_cumulative()` calculates the cumulative probabilities of all possible successes, a data.frame with class "bincum"  
* `plot()` method for a "bindis", "bincum" objects.  
* `summary()` method for a "binvar" object.  
* `bin_mean()`caculates the mean of a binomial variable.  
* `bin_variance()`caculates the variance of a binomial variable.  
* `bin_mode()`caculates the mode of a binomial variable.  
* `bin_skewness()`caculates the skewness of a binomial variable.  
* `bin_kurtosis()`caculates the kurtosis of a binomial variable.  


<h2>Motivation  

***

This package has been developed to practice how to use concepts of creation of an R package to build a package.  

<h2>Installation  

***
```
# development version from GitHub:
#install.packages("devtools") 

# install "binomial" (without vignettes)
devtools::install_github("foldername/binomial")

# install "cointoss" (with vignettes)
devtools::install_github("foldername/binomial", build_vignettes = TRUE)
```

<h2>Usage   

***
```
bin_choose(n=6,k=2)  
  
bin_probability(2,6,0.8)  
  
bin_dist <- bin_distribution(trials = 5, prob = 0.5)  
  
plot(bin_dist)  
  
bin_cum <- bin_cumulative(5,0.5)  
    
plot(bin_cum)  
  
bin1 <- bin_variable(5,0.5)  
  
summary(bin1)  
  
bin_mean(5,0.5)  
  
bin_variance(5,0.5)  

bin_mode(5,0.5)  

bin_skewness(5,0.5)  

bin_kurtosis(5,0.5)

```
