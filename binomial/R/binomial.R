
# title Check Probability input
# description A private checker function that thecks whether the probability is between 0 and 1(inclusively)
# param prob the probability of success in a trial
# return Logical value (TRUE or FALSE)
# example
# check_prob(0.5)
# check_prob(2)
check_prob <- function(prob){
  if (prob >=0 & prob<=1){
    return(TRUE)
  }else{
    stop("invalid prob value, p has to be a number vetween 0 and 1")
  }
}


# title Check the trials input
# description A private checker function that checks whether the trials input is a num-negative integer
# param trials total number of trials
# return Logical value (TRUE or FALSE)
# example
# check_trials(0.5)
# check_trials(2)
check_trials <- function(trials){
  if (trials>0 & (trials%%1==0)){
    return(TRUE)
  }else{
    stop("invalid trials value")
  }
}

#
# title Check the Success input
# description A private checker function that checks whether the successes input is between 0 and number of trials(inclusivelly)
# param success number of successes in n trials (positive integer(s))
# param trials total number of trials (positive integer(s))
# return Logical value (TRUE or FALSE)
# example
# check_success(5,1)
# check_success(2,5)
check_success <- function(success, trials){
  if (
    (all(success >= 0))&
    (all(success%%1==0))&
    (check_trials(trials))&
    (all(success<=trials) )
  ){
    return(TRUE)
  }else{
    stop("invalid success value")
  }
}


# title Auxilary function for mean of a binomial variable
# description A private auxiliary function "mean" that calculate the mean based on the inputs, tirals and prob
# param trials total number of trials
# param prob the probability of success in a trial
# return mean of a binomial variable
# example
# aux_mean(2,0.5)
# 1
# aux_mean(5,0.5)
# 2.5
aux_mean <- function(trials,prob){
  check_trials(trials)
  check_prob(prob)
  return(trials*prob)
}

# title Auxilary function for variance of a binomial variable
# description A private auxiliary function "variance" that calculate the variance based on the inputs, tirals and prob
# param trials total number of trials
# param prob the probability of success in a trial
# return variance of a binomial variable
# example
# aux_variance(2,0.5)
# 0.5
# aux_variance(5,0.5)
# 1.25
aux_variance <- function(trials,prob){
  check_trials(trials)
  check_prob(prob)
  q <- 1-prob
  return(trials*prob*q )
}

# title Auxilary function for mode of a binomial variable
# description A private auxiliary function "mode" that calculate the mode based on the inputs, tirals and prob
# param trials total number of trials
# param prob the probability of success in a trial
# return mode of a binomial variable
# example
# aux_mode(2,0.5)
# 1
# aux_mode(5,0.5)
# 2 3
aux_mode <- function(trials,prob){
  check_trials(trials)
  check_prob(prob)
  if (prob==0){
    return(0)
  }else if(prob==1){
    return(trials)
  }else if((trials*prob+prob)%%1==0){
    m = as.integer(trials*prob+prob)
    return(c(m-1,m))
  }else{
    return(as.integer(trials*prob+prob))
  }
}


# title Auxilary function for skewness of a binomial variable
# description A private auxiliary function "skewness" that calculate the skewness based on the inputs, tirals and prob
# param trials total number of trials
# param prob the probability of success in a trial
# return skewness of a binomial variable
# example
# aux_skewness(2,0.8)
# -1.06066
# aux_skewness(5,0.5)
# 0
aux_skewness <- function(trials,prob){
  check_trials(trials)
  check_prob(prob)
  numerator <- 1-2*prob
  q <- 1-prob
  denomenator <- sqrt(trials*prob*q)
  return(numerator/denomenator)
}



# title Auxilary function for kurtosis of a binomial variable
# description A private auxiliary function "kurtosis" that calculate the kurtosis based on the inputs, tirals and prob
# param trials total number of trials
# param prob the probability of success in a trial
# return kurtosis of a binomial variable
# example
# aux_kurtosiss(2,0.8)
# 0.125
# aux_kurtosis(5,0.5)
# -0.4
aux_kurtosis <- function(trials,prob){
  check_trials(trials)
  check_prob(prob)
  q <- 1-prob
  numerator <- 1-6*prob*q
  denomenator <- trials*prob*q
  return(numerator/denomenator)
}

#' @title binomial choosing factor
#' @description Calculates the number of combinations in which k successes can occur in n trials
#' @param n total number of trials (positive integer)
#' @param k number of successes in n trials (positive integer)
#' @return return the number(s) of combinations
#' @export
#' @examples
#' bin_choose(n=5,k=2)
#' 10
#' bin_choose(n=5,k=0)
#' 1
#' bin_choose(n=5,1:3)
#' 5 10 10
#'
bin_choose <- function(n,k){
  if (check_success(k,n)){
    numerator <- factorial(n)
    de1 <- factorial(k)
    y <- n-k
    de2 <- factorial(y)
    return(numerator/(de1*de2))
  }else{
    stop("invalid k or n value")
  }
}




#' @title Binomial's Probability
#' @description calculate the probability of a binomial distribution which has success, trials, and prob.
#' @param success number of successes
#' @param trials total number of trials
#' @param prob probability of success in each trial
#' @return the calculated probability according to the success, trials and prob inputs
#' @export
#' @examples
#' bin_probability(success =2, trials = 5, prob = 0.5)
#' 0.3125
#' bin_probability(success =55, trials = 100, prob = 0.45)
#' 0.01075277
bin_probability <- function(success,trials,prob){
  if (check_trials(trials) & check_prob(prob) & check_success(success,trials)){
    factor <-  bin_choose(trials,success)
    q <- 1-prob
    fail <- trials-success
    return(factor*(prob^success)*(q^fail))
  }else{
    stop("At least one of the values of success, trials, or prob is invalid")
  }
}


#' @title Distribution of a Binomial Distribution
#' @description Calculating all probabilities of successes of a binomial distribution
#' @param trials total number of trials
#' @param prob the probability of a success in one trial
#' @return a dataframe which each row has the number of successes and its corresponding probability
#' @export
#' @examples
#' bin_distribution(trials = 5, prob=0.5)
#' ##  success   probability
#' ##1    0         0.03125
#' ##2    1         0.15625
#' ##3    2         0.31250
#' ##4    3         0.31250
#' ##5    4         0.15625
#' ##6    5         0.03125
bin_distribution <- function(trials,prob){
  success <- c(0:trials)
  probability <- bin_probability(success,trials,prob)
  df <- data.frame(success=success,probability=probability)
  class(df) <- c("bindis","data.frame")
  return(df)
}


#' @export
plot.bindis <- function(x){
  graph <- ggplot2::ggplot(data=x,ggplot2::aes(x=factor(success),y=probability)) +
    ggplot2::geom_bar(stat='identity',fill="grey") +ggplot2::theme_light()+
    ggplot2::scale_x_discrete(breaks=0:max(x$success), labels=0:max(x$success) ) + ggplot2::xlab("successes")
  return(graph)
}

#' @title Binomial Cumulative probabilities
#' @description Calculating cumulative probabilities for a binomial distribution
#' @param trials total number of trials
#' @param prob the probability of success of each trial
#' @return return a dataframe listing all cumulative probabilities of the binomial distribution
#' @export
#' @examples
#'  success probability cumulative
#'1       0     0.03125    0.03125
#'2       1     0.15625    0.18750
#'3       2     0.31250    0.50000
#'4       3     0.31250    0.81250
#'5       4     0.15625    0.96875
#'6       5     0.03125    1.00000

bin_cumulative <- function(trials,prob){
  success <- c(0:trials)
  cumulative <- rep(NA,trials+1)
  for (i in 0:trials){
    s <- c(0:i)
    cumulative[i+1] <- sum(bin_probability(s,trials,prob))
  }
  probability <- bin_probability(c(0:trials),trials,prob)
  df <- data.frame(success=success,probability=probability,cumulative=cumulative)
  class(df) <- c('bincum','data.frame')
  return(df)

}

#' @export
plot.bincum <- function(x){

  graph <- ggplot2::ggplot(data=x,ggplot2::aes(x=success,y=cumulative))+
    ggplot2::geom_line()+ggplot2::geom_point(shape=1,size=4)+ggplot2::theme_bw()+
    ggplot2::ylab("probability")+ggplot2::xlab("successes")
  return(graph)
}



#' @title Binomial Random Variable
#' @description Create a binomial random variable object based on number of trials and probability of success
#' @param trials total number of trials
#' @param prob the probability of success in each trial
#' @return return an object of class"binvar", a binomial random variable
#' @example
#' bin1 <- bin_variable(5,0.5)
#' @export
bin_variable <- function(trials,prob){
  check_trials(trials)
  check_prob(prob)
  object <- list(
    trials = trials,
    prob = prob)
  class(object) <-  "binvar"
  return(object)
}


#' @export
print.binvar <- function(x){
  cat('"Binomial Variable"\n\n')
  cat('Paramaters\n')
  cat('-number of trials: ',x$trials,'\n')
  cat('-prob of success : ',x$prob)

}


#' @export
summary.binvar <- function(x){
  summary <- list(
    trials=x$trials,
    prob = x$prob,
    mean = aux_mean(x$trials,x$prob),
    variance = aux_variance(x$trials,x$prob),
    mode = aux_mode(x$trials,x$prob),
    skewness = aux_skewness(x$trials,x$prob),
    kurtosis = aux_kurtosis(x$trials,x$prob))
  class(summary) <- "summary.binvar"
  return(summary)
}

#' @export
print.summary.binvar <- function(x){
  cat('"Summary Binomial"\n\n')
  cat('Paramaters\n')
  cat('-number of trials: ',x$trials,'\n')
  cat('-prob of success : ',x$prob,'\n\n')
  cat('Measures\n')
  cat('-mean    : ',x$mean,'\n')
  cat('-variance: ',x$variance, '\n')
  cat('-mode    : ',x$mode,'\n')
  cat('-skewness: ',x$skewness,'\n')
  cat('-kurtosis: ',x$kurtosis)
}


#main function to calculate mean based on paramaters, tirals and prob
#' @title Mean of a Binomial Distribution
#' @description Calculating the mean of a binomial distribution based on inputs, trials and probability
#' @param trials total numbeer of trials (numeric)
#' @param prob the probability of a success in a trial (numeric)
#' @return a number indicates the mean of the binomial distribution
#' @export
#' @examples
#' bin_mean(5,0.5)
#' 2.5
#' bin_mean(6,0.3)
#' 1.8
bin_mean <- function(trials,prob){
  check_trials(trials)
  check_prob(prob)
  return(aux_mean(trials,prob))
}

#main function to calculate variance based on paramaters, tirals and prob
#' @title Variance of a Binomial Distribution
#' @description Calculating the variance of a binomial distribution based on inputs, trials and probability
#' @param trials total numbeer of trials (numeric)
#' @param prob the probability of a success in a trial (numeric)
#' @return a number indicates the variance of the binomial distribution
#' @export
#' @examples
#' bin_variance(5,0.5)
#' 1.25
#' bin_variance(6,0.3)
#' 1.26
bin_variance <- function(trials,prob){
  check_trials(trials)
  check_prob(prob)
  return(aux_variance(trials,prob))
}

#main function to calculate mode based on paramaters, tirals and prob
#' @title Mode of a Binomial Distribution
#' @description Calculating the mode of a binomial distribution based on inputs, trials and probability
#' @param trials total numbeer of trials (numeric)
#' @param prob the probability of a success in a trial (numeric)
#' @return a number indicates the mode of the binomial distribution
#' @export
#' @examples
#' bin_mode(5,0.5)
#' 3
#' bin_mode(6,0.3)
#' 2
bin_mode <- function(trials,prob){
  check_trials(trials)
  check_prob(prob)
  return(aux_mode(trials,prob))
}

#main function to calculate skewness based on paramaters, tirals and prob
#' @title Skewness of a Binomial Distribution
#' @description Calculating the skewness of a binomial distribution based on inputs, trials and probability
#' @param trials total numbeer of trials (numeric)
#' @param prob the probability of a success in a trial (numeric)
#' @return a number indicates the skewness of the binomial distribution
#' @export
#' @examples
#' bin_skewness(5,0.5)
#' 0
#' bin_skewness(6,0.3)
#' 0.3563483
bin_skewness <- function(trials,prob){
  check_trials(trials)
  check_prob(prob)
  return(aux_skewness(trials,prob))
}

#main function to calculate kurtosis based on paramaters, tirals and prob
#' @title Kurtosis of a Binomial Distribution
#' @description Calculating the kurtosis of a binomial distribution based on inputs, trials and probability
#' @param trials total numbeer of trials (numeric)
#' @param prob the probability of a success in a trial (numeric)
#' @return a number indicates the kurtosis of the binomial distribution
#' @export
#' @examples
#' bin_kurtosis(5,0.5)
#' -0.4
#' bin_kurtosis(6,0.3)
#' -0.2063492
bin_kurtosis <- function(trials,prob){
  check_trials(trials)
  check_prob(prob)
  return(aux_kurtosis(trials,prob))
}
