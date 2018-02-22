library(tidyverse)
library(rgl)

firvecs <- read_csv("~/thesis/firingvectors.csv")

firvecs <- firvecs %>%
  mutate(r = sqrt(x^2 + y^2), zero = 0)
maxpower <- 100

models = tibble(power=0, meanAdjRSq=0)

# will return a table with the adjusted r-squared for a function of the given
# power over each diameter of firing vector
make_lrs <- function(firing_vectors, power) {
  #formula z ~ r2 + r4 + ... + ri
  myvars <- as.formula(paste("z~r", paste(seq(2,power,by=2), collapse="+ r"), sep=""))
  
  firing_vectors %>%
    group_by(diameter) %>%
    do(lr = lm(myvars, data = .)) %>%
    mutate(intercept = coef(lr)["(Intercept)"],
           rsq=summary(lr)$adj.r.squared,
           unrsq=summary(lr)$r.squared)
}

# this will make a table of models of each succesive power
for (i in seq(2,maxpower, by=2)){
  
  # make new column with r^i
  firvecs <- firvecs %>%
    mutate(newcolumn = r^i)
  names(firvecs)[names(firvecs) == "newcolumn"] <- paste("r",toString(i), sep="")
  
  lrs_r <- make_lrs(firvecs, i)
  
  meanAdjRSq <- lrs_r %>%
    ungroup() %>%
    summarise(meanAdjRSqr = mean(rsq)) %>%
    pull()
  models <- models %>%
    add_row(power = i, meanAdjRSq = meanAdjRSq)
}

# function power with the lowest adj r squared
best_model_power <- models %>%
  filter(meanAdjRSq == max(meanAdjRSq)) %>%
  modify_at("meanAdjRSq", ~NULL) %>%
  pull()

best_model_power <- 6

# now we need to find the best power of diameters to fit our model
maxpower_d <- 100
models_d = tibble(power=0, meanAdjRSq=0)

my_r_model <- make_lrs(firvecs, best_model_power)
# add columns for every r power
for (j in seq(2,best_model_power, by=2)){
  rpow <- paste("r", j, sep="")
  my_r_model <- my_r_model %>%
    mutate(newcolumn = coef(lr)[rpow])
  names(my_r_model)[names(my_r_model) == "newcolumn"] <- rpow
}

my_r_model <- my_r_model %>%
  modify_at("lr", ~NULL)


# this will add every d^i we care about to my_r_model
for (i in seq(2,maxpower_d, by=2)){
  # make new columns with d^i
  my_r_model <- my_r_model %>%
    mutate(newcolumn = diameter^i)
  names(my_r_model)[names(my_r_model) == "newcolumn"] <- paste("d",toString(i), sep="")
}

# this will hold what power of model of coef ~ d^2 + d^4 +...+d^power for the power that
# gives us the lowest r squared adjusted for each coefficient in my_r_model
d_powers <- list()

# populate d_powers
for (mycoef in colnames(my_r_model)){
  # this will make a table of models of each succesive power
  for (i in seq(2,maxpower_d, by=2)){
    
    #formula coef ~ d2 + d4 + ... + di
    myvars <- as.formula(paste(paste(mycoef, "~d"), paste(seq(2,power,by=2), collapse="+ d"), sep=""))
    
    firing_vectors %>%
      group_by(diameter) %>%
      do(lr = lm(myvars, data = .)) %>%
      mutate(intercept = coef(lr)["(Intercept)"],
             rsq=summary(lr)$adj.r.squared)
    
    
    meanAdjRSq <- lrs_r %>%
      ungroup() %>%
      summarise(meanAdjRSqr = mean(rsq)) %>%
      pull()
    models <- models %>%
      add_row(power = i, meanAdjRSq = meanAdjRSq)
  }
  
  # function power with the lowest adj r squared
  best_model_power <- models %>%
    filter(meanAdjRSq == max(meanAdjRSq)) %>%
    modify_at("meanAdjRSq", ~NULL) %>%
    pull()
}

intercept = lm(intercept ~ diameter + d2, data=lrs_r)
r = lm(r ~ diameter + d2, data=lrs_r)
r2 = lm(r2 ~ diameter + d2, data=lrs_r)