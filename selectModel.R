library(tidyverse)
library(rgl)

firvecs <- read_csv("~/thesis/firingvectors.csv")

firvecs <- firvecs %>%
  mutate(r = sqrt(x^2 + y^2))
maxpower <- 100

models = tibble(power=0, meanAdjRSq=0)

# this will make a table of models of each succesive power
for (i in seq(2,maxpower, by=2)){
  
  # make new columns with r and d to our power
  firvecs <- firvecs %>%
    mutate(newcolumn = r^i)
  names(firvecs)[names(firvecs) == "newcolumn"] <- paste("r",toString(i), sep="")
  firvecs <- firvecs %>%
    mutate(newcolumn = diameter^i)
  names(firvecs)[names(firvecs) == "newcolumn"] <- paste("d",toString(i), sep="")
  
  #formula z ~ r2 + r4 + ... + ri
  myvars <- as.formula(paste("z~r", paste(seq(2,i,by=2), collapse="+ r"), sep=""))
  
  lrs_r <- firvecs %>%
    group_by(diameter) %>%
    do(lr = lm(myvars, data = .)) %>%
    mutate(intercept = coef(lr)["(Intercept)"],
           rsq=summary(lr)$adj.r.squared)
  # add columns for every r power
  for (j in seq(2,i, by=2)){
    rpow <- paste("r", j, sep="")
    lrs_r <- lrs_r %>%
      mutate(newcolumn = coef(lr)[rpow])
    names(lrs_r)[names(lrs_r) == "newcolumn"] <- rpow
  }
  meanAdjRSq <- lrs_r %>%
    ungroup() %>%
    summarise(meanAdjRSqr = mean(rsq)) %>%
    pull()
  models <- models %>%
    add_row(power = i, meanAdjRSq = meanAdjRSq)
}

intercept = lm(intercept ~ diameter + d2, data=lrs_r)
r = lm(r ~ diameter + d2, data=lrs_r)
r2 = lm(r2 ~ diameter + d2, data=lrs_r)