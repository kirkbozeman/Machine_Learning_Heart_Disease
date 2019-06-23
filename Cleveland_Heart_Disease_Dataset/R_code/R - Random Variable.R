
############################# FAKE DATA OF RVs #############################

# use for testing R code

data = data.frame(num_binary = as.factor(sample(c(0, 1), 200, replace=T)),
                  sex = as.factor(sample(c(0, 1), 200, replace=T)),
                  cp = as.factor(sample(c(0, 4), 200, replace=T)),
                  fbs = as.factor(sample(c(0, 1), 200, replace=T)),
                  restecg = as.factor(sample(c(0, 1), 200, replace=T)),
                  exang = as.factor(sample(c(0, 1), 200, replace=T)),
                  slope = as.factor(sample(c(1,3), 200, replace=T)),
                  thal = as.factor(sample(c(3,6,7), 200, replace=T)),
                  ca = as.factor(sample(c(1, 3), 200, replace=T)),
                  age = rnorm(100, 55, 9),
                  trestbps = rnorm(100, 130, 15),
                  chol = rnorm(100, 250, 50),
                  thalach = rnorm(100, 150, 25),
                  oldpeak = rnorm(100, 1.5, 1))

data
show(data)