context("Check ABC function")
source("project.Rmd")


# test data generation

qc = 0.25
qh = 0.75

model_1(j = 1, s = 3, qc = qc, qh =  qh, wjj =  (3/4))
data_gen(qc = 0.25, qh = 0.75, m = 4, n =3)

test1 = data_gen(qc = 0.25, qh = 0.75, m = 4, n =3)

onethree = choose(3,1) * (3/4) * (qc * qh^1)^2
twothree = choose(3,2) * (27/32) * (qc * qh^2)^1
test = matrix(c(0.25, 0.75, NA, NA,
                (1/16), (3/32), (27/32), NA,
                (1/64), onethree, twothree, (1 - onethree - twothree - (1/64))),
              ncol = 3, nrow = 4)

test_that("data generation works", {
  expect_equal(test, test1)
})



#this is to see a good epsilon for 3a
test_f <- function() {
  test = simulate_parameters()
  res1 = data_gen(test$qc1, test$qh1,6,5)
  res2 = data_gen(test$qc2, test$qh2,6,5)
  
  d = distance(d1 = table_2_1979[, -1],
               d2 = table_2_1980[, -1],
               d_star1 = res1,
               d_star2 = res2)
  
  return(d)
}

x = replicate(1000, test_f())
quantile(x, seq(0.01, 0.2, 0.01))

test_that("good epsilon for 3a", {
  
})



#this is to see a good epsilon for 3c
test_f <- function(d1,d2) {
  d1 = d1[, -1]
  d2 = d2[, -1]
  dim_d1 = dim(d1)
  dim_d2 = dim(d2)
  prior = simulate_parameters() 
  
  # generate data
  res1 = data_gen(prior$qc1, prior$qh1, m = dim_d1[1], n = dim_d1[2])
  res2 = data_gen(prior$qc2, prior$qh2, m = dim_d2[1], n = dim_d2[2])
  
  distance = distance(d1,
                      d2,
                      d_star1 = res1,
                      d_star2 = res2)
}

x = replicate(1000, test_f(d1 = table_3_1975, d2 = table_3_1978))
quantile(x, seq(0.01, 0.2, 0.01))

test_that("good epsilon for 3c", {

})
