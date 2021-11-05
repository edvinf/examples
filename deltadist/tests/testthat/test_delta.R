
n <- 91
m <- 41
ybar <- 2.09
ssq <- 4.41

mean_est <- mean_c(m,n,ybar,ssq)
expect_true(abs(mean_est - 28.4) < .1)

var_est <- var_est(m,n,ybar,ssq)
expect_true(abs(var_est - 187) < 1)

mean_est <- delta_mean(fishmethods::catch$value)
var_est <- var_delta_mean(fishmethods::catch$value)
fishmethod_est <- fishmethods::deltadist(fishmethods::catch$value)
expect_true(abs(mean_est - fishmethod_est[1])/mean_est < 1e-6)
expect_true(abs(var_est - fishmethod_est[2])/var_est < 1e-6)
