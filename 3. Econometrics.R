################################################################################
#                                                                              #
#                 Applied Labor Economics - Gender reservation wage gap        #
#                             Audin Roger & Eva Youinou                        #
#                                                                              #
################################################################################

# Input : our sample from the FH-DADS database
# Output : regression results 

library(dplyr)
library(ggplot2)
library(hdm) # Double Lasso
library(stargazer)
library(quantreg)
library(plm)
library(SortedEffects)
library(qte)


# Importing data
data <- read.csv("data_final.csv")

##################### FE-OLS regression ########################################

# Reservation wage equation

model_rw = plm(log(rw_h)~as.factor(sx), data = data, index = c("idfhda", "datins"), model = "within", effect = "time")
summary(model_rw)
model_rw_c = plm(log(rw_h)~as.factor(sx)+ age + log(past_sb_h) + married + enf + past_ce + past_cdi + contrat + duration + mobdist + exper + diplome, data = data, index = c("idfhda","datins"), model = "within", effect = "time")
summary(model_rw_c)


# Next wage equation

#model 1
model_w = plm(log(next_sb_h)~ as.factor(sx), data = data, index = c("idfhda", "datins"), model = "within", effect = "time")
summary(model_w)

#model 2
model_w_c = plm(log(next_sb_h)~ as.factor(sx) + age + log(past_sb_h) + diplome + married + enf + past_ce + past_cdi + contrat + duration + mobdist + exper, data = data, index = c("idfhda", "datins"), model = "within", effect = "time")
summary(model_w_c)

#model 3a
model_w_rw = plm(log(next_sb_h)~ as.factor(sx) + log(rw_h), data = data, index = c("idfhda", "datins"), model = "within", effect = "time")
summary(model_w_rw)

#model 3b
model_w_c_rw = plm(log(next_sb_h)~ as.factor(sx) + log(rw_h) + age + log(past_sb_h) + diplome + married + enf + past_ce + past_cdi + contrat + duration + mobdist + exper, data = data, index = c("idfhda", "datins"), model = "within", effect = "time")
summary(model_w_c_rw)

############# DOUBLE LASSO PROCEDURE ###########################################

data_collapse$diplome = ifelse(data_collapse$diplome == "D",1,0)
data_collapse$past_ce = ifelse(data_collapse$past_ce=="C",1,0)

N <- nrow(data_collapse)
list_var = c("mean_age", "mean_past_salary", "diplome","married","enf","past_ce","past_cdi","contrat","mean_duration","mean_mobility","mean_exp")
st1 <- rlasso(log(mean_rw)~ mean_age + log(mean_past_salary) + married + enf + past_ce + past_cdi + contrat + mean_duration + mean_mobility + mean_exp + diplome, data = data_collapse, penalty = list(homoscedastic = F, c=1.1, gamma = 0.1/log(N)))
#summary(st1)

n1 <- names(st1$coefficients[(st1$coefficients!=0)==T])[-1]

st2 <- rlasso(sx ~ mean_age + log(mean_past_salary) + married + enf + past_ce + past_cdi + contrat + mean_duration + mean_mobility + mean_exp + diplome, data = data_collapse, penalty = list(homoscedastic = F, c=1.1, gamma = 0.1/log(N)))
#summary(st2)

n2 <- names(st2$coefficients[(st2$coefficients!=0)==T])[-1]

selected_covariates <- c("sx",unique(c(n1,n2)))
selected_covariates["log(mean_past_salary)"] = "mean_past_salary"

sumx <- paste(selected_covariates, collapse = "+")
linear <- paste("log(mean_rw)", paste(sumx, sep = "+"), sep = "~")
linear <- as.formula(linear)

model_lasso <- lm(linear, data = data_collapse)
summary(model_lasso)



############### Sorted effects to account for heterogeneity ####################
# SE Reservation wage 
data_test = data %>% filter(is.na(data$past_cdi)== FALSE)

fm = log(rw_h)~sx*(age + log(past_sb_h) + married + enf + past_ce + past_cdi + contrat + duration + mobdist + exper + diplome)
set = spe(fm= fm, data = data_test, var = "sx", b=500)
plot(set, ylim = NULL, main = "APE and SPE of Being a woman on the reservation wage", sub = NULL, xlab = "Percentile Index", ylab = "Sorted Effects", )

# SE Model 3b => effect of final wage and reservation wage 
se_model3b =   log(next_sb_h)~sx*(age + log(past_sb_h) + log(rw_h) + married + enf + past_ce + past_cdi + contrat + duration + mobdist + exper + diplome)
model3b = spe(fm= se_model3b, data = data_test, var = "sx", b=500)
plot(model3b, ylim = NULL, main = "APE and SPE of Being a woman on the final wage", sub = NULL, xlab = "Percentile Index", ylab = "Sorted Effects", )

# SE Model 2
se_model_test =   log(next_sb_h)~sx*(age + log(past_sb_h) + married + past_cdi + enf + past_ce + contrat + duration + mobdist + exper + diplome)
model_test = spe(fm= se_model_test, data = data_test, var = "sx", b=500)
plot(model_test, ylim = NULL, main = "APE and SPE of Being a woman on the final wage", sub = NULL, xlab = "Percentile Index", ylab = "Sorted Effects", )

########################### Building of data collapse ##########################
data_collapse <- data %>%
  group_by(idfhda) %>%
  summarise(mean_rw = mean(rw_h), mean_age = mean(age), mean_next_salary = mean(next_sb_h), mean_mobility = mean(mobdist), mean_past_salary = mean(past_sb_h), mean_exp = mean(exper), mean_duration = mean(duration))

data$past_job <- as.Date(data$past_job)
data_grouped <- data %>%
  group_by(idfhda) %>%
  slice_min(past_job) %>%
  select(idfhda, sx, past_cdi, diplome, married, enf, past_ce, contrat)

data_collapse <- data_collapse %>%
  left_join(data_grouped,by=c("idfhda"))

data_collapse <- data_collapse %>% filter(is.na(data_collapse$past_cdi)== FALSE)

############# QUANTILE REGRESSION ##############################################

# Reservation wage equation
quant_reservation_wage_reg = rq(log(mean_rw) ~ sx + mean_age + diplome  + enf + log(mean_past_salary) + married + enf + past_ce + past_cdi + contrat + mean_duration + mean_mobility + mean_exp + diplome , tau = c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9), data = data_collapse, method = "br")
#summary(quant__reservation_wage_reg)

# Wage Equation

#Model 2
quant_wage_reg_without_rw = rq(log(mean_next_salary) ~ sx + mean_age + diplome + enf + log(mean_past_salary) + married + enf + past_ce + past_cdi + contrat + mean_duration + mean_mobility + mean_exp + diplome , tau = c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9), data = data_collapse, method = "br")
#summary(quant_wage_reg_without_rw)

#Model 3b
quant_wage_reg = rq(log(mean_next_salary) ~ sx + mean_age + diplome + log(mean_rw) + enf + log(mean_past_salary) + married + enf + past_ce + past_cdi + contrat + mean_duration + mean_mobility + mean_exp + diplome , tau = c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9), data = data_collapse, method = "br")
summary(quant_wage_reg)

# Plotting coefficients of regressions
# Reservation wage equation 
coef = as.data.frame(quant_reservation_wage_reg$coefficients)
coef = as.data.frame(t(coef["sx",]))
coef$quantile = seq(1:9)
colnames(coef) = c("Coefficient","Quantile")
ggplot(coef, aes(x=Quantile, y=Coefficient))+
  geom_line() + labs(x= "Quantiles", y= "Gender reservation wage gap", title = "Reservation wage gap per quantile") +
  theme_classic() + scale_x_continuous(breaks = seq(1,10,by=1))

# Model 2
coef = as.data.frame(quant_wage_reg_without_rw$coefficients)
coef = as.data.frame(t(coef["sx",]))
coef$quantile = seq(1:9)
colnames(coef) = c("Coefficient","Quantile")
ggplot(coef, aes(x=Quantile, y=Coefficient))+
  geom_line() + labs(x= "Quantiles", y= "Gender wage gap", title = "Gender wage gap per quantile (without reservation wage)") +
  theme_classic() + scale_x_continuous(breaks = seq(1,10,by=1))

# Model 3b
coef = as.data.frame(quant_wage_reg$coefficients)
coef = as.data.frame(t(coef["sx",]))
coef$quantile = seq(1:9)
colnames(coef) = c("Coefficient","Quantile")
ggplot(coef, aes(x=Quantile, y=Coefficient))+
  geom_line() + labs(x= "Quantiles", y= "Gender wage gap", title = "Gender wage gap per quantile") +
  theme_classic() + scale_x_continuous(breaks = seq(1,10,by=1))

########################### QTE Regressions ####################################
# Reservation wage 
rw_QTE_raw <- ci.qtet(log(mean_rw)~sx, data= data_collapse, probs = seq(0.05, 0.95, 0.05), se = T, iters=10)
class(rw_QTE_raw)
summary(rw_QTE_raw)
ggqte(rw_QTE_raw)

# Reservation wage with controls
rw_QTE <- ci.qtet(log(mean_rw)~sx, xformla = ~ mean_age + past_cdi +log(mean_past_salary) + diplome + married + enf + past_ce + contrat + mean_duration + mean_mobility + mean_exp, data= data_collapse, probs = seq(0.05, 0.95, 0.05), se = T, iters=10)
class(rw_QTE)
summary(rw_QTE)
ggqte(rw_QTE)

# Model 1
reg_QTE_m1 <- ci.qtet(log(mean_next_salary)~sx, data= data_collapse, probs = seq(0.05, 0.95, 0.05), se = T, iters=10)
summary(reg_QTE_m1)
ggqte(reg_QTE_m1)

# Model 2
reg_QTE_m2 <- ci.qtet(log(mean_next_salary)~sx, xformla = ~ mean_age + past_cdi +log(mean_past_salary) + diplome + married + enf + past_ce + contrat + mean_duration + mean_mobility + mean_exp, data= data_collapse, probs = seq(0.05, 0.95, 0.05), se = T, iters=10)
summary(reg_QTE_m2)
ggqte(reg_QTE_m2)

# Model 3a
reg_QTE_m3a <- ci.qtet(log(mean_next_salary)~ sx, xformla= ~ log(mean_rw), data= data_collapse, probs = seq(0.05, 0.95, 0.05), se = T, iters=10)
summary(reg_QTE_m3a)
ggqte(reg_QTE_m3a)

# Model 3b
reg_QTE_m3b <- ci.qtet(log(mean_next_salary)~ sx, xformla = ~ log(mean_rw) + mean_age + past_cdi + log(mean_past_salary) + diplome + married + enf + past_ce + contrat + mean_duration + mean_mobility + mean_exp, data = data_collapse, probs = seq(0.05, 0.95, 0.05), se = T, iters = 10)
summary(reg_QTE_m3b)
ggqte(reg_QTE_m3b)