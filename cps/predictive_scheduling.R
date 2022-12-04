library(haven)
library(dplyr)
library(ggplot2)
library(Hmisc)

cps <- read_dta('cps.dta')

# standard deviation function
weighted.sd <- function(x, wt, na.rm = FALSE) {
  return(sqrt(wtd.var(x, wt, na.rm = na.rm)))
}	

# CLEANING
cps_clean <- cps[!(cps$firmsize == 0) & !(is.na(cps$firmsize)),]
cps_oregon <- cps_clean[(cps_clean$statefip == 41),] #41 is oregon

# treated if firm size > 500 and in retail, hospitality, food services industries
# firm size codes 
# industry codes:
# - 2014-2019 1190, 4690-5590, 8660-8670
# - 2009-2013 1190, 4690-5590, 8660-8670
# - 2003-2008 1190, 4690-5590, 8660-8670
# - 1992-2002 580-662, 681-691, 762-770
cps_oregon$treatind <- ifelse(cps_oregon$year >= 2003,
                         ifelse((cps_oregon$ind == 1190 
                                 |(cps_oregon$ind >= 4690 & cps_oregon$ind <= 5590) 
                                 | (cps_oregon$ind >= 8660 & cps_oregon$ind <= 8670)),
                                1, 0),
                         ifelse((cps_oregon$ind >= 580 & cps_oregon$ind <= 662
                                 |(cps_oregon$ind >= 681 & cps_oregon$ind <= 691) 
                                 | (cps_oregon$ind >= 762 & cps_oregon$ind <= 770)),
                                1, 0))
cps_oregon$treat <- ifelse((cps_oregon$treatind == 1) & (cps_oregon$firmsize >= 8), 1, 0)

# education levels
cps_oregon$school[cps_oregon$educ == 0] <- NA
cps_oregon$school[cps_oregon$educ == 1] <- NA
cps_oregon$school[cps_oregon$educ == 999] <- NA
cps_oregon$school[cps_oregon$educ == 2] <- 0
cps_oregon$school[cps_oregon$educ == 10] <- 2.5
cps_oregon$school[cps_oregon$educ == 11] <- 1 
cps_oregon$school[cps_oregon$educ == 12] <- 2 
cps_oregon$school[cps_oregon$educ == 13] <- 3 
cps_oregon$school[cps_oregon$educ == 14] <- 4
cps_oregon$school[cps_oregon$educ == 20] <- 5.5 
cps_oregon$school[cps_oregon$educ == 21] <- 5
cps_oregon$school[cps_oregon$educ == 22] <- 6 
cps_oregon$school[cps_oregon$educ == 31] <- 7 
cps_oregon$school[cps_oregon$educ == 32] <- 8
cps_oregon$school[cps_oregon$educ == 40] <- 9 
cps_oregon$school[cps_oregon$educ == 50] <- 10
cps_oregon$school[cps_oregon$educ == 60] <- 11
cps_oregon$school[cps_oregon$educ == 70] <- 12
cps_oregon$school[cps_oregon$educ == 71] <- 12
cps_oregon$school[cps_oregon$educ == 72] <- 12
cps_oregon$school[cps_oregon$educ == 73] <- 12
cps_oregon$school[cps_oregon$educ == 80] <- 13
cps_oregon$school[cps_oregon$educ == 81] <- 13
cps_oregon$school[cps_oregon$educ == 90] <- 14
cps_oregon$school[cps_oregon$educ == 91] <- 14
cps_oregon$school[cps_oregon$educ == 92] <- 14
cps_oregon$school[cps_oregon$educ == 100] <- 15
cps_oregon$school[cps_oregon$educ == 111] <- 16
cps_oregon$school[cps_oregon$educ == 121] <- 17
cps_oregon$school[cps_oregon$educ == 122] <- 17
cps_oregon$school[cps_oregon$educ == 123] <- 18
cps_oregon$school[cps_oregon$educ == 124] <- 18
cps_oregon$school[cps_oregon$educ == 125] <- 21

cps_oregon_clean <- cps_oregon # clean dataset with full variables, can use to revert

# EXPLORATION
# enacted july 2018, cps survey is in march, so first treated year is 2019 — but firms could be changing schedules in advance to prepare for the change
# explore hours only for employed workers 
cps_oregon <- cps_oregon_clean
cps_oregon <- cps_oregon[(cps_oregon$empstat == 10) | (cps_oregon$empstat == 12),] # at work or has work and not at job last week

# uhrsworkt: 997 if hours vary: plot fraction of workers who report hours vary 
cps_oregon$hrs_vary <- ifelse(cps_oregon$uhrsworkt == 997, 1, 0)
cps_oregon_hrs_vary <- cps_oregon %>% group_by(year, treat) %>% summarise(ahrs_vary = weighted.mean(hrs_vary, asecwt))
cps_oregon_hrs_vary %>%
  ggplot(aes(x=year, y=ahrs_vary, group=treat, color=treat)) +
  geom_line()
# 5% to 15% of workers report that their hours vary
# in the treated group, the % of workers who report that their hours vary fluctuates wildly from year to year, could be a reporting issue
# using report that hours vary is probably not a good measure

# uhrsworkt, ahrsworkt: drop if hours worked is 999 for not in universe or 997 for hours vary 
cps_oregon <- cps_oregon[!(cps_oregon$ahrsworkt == 999) & !(cps_oregon$uhrsworkt >= 997),]

# calculate hours volatility
cps_oregon$hrsvol <- abs(cps_oregon$ahrsworkt - cps_oregon$uhrsworkt)
cps_oregon$hrsvolpct <- abs(cps_oregon$ahrsworkt - cps_oregon$uhrsworkt)/cps_oregon$uhrsworkt
cps_oregon$hrsvol2 <- (cps_oregon$ahrsworkt - cps_oregon$uhrsworkt)^2
cps_oregon$hrsvolpos <- ifelse(((cps_oregon$ahrsworkt - cps_oregon$uhrsworkt) >= 0), (cps_oregon$ahrsworkt - cps_oregon$uhrsworkt), NA)
cps_oregon$hrsvolpospct <- ifelse(((cps_oregon$ahrsworkt - cps_oregon$uhrsworkt) >= 0), (cps_oregon$ahrsworkt - cps_oregon$uhrsworkt)/cps_oregon$uhrsworkt, NA)
cps_oregon$hrsvolneg <- ifelse(((cps_oregon$ahrsworkt - cps_oregon$uhrsworkt) <= 0), (cps_oregon$ahrsworkt - cps_oregon$uhrsworkt), NA)
cps_oregon$hrsvolnegpct <- ifelse(((cps_oregon$ahrsworkt - cps_oregon$uhrsworkt) <= 0), (cps_oregon$ahrsworkt - cps_oregon$uhrsworkt)/cps_oregon$uhrsworkt, NA)

# plot hours, hours volatility for average worker in each group 
cps_oregon_avg <- cps_oregon %>% group_by(year, treat) %>% summarise(ahrs = weighted.mean(ahrsworkt, asecwt), 
                                                                     auhrs = weighted.mean(uhrsworkt, asecwt), 
                                                                     ahrsvol = weighted.mean(hrsvol, asecwt),
                                                                     ahrsvolpct = weighted.mean(hrsvolpct, asecwt),
                                                                     ahrsvol2 = weighted.mean(hrsvol2, asecwt), 
                                                                     ahrsvolpos = weighted.mean(hrsvolpos, asecwt, na.rm = TRUE),
                                                                     ahrsvolpospct = weighted.mean(hrsvolpospct, asecwt, na.rm = TRUE), 
                                                                     ahrsvolneg = weighted.mean(hrsvolneg, asecwt, na.rm = TRUE), 
                                                                     ahrsvolnegpct = weighted.mean(hrsvolnegpct, asecwt, na.rm = TRUE))

cps_oregon_avg %>%
  ggplot(aes(x=year, y=ahrs, group=treat, color=treat)) +
  geom_line()
cps_oregon_avg %>%
  ggplot(aes(x=year, y=auhrs, group=treat, color=treat)) +
  geom_line()
cps_oregon_avg %>%
  ggplot(aes(x=year, y=ahrsvol, group=treat, color=treat)) +
  geom_line()
cps_oregon_avg %>%
  ggplot(aes(x=year, y=ahrsvolpct, group=treat, color=treat)) +
  geom_line()
cps_oregon_avg %>%
  ggplot(aes(x=year, y=ahrsvol2, group=treat, color=treat)) +
  geom_line()
cps_oregon_avg %>%
  ggplot(aes(x=year, y=ahrsvolpos, group=treat, color=treat)) +
  geom_line()
cps_oregon_avg %>%
  ggplot(aes(x=year, y=ahrsvolpospct, group=treat, color=treat)) +
  geom_line()
cps_oregon_avg %>%
  ggplot(aes(x=year, y=ahrsvolneg, group=treat, color=treat)) +
  geom_line()
cps_oregon_avg %>%
  ggplot(aes(x=year, y=ahrsvolnegpct, group=treat, color=treat)) +
  geom_line()

# compare characteristics of workers in treated and untreated groups 
# count treatment and control
cps_oregon_treatct <- cps_oregon %>% group_by(treat) %>% tally(asecwt)

# mean and stdev of hours worked, education, gender, age
cps_oregon_worker <- cps_oregon %>% group_by(treat) %>% summarise(ahrswork = weighted.mean(ahrsworkt, asecwt), 
                                                                 sdhrswork = weighted.sd(ahrsworkt, asecwt),
                                                                 auhrswork = weighted.mean(uhrsworkt, asecwt),
                                                                 sduhrswork = weighted.sd(uhrsworkt, asecwt), 
                                                                 aschool = weighted.mean(school, asecwt, na.rm = TRUE), 
                                                                 sdschool = weighted.sd(school, asecwt, na.rm = TRUE), 
                                                                 aage = weighted.mean(age, asecwt), 
                                                                 asex = weighted.mean(sex, asecwt))

# DiD regression of hours volatility, controlling for characteristics of workers in treated and untreated groups
cps_oregon$timetreat <- ifelse(cps_oregon$year >= 2019, 1, 0)
cps_oregon$year <- factor(cps_oregon$year) 

reghrsvol <- lm(hrsvol ~ timetreat + treat + timetreat * treat + school + age + sex + year, cps_oregon, weights = asecwt)
summary(reghrsvol) # estimate -0.864, se 0.043, sig 0.05 — seems potentially promising

reghrsvol2 <- lm(hrsvol2 ~ timetreat + treat + timetreat * treat + school + age + sex + year, cps_oregon, weights = asecwt)
summary(reghrsvol2) # estimate -22.41, se 12.62, sig 0.1

reghrsvolpos<- lm(hrsvolpos~ timetreat + treat + timetreat * treat + school + age + sex + year, cps_oregon, weights = asecwt)
summary(reghrsvolpos) # estimate -0.179, se 0.287, sig 1

reghrsvolneg<- lm(hrsvolneg~ timetreat + treat + timetreat * treat + school + age + sex + year, cps_oregon, weights = asecwt)
summary(reghrsvolneg) # estimate 0.778, se 0.419, sig 0.1

cps_oregon[is.infinite(cps_oregon$hrsvolpct),] <- NA
reghrsvolpct <- lm(hrsvolpct ~ timetreat + treat + timetreat * treat + school + age + sex + year, cps_oregon, weights = asecwt)
summary(reghrsvolpct) # estimate -0.024, se 0.022, sig 1


# Employment analysis 
# Total employment in treatment vs untreated group over time
# Issue is I cannot see who was employed in a valid firm in the previous year because these are cross-sections
cps_oregon <- cps_oregon_clean
cps_oregon$emp <- ifelse((cps_oregon$empstat == 10) | (cps_oregon$empstat == 12), 1, 0)
cps_oregon$unemp <- ifelse((cps_oregon$empstat >= 20) & (cps_oregon$empstat <= 22), 1, 0)
cps_oregon$nilf <- ifelse((cps_oregon$empstat >= 30) & (cps_oregon$empstat <= 36), 1, 0)
cps_oregon_emp <- cps_oregon %>% group_by(year, treat) %>% summarize(aemp = weighted.mean(emp, asecwt), 
                                                                  aunemp = weighted.mean(unemp, asecwt), 
                                                                  anilf = weighted.mean(nilf, aswecwt))
cps_oregon_emp %>%
  ggplot(aes(x=year, y=aemp, group=treat, color=treat)) +
  geom_line()
cps_oregon_emp %>%
  ggplot(aes(x=year, y=aunemp, group=treat, color=treat)) +
  geom_line()
cps_oregon_emp %>%
  ggplot(aes(x=year, y=anilf, group=treat, color=treat)) +
  geom_line()

# Part time vs full time in treatment vs untreated group over time 



