library(data.table)
library(stargazer)
library(ggplot2)
# note: to read the date, you may need to set your working directory
EVE_char <- fread('EVE_char.csv')

# regressions
m1 <- lm(LogonHours ~ CGender + CCareer + Uage_decile ,
         data=EVE_char)
m2 <- lm(LogonHours ~ CGender + CCareer + Uage_decile + CViolence + CCorp ,
         data=EVE_char)
# formatted output
stargazer(m1,m2,single.row = FALSE,type="text",omit.stat = "f",ci=F)

summary(m1)456
