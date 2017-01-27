library(readr)
ho_grps_3 <- read_csv("ho_grps_3.csv")
he_grps_3 <- read_csv("he_grps_3.csv")

ho_grps_5 <- read_csv("ho_grps_5.csv")
he_grps_5 <- read_csv("he_grps_5.csv")

ho_grps_7 <- read_csv("ho_grps_7.csv")
he_grps_7 <- read_csv("he_grps_7.csv")

summary(ho_grps_3)

library(data.table)
library(dplyr)
ho_grps_3 <- ho_grps_3 %>% select(-1)
he_grps_3 <- he_grps_3 %>% select(-1)
ho_grps_5 <- ho_grps_5 %>% select(-1)
he_grps_5 <- he_grps_5 %>% select(-1)
ho_grps_7 <- ho_grps_7 %>% select(-1)
he_grps_7 <- he_grps_7 %>% select(-1)

c_names <- c("group", "LM", "MP", "AV", "MU")
colnames(ho_grps_3) <- c_names
colnames(ho_grps_5) <- c_names
colnames(ho_grps_7) <- c_names
colnames(he_grps_3) <- c_names
colnames(he_grps_5) <- c_names
colnames(he_grps_7) <- c_names

# Means and SD
ho3 <- sapply(ho_grps_3[,-1], function(cl) list(means=round(mean(cl,na.rm=TRUE),2), sds=round(sd(cl,na.rm=TRUE),2)))
he3 <- sapply(he_grps_3[,-1], function(cl) list(means=round(mean(cl,na.rm=TRUE),2), sds=round(sd(cl,na.rm=TRUE),2)))
ho5 <- sapply(ho_grps_5[,-1], function(cl) list(means=round(mean(cl,na.rm=TRUE),2), sds=round(sd(cl,na.rm=TRUE),2)))
he5 <- sapply(he_grps_5[,-1], function(cl) list(means=round(mean(cl,na.rm=TRUE),2), sds=round(sd(cl,na.rm=TRUE),2)))
ho7 <- sapply(ho_grps_7[,-1], function(cl) list(means=round(mean(cl,na.rm=TRUE),2), sds=round(sd(cl,na.rm=TRUE),2)))
he7 <- sapply(he_grps_7[,-1], function(cl) list(means=round(mean(cl,na.rm=TRUE),2), sds=round(sd(cl,na.rm=TRUE),2)))


ho3
ho5
ho7

he3
he5
he7

colnames(ho_grps_3)

#ANOVA for ho_grps_3
ho3LM <- ho_grps_3[,"LM"]
ho3MP <- ho_grps_3[,"MP"]
ho3AV <- ho_grps_3[,"AV"]
ho3MU <- ho_grps_3[,"MU"]
dati <- c(ho3LM, ho3MP, ho3AV, ho3MU)
datu <- c("ho3LM", "ho3MP", "ho3AV", "ho3MU")
groups = factor(rep(datu, each = 20))

bartlett.test(dati, groups)


fit = lm(formula = dati ~ groups)


#THIS IS IT
test = he_grps_7

bartlett.test(value~variable, melt(test))
res_val <- aov(value~variable, melt(test))
summary(res_val)

TukeyHSD(res_val, which="variable")
