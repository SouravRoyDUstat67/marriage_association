library(foreign)
library(dplyr)

data= read.spss("G:/qmh statistics club/bdhs2014-updated.sav") %>% as.data.frame()
dim(data)

# Weighted percentage calculation----------------------
div_weight= data %>% select(division) %>% mutate(x= 1) %>% 
  group_by(division) %>% summarise(count= sum(x)) %>% 
  mutate(weight= count/sum(count)) %>% select(-count)

data_weight= data %>% select(degree, degree_spouse, division) %>%
  filter(degree_spouse != 9) %>%
  mutate(x= 1) %>%
  group_by(division, degree_spouse, degree) %>%
  summarise(count= sum(x)) %>% 
  left_join(div_weight, by= c("division" = "division"))

data_weight %>% group_by(degree_spouse, degree) %>%
  summarise(count= sum(weight* count)) %>% 
  mutate(percentage= scales::percent(count/sum(count))) %>% select(-count)

# Ho: don't have any association
# H1: has any association bet. husband's edu. sts & wife's edu. sts

d= data %>% select(degree_spouse, degree) %>%
  mutate(degree_spouse= as.character(degree_spouse))%>%
  filter(degree_spouse != "9")

test= chisq.test(table(d$degree, d$degree_spouse))
test$expected
test$observed
