#math.general = read.csv("C:/Users/jbrin/Documents/math/fall.19_23.ssc.math.csv",header=T)
math.general = read.csv("C:/Users/jon/OneDrive/Documents/Spring 2024/fall.19_23_2.ssc.math.csv",header=T)
math.general = read.csv("C:/Users/jon/OneDrive/Documents/Spring 2024/fall.19_23.ssc.math.csv",header=T)

# df.dup = data.frame(table(math.general$Student_ID ))
# df.dups <- df.dup %>% filter(Freq >1 )

# math.general$tutor[math.general$tutor=='no'] <- 0
# math.general$tutor[math.general$tutor=='yes'] <- 1
# math.general$tutor <- as.numeric(math.general$tutor)
# math.general$tutor <- as.factor(math.general$tutor)
# 
# character_columns <- sapply(math.general, is.character)
# math.general[character_columns] <- lapply(math.general[character_columns], as.factor)

math.general$pass_rate <- math.general$passed/math.general$enrolled

math.general$ret.f2f[is.na(math.general$ret.f2f)] <- 0
math.general$pers.f2s[is.na(math.general$pers.f2s)] <- 0

library(stats)
math.general$session.count = as.factor(math.general$session.count)
contrasts(math.general$session.count)
math.general$session.count <- relevel(math.general$session.count, ref = "none")
math.general$first_gen = as.factor(math.general$first_gen)
contrasts(math.general$first_gen)
math.general$first_gen <- relevel(math.general$first_gen, ref = "Not First Generation")

gpa <- cut(math.general$CUM_GPA, breaks=c(0,2,3,4), labels=c('low', 'mid', 'high'))
math.general = cbind(math.general,gpa)
math.general$gpa[is.na(math.general$gpa)] <- "low"
math.general$gpa = as.factor(math.general$gpa)

#collapse session counts
math.general$session.count[math.general$session.count == "rare"] <- "occasionally"
math.general$session.count[math.general$session.count == "very.frequently"] <- "frequently"
math.general$session.count = as.character(math.general$session.count)
math.general$session.count = as.factor(math.general$session.count)

#collapse ethnicity
math.general$ethnicity[math.general$ethnicity == "Black or African American"] <- "underrepresented"
math.general$ethnicity[math.general$ethnicity == "Native American or Pacific Islander"] <- "underrepresented"
math.general$ethnicity[math.general$ethnicity == "Two or More Races"] <- "white/two or more races"
math.general$ethnicity[math.general$ethnicity == "White"] <- "white/two or more races"
math.general$ethnicity = as.character(math.general$ethnicity)
math.general$ethnicity = as.factor(math.general$ethnicity)

#########################Add course information for 2019
#########################Remove students taking multiple classes to compare to embedded
library(dplyr)
library(stringr)
math.oneclass.19 = math.general %>% filter(str_detect(STRM, '199')) %>% filter(enrolled == 1) #4516 observations
#########################obtain course info
courses.2019 = enrollment %>% filter(str_detect(STRM, '199')) %>% filter(str_detect(SUBJECT, 'MATH')) %>% filter(enrolled == TRUE) %>% filter(Student_ID %in% math.oneclass.19$Student_ID)
df.dup = data.frame(table(courses.2019$Student_ID ))
df.dups <- df.dup %>% filter(Freq > 1)
courses.2019 <- courses.2019 %>% filter(!(Student_ID %in% df.dups$Var1)) #4040 observations

math.oneclass.19 = math.oneclass.19 %>% filter(Student_ID %in% courses.2019$Student_ID) #make observations match

#add course information
math.oneclass.19 = math.oneclass.19 %>% arrange(Student_ID)
courses.2019 = courses.2019 %>% arrange(Student_ID)
math.oneclass.19 = cbind(math.oneclass.19,courses.2019$CATALOG_NBR,courses.2019$DESCR)

#########################Add course information for 2020
#########################Remove students taking multiple classes to compare to embedded
library(dplyr)
library(stringr)
math.oneclass.20 = math.general %>% filter(str_detect(STRM, '209')) %>% filter(enrolled == 1) #4271 observations
#########################obtain course info
courses.2020 = enrollment %>% filter(str_detect(STRM, '209')) %>% filter(str_detect(SUBJECT, 'MATH')) %>% filter(Student_ID %in% math.oneclass.20$Student_ID) %>% filter(enrolled == TRUE)
df.dup = data.frame(table(courses.2020$Student_ID ))
df.dups <- df.dup %>% filter(Freq > 1)
courses.2020 <- courses.2020 %>% filter(!(Student_ID %in% df.dups$Var1)) #4040 observations

math.oneclass.20 = math.oneclass.20 %>% filter(Student_ID %in% courses.2020$Student_ID) #make observations match

#add course information
math.oneclass.20 = math.oneclass.20 %>% arrange(Student_ID)
courses.2020 = courses.2020 %>% arrange(Student_ID)
math.oneclass.20 = cbind(math.oneclass.20,courses.2020$CATALOG_NBR,courses.2020$DESCR)

#########################Add course information for 2021
#########################Remove students taking multiple classes to compare to embedded
library(dplyr)
library(stringr)
math.oneclass.21 = math.general %>% filter(str_detect(STRM, '219')) %>% filter(enrolled == 1) #3552 observations
#########################obtain course info
courses.2021 = enrollment %>% filter(str_detect(STRM, '219')) %>% filter(str_detect(SUBJECT, 'MATH')) %>% filter(Student_ID %in% math.oneclass.21$Student_ID) %>% filter(enrolled == TRUE) #3175 observations
# df.dup = data.frame(table(courses.2021$Student_ID ))
# df.dups <- df.dup %>% filter(Freq > 1)
# courses.2020 <- courses.2020 %>% filter(!(Student_ID %in% df.dups$Var1)) #4040 observations

math.oneclass.21 = math.oneclass.21 %>% filter(Student_ID %in% courses.2021$Student_ID) #make observations match

#add course information
math.oneclass.21 = math.oneclass.21 %>% arrange(Student_ID)
courses.2021 = courses.2021 %>% arrange(Student_ID)
math.oneclass.21 = cbind(math.oneclass.21,courses.2021$CATALOG_NBR,courses.2021$DESCR)
# math.general$tutor[math.general$tutor == "yes"] = 1
# math.general$tutor[math.general$tutor == "no"] = 0
# math.general$tutor = as.factor(math.general$tutor)

# first.gen = demographics %>% filter(Student_ID %in% math.general$math_stu_id) %>% arrange(Student_ID) %>% dplyr::select(first_gen)
# gender = demographics %>% filter(Student_ID %in% math.general$math_stu_id) %>% arrange(Student_ID) %>% dplyr::select(gender)

#names(math.general)[names(math.general) == "math_stu_id"] <- "Student_ID"
# gen.fg <- demographics %>% dplyr::select(gender,first_gen,Student_ID)
# gen.fg$Student_ID <- as.numeric(gen.fg$Student_ID)
# math.general2 <- inner_join(gen.fg,math.general,"Student_ID")

# write.csv(math.general2,"C:/Users/jbrin/Documents/math/fall.19_23_2.ssc.math.csv",row.names = F)
# math.general = math.general2
# 
# balanced_df = math.general %>% 
#   filter(tutor == "yes") %>%
#   bind_rows(math.general %>% 
#               filter(tutor=="no") %>% 
#               sample_n(4312, replace = FALSE))
# library(dplyr)
# balanced_df2 = balanced_df %>% slice_sample(n = nrow(.), replace=F)

# math.general = balanced_df
# 
# library(ROSE)
# balanced_sample = NULL
# for (c in unique(df$tutor)) {
#   tmp_df = df%>%filter(tutor==c)
#   tmp<-ovun.sample(Click ~ ., data = tmp_df, method = "under", p = 0.6, seed = 5)$data
#   balanced_sample<-rbind(balanced_sample, tmp)
# } 

#separate data by term
library(dplyr)
math.2019 = math.general %>% filter(STRM == 1199)
math.2020 = math.general %>% filter(STRM == 1209)
math.2021 = math.general %>% filter(STRM == 1219)
math.2022 = math.general %>% filter(STRM == 1229)
math.2023 = math.general %>% filter(STRM == 1239)

set.seed(123)
#2019 balanced
tutored = math.2019 %>% filter(tutor == "yes")
not.tutored = math.2019 %>% filter(tutor == "no")
not.tutored <- not.tutored[sample(1:nrow(not.tutored), size=(2*dim(tutored)[1])),]
math.balanced = rbind(tutored,not.tutored)

math.2019 = math.balanced

#2020 balanced
tutored = math.2020 %>% filter(tutor == "yes")
not.tutored = math.2020 %>% filter(tutor == "no")
not.tutored <- not.tutored[sample(1:nrow(not.tutored), size=(2*dim(tutored)[1])),]
math.balanced = rbind(tutored,not.tutored)

math.2020 = math.balanced

#2021 balanced
tutored = math.2021 %>% filter(tutor == "yes")
not.tutored = math.2021 %>% filter(tutor == "no")
not.tutored <- not.tutored[sample(1:nrow(not.tutored), size=(2*dim(tutored)[1])),]
math.balanced = rbind(tutored,not.tutored)

math.2021 = math.balanced

#2022 balanced
tutored = math.2022 %>% filter(tutor == "yes")
not.tutored = math.2022 %>% filter(tutor == "no")
not.tutored <- not.tutored[sample(1:nrow(not.tutored), size=(2*dim(tutored)[1])),]
math.balanced = rbind(tutored,not.tutored)

math.2022 = math.balanced

#2023 balanced
tutored = math.2023 %>% filter(tutor == "yes")
not.tutored = math.2023 %>% filter(tutor == "no")
not.tutored <- not.tutored[sample(1:nrow(not.tutored), size=(2*dim(tutored)[1])),]
math.balanced = rbind(tutored,not.tutored)

math.2023 = math.balanced

#split into training and testing sets
#2019
sample <- sample(c(TRUE,FALSE), nrow(math.2019),  
                 replace=TRUE, prob=c(0.8,0.2)) 
train.2019 <- math.2019[sample,]
table(train.2019$tutor)
test.2019 <- anti_join(math.2019, train.2019)
table(test.2019$tutor)

#2020
sample <- sample(c(TRUE,FALSE), nrow(math.2020),  
                 replace=TRUE, prob=c(0.8,0.2)) 
train.2020 <- math.2020[sample,]
table(train.2020$tutor)
test.2020 <- anti_join(math.2020, train.2020)
table(test.2020$tutor)

#2021
sample <- sample(c(TRUE,FALSE), nrow(math.2021),  
                 replace=TRUE, prob=c(0.8,0.2)) 
train.2021 <- math.2021[sample,]
table(train.2021$tutor)
test.2021 <- anti_join(math.2021, train.2021)
table(test.2021$tutor)

#2022
sample <- sample(c(TRUE,FALSE), nrow(math.2022),  
                 replace=TRUE, prob=c(0.8,0.2)) 
train.2022 <- math.2022[sample,]
table(train.2022$tutor)
test.2022 <- anti_join(math.2022, train.2022)
table(test.2022$tutor)

#2023
sample <- sample(c(TRUE,FALSE), nrow(math.2023),  
                 replace=TRUE, prob=c(0.8,0.2)) 
train.2023 <- math.2023[sample,]
table(train.2023$tutor)
test.2023 <- anti_join(math.2023, train.2023)
table(test.2023$tutor)

#linear models
#2019
model.2019 = lm(pass_rate~session.count+ethnicity+first_gen+gender+gpa,data = math.2019)
summary(model.2019)

#2020
model.2020 = lm(pass_rate~session.count+ethnicity+first_gen+gender+gpa,data = math.2020)
summary(model.2020)

#2021
model.2021 = lm(pass_rate~session.count+ethnicity+first_gen+gender+gpa,data = math.2021)
summary(model.2021)

#2022
model.2022 = lm(pass_rate~session.count+ethnicity+first_gen+gender +gpa,data = math.2022)
summary(model.2022)

#2023
model.2023 = lm(pass_rate~session.count+ethnicity+first_gen+gender+gpa,data = math.2023)
summary(model.2023)

#plot success vs. session count, grouped by ethnicity
# math.20192 = math.2019
# math.20192$ethnicity = as.factor(as.numeric(as.factor(math.20192$ethnicity)))
# 
# model.20192 = lm(pass_rate~n+ethnicity,data = math.20192)
# 
# newdata = with(math.20192,
#                data.frame(n = rep(seq(from = 0, to=50, 
#                                       length.out = 100),6),
#                           ethnicity = factor(rep(1:6, each = 100))))
# 
# newdata2 = cbind(newdata, predict(model.20192, newdata))
# # newdata2 = within(newdata2, {
# #   pers.pred = plogis(fit)
# # })
# 
# library(plyr)
# newdata2$Ethnicity = as.factor(mapvalues(newdata2$ethnicity, 
#                                          from=c(1,2,3,4,5,6), 
#                                          to= c("Asian",
#                                                "Black or African American",
#                                                "Hispanic/Latino",
#                                                "Native American or Pacific Islander",
#                                                "Two or More Races",
#                                                "White")))
# library(ggplot2)
# ggplot(newdata2, aes(n,predict(model.20192, newdata),colour = Ethnicity, group = Ethnicity))+geom_line()+ylab("Success Rate")+xlab("Session Count")

#cross validation
# library("cv")
# cv(model.2019)

#calculate MSE
preds.19 = predict(model.2019,newdata=test.2019)
mean((test.2019$pass_rate - preds.19)^2)

preds.19.2 = predict(model.2019.2,newdata=test.2019)
mean((test.2019$pass_rate - preds.19.2)^2)

preds.20 = predict(model.2020,newdata=test.2020)
mean((test.2020$pass_rate - preds.20)^2)

preds.21 = predict(model.2021,newdata=test.2021)
mean((test.2021$pass_rate - preds.21)^2)

preds.22 = predict(model.2022,newdata=test.2022)
mean((test.2022$pass_rate - preds.22)^2)

preds.23 = predict(model.2023,newdata=test.2021)
mean((test.2023$pass_rate - preds.23)^2)

########################Random Forest
library(randomForest)
#math.general$pass_rate = as.factor(math.general$pass_rate) #set as factor for classification
math.general$gpa = as.factor(math.general$gpa)
out = randomForest(pass_rate~session.count+ethnicity+time.spent+gender+first_gen+gpa, data = math.general, importance =TRUE, do.trace = 100,ntree=46,mtry=6/3)

#find number of trees that produce lowest test MSE
which.min(out$mse) #46

#find RMSE of best model
sqrt(out$mse[which.min(out$mse)]) 

#plot the test MSE by number of trees
plot(out) #choose 46 trees

#variable importance plot
varImpPlot(out)

##########################clean up importance plot
# To plot the importance measures
#varImpPlot(rf.mod1)
imp2 <- varImpPlot(out)

# To change the names of the variables
rownames(imp2) = c("Session Count" ,"Ethnicity",     "Time Spent" ,   "Gender","First Gen.", 
                   "GPA")

# make dataframe from importance() output
feat_imp_df2 <- imp2 %>% 
  data.frame() %>% 
  mutate(feature = row.names(.)) 
library(ggplot2)
# plot dataframe
ggplot(feat_imp_df2, aes(x = reorder(feature, IncNodePurity), 
                         y = IncNodePurity)) +
  geom_bar(stat='identity', fill = "cornflowerblue") +
  coord_flip() +
  theme_classic() +
  labs(
    x     = "Feature",
    y     = "Importance",
    title = "Feature Importance: Success in Math Courses F19-F23")

#random forest for persistence
pers.for = randomForest(pers.f2s~session.count+ethnicity+gender+first_gen+gpa, data = math.general, importance =TRUE, do.trace = 100,ntree=76,mtry=5/3)
#find number of trees that produce lowest test MSE
which.min(pers.for$mse) #46

imp2 <- varImpPlot(pers.for)

# To change the names of the variables
rownames(imp2) = c("Session Count" ,"Ethnicity",   "Gender","First Gen.", 
                   "GPA")

# make dataframe from importance() output
feat_imp_df2 <- imp2 %>% 
  data.frame() %>% 
  mutate(feature = row.names(.)) 
library(ggplot2)
# plot dataframe
ggplot(feat_imp_df2, aes(x = reorder(feature, IncNodePurity), 
                         y = IncNodePurity)) +
  geom_bar(stat='identity', fill = "cornflowerblue") +
  coord_flip() +
  theme_classic() +
  labs(
    x     = "Feature",
    y     = "Importance",
    title = "Feature Importance: Persistence of Math Students F19-F23")

#random forest for retention
ret.for = randomForest(ret.f2f~session.count+ethnicity+gender+first_gen+gpa, data = math.general, importance =TRUE, do.trace = 85,ntree=100,mtry=5/3)
#find number of trees that produce lowest test MSE
which.min(ret.for$mse) #46

imp2 <- varImpPlot(ret.for)

# To change the names of the variables
rownames(imp2) = c("Session Count" ,"Ethnicity",   "Gender","First Gen.", 
                   "GPA")

# make dataframe from importance() output
feat_imp_df2 <- imp2 %>% 
  data.frame() %>% 
  mutate(feature = row.names(.)) 
library(ggplot2)
# plot dataframe
ggplot(feat_imp_df2, aes(x = reorder(feature, IncNodePurity), 
                         y = IncNodePurity)) +
  geom_bar(stat='identity', fill = "cornflowerblue") +
  coord_flip() +
  theme_classic() +
  labs(
    x     = "Feature",
    y     = "Importance",
    title = "Feature Importance: Retention of Math Students F19-F23")


##################################################

#cross-validation
trainy = math.general$pass_rate
trainx = math.general %>% select(c(ethnicity,gpa,first_gen,gender,session.count,time.spent))
cv.forest = rfcv(trainx,trainy)

with(cv.forest, plot(n.var, error.cv,xlab="Number of Predictors",ylab="Error"))

ggplot(aes(n.var,error.cv))

########################logistic regression
########################use categorical session count, ethnicity, first gen, gender

log.pers.19 = glm(pers.f2s~session.count+ethnicity+first_gen+gender+gpa,data=math.2019,family="binomial")
log.pers.20 = glm(pers.f2s~session.count+ethnicity+first_gen+gender+gpa,data=math.2020,family="binomial")
log.pers.21 = glm(pers.f2s~session.count+ethnicity+first_gen+gender+gpa,data=math.2021,family="binomial")
log.pers.22 = glm(pers.f2s~session.count+ethnicity+first_gen+gender+gpa,data=math.2022,family="binomial")
log.pers.23 = glm(pers.f2s~session.count+ethnicity+first_gen+gender+gpa,data=math.2023,family="binomial")

log.ret19 = glm(ret.f2f~session.count+ethnicity+first_gen+gender+gpa,data=math.2019,family="binomial")
log.ret20 = glm(ret.f2f~session.count+ethnicity+first_gen+gender+gpa,data=math.2020,family="binomial")
log.ret21 = glm(ret.f2f~session.count+ethnicity+first_gen+gender+gpa,data=math.2021,family="binomial")
log.ret22 = glm(ret.f2f~session.count+ethnicity+first_gen+gender+gpa,data=math.2022,family="binomial")
log.ret23 = glm(ret.f2f~n+ethnicity+first_gen+gender,data=math.2023,family="binomial")

#persistence 2019
math.20192 = math.2019
math.20192$ethnicity = as.factor(as.numeric(as.factor(math.20192$ethnicity)))

pers_n = glm(pers.f2s ~ n + ethnicity, data = math.20192, 
             family = "binomial")

newdata = with(math.20192,
               data.frame(n = rep(seq(from = 0, to=50, 
                                      length.out = 100),6),
                          ethnicity = factor(rep(1:6, each = 100))))

newdata2 = cbind(newdata, predict(pers_n, newdata, type='link', se=T))
newdata2 = within(newdata2, {
  pers.pred = plogis(fit)
})

library(plyr)
newdata2$Ethnicity = as.factor(mapvalues(newdata2$ethnicity, 
                                         from=c(1,2,3,4,5,6), 
                                         to= c("Asian",
                                               "Black or African American",
                                               "Hispanic/Latino",
                                               "Native American or Pacific Islander",
                                               "Two or More Races",
                                               "White")))
library(ggplot2)
ggplot(newdata2, aes(n,pers.pred,colour = Ethnicity, group = Ethnicity))+geom_line()+ylab("Probability of Persistence")+xlab("Session Count")+ggtitle("Persistence 2019")

#Persistence 2020
#log.pers = glm(pers.f2s~session.count+ethnicity+first_gen+gender,data=math.2020,family="binomial")


math.20202 = math.2020
math.20202$ethnicity = as.factor(as.numeric(as.factor(math.20202$ethnicity)))

pers_n = glm(pers.f2s ~ n + ethnicity, data = math.20202, 
             family = "binomial")

newdata = with(math.20202,
               data.frame(n = rep(seq(from = 0, to=50, 
                                      length.out = 100),6),
                          ethnicity = factor(rep(1:6, each = 100))))

newdata2 = cbind(newdata, predict(pers_n, newdata, type='link', se=T))
newdata2 = within(newdata2, {
  pers.pred = plogis(fit)
})

library(plyr)
newdata2$Ethnicity = as.factor(mapvalues(newdata2$ethnicity, 
                                         from=c(1,2,3,4,5,6), 
                                         to= c("Asian",
                                               "Black or African American",
                                               "Hispanic/Latino",
                                               "Native American or Pacific Islander",
                                               "Two or More Races",
                                               "White")))
library(ggplot2)
ggplot(newdata2, aes(n,pers.pred,colour = Ethnicity, group = Ethnicity))+geom_line()+ylab("Probability of Persistence")+xlab("Session Count")+ggtitle("Persistence 2020")


#retention 2019
ret_n = glm(ret.f2f ~ n + ethnicity, data = math.20192, 
             family = "binomial")

newdata = with(math.20192,
               data.frame(n = rep(seq(from = 0, to=50, 
                                      length.out = 100),6),
                          ethnicity = factor(rep(1:6, each = 100))))

newdata5 = cbind(newdata, predict(ret_n, newdata, type='link', se=T))
newdata5 = within(newdata5, {
  ret.pred = plogis(fit)
})

newdata5$Ethnicity = as.factor(mapvalues(newdata5$ethnicity, 
                                         from=c(1,2,3,4,5,6), 
                                         to= c("Asian",
                                               "Black or African American",
                                               "Hispanic/Latino",
                                               "Native American or Pacific Islander",
                                               "Two or More Races",
                                               "White")))

ggplot(newdata5, aes(n,ret.pred,colour = Ethnicity, group = Ethnicity))+geom_line()+ylab("Probability of Retention")+xlab("Session Count")+ggtitle("Retention 2019")


#retention 2020
ret_n = glm(ret.f2f ~ n + ethnicity, data = math.20202, 
            family = "binomial")

newdata = with(math.20202,
               data.frame(n = rep(seq(from = 0, to=50, 
                                      length.out = 100),6),
                          ethnicity = factor(rep(1:6, each = 100))))

newdata4 = cbind(newdata, predict(ret_n, newdata, type='link', se=T))
newdata4 = within(newdata4, {
  ret.pred = plogis(fit)
})

newdata4$Ethnicity = as.factor(mapvalues(newdata4$ethnicity, 
                                         from=c(1,2,3,4,5,6), 
                                         to= c("Asian",
                                               "Black or African American",
                                               "Hispanic/Latino",
                                               "Native American or Pacific Islander",
                                               "Two or More Races",
                                               "White")))
library(ggplot2)
ggplot(newdata4, aes(n,ret.pred,colour = Ethnicity, group = Ethnicity))+geom_line()+ylab("Probability of Retention")+xlab("Session Count")+ggtitle("Retention 2020")

########################################compare students with mid gpa
#2019
gpa.mid = math.2019 %>% filter(gpa == "mid")
table(gpa.mid$tutor)
lin.mid.gpa = lm(pass_rate~session.count+ethnicity+first_gen+gender, data = gpa.mid)
summary(lin.mid.gpa)

#2020
gpa.mid = math.2020 %>% filter(gpa == "mid")
table(gpa.mid$tutor)
lin.mid.gpa = lm(pass_rate~session.count+ethnicity+first_gen+gender, data = gpa.mid)
summary(lin.mid.gpa)

#2021
gpa.mid = math.2021 %>% filter(gpa == "mid")
table(gpa.mid$tutor)
lin.mid.gpa = lm(pass_rate~session.count+ethnicity+first_gen+gender, data = gpa.mid)
summary(lin.mid.gpa)

#2022
gpa.mid = math.2022 %>% filter(gpa == "mid")
table(gpa.mid$tutor)
lin.mid.gpa = lm(pass_rate~session.count+ethnicity+first_gen+gender, data = gpa.mid)
summary(lin.mid.gpa)

#2023
gpa.mid = math.2023 %>% filter(gpa == "mid")
table(gpa.mid$tutor)
lin.mid.gpa = lm(pass_rate~session.count+ethnicity+first_gen+gender, data = gpa.mid)
summary(lin.mid.gpa)

#logistic regression for persistence and retention

#retention
#2019
gpa.mid = math.2019 %>% filter(gpa == "mid")
ret.19 = glm(ret.f2f~session.count+ethnicity+first_gen+gender,data=gpa.mid,family="binomial")
summary(ret.19)

#2020
gpa.mid = math.2020 %>% filter(gpa == "mid")
ret.20 = glm(ret.f2f~session.count+ethnicity+first_gen+gender,data=gpa.mid,family="binomial")
summary(ret.20)

#2021
gpa.mid = math.2021 %>% filter(gpa == "mid")
ret.21 = glm(ret.f2f~session.count+ethnicity+first_gen+gender,data=gpa.mid,family="binomial")
summary(ret.21)

#2022
gpa.mid = math.2022 %>% filter(gpa == "mid")
ret.22 = glm(ret.f2f~session.count+ethnicity+first_gen+gender,data=gpa.mid,family="binomial")
summary(ret.22)

#persistence
#2019
gpa.mid = math.2019 %>% filter(gpa == "mid")
pers.19 = glm(pers.f2s~session.count+ethnicity+first_gen+gender,data=gpa.mid,family="binomial")
summary(pers.19)

#2020
gpa.mid = math.2020 %>% filter(gpa == "mid")
pers.20 = glm(pers.f2f~session.count+ethnicity+first_gen+gender,data=gpa.mid,family="binomial")
summary(pers.20)

#2021
gpa.mid = math.2021 %>% filter(gpa == "mid")
pers.21 = glm(pers.f2f~session.count+ethnicity+first_gen+gender,data=gpa.mid,family="binomial")
summary(pers.21)

#2022
gpa.mid = math.2022 %>% filter(gpa == "mid")
pers.22 = glm(pers.f2f~session.count+ethnicity+first_gen+gender,data=gpa.mid,family="binomial")
summary(pers.22)

#2023
gpa.mid = math.2023 %>% filter(gpa == "mid")
pers.23 = glm(pers.f2s~session.count+ethnicity+first_gen+gender,data=gpa.mid,family="binomial")
summary(pers.23)

#combine black and native american
#combine white and two or more races?
#collapse session counts
#model gpa only, model session count after collapsing, gpa*session, ethnicity only, gpa+session+ethnicity
#gpa*session, gpa*ethnicity

summary(lm(pass_rate~session.count*ethnicity*gpa, data=math.2021))
summary(lm(pass_rate~session.count, data=math.2020))
summary(lm(pass_rate~session.count, data=math.2021))
summary(lm(pass_rate~session.count, data=math.2022))
summary(lm(pass_rate~session.count, data=math.2023))

