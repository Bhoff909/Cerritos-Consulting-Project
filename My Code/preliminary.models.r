math.general = read.csv("C:/Users/jbrin/OneDrive/Documents/Spring 2024/fall.19_23_2.ssc.math.csv",header=T)
math.general = read.csv("C:/Users/jon/OneDrive/Documents/Spring 2024/fall.19_23_2.ssc.math.csv",header=T)

math.courses = read.csv("C:/Users/jbrin/OneDrive/Documents/Spring 2024/math.courses.final.csv",header=T)

math.embedded = read.csv("C:/Users/jbrin/OneDrive/Documents/Spring 2024/ET.MTH.19.23.courseinfo.csv",header=T)
gpa <- cut(math.embedded$CUM_GPA, breaks=c(0,2,3,4), labels=c('low', 'mid', 'high'))
math.embedded = cbind(math.embedded,gpa)
math.embedded$gpa[is.na(math.embedded$gpa)] <- "low"
math.embedded$gpa = as.factor(math.embedded$gpa)

math.general$pass_rate <- math.general$passed/math.general$enrolled

math.general$ret.f2f[is.na(math.general$ret.f2f)] <- 0
math.general$pers.f2s[is.na(math.general$pers.f2s)] <- 0

gpa <- cut(math.general$CUM_GPA, breaks=c(0,2,3,4), labels=c('low', 'mid', 'high'))
math.general = cbind(math.general,gpa)
math.general$gpa[is.na(math.general$gpa)] <- "low"
math.general$gpa = as.factor(math.general$gpa)

#collapse gender
library(dplyr)
summary(math.general$gender)
unknown.ids <- math.general %>% filter(gender == 'Unknown') %>% dplyr::select(Student_ID)
set.seed(3)
ids = sample(unknown.ids$Student_ID, floor(dim(unknown.ids)[1]/2)) # males 
fem.id = unknown.ids %>% filter(!(unknown.ids$Student_ID %in% ids))
fem.id = fem.id$Student_ID
for(i in 1:length(ids)){
  math.general[math.general$Student_ID == ids[i], 'gender'] <- 'Male'}
for(i in 1:length(fem.id)){
  math.general[math.general$Student_ID == fem.id[i], 'gender'] <- 'Female'}

math.general$gender = as.factor(math.general$gender)

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

library(stats)
math.general$session.count = as.factor(math.general$session.count)
contrasts(math.general$session.count)
math.general$session.count <- relevel(math.general$session.count, ref = "none")
math.general$first_gen = as.factor(math.general$first_gen)
contrasts(math.general$first_gen)
math.general$first_gen <- relevel(math.general$first_gen, ref = "Not First Generation")

term.2019 = math.general %>% filter(STRM == '1199')
summary(lm(pass_rate~interaction(gpa,session.count),data=term.2019))
summary(lm(pass_rate~(session.count + gpa)^2,data=term.2019))

#one-hot encoding
library(dplyr)
library(tidyr)
library(stringr)

newdata <- math.general %>% mutate(value = 1)  %>% spread(STRM, value,  fill = 0 ) 
colnames(newdata)[colnames(newdata) == 1199] = "fall19"
colnames(newdata)[colnames(newdata) == 1209] = "fall20"
colnames(newdata)[colnames(newdata) == 1219] = "fall21"
colnames(newdata)[colnames(newdata) == 1229] = "fall22"
colnames(newdata)[colnames(newdata) == 1239] = "fall23"

##------------------------------------------------- add prior gpa info
# getting student GPA-ACADEMIC DATASET

application2 <- application %>% 
  select(Student_ID, 
         CER_AD_HIGH_GPA) %>% 
  distinct() %>% 
  group_by(Student_ID) %>%
  filter(CER_AD_HIGH_GPA == max(CER_AD_HIGH_GPA)) %>% # just take the highest gpa they have listed....
  ungroup() %>% 
  mutate(# boy, was I not aware of this... these are the actual bucket meanings.
    # CER_AD_HIGH_GPA2 = case_when(CER_AD_HIGH_GPA == 1 ~ '3.50 - 4',
    #                              CER_AD_HIGH_GPA == 2 ~ '3.00 - 3.49',
    #                              CER_AD_HIGH_GPA == 3 ~ '2.50 - 2.99',
    #                              CER_AD_HIGH_GPA == 4 ~ '2.00 - 2.49',
    #                              CER_AD_HIGH_GPA == 5 ~ '1.50 - 1.99',
    #                              CER_AD_HIGH_GPA == 6 ~ '1.00 - 1.50',
    #                              CER_AD_HIGH_GPA == 7 ~ '0.00 - 0.00',
    #                              TRUE ~ 'Unknown'),
    CER_AD_HIGH_GPA2 = case_when(CER_AD_HIGH_GPA == 1 ~ 4, # taking the value at the high end?? Thoughts?
                                 CER_AD_HIGH_GPA == 2 ~ 3.49,
                                 CER_AD_HIGH_GPA == 3 ~ 2.99,
                                 CER_AD_HIGH_GPA == 4 ~ 2.49,
                                 CER_AD_HIGH_GPA == 5 ~ 1.99,
                                 CER_AD_HIGH_GPA == 6 ~ 1.50,
                                 CER_AD_HIGH_GPA == 7 ~ 0.00,
                                 TRUE ~ NA_real_))
application2 %>% group_by(Student_ID) %>% mutate(n = n()) %>% filter(n > 1) %>% arrange(Student_ID) 
application = application2

#2019
f19.gpa1 <-academic_career %>% filter(str_detect(STRM, "1193")) %>% dplyr::select(Student_ID, CUM_GPA)

math.courses$Student_ID = stringr::str_pad(math.courses$Student_ID, side = 'left', width = 7, pad = '0')
math.courses.2019 = math.courses %>% filter(STRM == 1199)

student.gpa.cum.2019 <-inner_join(math.courses.2019, f19.gpa1 , by="Student_ID")
colnames(student.gpa.cum.2019)[colnames(student.gpa.cum.2019) == "CUM_GPA.y"] = "prior.gpa"
colnames(student.gpa.cum.2019)[colnames(student.gpa.cum.2019) == "CUM_GPA.x"] = "CUM_GPA"

###--------------- Grabbing students that are not in student.gpa.cum
new.students <- anti_join(math.courses.2019, student.gpa.cum.2019 , by="Student_ID")
student.HS.gpa <- application %>% dplyr::select(Student_ID, CER_AD_HIGH_GPA) # using application2 from file in google drive
new.gpa.2019 <- inner_join(new.students,student.HS.gpa ,"Student_ID") 
colnames(new.gpa.2019)[colnames(new.gpa.2019) == "CER_AD_HIGH_GPA"] = "prior.gpa" # changing column name to rbind 
student.gpa.all.2019 <- rbind(student.gpa.cum.2019 ,new.gpa.2019 ) # then inner_join with the main data set 

#2020
f20.gpa1 <-academic_career %>% filter(str_detect(STRM, "1203")) %>% dplyr::select(Student_ID, CUM_GPA)

#math.courses$Student_ID = stringr::str_pad(math.courses$Student_ID, side = 'left', width = 7, pad = '0')
math.courses.2020 = math.courses %>% filter(STRM == 1209)

student.gpa.cum.2020 <-inner_join(math.courses.2020, f20.gpa1 , by="Student_ID")
colnames(student.gpa.cum.2020)[colnames(student.gpa.cum.2020) == "CUM_GPA.y"] = "prior.gpa"
colnames(student.gpa.cum.2020)[colnames(student.gpa.cum.2020) == "CUM_GPA.x"] = "CUM_GPA"

###--------------- Grabbing students that are not in student.gpa.cum
new.students <- anti_join(math.courses.2020, student.gpa.cum.2020 , by="Student_ID")
student.HS.gpa <- application %>% dplyr::select(Student_ID, CER_AD_HIGH_GPA) # using application2 from file in google drive
new.gpa.2020 <- inner_join(new.students,student.HS.gpa ,"Student_ID") 
colnames(new.gpa.2020)[colnames(new.gpa.2020) == "CER_AD_HIGH_GPA"] = "prior.gpa" # changing column name to rbind 
student.gpa.all.2020 <- rbind(student.gpa.cum.2020 ,new.gpa.2020 ) # then inner_join with the main data set 

#2021
f21.gpa1 <-academic_career %>% filter(str_detect(STRM, "1213")) %>% dplyr::select(Student_ID, CUM_GPA)

#math.courses$Student_ID = stringr::str_pad(math.courses$Student_ID, side = 'left', width = 7, pad = '0')
math.courses.2021 = math.courses %>% filter(STRM == 1219)

student.gpa.cum.2021 <-inner_join(math.courses.2021, f21.gpa1 , by="Student_ID")
colnames(student.gpa.cum.2021)[colnames(student.gpa.cum.2021) == "CUM_GPA.y"] = "prior.gpa"
colnames(student.gpa.cum.2021)[colnames(student.gpa.cum.2021) == "CUM_GPA.x"] = "CUM_GPA"

###--------------- Grabbing students that are not in student.gpa.cum
new.students <- anti_join(math.courses.2021, student.gpa.cum.2021 , by="Student_ID")
student.HS.gpa <- application %>% dplyr::select(Student_ID, CER_AD_HIGH_GPA) # using application2 from file in google drive
new.gpa.2021 <- inner_join(new.students,student.HS.gpa ,"Student_ID") 
colnames(new.gpa.2021)[colnames(new.gpa.2021) == "CER_AD_HIGH_GPA"] = "prior.gpa" # changing column name to rbind 
student.gpa.all.2021 <- rbind(student.gpa.cum.2021 ,new.gpa.2021 ) # then inner_join with the main data set 

#2022
f22.gpa1 <-academic_career %>% filter(str_detect(STRM, "1223")) %>% dplyr::select(Student_ID, CUM_GPA)

#math.courses$Student_ID = stringr::str_pad(math.courses$Student_ID, side = 'left', width = 7, pad = '0')
math.courses.2022 = math.courses %>% filter(STRM == 1229)

student.gpa.cum.2022 <-inner_join(math.courses.2022, f22.gpa1 , by="Student_ID")
colnames(student.gpa.cum.2022)[colnames(student.gpa.cum.2022) == "CUM_GPA.y"] = "prior.gpa"
colnames(student.gpa.cum.2022)[colnames(student.gpa.cum.2022) == "CUM_GPA.x"] = "CUM_GPA"

###--------------- Grabbing students that are not in student.gpa.cum
new.students <- anti_join(math.courses.2022, student.gpa.cum.2022 , by="Student_ID")
student.HS.gpa <- application %>% dplyr::select(Student_ID, CER_AD_HIGH_GPA) # using application2 from file in google drive
new.gpa.2022 <- inner_join(new.students,student.HS.gpa ,"Student_ID") 
colnames(new.gpa.2022)[colnames(new.gpa.2022) == "CER_AD_HIGH_GPA"] = "prior.gpa" # changing column name to rbind 
student.gpa.all.2022 <- rbind(student.gpa.cum.2022 ,new.gpa.2022 ) # then inner_join with the main data set 

#2023
f23.gpa1 <-academic_career %>% filter(str_detect(STRM, "1233")) %>% dplyr::select(Student_ID, CUM_GPA)

#math.courses$Student_ID = stringr::str_pad(math.courses$Student_ID, side = 'left', width = 7, pad = '0')
math.courses.2023 = math.courses %>% filter(STRM == 1239)

student.gpa.cum.2023 <-inner_join(math.courses.2023, f23.gpa1 , by="Student_ID")
colnames(student.gpa.cum.2023)[colnames(student.gpa.cum.2023) == "CUM_GPA.y"] = "prior.gpa"
colnames(student.gpa.cum.2023)[colnames(student.gpa.cum.2023) == "CUM_GPA.x"] = "CUM_GPA"

###--------------- Grabbing students that are not in student.gpa.cum
new.students <- anti_join(math.courses.2023, student.gpa.cum.2023 , by="Student_ID")
student.HS.gpa <- application %>% dplyr::select(Student_ID, CER_AD_HIGH_GPA) # using application2 from file in google drive
new.gpa.2023 <- inner_join(new.students,student.HS.gpa ,"Student_ID") 
colnames(new.gpa.2023)[colnames(new.gpa.2023) == "CER_AD_HIGH_GPA"] = "prior.gpa" # changing column name to rbind 
student.gpa.all.2023 <- rbind(student.gpa.cum.2023 ,new.gpa.2023 ) # then inner_join with the main data set 

final.table = rbind(student.gpa.all.2019,student.gpa.all.2020,student.gpa.all.2021,student.gpa.all.2022,student.gpa.all.2023)
#categorize gpa
prior.gpa.cat <- cut(final.table$prior.gpa, breaks=c(0,2,3,4), labels=c('low', 'mid', 'high'))
final.table = cbind(final.table,prior.gpa.cat)
final.table$prior.gpa.cat[is.na(final.table$prior.gpa.cat)] <- "low"
final.table$prior.gpa.cat = as.factor(final.table$prior.gpa.cat)

#collapse gender
library(dplyr)
summary(final.table$gender)
unknown.ids <- final.table %>% filter(gender == 'Unknown') %>% dplyr::select(Student_ID)
set.seed(3)
ids = sample(unknown.ids$Student_ID, floor(dim(unknown.ids)[1]/2)) # males 
fem.id = unknown.ids %>% filter(!(unknown.ids$Student_ID %in% ids))
fem.id = fem.id$Student_ID
for(i in 1:length(ids)){
  final.table[final.table$Student_ID == ids[i], 'gender'] <- 'Male'}
for(i in 1:length(fem.id)){
  final.table[final.table$Student_ID == fem.id[i], 'gender'] <- 'Female'}

final.table$gender = as.factor(final.table$gender)

#collapse session counts
final.table$session.count[final.table$session.count == "rare"] <- "occasionally"
final.table$session.count[final.table$session.count == "very.frequently"] <- "frequently"
final.table$session.count = as.character(final.table$session.count)
final.table$session.count = as.factor(final.table$session.count)

#collapse ethnicity
final.table$ethnicity[final.table$ethnicity == "Black or African American"] <- "underrepresented"
final.table$ethnicity[final.table$ethnicity == "Native American or Pacific Islander"] <- "underrepresented"
final.table$ethnicity[final.table$ethnicity == "Two or More Races"] <- "white/two or more races"
final.table$ethnicity[final.table$ethnicity == "White"] <- "white/two or more races"
final.table$ethnicity = as.character(final.table$ethnicity)
final.table$ethnicity = as.factor(final.table$ethnicity)

write.csv(final.table,"C:/Users/jbrin/Documents/math.courses.final.csv",row.names = F)

##-------------------------------------------------


#log regression persistence
summary(glm(pers.f2s~ethnicity,data=newdata,family="binomial"))
#log regression retention
summary(glm(ret.f2f~ethnicity,data=newdata,family="binomial"))
#regression success
summary(lm(pass_rate~session.count,data=newdata))

summary(lm(pass_rate~ethnicity + session.count + ethnicity*session.count,data=newdata))

summary(lm(pass_rate~ethnicity + session.count + gpa,data=newdata))

summary(lm(pass_rate~ethnicity + session.count + gpa + gpa*session.count,data=newdata))

#retention plot
math.general$ethnicity = as.factor(as.numeric(as.factor(math.general$ethnicity)))
ret_n = glm(ret.f2f ~ n + ethnicity + n*ethnicity, data = math.general, 
            family = "binomial")

newdata = with(math.general,
               data.frame(n = rep(seq(from = 0, to=50, 
                                      length.out = 100),4),
                          ethnicity = factor(rep(1:4, each = 100))))

newdata5 = cbind(newdata, predict(ret_n, newdata, type='link', se=T))
newdata5 = within(newdata5, {
  ret.pred = plogis(fit)
})
library(plyr)
newdata5$Ethnicity = as.factor(mapvalues(newdata5$ethnicity, 
                                         from=c(1,2,3,4), 
                                         to= c("Asian",
                                               "Hispanic/Latino",
                                               "Underrepresented",
                                               "White/Two or more races")))
library(ggplot2)
ggplot(newdata5, aes(n,ret.pred,colour = Ethnicity, group = Ethnicity))+geom_line()+ylab("Probability of Retention")+xlab("Session Count")+ggtitle("Retention")

#persistence plot
#math.general$ethnicity = as.factor(as.numeric(as.factor(math.general$ethnicity)))
ret_n = glm(pers.f2s ~ n + ethnicity + n*ethnicity, data = math.general, 
            family = "binomial")

newdata = with(math.general,
               data.frame(n = rep(seq(from = 0, to=50, 
                                      length.out = 100),4),
                          ethnicity = factor(rep(1:4, each = 100))))

newdata5 = cbind(newdata, predict(ret_n, newdata, type='link', se=T))
newdata5 = within(newdata5, {
  ret.pred = plogis(fit)
})
library(plyr)
newdata5$Ethnicity = as.factor(mapvalues(newdata5$ethnicity, 
                                         from=c(1,2,3,4), 
                                         to= c("Asian",
                                               "Hispanic/Latino",
                                               "Underrepresented",
                                               "White/Two or more races")))
library(ggplot2)
ggplot(newdata5, aes(n,ret.pred,colour = Ethnicity, group = Ethnicity))+geom_line()+ylab("Probability of Persistence")+xlab("Session Count")+ggtitle("Persistence")

#gpa persistence
math.general$gpa = as.factor(as.numeric(as.factor(math.general$gpa)))
ret_n = glm(pers.f2s ~ n + gpa + n*gpa, data = math.general, 
            family = "binomial")

newdata = with(math.general,
               data.frame(n = rep(seq(from = 0, to=50, 
                                      length.out = 100),3),
                          gpa = factor(rep(1:3, each = 100))))

newdata5 = cbind(newdata, predict(ret_n, newdata, type='link', se=T))
newdata5 = within(newdata5, {
  ret.pred = plogis(fit)
})
library(plyr)
newdata5$gpa = as.factor(mapvalues(newdata5$gpa, 
                                         from=c(1,2,3), 
                                         to= c("low",
                                               "mid",
                                               "high")))
library(ggplot2)
ggplot(newdata5, aes(n,ret.pred,colour = gpa, group = gpa))+geom_line()+ylab("Probability of Persistence")+xlab("Session Count")+ggtitle("Persistence")

#gpa retention
#math.general$gpa = as.factor(as.numeric(as.factor(math.general$gpa)))
ret_n = glm(ret.f2f ~ n + gpa + n*gpa, data = math.general, 
            family = "binomial")

newdata = with(math.general,
               data.frame(n = rep(seq(from = 0, to=50, 
                                      length.out = 100),3),
                          gpa = factor(rep(1:3, each = 100))))

newdata5 = cbind(newdata, predict(ret_n, newdata, type='link', se=T))
newdata5 = within(newdata5, {
  ret.pred = plogis(fit)
})
library(plyr)
newdata5$gpa = as.factor(mapvalues(newdata5$gpa, 
                                   from=c(1,2,3), 
                                   to= c("low",
                                         "mid",
                                         "high")))
library(ggplot2)
ggplot(newdata5, aes(n,ret.pred,colour = gpa, group = gpa))+geom_line()+ylab("Probability of Retention")+xlab("Session Count")+ggtitle("Retention")
############################################################################chemistry
chem.general = read.csv("C:/Users/jbrin/OneDrive/Documents/Spring 2024/fall.19_23.ssc.chem.courseinfo.csv",header=T)

#collapse gender
library(dplyr)
summary(chem.general$gender)
unknown.ids <- chem.general %>% filter(gender == 'Unknown') %>% dplyr::select(Student_ID)
set.seed(3)
ids = sample(unknown.ids$Student_ID, floor(dim(unknown.ids)[1]/2)) # males 
fem.id = unknown.ids %>% filter(!(unknown.ids$Student_ID %in% ids))
fem.id = fem.id$Student_ID
for(i in 1:length(ids)){
  chem.general[chem.general$Student_ID == ids[i], 'gender'] <- 'Male'}
for(i in 1:length(fem.id)){
  chem.general[chem.general$Student_ID == fem.id[i], 'gender'] <- 'Female'}

chem.general$gender = as.factor(chem.general$gender)

#collapse session counts
chem.general$session.count[chem.general$session.count == "rare"] <- "occasionally"
chem.general$session.count[chem.general$session.count == "very.frequently"] <- "frequently"
chem.general$session.count = as.character(chem.general$session.count)
chem.general$session.count = as.factor(chem.general$session.count)

#collapse ethnicity
chem.general$ethnicity[chem.general$ethnicity == "Black or African American"] <- "underrepresented"
chem.general$ethnicity[chem.general$ethnicity == "Native American or Pacific Islander"] <- "underrepresented"
chem.general$ethnicity[chem.general$ethnicity == "Two or More Races"] <- "white/two or more races"
chem.general$ethnicity[chem.general$ethnicity == "White"] <- "white/two or more races"
chem.general$ethnicity = as.character(chem.general$ethnicity)
chem.general$ethnicity = as.factor(chem.general$ethnicity)

###############################add prior gpa info to chem data
#2019
f19.gpa1 <-academic_career %>% filter(str_detect(STRM, "1193")) %>% dplyr::select(Student_ID, CUM_GPA)

chem.general$Student_ID = stringr::str_pad(chem.general$Student_ID, side = 'left', width = 7, pad = '0')
chem.courses.2019 = chem.general %>% filter(STRM == 1199)

student.gpa.cum.2019 <-inner_join(chem.courses.2019, f19.gpa1 , by="Student_ID")
colnames(student.gpa.cum.2019)[colnames(student.gpa.cum.2019) == "CUM_GPA.y"] = "prior.gpa"
colnames(student.gpa.cum.2019)[colnames(student.gpa.cum.2019) == "CUM_GPA.x"] = "CUM_GPA"

###--------------- Grabbing students that are not in student.gpa.cum
new.students <- anti_join(chem.courses.2019, student.gpa.cum.2019 , by="Student_ID")
student.HS.gpa <- application %>% dplyr::select(Student_ID, CER_AD_HIGH_GPA) # using application2 from file in google drive
new.gpa.2019 <- inner_join(new.students,student.HS.gpa ,"Student_ID") 
colnames(new.gpa.2019)[colnames(new.gpa.2019) == "CER_AD_HIGH_GPA"] = "prior.gpa" # changing column name to rbind 
student.gpa.all.2019 <- rbind(student.gpa.cum.2019 ,new.gpa.2019 ) # then inner_join with the main data set 

#2020
f20.gpa1 <-academic_career %>% filter(str_detect(STRM, "1203")) %>% dplyr::select(Student_ID, CUM_GPA)

#math.courses$Student_ID = stringr::str_pad(math.courses$Student_ID, side = 'left', width = 7, pad = '0')
chem.courses.2020 = chem.general %>% filter(STRM == 1209)

student.gpa.cum.2020 <-inner_join(chem.courses.2020, f20.gpa1 , by="Student_ID")
colnames(student.gpa.cum.2020)[colnames(student.gpa.cum.2020) == "CUM_GPA.y"] = "prior.gpa"
colnames(student.gpa.cum.2020)[colnames(student.gpa.cum.2020) == "CUM_GPA.x"] = "CUM_GPA"

###--------------- Grabbing students that are not in student.gpa.cum
new.students <- anti_join(chem.courses.2020, student.gpa.cum.2020 , by="Student_ID")
student.HS.gpa <- application %>% dplyr::select(Student_ID, CER_AD_HIGH_GPA) # using application2 from file in google drive
new.gpa.2020 <- inner_join(new.students,student.HS.gpa ,"Student_ID") 
colnames(new.gpa.2020)[colnames(new.gpa.2020) == "CER_AD_HIGH_GPA"] = "prior.gpa" # changing column name to rbind 
student.gpa.all.2020 <- rbind(student.gpa.cum.2020 ,new.gpa.2020 ) # then inner_join with the main data set 

#2021
f21.gpa1 <-academic_career %>% filter(str_detect(STRM, "1213")) %>% dplyr::select(Student_ID, CUM_GPA)

#math.courses$Student_ID = stringr::str_pad(math.courses$Student_ID, side = 'left', width = 7, pad = '0')
chem.courses.2021 = chem.general %>% filter(STRM == 1219)

student.gpa.cum.2021 <-inner_join(chem.courses.2021, f21.gpa1 , by="Student_ID")
colnames(student.gpa.cum.2021)[colnames(student.gpa.cum.2021) == "CUM_GPA.y"] = "prior.gpa"
colnames(student.gpa.cum.2021)[colnames(student.gpa.cum.2021) == "CUM_GPA.x"] = "CUM_GPA"

###--------------- Grabbing students that are not in student.gpa.cum
new.students <- anti_join(chem.courses.2021, student.gpa.cum.2021 , by="Student_ID")
student.HS.gpa <- application %>% dplyr::select(Student_ID, CER_AD_HIGH_GPA) # using application2 from file in google drive
new.gpa.2021 <- inner_join(new.students,student.HS.gpa ,"Student_ID") 
colnames(new.gpa.2021)[colnames(new.gpa.2021) == "CER_AD_HIGH_GPA"] = "prior.gpa" # changing column name to rbind 
student.gpa.all.2021 <- rbind(student.gpa.cum.2021 ,new.gpa.2021 ) # then inner_join with the main data set 

#2022
f22.gpa1 <-academic_career %>% filter(str_detect(STRM, "1223")) %>% dplyr::select(Student_ID, CUM_GPA)

#math.courses$Student_ID = stringr::str_pad(math.courses$Student_ID, side = 'left', width = 7, pad = '0')
chem.courses.2022 = chem.general %>% filter(STRM == 1229)

student.gpa.cum.2022 <-inner_join(chem.courses.2022, f22.gpa1 , by="Student_ID")
colnames(student.gpa.cum.2022)[colnames(student.gpa.cum.2022) == "CUM_GPA.y"] = "prior.gpa"
colnames(student.gpa.cum.2022)[colnames(student.gpa.cum.2022) == "CUM_GPA.x"] = "CUM_GPA"

###--------------- Grabbing students that are not in student.gpa.cum
new.students <- anti_join(chem.courses.2022, student.gpa.cum.2022 , by="Student_ID")
student.HS.gpa <- application %>% dplyr::select(Student_ID, CER_AD_HIGH_GPA) # using application2 from file in google drive
new.gpa.2022 <- inner_join(new.students,student.HS.gpa ,"Student_ID") 
colnames(new.gpa.2022)[colnames(new.gpa.2022) == "CER_AD_HIGH_GPA"] = "prior.gpa" # changing column name to rbind 
student.gpa.all.2022 <- rbind(student.gpa.cum.2022 ,new.gpa.2022 ) # then inner_join with the main data set 

#2023
f23.gpa1 <-academic_career %>% filter(str_detect(STRM, "1233")) %>% dplyr::select(Student_ID, CUM_GPA)

#math.courses$Student_ID = stringr::str_pad(math.courses$Student_ID, side = 'left', width = 7, pad = '0')
chem.courses.2023 = chem.general %>% filter(STRM == 1239)

student.gpa.cum.2023 <-inner_join(chem.courses.2023, f23.gpa1 , by="Student_ID")
colnames(student.gpa.cum.2023)[colnames(student.gpa.cum.2023) == "CUM_GPA.y"] = "prior.gpa"
colnames(student.gpa.cum.2023)[colnames(student.gpa.cum.2023) == "CUM_GPA.x"] = "CUM_GPA"

###--------------- Grabbing students that are not in student.gpa.cum
new.students <- anti_join(chem.courses.2023, student.gpa.cum.2023 , by="Student_ID")
student.HS.gpa <- application %>% dplyr::select(Student_ID, CER_AD_HIGH_GPA) # using application2 from file in google drive
new.gpa.2023 <- inner_join(new.students,student.HS.gpa ,"Student_ID") 
colnames(new.gpa.2023)[colnames(new.gpa.2023) == "CER_AD_HIGH_GPA"] = "prior.gpa" # changing column name to rbind 
student.gpa.all.2023 <- rbind(student.gpa.cum.2023 ,new.gpa.2023 ) # then inner_join with the main data set 

final.table = rbind(student.gpa.all.2019,student.gpa.all.2020,student.gpa.all.2021,student.gpa.all.2022,student.gpa.all.2023)
#categorize gpa
prior.gpa.cat <- cut(final.table$prior.gpa, breaks=c(0,2,3,4), labels=c('low', 'mid', 'high'))
final.table = cbind(final.table,prior.gpa.cat)
final.table$prior.gpa.cat[is.na(final.table$prior.gpa.cat)] <- "low"
final.table$prior.gpa.cat = as.factor(final.table$prior.gpa.cat)


write.csv(final.table,"C:/Users/jbrin/Documents/chem.courses.final.csv",row.names = F)
##############################################################

library(stats)
chem.general$session.count = as.factor(chem.general$session.count)
contrasts(chem.general$session.count)
chem.general$session.count <- relevel(chem.general$session.count, ref = "none")
chem.general$first_gen = as.factor(chem.general$first_gen)
contrasts(chem.general$first_gen)
chem.general$first_gen <- relevel(chem.general$first_gen, ref = "Not First Generation")

chem.2019 = chem.general %>% filter(STRM == 1199)
chem.2020 = chem.general %>% filter(STRM == 1209)
chem.2021 = chem.general %>% filter(STRM == 1219)
chem.2022 = chem.general %>% filter(STRM == 1229)
chem.2023 = chem.general %>% filter(STRM == 1239)

newchem <- chem.general %>% mutate(value = 1)  %>% spread(STRM, value,  fill = 0 ) 
colnames(newchem)[colnames(newchem) == 1199] = "fall19"
colnames(newchem)[colnames(newchem) == 1209] = "fall20"
colnames(newchem)[colnames(newchem) == 1219] = "fall21"
colnames(newchem)[colnames(newchem) == 1229] = "fall22"
colnames(newchem)[colnames(newchem) == 1239] = "fall23"

summary(lm(pass_rate~session.count +fall19 +fall20 +fall21 +fall22 +fall23, data=newchem))
gpamid = newchem %>% filter(gpa == "mid")
summary(lm(pass_rate~session.count +fall19 +fall20 +fall21 +fall22 +fall23, data=gpamid))
gpahigh = newchem %>% filter(gpa == "high")
summary(lm(pass_rate~session.count +fall19 +fall20 +fall21 +fall22 +fall23, data=gpahigh))
gpalow = newchem %>% filter(gpa == "low")
summary(lm(pass_rate~session.count +fall19 +fall20 +fall21 +fall22 +fall23, data=gpalow))

#sort chem data by course
table(newchem$DESCR)
#select intro chem
intro.chem = newchem %>% filter(DESCR == "Introductory Chemistry")
#mid = intro.chem %>% filter(gpa == "mid")
summary(glm(pass_rate~session.count +fall19 +fall20 +fall21 +fall22 +fall23, data=intro.chem,family = "binomial"))

###########################################propensity score matching
#collapse gender
library(dplyr)
summary(math.courses$gender)
unknown.ids <- math.courses %>% filter(gender == 'Unknown') %>% dplyr::select(Student_ID)
set.seed(3)
ids = sample(unknown.ids$Student_ID, floor(dim(unknown.ids)[1]/2)) # males 
fem.id = unknown.ids %>% filter(!(unknown.ids$Student_ID %in% ids))
fem.id = fem.id$Student_ID
for(i in 1:length(ids)){
  math.courses[math.courses$Student_ID == ids[i], 'gender'] <- 'Male'}
for(i in 1:length(fem.id)){
  math.courses[math.courses$Student_ID == fem.id[i], 'gender'] <- 'Female'}

math.courses$gender = as.factor(math.courses$gender)

#collapse session counts
math.courses$session.count[math.courses$session.count == "rare"] <- "occasionally"
math.courses$session.count[math.courses$session.count == "very.frequently"] <- "frequently"
math.courses$session.count = as.character(math.courses$session.count)
math.courses$session.count = as.factor(math.courses$session.count)

#collapse ethnicity
math.courses$ethnicity[math.courses$ethnicity == "Black or African American"] <- "underrepresented"
math.courses$ethnicity[math.courses$ethnicity == "Native American or Pacific Islander"] <- "underrepresented"
math.courses$ethnicity[math.courses$ethnicity == "Two or More Races"] <- "white/two or more races"
math.courses$ethnicity[math.courses$ethnicity == "White"] <- "white/two or more races"
math.courses$ethnicity = as.character(math.courses$ethnicity)
math.courses$ethnicity = as.factor(math.courses$ethnicity)

library(stats)
math.courses$session.count = as.factor(math.courses$session.count)
contrasts(math.courses$session.count)
math.courses$session.count <- relevel(math.courses$session.count, ref = "none")
math.courses$first_gen = as.factor(math.courses$first_gen)
contrasts(math.courses$first_gen)
math.courses$first_gen <- relevel(math.courses$first_gen, ref = "Not First Generation")

library(MatchIt)
math.courses$tutor[math.courses$tutor == "yes"] = 1
math.courses$tutor[math.courses$tutor == "no"] = 0
math.courses$tutor = as.numeric(math.courses$tutor)

math.stats = math.courses %>% filter(DESCR == "Elementary Statistics")
math.stats$tutor[math.stats$tutor == "yes"] = 1
math.stats$tutor[math.stats$tutor == "no"] = 0
math.stats$tutor = as.numeric(math.stats$tutor)


test = matchit(tutor~gpa+first_gen+ethnicity,data=math.courses)

matched_data = match.data(test)

summary(glm(pass_rate~session.count+ethnicity,data=matched_data,weights = weights,family="binomial"))
summary(glm(pers.f2s~session.count+ethnicity,data=matched_data,weights = weights,family="binomial"))
summary(glm(ret.f2f~session.count+ethnicity,data=matched_data,weights = weights,family="binomial"))

#embedded
math.embedded$tutor[math.embedded$tutor == "yes"] = 1
math.embedded$tutor[math.embedded$tutor == "no"] = 0
math.embedded$tutor = as.numeric(math.embedded$tutor)

math.embedded$session.count2 = as.factor(math.embedded$session.count2)
contrasts(math.embedded$session.count2)
math.embedded$session.count2 <- relevel(math.embedded$session.count2, ref = "none")

emb = matchit(tutor~gpa+first_gen+gender,data=math.embedded)

matched_data_emb = match.data(emb)

summary(glm(pass~session.count2+ethnicity,data=matched_data_emb,weights = weights,family="binomial"))

#chem
chem.general$tutor[chem.general$tutor == "yes"] = 1
chem.general$tutor[chem.general$tutor == "no"] = 0
chem.general$tutor = as.numeric(chem.general$tutor)

chem.courses = c(100,111,110)
chem.model = chem.general %>% filter(as.numeric(CATALOG_NBR) %in% chem.courses)

chem.out = matchit(tutor~gpa + first_gen + ethnicity,data=chem.general)
matched_data_chem = match.data(chem.out)

summary(glm(pass_rate~session.count+ethnicity,data=matched_data_chem,weights = weights,family="binomial"))
summary(glm(pers.f2s~session.count+ethnicity,data=matched_data_chem,weights = weights,family="binomial"))
summary(glm(ret.f2f~session.count+ethnicity,data=matched_data_chem,weights = weights,family="binomial"))
