#math.general = read.csv("C:/Users/jbrin/Documents/math/fall.19_23.ssc.math.csv",header=T)
math.general = read.csv("C:/Users/jbrin/OneDrive/Documents/Spring 2024/fall.20.ssc.math.courseinfo.csv",header=T)
math.embedded = read.csv("C:/Users/jbrin/OneDrive/Documents/Spring 2024/ET.MTH.19.23.courseinfo.csv",header=T)
chem.general = read.csv("C:/Users/jbrin/Documents/fall.19_23.ssc.chem.csv",header=T)
#math.general = read.csv("C:/Users/jon/OneDrive/Documents/Spring 2024/fall.19_23.ssc.math.csv",header=T)

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

#add leading zeroes for id's with fewer than 7 digits
math.general$Student_ID = stringr::str_pad(math.general$Student_ID, side = 'left', width = 7, pad = '0')
math.embedded$Student_ID = stringr::str_pad(math.embedded$Student_ID, side = 'left', width = 7, pad = '0')
chem.general$Student_ID = stringr::str_pad(chem.general$Student_ID, side = 'left', width = 7, pad = '0')

gpa <- cut(math.general$CUM_GPA, breaks=c(0,2,3,4), labels=c('low', 'mid', 'high'))
math.general = cbind(math.general,gpa)
math.general$gpa[is.na(math.general$gpa)] <- "low"
math.general$gpa = as.factor(math.general$gpa)

#Add ethnicity
library(dplyr)
ethnicity.math.embedded <- demographics %>% filter(Student_ID %in% math.embedded$Student_ID) %>% arrange(Student_ID) %>% select(ethnicity)


#########################Add course information for 2019
#########################Remove students taking multiple classes to compare to embedded
library(dplyr)
library(stringr)
math.oneclass.19 = math.general %>% filter(STRM == "1199") %>% filter(enrolled == 1) #4516 observations
#########################obtain course info
courses.2019 = enrollment %>% filter(str_detect(STRM, '199')) %>% filter(str_detect(SUBJECT, 'MATH')) %>% filter(enrolled == TRUE) %>% filter(Student_ID %in% math.oneclass.19$Student_ID)

#add course information
math.oneclass.19 = math.oneclass.19 %>% arrange(Student_ID)
courses.2019 = courses.2019 %>% arrange(Student_ID)
math.oneclass.19 = cbind(math.oneclass.19,CATALOG_NBR=courses.2019$CATALOG_NBR,DESCR=courses.2019$DESCR)

#########################Add course information for 2020
#########################Remove students taking multiple classes to compare to embedded
library(dplyr)
library(stringr)
math.oneclass.20 = math.general %>% filter(str_detect(STRM, '209')) %>% filter(enrolled == 1) #4271 observations
#########################obtain course info
courses.2020 = enrollment %>% filter(str_detect(STRM, '209')) %>% filter(str_detect(SUBJECT, 'MATH')) %>% filter(Student_ID %in% math.oneclass.20$Student_ID) %>% filter(enrolled == TRUE)

#add course information
math.oneclass.20 = math.oneclass.20 %>% arrange(Student_ID)
courses.2020 = courses.2020 %>% arrange(Student_ID)
math.oneclass.20 = cbind(math.oneclass.20,CATALOG_NBR=courses.2020$CATALOG_NBR,DESCR=courses.2020$DESCR)

#########################Add course information for 2021
#########################Remove students taking multiple classes to compare to embedded
library(dplyr)
library(stringr)
math.oneclass.21 = math.general %>% filter(str_detect(STRM, '219')) %>% filter(enrolled == 1) #3552 observations
#########################obtain course info
courses.2021 = enrollment %>% filter(str_detect(STRM, '219')) %>% filter(str_detect(SUBJECT, 'MATH')) %>% filter(Student_ID %in% math.oneclass.21$Student_ID) %>% filter(enrolled == TRUE) #3175 observations

#add course information
math.oneclass.21 = math.oneclass.21 %>% arrange(Student_ID)
courses.2021 = courses.2021 %>% arrange(Student_ID)
math.oneclass.21 = cbind(math.oneclass.21,CATALOG_NBR=courses.2021$CATALOG_NBR,DESCR=courses.2021$DESCR)

#########################Add course information for 2022
#########################Remove students taking multiple classes to compare to embedded
library(dplyr)
library(stringr)
math.oneclass.22 = math.general %>% filter(str_detect(STRM, '229')) %>% filter(enrolled == 1) #3552 observations
#########################obtain course info
courses.2022 = enrollment %>% filter(str_detect(STRM, '229')) %>% filter(str_detect(SUBJECT, 'MATH')) %>% filter(Student_ID %in% math.oneclass.22$Student_ID) %>% filter(enrolled == TRUE) #3175 observations

#add course information
math.oneclass.22 = math.oneclass.22 %>% arrange(Student_ID)
courses.2022 = courses.2022 %>% arrange(Student_ID)
math.oneclass.22 = cbind(math.oneclass.22,CATALOG_NBR=courses.2022$CATALOG_NBR,DESCR=courses.2022$DESCR)

#########################Add course information for 2023
#########################Remove students taking multiple classes to compare to embedded
library(dplyr)
library(stringr)
math.oneclass.23 = math.general %>% filter(str_detect(STRM, '239')) %>% filter(enrolled == 1) #3552 observations
#########################obtain course info
courses.2023 = enrollment %>% filter(str_detect(STRM, '239')) %>% filter(str_detect(SUBJECT, 'MATH')) %>% filter(Student_ID %in% math.oneclass.23$Student_ID) %>% filter(enrolled == TRUE) #3175 observations

#add course information
math.oneclass.23 = math.oneclass.23 %>% arrange(Student_ID)
courses.2023 = courses.2023 %>% arrange(Student_ID)
math.oneclass.23 = cbind(math.oneclass.23,CATALOG_NBR=courses.2023$CATALOG_NBR,DESCR=courses.2023$DESCR)

out = rbind(math.oneclass.19,math.oneclass.20,math.oneclass.21,math.oneclass.22,math.oneclass.23)
write.csv(out,"C:/Users/jbrin/Documents/math/fall.20.ssc.math.courseinfo.csv",row.names = F)

#add course information to embedded data######################################################################
#2019
library(dplyr)
math.embedded.19 = math.embedded %>% filter(term == "1199")
embedded.courses.19 = enrollment %>% filter(str_detect(STRM, '199')) %>% filter(CLASS_NBR %in% math.embedded.19$CLASS_NBR) %>% filter(Student_ID %in% math.embedded.19$Student_ID)

#add course information
math.embedded.19 = math.embedded.19 %>% arrange(Student_ID)
embedded.courses.19 = embedded.courses.19 %>% arrange(Student_ID)
math.embedded.19 = cbind(math.embedded.19,CATALOG_NBR=embedded.courses.19$CATALOG_NBR,DESCR=embedded.courses.19$DESCR)

#2020
library(dplyr)
math.embedded.20 = math.embedded %>% filter(str_detect(term, '209'))
embedded.courses.20 = enrollment %>% filter(str_detect(STRM, '209')) %>% filter(CLASS_NBR %in% math.embedded.20$CLASS_NBR) %>% filter(Student_ID %in% math.embedded.20$Student_ID) %>% filter(enrolled==TRUE)

#add course information
math.embedded.20 = math.embedded.20 %>% arrange(Student_ID)
embedded.courses.20 = embedded.courses.20 %>% arrange(Student_ID)
math.embedded.20 = cbind(math.embedded.20,CATALOG_NBR=embedded.courses.20$CATALOG_NBR,DESCR=embedded.courses.20$DESCR)

#2021
library(dplyr)
math.embedded.21 = math.embedded %>% filter(str_detect(term, '219'))
embedded.courses.21 = enrollment %>% filter(str_detect(STRM, '219')) %>% filter(CLASS_NBR %in% math.embedded.21$CLASS_NBR) %>% filter(Student_ID %in% math.embedded.21$Student_ID) %>% filter(enrolled==TRUE)

#add course information
math.embedded.21 = math.embedded.21 %>% arrange(Student_ID)
embedded.courses.21 = embedded.courses.21 %>% arrange(Student_ID)
math.embedded.21 = cbind(math.embedded.21,CATALOG_NBR=embedded.courses.21$CATALOG_NBR,DESCR=embedded.courses.21$DESCR)

#2022
library(dplyr)
math.embedded.22 = math.embedded %>% filter(str_detect(term, '229'))
embedded.courses.22 = enrollment %>% filter(str_detect(STRM, '229')) %>% filter(CLASS_NBR %in% math.embedded.22$CLASS_NBR) %>% filter(Student_ID %in% math.embedded.22$Student_ID) %>% filter(enrolled==TRUE)

#add course information
math.embedded.22 = math.embedded.22 %>% arrange(Student_ID)
embedded.courses.22 = embedded.courses.22 %>% arrange(Student_ID)
math.embedded.22 = cbind(math.embedded.22,CATALOG_NBR=embedded.courses.22$CATALOG_NBR,DESCR=embedded.courses.22$DESCR)

#2023
library(dplyr)
math.embedded.23 = math.embedded %>% filter(str_detect(term, '239'))
embedded.courses.23 = enrollment %>% filter(str_detect(STRM, '239')) %>% filter(CLASS_NBR %in% math.embedded.23$CLASS_NBR) %>% filter(Student_ID %in% math.embedded.23$Student_ID) %>% filter(enrolled==TRUE)

#add course information
math.embedded.23 = math.embedded.23 %>% arrange(Student_ID)
embedded.courses.23 = embedded.courses.23 %>% arrange(Student_ID)
math.embedded.23 = cbind(math.embedded.23,CATALOG_NBR=embedded.courses.23$CATALOG_NBR,DESCR=embedded.courses.23$DESCR)

out = rbind(math.embedded.19,math.embedded.20,math.embedded.21,math.embedded.22,math.embedded.23)
write.csv(out,"C:/Users/jbrin/Documents/math/ET.MTH.19.23.courseinfo.csv",row.names = F)

#add first-gen and gender to embedded data
gen.fg <- demographics %>% dplyr::select(gender,first_gen,Student_ID)
Math.ET.dat2<-inner_join(math.embedded,gen.fg,"Student_ID") 
write.csv(Math.ET.dat2,"C:/Users/jbrin/Documents/math/ET.MTH.19.23.courseinfo.csv",row.names = F)

#############################################################################add course information to chemistry
#2019
chem.oneclass.19 = chem.general %>% filter(STRM == "1199") %>% filter(enrolled == 1) #4516 observations
#########################obtain course info
courses.2019 = enrollment %>% filter(str_detect(STRM, '199')) %>% filter(str_detect(SUBJECT, 'CHEM')) %>% filter(enrolled == TRUE) %>% filter(Student_ID %in% chem.oneclass.19$Student_ID)

#add course information
chem.oneclass.19 = chem.oneclass.19 %>% arrange(Student_ID)
courses.2019 = courses.2019 %>% arrange(Student_ID)
chem.oneclass.19 = cbind(chem.oneclass.19,CATALOG_NBR=courses.2019$CATALOG_NBR,DESCR=courses.2019$DESCR)

#2020
chem.oneclass.20 = chem.general %>% filter(STRM == "1209") %>% filter(enrolled == 1) #4516 observations
#########################obtain course info
courses.2020 = enrollment %>% filter(str_detect(STRM, '209')) %>% filter(str_detect(SUBJECT, 'CHEM')) %>% filter(enrolled == TRUE) %>% filter(Student_ID %in% chem.oneclass.20$Student_ID)

#add course information
chem.oneclass.20 = chem.oneclass.20 %>% arrange(Student_ID)
courses.2020 = courses.2020 %>% arrange(Student_ID)
chem.oneclass.20 = cbind(chem.oneclass.20,CATALOG_NBR=courses.2020$CATALOG_NBR,DESCR=courses.2020$DESCR)

#2021
chem.oneclass.21 = chem.general %>% filter(STRM == "1219") %>% filter(enrolled == 1) #4516 observations
#########################obtain course info
courses.2021 = enrollment %>% filter(str_detect(STRM, '219')) %>% filter(str_detect(SUBJECT, 'CHEM')) %>% filter(enrolled == TRUE) %>% filter(Student_ID %in% chem.oneclass.21$Student_ID)

#add course information
chem.oneclass.21 = chem.oneclass.21 %>% arrange(Student_ID)
courses.2021 = courses.2021 %>% arrange(Student_ID)
chem.oneclass.21 = cbind(chem.oneclass.21,CATALOG_NBR=courses.2021$CATALOG_NBR,DESCR=courses.2021$DESCR)

#2022
chem.oneclass.22 = chem.general %>% filter(STRM == "1229") %>% filter(enrolled == 1) #4516 observations
#########################obtain course info
courses.2022 = enrollment %>% filter(str_detect(STRM, '229')) %>% filter(str_detect(SUBJECT, 'CHEM')) %>% filter(enrolled == TRUE) %>% filter(Student_ID %in% chem.oneclass.22$Student_ID)

#add course information
chem.oneclass.22 = chem.oneclass.22 %>% arrange(Student_ID)
courses.2022 = courses.2022 %>% arrange(Student_ID)
chem.oneclass.22 = cbind(chem.oneclass.22,CATALOG_NBR=courses.2022$CATALOG_NBR,DESCR=courses.2022$DESCR)

#2023
chem.oneclass.23 = chem.general %>% filter(STRM == "1239") %>% filter(enrolled == 1) #4516 observations
#########################obtain course info
courses.2023 = enrollment %>% filter(str_detect(STRM, '239')) %>% filter(str_detect(SUBJECT, 'CHEM')) %>% filter(enrolled == TRUE) %>% filter(Student_ID %in% chem.oneclass.23$Student_ID)

#add course information
chem.oneclass.23 = chem.oneclass.23 %>% arrange(Student_ID)
courses.2023 = courses.2023 %>% arrange(Student_ID)
chem.oneclass.23 = cbind(chem.oneclass.23,CATALOG_NBR=courses.2023$CATALOG_NBR,DESCR=courses.2023$DESCR)

out = rbind(chem.oneclass.19,chem.oneclass.20,chem.oneclass.21,chem.oneclass.22,chem.oneclass.23)
write.csv(out,"C:/Users/jbrin/Documents/fall.19_23.ssc.chem.courseinfo.csv",row.names = F)

#compare math students who did not go to CLASS_NBR#compare math students who did not go to tutoring, with math students in embedded course who didn't attend additional sessions
no.tutoring.19 = math.oneclass.19 %>% filter(tutor == "no")
embedded.no.tutor = math.embedded %>% filter(str_detect(term, '199')) %>% filter(n == 0)

no.tutoring.19$ethnicity = as.factor(no.tutoring.20$ethnicity)
embedded.no.tutor$ethnicity = as.factor(embedded.no.tutor$ethnicity)    

lin.gen = glm(as.factor(pass_rate)~ethnicity,data = no.tutoring.20,family="binomial")
lin.emb = glm(as.factor(pass)~ethnicity,data = embedded.no.tutor,family="binomial")

#compare students in embedded courses who did not go to additional sessions with students who visited the ssc
tutoring.19 = math.general %>% filter(str_detect(STRM, '199')) %>% filter(tutor == "yes")
lin.gen2 = lm(pass_rate~ethnicity,data = tutoring.22)

#compare performance of students who went to additional sessions to those who visited the ssc
embedded.tutor = math.embedded %>% filter(str_detect(term, '199'))

lin.emb2 = lm(pass~ethnicity,data = embedded.tutor)

##########################################stats 2019
#math.general$CATALOG_NBR = as.numeric(math.general$CATALOG_NBR)
stat.19.notutor = math.general %>% filter(str_detect(STRM, '199')) %>% filter(str_detect(CATALOG_NBR, '112')) %>% filter(tutor=="no")
stat.19.embedded.notutor = math.embedded %>% filter(str_detect(term, '199')) %>% filter(str_detect(CATALOG_NBR, '112')) %>% filter(tutor==0)
mean(stat.19.embedded.notutor$CUM_GPA)
mean(stat.19.notutor$CUM_GPA)

summary(lm(pass_rate~ethnicity+first_gen+gender,data=stat.19.notutor))
summary(lm(pass~ethnicity,data=stat.19.embedded.notutor))

#############################################################
library(dplyr)
term.2019 = math.general %>% filter(STRM == 1199)
summary(lm(pass_rate~gpa*session.count,data=term.2019))
