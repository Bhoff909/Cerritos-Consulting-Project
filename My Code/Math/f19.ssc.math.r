##################################Remove embedded data from ssc_tutor
library(dplyr)
success_center = anti_join(ssc_tutor, embedded_tutor, by = c("Subtask","Taskdesc"))

unique(success_center$Subtask)
unique(success_center$Taskdesc)

#remove residual embedded information
success_center = success_center %>% filter(Taskdesc != 'FA23 EMB Tutoring PHIL' & 
                                           Taskdesc != 'FA23 EMB Tutoring AP' & 
                                           Taskdesc != 'FA23 EMB Tutoring PHIL' & 
                                           Taskdesc != 'FA23 EMB Tutoring CHEM' &
                                           Taskdesc != 'FA23 EMB Tutoring ACCT')

#students who visited the ssc in fall of 2019
f19.ssc <-success_center %>% filter(between(Logintime, as.Date('2019-08-19'), as.Date('2019-12-20'))) # 24165 students
# unique(f19.ssc$Taskdesc)
# unique(f19.ssc$Subtask)

#students who visited the ssc in fall of 2019 for math tutoring
f19.ssc.math = filter(f19.ssc, grepl('Math', Subtask))
unique(f19.ssc.math$Taskdesc)
unique(f19.ssc.math$Subtask)

#select students who enrolled in math courses in fall 2019
library(tidyverse)
library(stringr)
fall.2019 <- enrollment %>% filter(str_detect(STRM, '199'))
length(unique(fall.2019$Student_ID)) # 24639 student enrolled in fall 2019
fall.2019.math <- fall.2019 %>% filter(str_detect(SUBJECT, 'MATH'))

#compare number of math students who went to tutoring to those who didn't
length(unique(fall.2019.math$Student_ID)) #7549 math students total
length(unique(f19.ssc.math$Student_ID)) #1888 went to tutoring
#remove students not in enrollment table?
#test = anti_join(f19.ssc.math,enrollment,by="Student_ID")

#remove students who went to tutoring from enrollment table
f19.notutoring = anti_join(fall.2019.math, f19.ssc.math, by = "Student_ID")
#get enrollment info for students who went to tutoring
f19.tutoring = enrollment %>% filter(str_detect(STRM, '199')) %>% filter(str_detect(SUBJECT, 'MATH')) %>% filter(Student_ID %in% f19.ssc.math$Student_ID)
length(unique(f19.notutoring$Student_ID)) #5915 students didn't go to tutoring
length(unique(f19.tutoring$Student_ID)) #1634 students went to tutoring

#remove students with zero enrollment
f19.notutoring <- f19.notutoring %>%filter(enrolled != 0) #1723 of 5915 dropped
f19.tutoring <- f19.tutoring %>%filter(enrolled != 0) #60 of 1634 dropped 

# removing students with no ethnicity data 
ethn.inx <-which(!(fall.2019.math$Student_ID %in% demographics$Student_ID),arr.ind = TRUE )
eth.not.present <- fall.2019.math[ethn.inx,] ## some students not found?????
no.ethnicity <- eth.not.present$Student_ID
f19.notutoring <- f19.notutoring %>%  filter(!(Student_ID %in% no.ethnicity))
f19.tutoring <- f19.tutoring %>%  filter(!(Student_ID %in% no.ethnicity))

#count pass rates for students who attended and didn't attend tutoring
f.19.notutoring.enroll.pass <-f19.notutoring %>%  group_by(Student_ID) %>% 
  summarise(courses_enrolled = sum(enrolled), courses_passed = sum(success), pass_rate = sum(success)/sum(enrolled))

f.19.tutoring.enroll.pass <-f19.tutoring %>%  group_by(Student_ID) %>% 
  summarise(courses_enrolled = sum(enrolled), courses_passed = sum(success), pass_rate = sum(success)/sum(enrolled))

f19.tutoring.courses = unique(f19.tutoring$DESCR)

#remove students not enrolled from ssc logs
#f19.ssc.math = f19.ssc.math %>% filter(Student_ID %in% f.19.tutoring.enroll.pass$Student_ID)
f19.ssc.math = f19.ssc.math %>% filter(Student_ID %in% f19.tutoring$Student_ID)

#Count tutoring sessions and times for students who attended tutoring
math_stu_id =f19.ssc.math$Student_ID
math_in_time = f19.ssc.math$Logintime
math_out_time = f19.ssc.math$Logouttime

math_time_diff = difftime(math_out_time, math_in_time, units = "mins")
math_time_diff = as.double(math_time_diff)
math_time_df = data.frame(math_stu_id, math_in_time, 
                          math_out_time, math_time_diff)

aggregate_math = aggregate(math_time_diff ~ math_stu_id, data = math_time_df, FUN = sum) 

##### 4.3) counting the number of times the student went to tutoring  #####
session.count.ssc <- f19.ssc.math %>% count(Student_ID) %>% select(n)
#Add ethnicity
ethnicity.math.ssc <- demographics %>% filter(Student_ID %in% f.19.tutoring.enroll.pass$Student_ID) %>% arrange(Student_ID) %>% select(ethnicity)
aggregate_math$ethnicity = ethnicity.math.ssc #add ethnicity to table
aggregate_math$sessions = session.count.ssc #add session count to table
aggregate_math$enrolled = f.19.tutoring.enroll.pass$courses_enrolled
aggregate_math$passed = f.19.tutoring.enroll.pass$courses_passed
aggregate_math$pass_rate = f.19.tutoring.enroll.pass$pass_rate
aggregate_math$STRM = rep(1199, dim(aggregate_math)[1])
aggregate_math$tutor = rep('yes',nrow(aggregate_math))

#add information to students who did not attend tutoring
f.19.notutoring.enroll.pass$ethnicity = demographics %>% filter(Student_ID %in% f.19.notutoring.enroll.pass$Student_ID) %>% arrange(Student_ID) %>% select(ethnicity)
f.19.notutoring.enroll.pass$math_time_diff = rep(0,dim(f.19.notutoring.enroll.pass)[1]) 
f.19.notutoring.enroll.pass$sessions = rep(0,dim(f.19.notutoring.enroll.pass)[1]) 
f.19.notutoring.enroll.pass$STRM = rep(1199,dim(f.19.notutoring.enroll.pass)[1])
#reorganize columns
f.19.notutoring.enroll.pass = f.19.notutoring.enroll.pass[c("Student_ID", "math_time_diff", "ethnicity", "sessions","courses_enrolled","courses_passed","pass_rate","STRM")]
#make names match
f.19.notutoring.enroll.pass <- setNames(f.19.notutoring.enroll.pass, names(aggregate_math)) 
f.19.notutoring.enroll.pass$tutor = rep('no',nrow(f.19.notutoring.enroll.pass))

#complete table
fall.19.SSC = rbind(aggregate_math,f.19.notutoring.enroll.pass)
fall.19.SSC = fall.19.SSC %>% arrange(math_stu_id)

#add cumulative gpa
gpa = academic_career %>% filter(Student_ID %in% fall.19.SSC$math_stu_id) %>% filter(str_detect(STRM, '199')) %>% arrange(Student_ID) %>% select(CUM_GPA)
fall.19.SSC$cum_gpa = gpa

########################add persistence/retention
f19.reten.pers = retention_persistence %>% filter(str_detect(STRM, '199'))
index <- which(f19.reten.pers$Student_ID %in% fall.19.SSC$math_stu_id, arr.ind = TRUE)
f19.reten.pers.math <- f19.reten.pers[index,]
f19.reten.pers.math = f19.reten.pers.math %>% arrange(Student_ID)
fall.19.SSC$ret.f2f = f19.reten.pers.math$f2f_num/f19.reten.pers.math$f2f_denom
fall.19.SSC$pers.f2s = f19.reten.pers.math$f2s_num/f19.reten.pers.math$f2s_denom

#categorize session count and time spent
session.count <- cut(fall.19.SSC$sessions$n, breaks=c(0,3,4,14,214), labels=c('rare', 'occasionally', 'frequently', 'very.frequently')) 
fall.19.SSC$session.count = session.count
time.spent <- cut(fall.19.SSC$math_time_diff, breaks=c(0, 90, 270, 621, 696,15000), labels=c('very.low', 'low', 'average', 'above.average','substantial'))
fall.19.SSC$time.spent = time.spent

#remove students with NA of unknown ethnicity
fall.19.SSC <-fall.19.SSC %>% filter(!str_detect(ethnicity$ethnicity, "NA")) %>% filter(!str_detect(ethnicity$ethnicity, "Unknown"))
# combine alaskan native and native hawaiian
fall.19.SSC$ethnicity[fall.19.SSC$ethnicity == "American Indian or Alaska Native"] <- "Native American or Pacific Islander"
fall.19.SSC$ethnicity[fall.19.SSC$ethnicity == "Native Hawaiian or Other Pacific Islander"] <- "Native American or Pacific Islander"

###############################write table##########################################
fall.19.embedded = read.csv("C:/Users/jbrin/Documents/chem/ET.F19.CHEM.csv",header=T)
fall.19.embedded = fall.19.embedded[,-1]

embedded = read.csv("C:/Users/jbrin/Documents/chem/ET.MTH.19.23.csv",header=T)

test2 = fall.19.SSC
islst <- sapply(test2, is.list)
# ASSERT: number of rows and embedded lists/frames are the same length
all(nrow(test2) == sapply(test2[islst], lengths))
test2 <- do.call("cbind.data.frame", c(test2[!islst], test2[islst], stringsAsFactors = FALSE))
table(test2$tutor)
fall19 = embedded %>% filter(term == 1199)
test2 = test2 %>% filter(!(math_stu_id %in% fall19$Student_ID))
table(test2$tutor)

write.csv(test2,"C:/Users/jbrin/Documents/math/fall.19.ssc.math.csv",row.names = F)
#new=read.csv("C:/Users/jbrin/Documents/chem/fall.19.ssc.chem.csv",header=T)



#need to add levels of time spent in tutoring, levels Student_ID#need to add levels of time spent in tutoring, levels of numbers of sessions attended, whether student started attending tutoring in the beginning, middle, or end of semester

#plot gpa densities for two groups
plot(density(fall.19.SSC$cum_gpa$CUM_GPA[fall.19.SSC$sessions > 0]),col="blue", main="GPA")
lines(density(fall.19.SSC$cum_gpa$CUM_GPA[fall.19.SSC$sessions == 0]),col="red")

#group by ethnicity
plot(density(na.omit(fall.19.SSC$cum_gpa$CUM_GPA[fall.19.SSC$ethnicity$ethnicity == "Asian"])),col="blue", main="GPA")
lines(density(na.omit(fall.19.SSC$cum_gpa$CUM_GPA[fall.19.SSC$ethnicity$ethnicity == "Hispanic/Latino"])),col="red")
lines(density(na.omit(fall.19.SSC$cum_gpa$CUM_GPA[fall.19.SSC$ethnicity$ethnicity == "White"])),col="green")
lines(density(na.omit(fall.19.SSC$cum_gpa$CUM_GPA[fall.19.SSC$ethnicity$ethnicity == "Black or African American"])),col="orange")

#plot success rates for two groups
tutoring_success = sum(fall.19.SSC$passed[fall.19.SSC$sessions > 0])/sum(fall.19.SSC$enrolled[fall.19.SSC$sessions > 0])
notutoring_success = sum(fall.19.SSC$passed[fall.19.SSC$sessions == 0])/sum(fall.19.SSC$enrolled[fall.19.SSC$sessions == 0])
success = data.frame(tutoring_success,notutoring_success)

# barplot(c(tutoring_success,notutoring_success),names.arg = c("Tutoring","No Tutoring"),xlab = "Success Rate")
# sum(fall.19.SSC$passed[fall.19.SSC$sessions > 0])/sum(fall.19.SSC$enrolled[fall.19.SSC$sessions > 0]) %>% ggplot()

##########################success by tutored and non tutored
success.math.tutoring <- ggplot(fall.19.SSC, aes(x = passed, fill = tutor)) + geom_bar()+labs()+
  stat_count(geom = "text", colour = "navy", size =4,
             aes(label = ..prop.. %>% scales::percent()),position=position_stack(vjust=0.5))+
  ggtitle("Success Rates")

# scc.tutor<-fall.19.SSC %>% filter(sessions>0)
# success_rate_plot = ggplot(scc.tutor, x = pass_rate) + geom_bar(aes(fill = ethnicity$ethnicity))+labs()+
#   stat_count(geom = "text", colour = "navy", size =5,
#              aes(label = ..count..),position=position_stack(vjust=0.5))+
#   ggtitle("Success Rate")
# 
# success_rate_plot = ggplot(success, x = pass_rate) + geom_bar(aes(fill = ethnicity$ethnicity))+labs()+
#   stat_count(geom = "text", colour = "navy", size =5,
#              aes(label = ..count..),position=position_stack(vjust=0.5))+
#   ggtitle("Success Rate")
# ggplot(success, x = success)
# 
# success.rate <-data.frame(success.rate = c(rep("tutor", 53), rep("no.tutor",38)))
# ggplot(success.rate, aes(success.rate))+geom_bar(aes(fill = success.rate))
# 
# tutoring.ssc <- fall.19.SSC %>% filter(sessions>0) %>% group_by(ethnicity) %>% sum(passed)/sum(enrolled)
# #out = tutoring.ssc %>% group_by(ethnicity) %>% sum(passed)/sum(enrolled)

##########################success by ethnicity
YET.success.rate.ethnicity.tutoring <- fall.19.SSC %>% filter(sessions>0) %>% select(ethnicity, passed) %>% group_by(ethnicity) %>% 
  count(passed) %>%  mutate(prop= n / sum(n))

plot1 <- ggplot(YET.success.rate.ethnicity.tutoring ,aes(ethnicity$ethnicity, n, fill =as.factor(passed))) + 
  geom_col(position = position_stack()) + 
  geom_text(aes(label = n), position = position_stack(vjust =0.5), size =5)

YET.success.rate.ethnicity.notutoring <- fall.19.SSC %>% filter(sessions==0) %>% select(ethnicity, passed) %>% group_by(ethnicity) %>% 
  count(passed) %>%  mutate(prop= n / sum(n))

plot2 <- ggplot(YET.success.rate.ethnicity.notutoring ,aes(ethnicity$ethnicity, n, fill =as.factor(passed))) + 
  geom_col(position = position_stack()) + 
  geom_text(aes(label = n), position = position_stack(vjust =0.5), size =5)

library(cowplot)
plot_grid(plot1,plot2)
##########################
#distribution of time spent
##########################

#plot persistence rate between groups
tutoring_persistence = sum(!is.nan(fall.19.SSC$pers_f2s[fall.19.SSC$sessions > 0 & fall.19.SSC$pers_f2s == 1]))/sum(!is.nan(fall.19.SSC$pers_f2s[fall.19.SSC$sessions > 0]))
notutoring_persistence = sum(!is.nan(fall.19.SSC$pers_f2s[fall.19.SSC$sessions == 0 & fall.19.SSC$pers_f2s == 1]))/sum(!is.nan(fall.19.SSC$pers_f2s[fall.19.SSC$sessions == 0]))

barplot(c(tutoring_persistence,notutoring_persistence),names.arg = c("Tutoring","No Tutoring"),xlab = "Persistence Rate")

#plot retention rate between groups
tutoring_retention = sum(!is.nan(fall.19.SSC$ret.f2f[fall.19.SSC$sessions > 0 & fall.19.SSC$ret.f2f == 1]))/sum(!is.nan(fall.19.SSC$ret.f2f[fall.19.SSC$sessions > 0]))
notutoring_retention = sum(!is.nan(fall.19.SSC$ret.f2f[fall.19.SSC$sessions == 0 & fall.19.SSC$ret.f2f == 1]))/sum(!is.nan(fall.19.SSC$ret.f2f[fall.19.SSC$sessions == 0]))

barplot(c(tutoring_retention,notutoring_retention),names.arg = c("Tutoring","No Tutoring"),xlab = "Retention Rate")

persist.YET.barplot <- ggplot(fall.19.SSC, aes(x = pers.f2s, fill = ethnicity$ethnicity)) + geom_bar()+labs()+
  stat_count(geom = "text", colour = "navy", size =5,
             aes(label = ..count..),position=position_stack(vjust=0.5))+
  ggtitle("Fall to Spring Persistence")

reten.YET.barplot <- ggplot(fall.19.SSC, aes(x =ret.f2f, fill = ethnicity$ethnicity)) + geom_bar()+labs()+
  stat_count(geom = "text", colour = "navy", size =5,
             aes(label = ..count..),position=position_stack(vjust=0.5))+
  ggtitle("Fall to Fall Retention")

plot_grid(persist.YET.barplot,reten.YET.barplot)

#success rate by # of sessions

success.rate.sessions.tutoring = fall.19.SSC %>% filter(sessions>0) %>% select(session.count, passed, enrolled) %>% group_by(enrolled) %>% 
  count(passed) %>%  mutate(prop= n / sum(n))

plot5 <- ggplot(success.rate.sessions.tutoring ,aes(enrolled, n, fill =as.factor(passed))) + 
  geom_col(position = position_stack()) + 
  geom_text(aes(label = round(prop,2)), position = position_stack(vjust =0.5), size =3)+
  ggtitle("Success Rates by Number of Courses Taken, with Tutoring")

success.rate.sessions.notutoring = fall.19.SSC %>% filter(sessions==0) %>% select(session.count, passed, enrolled) %>% group_by(enrolled) %>% 
  count(passed) %>%  mutate(prop= n / sum(n))

plot7 <- ggplot(success.rate.sessions.notutoring ,aes(enrolled, n, fill =as.factor(passed))) + 
  geom_col(position = position_stack()) + 
  geom_text(aes(label = round(prop,2)), position = position_stack(vjust =0.5), size =3)+
  ggtitle("Success Rates by Number of Courses Taken, without Tutoring")

#counts of student visits########################################################
test = fall.19.SSC %>% filter(sessions>0)
ET.count.barplot <-ggplot(test, aes(session.count,color=session.count,sort.val = "desc")) +geom_bar(fill = "white")+
  stat_count(geom = "text", colour = "navy", size =5,
             aes(label = ..count..),position=position_stack(vjust=0.5))+
  ggtitle("Number of times F19 Math students went to the SSC")+
  xlab("")+labs(color ="")+
  theme(text = element_text(size=15))

ET.count.barplot+
  scale_color_manual(name = "Count",
                     values = c("black","limegreen","blue", "red"), 
                     labels = c("rare: 1-2", "occasionally: 3-4", "frequently: 5-14","very frequently: 15-214"),
                     guide = guide_legend(override.aes = list(fill = c("NA","NA","NA","NA"))))

#time spent
ET.time.barplot<- ggplot(na.omit(test), aes(time.spent,color=time.spent,sort.val = "desc")) +geom_bar(fill = "white")+ 
  stat_count(geom = "text", colour = "navy", size =5,
             aes(label = ..count..),position=position_stack(vjust=0.5))+
  ggtitle("Total Time Spent at SSC")+xlab("")+labs(color ="")

ET.time.barplot+
  scale_color_manual(name = "Minutes Spent",
                     values = c("pink2","orange","purple","navy", "forestgreen"),
                     labels=c("Very low: 0-49", "Low: 50-249", "Average: 250-599","Above average: 600-1249","Substantial: 1250-1910"))

#miriams
gpa.cur.curve <-ggplot(fall.19.SSC,aes(x = cum_gpa$CUM_GPA, col =ethnicity$ethnicity ))+geom_density(alpha = 0.5, position = "identity")+
  ggtitle("F19 Cumulative GPA of Math Students")
gpa.cur.curve
# 
# time_spent2 <- cut(time_spent$math_time_diff,
#                    breaks=c(0, 50, 250, 600, 1250,1910),
#                    labels=c('very.low', 'low', 'average', 'above.avergae','substantial'))
# #pass rate for tutored students
# sum(f.19.chem.tutoring.enroll.pass$courses_passed)/sum(f.19.chem.tutoring.enroll.pass$courses_enrolled)
# sum(f.19.chem.notutoring.enroll.pass$passed)/sum(f.19.chem.notutoring.enroll.pass$enrolled)

#ethnicity cleaning
# unique(demographics$ethnicity)
# clean.ethn <-demographics %>% filter(!str_detect(ethnicity, "NA")) %>% filter(!str_detect(ethnicity, "Unknown"))
# # combine alaskan native and native hawaiian
# clean.ethn$ethnicity[clean.ethn$ethnicity == "American Indian or Alaska Native"] <- "Native American or Pacific Islander"
# clean.ethn$ethnicity[clean.ethn$ethnicity == "Native Hawaiian or Other Pacific Islander"] <- "Native American or Pacific Islander"
# unique(clean.ethn$ethnicity)
# # remove student with unknown ethnicity
# ethnicity.na.math <- demographics %>% filter(Student_ID %in% mt.f19.ET$Student_ID) %>% arrange(Student_ID) %>% select(Student_ID,ethnicity)
# ethnicity.na.math <- ethnicity.na.math %>% filter(str_detect(ethnicity, "Unknown"))
# 
# ethnicity.math <- clean.ethn %>% filter(Student_ID %in% aggregate_math$math_stu_id) %>% arrange(Student_ID) %>% select(ethnicity)
# 
# eth.subset <- ret3 %>%
#   filter(ethnicity == c(Asian, Hispanic,Black)

# #save table
# library(readr)
# write_csv(fall.19.SSC,"C:\\fall.19.SSC.csv")
