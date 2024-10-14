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
f20.ssc <-success_center %>% filter(between(Logintime, as.Date('2020-08-17'), as.Date('2020-12-18'))) # 24165 students
# unique(f19.ssc$Taskdesc)
# unique(f19.ssc$Subtask)

#students who visited the ssc in fall of 2019 for math tutoring
f20.ssc.math = filter(f20.ssc, grepl('Math', Subtask))
unique(f20.ssc.math$Taskdesc)
unique(f20.ssc.math$Subtask)

#select students who enrolled in math courses in fall 2019
library(tidyverse)
library(stringr)
fall.2020 <- enrollment %>% filter(str_detect(STRM, '209'))
length(unique(fall.2020$Student_ID)) # 24639 student enrolled in fall 2019
fall.2020.math <- fall.2020 %>% filter(str_detect(SUBJECT, 'MATH'))

#compare number of math students who went to tutoring to those who didn't
length(unique(fall.2020.math$Student_ID)) #7549 math students total
length(unique(f20.ssc.math$Student_ID)) #1888 went to tutoring
#remove students not in enrollment table?
#test = anti_join(f19.ssc.math,enrollment,by="Student_ID")

#remove students who went to tutoring from enrollment table
f20.notutoring = anti_join(fall.2020.math, f20.ssc.math, by = "Student_ID")
#get enrollment info for students who went to tutoring
f20.tutoring = enrollment %>% filter(str_detect(STRM, '209')) %>% filter(str_detect(SUBJECT, 'MATH')) %>% filter(Student_ID %in% f20.ssc.math$Student_ID)
length(unique(f20.notutoring$Student_ID)) #5915 students didn't go to tutoring
length(unique(f20.tutoring$Student_ID)) #1634 students went to tutoring

#remove students with zero enrollment
f20.notutoring <- f20.notutoring %>%filter(enrolled != 0) #1723 of 5915 dropped
f20.tutoring <- f20.tutoring %>%filter(enrolled != 0) #60 of 1634 dropped 

# removing students with no ethnicity data 
ethn.inx <-which(!(fall.2020.math$Student_ID %in% demographics$Student_ID),arr.ind = TRUE )
eth.not.present <- fall.2020.math[ethn.inx,] ## some students not found?????
no.ethnicity <- eth.not.present$Student_ID
f20.notutoring <- f20.notutoring %>%  filter(!(Student_ID %in% no.ethnicity))
f20.tutoring <- f20.tutoring %>%  filter(!(Student_ID %in% no.ethnicity))

#count pass rates for students who attended and didn't attend tutoring
f.20.notutoring.enroll.pass <-f20.notutoring %>%  group_by(Student_ID) %>% 
  summarise(courses_enrolled = sum(enrolled), courses_passed = sum(success), pass_rate = sum(success)/sum(enrolled))

f.20.tutoring.enroll.pass <-f20.tutoring %>%  group_by(Student_ID) %>% 
  summarise(courses_enrolled = sum(enrolled), courses_passed = sum(success), pass_rate = sum(success)/sum(enrolled))

#remove students not enrolled from ssc logs
#f19.ssc.math = f19.ssc.math %>% filter(Student_ID %in% f.19.tutoring.enroll.pass$Student_ID)
f20.ssc.math = f20.ssc.math %>% filter(Student_ID %in% f20.tutoring$Student_ID)

#Count tutoring sessions and times for students who attended tutoring
math_stu_id =f20.ssc.math$Student_ID
math_in_time = f20.ssc.math$Logintime
math_out_time = f20.ssc.math$Logouttime

math_time_diff = difftime(math_out_time, math_in_time, units = "mins")
math_time_diff = as.double(math_time_diff)
math_time_df = data.frame(math_stu_id, math_in_time, 
                          math_out_time, math_time_diff)

aggregate_math = aggregate(math_time_diff ~ math_stu_id, data = math_time_df, FUN = sum) 

##### 4.3) counting the number of times the student went to tutoring  #####
session.count.ssc <- f20.ssc.math %>% count(Student_ID) %>% select(n)
#Add ethnicity
ethnicity.math.ssc <- demographics %>% filter(Student_ID %in% f.20.tutoring.enroll.pass$Student_ID) %>% arrange(Student_ID) %>% select(ethnicity)
aggregate_math$ethnicity = ethnicity.math.ssc #add ethnicity to table
aggregate_math$sessions = session.count.ssc #add session count to table
aggregate_math$enrolled = f.20.tutoring.enroll.pass$courses_enrolled
aggregate_math$passed = f.20.tutoring.enroll.pass$courses_passed
aggregate_math$pass_rate = f.20.tutoring.enroll.pass$pass_rate
aggregate_math$STRM = rep(1209, dim(aggregate_math)[1])
aggregate_math$tutor = rep('yes',nrow(aggregate_math))

#add information to students who did not attend tutoring
f.20.notutoring.enroll.pass$ethnicity = demographics %>% filter(Student_ID %in% f.20.notutoring.enroll.pass$Student_ID) %>% arrange(Student_ID) %>% select(ethnicity)
f.20.notutoring.enroll.pass$math_time_diff = rep(0,dim(f.20.notutoring.enroll.pass)[1]) 
f.20.notutoring.enroll.pass$sessions = rep(0,dim(f.20.notutoring.enroll.pass)[1]) 
f.20.notutoring.enroll.pass$STRM = rep(1209,dim(f.20.notutoring.enroll.pass)[1])
#reorganize columns
f.20.notutoring.enroll.pass = f.20.notutoring.enroll.pass[c("Student_ID", "math_time_diff", "ethnicity", "sessions","courses_enrolled","courses_passed","pass_rate","STRM")]
#make names match
f.20.notutoring.enroll.pass <- setNames(f.20.notutoring.enroll.pass, names(aggregate_math)) 
f.20.notutoring.enroll.pass$tutor = rep('no',nrow(f.20.notutoring.enroll.pass))

#complete table
fall.20.SSC = rbind(aggregate_math,f.20.notutoring.enroll.pass)
fall.20.SSC = fall.20.SSC %>% arrange(math_stu_id)

#add cumulative gpa
gpa = academic_career %>% filter(Student_ID %in% fall.20.SSC$math_stu_id) %>% filter(str_detect(STRM, '209')) %>% arrange(Student_ID) %>% select(CUM_GPA)
fall.20.SSC$cum_gpa = gpa

########################add persistence/retention
f20.reten.pers = retention_persistence %>% filter(str_detect(STRM, '209'))
index <- which(f20.reten.pers$Student_ID %in% fall.20.SSC$math_stu_id, arr.ind = TRUE)
f20.reten.pers.math <- f20.reten.pers[index,]
f20.reten.pers.math = f20.reten.pers.math %>% arrange(Student_ID)
fall.20.SSC$ret.f2f = f20.reten.pers.math$f2f_num/f20.reten.pers.math$f2f_denom
fall.20.SSC$pers.f2s = f20.reten.pers.math$f2s_num/f20.reten.pers.math$f2s_denom

#categorize session count and time spent
session.count <- cut(fall.20.SSC$sessions$n, breaks=c(0,3,4,14,214), labels=c('rare', 'occasionally', 'frequently', 'very.frequently')) 
fall.20.SSC$session.count = session.count
time.spent <- cut(fall.20.SSC$math_time_diff, breaks=c(0, 90, 270, 621, 696,15000), labels=c('very.low', 'low', 'average', 'above.average','substantial'))
fall.20.SSC$time.spent = time.spent

#remove students with NA of unknown ethnicity
fall.20.SSC <-fall.20.SSC %>% filter(!str_detect(ethnicity$ethnicity, "NA")) %>% filter(!str_detect(ethnicity$ethnicity, "Unknown"))
# combine alaskan native and native hawaiian
fall.20.SSC$ethnicity[fall.20.SSC$ethnicity == "American Indian or Alaska Native"] <- "Native American or Pacific Islander"
fall.20.SSC$ethnicity[fall.20.SSC$ethnicity == "Native Hawaiian or Other Pacific Islander"] <- "Native American or Pacific Islander"

###############################write table##########################################
# fall.19.embedded = read.csv("C:/Users/jbrin/Documents/chem/ET.F19.CHEM.csv",header=T)
# fall.19.embedded = fall.19.embedded[,-1]

embedded = read.csv("C:/Users/jbrin/Documents/chem/ET.MTH.19.23.csv",header=T)

test2 = fall.20.SSC
islst <- sapply(test2, is.list)
# ASSERT: number of rows and embedded lists/frames are the same length
all(nrow(test2) == sapply(test2[islst], lengths))
test2 <- do.call("cbind.data.frame", c(test2[!islst], test2[islst], stringsAsFactors = FALSE))
table(test2$tutor)
fall20 = embedded %>% filter(term == 1209)
test2 = test2 %>% filter(!(math_stu_id %in% fall20$Student_ID))
table(test2$tutor)

write.csv(test2,"C:/Users/jbrin/Documents/math/fall.20.ssc.math.csv",row.names = F)
#new=read.csv("C:/Users/jbrin/Documents/chem/fall.19.ssc.chem.csv",header=T)