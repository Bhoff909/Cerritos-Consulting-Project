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

#students who visited the ssc in spring of 2020
f20.ssc <-success_center %>% filter(between(Logintime, as.Date('2020-08-17'), as.Date('2020-12-18'))) # 24165 students
unique(f20.ssc$Taskdesc)
unique(f20.ssc$Subtask)

#students who visited the ssc in spring of 2020 for chemistry tutoring
f20.ssc.chem = filter(f20.ssc, grepl('Chemistry', Subtask))
#f19.ssc.chem = f19.ssc %>% filter(str_detect(Subtask, 'Chemistry'))
unique(f20.ssc.chem$Taskdesc)
unique(f20.ssc.chem$Subtask)

#select students who enrolled in chemistry courses in fall 2019
library(tidyverse)
library(stringr)
fall.2020 <- enrollment %>% filter(str_detect(STRM, '209'))
length(unique(fall.2020$Student_ID)) # 24639 student enrolled in fall 2019
fall.2020.chem <- fall.2020 %>% filter(str_detect(SUBJECT, 'CHEM'))

#compare number of chemistry students who went to tutoring to those who didn't
length(unique(fall.2020.chem$Student_ID)) #1078 chem students total
length(unique(f20.ssc.chem$Student_ID)) #215 went to tutoring, should be 199

#remove students who went to tutoring from enrollment table
f20.chem.notutoring = anti_join(fall.2020.chem, f20.ssc.chem, by = "Student_ID")
#get enrollment info for students who went to tutoring
f20.chem.tutoring = enrollment %>% filter(str_detect(STRM, '209')) %>% filter(str_detect(SUBJECT, 'CHEM')) %>% filter(Student_ID %in% f20.ssc.chem$Student_ID)
length(unique(f20.chem.notutoring$Student_ID)) #879 students didn't go to tutoring
length(unique(f20.chem.tutoring$Student_ID)) #199 students went to tutoring

#Why don't the totals on lines 32, 33, 39, and 40, match?
#too many students in line 33

#remove students with zero enrollment
f20.chem.notutoring <- f20.chem.notutoring %>%filter(enrolled != 0) #1723 of 5915 dropped
f20.chem.tutoring <- f20.chem.tutoring %>%filter(enrolled != 0) #60 of 1634 dropped 

# removing students with no ethnicity data 
ethn.inx <-which(!(fall.2020.chem$Student_ID %in% demographics$Student_ID),arr.ind = TRUE )
eth.not.present <- fall.2020.chem[ethn.inx,] ## some students not found?????
no.ethnicity <- eth.not.present$Student_ID
f20.chem.notutoring <- f20.chem.notutoring %>%  filter(!(Student_ID %in% no.ethnicity))
f20.chem.tutoring <- f20.chem.tutoring %>%  filter(!(Student_ID %in% no.ethnicity))

#count pass rates for students who attended and didn't attend tutoring
f.20.chem.notutoring.enroll.pass <-f20.chem.notutoring %>%  group_by(Student_ID) %>% 
  summarise(courses_enrolled = sum(enrolled), courses_passed = sum(success), pass_rate = sum(success)/sum(enrolled))

f.20.chem.tutoring.enroll.pass <-f20.chem.tutoring %>%  group_by(Student_ID) %>% 
  summarise(courses_enrolled = sum(enrolled), courses_passed = sum(success), pass_rate = sum(success)/sum(enrolled))

#remove students not enrolled from ssc logs
#f19.ssc.math = f19.ssc.math %>% filter(Student_ID %in% f.19.tutoring.enroll.pass$Student_ID)
f20.ssc.chem = f20.ssc.chem %>% filter(Student_ID %in% f.20.chem.tutoring.enroll.pass$Student_ID)

#Count tutoring sessions and times for students who attended tutoring
chem_stu_id =f20.ssc.chem$Student_ID
chem_in_time = f20.ssc.chem$Logintime
chem_out_time = f20.ssc.chem$Logouttime

chem_time_diff = difftime(chem_out_time, chem_in_time, units = "mins")
chem_time_diff = as.double(chem_time_diff)
chem_time_df = data.frame(chem_stu_id, chem_in_time,
                          chem_out_time, chem_time_diff)

aggregate_chem = aggregate(chem_time_diff ~ chem_stu_id, data = chem_time_df, FUN = sum)

##### 4.3) counting the number of times the student went to tutoring  #####
session.count.ssc <- f20.ssc.chem %>% count(Student_ID) %>% select(n)
#Add ethnicity
ethnicity.chem.ssc <- demographics %>% filter(Student_ID %in% f.20.chem.tutoring.enroll.pass$Student_ID) %>% arrange(Student_ID) %>% select(ethnicity)
aggregate_chem$ethnicity = ethnicity.chem.ssc #add ethnicity to table
aggregate_chem$sessions = session.count.ssc #add session count to table
aggregate_chem$enrolled = f.20.chem.tutoring.enroll.pass$courses_enrolled
aggregate_chem$passed = f.20.chem.tutoring.enroll.pass$courses_passed
aggregate_chem$pass_rate = f.20.chem.tutoring.enroll.pass$pass_rate
aggregate_chem$STRM = rep(1209, dim(aggregate_chem)[1])
aggregate_chem$tutor = rep('yes',nrow(aggregate_chem))

#add information to students who did not attend tutoring
f.20.chem.notutoring.enroll.pass$ethnicity = demographics %>% filter(Student_ID %in% f.20.chem.notutoring.enroll.pass$Student_ID) %>% arrange(Student_ID) %>% select(ethnicity)
f.20.chem.notutoring.enroll.pass$math_time_diff = rep(0,dim(f.20.chem.notutoring.enroll.pass)[1]) 
f.20.chem.notutoring.enroll.pass$sessions = rep(0,dim(f.20.chem.notutoring.enroll.pass)[1]) 
f.20.chem.notutoring.enroll.pass$STRM = rep(1209,dim(f.20.chem.notutoring.enroll.pass)[1])
#reorganize columns
f.20.chem.notutoring.enroll.pass = f.20.chem.notutoring.enroll.pass[c("Student_ID", "math_time_diff", "ethnicity", "sessions","courses_enrolled","courses_passed","pass_rate","STRM")]
#make names match
f.20.chem.notutoring.enroll.pass <- setNames(f.20.chem.notutoring.enroll.pass, names(aggregate_chem)) 
f.20.chem.notutoring.enroll.pass$tutor = rep('no',nrow(f.20.chem.notutoring.enroll.pass))

#complete table
fall.20.SSC.chem = rbind(aggregate_chem,f.20.chem.notutoring.enroll.pass)
fall.20.SSC.chem = fall.20.SSC.chem %>% arrange(chem_stu_id)

#add cumulative gpa
gpa.chem = academic_career %>% filter(Student_ID %in% fall.20.SSC.chem$chem_stu_id) %>% filter(str_detect(STRM, '209')) %>% arrange(Student_ID) %>% select(CUM_GPA)
fall.20.SSC.chem$cum_gpa = gpa.chem

########################add persistence/retention
f20.reten.pers = retention_persistence %>% filter(str_detect(STRM, '209'))
index <- which(f20.reten.pers$Student_ID %in% fall.20.SSC.chem$chem_stu_id, arr.ind = TRUE)
f20.reten.pers.chem <- f20.reten.pers[index,]
f20.reten.pers.chem = f20.reten.pers.chem %>% arrange(Student_ID)
fall.20.SSC.chem$ret.f2f = f20.reten.pers.chem$f2f_num/f20.reten.pers.chem$f2f_denom
fall.20.SSC.chem$pers.f2s = f20.reten.pers.chem$f2s_num/f20.reten.pers.chem$f2s_denom

#categorize session count and time spent
session.count <- cut(fall.20.SSC.chem$sessions$n, breaks=c(0,3,4,14,37), labels=c('rare', 'occasionally', 'frequently', 'very.frequently')) 
fall.20.SSC.chem$session.count = session.count
time.spent <- cut(fall.20.SSC.chem$chem_time_diff, breaks=c(0, 60, 153, 386, 467,2800), labels=c('very.low', 'low', 'average', 'above.average','substantial'))
fall.20.SSC.chem$time.spent = time.spent

#remove students with NA of unknown ethnicity
fall.20.SSC.chem <-fall.20.SSC.chem %>% filter(!str_detect(ethnicity$ethnicity, "NA")) %>% filter(!str_detect(ethnicity$ethnicity, "Unknown"))
# combine alaskan native and native hawaiian
fall.20.SSC.chem$ethnicity[fall.20.SSC.chem$ethnicity == "American Indian or Alaska Native"] <- "Native American or Pacific Islander"
fall.20.SSC.chem$ethnicity[fall.20.SSC.chem$ethnicity == "Native Hawaiian or Other Pacific Islander"] <- "Native American or Pacific Islander"

###############################write table##########################################
embedded = read.csv("C:/Users/jbrin/Documents/chem/ET.chem.ids.csv",header=T)
fall.20.embedded = embedded %>% filter(term == 1209)

test2 = fall.20.SSC.chem
islst <- sapply(test2, is.list)
# ASSERT: number of rows and embedded lists/frames are the same length
all(nrow(test2) == sapply(test2[islst], lengths))
test2 <- do.call("cbind.data.frame", c(test2[!islst], test2[islst], stringsAsFactors = FALSE))
table(test2$tutor)
test2 = test2 %>% filter(!(chem_stu_id %in% fall.20.embedded$Student_ID))
table(test2$tutor)

write.csv(test2,"C:/Users/jbrin/Documents/fall.20.ssc.chem.csv",row.names = F)

