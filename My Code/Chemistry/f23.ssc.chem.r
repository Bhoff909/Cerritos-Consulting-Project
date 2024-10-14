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
f23.ssc <-success_center %>% filter(between(Logintime, as.Date('2023-08-14'), as.Date('2023-12-15'))) # 24165 students
unique(f23.ssc$Taskdesc)
unique(f23.ssc$Subtask)

#students who visited the ssc in spring of 2020 for chemistry tutoring
f23.ssc.chem = filter(f23.ssc, grepl('CHEM', Subtask))
#f19.ssc.chem = f19.ssc %>% filter(str_detect(Subtask, 'Chemistry'))
unique(f23.ssc.chem$Taskdesc)
unique(f23.ssc.chem$Subtask)

#select students who enrolled in chemistry courses in fall 2019
library(tidyverse)
library(stringr)
fall.2023 <- enrollment %>% filter(str_detect(STRM, '239'))
length(unique(fall.2023$Student_ID)) # 24639 student enrolled in fall 2019
fall.2023.chem <- fall.2023 %>% filter(str_detect(SUBJECT, 'CHEM'))

#compare number of chemistry students who went to tutoring to those who didn't
length(unique(fall.2023.chem$Student_ID)) #1078 chem students total
length(unique(f23.ssc.chem$Student_ID)) #215 went to tutoring, should be 199

#remove students who went to tutoring from enrollment table
f23.chem.notutoring = anti_join(fall.2023.chem, f23.ssc.chem, by = "Student_ID")
#get enrollment info for students who went to tutoring
f23.chem.tutoring = enrollment %>% filter(str_detect(STRM, '239')) %>% filter(str_detect(SUBJECT, 'CHEM')) %>% filter(Student_ID %in% f23.ssc.chem$Student_ID)
length(unique(f23.chem.notutoring$Student_ID)) #879 students didn't go to tutoring
length(unique(f23.chem.tutoring$Student_ID)) #199 students went to tutoring

#Why don't the totals on lines 32, 33, 39, and 40, match?
#too many students in line 33

#remove students with zero enrollment
f23.chem.notutoring <- f23.chem.notutoring %>%filter(enrolled != 0) #1723 of 5915 dropped
f23.chem.tutoring <- f23.chem.tutoring %>%filter(enrolled != 0) #60 of 1634 dropped 

# removing students with no ethnicity data 
ethn.inx <-which(!(fall.2023.chem$Student_ID %in% demographics$Student_ID),arr.ind = TRUE )
eth.not.present <- fall.2023.chem[ethn.inx,] ## some students not found?????
no.ethnicity <- eth.not.present$Student_ID
f23.chem.notutoring <- f23.chem.notutoring %>%  filter(!(Student_ID %in% no.ethnicity))
f23.chem.tutoring <- f23.chem.tutoring %>%  filter(!(Student_ID %in% no.ethnicity))

#count pass rates for students who attended and didn't attend tutoring
f.23.chem.notutoring.enroll.pass <-f23.chem.notutoring %>%  group_by(Student_ID) %>% 
  summarise(courses_enrolled = sum(enrolled), courses_passed = sum(success), pass_rate = sum(success)/sum(enrolled))

f.23.chem.tutoring.enroll.pass <-f23.chem.tutoring %>%  group_by(Student_ID) %>% 
  summarise(courses_enrolled = sum(enrolled), courses_passed = sum(success), pass_rate = sum(success)/sum(enrolled))

#remove students not enrolled from ssc logs
#f19.ssc.math = f19.ssc.math %>% filter(Student_ID %in% f.19.tutoring.enroll.pass$Student_ID)
f23.ssc.chem = f23.ssc.chem %>% filter(Student_ID %in% f.23.chem.tutoring.enroll.pass$Student_ID)

#Count tutoring sessions and times for students who attended tutoring
chem_stu_id =f23.ssc.chem$Student_ID
chem_in_time = f23.ssc.chem$Logintime
chem_out_time = f23.ssc.chem$Logouttime

chem_time_diff = difftime(chem_out_time, chem_in_time, units = "mins")
chem_time_diff = as.double(chem_time_diff)
chem_time_df = data.frame(chem_stu_id, chem_in_time,
                          chem_out_time, chem_time_diff)

aggregate_chem = aggregate(chem_time_diff ~ chem_stu_id, data = chem_time_df, FUN = sum)

##### 4.3) counting the number of times the student went to tutoring  #####
session.count.ssc <- f23.ssc.chem %>% count(Student_ID) %>% select(n)
#Add ethnicity
ethnicity.chem.ssc <- demographics %>% filter(Student_ID %in% f.23.chem.tutoring.enroll.pass$Student_ID) %>% arrange(Student_ID) %>% select(ethnicity)
aggregate_chem$ethnicity = ethnicity.chem.ssc #add ethnicity to table
aggregate_chem$sessions = session.count.ssc #add session count to table
aggregate_chem$enrolled = f.23.chem.tutoring.enroll.pass$courses_enrolled
aggregate_chem$passed = f.23.chem.tutoring.enroll.pass$courses_passed
aggregate_chem$pass_rate = f.23.chem.tutoring.enroll.pass$pass_rate
aggregate_chem$STRM = rep(1239, dim(aggregate_chem)[1])
aggregate_chem$tutor = rep('yes',nrow(aggregate_chem))

#add information to students who did not attend tutoring
f.23.chem.notutoring.enroll.pass$ethnicity = demographics %>% filter(Student_ID %in% f.23.chem.notutoring.enroll.pass$Student_ID) %>% arrange(Student_ID) %>% select(ethnicity)
f.23.chem.notutoring.enroll.pass$math_time_diff = rep(0,dim(f.23.chem.notutoring.enroll.pass)[1]) 
f.23.chem.notutoring.enroll.pass$sessions = rep(0,dim(f.23.chem.notutoring.enroll.pass)[1]) 
f.23.chem.notutoring.enroll.pass$STRM = rep(1239,dim(f.23.chem.notutoring.enroll.pass)[1])
#reorganize columns
f.23.chem.notutoring.enroll.pass = f.23.chem.notutoring.enroll.pass[c("Student_ID", "math_time_diff", "ethnicity", "sessions","courses_enrolled","courses_passed","pass_rate","STRM")]
#make names match
f.23.chem.notutoring.enroll.pass <- setNames(f.23.chem.notutoring.enroll.pass, names(aggregate_chem)) 
f.23.chem.notutoring.enroll.pass$tutor = rep('no',nrow(f.23.chem.notutoring.enroll.pass))

#complete table
fall.23.SSC.chem = rbind(aggregate_chem,f.23.chem.notutoring.enroll.pass)
fall.23.SSC.chem = fall.23.SSC.chem %>% arrange(chem_stu_id)

#add cumulative gpa
gpa.chem = academic_career %>% filter(Student_ID %in% fall.23.SSC.chem$chem_stu_id) %>% filter(str_detect(STRM, '239')) %>% arrange(Student_ID) %>% select(CUM_GPA)
fall.23.SSC.chem$cum_gpa = gpa.chem

########################add persistence/retention
f23.reten.pers = retention_persistence %>% filter(str_detect(STRM, '239'))
index <- which(f23.reten.pers$Student_ID %in% fall.23.SSC.chem$chem_stu_id, arr.ind = TRUE)
f23.reten.pers.chem <- f23.reten.pers[index,]
f23.reten.pers.chem = f23.reten.pers.chem %>% arrange(Student_ID)
fall.23.SSC.chem$ret.f2f = f23.reten.pers.chem$f2f_num/f23.reten.pers.chem$f2f_denom
fall.23.SSC.chem$pers.f2s = f23.reten.pers.chem$f2s_num/f23.reten.pers.chem$f2s_denom

#categorize session count and time spent
session.count <- cut(fall.23.SSC.chem$sessions$n, breaks=c(0,3,4,14,37), labels=c('rare', 'occasionally', 'frequently', 'very.frequently')) 
fall.23.SSC.chem$session.count = session.count
time.spent <- cut(fall.23.SSC.chem$chem_time_diff, breaks=c(0, 60, 153, 386, 467,2800), labels=c('very.low', 'low', 'average', 'above.average','substantial'))
fall.23.SSC.chem$time.spent = time.spent

#remove students with NA of unknown ethnicity
fall.23.SSC.chem <-fall.23.SSC.chem %>% filter(!str_detect(ethnicity$ethnicity, "NA")) %>% filter(!str_detect(ethnicity$ethnicity, "Unknown"))
# combine alaskan native and native hawaiian
fall.23.SSC.chem$ethnicity[fall.23.SSC.chem$ethnicity == "American Indian or Alaska Native"] <- "Native American or Pacific Islander"
fall.23.SSC.chem$ethnicity[fall.23.SSC.chem$ethnicity == "Native Hawaiian or Other Pacific Islander"] <- "Native American or Pacific Islander"

###############################write table##########################################
embedded = read.csv("C:/Users/jbrin/Documents/chem/ET.chem.ids.csv",header=T)
fall.23.embedded = embedded %>% filter(term == 1239)

test2 = fall.23.SSC.chem
islst <- sapply(test2, is.list)
# ASSERT: number of rows and embedded lists/frames are the same length
all(nrow(test2) == sapply(test2[islst], lengths))
test2 <- do.call("cbind.data.frame", c(test2[!islst], test2[islst], stringsAsFactors = FALSE))
table(test2$tutor)
test2 = test2 %>% filter(!(chem_stu_id %in% fall.23.embedded$Student_ID))
table(test2$tutor)

write.csv(test2,"C:/Users/jbrin/Documents/fall.23.ssc.chem.csv",row.names = F)
