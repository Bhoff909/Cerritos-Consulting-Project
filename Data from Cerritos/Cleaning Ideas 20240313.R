#' ------------------------------------------------------------------------------ 
#' ---- Project: CSUF consulting project
#' ----- Sub Task: Identify Embedded courses
#' 
#' ---- Author: Eden Ellis 
#' 
#' ---- Goal: 
#' Identify embed courses in the enrollment file
#' Will allow to:
#' (1) obtain success rates in courses that student received tutoring
#' (2) create a comparison group of others in that course and no tutoring
#' 
#' ---- Methodology: 
#' 
#' ---- Notes:  
#' (1) embeded_tutor_info: taken from accutrak data - manually cleaned up
#' (2) et_schedule: taken from schedule excel files - manually cleaned up
#' 
#' This is a starting point to find the correct embedded class across all terms 
#' at once. You do not have to use this and please double check the work if you 
#' do :-P. I have already noted some TODOs where it may need adjusting.
#' 
#' It would be great if we can to do some comparisons to the F19 file you already 
#' matched and see how the outputs line up. It may be best incorporate some of 
#' your logic.
#' 
#' I think some of the files from the SSC are potentially inaccurate. I am seeing
#' students with taskdesc/subtasks that do not align with their enrollment.
#'  e.g. 1145850, GEOL 101 is not in enrollment
#' 
#' ---- To-Do
#' [] Get missing class_nbr in et_schedule
#' [] Get examples to share with SSC to check on possible errors in data
#'  [] schedule
#'  [] taskdesc/subtask
#' [] check on those where no match is found (embedded_tutor3_nomatch)
#' [] check on those where cannot find best match (embedded_tutor3_match_notclean)

# ------------------------------------------------------------------------------ 
library(dplyr)


# ---- Step 0: Set Up ----------------------------------------------------------
path <- 'R:/Special Projects/CSUF Consulting Course/Spring 2024/Data/'
setwd(path)


# ---- Step 1: Read in Data ----------------------------------------------------
load('De-Identified Data/Data from Cerritos.Rdata')


# ---- Step 2: Do the thing ----------------------------------------------------
# join on info, this will help break down the subtask
embedded_tutor2 <- embedded_tutor %>% 
  left_join(embedded_tutor_info, 
            by = c('STRM' = 'STRM - LOGIN', 
                   'Taskdesc',
                   'Subtask')) 

# join creates mult row 
# TODO check on this, maybe issues in embedded_tutor_info file


# --- Join Datasets (try align with corect enrollment) ----

# join schedule to enrollment to get a flag for if class is a known embedded section
# NOTE: since not all course identified accurately on schedule, this is not complete
enrollment2 <- enrollment %>% 
  mutate(STRM = as.character(STRM),
         CLASS_NBR = as.character(CLASS_NBR)) %>% 
  left_join(et_schedule %>% 
              mutate(embed = TRUE), 
            by = c('STRM', 
                   'CLASS_NBR')) %>% 
  tidyr::replace_na(list(embed = FALSE))


# join enrollment onto schedule data (id, term, subject - try others)
# will give possible classes that align with tutoring
  # NOTE: there will be duplication (see num_rec, these should be unique)
# set up some flags to help identify if this is the right course (same_*)


embedded_tutor3 <- embedded_tutor2 %>% 
  select(-c(Logintime, Logouttime)) %>%
  distinct() %>% 
  mutate(num_rec = row_number()) %>% # unique id for each accutrak entry (student x task/subtask)
  
  # join in enrollment data (enrolled classes only)
  left_join(enrollment2 %>% 
              filter(enrolled) %>% 
              transmute(Student_ID,
                        STRM = as.character(STRM), 
                        SUBJECT = tolower(SUBJECT),
                        CATALOG_NBR, 
                        CLASS_NBR,
                        CRSE_GRADE_OFF,
                        enrolled, success, completion, 
                        embed,
                        Course, 
                        name,
                        LAST_NAME_SRCH_PREF, LAST_NAME_SRCH_PRI), 
            
            # may need to adjust join criteria
            # now will join all classes in same subject identified by task/subtask
            by = c('Student_ID', 'STRM', 'SUBJECT'),
            suffix = c('_sched', '_enrl')) %>% 
  
  distinct() %>% # idk why there were dupes so im just removing
  
  # create flags to help with logic to get the correct class
  mutate(same_cat_nbr = tolower(trimws(CATALOG_NBR_sched)) == tolower(trimws(CATALOG_NBR_enrl)), 
         same_class_nbr = CLASS_NBR_sched == CLASS_NBR_enrl,
         same_inst = str_detect(string = tolower(paste0(LAST_NAME_SRCH_PREF, LAST_NAME_SRCH_PRI)), 
                                pattern = tolower(trimws(INSTR)))) %>% 
  group_by(num_rec) %>% 
  mutate(n_join = n()) %>% # how many rows created by join for each num_rec (if 1 possibly clean, if >1 need to find correct row)
  ungroup()

# embedded_tutor3 %>% filter(n == 1)
# embedded_tutor3 %>% filter(n > 1)

# ---- Step 3: Are the joined rows correct? ------------------------------------

# ---- no match ----
# these ones we can't find enrollment info
embedded_tutor3_nomatch <- embedded_tutor3 %>% 
  filter(is.na(CLASS_NBR_enrl))

#TODO next steps: do some spot cheking, make sure we can't find anything
# are these mistakes by the ssc?
# compile a list of examples to ask about.

# ---- matched ----
# will need to figure out which joined class record is correctish
# NOTE: even if n == 1, it may not be the correct class
# if n > 1, will need to determine if any are correct --> group by num_rec & create logic to flag correct course row
  #idea: embed has to be TRUE, and possibly some of the same_*
  # wary of just filter(embed) since if there wasnt a class_nbr in sched file it may not be flagged..........

embedded_tutor3_match <- embedded_tutor3  %>% 
  filter(!is.na(CLASS_NBR_enrl)) %>% 
  
  # how many of the test criteria are matched?
  mutate(tests = rowSums(pick(embed, same_cat_nbr, same_class_nbr, same_inst), na.rm = TRUE)) %>% 
  group_by(num_rec) %>% 
  mutate(test_max = tests == max(tests),
         test_max_sum = sum(test_max),
         which_test_max = ifelse(test_max_sum == 1, test_max, FALSE)) %>%  # which row has the most matched, assume that one is correct.
  ungroup() %>%
  
  mutate(clean_sorta = n_join == 1 | which_test_max) %>% # if only 1 match, or had most criteria matched
  
  group_by(num_rec) %>% 
  mutate(clean_sorta_n = sum(clean_sorta)) # how many now, are there still mult records matched?
  

embedded_tutor3_match <- embedded_tutor3  %>% 
  filter(!is.na(CLASS_NBR_enrl)) %>% 
  
  # how many of the test criteria are matched?
  mutate(tests = rowSums(pick(embed, same_cat_nbr, same_class_nbr, same_inst), na.rm = TRUE)) %>% 
  
  group_by(num_rec) %>% 
  mutate(test_max = tests == max(tests), #which row had max criteria matched
         test_max_sum = sum(test_max), # how many rows were identified as max (hopefully 1)
         which_test_max = ifelse(test_max_sum == 1, test_max, FALSE), # which row has the most matched, assume that one is correct. If mult, needs more work.
         clean_sorta = n_join == 1 | which_test_max, # if only 1 match based on join or identifed in row above as most criteria matched
         clean_sorta_n = sum(clean_sorta))   # how many identified now as correct row (hopefully 1)



embedded_tutor3_match_clean <- embedded_tutor3_match2 %>% 
  filter(clean_sorta_n == 1 & which_test_max) # can identify most likely row

embedded_tutor3_match_notclean <- embedded_tutor3_match2 %>% 
  filter(clean_sorta_n != 1) # cannot identify most likely row

# TODO - check on these - are there no enrollment that matches, are the mistakes?


