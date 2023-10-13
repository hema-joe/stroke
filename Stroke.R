#Josephat S. Hema's code-Analysis 
#Project Title:Post Stroke Management Quality of Care in Wales

#Necessary Libraries

#install.packages("RODBC")
library(RODBC)
#install.packages("tcltk")
library(tcltk)
#install.packages("tidyverse")
library(tidyverse)
#install.packages("psych")
library(psych)
#install.packages("survival")
library(survival)
#install.packages("survminer")
library(survminer)
#install.packages("mfp")
library(mfp)
#install.packages("lubridate")
library(lubridate)
#install.packages("rpart")
library(rpart)
#install.packages("caret")
library(caret)
#install.packages("skimr")
library(skimr)
#install.packages("gtsummary")
library(gtsummary)
#install.packages("consort")
library(consort)
#install.packages("janitor")
library(janitor)
#install.packages("flextable")
library(flextable)
#install.packages("ROCR")
library(ROCR)
#install.packages("pROC")
library(pROC)
#install.packages("stats")
library(stats)

#LOADING THE DATASET

stroke <- Josephat_MSc_Dataset

colnames(stroke)

#DATASET SUMMARY

skim(stroke)

#Display few rows
head(stroke)
tail(stroke)

#summary statistics of numerical variables
summary(stroke)

#Basic information of data sets
str(stroke)

ncol(stroke)
colnames(stroke)

nrow(stroke)
rownames(stroke)

#Missing/unconfirmed (NA) diagnosis values
is.na(stroke)
sum(is.na(stroke))


unique(stroke$STROKE_TYPE)
sum(is.na(stroke$STROKE_TYPE))

#NA=UNSPEC
stroke$STROKE_TYPE <- ifelse(is.na(stroke$STROKE_TYPE), "UNSPEC",
                             stroke$STROKE_TYPE)

unique(stroke$STROKE_TYPE)

colnames(stroke)

#

stroke <- stroke %>% mutate (across(c(DEMENTIA_BEFORE,
                                      RESP_BEFORE ,LIVER_BEFORE,CKD_BEFORE,
                                      HYPERTENSION_BEFORE,AF_BEFORE,
                                      LLT_HIGH_STATIN,LLT_LOW_STATIN,
                                      LLT_INEGY,LLT_EZETIMIBE, 
                                      LLT_FIBRATE,P2Y12_ANTIPLATELET,
                                      H2ANTAG, PPI,GLYCOSIDES,
                                      THIAZIDE_DIURETICS,LOOP_DIURETICS,
                                      POTSPARING_DIURETICS, 
                                      POT_SPARING_COMPOUND_DIURTETICS,
                                      ALDOSTERONE_ANTAG,VERAPMAIL_DILTIAZEM,
                                      BETABLOCKER,VASODILATOR_ANTIHYPERTENSIVES,
                                      CENTRAL_ANTIHYPERTENSIVES,
                                      ALFA_BLOCKERS,ACEI_ARB_ENTRESTO,
                                      DIHYDROPYRIDINE_CALCIUM_CHANNEL,
                                      PARENTERAL_ANTICOAG,VIT_K_ANTICOAG ,
                                      DOAC,ASPIRIN,FISH,OTHER_LIPID,INSULIN,
                                      ORAL_ANTIDIABETICS,NSAID,
                                      STUDY_30D_FOLLOW_UP,STUDY_60D_FOLLOW_UP,
                                      STUDY_90D_FOLLOW_UP,STUDY_120D_FOLLOW_UP,
                                       STUDY_365D_FOLLOW_UP),
                                      ~ifelse (is.na(.), 0, .)))


sum(is.na(stroke))

#

R <- c("DEMENTIA_BEFORE",
       "RESP_BEFORE" ,"LIVER_BEFORE","CKD_BEFORE",
       "HYPERTENSION_BEFORE","AF_BEFORE",
       "LLT_HIGH_STATIN","LLT_LOW_STATIN",
       "LLT_INEGY","LLT_EZETIMIBE", 
       "LLT_FIBRATE","P2Y12_ANTIPLATELET",
       "H2ANTAG", "PPI","GLYCOSIDES",
       "THIAZIDE_DIURETICS","LOOP_DIURETICS",
       "POTSPARING_DIURETICS", 
       "POT_SPARING_COMPOUND_DIURTETICS",
       "ALDOSTERONE_ANTAG","VERAPMAIL_DILTIAZEM",
       "BETABLOCKER","VASODILATOR_ANTIHYPERTENSIVES",
       "CENTRAL_ANTIHYPERTENSIVES",
       "ALFA_BLOCKERS","ACEI_ARB_ENTRESTO",
       "DIHYDROPYRIDINE_CALCIUM_CHANNEL",
       "PARENTERAL_ANTICOAG","VIT_K_ANTICOAG" ,
       "DOAC","ASPIRIN","FISH","OTHER_LIPID","INSULIN",
       "ORAL_ANTIDIABETICS","NSAID",
       "STUDY_30D_FOLLOW_UP","STUDY_60D_FOLLOW_UP",
       "STUDY_90D_FOLLOW_UP","STUDY_120D_FOLLOW_UP",
       "STUDY_365D_FOLLOW_UP")

sum(is.na(stroke))

stroke[R][is.na(stroke[R])] <-0

sum(is.na(stroke))


#Which columns have NAs?
missing_columns <- colSums(is.na(stroke)) >0

NAsColumns <- names(stroke[missing_columns])


NAsColumns #Columns with NAs

#

unique(stroke$WIMD_2019_QUINTILE)

stroke$WIMD_2019_QUINTILE <- ifelse(is.na(stroke$WIMD_2019_QUINTILE)
, "Unknown",stroke$WIMD_2019_QUINTILE) 

unique(stroke$WIMD_2019_QUINTILE)
#

#Exclusion/Inclusion Criteria

stroke_1 <- stroke %>% filter (AGE_DIAG >= 18, STUDY_365D_FOLLOW_UP ==1 )

#CONSORT DIAGRAM

stroke_consort <- stroke %>%
  mutate(Excluded = case_when(AGE_DIAG < 18 ~ "Less than 18 Years old",
                    STUDY_365D_FOLLOW_UP !=1 ~ "Less than 365 Days Follow Up"))

stroke_consort %>% count(AGE_DIAG)
unique(stroke_consort$Excluded)

stroke_consort %>% count(Excluded)

orders = c( ALF_PE = "Started with "
            ,Excluded = "Excluded records"
            ,ALF_PE = "Finally left with"
            )

side_box = c("Excluded")

consort_plot (stroke_consort
              , orders = orders
              ,side_box = side_box)


#GENERAL SUMMARY

stroke_1 <- stroke %>% filter (AGE_DIAG >= 18, STUDY_365D_FOLLOW_UP ==1 )

stroke_1$PRE_POST_COVID <- factor(stroke_1$PRE_POST_COVID,
                                  levels = c("PRE_COVID", "POST_COVID"))

#RISK FACTORS
stroke_risk_factors <- stroke_1 %>% select(ALF_PE,
                                           DEMENTIA_BEFORE,
                                           SMOKING_STATUS,
                                           RESP_BEFORE ,
                                           LIVER_BEFORE,
                                           CKD_BEFORE,
                                           HYPERTENSION_BEFORE,
                                           AF_BEFORE)

summary (stroke_risk_factors)

stroke_1 <- stroke_1 %>%
  mutate(SMOKING_STATUS = case_when(SMOKING_STATUS == "E" ~ "Ex-Smoker",
                                      SMOKING_STATUS == "N" ~ "Non-Smoker",
                                      SMOKING_STATUS == "S" ~ "Smoker",
                                      SMOKING_STATUS == "U" ~ "Unknown Smoker"))

stroke_1 <- stroke_1 %>%
mutate(WIMD_2019_QUINTILE = case_when(WIMD_2019_QUINTILE == 1 ~ "Most Deprived",
                                    WIMD_2019_QUINTILE == 2 ~ "Intermediate",
                                    WIMD_2019_QUINTILE == 3 ~ "Intermediate",
                                    WIMD_2019_QUINTILE == 4 ~ "Intermediate",
                                    WIMD_2019_QUINTILE == 5 ~ "Least Deprived"))

risk_factors <- stroke_1 %>% select(PRE_POST_COVID,AGE_DIAG,
                                    AF_BEFORE, DEMENTIA_BEFORE,
                                    DIABETES_BEFORE,
                                    RESP_BEFORE ,LIVER_BEFORE,
                                    CKD_BEFORE, HYPERTENSION_BEFORE,
                                    SMOKING_STATUS, WIMD_2019_QUINTILE)

mean(stroke_1$AGE_DIAG) #Mean age is found to be 68.62567
summary(stroke_1$AGE_DIAG)

risk_factors$AGE_DIAG <- ifelse((risk_factors$AGE_DIAG >= 70), "70 and above" ,
                                "Below 70")

#risk_factors_table <- risk_factors %>% filter(AF_BEFORE==1|DEMENTIA_BEFORE==1|
                                             #DIABETES_BEFORE==1|RESP_BEFORE==1|
                                                 #LIVER_BEFORE==1|CKD_BEFORE==1|
                                      #HYPERTENSION_BEFORE==1|SMOKING_STATUS==1|
                                             #AGE_DIAG==1|WIMD_2019_QUINTILE==1)

risk_factors_table <-tbl_summary(risk_factors, by = PRE_POST_COVID) %>% add_p()

risk_factors_table       #TABLE NUMBER ONE

#COLUMN/VARIABLE GROUPS PRESENTS

#GENERAL CHARACTERISTICS
stroke_general <- stroke_1 %>% select(ALF_PE, AGE_DIAG,
                                        WIMD_2019_QUINTILE,
                                        STROKE_TYPE, GP_ADMIS_FIRST,
                                      PEDW_ADMIS_FIRST, EVENT_DT, PRE_POST_COVID,
                                      DEATH_DT, CENSOR_DT,
                                      CENSOR_REASON)

summary(stroke_general)

#MANAGEMENT
stroke_management <- stroke_1 %>% select(ALF_PE,LLT_HIGH_STATIN,LLT_LOW_STATIN,
                                    LLT_INEGY,LLT_EZETIMIBE, 
                                    LLT_FIBRATE,P2Y12_ANTIPLATELET,
                                    H2ANTAG, PPI,GLYCOSIDES,
                                    THIAZIDE_DIURETICS,LOOP_DIURETICS,
                                    POTSPARING_DIURETICS, 
                                    POT_SPARING_COMPOUND_DIURTETICS,
                                    ALDOSTERONE_ANTAG,VERAPMAIL_DILTIAZEM,
                                    BETABLOCKER,VASODILATOR_ANTIHYPERTENSIVES,
                                    CENTRAL_ANTIHYPERTENSIVES,
                                    ALFA_BLOCKERS,ACEI_ARB_ENTRESTO,
                                    DIHYDROPYRIDINE_CALCIUM_CHANNEL,
                                    PARENTERAL_ANTICOAG,VIT_K_ANTICOAG ,
                                    DOAC,ASPIRIN,FISH ,
                                    OTHER_LIPID,INSULIN,ORAL_ANTIDIABETICS,
                                    NSAID)

summary(stroke_management)

#FOLLOW UP
stroke_follow_up <- stroke_1 %>% select(ALF_PE, STUDY_30D_FOLLOW_UP,
                                    STUDY_60D_FOLLOW_UP, STUDY_90D_FOLLOW_UP,
                                    STUDY_120D_FOLLOW_UP, STUDY_365D_FOLLOW_UP)


summary(stroke_follow_up)

#TESTS

stroke_tests <- stroke_1 %>% select(ALF_PE, SYS_60, DYS_60, SYS_120,
                                      DYS_120, SYS_180, DYS_180, SYS_365,
                                      DYS_365,
                                    LDL_60,  NON_HDL_60 , TG_60, LDL_120,
                                    NON_HDL_120, TG_120, LDL_180,
                                    NON_HDL_180, TG_180, LDL_365,
                                    NON_HDL_365, TG_365)

summary(stroke_tests)

#COHORTS CHARACTERISTICS

#PRE & COVID COHORTS
unique(stroke_1$PRE_POST_COVID)
stroke_1 %>% count(PRE_POST_COVID)

#PRE COVID
precovid_stroke <- stroke_1 %>% filter(PRE_POST_COVID == "PRE_COVID")
precovid_stroke <- precovid_stroke %>% filter(STUDY_365D_FOLLOW_UP == 1)

pre <- precovid_stroke %>% select(STROKE_TYPE)
pre <- precovid_stroke %>% count(STROKE_TYPE)

pre

isch_precovid_stroke <- precovid_stroke %>% filter(STROKE_TYPE == "ISCH")
haem_precovid_stroke <- precovid_stroke %>% filter(STROKE_TYPE == "HAEM")
unspec_precovid_stroke <- precovid_stroke %>% filter(STROKE_TYPE == "UNSPEC")


barplot(pre$n, names.arg = pre$STROKE_TYPE, main = "Pre Covid Stroke Patients",
        ylab = "Number of Patients",xlab = "Types of Stroke")


#COVID
postcovid_stroke <- stroke_1 %>% filter(PRE_POST_COVID == "POST_COVID")
postcovid_stroke <- postcovid_stroke %>% filter(STUDY_365D_FOLLOW_UP == 1) 

co <- postcovid_stroke %>% select(STROKE_TYPE)
co <- postcovid_stroke %>% count(STROKE_TYPE)

co

isch_postcovid_stroke <- postcovid_stroke %>% filter(STROKE_TYPE == "ISCH")
haem_postcovid_stroke <- postcovid_stroke %>% filter(STROKE_TYPE == "HAEM")
unspec_postcovid_stroke <- postcovid_stroke %>% filter(STROKE_TYPE == "UNSPEC")

barplot(co$n, names.arg = co$STROKE_TYPE, main = "Post-Covid Stroke Patients",
        ylab = "Number of Patients",xlab = "Types of Stroke")

#Visualize both barplots

par(mfrow = c(1,2))
barplot(pre$n, names.arg = pre$STROKE_TYPE, col = "blue",
        main = "Stroke Patients Before Covid-19",
        ylab = "Number of Patients",xlab = "Types of Stroke")

barplot(co$n, names.arg = co$STROKE_TYPE, col = "red",
        main = "Stroke Patients during Covid-19",
        ylab = "Number of Patients",xlab = "Types of Stroke")
par(mfrow = c(1,1)) #PLOT NUMBER TWO


#Table Summary (Including patients with Atrial Fibrillation before Stroke diagnosis)

stroke_1$PRE_POST_COVID <- factor(stroke_1$PRE_POST_COVID,
                                  levels = c("PRE_COVID", "POST_COVID"))

#Pre
preaf <- precovid_stroke %>% select(STROKE_TYPE, AF_BEFORE)

preaf_table <-tbl_summary(preaf, by = STROKE_TYPE) %>% add_p()

preaf_table 

#Covid
postaf <- postcovid_stroke %>% select(STROKE_TYPE, AF_BEFORE)

postaf_table <-tbl_summary(postaf, by = STROKE_TYPE) %>% add_p()

postaf_table


#Table Merge
af_summary_table <- tbl_merge(tbls = list(preaf_table, postaf_table),
                              tab_spanner = c("Pre-Covid", "Covid"))

af_summary_table #TABLE NUMBER THREE


#Total Stroke types with AF
aft <- stroke_1 %>% select(PRE_POST_COVID, AF_BEFORE)

aft_table <-tbl_summary(aft, by = PRE_POST_COVID) %>% add_p()

aft_table #PLOT NUMBER FOUR


#GENERAL CHARACTERISTICS

prestroke_general <- precovid_stroke %>% select(ALF_PE, AGE_DIAG,
                                                WIMD_2019_QUINTILE, STROKE_TYPE,
                                                GP_ADMIS_FIRST,
                                                PEDW_ADMIS_FIRST, EVENT_DT,
                                                PRE_POST_COVID, DEATH_DT,
                                                CENSOR_DT,
                                                CENSOR_REASON)

poststroke_general <- postcovid_stroke %>% select(ALF_PE, AGE_DIAG,
                                                WIMD_2019_QUINTILE, STROKE_TYPE,
                                                GP_ADMIS_FIRST,
                                                PEDW_ADMIS_FIRST, EVENT_DT,
                                                PRE_POST_COVID, DEATH_DT,
                                                CENSOR_DT,
                                                CENSOR_REASON)                                                                                                  "CENSOR_REASON"))

#WIMD
prewimd <- precovid_stroke %>% tbl_cross(row = STROKE_TYPE,
                                         col = WIMD_2019_QUINTILE)
prewimd

postwimd <- postcovid_stroke %>% tbl_cross(row = STROKE_TYPE,
                                           col = WIMD_2019_QUINTILE)
postwimd

#Tables merge
wimd_table <- tbl_merge(tbls = list(prewimd, postwimd),
                        tab_spanner = c("Pre-Covid", "Covid"))
wimd_table

#FIRST GP ADMISSION
pregp <- precovid_stroke %>% tbl_cross(row = STROKE_TYPE, col = GP_ADMIS_FIRST)

pregp

postgp <- postcovid_stroke %>% tbl_cross(row = STROKE_TYPE, col = GP_ADMIS_FIRST)

postgp

#Tables merge
gp_table <- tbl_merge(tbls = list(pregp, postgp), tab_spanner = c("Pre-Covid",
                                                                  "Covid"))

gp_table

#AGE
#Pre
precovid_stroke_temp <- precovid_stroke
precovid_stroke_temp$AGE_DIAG <- ifelse((precovid_stroke_temp$AGE_DIAG >= 70),
                                        "70 and above", "Below 70")
preage <- precovid_stroke_temp %>% tbl_cross(row = STROKE_TYPE, col = AGE_DIAG)

preage

#Co

postcovid_stroke_temp <- postcovid_stroke
postcovid_stroke_temp$AGE_DIAG <- ifelse((postcovid_stroke_temp$AGE_DIAG >= 70),
                                         "70 and above", "Below 70")
postage <- postcovid_stroke_temp %>% tbl_cross(row = STROKE_TYPE, col = AGE_DIAG)

postage

#Table Merge
age_table <- tbl_merge(tbls = list(preage, postage),
                       tab_spanner = c("Pre-Covid", "Covid"))

age_table

#RISK FACTORS

pre_stroke_risk_factors <- precovid_stroke %>%
                              select(c("ALF_PE","DEMENTIA_BEFORE",
                              "SMOKING_STATUS", "DIABETES_BEFORE",
                              "RESP_BEFORE" ,"LIVER_BEFORE",
                              "CKD_BEFORE", "HYPERTENSION_BEFORE","AF_BEFORE" ))
co_stroke_risk_factors <- postcovid_stroke %>%
                                        select(c("ALF_PE","DEMENTIA_BEFORE",
                                        "SMOKING_STATUS", "DIABETES_BEFORE",
                                        "RESP_BEFORE" ,"LIVER_BEFORE",
                                        "CKD_BEFORE", "HYPERTENSION_BEFORE",
                                        "AF_BEFORE" ))
#SUMMARY TABLES OF RISK FACTORS 
#Pre Stroke

precovid_stroke %>% tbl_cross(row = STROKE_TYPE, col = CKD_BEFORE)
precovid_stroke %>% tbl_cross(row = STROKE_TYPE, col = AF_BEFORE) 
precovid_stroke %>% tbl_cross(row = STROKE_TYPE, col = DIABETES_BEFORE) 
precovid_stroke %>% tbl_cross(row = STROKE_TYPE, col = LIVER_BEFORE) 
precovid_stroke %>% tbl_cross(row = STROKE_TYPE, col = RESP_BEFORE) 
precovid_stroke %>% tbl_cross(row = STROKE_TYPE, col = HYPERTENSION_BEFORE) 
precovid_stroke %>% tbl_cross(row = STROKE_TYPE, col = DEMENTIA_BEFORE) 
precovid_stroke %>% tbl_cross(row = STROKE_TYPE, col = SMOKING_STATUS) 

#Covid
postcovid_stroke %>% tbl_cross(row = STROKE_TYPE, col = CKD_BEFORE) 
postcovid_stroke %>% tbl_cross(row = STROKE_TYPE, col = AF_BEFORE) 
postcovid_stroke %>% tbl_cross(row = STROKE_TYPE, col = DIABETES_BEFORE) 
postcovid_stroke %>% tbl_cross(row = STROKE_TYPE, col = LIVER_BEFORE) 
postcovid_stroke %>% tbl_cross(row = STROKE_TYPE, col = RESP_BEFORE) 
postcovid_stroke %>% tbl_cross(row = STROKE_TYPE, col = HYPERTENSION_BEFORE) 
postcovid_stroke %>% tbl_cross(row = STROKE_TYPE, col = DEMENTIA_BEFORE) 
postcovid_stroke %>% tbl_cross(row = STROKE_TYPE, col = SMOKING_STATUS) 

#Tables Merging
#WIMD
wimd1 <- precovid_stroke %>% tbl_cross(row = STROKE_TYPE,
                                       col = WIMD_2019_QUINTILE)
wimd2 <- postcovid_stroke %>% tbl_cross(row = STROKE_TYPE,
                                        col = WIMD_2019_QUINTILE)

wimd_table <- tbl_merge(tbls = list(wimd1, wimd2), tab_spanner = c("Pre-Covid",
                                                                   "Covid"))

wimd_table


#CKD
ckd1 <- precovid_stroke %>% tbl_cross(row = STROKE_TYPE, col = CKD_BEFORE)
ckd2 <- postcovid_stroke %>% tbl_cross(row = STROKE_TYPE, col = CKD_BEFORE)

ckd_table <- tbl_merge(tbls = list(ckd1, ckd2), tab_spanner = c("Pre-Covid",
                                                                "Covid"))

ckd_table

#AF
af1 <- precovid_stroke %>% tbl_cross(row = STROKE_TYPE, col = AF_BEFORE)
af2 <- postcovid_stroke %>% tbl_cross(row = STROKE_TYPE, col = AF_BEFORE) 

af_table <- tbl_merge(tbls = list(af1, af2), tab_spanner = c("Pre-Covid",
                                                             "Covid"))

af_table


#DIABETES
diab1 <- precovid_stroke %>% tbl_cross(row = STROKE_TYPE, col = DIABETES_BEFORE)
diab2 <- postcovid_stroke %>% tbl_cross(row = STROKE_TYPE, col = DIABETES_BEFORE) 

diab_table <- tbl_merge(tbls = list(diab1, diab2), tab_spanner = c("Pre-Covid",
                                                                   "Covid"))

diab_table

#LIVER
liver1 <- precovid_stroke %>% tbl_cross(row = STROKE_TYPE, col = LIVER_BEFORE) 
liver2 <- postcovid_stroke %>% tbl_cross(row = STROKE_TYPE, col = LIVER_BEFORE) 

liver_table <- tbl_merge(tbls = list(liver1, liver2),
                         tab_spanner = c("Pre-Covid", "Covid"))

liver_table

#RESP
resp1 <- precovid_stroke %>% tbl_cross(row = STROKE_TYPE, col = RESP_BEFORE) 
resp2 <- postcovid_stroke %>% tbl_cross(row = STROKE_TYPE, col = RESP_BEFORE) 

resp_table <- tbl_merge(tbls = list(resp1, resp2), tab_spanner = c("Pre-Covid",
                                                                   "Covid"))

#HTN
htn1 <- precovid_stroke %>% tbl_cross(row = STROKE_TYPE, col = HYPERTENSION_BEFORE) 
htn2 <- postcovid_stroke %>% tbl_cross(row = STROKE_TYPE, col = HYPERTENSION_BEFORE) 

htn_table <- tbl_merge(tbls = list(htn1, htn2), tab_spanner = c("Pre-Covid",
                                                                "Covid"))

htn_table

#DEMENTIA
dem1 <- precovid_stroke %>% tbl_cross(row = STROKE_TYPE, col = DEMENTIA_BEFORE) 
dem2 <- postcovid_stroke %>% tbl_cross(row = STROKE_TYPE, col = DEMENTIA_BEFORE) 

dem_table <- tbl_merge(tbls = list(dem1, dem2), tab_spanner = c("Pre-Covid",
                                                                "Covid"))

dem_table

#SMOKING
smoke1 <- precovid_stroke %>% tbl_cross(row = STROKE_TYPE, col = SMOKING_STATUS) 
smoke2 <- postcovid_stroke %>% tbl_cross(row = STROKE_TYPE, col = SMOKING_STATUS) 

smoke_table <- tbl_merge(tbls = list(smoke1, smoke2),
                         tab_spanner = c("Pre-Covid", "Covid"))

smoke_table

#A COMBINED TABLE OF ALL RISK FACTORS
#Pre
precovid_stroke <- precovid_stroke %>%
        mutate(SMOKING_STATUS = case_when(SMOKING_STATUS == "E" ~ "Ex-Smoker",
                                      SMOKING_STATUS == "N" ~ "Non-Smoker",
                                      SMOKING_STATUS == "S" ~ "Smoker",
                                      SMOKING_STATUS == "U" ~ "Unknown Smoker"))

all1 <- precovid_stroke %>% select(STROKE_TYPE,AF_BEFORE, DEMENTIA_BEFORE,
                                   DIABETES_BEFORE,
                                   RESP_BEFORE ,LIVER_BEFORE,
                                   CKD_BEFORE, HYPERTENSION_BEFORE,
                                   SMOKING_STATUS)

#all_table1 <- all1 %>% filter(AF_BEFORE==1|DEMENTIA_BEFORE==1
                              #|DIABETES_BEFORE==1|RESP_BEFORE==1
                              #|LIVER_BEFORE==1|CKD_BEFORE==1|
                              #|HYPERTENSION_BEFORE==1|SMOKING_STATUS==1)

all_summary1 <-tbl_summary(all1, by = STROKE_TYPE)

all_summary1 

#Covid
postcovid_stroke <- postcovid_stroke %>%
  mutate(SMOKING_STATUS = case_when(SMOKING_STATUS == "E" ~ "Ex-Smoker",
                                      SMOKING_STATUS == "N" ~ "Non-Smoker",
                                      SMOKING_STATUS == "S" ~ "Smoker",
                                      SMOKING_STATUS == "U" ~ "Unknown Smoker"))
all2 <- postcovid_stroke %>% select(STROKE_TYPE,AF_BEFORE, DEMENTIA_BEFORE,
                              DIABETES_BEFORE,
                              RESP_BEFORE ,LIVER_BEFORE,
                              CKD_BEFORE, HYPERTENSION_BEFORE, SMOKING_STATUS)

#all_table2 <- all2 %>% filter(AF_BEFORE==1|DEMENTIA_BEFORE==1|
                              #DIABETES_BEFORE==1|RESP_BEFORE==1
                              #|LIVER_BEFORE==1|CKD_BEFORE==1|
                              #|HYPERTENSION_BEFORE==1|SMOKING_STATUS==1)

all_summary2 <-tbl_summary(all2, by = STROKE_TYPE)

all_summary2

#Table Merge
all_table <- tbl_merge(tbls = list(all_summary1, all_summary2),
                       tab_spanner = c("Pre-Covid", "Covid"))

all_table


#PHARMACEUTICAL MANAGEMENT

pre_stroke_management <- precovid_stroke %>%
  select(c("ALF_PE", "LLT_HIGH_STATIN",
                          "LLT_LOW_STATIN", "LLT_INEGY",
                          "LLT_EZETIMIBE", "LLT_FIBRATE",
                          "P2Y12_ANTIPLATELET", "H2ANTAG",
                          "PPI", "GLYCOSIDES", "THIAZIDE_DIURETICS",
                          "LOOP_DIURETICS", "POTSPARING_DIURETICS", 
                          "POT_SPARING_COMPOUND_DIURTETICS",
                          "ALDOSTERONE_ANTAG", "VERAPMAIL_DILTIAZEM",
                          "BETABLOCKER", "VASODILATOR_ANTIHYPERTENSIVES",
                          "CENTRAL_ANTIHYPERTENSIVES","ALFA_BLOCKERS",
                         "ACEI_ARB_ENTRESTO", "DIHYDROPYRIDINE_CALCIUM_CHANNEL",
                          "PARENTERAL_ANTICOAG", "VIT_K_ANTICOAG" ,
                          "VIT_K_ANTICOAG", "ASPIRIN" , "FISH" ,"OTHER_LIPID" ,
                          "INSULIN", "ORAL_ANTIDIABETICS" , "NSAID"))

post_stroke_management <- precovid_stroke %>%
                        select(c("ALF_PE", "LLT_HIGH_STATIN",
                        "LLT_LOW_STATIN", "LLT_INEGY",
                        "LLT_EZETIMIBE", "LLT_FIBRATE",
                        "P2Y12_ANTIPLATELET", "H2ANTAG",
                        "PPI", "GLYCOSIDES", "THIAZIDE_DIURETICS",
                        "LOOP_DIURETICS", "POTSPARING_DIURETICS", 
                        "POT_SPARING_COMPOUND_DIURTETICS",
                        "ALDOSTERONE_ANTAG", "VERAPMAIL_DILTIAZEM",
                        "BETABLOCKER", "VASODILATOR_ANTIHYPERTENSIVES",
                        "CENTRAL_ANTIHYPERTENSIVES","ALFA_BLOCKERS",
                        "ACEI_ARB_ENTRESTO", "DIHYDROPYRIDINE_CALCIUM_CHANNEL",
                        "PARENTERAL_ANTICOAG", "VIT_K_ANTICOAG" ,
                        "DOAC", "ASPIRIN" , "FISH" ,"OTHER_LIPID" ,
                        "INSULIN", "ORAL_ANTIDIABETICS" , "NSAID"))


#Anticoagulation/Antithrombotic


stroke_blood <- stroke_1 %>% select(PRE_POST_COVID, P2Y12_ANTIPLATELET, DOAC, 
                                    PARENTERAL_ANTICOAG, VIT_K_ANTICOAG, ASPIRIN)


#stroke_blood_table <- stroke_blood %>% filter(P2Y12_ANTIPLATELET==1|DOAC ==1 |
                                                #PARENTERAL_ANTICOAG==1|
                                                #VIT_K_ANTICOAG==1| ASPIRIN==1)

stroke_blood_summary <-tbl_summary(stroke_blood, by = PRE_POST_COVID) %>% add_p()

stroke_blood_summary  #TABLE NUMBER FOUR

#Pre and Post tables
#Pre
blood1 <- precovid_stroke %>% select(STROKE_TYPE, P2Y12_ANTIPLATELET, DOAC, 
                                    PARENTERAL_ANTICOAG, VIT_K_ANTICOAG, ASPIRIN)

#blood_table1 <- blood1 %>% filter(P2Y12_ANTIPLATELET==1|DOAC ==1 |
                         #PARENTERAL_ANTICOAG==1| VIT_K_ANTICOAG==1| ASPIRIN==1)

blood_summary1 <-tbl_summary(blood1, by = STROKE_TYPE) %>% add_p()

blood_summary1 
  
#Covid
blood2 <- postcovid_stroke %>% select(STROKE_TYPE, P2Y12_ANTIPLATELET, DOAC,
                                  PARENTERAL_ANTICOAG, VIT_K_ANTICOAG, ASPIRIN)

#blood_table2 <- blood2 %>% filter(P2Y12_ANTIPLATELET==1|DOAC ==1 |
                        #PARENTERAL_ANTICOAG==1| VIT_K_ANTICOAG==1| ASPIRIN==1)

blood_summary2 <-tbl_summary(blood2, by = STROKE_TYPE) %>% add_p()

blood_summary2

#Table Merge
blood_table <- tbl_merge(tbls = list(blood_summary1, blood_summary2),
                         tab_spanner = c("Pre-Covid", "Covid"))

blood_table


#With AF
strokeAF <- stroke_1 %>% filter(AF_BEFORE == 1)

#Pre
bloodaf <- strokeAF %>% select(PRE_POST_COVID,PARENTERAL_ANTICOAG, DOAC,
                               VIT_K_ANTICOAG)

#blood_tableaf <- bloodaf %>% filter(PARENTERAL_ANTICOAG==1|DOAC ==1|
VIT_K_ANTICOAG==1)

blood_summaryaf <-tbl_summary(bloodaf, by = PRE_POST_COVID) %>% add_p()

blood_summaryaf #TABLE NUMBER FIVE


precovid_strokeaf <- stroke_1 %>% filter(AF_BEFORE == 1)
postcovid_strokeaf <- stroke_1 %>% filter(AF_BEFORE == 1)

#Pre
bloodaf1 <- precovid_strokeaf %>% select(PRE_POST_COVID,PARENTERAL_ANTICOAG,
                                         DOAC, VIT_K_ANTICOAG)

#blood_tableaf1 <- bloodaf1 %>% filter(PARENTERAL_ANTICOAG==1|DOAC ==1|
VIT_K_ANTICOAG==1)

blood_summaryaf1 <-tbl_summary(bloodaf1, by = PRE_POST_COVID) %>% add_p()

blood_summaryaf1 

#Covid
bloodaf2 <- postcovid_strokeaf %>% select(PRE_POST_COVID,PARENTERAL_ANTICOAG,
                                          DOAC, VIT_K_ANTICOAG)

#blood_tableaf2 <- bloodaf2 %>% filter(PARENTERAL_ANTICOAG==1|DOAC ==1|
VIT_K_ANTICOAG==1)

blood_summaryaf2 <-tbl_summary(bloodaf2, by = PRE_POST_COVID) %>% add_p()

blood_summaryaf2

#Table Merge
#blood_tableaf <- tbl_merge(tbls = list(blood_summaryaf1, blood_summaryaf2),
tab_spanner = c("Pre-Covid", "Covid"))

#blood_tableaf



#Hypertension
#General table



stroke_htn <- stroke_1 %>% select(PRE_POST_COVID, THIAZIDE_DIURETICS,
                                  LOOP_DIURETICS,
                                  ALDOSTERONE_ANTAG,
                                  VERAPMAIL_DILTIAZEM, BETABLOCKER,
                                  CENTRAL_ANTIHYPERTENSIVES, ALFA_BLOCKERS,
                                  ACEI_ARB_ENTRESTO,
                                  DIHYDROPYRIDINE_CALCIUM_CHANNEL)


#stroke_htn_table <- stroke_htn %>% filter(THIAZIDE_DIURETICS==1|
                                        #LOOP_DIURETICS==1|
                                        # ALDOSTERONE_ANTAG==1|
                                        #VERAPMAIL_DILTIAZEM==1|
                                        #BETABLOCKER==1
                                        #CENTRAL_ANTIHYPERTENSIVES==1|
                                        #ALFA_BLOCKERS==1| ACEI_ARB_ENTRESTO==1|
                                        #DIHYDROPYRIDINE_CALCIUM_CHANNEL==1)

stroke_htn_summary <- tbl_summary(stroke_htn, by = PRE_POST_COVID) %>% add_p()

stroke_htn_summary #TABLE NUMBER SIX

#fewer meds
#stroke_htn2 <- stroke_1 %>% select(PRE_POST_COVID, THIAZIDE_DIURETICS,
                                  #LOOP_DIURETICS,
                                  #ALDOSTERONE_ANTAG,
                                  # VERAPMAIL_DILTIAZEM, BETABLOCKER,
                                  #CENTRAL_ANTIHYPERTENSIVES, ALFA_BLOCKERS,
                                  #ACEI_ARB_ENTRESTO,
                                  # DIHYDROPYRIDINE_CALCIUM_CHANNEL)


#strokehtn_table <- stroke_htn %>%
 # filter(THIAZIDE_DIURETICS==1|
        #LOOP_DIURETICS==1|
        #ALDOSTERONE_ANTAG==1|VERAPMAIL_DILTIAZEM==1| BETABLOCKER==1|
        #CENTRAL_ANTIHYPERTENSIVES==1| ALFA_BLOCKERS==1| ACEI_ARB_ENTRESTO==1|
        #DIHYDROPYRIDINE_CALCIUM_CHANNEL==1)

#strokehtn_summary <- tbl_summary(strokehtn_table, by = PRE_POST_COVID)

#strokehtn_summary


#Pre and Post tables
#Pre
h_table1 <- precovid_stroke %>% select(STROKE_TYPE,THIAZIDE_DIURETICS,
                                       LOOP_DIURETICS, POTSPARING_DIURETICS,
                                       POT_SPARING_COMPOUND_DIURTETICS,
                                       ALDOSTERONE_ANTAG,
                                       VERAPMAIL_DILTIAZEM, BETABLOCKER,
                                       VASODILATOR_ANTIHYPERTENSIVES,
                                       CENTRAL_ANTIHYPERTENSIVES, ALFA_BLOCKERS,
                                       ACEI_ARB_ENTRESTO,
                                       DIHYDROPYRIDINE_CALCIUM_CHANNEL)

h1 <- h_table1 %>% filter(THIAZIDE_DIURETICS==1|
                                LOOP_DIURETICS==1| POTSPARING_DIURETICS==1|
                                POT_SPARING_COMPOUND_DIURTETICS==1|
                                ALDOSTERONE_ANTAG==1|
                                VERAPMAIL_DILTIAZEM==1| BETABLOCKER==1|
                                VASODILATOR_ANTIHYPERTENSIVES==1|
                                CENTRAL_ANTIHYPERTENSIVES==1| ALFA_BLOCKERS==1|
                                ACEI_ARB_ENTRESTO==1|
                                DIHYDROPYRIDINE_CALCIUM_CHANNEL==1)

h1_summary <- tbl_summary(h1, by = STROKE_TYPE)

h1_summary

#Covid
h_table2 <- postcovid_stroke %>% select(STROKE_TYPE, THIAZIDE_DIURETICS,
                                  LOOP_DIURETICS, POTSPARING_DIURETICS,
                                  POT_SPARING_COMPOUND_DIURTETICS,
                                  ALDOSTERONE_ANTAG,
                                  VERAPMAIL_DILTIAZEM, BETABLOCKER,
                                  VASODILATOR_ANTIHYPERTENSIVES,
                                  CENTRAL_ANTIHYPERTENSIVES, ALFA_BLOCKERS,
                                  ACEI_ARB_ENTRESTO,
                                  DIHYDROPYRIDINE_CALCIUM_CHANNEL)

h2 <- h_table2 %>% filter(THIAZIDE_DIURETICS==1|
                            LOOP_DIURETICS==1| POTSPARING_DIURETICS==1|
                            POT_SPARING_COMPOUND_DIURTETICS==1|
                            ALDOSTERONE_ANTAG==1|
                            VERAPMAIL_DILTIAZEM==1| BETABLOCKER==1|
                            VASODILATOR_ANTIHYPERTENSIVES==1|
                            CENTRAL_ANTIHYPERTENSIVES==1| ALFA_BLOCKERS==1|
                            ACEI_ARB_ENTRESTO==1|
                            DIHYDROPYRIDINE_CALCIUM_CHANNEL==1)

h2_summary <- tbl_summary(h2, by = STROKE_TYPE)

h2_summary

#Table Merge

h_table <- tbl_merge(tbls = list(h1_summary, h2_summary),
                     tab_spanner = c("Pre-Covid", "Covid"))

h_table



#Lipids Modifying Therapy
#General table

stroke_lipid <- stroke_1 %>% select(PRE_POST_COVID, LLT_HIGH_STATIN ,
                                    LLT_LOW_STATIN, LLT_INEGY, 
                                    LLT_EZETIMIBE, LLT_FIBRATE)


#stroke_lipid_table <- stroke_lipid %>% filter(LLT_HIGH_STATIN==1|
                                            #LLT_LOW_STATIN==1 | LLT_INEGY ==1 |
                                            #LLT_EZETIMIBE==1 | LLT_FIBRATE==1)

stroke_lipid_summary <-tbl_summary(stroke_lipid, by = PRE_POST_COVID) %>% add_p()

stroke_lipid_summary #TABLE NUMBER SEVEN

#Pre and Post tables
#Pre
l_table1 <- precovid_stroke %>% select(STROKE_TYPE, LLT_HIGH_STATIN ,
                                LLT_LOW_STATIN, LLT_INEGY, 
                                LLT_EZETIMIBE, LLT_FIBRATE)


#l1 <- l_table1 %>% filter(LLT_HIGH_STATIN==1| LLT_LOW_STATIN==1 | LLT_INEGY ==1 |
                     #LLT_EZETIMIBE==1 | LLT_FIBRATE==1)

l1_summary <-tbl_summary(l_table1, by = STROKE_TYPE)

l1_summary


#Cov
l_table2 <- postcovid_stroke %>% select(STROKE_TYPE, LLT_HIGH_STATIN ,
                                        LLT_LOW_STATIN, LLT_INEGY,
                                       LLT_EZETIMIBE, LLT_FIBRATE)

#l2 <- l_table2 %>% filter(LLT_HIGH_STATIN==1| LLT_LOW_STATIN==1 | LLT_INEGY ==1 |
                            #LLT_EZETIMIBE==1 | LLT_FIBRATE==1)

l2_summary <-tbl_summary(l_table2, by = STROKE_TYPE)

l2_summary

l_table <- tbl_merge(tbls = list(l1_summary, l2_summary),
                     tab_spanner = c("Pre-Covid", "Covid"))

l_table


#Others Medications
#General table

stroke_other <- stroke_1 %>% select(PRE_POST_COVID, PPI, FISH, OTHER_LIPID,
                                    INSULIN,ORAL_ANTIDIABETICS, NSAID)
                                    


stroke_other_table <- stroke_other %>% filter(PPI==1|FISH==1|OTHER_LIPID==1|
                                    INSULIN==1|ORAL_ANTIDIABETICS==1|NSAID==1)

stroke_other_summary <-tbl_summary(stroke_other_table, by = PRE_POST_COVID) %>%
  add_p()

stroke_other_summary

#Pre and Post tables
#Pre
other_table1 <- precovid_stroke %>% select(STROKE_TYPE, PPI, FISH, OTHER_LIPID,
                                           INSULIN, ORAL_ANTIDIABETICS, NSAID)

#other1 <- other_table1 %>% filter(PPI==1|FISH==1|OTHER_LIPID==1|INSULIN==1|
                                            #ORAL_ANTIDIABETICS==1|NSAID==1)

other_summary1 <- tbl_summary(other1, by = STROKE_TYPE)

other_summary1


#Covid
other_table2 <- postcovid_stroke %>% select(STROKE_TYPE, PPI, FISH, OTHER_LIPID,
                                            INSULIN, ORAL_ANTIDIABETICS, NSAID)

#other2 <- other_table2 %>% filter(PPI==1|FISH==1|OTHER_LIPID==1|INSULIN==1|
                                    #ORAL_ANTIDIABETICS==1|NSAID==1)

other_summary2 <- tbl_summary(other2, by = STROKE_TYPE)

other_summary2

#not main ones

#Table merge
other_table <- tbl_merge(tbls = list(other_summary1, other_summary2),
                         tab_spanner = c("Pre-Covid", "Covid"))

other_table


#FOLLOW UP
#General table

stroke_follow <- stroke_1 %>% select(PRE_POST_COVID,STUDY_120D_FOLLOW_UP,
                                     STUDY_365D_FOLLOW_UP)


stroke_follow_table <- stroke_follow %>%
  filter(STUDY_120D_FOLLOW_UP==1|STUDY_365D_FOLLOW_UP==1)

stroke_follow_summary <-tbl_summary(stroke_follow_table, by = PRE_POST_COVID)

stroke_follow_summary


#Pre and Post tables
#Pre
prestroke_followup_table <- precovid_stroke %>%
  select(STROKE_TYPE, STUDY_120D_FOLLOW_UP, STUDY_365D_FOLLOW_UP)

prefollow <- prestroke_followup_table %>%
  filter(STUDY_120D_FOLLOW_UP==1|STUDY_365D_FOLLOW_UP==1)

prefollow_summary <- tbl_summary(prefollow, by = STROKE_TYPE)

prefollow_summary

#Covid
poststroke_followup_table <- postcovid_stroke %>%
  select(STROKE_TYPE, STUDY_120D_FOLLOW_UP, STUDY_365D_FOLLOW_UP)

postfollow <- poststroke_followup_table %>%
  filter(STUDY_120D_FOLLOW_UP==1|STUDY_365D_FOLLOW_UP==1)

postfollow_summary <- tbl_summary(postfollow, by = STROKE_TYPE)

postfollow_summary

#Table merge
followup_table <- tbl_merge(tbls = list(prefollow_summary, postfollow_summary),
                            tab_spanner = c("Pre-Covid", "Covid"))

followup_table


#HYPERTENSION AND LIPID PROFILE TESTS

pre_stroke_tests <- precovid_stroke %>% select(c("ALF_PE", "SYS_60", "DYS_60",
                                          "SYS_120", "DYS_120", "SYS_180",
                                          "DYS_180", "SYS_365", "DYS_365",
                                          "LDL_60",  "NON_HDL_60" , "TG_60",
                                          "LDL_120", "NON_HDL_120", "TG_120",
                                          "LDL_180", "NON_HDL_180", "TG_180",
                                          "LDL_365", "NON_HDL_365", "TG_365"))

post_stroke_tests <- precovid_stroke %>% select(c("ALF_PE", "SYS_60", "DYS_60",
                                            "SYS_120", "DYS_120", "SYS_180",
                                            "DYS_180", "SYS_365", "DYS_365",
                                            "LDL_60",  "NON_HDL_60" , "TG_60",
                                            "LDL_120", "NON_HDL_120", "TG_120",
                                            "LDL_180", "NON_HDL_180", "TG_180",
                                            "LDL_365", "NON_HDL_365", "TG_365"))


#TARGETED BLOOD PRESSURE (140/90)

#At day 60

#Documented vs Undocumented

for(i in 1:nrow(stroke_1)){
  if (is.na(stroke_1$SYS_60[i]) || is.na(stroke_1$DYS_60[i])) {
    stroke_1$Blood_Pressure[i] <- "Not Documented"
  } else if (stroke_1$SYS_60[i] >= 140 || stroke_1$DYS_60[i] >= 90){
    stroke_1$Blood_Pressure[i] <- "Documented"
  } else {
    stroke_1$Blood_Pressure[i] <- "Documented"
  }  
}


stroke_bpTest1.1 <- stroke_1 %>% select(PRE_POST_COVID, Blood_Pressure)

stroke_bpProfile1.1 <- stroke_bpTest1.1 %>%
                                     filter(Blood_Pressure == "Not Documented" 
                                             |Blood_Pressure == "Documented" )

stroke_lbp_summary1.1 <-tbl_summary(stroke_bpProfile1.1,
                                    by = PRE_POST_COVID) %>% add_p()

stroke_lbp_summary1.1 

#High vs Normal

for(i in 1:nrow(stroke_1)){
  if (is.na(stroke_1$SYS_60[i]) || is.na(stroke_1$DYS_60[i])) {
    stroke_1$BP[i] <- "Not Documented"
  } else if (stroke_1$SYS_60[i] >= 140 || stroke_1$DYS_60[i] >= 90){
    stroke_1$BP[i] <- "High"
  } else {
    stroke_1$BP[i] <- "Normal"
  }  
}

stroke_bpTest1.2 <- stroke_1 %>% select(PRE_POST_COVID, BP)

stroke_bpProfile1.2 <- stroke_bpTest1.2 %>% filter(BP == "High"|BP == "Normal")
                                                

stroke_lbp_summary1.2 <-tbl_summary(stroke_bpProfile1.2,
                                    by = PRE_POST_COVID) %>% add_p()

stroke_lbp_summary1.2


#At day 365
#Documented vs Undocumented

for(i in 1:nrow(stroke_1)){
  if (is.na(stroke_1$SYS_365[i]) || is.na(stroke_1$DYS_365[i])) {
    stroke_1$Blood_Pressure[i] <- "Not Documented"
  } else if (stroke_1$SYS_365[i] >= 140 || stroke_1$DYS_365[i] >= 90){
    stroke_1$Blood_Pressure[i] <- "Documented"
  } else {
    stroke_1$Blood_Pressure[i] <- "Documented"
  }  
}


stroke_bpTest2.1 <- stroke_1 %>% select(PRE_POST_COVID, Blood_Pressure)

stroke_bpProfile2.1 <- stroke_bpTest2.1 %>%
                                     filter(Blood_Pressure == "Not Documented" 
                                              |Blood_Pressure == "Documented")

stroke_lbp_summary2.1 <-tbl_summary(stroke_bpProfile2.1,
                                    by = PRE_POST_COVID) %>% add_p()

stroke_lbp_summary2.1

#High vs Normal

for(i in 1:nrow(stroke_1)){
  if (is.na(stroke_1$SYS_365[i]) || is.na(stroke_1$DYS_365[i])) {
    stroke_1$BP[i] <- "Not Documented"
  } else if (stroke_1$SYS_365[i] >= 140 || stroke_1$DYS_365[i] >= 90){
    stroke_1$BP[i] <- "High"
  } else {
    stroke_1$BP[i] <- "Normal"
  }  
}


stroke_bpTest2.2 <- stroke_1 %>% select(PRE_POST_COVID, BP)

stroke_bpProfile2.2 <- stroke_bpTest2.2 %>% filter(BP == "High"|BP == "Normal" )

stroke_lbp_summary2.2 <-tbl_summary(stroke_bpProfile2.2,
                                    by = PRE_POST_COVID) %>% add_p()

stroke_lbp_summary2.2

#Table merge

#Documented vs Undocumented
stroke_bp_table1 <- tbl_merge(tbls = list(stroke_lbp_summary1.1,
                                          stroke_lbp_summary2.1),
                             tab_spanner = c("Day 60", "Day 365"))

stroke_bp_table1 #TABLE NUMBER EIGHT

#High vs Normal
stroke_bp_table2 <- tbl_merge(tbls = list(stroke_lbp_summary1.2,
                                          stroke_lbp_summary2.2),
                             tab_spanner = c("Day 60", "Day 365"))

stroke_bp_table2 #TABLE NUMBER NINE


#LIPID LEVELS
#TARGETED LIPID LEVELS (LDL<1.8 mmol/l, non-HDL-C <2.6mmol/l, TG <2.3 mmol/l)
#LDL<1.8 mmol/l


#Day 60
#LDL Profile


for(i in 1:nrow(stroke_1)){
  if (is.na(stroke_1$LDL_60[i])) {
    stroke_1$LDL_Profile[i] <- "Not Documented"
  } else if (stroke_1$LDL_60[i] <= 1.8){
    stroke_1$LDL_Profile[i] <- "Documented"
  } else {
    stroke_1$LDL_Profile[i] <- "Documented"
  }  
}

#Documented vs Undocumented


stroke_lipidTest1.1 <- stroke_1 %>% select(PRE_POST_COVID, LDL_Profile)

stroke_lipidProfile1.1 <- stroke_lipidTest1.1 %>%
                                        filter(LDL_Profile == "Not Documented" 
                                                |LDL_Profile == "Documented" )

stroke_lLipid_summary1.1 <-tbl_summary(stroke_lipidProfile1.1,
                                       by = PRE_POST_COVID) %>% add_p()

stroke_lLipid_summary1.1


# Target (<=1.8 mmol/l) vs (>1.8 mmol/l)

for(i in 1:nrow(stroke_1)){
  if (is.na(stroke_1$LDL_60[i])) {
    stroke_1$LDL_P[i] <- "Not Documented"
  } else if (stroke_1$LDL_60[i] <= 1.8){
    stroke_1$LDL_P[i] <- "Target (<=1.8 mmol/l)"
  } else {
    stroke_1$LDL_P[i] <- ">1.8 mmol/l"
  }  
}



stroke_lipidTest1.2 <- stroke_1 %>% select(PRE_POST_COVID, LDL_P)

stroke_lipidProfile1.2 <- stroke_lipidTest1.2 %>%
                                       filter(LDL_P == "Target (<=1.8 mmol/l)"
                                                       |LDL_P == ">1.8 mmol/l" )

stroke_lLipid_summary1.2 <-tbl_summary(stroke_lipidProfile1.2,
                                       by = PRE_POST_COVID) %>% add_p()

stroke_lLipid_summary1.2


#Day 365

#LDL Profile


for(i in 1:nrow(stroke_1)){
  if (is.na(stroke_1$LDL_365[i])) {
    stroke_1$LDL_Profile[i] <- "Not Documented"
  } else if (stroke_1$LDL_365[i] <= 1.8){
    stroke_1$LDL_Profile[i] <- "Documented"
  } else {
    stroke_1$LDL_Profile[i] <- "Documented"
  }  
}

#Documented vs Undocumented


stroke_lipidTest2.1 <- stroke_1 %>% select(PRE_POST_COVID, LDL_Profile)

stroke_lipidProfile2.1 <- stroke_lipidTest2.1 %>%
                                         filter(LDL_Profile == "Not Documented" 
                                                  |LDL_Profile == "Documented" )

stroke_lLipid_summary2.1 <-tbl_summary(stroke_lipidProfile2.1,
                                       by = PRE_POST_COVID) %>% add_p()

stroke_lLipid_summary2.1


# Target (<=1.8 mmol/l) vs (>1.8 mmol/l)

for(i in 1:nrow(stroke_1)){
  if (is.na(stroke_1$LDL_365[i])) {
    stroke_1$LDL_P[i] <- "Not Documented"
  } else if (stroke_1$LDL_365[i] <= 1.8){
    stroke_1$LDL_P[i] <- "Target (<=1.8 mmol/l)"
  } else {
    stroke_1$LDL_P[i] <- ">1.8 mmol/l"
  }  
}


stroke_lipidTest2.2 <- stroke_1 %>% select(PRE_POST_COVID, LDL_P)

stroke_lipidProfile2.2 <- stroke_lipidTest2.2 %>%
                                         filter(LDL_P == "Target (<=1.8 mmol/l)"
                                                       |LDL_P == ">1.8 mmol/l" )

stroke_lLipid_summary2.2 <-tbl_summary(stroke_lipidProfile2.2,
                                       by = PRE_POST_COVID) %>% add_p()

stroke_lLipid_summary2.2

#Table Merge 1
stroke_ldl_table1 <- tbl_merge(tbls = list(stroke_lLipid_summary1.1,
                                           stroke_lLipid_summary2.1),
                              tab_spanner = c("Day 60", "Day 365"))

stroke_ldl_table1 #TABLE NUMBER TEN

#Table Merge 2
stroke_ldl_table2 <- tbl_merge(tbls = list(stroke_lLipid_summary1.2,
                                           stroke_lLipid_summary2.2),
                             tab_spanner = c("Day 60", "Day 365"))

stroke_ldl_table2 #TABLE NUMBER ELEVEN



#non-HDL-C <2.6mmol/l

#non-HDL-C Profile

#Day 60
for(i in 1:nrow(stroke_1)){
  if (is.na(stroke_1$NON_HDL_60[i])) {
    stroke_1$nonHDL_Profile[i] <- "Not Documented"
  } else if (stroke_1$NON_HDL_60[i] <= 2.6){
    stroke_1$nonHDL_Profile[i] <- "Documented"
  } else {
    stroke_1$nonHDL_Profile[i] <- "Documented"
  }  
}


#Documented vs Undocumented
stroke_HDLTest1.1 <- stroke_1 %>% select(PRE_POST_COVID, nonHDL_Profile)

stroke_HDLProfile1.1 <- stroke_HDLTest1.1 %>%
                                       filter(nonHDL_Profile == "Not Documented" 
                                               |nonHDL_Profile == "Documented" )

stroke_lHDL_summary1.1 <-tbl_summary(stroke_HDLProfile1.1,
                                     by = PRE_POST_COVID) %>% add_p()

stroke_lHDL_summary1.1


#Target (<=2.6 mmol/l) vs (>2.6 mmol/l)

for(i in 1:nrow(stroke_1)){
  if (is.na(stroke_1$NON_HDL_60[i])) {
    stroke_1$nonHDL_P[i] <- "Not Documented"
  } else if (stroke_1$NON_HDL_60[i] <= 2.6){
    stroke_1$nonHDL_P[i] <- "Target (<=2.6 mmol/l)"
  } else {
    stroke_1$nonHDL_P[i] <- ">2.6 mmol/l"
  }  
}



stroke_HDLTest1.2 <- stroke_1 %>% select(PRE_POST_COVID, nonHDL_P)

stroke_HDLProfile1.2 <- stroke_HDLTest1.2 %>%
                                     filter(nonHDL_P == "Target (<=2.6 mmol/l)" 
                                                    |nonHDL_P == ">2.6 mmol/l" )

stroke_lHDL_summary1.2 <-tbl_summary(stroke_HDLProfile1.2,
                                     by = PRE_POST_COVID) %>% add_p()

stroke_lHDL_summary1.2


#Day 365

#Documented vs Undocumented

for(i in 1:nrow(stroke_1)){
  if (is.na(stroke_1$NON_HDL_365[i])) {
    stroke_1$nonHDL_Profile[i] <- "Not Documented"
  } else if (stroke_1$NON_HDL_365[i] <= 2.6){
    stroke_1$nonHDL_Profile[i] <- "Documented"
  } else {
    stroke_1$nonHDL_Profile[i] <- "Documented"
  }  
}

#non-HDL-C Profile

stroke_HDLTest2.1 <- stroke_1 %>% select(PRE_POST_COVID, nonHDL_Profile)

stroke_HDLProfile2.1 <- stroke_HDLTest2.1 %>%
                                      filter(nonHDL_Profile == "Not Documented" 
                                              |nonHDL_Profile == "Documented" )

stroke_lHDL_summary2.1 <-tbl_summary(stroke_HDLProfile2.1,
                                     by = PRE_POST_COVID) %>% add_p()

stroke_lHDL_summary2.1


#Target (<=2.6 mmol/l) vs (>2.6 mmol/l)

for(i in 1:nrow(stroke_1)){
  if (is.na(stroke_1$NON_HDL_365[i])) {
    stroke_1$nonHDL_P[i] <- "Not Documented"
  } else if (stroke_1$NON_HDL_365[i] <= 2.6){
    stroke_1$nonHDL_P[i] <- "Target (<=2.6 mmol/l)"
  } else {
    stroke_1$nonHDL_P[i] <- ">2.6 mmol/l"
  }  
}



stroke_HDLTest2.2 <- stroke_1 %>% select(PRE_POST_COVID, nonHDL_P)

stroke_HDLProfile2.2 <- stroke_HDLTest2.2 %>%
                                      filter(nonHDL_P == "Target (<=2.6 mmol/l)" 
                                                   |nonHDL_P == ">2.6 mmol/l" )

stroke_lHDL_summary2.2 <-tbl_summary(stroke_HDLProfile2.2,
                                     by = PRE_POST_COVID) %>% add_p()

stroke_lHDL_summary2.2


#Table Merge 1
stroke_hdl_table1 <- tbl_merge(tbls = list(stroke_lHDL_summary1.1,
                                           stroke_lHDL_summary2.1),
                              tab_spanner = c("Day 60", "Day 365"))

stroke_hdl_table1 #TABLE NUMBER TWELVE

#Table Merge 2
stroke_hdl_table2 <- tbl_merge(tbls = list(stroke_lHDL_summary1.2,
                                           stroke_lHDL_summary2.2),
                              tab_spanner = c("Day 60", "Day 365"))

stroke_hdl_table2 #TABLE NUMBER THIRTEEN

                                                                                                                          
#TG <2.3 mmol/l
#Day 60

#Documented vs Undocumented

for(i in 1:nrow(stroke_1)){
  if (is.na(stroke_1$TG_60[i])) {
    stroke_1$TG_Profile[i] <- "Not Documented"
  } else if (stroke_1$TG_60[i] <= 2.3){
    stroke_1$TG_Profile[i] <- "Documented"
  } else {
    stroke_1$TG_Profile[i] <- "Documented"
  }  
}

#TG Profile

stroke_TGTest1.1 <- stroke_1 %>% select(PRE_POST_COVID, TG_Profile)

stroke_TGProfile1.1 <- stroke_TGTest1.1 %>% filter(TG_Profile == "Not Documented" 
                                             |TG_Profile == "Documented" )

stroke_lTG_summary1.1 <-tbl_summary(stroke_TGProfile1.1,
                                    by = PRE_POST_COVID) %>% add_p()

stroke_lTG_summary1.1

#Target (<= 2.3) vs (>2.3)
for(i in 1:nrow(stroke_1)){
  if (is.na(stroke_1$TG_60[i])) {
    stroke_1$TG_P[i] <- "Not Documented"
  } else if (stroke_1$TG_60[i] <= 2.3){
    stroke_1$TG_P[i] <- "Target (<= 2.3)"
  } else {
    stroke_1$TG_P[i] <- ">2.3"
  }  
}

#TG Profile

stroke_TGTest1.2 <- stroke_1 %>% select(PRE_POST_COVID, TG_P)

stroke_TGProfile1.2 <- stroke_TGTest1.2 %>% filter(TG_P == "Target (<= 2.3)" 
                                                   |TG_P == ">2.3" )

stroke_lTG_summary1.2 <-tbl_summary(stroke_TGProfile1.2,
                                    by = PRE_POST_COVID) %>% add_p()

stroke_lTG_summary1.2


#Day 365
#Documented vs Undocumented

for(i in 1:nrow(stroke_1)){
  if (is.na(stroke_1$TG_365[i])) {
    stroke_1$TG_Profile[i] <- "Not Documented"
  } else if (stroke_1$TG_365[i] <= 2.3){
    stroke_1$TG_Profile[i] <- "Documented"
  } else {
    stroke_1$TG_Profile[i] <- "Documented"
  }  
}

#TG Profile

stroke_TGTest2.1 <- stroke_1 %>% select(PRE_POST_COVID, TG_Profile)

stroke_TGProfile2.1 <- stroke_TGTest2.1 %>%
                                          filter(TG_Profile == "Not Documented" 
                                          |TG_Profile == "Documented" )

stroke_lTG_summary2.1 <-tbl_summary(stroke_TGProfile2.1,
                                    by = PRE_POST_COVID) %>% add_p()

stroke_lTG_summary2.1

#Target (<= 2.3) vs >2.3

for(i in 1:nrow(stroke_1)){
  if (is.na(stroke_1$TG_365[i])) {
    stroke_1$TG_P[i] <- "Not Documented"
  } else if (stroke_1$TG_365[i] <= 2.3){
    stroke_1$TG_P[i] <- "Target (<= 2.3)"
  } else {
    stroke_1$TG_P[i] <- ">2.3"
  }  
}

#TG Profile

stroke_TGTest2.2 <- stroke_1 %>% select(PRE_POST_COVID, TG_P)

stroke_TGProfile2.2 <- stroke_TGTest2.2 %>% filter(TG_P == "Target (<= 2.3)" 
                                                   |TG_P == ">2.3" )

stroke_lTG_summary2.2 <-tbl_summary(stroke_TGProfile2.2,
                                    by = PRE_POST_COVID) %>% add_p()

stroke_lTG_summary2.2


#Table Merge 1-Documented vs Undocumented 
stroke_tg_table1 <- tbl_merge(tbls = list(stroke_lTG_summary1.1,
                                          stroke_lTG_summary2.1),
                             tab_spanner = c("Day 60", "Day 365"))

stroke_tg_table1 #TABLE NUMBER FOURTEEN

#Table Merge 2-Target (<= 2.3) vs >2.3

stroke_tg_table2 <- tbl_merge(tbls = list(stroke_lTG_summary1.2,
                                          stroke_lTG_summary2.2),
                              tab_spanner = c("Day 60", "Day 365"))

stroke_tg_table2 #TABLE NUMBER FIFTEEN


#MEDICATION
#FOR BLOOD PRESSURE CONTROL
precovid_stroke <- stroke_1 %>% filter(PRE_POST_COVID == "PRE_COVID")

#precovid_stroke_new <- precovid_stroke_new %>%
#filter(CENSOR_DT >= as.Date("2015-03-01") &
#CENSOR_DT <= as.Date("2019-03-31"))

postcovid_stroke <- stroke_1 %>% filter(PRE_POST_COVID == "POST_COVID")

#postcovid_stroke_new <- postcovid_stroke_new %>%
#filter(CENSOR_DT >= as.Date("2020-03-23") &
 #CENSOR_DT <= as.Date("2021-03-31"))

#Pre and Post tables for Blood Pressure Medication
#Pre
bpmed1 <- precovid_stroke %>% select(Blood_Pressure,THIAZIDE_DIURETICS,
                                             LOOP_DIURETICS,ALDOSTERONE_ANTAG,
                                             VERAPMAIL_DILTIAZEM, BETABLOCKER,
                                             ALFA_BLOCKERS, ACEI_ARB_ENTRESTO,
                                             DIHYDROPYRIDINE_CALCIUM_CHANNEL)

#bpmed1_table <- bpmed1 %>% filter(THIAZIDE_DIURETICS==1|
                                  #LOOP_DIURETICS==1|ALDOSTERONE_ANTAG==1|
                                  #VERAPMAIL_DILTIAZEM | BETABLOCKER==1|
                                  #ALFA_BLOCKERS==1| ACEI_ARB_ENTRESTO==1|
                                  #DIHYDROPYRIDINE_CALCIUM_CHANNEL==1)

bpmed1summary <- tbl_summary(bpmed1, by = Blood_Pressure) %>% add_p()

bpmed1summary

#Covid
bpmed2 <- postcovid_stroke %>% select(Blood_Pressure,THIAZIDE_DIURETICS,
                                        LOOP_DIURETICS,ALDOSTERONE_ANTAG,
                                        VERAPMAIL_DILTIAZEM, BETABLOCKER,
                                        ALFA_BLOCKERS, ACEI_ARB_ENTRESTO,
                                        DIHYDROPYRIDINE_CALCIUM_CHANNEL)

#bpmed2_table <- bpmed2 %>% filter(THIAZIDE_DIURETICS==1|
                             #LOOP_DIURETICS==1|ALDOSTERONE_ANTAG==1|
                             #VERAPMAIL_DILTIAZEM | BETABLOCKER==1|
                             #ALFA_BLOCKERS==1| ACEI_ARB_ENTRESTO==1|
                             #DIHYDROPYRIDINE_CALCIUM_CHANNEL==1)

bpmed2summary <- tbl_summary(bpmed2, by = Blood_Pressure) %>% add_p()

bpmed2summary

#Table Merge

bpmed_table <- tbl_merge(tbls = list(bpmed1summary, bpmed2summary),
                         tab_spanner = c("Pre-Covid", "Covid"))

bpmed_table #TABLE NUMBER SIXTEEN


#FOR LIPIDS LEVELS CONTROL
precovid_stroke <- stroke_1 %>% filter(PRE_POST_COVID == "PRE_COVID")

#precovid_stroke_new <- precovid_stroke_new %>% 
#filter(CENSOR_DT >= as.Date("2015-03-01") &
#CENSOR_DT <= as.Date("2019-03-31"))

postcovid_stroke <- stroke_1 %>% filter(PRE_POST_COVID == "POST_COVID")

#postcovid_stroke_new <- postcovid_stroke_new %>%
#filter(CENSOR_DT >= as.Date("2020-03-23") &
#CENSOR_DT <= as.Date("2021-03-31"))
                                                          
#Pre and Post tables for Lipid Lowering therapy

#1. LDL
#Pre
ldl_med1 <- precovid_stroke %>% select(LDL_Profile, LLT_HIGH_STATIN,
                                       LLT_LOW_STATIN, LLT_INEGY,
                                             LLT_EZETIMIBE, LLT_FIBRATE)

#ldl1 <- ldl_med1 %>% filter(LLT_HIGH_STATIN==1 | LLT_LOW_STATIN==1 |
                              #LLT_INEGY ==1 |
                              #LLT_EZETIMIBE==1 | LLT_FIBRATE==1)


ldl1_summary <- tbl_summary(ldl_med1, by = LDL_Profile) %>% add_p()

ldl1_summary

#Covid
ldl_med2 <- postcovid_stroke %>% select(LDL_Profile, LLT_HIGH_STATIN,
                                        LLT_LOW_STATIN, LLT_INEGY,
                                             LLT_EZETIMIBE, LLT_FIBRATE)

#ldl2 <- ldl_med2 %>% filter(LLT_HIGH_STATIN==1| LLT_LOW_STATIN==1 |
                                  #LLT_INEGY ==1 |
                                  #LLT_EZETIMIBE==1 | LLT_FIBRATE==1)

ldl2_summary <- tbl_summary(ldl_med2, by = LDL_Profile) %>% add_p

ldl2_summary

#Table Merge

ldl_table <- tbl_merge(tbls = list(ldl1_summary, ldl2_summary),
                       tab_spanner = c("Pre-Covid", "Covid"))

ldl_table #TABLE NUMBER SEVENTEEN


#2.non-HDL

#Pre
hdl_med1 <- precovid_stroke %>% select(nonHDL_Profile, LLT_HIGH_STATIN,
                                       LLT_LOW_STATIN, LLT_INEGY,
                                           LLT_EZETIMIBE, LLT_FIBRATE)

#hdl1 <- hdl_med1 %>% filter(LLT_HIGH_STATIN==1| LLT_LOW_STATIN==1 |
                              #LLT_INEGY ==1 |
                              #LLT_EZETIMIBE==1 | LLT_FIBRATE==1)

hdl1_summary <- tbl_summary(hdl_med1, by = nonHDL_Profile) %>% add_p()

hdl1_summary

#Covid
hdl_med2 <- postcovid_stroke %>% select(nonHDL_Profile, LLT_HIGH_STATIN,
                                        LLT_LOW_STATIN, LLT_INEGY,
                                            LLT_EZETIMIBE, LLT_FIBRATE)

#hdl2 <- hdl_med2 %>% filter(LLT_HIGH_STATIN==1| LLT_LOW_STATIN==1 |
                                #LLT_INEGY ==1 |
                                #LLT_EZETIMIBE==1 | LLT_FIBRATE==1)

hdl2_summary <- tbl_summary(hdl_med2, by = nonHDL_Profile) %>% add_p()

hdl2_summary

#Table Merge

hdl_table <- tbl_merge(tbls = list(hdl1_summary, hdl2_summary),
                       tab_spanner = c("Pre-Covid", "Covid"))

hdl_table #TABLE NUMBER EIGHTEEN

#3. TG

tg_med1 <- precovid_stroke %>% select(TG_Profile, LLT_HIGH_STATIN,
                                      LLT_LOW_STATIN, LLT_INEGY,
                                           LLT_EZETIMIBE, LLT_FIBRATE)

#tg1 <- tg_med1 %>% filter(LLT_HIGH_STATIN==1| LLT_LOW_STATIN==1 | LLT_INEGY ==1 |
                              #LLT_EZETIMIBE==1 | LLT_FIBRATE==1)

tg_summary1 <- tbl_summary(tg_med1, by = TG_Profile) %>% add_p()

tg_summary1

#Covid
tg_med2 <- postcovid_stroke %>% select(TG_Profile, LLT_HIGH_STATIN,
                                       LLT_LOW_STATIN, LLT_INEGY,
                                          LLT_EZETIMIBE, LLT_FIBRATE)

#tg2 <- tg_med2 %>% filter(LLT_HIGH_STATIN==1| LLT_LOW_STATIN==1 | LLT_INEGY ==1 |
                            #LLT_EZETIMIBE==1 | LLT_FIBRATE==1)

tg_summary2 <- tbl_summary(tg_med2, by = TG_Profile) %>% add_p()

tg_summary2

#Table Merge

tg_table <- tbl_merge(tbls = list(tg_summary1, tg_summary2),
                      tab_spanner = c("Pre-Covid", "Covid"))

tg_table #TABLE NUMBER NINETEEN


#REGRESSION ANALYSIS

stroke_1$Optimum_Care <- ifelse ((stroke_1$LLT_HIGH_STATIN ==1 |
                                stroke_1$LLT_LOW_STATIN ==1 |
                                stroke_1$LLT_INEGY==1 |
                                stroke_1$LLT_EZETIMIBE==1 | 
                                stroke_1$LLT_FIBRATE==1 |
                                stroke_1$P2Y12_ANTIPLATELET==1 |
                                stroke_1$PARENTERAL_ANTICOAG==1 |
                                stroke_1$VIT_K_ANTICOAG ==1 |
                                stroke_1$DOAC==1 |stroke_1$ASPIRIN==1 |
                                stroke_1$NSAID == 1 |stroke_1$BP == "Normal"&
                                stroke_1$Blood_Pressure == "Documented"), 1, 0)

unique(stroke_1$Optimum_Care)
stroke_1 %>% count(Optimum_Care)

stroke_2 <- stroke_1 %>% select(AGE_DIAG, AF_BEFORE, DEMENTIA_BEFORE,
                                DIABETES_BEFORE,RESP_BEFORE ,LIVER_BEFORE,
                                CKD_BEFORE, HYPERTENSION_BEFORE, Optimum_Care) %>%
  filter(stroke_1$STROKE_TYPE == "ISCH")


#Split the data into training & testing sets
set.seed(123) #set seed for reproducibility

#Data splits into training and testing sets
train_index <- sample(1:nrow(stroke_2), 0.65*nrow(stroke_1))
train_data <- stroke_2[train_index,]
test_data <- stroke_2[-train_index,]

#LOGISTIC REGRESSION
#Fit a logistic regression model for binary response
logistic_model <- glm(Optimum_Care ~AGE_DIAG + AF_BEFORE + DEMENTIA_BEFORE + 
                        DIABETES_BEFORE + RESP_BEFORE + LIVER_BEFORE + 
                        CKD_BEFORE + HYPERTENSION_BEFORE, data = train_data)
logistic_model

par(mfrow=c(2,2), xpd=NA)
plot(logistic_model) #PLOT NUMBER TWENTY

summary(logistic_model)
model_summary <- summary(logistic_model)

coff <- model_summary$coefficients
coff
write.csv(coff, file = "coff.csv", sep = ",") #TABLE NUMBER TWENTY ONE

#Make Predictions

#Predict using Logistic regression model
logistic_prediction <- predict(logistic_model, newdata = test_data,
                               type = "response")

logistic_prediction

summary(logistic_prediction)


#Evaluate the model
#logistic model-ROC

pred <- prediction (logistic_prediction, test_data$Optimum_Care)
perf <- performance(pred, measure = "tpr", x.measure = "fpr")
plot(perf, main = "ROC curve for Optimum Care", col = "blue", lwd = 3)
abline(a = 0, b = 1, lwd = 2, lty = 2) #PLOT NUMBER TWENTY TWO

perf.auc <- performance(pred, measure = "auc")
str(perf.auc)
perf.auc

auc_value <- unlist(perf.auc@y.values)

auc_value

write.csv(auc_value, file = "auc_value.csv", sep = ",")#VALUE NUMBER TWENTY THREE



#SURVIVAL ANALYSIS

precovid_stroke <- precovid_stroke %>%
  mutate ("SURVIVAL DAYS" = as.numeric(DEATH_DT - EVENT_DT))
postcovid_stroke <- postcovid_stroke %>%
  mutate ("SURVIVAL DAYS" = as.numeric(DEATH_DT - EVENT_DT))

#
precovid_surv <- precovid_stroke %>%
  select(STUDY_30D_FOLLOW_UP, STUDY_60D_FOLLOW_UP,
                                      STUDY_90D_FOLLOW_UP, STUDY_120D_FOLLOW_UP,
                                      STUDY_365D_FOLLOW_UP, `SURVIVAL DAYS`)

postcovid_surv <- postcovid_stroke %>%
  select(STUDY_30D_FOLLOW_UP, STUDY_60D_FOLLOW_UP,
                                      STUDY_90D_FOLLOW_UP, STUDY_120D_FOLLOW_UP,
                                      STUDY_365D_FOLLOW_UP, `SURVIVAL DAYS`)

#Survival objects

surv_obj_pre <- Surv(precovid_surv$`SURVIVAL DAYS`)
surv_obj_co <- Surv(postcovid_surv$`SURVIVAL DAYS`)

#Kaplan Meier Survival Curves

fit_pre <- survfit(surv_obj_pre~ precovid_surv$STUDY_30D_FOLLOW_UP +
        precovid_surv$STUDY_60D_FOLLOW_UP + precovid_surv$STUDY_90D_FOLLOW_UP +
        precovid_surv$STUDY_120D_FOLLOW_UP + precovid_surv$STUDY_365D_FOLLOW_UP)


fit_post <- survfit(surv_obj_co~ postcovid_surv$STUDY_30D_FOLLOW_UP +
      postcovid_surv$STUDY_60D_FOLLOW_UP + postcovid_surv$STUDY_90D_FOLLOW_UP +
      postcovid_surv$STUDY_120D_FOLLOW_UP + postcovid_surv$STUDY_365D_FOLLOW_UP)


#plot
plot(fit_pre, col = 1:5, lty = 1, xlab = "Time in Days", 
     ylab = "Survival Probability", main = "Survival Curve")

lines(fit_post, col = 1:5, lty = 2)

#
legend("topright", legend = c("pre", "post"),
       col = 1:5, lty = c(1,2), title = "Pre and Covid Periods")


#Smoothing the Curve-1

#Time points for smoothed curve

time_points_1 <- seq(0, max (fit_pre$time), length.out = 1000)
time_points_2 <- seq(0, max (fit_post$time), length.out = 1000)

#Fitting a cubic spline to smoothen the Kaplan-Meier curve
#smoothed_fit <- smooth.spline(fit_pre$time, fit_pre$surv, df = df_color)

smoothed_fit_1 <- smooth.spline(fit_pre$time, fit_pre$surv)
smoothed_fit_2 <- smooth.spline(fit_post$time, fit_post$surv)

#Calculate smoothed survival probabilities at the time points

smoothed_survival_probs_1 <- predict(smoothed_fit_1, time_points_1)$y
smoothed_survival_probs_2 <- predict(smoothed_fit_2, time_points_2)$y

#Plot smoothed Kaplan-Meier survival curve

plot(time_points_1, smoothed_survival_probs_1, type = "line" ,
     col = "blue", xlab = "Time (Days)",
     ylab = "Smoothed Survival Probability", main = title)

plot(time_points_2, smoothed_survival_probs_2, type = "line",
     col = "red", xlab = "Time (Days)",
     ylab = "Smoothed Survival Probability", main = title)

#Perform survival curve analysis and smoothed the curve for both data frame
par(mfrow = c(1,2)) #Plot side by side

smoothened_survival_curve_analysis(precovid_surv,
                            "Smoothed Survival Curve for Precov", df_color = 5)

smoothened_survival_curve_analysis (postcovid_surv,
                            "Smoothed Survival Curve for Postcov", df_color = 4)

#