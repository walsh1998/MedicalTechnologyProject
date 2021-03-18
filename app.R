##############Install necessary packages
#install.packages("shiny")
library(shiny)
#install.packages("bslib")
library(bslib)
library(tidyverse)
#install.packages("devtools")
#devtools::install_github("daattali/shinyforms")
#install.packages("googlesheets")
# library(shinydashboard)
library(DT)
library(caret)
library(mice)
# bs_theme(version = 4, bootswatch = "cerulean") %>%
#   bs_theme_preview()
setwd("/Users/stefaniewalsh/newdir")

##########Set up For RShiny Data########3
df_table <- read.csv("ShinyAppData_Full.csv")

#Get only necessary columns for patient
patient_df_table <- df_table %>% 
  dplyr::select(-c(X, PatientVisitId, PatientProfileId, PatientId, PId, EmpStatusMId, SSN, ReferenceSourceMId, PatientSameAsGuarantor, 
            GuarantorId, PatientRelationToGuarantorMId, AllocationSetId, DoctorId, FacilityId, 
            BenefitAssignmentMId, PrivacyPolicyAcknowledgementMId, ContactByMId, 
            SensitiveChart, LocationId, lupd, masterlock, visdocnum, pstatus, InTrigger, LanguageId, 
            HasPatientAccess, description_marital_status_mid, description_financial_class_mid, description_patient_status_mid, 
            description_modifier_1_mid, CPTCode, COUNT.DISTINCT.CasesId., 
            COUNT.DISTINCT.AppointmentsId., Obseity, FacilityId., CurrentCarrier, sucess, Zip))

#Rename columns for easy readibility by patients
patient_df_table <- patient_df_table %>%
  rename(SurgeryDescription = Description, InPatientOrOutPatient = description_place_of_service_mid,WhichKnee =  modifier, 
         Sex = Se, Race = description_code_patient_race_mid, 
         EmploymentStatus = description_emp_status_mid, DoYouSmoke = Smoke, DoYouHaveHighBloodPressure = HighBloodPressure, 
         DoYouHaveDiabetes = Diabetes, DoYouHaveOsteoporosis = osteoporosis, WorkersComp = MAX.WorkersComp.) %>%
  mutate(State=ifelse(State=="IL.","IL",State),
         Race=ifelse(max.b.PatientRaceMId.==42445,"Unspecified",Race),
         Race=ifelse(max.b.PatientRaceMId.==62785,"White",Race),
         EmploymentStatus=ifelse(is.na(EmploymentStatus),"Unknown",EmploymentStatus),
         MaritalStatus=ifelse(MaritalStatus=="Seperated","Separated",MaritalStatus),
         SurgeryDescription = ifelse(SurgeryDescription=="arthroplasty, knee, condyle and plateau; medial AND lateral compartments, total knee arthroplasty","Arthroplasty, knee, condyle and plateau; medial AND lateral compartments, total knee arthroplasty",SurgeryDescription),
         SurgeryDescription = ifelse(SurgeryDescription=="rtArthroplasty, knee, condyle and plateau; medial OR lateral compartment","Arthroplasty, knee, condyle and plateau; medial OR lateral compartment",SurgeryDescription))%>%
  select(-c(max.b.PatientRaceMId.)) %>%
  filter(InPatientOrOutPatient!="Office",WhichKnee!="surgery")
  
#Change 0s to Nos

patient_df_table[, 19:62][patient_df_table[, 19:62] == 0] <- "No"
patient_df_table[, 19:62][patient_df_table[, 19:62] == 1] <- "Yes"

#Change order of dataframe
patient_df_table <- patient_df_table[,c(12, 1:11, 13:62)]
################Import Model#########
library(tidyverse)
library(caret)
library(randomForest)

rf<-readRDS(file = "rfFINAL.rds")
df <- read.csv("ShinyAppData_Full.csv")

patient_df<-df%>%
  rename(EmpStatus=description_emp_status_mid,
         Race = description_code_patient_race_mid,
         PlaceOfService=description_place_of_service_mid)%>%
  mutate(State=ifelse(State=="IL.","IL",State),
         Race=ifelse(max.b.PatientRaceMId.==42445,"Unspecified",Race),
         Race=ifelse(max.b.PatientRaceMId.==62785,"White",Race),
         EmpStatus=ifelse(is.na(EmpStatus),"Unknown",EmpStatus),
         MaritalStatus=ifelse(MaritalStatus=="Seperated","Separated",MaritalStatus),
         Description = ifelse(Description=="arthroplasty, knee, condyle and plateau; medial AND lateral compartments, total knee arthroplasty","Arthroplasty, knee, condyle and plateau; medial AND lateral compartments, total knee arthroplasty",Description),
         Description = ifelse(Description=="rtArthroplasty, knee, condyle and plateau; medial OR lateral compartment","Arthroplasty, knee, condyle and plateau; medial OR lateral compartment",Description))%>%
  select(-c(X, PatientProfileId, PatientId, PId, EmpStatusMId, SSN, ReferenceSourceMId, PatientSameAsGuarantor, 
            GuarantorId, PatientRelationToGuarantorMId, AllocationSetId, DoctorId, FacilityId.,FacilityId, 
            BenefitAssignmentMId, PrivacyPolicyAcknowledgementMId, ContactByMId, 
            SensitiveChart, LocationId, lupd, masterlock, visdocnum, pstatus, InTrigger, LanguageId, 
            HasPatientAccess, description_marital_status_mid, description_financial_class_mid, description_patient_status_mid, 
            description_modifier_1_mid, CPTCode,max.b.PatientRaceMId.,
            Age,CurrentCarrier,Patient_Status,City,Zip,
            Family.History.of.Birth.Defects,
            Family.History.of.Endometriosis,
            Family.History.of.Growth.Development.Disorder,
            Family.History.of.Headaches, Last, Birthdate))%>%
  filter(PlaceOfService!="Office",modifier!="surgery")

d_final<-patient_df%>%
  mutate(FH.Blood = pmax(Family.History.of.Anemia
                         ,Family.History.of.Bleeding.Disease
                         ,Family.History.of.High.Cholesterol
                         ,Family.History.of.Hypertension),
         FH.Cancer = pmax(Family.History.of.Breast.Cancer
                          ,Family.History.of.Cervical.Cancer
                          ,Family.History.of.Colon.Cancer
                          ,Family.History.of.Lung.Cancer
                          ,Family.History.of.Melanoma
                          ,Family.History.of.Other.Cancer
                          ,Family.History.of.Ovarian.Cancer
                          ,Family.History.of.Uterine.Cancer),
         FH.Cardio.Disease = pmax(Family.History.of.Angina
                                  ,Family.History.of.Coronary.Heart.Disease
                                  ,Family.History.of.Heart.Disease
                                  ,Family.History.of.Stroke.CVA),
         FH.Inflammatory = pmax(Family.History.of.Arthritis
                                ,Family.History.of.Severe.Allergies),
         FH.Metabolic = pmax(Family.History.of.Diabetes
                             ,Family.History.of.Thyroid.Disorder),
         FH.Respiratory = pmax(Family.History.of.Asthma
                               ,Family.History.of.Lung.Respiratory.Disease),
         FH.Other = pmax(Family.History.of.Anaesthetic.Complications
                         ,Family.History.of.Kidney.Renal.Disease
                         ,Family.History.of.Migraines
                         ,Family.History.of.Osteoporosis
                         ,Family.History.of.Other.Medical.Problems
                         ,Family.History.of.Seizures
                         ,Family.History.of.Weight.Disorder
                         ,Family.History.of.Alcoholism
                         ,Family.History.of.Anxiety
                         ,Family.History.of.Depression
                         ,Family.History.of.Psychiatric.Care))%>%
  select(-starts_with("Family.History"),
         -c(No.Known.Family.History,No.Known.Relative))

d_final$State<-ifelse(d_final$State!="IL"&d_final$State!='IN',"Other",d_final$State)
d_final$EmpStatus<-ifelse(d_final$EmpStatus%in%c("Full-time","Part-time","Self-employed"),"Employed",d_final$EmpStatus)
d_final$EmpStatus<-ifelse(d_final$EmpStatus=="Student","Other",d_final$EmpStatus)
d_final$Race<-ifelse(d_final$Race=="White","White","Other")
d_final$MaritalStatus<-ifelse(d_final$MaritalStatus%in%c("Widow","Widower"),"Widow(er)",d_final$MaritalStatus)
FCareKeep<-c("Blue Cross","Commercial","Managed Care","Medicaid","Medicare","Work Comp")
d_final$FinancialClass<-ifelse(d_final$FinancialClass%in%FCareKeep,d_final$FinancialClass,"Other")
d_final$height<-ifelse(d_final$bmi>100|d_final$bmi<10,0,d_final$height)
d_final$weight<-ifelse(d_final$bmi>100|d_final$bmi<10,0,d_final$weight)
d_final$bmi<-d_final$weight/(d_final$height^2)*703
d_final$height<-ifelse(d_final$height==0,-1,d_final$height)
d_final$weight<-ifelse(d_final$weight==0,-1,d_final$weight)
d_final$bmi<-ifelse(is.na(d_final$bmi),-1,d_final$bmi)
d_final$Description<-ifelse(d_final$Description=="Arthroplasty, knee, condyle and plateau; medial AND lateral compartments, total knee arthroplasty","Total Knee","Partial Knee")

d_final <- d_final[,c(13, 1:12, 14:30)]
names(d_final)[1] <- "y"

dummies <- dummyVars(y ~ ., data = d_final)            # create dummies for Xs
ex <- data.frame(predict(dummies, newdata = d_final))  # actually creates the dummies
names(ex) <- gsub("\\.", "", names(ex))          # removes dots from col names
d1 <- cbind(d_final$y, ex)                              # combine target var with Xs
names(d1)[1] <- "y"                               # name target var 'y'
rm(dummies, ex)
d2<-d1[,-1]
d3 <- d2[,-c(2,4,13,19,21,22,8,11,18,31,37)]

rf_df <- predict(rf,newdata = d3,type='prob')[,1]


#get last and birthday for prediction dataframe
patient_df1<-df%>%
  rename(EmpStatus=description_emp_status_mid,
         Race = description_code_patient_race_mid,
         PlaceOfService=description_place_of_service_mid)%>%
  mutate(State=ifelse(State=="IL.","IL",State),
         Race=ifelse(max.b.PatientRaceMId.==42445,"Unspecified",Race),
         Race=ifelse(max.b.PatientRaceMId.==62785,"White",Race),
         EmpStatus=ifelse(is.na(EmpStatus),"Unknown",EmpStatus),
         MaritalStatus=ifelse(MaritalStatus=="Seperated","Separated",MaritalStatus),
         Description = ifelse(Description=="arthroplasty, knee, condyle and plateau; medial AND lateral compartments, total knee arthroplasty","Arthroplasty, knee, condyle and plateau; medial AND lateral compartments, total knee arthroplasty",Description),
         Description = ifelse(Description=="rtArthroplasty, knee, condyle and plateau; medial OR lateral compartment","Arthroplasty, knee, condyle and plateau; medial OR lateral compartment",Description))%>%
  select(-c(PatientProfileId, PatientId, PId, EmpStatusMId, SSN, ReferenceSourceMId, PatientSameAsGuarantor, 
            GuarantorId, PatientRelationToGuarantorMId, AllocationSetId, DoctorId, FacilityId.,FacilityId, 
            BenefitAssignmentMId, PrivacyPolicyAcknowledgementMId, ContactByMId, 
            SensitiveChart, LocationId, lupd, masterlock, visdocnum, pstatus, InTrigger, LanguageId, 
            HasPatientAccess, description_marital_status_mid, description_financial_class_mid, description_patient_status_mid, 
            description_modifier_1_mid, CPTCode,max.b.PatientRaceMId.,
            Age,CurrentCarrier,Patient_Status,City,Zip,
            Family.History.of.Birth.Defects,
            Family.History.of.Endometriosis,
            Family.History.of.Growth.Development.Disorder,
            Family.History.of.Headaches))%>%
  dplyr::filter(PlaceOfService!="Office",modifier!="surgery")

d_final1<-patient_df1%>%
  mutate(FH.Blood = pmax(Family.History.of.Anemia
                         ,Family.History.of.Bleeding.Disease
                         ,Family.History.of.High.Cholesterol
                         ,Family.History.of.Hypertension),
         FH.Cancer = pmax(Family.History.of.Breast.Cancer
                          ,Family.History.of.Cervical.Cancer
                          ,Family.History.of.Colon.Cancer
                          ,Family.History.of.Lung.Cancer
                          ,Family.History.of.Melanoma
                          ,Family.History.of.Other.Cancer
                          ,Family.History.of.Ovarian.Cancer
                          ,Family.History.of.Uterine.Cancer),
         FH.Cardio.Disease = pmax(Family.History.of.Angina
                                  ,Family.History.of.Coronary.Heart.Disease
                                  ,Family.History.of.Heart.Disease
                                  ,Family.History.of.Stroke.CVA),
         FH.Inflammatory = pmax(Family.History.of.Arthritis
                                ,Family.History.of.Severe.Allergies),
         FH.Metabolic = pmax(Family.History.of.Diabetes
                             ,Family.History.of.Thyroid.Disorder),
         FH.Respiratory = pmax(Family.History.of.Asthma
                               ,Family.History.of.Lung.Respiratory.Disease),
         FH.Other = pmax(Family.History.of.Anaesthetic.Complications
                         ,Family.History.of.Kidney.Renal.Disease
                         ,Family.History.of.Migraines
                         ,Family.History.of.Osteoporosis
                         ,Family.History.of.Other.Medical.Problems
                         ,Family.History.of.Seizures
                         ,Family.History.of.Weight.Disorder
                         ,Family.History.of.Alcoholism
                         ,Family.History.of.Anxiety
                         ,Family.History.of.Depression
                         ,Family.History.of.Psychiatric.Care))%>%
  select(-starts_with("Family.History"),
         -c(No.Known.Family.History,No.Known.Relative))

d_final1$State<-ifelse(d_final1$State!="IL"&d_final1$State!='IN',"Other",d_final1$State)
d_final1$EmpStatus<-ifelse(d_final1$EmpStatus%in%c("Full-time","Part-time","Self-employed"),"Employed",d_final1$EmpStatus)
d_final1$EmpStatus<-ifelse(d_final1$EmpStatus=="Student","Other",d_final1$EmpStatus)
d_final1$Race<-ifelse(d_final1$Race=="White","White","Other")
d_final1$MaritalStatus<-ifelse(d_final1$MaritalStatus%in%c("Widow","Widower"),"Widow(er)",d_final1$MaritalStatus)
FCareKeep<-c("Blue Cross","Commercial","Managed Care","Medicaid","Medicare","Work Comp")
d_final1$FinancialClass<-ifelse(d_final1$FinancialClass%in%FCareKeep,d_final1$FinancialClass,"Other")
d_final1$height<-ifelse(d_final1$bmi>100|d_final1$bmi<10,0,d_final1$height)
d_final1$weight<-ifelse(d_final1$bmi>100|d_final1$bmi<10,0,d_final1$weight)
d_final1$bmi<-d_final1$weight/(d_final1$height^2)*703
d_final1$height<-ifelse(d_final1$height==0,-1,d_final1$height)
d_final1$weight<-ifelse(d_final1$weight==0,-1,d_final1$weight)
d_final1$bmi<-ifelse(is.na(d_final1$bmi),-1,d_final1$bmi)
d_final1$Description<-ifelse(d_final1$Description=="Arthroplasty, knee, condyle and plateau; medial AND lateral compartments, total knee arthroplasty","Total Knee","Partial Knee")

pred_yes<-as.data.frame(cbind(as.character(d_final1$Last),as.character(d_final1$Birthdate),rf_df))
names(pred_yes) <-c("Last","Birthdate","prob")




patient<-17
# Fix

Prediction<-function(patientDF){
  d_final<-patientDF%>%
    mutate(FH.Blood = pmax(Family.History.of.Anemia
                           ,Family.History.of.Bleeding.Disease
                           ,Family.History.of.High.Cholesterol
                           ,Family.History.of.Hypertension),
           FH.Cancer = pmax(Family.History.of.Breast.Cancer
                            ,Family.History.of.Cervical.Cancer
                            ,Family.History.of.Colon.Cancer
                            ,Family.History.of.Lung.Cancer
                            ,Family.History.of.Melanoma
                            ,Family.History.of.Other.Cancer
                            ,Family.History.of.Ovarian.Cancer
                            ,Family.History.of.Uterine.Cancer),
           FH.Cardio.Disease = pmax(Family.History.of.Angina
                                    ,Family.History.of.Coronary.Heart.Disease
                                    ,Family.History.of.Heart.Disease
                                    ,Family.History.of.Stroke.CVA),
           FH.Inflammatory = pmax(Family.History.of.Arthritis
                                  ,Family.History.of.Severe.Allergies),
           FH.Metabolic = pmax(Family.History.of.Diabetes
                               ,Family.History.of.Thyroid.Disorder),
           FH.Respiratory = pmax(Family.History.of.Asthma
                                 ,Family.History.of.Lung.Respiratory.Disease),
           FH.Other = pmax(Family.History.of.Anaesthetic.Complications
                           ,Family.History.of.Kidney.Renal.Disease
                           ,Family.History.of.Migraines
                           ,Family.History.of.Osteoporosis
                           ,Family.History.of.Other.Medical.Problems
                           ,Family.History.of.Seizures
                           ,Family.History.of.Weight.Disorder
                           ,Family.History.of.Alcoholism
                           ,Family.History.of.Anxiety
                           ,Family.History.of.Depression
                           ,Family.History.of.Psychiatric.Care))%>%
    select(-starts_with("Family.History"),
           -c(No.Known.Family.History,No.Known.Relative))
  
  d_final$State<-ifelse(d_final$State!="IL"&d_final$State!='IN',"Other",d_final$State)
  d_final$EmpStatus<-ifelse(d_final$EmpStatus%in%c("Full-time","Part-time","Self-employed"),"Employed",d_final$EmpStatus)
  d_final$EmpStatus<-ifelse(d_final$EmpStatus=="Student","Other",d_final$EmpStatus)
  d_final$Race<-ifelse(d_final$Race=="White","White","Other")
  d_final$MaritalStatus<-ifelse(d_final$MaritalStatus%in%c("Widow","Widower"),"Widow(er)",d_final$MaritalStatus)
  FCareKeep<-c("Blue Cross","Commercial","Managed Care","Medicaid","Medicare","Work Comp")
  d_final$FinancialClass<-ifelse(d_final$FinancialClass%in%FCareKeep,d_final$FinancialClass,"Other")
  d_final$height<-ifelse(d_final$bmi>100|d_final$bmi<10,0,d_final$height)
  d_final$weight<-ifelse(d_final$bmi>100|d_final$bmi<10,0,d_final$weight)
  d_final$bmi<-d_final$weight/(d_final$height^2)*703
  d_final$height<-ifelse(d_final$height==0,-1,d_final$height)
  d_final$weight<-ifelse(d_final$weight==0,-1,d_final$weight)
  d_final$bmi<-ifelse(is.na(d_final$bmi),-1,d_final$bmi)
  d_final$Description<-ifelse(d_final$Description=="Arthroplasty, knee, condyle and plateau; medial AND lateral compartments, total knee arthroplasty","Total Knee","Partial Knee")

  r<-7
  StockDF<-data.frame(
    y=rep("Stock",n=r),
    Description=c("Total Knee","Partial Knee","Partial Knee","Partial Knee","Partial Knee","Partial Knee","Partial Knee"),
    PlaceOfService=c("Inpatient Hospital","Outpatient Hospital","Outpatient Hospital","Outpatient Hospital","Outpatient Hospital","Outpatient Hospital","Outpatient Hospital"),
    modifier=c("Right","Left","Bilateral Procedure","Bilateral Procedure","Bilateral Procedure","Bilateral Procedure","Bilateral Procedure"),
    State=c("IL","IN","Other","Other","Other","Other","Other"),
    Se = c("M","F","F","F","F","F","F"),
    EmpStatus=c("Retired","Employed","Other", "Unknown","Unemployed","Unemployed","Unemployed"),
    Race=c("White","Other","Other","Other","Other","Other","Other"),
    height = rep(-1,n=r),
    weight = rep(-1,n=r),
    age = rep(-1,n=r),
    bmi = rep(-1,n=r),
    FinancialClass = c("Medicare","Blue Cross","Managed Care","Work Comp","Other","Commercial","Medicaid"),
    MaritalStatus=c("Married","Widow(er)","Divorced","Single","Separated","Unknown","Unknown"),
    Obseity=rep(0,n=r),
    Smoke=rep(0,n=r),
    HighBloodPressure=rep(0,n=r),
    Diabetes=rep(0,n=r),
    osteoporosis=rep(0,n=r),
    COUNT.DISTINCT.CasesId.=rep(0,n=r),
    MAX.WorkersComp.=rep(0,n=r),
    COUNT.DISTINCT.AppointmentsId.=rep(0,n=r),
    FH.Blood=rep(0,n=r),
    FH.Cancer=rep(0,n=r),
    FH.Cardio.Disease=rep(0,n=r),
    FH.Inflammatory=rep(0,n=r),
    FH.Metabolic=rep(0,n=r),
    FH.Respiratory=rep(0,n=r),
    FH.Other=rep(0,n=r)
  )
  d_final$Se<-ifelse(d_final$Se=="Male","M","F")
  d_final$MAX.WorkersComp.<-ifelse(d_final$MAX.WorkersComp.=="Yes",1,0)
  d_final<-rbind(StockDF,d_final)
  binary<-c("Smoke","HighBloodPressure","Diabetes",
            "osteoporosis","MAX.WorkersComp.",
            "FH.Blood","FH.Cancer","FH.Cardio.Disease",
            "FH.Inflammatory","FH.Metabolic",
            "FH.Respiratory","FH.Other")
  for(col in names(d_final)){
    if(col %in% binary){
      d_final[,col]<-as.numeric(d_final[,col])
    }
  }
  
  dummies <- dummyVars(y ~ ., data = d_final)            # create dummies for Xs
  ex <- data.frame(predict(dummies, newdata = d_final))  # actually creates the dummies
  names(ex) <- gsub("\\.", "", names(ex))          # removes dots from col names
  d1 <- cbind(d_final$y, ex)                              # combine target var with Xs
  names(d1)[1] <- "y"                               # name target var 'y'
  rm(dummies, ex)
  names(d1)
  d2 <- d1[,-c(2,4,13,19,21,22,8,11,18,31,37)]
  prob <- predict(rf, newdata=d2[8,2:41],type = "prob")[,2]
  return(prob*100)
}

# Prediction(CurrentPatient)
# 
#Description<-patient_df[patient,2]                       
# PlaceOfService<-patient_df[patient,3]                         
# modifier<-patient_df[patient,4]                                  
# State<-patient_df[patient,5]                                    
# Se<-patient_df[patient,6]                                      
# EmpStatus<-patient_df[patient,7]                                  
# Race<-patient_df[patient,8]                                       
# height<-patient_df[patient,9]                                     
# weight<-patient_df[patient,10]-10 #Here is the change                                    
# age<-patient_df[patient,11]                                        
# bmi<-patient_df[patient,12]                                        
# FinancialClass<-patient_df[patient,16]                             
# MaritalStatus<-patient_df[patient,17]                              
# Obseity<-patient_df[patient,18]                                    
# Smoke<-patient_df[patient,19]                                      
# HighBloodPressure<-patient_df[patient,20]                          
# Diabetes<-patient_df[patient,21]                                  
# osteoporosis<-patient_df[patient,22]                               
# COUNT.DISTINCT.CasesId.<-patient_df[patient,23]                    
# MAX.WorkersComp.<-patient_df[patient,24]                           
# COUNT.DISTINCT.AppointmentsId.<-patient_df[patient,25]            
# Family.History.of.Alcoholism<-patient_df[patient,26]              
# Family.History.of.Anaesthetic.Complications<-patient_df[patient,27]
# Family.History.of.Anemia<-patient_df[patient,28]                  
# Family.History.of.Angina<-patient_df[patient,29]                  
# Family.History.of.Anxiety<-patient_df[patient,30]                 
# Family.History.of.Arthritis<-patient_df[patient,31]                
# Family.History.of.Asthma<-patient_df[patient,32]                  
# Family.History.of.Bleeding.Disease<-patient_df[patient,33]         
# Family.History.of.Breast.Cancer<-patient_df[patient,34]            
# Family.History.of.Cervical.Cancer<-patient_df[patient,35]          
# Family.History.of.Colon.Cancer<-patient_df[patient,36]            
# Family.History.of.Coronary.Heart.Disease<-patient_df[patient,37]   
# Family.History.of.Depression<-patient_df[patient,38]               
# Family.History.of.Diabetes<-patient_df[patient,39]                 
# Family.History.of.Heart.Disease<-patient_df[patient,40]           
# Family.History.of.High.Cholesterol<-patient_df[patient,41]         
# Family.History.of.Hypertension<-patient_df[patient,42]            
# Family.History.of.Kidney.Renal.Disease<-patient_df[patient,43]    
# Family.History.of.Lung.Cancer<-patient_df[patient,44]             
# Family.History.of.Lung.Respiratory.Disease<-patient_df[patient,45] 
# Family.History.of.Melanoma<-patient_df[patient,46]                 
# Family.History.of.Migraines<-patient_df[patient,47]                
# Family.History.of.Osteoporosis<-patient_df[patient,48]             
# Family.History.of.Other.Cancer<-patient_df[patient,49]             
# Family.History.of.Other.Medical.Problems<-patient_df[patient,50]   
# Family.History.of.Ovarian.Cancer<-patient_df[patient,51]           
# Family.History.of.Psychiatric.Care<-patient_df[patient,52]         
# Family.History.of.Seizures<-patient_df[patient,53]                 
# Family.History.of.Severe.Allergies<-patient_df[patient,54]         
# Family.History.of.Stroke.CVA<-patient_df[patient,55]               
# Family.History.of.Thyroid.Disorder<-patient_df[patient,56]         
# Family.History.of.Uterine.Cancer<-patient_df[patient,57]           
# Family.History.of.Weight.Disorder<-patient_df[patient,58]          
# No.Known.Family.History<-patient_df[patient,59]                    
# No.Known.Relative<-patient_df[patient,60]
# 
# ComparePatient<-data.frame(
#   y="Patient",
#   Description,                       
#   PlaceOfService,                         
#   modifier,                                  
#   State,                                    
#   Se,                                      
#   EmpStatus,                                  
#   Race,                                       
#   height,                                     
#   weight,                                     
#   age,                                        
#   bmi,                                       
#   FinancialClass,                            
#   MaritalStatus,                              
#   Obseity,                                   
#   Smoke,                                      
#   HighBloodPressure,                          
#   Diabetes,                                  
#   osteoporosis,                               
#   COUNT.DISTINCT.CasesId.,                   
#   MAX.WorkersComp.,                          
#   COUNT.DISTINCT.AppointmentsId.,            
#   Family.History.of.Alcoholism,            
#   Family.History.of.Anaesthetic.Complications,
#   Family.History.of.Anemia,                  
#   Family.History.of.Angina,                  
#   Family.History.of.Anxiety,                 
#   Family.History.of.Arthritis,               
#   Family.History.of.Asthma,                  
#   Family.History.of.Bleeding.Disease,         
#   Family.History.of.Breast.Cancer,            
#   Family.History.of.Cervical.Cancer,          
#   Family.History.of.Colon.Cancer,           
#   Family.History.of.Coronary.Heart.Disease,   
#   Family.History.of.Depression,               
#   Family.History.of.Diabetes,                 
#   Family.History.of.Heart.Disease,           
#   Family.History.of.High.Cholesterol,         
#   Family.History.of.Hypertension,            
#   Family.History.of.Kidney.Renal.Disease,   
#   Family.History.of.Lung.Cancer,             
#   Family.History.of.Lung.Respiratory.Disease, 
#   Family.History.of.Melanoma,                 
#   Family.History.of.Migraines,                
#   Family.History.of.Osteoporosis,             
#   Family.History.of.Other.Cancer,            
#   Family.History.of.Other.Medical.Problems,   
#   Family.History.of.Ovarian.Cancer,           
#   Family.History.of.Psychiatric.Care,         
#   Family.History.of.Seizures,                
#   Family.History.of.Severe.Allergies,         
#   Family.History.of.Stroke.CVA,               
#   Family.History.of.Thyroid.Disorder,         
#   Family.History.of.Uterine.Cancer,           
#   Family.History.of.Weight.Disorder,          
#   No.Known.Family.History,                    
#   No.Known.Relative
# )
# 
# predict_current <- Prediction(CurrentPatient)
# predict_compare <- Prediction(ComparePatient)
  
##############Set up theme
#Data clean get rid of office, surgery
ui <- fluidPage(
  navbarPage("Company Name", theme = bs_theme(version = 4, bootswatch = "cerulean"),
             mainPanel(img(src = "shinyphoto.png", align = "center", height = 300, width = 1375)),     
             mainPanel(h2(strong("Welcome")),
                       p(strong("In order to make the surgery go as smoothly as possible, we ask that 
                 patients take a quick survey to assess the chances of post-surgical complications.
                 In addition, if you are at higher risk for complications like revision surgery, 
                 we will provide some suggestions to lower your risk of follow-up problems.")), 
                       p(em("In the boxes below, please fill in your information.")), 
                       textInput("lastname", "What's your last name?"),
                       dateInput("birthday", "What's your birthday? Please format as YYYY-MM-01 with the day of your birthday as the first of the month."), 
                       actionButton("submit_data", "Click here to see your information", class = "btn-info"),
            mainPanel(DT::dataTableOutput("table1", width = 900)),
            mainPanel(selectInput("correct", "Is the information displayed in the table correct? If the information is 
                                  incorrect, please select No, wait 5 seconds, and then scroll down. If the information is
                                  correct, please select Yes.", choices = c("", "Yes", "No")), 
                      uiOutput("yescorrect")),
                      uiOutput("recommendations"),
                      uiOutput("survey"),
                      
)
)
)


#############Set up server

server <- function(input, output) {
  
  
  data<- eventReactive(input$submit_data,{
    subset(patient_df_table, Last == input$lastname & Birthdate == as.character(input$birthday),
           select = Last:No.Known.Relative)
  })
  
  
  output$table1 <- DT::renderDataTable({
    dfoutput <- data()
    head(dfoutput,1)
  },
  options = list(scrollX = TRUE)
  )
  
  
  df3<-reactive({subset(pred_yes, Last == as.character(input$lastname) & Birthdate == as.character(input$birthday),
                        select = Last:prob)})
  
  
  output$yescorrect<-renderUI({
    if (input$correct == "Yes") {
      prob_yes <-head(round(as.numeric(as.character(df3()$prob)),2),1)
      print(paste("Based on your survey results, we believe that there is a",prob_yes*100," % chance of complications"))
    }
    
  })
  
  output$recommendations <- renderUI({
    if(input$correct == "Yes"){
      fluidRow(
        column(12,
               h5(strong("We believe that there are some factors playing a role in increased chances in unsuccessful surgery where unsuccessful means either the patient experiences direct complications
                            from the surgery or a revision surgery is needed.")),
               p("First, differences between surgeries can play a role in increased chances of an unsuccessful surgery. Specifically, having surgery at an outpatient hospital decreases risk
                            compared to having surgery at an inpatient hospital."),
               p("Next, some people have uncontrollable factors that also could increase your chances of having an unsuccessful surgery. 
                          One of them is gender. Females have slightly lower success rates than men. Being retired also improved surgical outcomes, probably
                          due to being able to rest and recover longer than those who work a job. Age was also a critical factor where the older you are, the better the chance
                          of a successful surgery."), 
               p("Personal and family medical history also plays a factor. Individuals with BMI greater than 30, high blood pressure, diabetes, and 
                     osteoporosis are less likely to have a successful surgery. In addition, inflammatory diseases in the family history tend to increase chances of complications."),
               h5(strong("With that said, we recommend several potential beneficial steps to reduce the chances of an unsuccessful surgery.")),
               p("Resting is a critical part of recovery. For those employed, we suggest taking at least a week off of significant movement. Thus, it would aid in the recovery process if you took some extra days off or worked remotely during that time."),
               p("Weight is a strong indicator of how successful a surgery will go. We suggest losing enough weight to get you BMI under 30.0 to increase your chances of success."),
               p("Having someone to help take care of you during the recovery period is incredibly beneficial in the recovery process.
                  We suggest having a friend, coworker, relative or spouse stop by throughout the day for the next week to improve recovery outcomes.")    ))}
  })
  
    output$survey <- renderUI(
    if(input$correct == "No") {
      fluidRow(
        column(12,
              h4("Sorry our database was not up to date."),
              p("Please fill out the survey below to the best of your abilities. For the family history section, select 0 if No, select 1 if yes."), 
              selectInput("surgery", "Please pick the description that fits your upcoming surgery", choices = unique(patient_df_table$SurgeryDescription)),
              selectInput("inorout", "Will your surgery be inpatient or outpatient?", choices = unique(patient_df_table$InPatientOrOutPatient)),
              selectInput("whichknee", "Which knee will be operated on?", choices = unique(patient_df_table$WhichKnee)),
              selectInput("workcomp", "Did this injury happen during your job? Specifically, is this surgery covered by Workers Compensation?", choices = unique(patient_df_table$WorkersComp)),
              selectInput("sex", "What's your sex?", choices = c("Female", "Male", "Prefer not to respond")), 
              selectInput("race", "Please select your race.", choices = unique(patient_df_table$Race)),
              selectInput("empstatus", "Please select the category that best fits your current employment situation.", choices = unique(patient_df_table$EmploymentStatus)),
              selectInput("state", "What state do you live in?", choices = unique(patient_df_table$State)),
              sliderInput("age", "What's your age?", value = 50, min = 0, max = 100, step = 1), 
              sliderInput("height", "Please input your height (in inches)", value = 66, min = 36, max = 96, step = 1), 
              sliderInput("weight", "Please input your weight (in lbs).", value = 200, min = 50, max = 500, step = 1),
              selectInput("insurance", "Which insurance do you hold?", choices = unique(patient_df_table$FinancialClass)), 
              selectInput("married", "Are you Married, Single, etc.?", choices = unique(patient_df_table$MaritalStatus)),
              selectInput("smoke", "Do you smoke regularly?", choices = c("0", "1")), 
              selectInput("highbp", "Do you have a history of high blood pressure?", choices = c("0", "1")),
              selectInput("diabetes", "Do you have a history of diabetes?", choices = c("0", "1")),
              selectInput("osteo", "Do you have a history of osteoporosis?", choices = c("0", "1")),
              selectInput("fh_alc", "Do you or anyone in your family have a history of alcoholism?", choices = c("0", "1")),
              selectInput("fh_anast", "Do you or anyone in your family have a history of anaesthetic complications?", choices = c("0", "1")),
              selectInput("fh_anemia", "Do you or anyone in your family have a history of anemia?", choices = c("0", "1")),
              selectInput("fh_angina", "Do you or anyone in your family have a history of angina?", choices = c("0", "1")),
              selectInput("fh_anx", "Do you or anyone in your family have a history of anxiety?", choices = c("0", "1")),
              selectInput("fh_arth", "Do you or anyone in your family have a history of arthritis?", choices = c("0", "1")),
              selectInput("fh_asthma", "Do you or anyone in your family have a history of asthma?", choices = c("0", "1")),
              selectInput("fh_birth_def", "Do you or anyone in your family have a history of birth defects?", choices = c("0", "1")),
              selectInput("fh_bleed", "Do you or anyone in your family have a history of bleeding disease?", choices = c("0", "1")),
              selectInput("fh_breast_canc", "Do you or anyone in your family have a history of breast cancer?",choices = c("0", "1")),
              selectInput("fh_cerv_canc", "Do you or anyone in your family have a history of cervical cancer?", choices = c("0", "1")),
              selectInput("fh_col_canc", "Do you or anyone in your family have a history of colon cancer?", choices = c("0", "1")),
              selectInput("fh_heart", "Do you or anyone in your family have a history of coronary heart disease?", choices = c("0", "1")),
              selectInput("fh_dep", "Do you or anyone in your family have a history of depression?", choices = c("0", "1")),
              selectInput("fh_diab", "Do you or anyone in your family have a history of diabetes?", choices = c("0", "1")),
              selectInput("fh_endo", "Do you or anyone in your family have a history of endometriosis?", choices = c("0", "1")),
              selectInput("fh_gdd", "Do you or anyone in your family have a history of growth development disorder?", choices = c("0", "1")),
              selectInput("fh_head", "Do you or anyone in your family have a history of headaches?", choices = c("0", "1")),
              selectInput("fh_heart_dis", "Do you or anyone in your family have a history of heart disease?", choices = c("0", "1")),
              selectInput("fh_high_chol", "Do you or anyone in your family have a history of high cholesteral?", choices = c("0", "1")),
              selectInput("fh_hyperten", "Do you or anyone in your family have a history of hypertension?", choices = c("0", "1")),
              selectInput("fh_renal", "Do you or anyone in your family have a history of kidney or renal disease?",choices = c("0", "1")),
              selectInput("fh_lung_canc", "Do you or anyone in your family have a history of lung cancer?", choices = c("0", "1")),
              selectInput("fh_resp_dis", "Do you or anyone in your family have a history of lung or respiratory disease?", choices = c("0", "1")),
              selectInput("fh_mel", "Do you or anyone in your family have a history of melanoma?", choices = c("0", "1")),
              selectInput("fh_mig", "Do you or anyone in your family have a history of migraines?",choices = c("0", "1")),
              selectInput("fh_osteo", "Do you or anyone in your family have a history of osteoporosis?",choices = c("0", "1")),
              selectInput("fh_ovar_canc", "Do you or anyone in your family have a history of ovarian cancer?", choices = c("0", "1")),
              selectInput("fh_psych", "Do you or anyone in your family have a history of needing psychiatric care?", choices = c("0", "1")),
              selectInput("fh_seize", "Do you or anyone in your family have a history of seizures?", choices = c("0", "1")),
              selectInput("fh_allergy", "Do you or anyone in your family have a history of severe allergies?", choices = c("0", "1")),
              selectInput("fh_stroke", "Do you or anyone in your family have a history of stroke?", choices = c("0", "1")),
              selectInput("fh_thyroid", "Do you or anyone in your family have a history of thyroid disorder?", choices = c("0", "1")),
              selectInput("fh_uterine", "Do you or anyone in your family have a history of uterine cancer?", choices = c("0", "1")),
              selectInput("fh_weight", "Do you or anyone in your family have a history of weight disorders?", choices = c("0", "1")),
              selectInput("fh_other_canc", "Do you or anyone in your family have a history of cancer that has not been mentioned previously?", choices = c("0", "1")),
              selectInput("fh_other", "Do you or anyone in your family have any other medical problems that has not been mentioned previously?", choices = c("0", "1")),
              actionButton("submit3", "Click here to submit your information", align = "center", class = "btn-info"), 
              DT::dataTableOutput("table2", width = 900),
              uiOutput("survey_new"), 
              uiOutput("recommendations2")
            ))
    }
    )
   
    bmi <- reactive({as.numeric(as.character(input$weight))/as.numeric(as.character((input$height)))^2 * 703})
    obesity <- reactive({ifelse(bmi() > 30, 1, 0)})
     
    data_new <- eventReactive(input$submit3,{
      data.frame(
                y="Patient",
                Description=input$surgery,                       
                PlaceOfService=input$inorout,                         
                modifier=input$whichknee,                                  
                State=input$state,                                    
                Se=input$sex,                                   
                EmpStatus=input$empstatus,                               
                Race=input$race,                                
                height=input$height,                                   
                weight=input$weight,                                   
                age=input$age,                                    
                bmi=bmi(),                                
                FinancialClass=input$insurance,                        
                MaritalStatus=input$married,                           
                Obseity=obesity(),                              
                Smoke=input$smoke,                                   
                HighBloodPressure=input$highbp,                         
                Diabetes=input$diabetes,                              
                osteoporosis=input$osteo,                            
                COUNT.DISTINCT.CasesId.=mean(patient_df$COUNT.DISTINCT.CasesId.) ,                  
                MAX.WorkersComp.=input$workcomp,                         
                COUNT.DISTINCT.AppointmentsId.=mean(patient_df$COUNT.DISTINCT.AppointmentsId.),          
                Family.History.of.Alcoholism=input$fh_alc,            
                Family.History.of.Anaesthetic.Complications=input$fh_anast,
                Family.History.of.Anemia=input$fh_anemia ,               
                Family.History.of.Angina=input$fh_angina ,                
                Family.History.of.Anxiety=input$fh_anx ,               
                Family.History.of.Arthritis=input$fh_arth,             
                Family.History.of.Asthma=input$fh_asthma,               
                Family.History.of.Bleeding.Disease=input$fh_bleed,     
                Family.History.of.Breast.Cancer=input$fh_breast_canc,          
                Family.History.of.Cervical.Cancer=input$fh_cerv_canc,        
                Family.History.of.Colon.Cancer=input$fh_col_canc,         
                Family.History.of.Coronary.Heart.Disease=input$fh_heart,
                Family.History.of.Depression=input$fh_dep,          
                Family.History.of.Diabetes=input$fh_diab,              
                Family.History.of.Heart.Disease=input$fh_heart_dis,        
                Family.History.of.High.Cholesterol=input$fh_high_chol,       
                Family.History.of.Hypertension=input$fh_hyperten,          
                Family.History.of.Kidney.Renal.Disease=input$fh_renal,  
                Family.History.of.Lung.Cancer=input$fh_lung_canc ,          
                Family.History.of.Lung.Respiratory.Disease=input$fh_resp_dis,
                Family.History.of.Melanoma=input$fh_mel,            
                Family.History.of.Migraines=input$fh_mig,             
                Family.History.of.Osteoporosis=input$fh_osteo,       
                Family.History.of.Other.Cancer=input$fh_other_canc,       
                Family.History.of.Other.Medical.Problems=input$fh_other,
                Family.History.of.Ovarian.Cancer=input$fh_ovar_canc,       
                Family.History.of.Psychiatric.Care=input$fh_psych,      
                Family.History.of.Seizures=input$fh_seize,              
                Family.History.of.Severe.Allergies=input$fh_allergy,      
                Family.History.of.Stroke.CVA=input$fh_stroke,           
                Family.History.of.Thyroid.Disorder=input$fh_thyroid,       
                Family.History.of.Uterine.Cancer=input$fh_uterine,        
                Family.History.of.Weight.Disorder=input$fh_weight,        
                No.Known.Family.History=0,                 
                No.Known.Relative=0)})
      
      #test data frame result
      output$table2<- DT::renderDataTable({
      dfoutput1 <- data_new()
      head(dfoutput1,1)
      },
      options = list(scrollX = TRUE)
      )
        
      output$survey_new <-renderUI({
        prob_yes <-head(round(as.numeric(as.character(Prediction(data_new()))),2),1)
        print(paste("Based on your survey results, we believe that there is a",prob_yes," % chance of complications"))
      })
      
      output$recommendations2 <- renderUI({
          fluidRow(
            column(12,
                   h5(strong("We believe that there are some factors playing a role in increased chances in unsuccessful surgery where unsuccessful means either the patient experiences direct complications
                            from the surgery or a revision surgery is needed.")),
                   p("First, differences between surgeries can play a role in increased chances of an unsuccessful surgery. Specifically, having surgery at an outpatient hospital decreases risk
                            compared to having surgery at an inpatient hospital."),
                   p("Next, some people have uncontrollable factors that also could increase your chances of having an unsuccessful surgery. 
                          One of them is gender. Females have slightly lower success rates than men. Being retired also improved surgical outcomes, probably
                          due to being able to rest and recover longer than those who work a job. Age was also a critical factor where the older you are, the better the chance
                          of a successful surgery."), 
                   p("Personal and family medical history also plays a factor. Individuals with BMI greater than 30, high blood pressure, diabetes, and 
                     osteoporosis are less likely to have a successful surgery. In addition, inflammatory diseases in the family history tend to increase chances of complications."),
                   h5(strong("With that said, we recommend several potential beneficial steps to reduce the chances of an unsuccessful surgery.")),
                   p("Resting is a critical part of recovery. For those employed, we suggest taking at least a week off of significant movement. Thus, it would aid in the recovery process if you took some extra days off or worked remotely during that time."),
                   p("Weight is a strong indicator of how successful a surgery will go. We suggest losing enough weight to get you BMI under 30.0 to increase your chances of success."),
                   p("Having someone to help take care of you during the recovery period is incredibly beneficial in the recovery process.
                  We suggest having a friend, coworker, relative or spouse stop by throughout the day for the next week to improve recovery outcomes.")
            ))}
      )
}

#######Run app

shinyApp(ui = ui, server = server)





