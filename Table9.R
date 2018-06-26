# Table 9
# Replica + DML


library(foreign);
library(quantreg);
library(mnormt);
library(gbm);
library(glmnet);
library(MASS);
library(rpart);
library(doParallel)
library(sandwich);
library(hdm);
library(randomForest);
library(nnet)
library(matrixStats)
library(quadprog)
library(xtable)
library(ivmodel)
#1. Replica in R

#baseline_controls:
#age 
#age2 
#male
#urban_dum 
#i.education 
#i.occupation 
#i.religion 
#i.living_conditions 
#district_ethnic_frac 
#frac_ethnicity_in_district 
#i.isocode"

#colonial_controls: 
#malaria_ecology 
#total_missions_area
#explorer_contact 
#railway_contact 
#cities_1400_dum 
#i.v30 
#v33


#@TABLE 3

# make Dx for each x regression in Table1
# prepare other additional control variables 

Y <- df$trust_neighbors
D=df$exports
D1= df$exports
D2= df$export_area
D3= df$export_pop
D4= df$ln_exports
D5= df$ln_export_area
D6= df$ln_export_pop
age = df$age
age2 = df$age2
male = df$male
urban_dum =df$urban_dum
district_ethnic_frac=df$district_ethnic_frac
frac_ethnicity_in_district=df$frac_ethnicity_in_district


#@ignore
#baline_controls=df[,c(1,2,5)]
#baline_controls=df[ , c("age","age2","male","urban_dum","district_ethnic_frac","frac_ethnicity_in_district")]
#baline_controls_plus_variables_not_in_dummyform = df[ , c("education","occupation","religion","living_conditions","isocode")]
Y_TN <- df$trust_neighbors
Y_TR <- df$trust_relatives
Y_TC <- df$trust_local_council
Y_tra <- df$intra_group_trust
Y_ter <- df$inter_group_trust
D=df$exports
D1= df$exports
D2= df$export_area
D3= df$export_pop
D4= df$ln_exports
D5= df$ln_export_area
D6= df$ln_export_pop

age = df$age
age2 = df$age2
male = df$male
urban_dum =df$urban_dum
district_ethnic_frac=df$district_ethnic_frac
frac_ethnicity_in_district=df$frac_ethnicity_in_district
baline_controls_plus_variables_not_in_dummyform = df[ , c("education","occupation","religion","living_conditions","isocode")]

education=df[ , c("education")]
education_fac= factor(df[ , c("education")])
occupation=df[ , c("occupation")]
occupation_fac=factor(df[ , c("occupation")])
religion=df[ , c("religion")]
religion_fac=factor(df[ , c("religion")])
living_conditions=df[ , c("living_conditions")]
living_conditions_fac=factor(df[ , c("living_conditions")])
isocode=df[ , c("isocode")]


# construct for those variables dummies ( in excel file those have one column but in Stata these are binary dummies ???)
#  i.education 
#  i.occupation 
#  i.religion 
#  i.living_conditions 
#  i.isocode




#"age age2 male urban_dum i.education i.occupation i.religion i.living_conditions district_ethnic_frac frac_ethnicity_in_district i.isocode"

#build Dummies for those variables like in Stata file:
baline_controls_plus_variables_not_in_dummyform = df[ , c("education","occupation","religion","living_conditions","isocode")]
education=df[ , c("education")]
education_fac= factor(df[ , c("education")])
occupation=df[ , c("occupation")]
occupation_fac=factor(df[ , c("occupation")])
religion=df[ , c("religion")]
religion_fac=factor(df[ , c("religion")])
living_conditions=df[ , c("living_conditions")]
living_conditions_fac=factor(df[ , c("living_conditions")])
isocode=df[ , c("isocode")]




#@ignore
#test_regression <- lm( Y~ D+ age+age2 +male +urban_dum+district_ethnic_frac+frac_ethnicity_in_district, data=df)
#summary(test_regression) 
# -0.00061982368 parameter deviates from ???0.00068
# try simplty with those regressors; FAIL
#test_regression <- lm( Y~ D+ age+age2 +male +urban_dum+district_ethnic_frac+frac_ethnicity_in_district+education+occupation+religion+living_conditions+isocode, data=df)
#summary(test_regression) 
# -0.00076440340 parameter deviates from ???0.00068
# works a bit; close to D 
#test_regression <- lm( Y~ D+ age+age2 +male +urban_dum+district_ethnic_frac+frac_ethnicity_in_district+education+occupation+religion+living_conditions+isocode, data=df)
#summary(test_regression) 
# -0.00076440340 parameter deviates from ???0.00068 exlucing several dummies


#@ignore
#create a dummy for each value of education occupation religion living_conditions and isode_code
#educ_0 <- as.numeric(education == 0)
#educ_1 <- as.numeric(education == 1)
#educ_2 <- as.numeric(education == 2)
#educ_3 <- as.numeric(education == 3)
#educ_4 <- as.numeric(education == 4)
#educ_5 <- as.numeric(education == 5)
#educ_6 <- as.numeric(education == 6)
#educ_7 <- as.numeric(education == 7)
#educ_8 <- as.numeric(education == 8)
#educ_9 <- as.numeric(education == 9)
#educ_0 +educ_1 +educ_2 +educ_3 +educ_4 +educ_5 +educ_6 +educ_7 +educ_8 +educ_9
#test_regression <- lm( Y~ D+ age+age2 +male +urban_dum+district_ethnic_frac+frac_ethnicity_in_district+educ_0 +educ_1 +educ_2 +educ_3 +educ_4 +educ_5 +educ_6 +educ_7 +educ_8 +educ_9 +occupation+religion+living_conditions+isocode, data=df)
#summary(test_regression) 

# additional controls

# colonial controls
# @ Stata   "malaria_ecology total_missions_area explorer_contact railway_contact cities_1400_dum i.v30 v33"

mal = df$malaria_ecology
mis=df$total_missions_area
ex_con=factor(df$explorer_contact)
rail=factor(df$railway_contact)
c1400_=factor(df$cities_1400_dum)
v30_fac= factor(df[ , c("v30")])
v33=factor(df$v33)


#anther control
intit_pop=df$ln_init_pop_density
intit_pop[is.na(intit_pop)] <- 1 #For instance

#distance
dist= df$distsea

#fishing
#dist_Saharan_node
#dist_Saharan_line if missing(`x')~=1,

fish=df$fishing
Sah_N=df$dist_Saharan_node
Sah_l=df$dist_Saharan_line
Sah_l[is.na(Sah_l)] <- 1 #For instance

#set 1 controls
#i.local_council_performance
#i.corrupt_local_council
#i.council_listen 
lcp=factor(df$local_council_performance)
clc=factor(df$corrupt_local_council)
cl=factor(df$council_listen)

lcp+clc+cl

#set 2 controls

#electricity_present
#piped_water_present
#sewage_present
#health_clinic_present
el_pre=factor(df$electricity_present)
pi_wa=factor(df$piped_water_present)
se_pre=factor(df$sewage_present)
hea_pre=factor(df$health_clinic_present)






#COL1
#/* Controlling for Performance & Corruption (FEs) */
#xi: cgmreg trust_local_council ln_export_area i.local_council_performance i.corrupt_local_council i.council_listen `baseline_controls' `colonial_controls' ln_init_pop_density, cluster(murdock_name district)
#6_1
test_regression1 <- lm( Y_TC~ D5+ age+age2 +male +urban_dum+district_ethnic_frac+frac_ethnicity_in_district+education_fac+occupation_fac+religion_fac+living_conditions_fac+isocode+mal+mis+ex_con+rail+c1400_+v30_fac+v33+intit_pop+lcp+clc+cl, data=df)
summary(test_regression1) 



#COL2
#/* Controlling for All - Add public goods */
#xi: cgmreg trust_local_council ln_export_area school_present electricity_present piped_water_present sewage_present health_clinic_present i.local_council_performance i.corrupt_local_council i.council_listen `baseline_controls' `colonial_controls' ln_init_pop_density, cluster(murdock_name district)
#6_2
test_regression2 <- lm( Y_TC~ D5+ age+age2 +male +urban_dum+district_ethnic_frac+frac_ethnicity_in_district+education_fac+occupation_fac+religion_fac+living_conditions_fac+isocode+mal+mis+ex_con+rail+c1400_+v30_fac+v33+intit_pop+lcp+clc+cl+el_pre+pi_wa+se_pre+hea_pre, data=df)
summary(test_regression2) 



#reg ln_export_area distsea `baseline_controls' `colonial_controls' ln_init_pop_density fishing dist_Saharan_node dist_Saharan_line if missing(`x')~=1, cluster(murdock_name)
#+fishing dist_Saharan_node dist_Saharan_line if missing(`x')~=1,
#Table 6_1
#ivreg(Y ~ X + W | W + Z, ... )
test_regression1 = ivreg( Y_TR~ D5+age+age2 +male +urban_dum+district_ethnic_frac+frac_ethnicity_in_district+education_fac+occupation_fac+religion_fac+living_conditions_fac+isocode+mal+mis+ex_con+rail+c1400_+v30_fac+v33+intit_pop1+fish+Sah_N+Sah_l
                          | dist+age+age2 +male +urban_dum+district_ethnic_frac+frac_ethnicity_in_district+education_fac+occupation_fac+religion_fac+living_conditions_fac+isocode+mal+mis+ex_con+rail+c1400_+v30_fac+v33+intit_pop1+fish+Sah_N+Sah_l,data = df)
summary(test_regression1)
summary(test_regression1, diagnostics=TRUE)

#@ignore
# Hausman
#first_stage <- lm(D5~dist+age+age2 +male +urban_dum+district_ethnic_frac+frac_ethnicity_in_district+education_fac+occupation_fac+religion_fac+living_conditions_fac+isocode+mal+mis+ex_con+rail+c1400_+v30_fac+v33+intit_pop,data=df)
# Hausman
#Hausman_reg <- lm(Y_TR~ D5+age+age2 +male +urban_dum+district_ethnic_frac+frac_ethnicity_in_district+education_fac+occupation_fac+religion_fac+living_conditions_fac+isocode+mal+mis+ex_con+rail+c1400_+v30_fac+v33+intit_pop+first_stage$residuals,data=df)
#print(summary(Hausman_reg))
#summary(test_regression1, vcov = sandwich, diagnostics = TRUE)

#Table 5_2
#ivreg(Y ~ X + W | W + Z, ... )
test_regression2 = ivreg( Y_TN~ D5+age+age2 +male +urban_dum+district_ethnic_frac+frac_ethnicity_in_district+education_fac+occupation_fac+religion_fac+living_conditions_fac+isocode+mal+mis+ex_con+rail+c1400_+v30_fac+v33+intit_pop1+fish+Sah_N+Sah_l
                          | dist+age+age2 +male +urban_dum+district_ethnic_frac+frac_ethnicity_in_district+education_fac+occupation_fac+religion_fac+living_conditions_fac+isocode+mal+mis+ex_con+rail+c1400_+v30_fac+v33+intit_pop1+fish+Sah_N+Sah_l,data = df)
summary(test_regression2)


#Table 5_3
test_regression3 = ivreg( Y_TC~ D5+age+age2 +male +urban_dum+district_ethnic_frac+frac_ethnicity_in_district+education_fac+occupation_fac+religion_fac+living_conditions_fac+isocode+mal+mis+ex_con+rail+c1400_+v30_fac+v33+intit_pop1+fish+Sah_N+Sah_l
                          | dist+age+age2 +male +urban_dum+district_ethnic_frac+frac_ethnicity_in_district+education_fac+occupation_fac+religion_fac+living_conditions_fac+isocode+mal+mis+ex_con+rail+c1400_+v30_fac+v33+intit_pop1+fish+Sah_N+Sah_l,data = df)
summary(test_regression3)


#Table 5_4
test_regression4 = ivreg( Y_tra~ D5+age+age2 +male +urban_dum+district_ethnic_frac+frac_ethnicity_in_district+education_fac+occupation_fac+religion_fac+living_conditions_fac+isocode+mal+mis+ex_con+rail+c1400_+v30_fac+v33+intit_pop1+fish+Sah_N+Sah_l
                          | dist+age+age2 +male +urban_dum+district_ethnic_frac+frac_ethnicity_in_district+education_fac+occupation_fac+religion_fac+living_conditions_fac+isocode+mal+mis+ex_con+rail+c1400_+v30_fac+v33+intit_pop1+fish+Sah_N+Sah_l,data = df)
summary(test_regression4)

#Table 5_5
test_regression5 = ivreg( Y_ter~ D5+age+age2 +male +urban_dum+district_ethnic_frac+frac_ethnicity_in_district+education_fac+occupation_fac+religion_fac+living_conditions_fac+isocode+mal+mis+ex_con+rail+c1400_+v30_fac+v33+intit_pop1+fish+Sah_N+Sah_l
                          | dist+age+age2 +male +urban_dum+district_ethnic_frac+frac_ethnicity_in_district+education_fac+occupation_fac+religion_fac+living_conditions_fac+isocode+mal+mis+ex_con+rail+c1400_+v30_fac+v33+intit_pop1+fish+Sah_N+Sah_l,data = df)
summary(test_regression5)

# final Table 1 with correct estimated coefficients and R^2
result <- matrix(0,3, 5)
result[1,1]=round(summary(test_regression1)$coefficients[2, 1], digits =5)
result[1,2]=round(summary(test_regression2)$coefficients[2, 1] , digits =3)
result[1,3]=round(summary(test_regression3)$coefficients[2, 1] , digits =3)
result[1,4]=round(summary(test_regression4)$coefficients[2, 1], digits =3)
result[1,5]=round(summary(test_regression5)$coefficients[2, 1], digits =3)

result[2,1]=round(summary(test_regression1)$r.squared, digits =2)
result[2,2]=round(summary(test_regression2)$r.squared , digits =2)
result[2,3]=round(summary(test_regression3)$r.squared , digits =2)
result[2,4]=round(summary(test_regression4)$r.squared , digits =2)
result[2,5]=round(summary(test_regression5)$r.squared , digits =2)


colnames(result) <- cbind("Trust of relatives","Trust of neighbors","Trust of local council","Intragroup trust","Intergroup council")
rownames(result) <- cbind(" estimated IV coefficient for D5", "R-squared", "DML coefficient")

result_table <- round(result, digits = 5)


for(i in 1:ncol(result_table)){
  for(j in seq(3,nrow(result_table),3)){
    
    result_table[j,i] <- paste(result_table[j,i], sep="")
    
  }
  for(j in seq(3,nrow(result_table),3)){
    
    result_table[j,i] <- paste( result_table[j,i], sep="")
    
  }
}
print(xtable(result_table, digits=4))
print(result_table)


###results differ slightly, why ???


