library(tidyverse)
library(ggrepel)
library(sapply)
library(scales)
library(reshape2)

getwd()
dat <- read.csv("interview_data.csv")
head(dat)
str(dat)

#Create column to differentiate between adult and child
dat <- dat %>% 
  mutate(Age_Description = ifelse(age < 18, "Child", ifelse(age >= 18, "Adult",0)))

dat %>% 
  select(geoid) %>%
  group_by(geoid) %>% 
  summarise(n = n(), percent = (n/(length(unique(dat$pk))))*100) %>% 
  arrange(desc(n))

##No Data in TM Categories


#Ages Listed
dat %>% 
  select(age) %>% 
  filter(age >= 18) %>% 
  group_by(age) %>% 
  summarise(n = n()) %>% 
  arrange(desc(n)) %>% 
  view()

#How does the living situation affect core needs?

dat %>% 
  select(cn_hs_1, cn_tr_1) %>% 
  group_by(cn_tr_1) %>% 
  filter(cn_hs_1 != "STDY_HSNG") %>% 
  summarise(n = n())

#Convert Scale to numeric
factorise <- factorise <- function(x) {
  case_when(x %in% c("NEVER", "NEVER_TRUE", "ZERO") ~ 0,
            x %in% c("RARELY", "SMTM_TRUE", "ONE") ~ 1,
            x %in% c("SOMETIMES", "OFTEN_TRUE", "TWO_OR_MORE") ~ 2,
            x %in% c("FRLY_OFTN") ~ 3,
            x %in% c("FREQUENTLY","ALWAYS") ~ 4)
  
}


#Apply scale to each row
dat$ii_2 <- sapply(dat$ii_2,factorise)
dat$sn_ff_2 <- sapply(dat$sn_ff_2 ,factorise)
dat$cn_sf_1 <- sapply(dat$cn_sf_1 ,factorise)
dat$cn_sf_2 <- sapply(dat$cn_sf_2 ,factorise)
dat$cn_sf_3 <- sapply(dat$cn_sf_3 ,factorise)
dat$cn_sf_4 <- sapply(dat$cn_sf_4 ,factorise)


###################### PLOT 1 Lonelyness related to Social Conflict in Children
dat %>% 
  filter(
    Age_Description == "Child",
    age > 5,
    ii_1 != "OTHER",
    sex != ""
    ) %>% 
  select(ii_1,
         sn_ff_2,
         cn_sf_1,
         cn_sf_2,
         cn_sf_3,
         cn_sf_4,
         age, 
         sex, 
         Race) %>% 
  mutate( sum = cn_sf_1 + 
           cn_sf_2 + 
           cn_sf_3 + 
           cn_sf_4) %>%
  ggplot(aes(x = sn_ff_2,
             y = sum, color = sex)) +
  geom_point() +
  geom_jitter(width = .2, 
              height = .15) +
  geom_smooth(method = lm, se = F) +
  labs(title = "Loneliness vs. Social Conflict",
       subtitle = "In Children under 18",
       x = "Lonliness", 
       y = "Social Conflict") +
  theme_bw()

###################### PLOT 2 Lonelyness related to Social Conflict in all Ages
dat %>% 
  filter(#Age_Description == "Child",
    age > 5,
    ii_1 != "OTHER",
    sex != "") %>% 
  select(ii_1,
         sn_ff_2,
         cn_sf_1,
         cn_sf_2,
         cn_sf_3,
         cn_sf_4,
         age, 
         sex,
         Race) %>% 
  mutate( sum = cn_sf_1 + 
            cn_sf_2 + 
            cn_sf_3 + 
            cn_sf_4) %>%
  ggplot(aes(x = sn_ff_2,
             y = sum, color = sex)) +
  geom_point() +
  geom_jitter(width = .2, 
              height = .15) +
  geom_smooth(method = lm, se = F) +
  labs(title = "Loneliness vs. Social Conflict",
       subtitle = "In All Ages",
       x = "Lonliness", 
       y = "Social Conflict") +
  theme_bw() 

str(dat$sex)

#Age and Lonelyness correlation?
dat %>% 
  select(age,
         sn_ff_2) %>% 
  ggplot(aes(x = age,
             y = sn_ff_2)) +
  geom_point() + 
  geom_smooth(method = lm)


#ER Visits vs Living situations?
#dat$ii_2 <- factor(dat$ii_2, levels = c("ZERO", "ONE", "TWO_OR_MORE"))
dat %>% 
  select(ii_2, 
         cn_hs_2_bug_infestation,
         cn_hs_2_mold, 
         cn_hs_2_lead_paint_pipes, 
         cn_hs_2_inadq_heat, 
         cn_hs_2_oven_not_wkg, 
         cn_hs_2_smoke_det_not_wkg,
         cn_hs_2_water_leaks, 
         sex,
         age,
         Age_Description) %>% 
  mutate(Danger = cn_hs_2_bug_infestation +
         cn_hs_2_mold + 
         cn_hs_2_lead_paint_pipes + 
         cn_hs_2_inadq_heat + 
         cn_hs_2_oven_not_wkg +
         cn_hs_2_smoke_det_not_wkg +
         cn_hs_2_water_leaks) %>% 
  filter(Danger != 0,
         sex != "") %>% 
  ggplot(aes(x = ii_2, y = Danger, color = Age_Description)) +
  geom_point() + 
  geom_jitter(height = .1, width = .15) +
  geom_smooth(method = "lm")


#Set Levels for Wealth question
dat$tm_6 <- factor(dat$tm_6, levels = c("LESS_THN_10K", 
                                        "MORE_THN_10_LESS_THEN_15K", 
                                        "MORE_THN_15_LESS_THEN_20K", 
                                        "MORE_THN_20_LESS_THEN_25K", 
                                        "MORE_THN_25_LESS_THEN_35K", 
                                        "MORE_THN_35_LESS_THEN_50K",  
                                        "MORE_THN_50_LESS_THEN_75K", 
                                        "MORE_THN_75K"))


#How does Income relate to Living situations?
Income_Labs <- c("<$10 k","$10 k-$15 k", "$15 k-$20 k","$20 k-$25 k", "$25 k-$35 k","$35 k-$50 k", "$50 k-$75 k", ">$75 k" )


#########  Plot 3 Household Income vs. Household Danger
dat %>% 
  select(tm_6, 
         cn_hs_2_bug_infestation,
         cn_hs_2_mold, 
         cn_hs_2_lead_paint_pipes, 
         cn_hs_2_inadq_heat, 
         cn_hs_2_oven_not_wkg, 
         cn_hs_2_smoke_det_not_wkg,
         cn_hs_2_water_leaks, 
         age,
         Age_Description,
         sex,
         Race) %>% 
  mutate(Danger = cn_hs_2_bug_infestation +
           cn_hs_2_mold + 
           cn_hs_2_lead_paint_pipes + 
           cn_hs_2_inadq_heat + 
           cn_hs_2_oven_not_wkg +
           cn_hs_2_smoke_det_not_wkg +
           cn_hs_2_water_leaks) %>% 
  filter(Danger != 0, 
         tm_6 != is.na(tm_6), 
         age >= 18, 
         sex != "") %>% 
  ggplot(aes(x = tm_6, 
             y = Danger)) +
  geom_point() + 
  geom_jitter(height = .1, width = .15) +
  labs(title = "Household Income vs. Danger",
       x = "Household Income", 
       y = "Danger") +
  scale_x_discrete(labels = Income_Labs) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 35, vjust = 0.4, hjust = .5)) 


######## Income vs. Social Discourse

dat %>% 
  select(tm_6, 
         sn_ff_2,
         cn_sf_1,
         cn_sf_2,
         cn_sf_3,
         cn_sf_4,
         age, 
         sex,
         Race) %>% 
  mutate(Conflict = cn_sf_1 + 
           cn_sf_2 + 
           cn_sf_3 + 
           cn_sf_4) %>% 
  filter(Conflict != 0, 
         tm_6 != is.na(tm_6), 
         age >= 18, 
         sex != "") %>% 
  ggplot(aes(x = tm_6, 
             y = Conflict)) +
  geom_point() + 
  geom_jitter(height = .1, width = .15) +
  labs(title = "Household Income vs. Social Conflict",
       x = "Household Income", 
       y = "Social Conflict") +
  scale_x_discrete(labels = Income_Labs) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 35, vjust = 0.4, hjust = .5)) 



#Create column determining any hispanic/Latino origin
dat <- dat %>% 
  mutate( tm_Hispanic_Origin = tm_2_yes_anthr_hisp_lat_spn_origin +
          tm_2_yes_cuban_origin +
          tm_2_yes_mex_am_chicano_orgn +
          tm_2_yes_puerto_rican)

dat <- dat %>% 
  mutate(Race = case_when(
   # tm_Hispanic_Origin > 0 ~ "Hispanic/Latino",
                               tm_3_amrcn_indn_alsk_ntv == 1 ~ "Native American/Alaska Native",
                               tm_3_asian == 1 ~ "Asian",
                               tm_3_blck_afrcn_amrcn == 1 ~ "Black/African American",
                               tm_3_hwan_other_pcfc_islander == 1 ~ "Native Hawaiian/Other Pacific Islander",
                               tm_3_white == 1 ~ "White"))


#Ethnicity
dat %>% 
  select(Race) %>% 
  group_by(Race) %>% 
  summarise(reports = n())



#Get population estimates from https://www.census.gov/data/tables/time-series/demo/popest/2010s-counties-detail.html
pop_dat <- read_csv("State Population Data.csv")


#format pop_dat for 2019 mesa county



pop_dat <- pop_dat %>% 
  filter(COUNTY == 77, YEAR == 12, AGEGRP == 0)




pop_dat <- pop_dat %>% 
  mutate(percent_Male = (TOT_MALE /TOT_POP),
         percent_Female = (TOT_FEMALE /TOT_POP),
         percent_White_Male = (WA_MALE / TOT_POP),
         percent_White_FEMALE = (WA_FEMALE / TOT_POP),
         percent_Black_Male = (BA_MALE / TOT_POP),
         percent_Black_FEMALE = (BA_FEMALE / TOT_POP),
         percent_American_Indian_Male = (IA_MALE / TOT_POP),
         percent_American_Indian_FEMALE = (IA_FEMALE / TOT_POP),
         percent_Asian_Male = (AA_MALE / TOT_POP),
         percent_Asian_FEMALE = (AA_FEMALE / TOT_POP),
         percent_Pacific_Islander_Male = (NA_MALE / TOT_POP),
         percent_Pacific_Islander_FEMALE = (NA_FEMALE / TOT_POP),) %>%
  select(-c(SUMLEV,STATE,STNAME,CTYNAME,YEAR,AGEGRP)) %>%
  select(label_percent(percent_Male),
         label_percent(percent_Female),
         label_percent(percent_White_Male),
         label_percent(percent_White_FEMALE),
         label_percent(percent_American_Indian_FEMALE),
         label_percent(percent_American_Indian_Male),
         label_percent(percent_Asian_FEMALE,percent_Asian_Male),
         label_percent(percent_Black_FEMALE),
         label_percent(percent_Black_Male),
         label_percent(percent_Pacific_Islander_FEMALE),
         label_percent(percent_Pacific_Islander_Male))

pop_dat %>% 
  select(percent_White_Male,
         percent_White_FEMALE,
         percent_American_Indian_FEMALE,
         percent_American_Indian_Male,
         percent_Asian_FEMALE,percent_Asian_Male,
         percent_Black_FEMALE,
         percent_Black_Male,
         percent_Pacific_Islander_FEMALE,
         percent_Pacific_Islander_Male) %>%
  summarise(total = percent_White_Male +
                percent_White_FEMALE +
                percent_American_Indian_FEMALE +
                percent_American_Indian_Male +
                percent_Asian_FEMALE,percent_Asian_Male +
                percent_Black_FEMALE +
                percent_Black_Male +
                percent_Pacific_Islander_FEMALE +
                percent_Pacific_Islander_Male)



Race_Count<- dat %>% 
  select(Race) %>% 
  filter( !is.na(Race)) %>% 
  group_by(Race) %>% 
  summarise(count = n()) %>% 
  ungroup()

Race_count_Total <- Race_Count %>%
  summarise(total = sum(count))

Race_Count <- Race_Count %>% 
  mutate(Percent = Race_Count$count *100 / Race_count_Total$total)

pop_dat %>% 
  select(-percent_Male, -percent_Female) %>% 
  pivot_longer(cols = everything()) %>% 
  view()


Race <- c("Asian", "Black/African American", "Native American/Alaska Native", "Native Hawaiian/Other Pacific Islander", "White")
numbers <- c(pop_dat$percent_Asian_Male + pop_dat$percent_Asian_FEMALE, 
             pop_dat$percent_Black_Male + pop_dat$percent_Black_FEMALE,
             pop_dat$percent_American_Indian_Male + pop_dat$percent_American_Indian_FEMALE,
             pop_dat$percent_Pacific_Islander_Male + pop_dat$percent_Pacific_Islander_FEMALE,
             pop_dat$percent_White_Male + pop_dat$percent_White_FEMALE)


County_Race <-data.frame(Race, numbers)


Race_Table_FINAL <- left_join(Race_Count, County_Race, by = "Race") %>% 
  select(-count) %>% 
  rename( Survey_Rep = Percent,  County_Rep = numbers) %>% 
  mutate_if(is.numeric, format, digits=3, nsmall = 0) %>% 
  melt(id = "Race")



###Plot Racial Table
Race_Table_FINAL %>% 
  ggplot(aes(x = Race, y = as.numeric(value), fill = variable ))+
  geom_bar(stat = "identity", position = "dodge")



dat %>% 
  filter(
    #Age_Description == "Child",
    age > 5,
    ii_1 != "OTHER",
    sex != "") %>% 
  select(ii_1,
         sn_ff_2,
         cn_sf_1,
         cn_sf_2,
         cn_sf_3,
         cn_sf_4,
         age, 
         sex) %>% 
  mutate( sum = cn_sf_1 + 
            cn_sf_2 + 
            cn_sf_3 + 
            cn_sf_4) %>%
  ggplot(
    aes(x = sn_ff_2,
        y = sum, 
        color = sex)) +
  geom_boxplot() +
  geom_jitter(width = .2, 
              height = .15) +
  geom_smooth(method = lm) +
  labs(title = "Relationship Between Loneliness and Social Discourse",
       x = "Level of Loneliness", 
       y = "Social Discourse",
       fill = "Sex") +
  theme_bw() 


#########  Plot 3 Household Income vs. Household Danger
dat %>% 
  select(tm_6, 
         cn_hs_2_bug_infestation,
         cn_hs_2_mold, 
         cn_hs_2_lead_paint_pipes, 
         cn_hs_2_inadq_heat, 
         cn_hs_2_oven_not_wkg, 
         cn_hs_2_smoke_det_not_wkg,
         cn_hs_2_water_leaks, 
         age,
         Age_Description,
         sex,
         Race) %>% 
  mutate(Danger = cn_hs_2_bug_infestation +
           cn_hs_2_mold + 
           cn_hs_2_lead_paint_pipes + 
           cn_hs_2_inadq_heat + 
           cn_hs_2_oven_not_wkg +
           cn_hs_2_smoke_det_not_wkg +
           cn_hs_2_water_leaks) %>% 
  filter(Danger > 0, 
         tm_6 != is.na(tm_6), 
         age >= 18, 
         sex != "") %>% 
  ggplot(aes(x = tm_6, 
             y = Danger)) +
  geom_point() + 
  geom_smooth(method = lm) +
  geom_jitter(height = .1, width = .15) +
  labs(title = "Household Income vs. Danger",
       x = "Household Income", 
       y = "Danger") +
  scale_x_discrete(labels = Income_Labs) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 35, vjust = 0.4, hjust = .5))
