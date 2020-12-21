library(readxl)
library(dplyr)
library(lavaan)
library(tidyverse)
library(ggplot2)
library(ggpubr)
library(ggalt)
library(grid)
library(gridExtra) 
library(patchwork)

setwd("~/Documents/Global Volunteer Survey/data")


df <- read_xlsx("volunteer_d4g_raw.xlsx") %>% 
  #  df %>% 
  mutate(Age=ifelse(Age < "20", NA_real_, as.numeric(Age))) %>% 
  mutate(AgeGroup=cut(Age, c(0, 25, 35, 45, 55, 65, 99), labels=FALSE)) %>%
  mutate(Age25.35=(AgeGroup > 1) %>% as.numeric, 
         Age35.45=(AgeGroup > 2) %>% as.numeric,
         Age45.55=(AgeGroup > 3) %>% as.numeric, 
         Age55.65=(AgeGroup > 4) %>% as.numeric, 
         Age65.up=(AgeGroup > 5) %>% as.numeric) %>% 
  mutate(GenderMale=as.numeric(Gender %in% "Male")) %>% 
  mutate(ChildrenYes=as.numeric(Children %in% "Yes")) %>% 
  mutate(DiversityYes=as.numeric(Diversity %in% "Yes")) %>% 
  mutate(SDG1=(grepl("SDG 1:", SDG_Interest)) %>% as.numeric %>% ordered, 
         SDG2=(grepl("SDG 2:", SDG_Interest)) %>% as.numeric %>% ordered, 
         SDG3=(grepl("SDG 3:", SDG_Interest)) %>% as.numeric %>% ordered, 
         SDG4=(grepl("SDG 4:", SDG_Interest)) %>% as.numeric %>% ordered, 
         SDG5=(grepl("SDG 5:", SDG_Interest)) %>% as.numeric %>% ordered, 
         SDG6=(grepl("SDG 6:", SDG_Interest)) %>% as.numeric %>% ordered, 
         SDG7=(grepl("SDG 7:", SDG_Interest)) %>% as.numeric %>% ordered, 
         SDG8=(grepl("SDG 8:", SDG_Interest)) %>% as.numeric %>% ordered, 
         SDG9=(grepl("SDG 9:", SDG_Interest)) %>% as.numeric %>% ordered, 
         SDG10=(grepl("SDG 10:", SDG_Interest)) %>% as.numeric %>% ordered, 
         SDG11=(grepl("SDG 11:", SDG_Interest)) %>% as.numeric %>% ordered, 
         SDG12=(grepl("SDG 12:", SDG_Interest)) %>% as.numeric %>% ordered, 
         SDG13=(grepl("SDG 13:", SDG_Interest)) %>% as.numeric %>% ordered, 
         SDG14=(grepl("SDG 14:", SDG_Interest)) %>% as.numeric %>% ordered, 
         SDG15=(grepl("SDG 15:", SDG_Interest)) %>% as.numeric %>% ordered
  ) %>%  
  mutate(EventAttendanceYes=ordered(as.numeric(DK_Event_Attendance %in% "Yes"))) 

# cfa component / see also https://lavaan.ugent.be/tutorial/cfa.html

cfa_formula <-  
  "
  
  Protective =~ Protective_1 + Protective_2 + Protective_3 + Protective_4 + Protective_5
  Values =~ Values_1 + Values_2 + Values_3 + Values_4 + Values_5
  Career =~ Career_1 + Career_2 + Career_3 + Career_4 + Career_5
  Social =~ Social_1 + Social_2 + Social_3 + Social_4 + Social_5
  Understanding =~ Understanding_1 + Understanding_2 + Understanding_3 + Understanding_4
  Enhancement =~ Enhancement_1 + Enhancement_2 + Enhancement_3 + Enhancement_4+ Enhancement_5

  "


# cfa(cfa_formula, data=df)


factor_diffs_formula <- 
  "
  Protective ~ Age25.35 + Age35.45 + Age45.55 + Age55.65 + Age65.up + GenderMale 
  Values ~ Age25.35 + Age35.45 + Age45.55 + Age55.65 + Age65.up + GenderMale 
  Career ~ Age25.35 + Age35.45 + Age45.55 + Age55.65 + Age65.up + GenderMale 
  Social ~ Age25.35 + Age35.45 + Age45.55 + Age55.65 + Age65.up + GenderMale 
  Understanding ~ Age25.35 + Age35.45 + Age45.55 + Age55.65 + Age65.up + GenderMale
  Enhancement ~ Age25.35 + Age35.45 + Age45.55 + Age55.65 + Age65.up + GenderMale

  "
#sem(paste(cfa_formula, factor_diffs_formula), df) %>% summary()
#sem(paste(cfa_formula, factor_diffs_formula), df) %>% summary(fit.measures=TRUE, rsquare=TRUE, standardized = TRUE)

ae <- sem(paste(cfa_formula, factor_diffs_formula), df)

df2 <- cbind(df[ae@Data@case.idx[[1]],], lavaan::predict(ae)) # extracting coefficients to make visualizations 
# of each factor




ggplot(df2) + aes(x=Career, y=Understanding, color=Gender) + geom_point() 
ggplot(df2) + aes(x=Career, y=Understanding, color=Gender) + geom_point() 

####### tibbles with raw data from event interest and means, ggplots placed below. 


motivation_total <- tribble(
  ~'Score', ~'mean', ~'sd',
  'Protective', 3.45, 1.53,
  'Values', 6.06, 1.01,
  'Career', 4.19, 1.64,
  'Social', 3.08, 1.57,
  'Enhancement', 4.43, 1.44,
  'Understanding', 5.55, 0.93
)

ggplot(motivation_total, aes(x = Score, y = mean)) +
  geom_bar(stat = "identity",
           position = position_dodge()) +
  geom_errorbar(aes(ymin= mean-sd, ymax=mean+sd), width =.2,
                position = position_dodge(.9)) +
  labs(title = "Cumulative Motivation Scores", x="Category", y = "Score") +
  theme_classic() +
  scale_fill_manual(values=c('#999999','#E69F00'))


skills_total <- tribble(
  ~'Skill', ~'Count', 
  'EDA', 208,
  'Data Wrangling', 
  'Predictive Modeling', 152,
  'NLP', 65,
  'Machine Learning', 128,
  'Deep Learning', 57,
  'Advanced Statistics',135, 
  'Computer Vision',
  'Time Series',102,
  'Operations Science', 22,
  'Network/Graph Theory', 34,
  'Data Security',
  'Dashboarding',
  'Other',
)
)

extra_opp <- tribble(
  ~'Activity',              ~'Count',
  'Product Management', 79,
  'Market Research', 49,
  'UI/UX Research', 25,
  'Blog Writing', 77,
  'Code Documentation', 65,
  'Open Source', 102,
  'Marketing & Comms', 18,
  'AI Ethics', 104,
  'Project Scoping', 111,
  'Subject Matter Expertise', 94,
  'Data Maturity Support', 60,
  'Volunteer Mentoring', 3
)

ggplot(data = extra_opp, aes(x = reorder(Activity, -Count), y = Count)) +
  geom_bar(stat = "identity") + 
  geom_text(aes(label = Count), position = position_dodge(width = 0.5), vjust = -0.25) +
  theme(axis.text.x = element_text(angle = 90)) +
  labs(x = "Activity", y = "Count")


events_virtual <- tribble(
  ~'Event', ~'Count',
  'Project Showcase', 162,
  'Technical Workshop', 179,
  'Networking', 133,
  'Scoping Workshops', 169,
  'Data Ethics Discusson', 156,
  'Reading/Study Groups', 130,
  
)

events_viz <-  ggplot(data = events_virtual, aes(x = reorder(Event, -Count), y = Count)) +
  geom_bar(stat = "identity", width = 0.60) + 
  geom_text(aes(label = Count), position = position_dodge(width = 1), vjust = -0.25) +
  theme(axis.text.x = element_text(angle = 90)) +
  labs(x = "Event Type", y = "Count")

ggsave("events.png", plot = events_viz, device = "png", width = 7 , height = 5, 
       units = "in", dpi = 300)


sdgs <- tribble(
  ~'SDG' ,             ~'Count',
  'SDG1: Poverty', 148,
  'SDG 2: Zero Hunger', 137,
  'SDG 4: Quality Education', 134,
  'SDG 5: Gender Equality', 174,
  'SDG 6: Clean Water and Sanitation', 114,
  'SDG 7: Affordable and Clean Energy', 118,
  'SDG 9: Industry, Innovation and Infrast', 84,
  'SDG 10: Reduced Inequality', 160,
  'SDG 11: Sustainable Cities and Communities', 121,
  'SDG 13: Climate Action', 137,
  'SDG 14: Life Below Water', 43,
  'SDG 15: Life on Land', 47,
  'SDG 16: Strong Instititons', 1
)

sdgs_viz <-  ggplot(data = sdgs, aes(x = reorder(SDG, -Count), y = Count)) +
  geom_bar(stat = "identity", width = 0.60) + 
  geom_text(aes(label = Count), position = position_dodge(width = 1), vjust = -0.25) +
  theme(axis.text.x = element_text(angle = 90)) +
  labs(x = "SDG", y = "Count")



