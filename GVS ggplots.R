library(tidyverse)
library(ggplot2)
library(ggpubr)
library(ggalt)
library(grid)
library(gridExtra) 
library(patchwork)

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
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  







