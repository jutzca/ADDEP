# DATA CLEANING 
## The original dataset is "DS1.csv". However, to respect the original file, we saved it as "ADDEP" in our data analysis. 
### In "ADDEP", we only included relevant info from "DS1.csv", changed the individual motor scores names and converted them into numeric.
### For example, "AASAC5AL" in "DS1.csv" is changed to "C5_R_SURG" in our "ADDEP.xlsx" based on the data dictionary.
ADDEP <- read_excel("~/Documents/AK/ADDEP Raw Data/ADDEP.xlsx")

### The "ASIA_updated.xlsx" is also taken from "DS1.csv" but only ASIA grades info 
ASIA_updated <- read_excel("~/Documents/AK/ADDEP Raw Data/ASIA_updated.xlsx")

#### Transforming some info
ASIA_updated$ASIA_ADM_numeric <- as.numeric(chartr("ABCDE", "12345", ASIA_updated$REVIEWASIAGRADEADM))
ASIA_updated$ASIA_DIS_numeric <- as.numeric(chartr("ABCDE", "12345", ASIA_updated$REVIEWASIAGRADEDC))
ASIA_updated$ASIA_CHANGE_updated <- as.factor(ifelse(ASIA_updated$ASIA_DIS_numeric-ASIA_updated$ASIA_ADM_numeric >=1 , 1, 0))

ASIA_updated$AFLMODDS <-  as.factor(parse_number(ASIA_updated$AFLMODDS))

#### Only showing the level of injury (ex, C or T, or L)
ASIA_updated$ASIA_LEVEL_ADM <- as.factor(substring(ASIA_updated$REVIEWASIALEVELADM, 1, 1))
ASIA_updated$ASIA_LEVEL_DIS <- as.factor(substring(ASIA_updated$REVIEWASIALEVELDC, 1, 1))

# New one
ADDEP_1 <- merge(ADDEP, ASIA_updated, by = "NEWID", all=TRUE)
ADDEP_1$REVIEWASIAGRADEADM <- as.factor(ADDEP_1$REVIEWASIAGRADEADM)

ADDEP_1$ASIA_CHANGE_updated_2 <- as.factor(ADDEP_1$ASIA_DIS_numeric-ADDEP_1$ASIA_ADM_numeric)

ADDEP_1$Walk <- as.factor(ifelse(ADDEP_1$AFLMODDS==1, 0, 1))
ADDEP_1$Marked_Recovery <- as.factor(ifelse(ADDEP_1$REVIEWASIAGRADEADM == "A" & 
                                              ADDEP_1$ASIA_DIS_numeric-ADDEP_1$ASIA_ADM_numeric >= 2, 1, 
                                            ifelse(ADDEP_1$REVIEWASIAGRADEADM == "B" &
                                                     ADDEP_1$ASIA_DIS_numeric-ADDEP_1$ASIA_ADM_numeric >= 2, 1,
                                                   ifelse(ADDEP_1$REVIEWASIAGRADEADM == "C" &
                                                            ADDEP_1$Walk == 1, 1,
                                                          ifelse(ADDEP_1$REVIEWASIAGRADEADM == "D" &
                                                                   ADDEP_1$Walk == 1, 1, 0)))))

### ASIA_update_2 is file from DS1.csv but only included "NEWID", "BASIMP, and "BFIMLMOD"
ASIA_updated_2$BASAIMP_ANNUAL_numeric <- as.numeric(chartr("ABCDE", "12345", ASIA_updated_2$BASAIMP))
ASIA_updated_2$BFIMLMOD <-  as.factor(parse_number(ASIA_updated_2$BFIMLMOD))

### Updated our file and created more variables
ADDEP_2 <- merge(ADDEP_1, ASIA_updated_2, by = "NEWID", all=TRUE)
ADDEP_2$WALK_ANNUAL_updated <- as.factor(ifelse(ADDEP_2$BFIMLMOD==1, 0, 1))
ADDEP_2$Marked_Recovery_Annual <- as.factor(ifelse(ADDEP_2$REVIEWASIAGRADEADM == "A" & 
                                                     ADDEP_2$BASAIMP_ANNUAL_numeric-ADDEP_2$ASIA_ADM_numeric >= 2, 1, 
                                                   ifelse(ADDEP_2$REVIEWASIAGRADEADM == "B" &
                                                            ADDEP_2$BASAIMP_ANNUAL_numeric-ADDEP_2$ASIA_ADM_numeric >= 2, 1,
                                                          ifelse(ADDEP_2$REVIEWASIAGRADEADM == "C" &
                                                                   ADDEP_2$WALK_ANNUAL_updated == 1, 1,
                                                                 ifelse(ADDEP_2$REVIEWASIAGRADEADM == "D" &
                                                                          ADDEP_2$WALK_ANNUAL_updated == 1, 1, 0)))))



ADDEP_2$ASIA_CHANGE_updated_3 <- as.factor(ADDEP_2$BASAIMP_ANNUAL_numeric-ADDEP_1$ASIA_ADM_numeric)

ADDEP_2$SEX_NUM <- as.factor(ADDEP_2$SEX_NUM)

ADDEP_2$NEWASIAGRADE <- as.factor(paste0("AIS ", ADDEP_2$REVIEWASIAGRADEADM))

### "ALB_DAYS" has "NEWID" and "CRLOWALBUMINDAYS" taken from DS1.csv
ADDEP_2 <- merge(ADDEP_2, ALB_DAYS, by = "NEWID", all=TRUE)

ADDEP_2$AGE_BINARY <- as.factor(ifelse(ADDEP_2$AGE_INJ_NUM=="1"|ADDEP_2$AGE_INJ_NUM=="2"|
                                         ADDEP_2$AGE_INJ_NUM=="3"|ADDEP_2$AGE_INJ_NUM=="4", 0,1))

ADDEP_2$AIS_BINARY <- as.factor(ifelse(ADDEP_2$REVIEWASIAGRADEADM=="A"|ADDEP_2$REVIEWASIAGRADEADM=="B", "Complete", "Incomplete"))

###Marked recovery if over 2 AIS grades for AIS A, over 2 grades or walking in B, walking for C and D
ADDEP_2$Marked_Recovery_Annual_2 <- as.factor(ifelse(ADDEP_2$REVIEWASIAGRADEADM == "A" & 
                                                       ADDEP_2$BASAIMP_ANNUAL_numeric-ADDEP_2$ASIA_ADM_numeric >= 2, 1, 
                                                     ifelse(ADDEP_2$REVIEWASIAGRADEADM == "B" &
                                                              (ADDEP_2$BASAIMP_ANNUAL_numeric-ADDEP_2$ASIA_ADM_numeric >= 2|ADDEP_2$WALK_ANNUAL_updated == 1), 1,
                                                            ifelse(ADDEP_2$REVIEWASIAGRADEADM == "C" &
                                                                     ADDEP_2$WALK_ANNUAL_updated == 1, 1,
                                                                   ifelse(ADDEP_2$REVIEWASIAGRADEADM == "D" &
                                                                            ADDEP_2$WALK_ANNUAL_updated == 1, 1, 0)))))

### FIM_Admission is from DS1.csv to make walking admission and discharge 
FIM_Admission$AFLMODRB <-  as.factor(parse_number(FIM_Admission$AFLMODRB))

FIM_Admission$AFLMODDS <-  as.factor(parse_number(FIM_Admission$AFLMODDS))

FIM_Admission$Walk_Admission <- as.factor(ifelse(FIM_Admission$AFLMODRB==1, 0, 1))

FIM_Admission$Walk_Discharge <- as.factor(ifelse(FIM_Admission$AFLMODDS==1, 0, 1))

ADDEP_3 <- merge(ADDEP_2, FIM_Admission, by = "NEWID", all=TRUE)

ADDEP_3$ASIAGRADE_CD <- as.factor(ifelse(ADDEP_3$REVIEWASIAGRADEADM=="A", "A", 
                                         ifelse(ADDEP_3$REVIEWASIAGRADEADM == "B", "B", "C/D")))

#### New AISA grades at admission 
ADDEP_3$ASIAGRADE_WALK <- as.factor(ifelse(ADDEP_3$REVIEWASIAGRADEADM=="A", "A", 
                                           ifelse(ADDEP_3$REVIEWASIAGRADEADM == "B", "B", 
                                                  ifelse(ADDEP_3$REVIEWASIAGRADEADM == "B" & ADDEP_3$Walk_Admission == 1, "B/Walk",
                                                         ifelse(ADDEP_3$REVIEWASIAGRADEADM == "C", "C",
                                                                ifelse(ADDEP_3$REVIEWASIAGRADEADM == "C" & ADDEP_3$Walk_Admission == "1", "C/Walk",
                                                                       ifelse(ADDEP_3$REVIEWASIAGRADEADM == "D" & ADDEP_3$Walk_Admission == "1", "D/Walk", "D")))))))
#### New ASIA grades at annual 
ADDEP_3$ASIAGRADE_WALK_ANNUAL <- as.factor(ifelse(ADDEP_3$BASAIMP=="A", "A", 
                                                  ifelse(ADDEP_3$BASAIMP == "B", "B", 
                                                         ifelse(ADDEP_3$BASAIMP == "B" & ADDEP_3$WALK_ANNUAL_updated == 1, "B&Walk",
                                                                ifelse(ADDEP_3$BASAIMP == "C", "C",
                                                                       ifelse(ADDEP_3$BASAIMP == "C" & ADDEP_3$WALK_ANNUAL_updated == 1, "C&Walk",
                                                                              ifelse(ADDEP_3$BASAIMP == "D" & ADDEP_3$WALK_ANNUAL_updated == 1, "D&Walk", "D")))))))


ADDEP_3$AIS_BINARY <- as.factor(ADDEP_3$AIS_BINARY)

ADDEP_3$AIS_WALK <- as.factor(ifelse(ADDEP_3$AIS_BINARY=="Complete","Complete", 
                                     ifelse(ADDEP_3$AIS_BINARY=="Incomplete" & ADDEP_3$Walk_Admission==0, "Incomplete/Not Walking", "Incomplete/Walking")))

ADDEP_3$Marked_Recovery_Discharge <- as.factor(ifelse(ADDEP_3$REVIEWASIAGRADEADM == "A" & 
                                                        ADDEP_3$ASIA_DIS_numeric-ADDEP_3$ASIA_ADM_numeric >= 2, 1, 
                                                      ifelse(ADDEP_3$REVIEWASIAGRADEADM == "B" &
                                                               (ADDEP_3$ASIA_DIS_numeric-ADDEP_3$ASIA_ADM_numeric >= 2|ADDEP_3$Walk_Discharge == 1), 1,
                                                             ifelse(ADDEP_3$REVIEWASIAGRADEADM == "C" &
                                                                      ADDEP_3$Walk_Discharge == 1, 1,
                                                                    ifelse(ADDEP_3$REVIEWASIAGRADEADM == "D" &
                                                                             ADDEP_3$Walk_Discharge == 1, 1, 0)))))

ADDEP_3$Change_Scores <- ADDEP_3$LOWER_MS_ANNUAL-ADDEP_3$LOWER_MS_REHAB

#### INJ_DAYS.xlsx has "NEWID", "POCADMDAY", and "DAYSINJTODC" from DS1.csv
ADDEP_4 <- merge(ADDEP_3, INJ_DAYS, by = "NEWID", all = TRUE)

#DATA ANALYSIS
## Some graphs at admission 
cleanup <- theme(panel.grid.major = element_blank(),
                 panel.grid.minor = element_blank(),
                 panel.background = element_blank(),
                 axis.line = element_line(colour="gray"),
                 plot.title = element_text(size = rel(1.2), 
                                           vjust = 1.5, 
                                           hjust = 0.5),
                 plot.subtitle = element_text(size = rel(1), 
                                              vjust = 1.5, 
                                              hjust = 0.5,
                                              face="italic"),
                 panel.border=element_rect(colour="gray",
                                           size=1, 
                                           fill=NA), 
                 panel.spacing = unit(0.2, "lines"),
                 axis.ticks.y = element_line(colour="gray"), 
                 axis.ticks.x = element_line(colour="gray"))

ALB_LEMS <- ggplot(data=subset(ADDEP_3, !is.na(ASIAGRADE_WALK)&ASIA_LEVEL_DIS==c("C", "T")&!is.na(CRLOWALBUMIN)), aes(x=CRLOWALBUMIN, y=LOWER_MS_REHAB, colour = ASIAGRADE_WALK))+
  geom_jitter(width=0.2, alpha=0.6)+
  scale_x_continuous(limits = c(0,5))+
  cleanup+
  theme(axis.text=element_text(size=11),
        axis.title=element_text(size=12),
        strip.text.x = element_text(size = 12),
        panel.border = element_blank(),
        legend.position='bottom',
        legend.key=element_blank())+
  labs(colour='AIS Grades') +
  labs(y="LEMS at Rehab Admission", x="Albumin concentration [g/dL]") 

ALB_AIS <- ggplot(data=subset(ADDEP_3, !is.na(ASIAGRADE_WALK)&ASIA_LEVEL_DIS==c("C", "T")&!is.na(CRLOWALBUMIN)), aes(y=CRLOWALBUMIN, x=ASIAGRADE_WALK, colour=ASIAGRADE_WALK))+
  geom_boxplot()+
  geom_jitter(width=0.2, alpha=0.6)+
  scale_y_continuous(limits = c(0,5))+
  cleanup+
  theme(axis.text=element_text(size=11),
        axis.title=element_text(size=12),
        strip.text.x = element_text(size = 12), 
        axis.title.x=element_blank(),
        axis.ticks.x=element_blank(), 
        axis.text.x=element_blank(), 
        panel.border = element_blank(),
        legend.key=element_blank())+
  labs(colour='AIS Grades')+
  labs(y="Albumin concentration [g/dL]", x="AIS grades") 

ggarrange(ALB_LEMS, ALB_AIS, nrow=2, labels = c("A", "B"),common.legend = TRUE, legend = "bottom")

#URP for admission
ADDEP_3 = apply_labels(ADDEP_3, CRLOWALBUMIN = "Min Albumin Concentration", 
                       LOWER_MS_REHAB = "LEMS at Admission", ASIAGRADE_WALK = "AIS Grades at Admission")

URP_LEMS_baseline <-use_labels(ADDEP_3, ctree(LOWER_MS_REHAB ~ CRLOWALBUMIN, controls=ctree_control(testtype = "Bonferroni",
                                                                                                    mincriterion = 0.95), data=subset(..data, !is.na(LOWER_MS_REHAB)&ASIA_LEVEL_ADM==c("C", "T")&!is.na(CRLOWALBUMIN))))

plot(URP_LEMS_baseline)


URP_AIS_baseline <-use_labels(ADDEP_3, ctree(ASIAGRADE_WALK ~ CRLOWALBUMIN,controls=ctree_control(testtype = "Bonferroni",
                                                                                                  mincriterion = 0.95), data=subset(..data, !is.na(ASIAGRADE_WALK)&ASIA_LEVEL_ADM==c("C", "T")&!is.na(CRLOWALBUMIN))))

plot(URP_AIS_baseline, terminal_panel=node_barplot(URP_AIS_baseline))

# URP for annual 
## Univariate URP for Change Scores 
plot(use_labels(ADDEP_3,ctree(Change_Scores ~ CRLOWALBUMIN, data=subset(..data, !is.na(Change_Scores)&!(Walk_Admission==1)&ASIA_LEVEL_ADM==c("C", "T")&!is.na(CRLOWALBUMIN)))))

## Bivariate URP for Change Scores 
plot(use_labels(ADDEP_3,ctree(Change_Scores ~ CRLOWALBUMIN+LOWER_MS_REHAB, data=subset(..data, !is.na(Change_Scores)&!(Walk_Admission==1)&ASIA_LEVEL_ADM==c("C", "T")&!is.na(CRLOWALBUMIN)))))

# Univariate URP for Marked Recovery annual 
plot(use_labels(ADDEP_3,ctree(Marked_Recovery_Annual_2~CRLOWALBUMIN, data=subset(..data, !is.na(Marked_Recovery_Annual_2)&!(Walk_Admission==1)&ASIA_LEVEL_ADM==c("C", "T")&!is.na(CRLOWALBUMIN)))))

# Bivariate URP for Marked Recovery annual 
plot(use_labels(ADDEP_3,ctree(Marked_Recovery_Annual_2~CRLOWALBUMIN+REVIEWASIAGRADEADM, data=subset(..data, !is.na(Marked_Recovery_Annual_2)&!(Walk_Admission==1)&ASIA_LEVEL_ADM==c("C", "T")&!is.na(CRLOWALBUMIN)))))
