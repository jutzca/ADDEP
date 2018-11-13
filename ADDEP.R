write.csv(da36724.0001, file="DS1.csv", na="")
write.csv(da36724.0002, file="DS2.csv", na="")
write.csv(da36724.0003, file="DS3.csv", na="")
write.csv(da36724.0004, file="DS4.csv", na="")
write.csv(da36724.0005, file="DS5.csv", na="")
write.csv(da36724.0006, file="DS6.csv", na="")
write.csv(da36724.0007, file="DS7.csv", na="")
write.csv(da36724.0008, file="DS8.csv", na="")
write.csv(da36724.0009, file="DS9.csv", na="")

ADDEP <- read_excel("~/Documents/AK/ADDEP Raw Data/ADDEP.xlsx")
ASIA_updated <- read_excel("~/Documents/AK/ADDEP Raw Data/ASIA_updated.xlsx")

#EDA for biomarkers 

ASIA_updated$ADMTODC_ASIA_CHANGE <- as.factor(parse_number(ASIA_updated$ADMTODC_ASIA_CHANGE))

ASIA_updated$ASIA_ADM_numeric <- as.numeric(chartr("ABCDE", "12345", ASIA_updated$REVIEWASIAGRADEADM))
ASIA_updated$ASIA_DIS_numeric <- as.numeric(chartr("ABCDE", "12345", ASIA_updated$REVIEWASIAGRADEDC))
ASIA_updated$ASIA_CHANGE_updated <- as.factor(ifelse(ASIA_updated$ASIA_DIS_numeric-ASIA_updated$ASIA_ADM_numeric >=1 , 1, 0))

ASIA_updated$AFLMODDS <-  as.factor(parse_number(ASIA_updated$AFLMODDS))

ASIA_updated$ASIA_LEVEL_ADM <- as.factor(substring(ASIA_updated$REVIEWASIALEVELADM, 1, 1))
ASIA_updated$ASIA_LEVEL_DIS <- as.factor(substring(ASIA_updated$REVIEWASIALEVELDC, 1, 1))


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



ASIA_updated_2$BASAIMP_ANNUAL_numeric <- as.numeric(chartr("ABCDE", "12345", ASIA_updated_2$BASAIMP))
ASIA_updated_2$BFIMLMOD <-  as.factor(parse_number(ASIA_updated_2$BFIMLMOD))


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

ASIA <- ADDEP_2[,c("ASIA_CHANGE_updated_3", "Marked_Recovery_Annual", "REVIEWASIAGRADEADM", "BASAIMP", "WALK_ANNUAL_updated", "BFIMLMOD")]
#Descriptive Statistics 
fun <- function(x){
  c(m=mean(x, na.rm=T), v=sd(x, na.rm=T), n=length(x))
}

summaryBy(NEWID~SEX, 
          data=ADDEP_1, FUN = fun)

stargazer(rbind("SEX"=table_sex, 
                "AGE"=table(ADDEP_1$AGE_INJ)), type="text")

table_sex <- table(ADDEP_1$SEX)
ADDEP$SEX_NUM <- factor(ADDEP$SEX_NUM, levels=c("1","2"), 
                    labels=c("Male", "Female"))
print.xtableFtable(ftable(ADDEP$SEX_NUM, ADDEP$AGE_INJ_NUM, 
                           row.vars=c(2,4),dnn=c("SEX", "AGE")))


ADDEP$TOT_MS_SURG <- rowSums(ADDEP[,c(117:136)], na.rm = FALSE)
ADDEP$LOWER_MS_SURG <- rowSums(ADDEP[,c(122:126, 132:136)], na.rm=FALSE)

ADDEP$TOT_MS_REHAB <- rowSums(ADDEP[, c(137:156)], na.rm=FALSE)
ADDEP$LOWER_MS_REHAB <- rowSums(ADDEP[,c(142:146, 152:156)], na.rm=FALSE)

ADDEP$TOT_MS_DISCHARGE <- rowSums(ADDEP[,c(157:176)], na.rm=FALSE)
ADDEP$LOWER_MS_DISCHARGE <- rowSums(ADDEP[,c(162:166, 172:176)], na.rm=FALSE)

ADDEP$TOT_MS_ANNUAL <- rowSums(ADDEP[,c(70:89)], na.rm=FALSE)
ADDEP$LOWER_MS_ANNUAL <- rowSums(ADDEP[,c(75:79, 85:89)], na.rm=FALSE)

factors <- c("SEX_NUM", "AGE_INJ_NUM", "ETIO_TR_NUM", 
             "ASS_INJ_NUM", "SPL_SURG_NUM", "ASIA_DISCHARGE")
ADDEP[,factors] <- lapply(ADDEP[,factors], as.factor)

ADDEP_noNA_PreALb <- ADDEP_2[!is.na(ADDEP_2$CRLOWPREALBUMIN),]

ADDEP_noNA <- ADDEP[c("LOWER_MS_ANNUAL", "RATIO_ALB")]
ADDEP_noNA <- ADDEP_noNA[!is.na(ADDEP_noNA$LOWER_MS_ANNUAL),]

ADDEP_noASIA <- ADDEP_2[ ! (ADDEP_2$ASIA_LEVEL_DIS=="I") & !(ADDEP_2$ASIA_LEVEL_DIS=="S"), ] 

cv.lm(data=ADDEP_noNA, form.lm=formula(LOWER_MS_ANNUAL~RATIO_ALB), m=5, 
      dots=FALSE, seed=29, plotit = TRUE, printit=TRUE)

#Lower Motor Scores 1 year 
lm_1 <- lm(LOWER_MS_ANNUAL~CRLOWALBUMIN,subset = REVIEWASIAGRADEADM == "A", data=ADDEP_1)

dat_text <- data.frame(
  label = c("p = .031", "p = .003"),
  NEWASIAGRADE   = c("AIS B", "AIS D")
)

geom_text(data    = dat_text,
          mapping = aes(x = 1, y = 4.75, label = label),
          hjust   = -0.1,
          vjust   = -1)

AIS_names <- list(
  'AIS A'="AIS A",
  'AIS B'="AIS B*",
  'AIS C'="AIS C",
  'AIS D'="AIS D*"
)

AIS_labeller <- function(variable,value){
  return(AIS_names[value])
}

ggplot(data=subset(ADDEP_2, !is.na(Marked_Recovery_Annual)), aes(y=CRLOWALBUMIN, x=Marked_Recovery_Annual, fill=Marked_Recovery_Annual))+
  geom_boxplot()+
  geom_jitter(width=0.2, alpha=0.6)+
  scale_fill_manual(name = "Marked recovery at annual exam", values=c("lightskyblue4", "lightblue1")
                    , labels = c("0" = "Not Achieved", "1" = "Achieved"))+
  scale_y_continuous(limits = c(0,5))+
  cleanup+
  facet_grid(.~NEWASIAGRADE, labeller=AIS_labeller)+
  scale_x_discrete(labels=c("0" = "Not Achieved", "1" = "Achieved"))+
  theme(axis.text=element_text(size=14),
        axis.title=element_text(size=18,face="bold"),
        strip.text.x = element_text(size = 14), 
        axis.title.x=element_blank(),
        axis.ticks.x=element_blank(), 
        axis.text.x=element_blank())+
  labs(y="Albumin concentration [g/dL]")


MR_A <- ggplot(data=subset(ADDEP_2, !is.na(Marked_Recovery_Annual)&REVIEWASIAGRADEADM=="A"), aes(y=CRLOWALBUMIN, x=Marked_Recovery_Annual))+
  geom_boxplot()+
  geom_jitter()+
  scale_y_continuous(limits = c(0,5))+
  cleanup+
  scale_x_discrete(labels=c("0" = "Not Achieved", "1" = "Achieved"))+
  theme(axis.title.x=element_blank(),
        axis.title.y=element_blank())

MR_B <- ggplot(data=subset(ADDEP_2, !is.na(Marked_Recovery_Annual)&REVIEWASIAGRADEADM=="B"), aes(y=CRLOWALBUMIN, x=Marked_Recovery_Annual))+
  geom_boxplot()+
  geom_jitter()+
  scale_y_continuous(limits = c(0,5))+
  cleanup+
  scale_x_discrete(labels=c("0" = "Not Achieved", "1" = "Achieved"))+
  annotate("text", x = 1.5, y=4.8, label= "p =.03 \n ORs = 4.3")+ 
  theme(axis.title.x=element_blank(),
        axis.title.y=element_blank())

MR_C <- ggplot(data=subset(ADDEP_2, !is.na(Marked_Recovery_Annual)&REVIEWASIAGRADEADM=="C"), aes(y=CRLOWALBUMIN, x=Marked_Recovery_Annual))+
  geom_boxplot()+
  geom_jitter()+
  scale_y_continuous(limits = c(0,5))+
  cleanup+
  scale_x_discrete(labels=c("0" = "Not Achieved", "1" = "Achieved"))+
  theme(axis.title.x=element_blank(),
        axis.title.y=element_blank())

MR_D <- ggplot(data=subset(ADDEP_2, !is.na(Marked_Recovery_Annual)&REVIEWASIAGRADEADM=="D"), aes(y=CRLOWALBUMIN, x=Marked_Recovery_Annual))+
  geom_boxplot()+
  geom_jitter()+
  scale_y_continuous(limits = c(0,5))+
  cleanup+
  scale_x_discrete(labels=c("0" = "Not Achieved", "1" = "Achieved"))+
  annotate("text", x = 1.5, y=4.8, label= "p =.003 \n ORs = 8.3")+ 
  theme(axis.title.x=element_blank(),
        axis.title.y=element_blank())

Fig_MR <- ggarrange(MR_A, MR_B, MR_C, MR_D, ncol=4,
                 labels=c("AIS-A", "AIS-B", "AIS-C", "AIS-D"))

annotate_figure(Fig_MR,
                top = text_grob("Albumin Concentration Across AIS-grades \n in Marked Recovery at 52 weeks", color = "red", face = "bold", size = 14),
                bottom = text_grob("Marked Recovery at Week 52"),
                left = text_grob("Albumin Concentration (g/dL)", rot = 90),
                fig.lab = "Figure 2", fig.lab.face = "bold"
)


ggplot(data=ADDEP_2, aes(y=CRLOWALBUMIN, x=LOWER_MS_ANNUAL))+
  geom_point(alpha=0.6)+
  scale_y_continuous(limits = c(0,5))+
  cleanup+
  facet_grid(.~NEWASIAGRADE)+
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"),
        strip.text.x = element_text(size = 12))+
  labs(x="LEMS at annual exam", y="Albumin concentration [g/dL]")



LEMS_A<-ggplot(data=subset(ADDEP_2, REVIEWASIAGRADEADM=="A"), aes(y=CRLOWALBUMIN, x=LOWER_MS_ANNUAL))+
  geom_jitter()+
  scale_y_continuous(limits=c(1,5))+
  scale_x_continuous(limits=c(0,50))+
  cleanup+
  theme(axis.title.x=element_blank(),
        axis.title.y=element_blank())

LEMS_B<-ggplot(data=subset(ADDEP_2, REVIEWASIAGRADEADM=="B"), aes(y=CRLOWALBUMIN, x=LOWER_MS_ANNUAL))+
  geom_jitter()+
  scale_y_continuous(limits=c(1,5))+
  cleanup+
  theme(axis.title.x=element_blank(),
        axis.title.y=element_blank())

LEMS_C<-ggplot(data=subset(ADDEP_2, REVIEWASIAGRADEADM=="C"), aes(y=CRLOWALBUMIN, x=LOWER_MS_ANNUAL))+
  geom_jitter()+
  scale_y_continuous(limits=c(1,5))+
  cleanup+
  theme(axis.title.x=element_blank(),
        axis.title.y=element_blank())

LEMS_D<-ggplot(data=subset(ADDEP_2, REVIEWASIAGRADEADM=="D"), aes(y=CRLOWALBUMIN, x=LOWER_MS_ANNUAL))+
  geom_jitter()+
  scale_y_continuous(limits=c(1,5))+
  scale_x_continuous(limits=c(0,50))+
  cleanup+
  theme(axis.title.x=element_blank(),
        axis.title.y=element_blank())

Fig <- ggarrange(LEMS_A, LEMS_B, LEMS_C, LEMS_D, ncol=4,
          labels=c("AIS-A", "AIS-B", "AIS-C", "AIS-D"))

annotate_figure(Fig,
                top = text_grob("Albumin Concentration Across AIS-grades \n in LEMS at 52 weeks", color = "red", face = "bold", size = 14),
                bottom = text_grob("Lower Extremity Motor Scores at Week 52"),
                left = text_grob("Albumin Concentration (g/dL)", rot = 90),
                fig.lab = "Figure 1", fig.lab.face = "bold"
)

stargazer(cbind("Estimate"=coef(lm_4),
                confint(lm_4),
                "Naive SE" = summary(lm_4)$coefficients[,2],
                "Robust SE" = sqrt(diag(vcovHC(lm_4, type="HC"))),
                "P value" = summary(lm_4)$coefficients[,4]), type = "text")

#Check influential points
ols_plot_resid_lev(lm_1)
influential_obs <- influence.measures(lm_1)

ADDEP_influential_obs <- which(apply(influential_obs$is.inf, 1, any)) 
ADDEP_sans_influential_obs <- ADDEP_1[-ADDEP_influential_obs,] 

lm_2 <- lm(LOWER_MS_ANNUAL~CRLOWALBUMIN*REVIEWASIAGRADEADM+SEX_NUM+AGE_INJ_NUM+
             SPL_SURG_NUM, data=ADDEP_1)

#heteroscedasticity corrected se
coeftest(lm_4, vcov. = vcovHC(lm_4))

#with prealbumin
lm_3 <- lm(LOWER_MS_DISCHARGE~CRLOWPREALBUMIN+SEX_NUM+AGE_INJ_NUM+
  SPL_SURG_NUM+REVIEWASIAGRADEADM, data=ADDEP_1)

lm_4 <- lm(LOWER_MS_ANNUAL~CRLOWALBUMIN+SEX_NUM+AGE_INJ_NUM+
  REVIEWASIAGRADEADM+ASIA_LEVEL_DIS, data= ADDEP_2)

#Set-validation 
set.seed(2000)
sample <- sample(seq(1, nrow(ADDEP_noASIA)), replace=FALSE)
training <- ADDEP_noASIA[sample[1:1199],]
test <-ADDEP_noASIA[sample[1200:nrow(ADDEP_noASIA)],]


smp_size <- floor(0.95 * nrow(ADDEP_noASIA))
set.seed(123)
train_ind <- sample(seq_len(nrow(ADDEP_noASIA)), size = smp_size)
training <- ADDEP_noASIA[train_ind, ]
test <- ADDEP_noASIA[-train_ind, ]

lm_training <- lm(LOWER_MS_ANNUAL~CRLOWALBUMIN+REVIEWASIAGRADEADM+SEX_NUM+AGE_INJ_NUM+ASIA_LEVEL_DIS
                      , data =training)
predict_lm <- predict(lm_training, data=training, newdata = test)

actuals_preds <- data.frame(cbind(actuals=test$LOWER_MS_ANNUAL, 
                                  predicteds=predict_lm))

accuracy <- data.frame(apply(actuals_preds, 1, min) / apply(actuals_preds, 1, max))
colnames(accuracy) <- "x"
accuracy <- accuracy[!is.infinite(rowSums(accuracy)),]
mean(accuracy, na.rm=TRUE)
R2(predict_lm, test$LOWER_MS_ANNUAL, na.rm=TRUE)
RMSE(predict_lm, test$LOWER_MS_ANNUAL, na.rm=TRUE)
RMSE(predict_lm, test$LOWER_MS_ANNUAL, na.rm=TRUE)/mean(test$LOWER_MS_ANNUAL, na.rm=TRUE)

#k-fold cross validation
set.seed(42)
train.control <- trainControl(method = "repeatedcv", 
                              number = 10, 
                              savePredictions=TRUE,
                              verboseIter=TRUE)

k_fold <- train(LOWER_MS_ANNUAL~CRLOWALBUMIN+SEX_NUM+AGE_INJ_NUM+
                  ASIA_LEVEL_DIS+REVIEWASIAGRADEADM, 
                data = ADDEP_1,
                method = "lm",
                trControl = train.control, na.action=na.omit)
k_fold



k_pred <- as.data.frame(k_fold$pred[1])
k_obs <- as.data.frame(k_fold$pred[2])
k_accuracy <- cbind(k_pred, k_obs)
k_acc <- data.frame(apply(k_accuracy, 1, min) / apply(k_accuracy, 1, max))
k_acc<- k_acc[!is.infinite(rowSums(k_acc)),]
mean(k_acc, na.rm=TRUE)


ADDEP$RATIO_ALB <- ADDEP$CRLOWALBUMIN/ADDEP$CRAVGALBUMIN

ggplot(data=ADDEP_1, aes(x=RATIO_ALB, y=LOWER_MS_ANNUAL))+
  geom_jitter()+
  facet_grid(.~REVIEWASIAGRADEADM)

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

MR_ALB<- ggplot(data=subset(ADDEP_2, !is.na(Marked_Recovery)), aes(x=Marked_Recovery, y=CRLOWALBUMIN))+
  geom_jitter()+
  geom_boxplot()+
  scale_y_continuous(limits=c(0,5))+
  cleanup+
  ggtitle("Lowest Recorded Alb across AIS-grades")+
  labs(x="Marked Recovery", y="Lowest Recorded Alb")+
  facet_grid(.~REVIEWASIAGRADEADM)

MR_PREALB <- ggplot(data=subset(ADDEP_2, !is.na(Marked_Recovery)), aes(x=Marked_Recovery, y=CRLOWPREALBUMIN))+
  geom_jitter()+
  geom_boxplot()+
  cleanup+
  scale_y_continuous(limits=c(0,1))+
  ggtitle("Lowest Recorded Pre-Alb across AIS-grades")+
  labs(x="Marked Recovery", y="Lowest Recorded Pre-Alb")+
  facet_grid(.~REVIEWASIAGRADEADM)

MR_HCT<- ggplot(data=subset(ADDEP_2, !is.na(Marked_Recovery)), aes(x=Marked_Recovery, y=CRLOWESTHCT))+
  geom_jitter()+
  geom_boxplot()+
  cleanup+
  ggtitle("Lowest Recorded HCT across AIS-grades")+
  labs(x="Marked Recovery", y="Lowest Recorded HCT")+
  facet_grid(.~REVIEWASIAGRADEADM)

MR_HGB<- ggplot(data=subset(ADDEP_1, !is.na(Marked_Recovery)), aes(x=Marked_Recovery, y=CRLOWESTHGB))+
  geom_jitter()+
  geom_boxplot()+
  cleanup+
  ggtitle("Lowest Recorded HGB across AIS-grades")+
  labs(x="Marked Recovery", y="Lowest Recorded HGB")+
  facet_grid(.~REVIEWASIAGRADEADM)

multiplot(MR_ALB, MR_PREALB, MR_HCT, MR_HGB, cols=2)

MR_ALB_1y <- ggplot(data=subset(ADDEP_2, !is.na(Marked_Recovery_Annual)), aes(x=Marked_Recovery_Annual, y=CRLOWALBUMIN))+
  geom_jitter()+
  geom_boxplot()+
  scale_y_continuous(limits=c(0,5))+
  cleanup+
  ggtitle("Lowest Recorded Alb across AIS-grades")+
  labs(x="Marked Recovery Annual", y="Lowest Recorded Alb")+
  facet_grid(.~REVIEWASIAGRADEADM)

MR_PREALB_1y <- ggplot(data=subset(ADDEP_2, !is.na(Marked_Recovery_Annual)), aes(x=Marked_Recovery_Annual, y=CRLOWPREALBUMIN))+
  geom_jitter()+
  geom_boxplot()+
  cleanup+
  scale_y_continuous(limits=c(0,1))+
  ggtitle("Lowest Recorded Pre-Alb across AIS-grades")+
  labs(x="Marked Recovery Annual", y="Lowest Recorded Pre-Alb")+
  facet_grid(.~REVIEWASIAGRADEADM)

MR_HCT_1y<- ggplot(data=subset(ADDEP_2, !is.na(Marked_Recovery_Annual)), aes(x=Marked_Recovery_Annual, y=CRLOWESTHCT))+
  geom_jitter()+
  geom_boxplot()+
  cleanup+
  ggtitle("Lowest Recorded HCT across AIS-grades")+
  labs(x="Marked Recovery Annual", y="Lowest Recorded HCT")+
  facet_grid(.~REVIEWASIAGRADEADM)

MR_HGB_1y<- ggplot(data=subset(ADDEP_2, !is.na(Marked_Recovery_Annual)), aes(x=Marked_Recovery_Annual, y=CRLOWESTHGB))+
  geom_jitter()+
  geom_boxplot()+
  cleanup+
  ggtitle("Lowest Recorded HGB across AIS-grades")+
  labs(x="Marked Recovery Annual", y="Lowest Recorded HGB")+
  facet_grid(.~REVIEWASIAGRADEADM)

multiplot(MR_ALB_1y,MR_PREALB_1y, MR_HCT_1y, MR_HGB_1y, cols=2)
#ASIA grades change in one year 
glm_1 <- glm(Marked_Recovery_Annual ~ REVIEWASIAGRADEADM+CRLOWALBUMIN+CRLOWPREALBUMIN, data=subset(ADDEP_2, !is.na(CRLOWPREALBUMIN)), family="binomial")

glm_1a <-glm(Marked_Recovery_Annual ~ REVIEWASIAGRADEADM, data=subset(ADDEP_2, !is.na(CRLOWALBUMIN)), family="binomial")

glm_1b <-glm(Marked_Recovery_Annual ~ REVIEWASIAGRADEADM, data=ADDEP_2, family="binomial")


anova(glm_1, glm_1a, test="Chisq")


prob_glm_1a <- predict(glm_1, newdata=ADDEP_2, type="response")
pred_glm_1a <- prediction(prob_glm_1a, ADDEP_2$Marked_Recovery_Annual)

#Plot for PR/ROC
library(ROCR)
perf_1a <- performance(pred_glm_1a, measure="prec", x.measure="rec")
plot(perf_1a)



#AUC for PR
library(PRROC)
pr_scores_1a <- na.omit(data.frame(prob_glm_1a,ADDEP_2$Marked_Recovery_Annual))
pr_1a <- pr.curve(scores.class0=pr_scores_1a[pr_scores_1a$ADDEP_2.Marked_Recovery_Annual=="1",]$prob_glm_1a,
               scores.class1=pr_scores_1a[pr_scores_1a$ADDEP_2.Marked_Recovery_Annual=="0",]$prob_glm_1a,
               curve=T)
pr_1a

curve_1a <- as.data.frame(pr_1a$curve)
glm_1a_pr <- ggplot(curve_1a, aes(curve_1a$V1, curve_1a$V2))+
  geom_path()+
  theme_bw()+
  cleanup+
  ggtitle("PR Curve for Albumin Multivariate Model")+
  labs(x="Recall", y="Precision")+
  annotate("text", x = 0.25, y=0.25, label = "AUC=0.76")

# Marked recovery 
training_noNA <- subset(training,!(is.na(training["CRLOWALBUMIN"]) | is.na(training["CRLOWPREALBUMIN"])))

glm_2 <- glm(Marked_Recovery ~ CRLOWALBUMIN+REVIEWASIAGRADEADM+SEX_NUM+AGE_INJ_NUM+ASIA_LEVEL_DIS, data=training, family="binomial")


prob_glm_2 <- predict(glm_2, newdata=test, type="response")
pred_glm_2 <- prediction(prob_glm_2, test$Marked_Recovery)

#Plot for PR/ROC
library(ROCR)
perf <- performance(pred_glm_2, "tpr", "fpr")
plot(perf)

glm_2_pred <- ifelse(prob_glm_2>0.14, 1, 0)
glm_2_accuracy <- table(glm_2_pred, test$Marked_Recovery)
sensitivity(glm_2_accuracy)
specificity(glm_2_accuracy)
confusionMatrix(glm_2_accuracy)

#AUC for PR
pr_scores <- na.omit(data.frame(prob_glm_2, test$Marked_Recovery))
pr <- pr.curve(scores.class0=pr_scores[pr_scores$test.Marked_Recovery=="1",]$prob_glm_2,
               scores.class1=pr_scores[pr_scores$test.Marked_Recovery=="0",]$prob_glm_2,
               curve=T)
pr

curve_pr <- as.data.frame(pr$curve)
pr_only <- curve_pr[,c("V1", "V2")]
pr_only$models <- rep(1,nrow(pr_only))

curve <- as.data.frame(pr$curve)
glm_2_pr <- ggplot(curve, aes(curve$V1, curve$V2))+
  geom_path()+
  theme_bw()+
  cleanup+
  ggtitle("PR Curve for Albumin Multivariate Model")+
  labs(x="Recall", y="Precision")+
  annotate("text", x = 0.25, y=0.25, label = "AUC=0.76")


#Another different ways to calculate AUC for PR

x <- perf@x.values[[1]] 
y <- perf@y.values[[1]] 
#Because y has NaN values so we need to get rid of it
y <- na.omit(y)
#Now making x the same lenght as y by removing one of them
x <- head(x, -1)

library(pracma)
auc_pr=trapz(x,y)


auc_glm_2 <- performance(pred_glm_2, measure="auc")
auc_glm_2@y.values[[1]]

#K-fold
#k-fold cross validation
set.seed(42)
train.control <- trainControl(method = "repeatedcv", 
                              number = 10, 
                              savePredictions=TRUE,
                              verboseIter=TRUE)

k_fold_glm_2 <- train(Marked_Recovery_Annual~CRLOWPREALBUMIN+SEX_NUM+AGE_INJ_NUM+
                  ASIA_LEVEL_DIS+ASIA_DISCHARGE, 
                data = ADDEP_2,
                method = "glm",
                trControl = train.control, na.action=na.omit)
k_fold_glm_2

k_pred_glm_2 <- as.data.frame(k_fold_glm_2$pred[1])
k_obs_glm_2 <- as.data.frame(k_fold_glm_2$pred[2])
k_accuracy_glm_2 <- cbind(k_pred_glm_2, k_obs_glm_2)

pr_k_glm_2 <- roc.curve(scores.class0=k_accuracy_glm_2[k_accuracy_glm_2$obs=="1",]$pred,
               scores.class1=k_accuracy_glm_2[k_accuracy_glm_2$obs=="0",]$pred,
               curve=T)
pr_k_glm_2

curve_k_glm_2 <- as.data.frame(pr_k_glm_2$curve)
glm_2_pr <- ggplot(curve_k_glm_2, aes(curve_k_glm_2$V1, curve_k_glm_2$V2))+
  geom_path()


#Individual auc for ASIA grades
plot(roc(Marked_Recovery ~ factor(training$REVIEWASIAGRADEADM, levels=c("A", "B", "C", "D"), ordered=TRUE), data=training))

#GlM for pre-albumin
glm_3 <- glm(Marked_Recovery ~ CRLOWPREALBUMIN+REVIEWASIAGRADEADM+SEX_NUM+AGE_INJ_NUM+ASIA_LEVEL_DIS, 
             data=training, family="binomial")



prob_glm_3 <- predict(glm_3, newdata=test, type="response", se.fit=FALSE)
pred_glm_3 <- prediction(prob_glm_3, test$Marked_Recovery)

#Plot for PR/ROC
library(ROCR)
perf_3 <- performance(pred_glm_3, measure="prec", x.measure="rec")
plot(perf_3)

#AUC for PR
pr_scores_3 <- na.omit(data.frame(prob_glm_3, test$Marked_Recovery))
pr_3 <- pr.curve(scores.class0=pr_scores_3[pr_scores_3$test.Marked_Recovery=="1",]$prob_glm_3,
               scores.class1=pr_scores_3[pr_scores_3$test.Marked_Recovery=="0",]$prob_glm_3,
               curve=T)

pr_3

curve_pr_3 <- as.data.frame(pr_3$curve)
pr_3_only <- curve_pr_3[,c("V1", "V2")]
pr_3_only$models <- rep(2,nrow(pr_3_only))

curve_3 <- as.data.frame(pr_3$curve)
glm_3_pr <- ggplot(curve_3, aes(curve_3$V1, curve_3$V2))+
  geom_path()+
  theme_bw()+
  cleanup+
  ggtitle("PR Curve for Pre-Albumin Multivariate Model")+
  labs(x="Recall", y="Precision")+
  annotate("text", x = 0.25, y=0.15, label = "AUC=0.54")


auc_glm_3 <- performance(pred_glm_3, measure="auc")
auc_glm_3@y.values[[1]]

#GLM for HCT
glm_4 <- glm(Marked_Recovery ~ CRLOWESTHCT+REVIEWASIAGRADEADM+SEX_NUM+AGE_INJ_NUM+ASIA_LEVEL_DIS, data=training, family="binomial")


prob_glm_4 <- predict(glm_4, newdata=test, type="response")
pred_glm_4 <- prediction(prob_glm_4, test$Marked_Recovery)

#Plot for PR/ROC
library(ROCR)
perf_4 <- performance(pred_glm_4, measure="prec", x.measure="rec")
plot(perf_4)

#AUC for PR
pr_scores_4 <- na.omit(data.frame(prob_glm_4, test$Marked_Recovery))
pr_4 <- pr.curve(scores.class0=pr_scores_4[pr_scores_4$test.Marked_Recovery=="1",]$prob_glm_4,
                 scores.class1=pr_scores_4[pr_scores_4$test.Marked_Recovery=="0",]$prob_glm_4,
                 curve=T)
pr_4

curve_pr_4 <- as.data.frame(pr_4$curve)
pr_4_only <- curve_pr_4[,c("V1", "V2")]
pr_4_only$models <- rep(3,nrow(pr_4_only))

curve_4 <- as.data.frame(pr_4$curve)
glm_4_pr <- ggplot(curve_4, aes(curve_4$V1, curve_4$V2))+
  geom_path()+
  theme_bw()+
  cleanup+
  ggtitle("PR Curve for HCT Multivariate Model")+
  labs(x="Recall", y="Precision")+
  annotate("text", x = 0.25, y=0.25, label = "AUC=0.72")


auc_glm_4 <- performance(pred_glm_4, measure="auc")
auc_glm_4@y.values[[1]]

#GLM for HGB
glm_5 <- glm(Marked_Recovery ~ CRLOWESTHGB+REVIEWASIAGRADEADM+SEX_NUM+AGE_INJ_NUM+ASIA_LEVEL_DIS, data=training, family="binomial")


prob_glm_5 <- predict(glm_5, newdata=test, type="response")
pred_glm_5 <- prediction(prob_glm_5, test$Marked_Recovery)

#Plot for PR/ROC
library(ROCR)
perf_5 <- performance(pred_glm_5, measure="prec", x.measure="rec")
plot(perf_5)

#AUC for PR
pr_scores_5 <- na.omit(data.frame(prob_glm_5, test$Marked_Recovery))
pr_5 <- pr.curve(scores.class0=pr_scores_5[pr_scores_5$test.Marked_Recovery=="1",]$prob_glm_5,
                 scores.class1=pr_scores_5[pr_scores_5$test.Marked_Recovery=="0",]$prob_glm_5,
                 curve=T)
pr_5

curve_pr_5 <- as.data.frame(pr_5$curve)
pr_5_only <- curve_pr_5[,c("V1", "V2")]
pr_5_only$models <- rep(4,nrow(pr_5_only))

curve_5 <- as.data.frame(pr_5$curve)
glm_5_pr <- ggplot(curve_5, aes(curve_5$V1, curve_5$V2))+
  geom_path()+
  theme_bw()+
  cleanup+
  ggtitle("PR Curve for HGB Multivariate Model")+
  labs(x="Recall", y="Precision")+
  annotate("text", x = 0.25, y=0.25, label = "AUC=0.73")

multiplot(glm_2_pr, glm_3_pr, glm_4_pr, glm_5_pr, cols=2)

#AIS only 
glm_6 <- glm(Marked_Recovery ~REVIEWASIAGRADEADM, data=training, family="binomial")


prob_glm_6 <- predict(glm_6, newdata=test, type="response")
pred_glm_6 <- prediction(prob_glm_6, test$Marked_Recovery)

#Plot for PR/ROC
library(ROCR)


pr_scores_6 <- na.omit(data.frame(prob_glm_6, test$Marked_Recovery))
pr_6 <-pr.curve(scores.class0=pr_scores_6[pr_scores_6$test.Marked_Recovery=="1",]$prob_glm_6,
                  scores.class1=pr_scores_6[pr_scores_6$test.Marked_Recovery=="0",]$prob_glm_6,
                  curve=T)
pr_6

curve_pr_6 <- as.data.frame(pr_6$curve)
pr_6_only <- curve_pr_6[,c("V1", "V2")]
pr_6_only$models <- rep(5,nrow(pr_6_only))

glm_6_pr <- ggplot(curve_pr_6, aes(curve_pr_6$V1, curve_pr_6$V2))+
  geom_path()+
  theme_bw()+
  cleanup+
  ggtitle("ROC Curve for HGB Multivariate Model")+
  labs(x="Specificity", y="Sensitivity")+
  annotate("text", x = 0.75, y=0.25, label = "AUC=0.89")

# PRs curve for all

pr_all <- merge_all(list(pr_only, pr_3_only,pr_4_only, pr_5_only, pr_6_only), by=c("models", "V1", "V2"))
pr_all$models <- factor(pr_all$models, levels=c("1", "2", "3", "4", "5"), labels=c("Albumin", "Pre-albumin", 
                                                                                       "HCT", "HGB", "only AIS"))

ggplot(pr_all, aes(pr_all$V1, pr_all$V2, colour=models))+
  geom_path()+
  theme_bw()+
  cleanup+
  ggtitle("PR Curves for Multivariate Models")+
  labs(x="Recall", y="Precision", fill="Multivariate Models")+
  labs(caption = "AUC For Albumin/HCT/HGB = 0.60
       AUC for Pre-albumin = 0.08
       AUC for AIS only =0.55")



#Marked Recovery at 1 year 
#Low Alb

glm_2_1y <- glm(Marked_Recovery_Annual ~ CRLOWALBUMIN+REVIEWASIAGRADEADM+SEX_NUM+AGE_INJ_NUM+ASIA_LEVEL_DIS, data=training, family="binomial")


prob_glm_2_1y <- predict(glm_2_1y, newdata=test, type="response")
pred_glm_2_1y <- prediction(prob_glm_2_1y, test$Marked_Recovery_Annual)

#Plot for PR/ROC
library(ROCR)
perf_1y <- performance(pred_glm_2_1y, "sens", "spec")
plot(perf_1y)

glm_2_pred_1y <- ifelse(prob_glm_2_1y>0.27, 1, 0)
glm_2_accuracy_1y <- table(glm_2_pred_1y, test$Marked_Recovery_Annual)
sensitivity(glm_2_accuracy_1y)
specificity(glm_2_accuracy_1y)
confusionMatrix(glm_2_accuracy_1y)

#AUC for ROC
roc_scores_1y <- na.omit(data.frame(prob_glm_2_1y, test$Marked_Recovery_Annual))
roc <-roc.curve(scores.class0=roc_scores_1y[roc_scores_1y$test.Marked_Recovery_Annual=="1",]$prob_glm_2_1y,
               scores.class1=roc_scores_1y[roc_scores_1y$test.Marked_Recovery_Annual=="0",]$prob_glm_2_1y,
               curve=T)
roc

curve_roc <- as.data.frame(roc$curve)
roc_only <- curve_roc[,c("V1", "V2")]
roc_only$models <- rep(1,nrow(roc_only))

glm_2_roc_1y <- ggplot(curve_roc, aes(curve_roc$V1, curve_roc$V2))+
  geom_path()+
  theme_bw()+
  cleanup+
  ggtitle("ROC Curve for Albumin Multivariate Model")+
  labs(x="Specificity", y="Sensitivity")+
  annotate("text", x = 0.75, y=0.25, label = "AUC=0.81")

#pre-alb

glm_3_1y <- glm(Marked_Recovery_Annual ~ CRLOWPREALBUMIN+REVIEWASIAGRADEADM+SEX_NUM+AGE_INJ_NUM+ASIA_LEVEL_DIS, data=training, family="binomial")


prob_glm_3_1y <- predict(glm_3_1y, newdata=test, type="response")
pred_glm_3_1y <- prediction(prob_glm_3_1y, test$Marked_Recovery_Annual)

#Plot for PR/ROC
library(ROCR)
perf_1y <- performance(pred_glm_3_1y, "tpr", "fpr")
plot(perf_1y)

glm_3_pred_1y <- ifelse(prob_glm_3_1y>0.22, 1, 0)
glm_3_accuracy_1y <- table(glm_3_pred_1y, test$Marked_Recovery_Annual)
sensitivity(glm_3_accuracy_1y)
specificity(glm_3_accuracy_1y)
confusionMatrix(glm_3_accuracy_1y)

#AUC for ROC
roc_scores_3_1y <- na.omit(data.frame(prob_glm_3_1y, test$Marked_Recovery_Annual))
roc_3 <-roc.curve(scores.class0=roc_scores_3_1y[roc_scores_3_1y$test.Marked_Recovery_Annual=="1",]$prob_glm_3_1y,
                scores.class1=roc_scores_3_1y[roc_scores_3_1y$test.Marked_Recovery_Annual=="0",]$prob_glm_3_1y,
                curve=T)
roc_3

curve_roc_3 <- as.data.frame(roc_3$curve)
roc_3_only <- curve_roc_3[,c("V1", "V2")]
roc_3_only$models <- rep(2,nrow(roc_3_only))

glm_3_roc_1y <- ggplot(curve_roc_3, aes(curve_roc_3$V1, curve_roc_3$V2))+
  geom_path()+
  theme_bw()+
  cleanup+
  ggtitle("ROC Curve for Pre-Albumin Multivariate Model")+
  labs(x="Specificity", y="Sensitivity")+
  annotate("text", x = 0.75, y=0.25, label = "AUC=0.79")

#HCT

glm_4_1y <- glm(Marked_Recovery_Annual ~ CRLOWESTHCT+REVIEWASIAGRADEADM+SEX_NUM+AGE_INJ_NUM+ASIA_LEVEL_DIS, data=training, family="binomial")


prob_glm_4_1y <- predict(glm_4_1y, newdata=test, type="response")
pred_glm_4_1y <- prediction(prob_glm_4_1y, test$Marked_Recovery_Annual)

#Plot for PR/ROC
library(ROCR)
perf_1y <- performance(pred_glm_4_1y, "prec", "rec")
plot(perf_1y)

glm_4_pred_1y <- ifelse(prob_glm_4_1y>0.22, 1, 0)
glm_4_accuracy_1y <- table(glm_4_pred_1y, test$Marked_Recovery_Annual)
sensitivity(glm_4_accuracy_1y)
specificity(glm_4_accuracy_1y)
confusionMatrix(glm_4_accuracy_1y)

#AUC for ROC
roc_scores_4_1y <- na.omit(data.frame(prob_glm_4_1y, test$Marked_Recovery_Annual))
roc_4 <-roc.curve(scores.class0=roc_scores_4_1y[roc_scores_4_1y$test.Marked_Recovery_Annual=="1",]$prob_glm_4_1y,
                  scores.class1=roc_scores_4_1y[roc_scores_4_1y$test.Marked_Recovery_Annual=="0",]$prob_glm_4_1y,
                  curve=T)
roc_4

curve_roc_4 <- as.data.frame(roc_4$curve)
roc_4_only <- curve_roc_4[,c("V1", "V2")]
roc_4_only$models <- rep(3,nrow(roc_4_only))

glm_4_roc_1y <- ggplot(curve_roc_4, aes(curve_roc_4$V1, curve_roc_4$V2))+
  geom_path()+
  theme_bw()+
  cleanup+
  ggtitle("ROC Curve for HCT Multivariate Model")+
  labs(x="Specificity", y="Sensitivity")+
  annotate("text", x = 0.75, y=0.25, label = "AUC=0.90")

#HGB

glm_5_1y <- glm(Marked_Recovery_Annual ~ CRLOWESTHGB+REVIEWASIAGRADEADM+SEX_NUM+AGE_INJ_NUM+ASIA_LEVEL_DIS, data=training, family="binomial")


prob_glm_5_1y <- predict(glm_5_1y, newdata=test, type="response")
pred_glm_5_1y <- prediction(prob_glm_5_1y, test$Marked_Recovery_Annual)

#Plot for PR/ROC
library(ROCR)
perf_1y <- performance(pred_glm_4_1y, "prec", "rec")
plot(perf_1y)

glm_5_pred_1y <- ifelse(prob_glm_5_1y>0.22, 1, 0)
glm_5_accuracy_1y <- table(glm_5_pred_1y, test$Marked_Recovery_Annual)
sensitivity(glm_5_accuracy_1y)
specificity(glm_5_accuracy_1y)
confusionMatrix(glm_5_accuracy_1y)

#AUC for all ROCs
roc_scores_5_1y <- na.omit(data.frame(prob_glm_5_1y, test$Marked_Recovery_Annual))
roc_5 <-roc.curve(scores.class0=roc_scores_5_1y[roc_scores_5_1y$test.Marked_Recovery_Annual=="1",]$prob_glm_5_1y,
                  scores.class1=roc_scores_5_1y[roc_scores_5_1y$test.Marked_Recovery_Annual=="0",]$prob_glm_5_1y,
                  curve=T)
roc_5

curve_roc_5 <- as.data.frame(roc_5$curve)
roc_5_only <- curve_roc_5[,c("V1", "V2")]
roc_5_only$models <- rep(4,nrow(roc_5_only))

glm_5_roc_1y <- ggplot(curve_roc_5, aes(curve_roc_5$V1, curve_roc_5$V2))+
  geom_path()+
  theme_bw()+
  cleanup+
  ggtitle("ROC Curve for HGB Multivariate Model")+
  labs(x="Specificity", y="Sensitivity")+
  annotate("text", x = 0.75, y=0.25, label = "AUC=0.89")

#only ASIA grades

glm_6_1y <- glm(Marked_Recovery_Annual ~ REVIEWASIAGRADEADM, data=training, family="binomial")


prob_glm_6_1y <- predict(glm_6_1y, newdata=test, type="response")
pred_glm_6_1y <- prediction(prob_glm_6_1y, test$Marked_Recovery_Annual)

#Plot for PR/ROC
library(ROCR)
perf_1y <- performance(pred_glm_4_1y, "prec", "rec")
plot(perf_1y)

glm_6_pred_1y <- ifelse(prob_glm_6_1y>0.23, 1, 0)
glm_6_accuracy_1y <- table(glm_6_pred_1y, test$Marked_Recovery_Annual)
sensitivity(glm_6_accuracy_1y)
specificity(glm_6_accuracy_1y)
confusionMatrix(glm_6_accuracy_1y)

#AUC for all ROCs
roc_scores_6_1y <- na.omit(data.frame(prob_glm_6_1y, test$Marked_Recovery_Annual))
roc_6 <-roc.curve(scores.class0=roc_scores_6_1y[roc_scores_6_1y$test.Marked_Recovery_Annual=="1",]$prob_glm_6_1y,
                  scores.class1=roc_scores_6_1y[roc_scores_6_1y$test.Marked_Recovery_Annual=="0",]$prob_glm_6_1y,
                  curve=T)
roc_6

curve_roc_6 <- as.data.frame(roc_6$curve)
roc_6_only <- curve_roc_6[,c("V1", "V2")]
roc_6_only$models <- rep(5,nrow(roc_6_only))

glm_6_roc_1y <- ggplot(curve_roc_6, aes(curve_roc_6$V1, curve_roc_6$V2))+
  geom_path()+
  theme_bw()+
  cleanup+
  ggtitle("ROC Curve for HGB Multivariate Model")+
  labs(x="Specificity", y="Sensitivity")+
  annotate("text", x = 0.75, y=0.25, label = "AUC=0.89")

#plots for all rocs
rocs_all <- merge_all(list(roc_only, roc_6_only), by=c("models", "V1", "V2"))
rocs_all$models <- factor(rocs_all$models, levels=c("1", "5"), labels=c("Albumin + confounders", "only AIS"))

ggplot(data=rocs_all, aes(rocs_all$V1, rocs_all$V2, colour=models))+
  geom_path()+
  theme_bw()+
  cleanup+
  labs(x="1 - Specificity", y="Sensitivity", fill="Multivariate Models")+
  theme(axis.text=element_text(size=14),
        axis.title=element_text(size=18,face="bold"),
        strip.text.x = element_text(size = 14))+
  geom_abline(intercept =0 , slope = 1)+
  annotate("text", x=0.65, y=0.25, label= "AUC for Albumin + confounders = 0.9
       AUC for AIS only = 0.86")

#URP
URP_MR <- ctree(Marked_Recovery_Annual ~ CRLOWALBUMIN+CRLOWPREALBUMIN, data=subset(ADDEP_2, !is.na(Marked_Recovery_Annual)&REVIEWASIAGRADEADM=="D"))
plot(URP_MR)


walk <- data.frame(table(ADDEP_1$AFLMODDS,ADDEP_1$ASIA_CHANGE_updated))
names(walk) <- c("Walk","Converted","Count")

ggplot(data=walk, aes(x=Converted, y=Count, fill=Walk)) + geom_bar(stat="identity")


stargazer(cbind("OR"=exp(coef(glm_2_1y)),
                exp(confint(glm_2_1y)),
                "SE" = summary(glm_2_1y)$coefficients[,2],
                "P value" = summary(glm_2_1y)$coefficients[,4]), type = "text")

describeBy(ADDEP_2$AGE_INJ, group=is.na(ADDEP_2$Marked_Recovery_Annual))

stargazer(cbind("Marked Recovery"=summary(ADDEP_2$Marked_Recovery),
                "Sex"=summary(ADDEP_2$SEX)), type = "text")

