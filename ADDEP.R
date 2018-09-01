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

ADDEP_noNA <- ADDEP[!is.na(ADDEP$LOWER_MS_ANNUAL),]

ADDEP_noNA <- ADDEP[c("LOWER_MS_ANNUAL", "RATIO_ALB")]
ADDEP_noNA <- ADDEP_noNA[!is.na(ADDEP_noNA$LOWER_MS_ANNUAL),]

ADDEP_noASIA <- ADDEP_1[ ! (ADDEP_1$ASIA_LEVEL_DIS=="I" ), ] 

cv.lm(data=ADDEP_noNA, form.lm=formula(LOWER_MS_ANNUAL~RATIO_ALB), m=5, 
      dots=FALSE, seed=29, plotit = TRUE, printit=TRUE)

#Lower Motor Scores 1 year 
lm_1 <- lm(LOWER_MS_ANNUAL~CRLOWALBUMIN,subset = REVIEWASIAGRADEADM == "A", data=ADDEP_1)

ggplot(data=ADDEP, aes(y=CRLOWPREALBUMIN, x=ASIA_REHAB))+
  geom_boxplot()+
  geom_jitter()+
  scale_y_continuous(limits = c(0,10))

ggplot(data=ADDEP, aes(y=CRAVGALBUMIN, x=LOWER_MS_ANNUAL))+
  geom_jitter()+
  scale_y_continuous(limits=c(1,5))+
  facet_grid(.~ASIA_REHAB)
  geom_smooth(method="lm", formula=y~x, se=FALSE)

stargazer(cbind("Estimate"=coef(lm_1),
                confint(lm_1),
                "Naive SE" = summary(lm_1)$coefficients[,2],
                "Robust SE" = sqrt(diag(vcovHC(lm_1, type="HC"))),
                "P value" = summary(lm_1)$coefficients[,4]), type = "text")

#Check influential points
ols_plot_resid_lev(lm_1)
influential_obs <- influence.measures(lm_1)

ADDEP_influential_obs <- which(apply(influential_obs$is.inf, 1, any)) 
ADDEP_sans_influential_obs <- ADDEP_1[-ADDEP_influential_obs,] 

lm_2 <- lm(LOWER_MS_ANNUAL~CRLOWALBUMIN*REVIEWASIAGRADEADM+SEX_NUM+AGE_INJ_NUM+
             SPL_SURG_NUM, data=ADDEP_1)

#heteroscedasticity corrected se
coeftest(lm_1, vcov. = vcovHC(lm_1))

#with prealbumin
lm_3 <- lm(LOWER_MS_DISCHARGE~CRLOWPREALBUMIN+SEX_NUM+AGE_INJ_NUM+
  SPL_SURG_NUM+REVIEWASIAGRADEADM, data=ADDEP_1)

lm_4 <- lm(LOWER_MS_ANNUAL~CRLOWALBUMIN+SEX_NUM+AGE_INJ_NUM+
  SPL_SURG_NUM+REVIEWASIAGRADEADM, data= subset(ADDEP_1, !is.na(CRLOWPREALBUMIN)))

#Set-validation 
set.seed(100)
sample <- sample(seq(1, nrow(ADDEP_1)), replace=FALSE)
training <- ADDEP_1[sample[1:1200],]
test <-ADDEP_1[sample[1201:nrow(ADDEP_1)],]


smp_size <- floor(0.95 * nrow(ADDEP_noASIA))

set.seed(123)
train_ind <- sample(seq_len(nrow(ADDEP_noASIA)), size = smp_size)

training <- ADDEP_noASIA[train_ind, ]
test <- ADDEP_noASIA[-train_ind, ]

lm_training <- lm(LOWER_MS_ANNUAL~CRLOWALBUMIN*REVIEWASIAGRADEADM+SEX_NUM+AGE_INJ_NUM
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

k_fold <- train(LOWER_MS_ANNUAL~CRLOWESTHCT+SEX_NUM+AGE_INJ_NUM+
                  ASIA_LEVEL_DIS+ASIA_DISCHARGE, 
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

MR_ALB<- ggplot(data=subset(ADDEP_1, !is.na(Marked_Recovery)), aes(x=Marked_Recovery, y=CRLOWALBUMIN))+
  geom_jitter()+
  geom_boxplot()+
  scale_y_continuous(limits=c(0,5))+
  cleanup+
  ggtitle("Lowest Recorded Alb across AIS-grades")+
  labs(x="Marked Recovery", y="Lowest Recorded Alb")+
  facet_grid(.~REVIEWASIAGRADEADM)

MR_PREALB <- ggplot(data=subset(ADDEP_1, !is.na(Marked_Recovery)), aes(x=Marked_Recovery, y=CRLOWPREALBUMIN))+
  geom_jitter()+
  geom_boxplot()+
  cleanup+
  scale_y_continuous(limits=c(0,1))+
  ggtitle("Lowest Recorded Pre-Alb across AIS-grades")+
  labs(x="Marked Recovery", y="Lowest Recorded Pre-Alb")+
  facet_grid(.~REVIEWASIAGRADEADM)

MR_HCT<- ggplot(data=subset(ADDEP_1, !is.na(Marked_Recovery)), aes(x=Marked_Recovery, y=CRLOWESTHCT))+
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

#ASIA grades change in one year 
glm_1 <- glm(Marked_Recovery ~ REVIEWASIAGRADEADM+CRLOWALBUMIN, data=subset(ADDEP_1, !is.na(CRLOWALBUMIN)), family="binomial")

glm_1a <-glm(Marked_Recovery ~ REVIEWASIAGRADEADM, data=subset(ADDEP_1, !is.na(CRLOWALBUMIN)), family="binomial")

anova(glm_1, glm_1a, test="LRT")

prob_glm_1a <- predict(glm_1, newdata=ADDEP_1, type="response")
pred_glm_1a <- prediction(prob_glm_1a, ADDEP_1$Marked_Recovery)

#Plot for PR/ROC
library(ROCR)
perf_1a <- performance(pred_glm_1a, measure="prec", x.measure="rec")
plot(perf_1a)

#AUC for PR
pr_scores_1a <- na.omit(data.frame(prob_glm_1a,ADDEP_1$Marked_Recovery))
pr_1a <- pr.curve(scores.class0=pr_scores_1a[pr_scores_1a$ADDEP_1.Marked_Recovery=="1",]$prob_glm_1a,
               scores.class1=pr_scores_1a[pr_scores_1a$ADDEP_1.Marked_Recovery=="0",]$prob_glm_1a,
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
perf <- performance(pred_glm_2, measure="prec", x.measure="rec")
plot(perf)

#AUC for PR
pr_scores <- na.omit(data.frame(prob_glm_2, test$Marked_Recovery))
pr <- pr.curve(scores.class0=pr_scores[pr_scores$test.Marked_Recovery=="1",]$prob_glm_2,
               scores.class1=pr_scores[pr_scores$test.Marked_Recovery=="0",]$prob_glm_2,
               curve=T)
pr

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

k_fold_glm_2 <- train(Marked_Recovery~CRLOWESTHCT+SEX_NUM+AGE_INJ_NUM+
                  ASIA_LEVEL_DIS+ASIA_DISCHARGE, 
                data = ADDEP_1,
                method = "glm",
                trControl = train.control, na.action=na.omit)
k_fold_glm_2

k_pred_glm_2 <- as.data.frame(k_fold_glm_2$pred[1])
k_obs_glm_2 <- as.data.frame(k_fold_glm_2$pred[2])
k_accuracy_glm_2 <- cbind(k_pred_glm_2, k_obs_glm_2)

pr_k_glm_2 <- pr.curve(scores.class0=k_accuracy_glm_2[k_accuracy_glm_2$obs=="1",]$pred,
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


curve_5 <- as.data.frame(pr_5$curve)
glm_5_pr <- ggplot(curve_5, aes(curve_5$V1, curve_5$V2))+
  geom_path()+
  theme_bw()+
  cleanup+
  ggtitle("PR Curve for HGB Multivariate Model")+
  labs(x="Recall", y="Precision")+
  annotate("text", x = 0.25, y=0.25, label = "AUC=0.73")

multiplot(glm_2_pr, glm_3_pr, glm_4_pr, glm_5_pr, cols=2)

auc_glm_4 <- performance(pred_glm_4, measure="auc")
auc_glm_4@y.values[[1]]



walk <- data.frame(table(ADDEP_1$AFLMODDS,ADDEP_1$ASIA_CHANGE_updated))
names(walk) <- c("Walk","Converted","Count")

ggplot(data=walk, aes(x=Converted, y=Count, fill=Walk)) + geom_bar(stat="identity")
