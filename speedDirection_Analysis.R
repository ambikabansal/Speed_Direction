require(dplyr)
require(lme4)
require(ggplot2)
require(cowplot)
require(openxlsx)
theme_set(theme_cowplot())
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

k = 0
flist = list.files("Data", full.names = TRUE)
for (j in flist){
  if (k == 0){
    Data = read.csv(j, header = FALSE)
    Data$ID = j
    k = 1
  } else{
    Data_Temp = read.csv(j, header = FALSE)
    Data_Temp$ID = j
    Data = rbind(Data,Data_Temp)
  }
}

colnames(Data) = c("Speed","Direction","Data","Distance_Real", "Task", "Participant")


#outlier removal
Dataframe_NoOutliers = Data %>% 
  mutate (Miss_Trials = case_when(           
    Data/Speed < 0.5 ~ "Outlier", 
    TRUE ~ "No Outlier")) %>%
  filter(Miss_Trials == "No Outlier")
Dataframe_NoOutliers = Dataframe_NoOutliers %>% 
  group_by(Distance_Real, Task, Direction, Speed) %>%
  mutate(IQR = IQR(Data),
         LowerQuartile = quantile(Data, prob=c(.25)),
         UpperQuartile = quantile(Data, prob=c(.75)),
         Outlier = case_when(
           Data > UpperQuartile+1.5*IQR | Data < LowerQuartile-1.5*IQR ~ "Outlier",
           TRUE ~ "No Outlier")) %>%
  filter(Outlier == "No Outlier") %>%
  ungroup()

#write.csv(Dataframe_NoOutliers, "C:\\Users\\ambika\\Documents\\AT_noOutliers.csv")


require(tidyr)
Data_Temp = Dataframe_NoOutliers %>% filter(Participant != "Data/P7.csv" & Outlier == "No Outlier") %>%
          select(Speed, Direction, Distance_Real, Participant, Data, Task)

Data2 <- spread(Data_Temp, Task, Data)


#invert the thing:
Data_Temp = Data_Temp %>%
  mutate(Actual_Travel_Distance = case_when(
           Task == 1 ~ Data,
           Task == 2 ~ Distance_Real),
         Perceived_Travel_Distance = case_when(
           Task == 1 ~ Distance_Real,
           Task == 2 ~ Data),
         Gain = Perceived_Travel_Distance/Actual_Travel_Distance)


library(lmerTest)


###Testing Differences Between Tasks###
#######################################

# Model_Tasks = lmer(Gain ~ as.factor(Task) + (as.factor(Speed) + as.factor(Direction) + as.factor(Task) + as.factor(Distance_Real)| Participant),
#                           data = Data_Temp,
#                           #control = lmerControl(optCtrl = list(maxfun = 1000000), optimizer = "bobyqa"))
#                           control = lmerControl(optimizer = "bobyqa"))
#  confint(Model_Tasks, method = "boot", level = 0.95)
#  summary(Model_Tasks)
# 
# Model_Tasks_Speed = lmer(Gain ~ as.factor(Task) + (as.factor(Direction) + as.factor(Task) + as.factor(Distance_Real)| Participant),
#                     data = Data_Temp,
#                     #control = lmerControl(optCtrl = list(maxfun = 1000000), optimizer = "bobyqa"))
#                     control = lmerControl(optimizer = "bobyqa"))
#  #confint(Model1_Everything, method = "boot", level = 0.95)
#  summary(Model_Tasks_Speed)
# 
# Model_Tasks_Direction = lmer(Gain ~ as.factor(Task) + (as.factor(Speed) + as.factor(Task) + as.factor(Distance_Real)| Participant),
#                     data = Data_Temp,
#                     #control = lmerControl(optCtrl = list(maxfun = 1000000), optimizer = "bobyqa"))
#                     control = lmerControl(optimizer = "bobyqa"))
#  #confint(Model1_Everything, method = "boot", level = 0.95)
#  summary(Model_Tasks_Direction)
# 
# Model_Tasks_Distance = lmer(Gain ~ as.factor(Task) + (as.factor(Speed) + as.factor(Direction) + as.factor(Task)| Participant),
#                     data = Data_Temp,
#                     #control = lmerControl(optCtrl = list(maxfun = 1000000), optimizer = "bobyqa"))
#                     control = lmerControl(optimizer = "bobyqa"))
#  #confint(Model1_Everything, method = "boot", level = 0.95)
#  summary(Model_Tasks_Distance)
# 
# 
# Model_Tasks_Tasks = lmer(Gain ~ as.factor(Task) + (as.factor(Speed) + as.factor(Direction) + as.factor(Distance_Real)| Participant),
#                     data = Data_Temp,
#                     #control = lmerControl(optCtrl = list(maxfun = 1000000), optimizer = "bobyqa"))
#                     control = lmerControl(optimizer = "bobyqa"))
#  #confint(Model1_Everything, method = "boot", level = 0.95)
#  summary(Model_Tasks_Tasks)
# 
#  anova(Model_Tasks, Model_Tasks_Tasks)



#Figure 2

tasklabels <- c('1' = "Move-To-Target", '2' = "Adjust-Target")

ggplot(Data_Temp,aes(as.factor(Speed), Gain, color=as.factor(Direction))) +
  geom_boxplot() + 
  facet_grid(Task~., labeller = labeller(Task = tasklabels)) +
  labs (x = "Speed (m/s)") +
  geom_hline(yintercept = 1, linetype = "dashed") +
  scale_color_manual(name = "Direction", labels = c("Forward", "Backward", "Up", "Down" ), values = c("red", "blue", "forestgreen", "orange"))


ggplot(Data_Temp,aes(as.factor(Direction), Gain, color=as.factor(Speed))) +
  geom_boxplot() + 
  facet_grid(Task~., labeller = labeller(Task = tasklabels)) +
  labs (x = "Direction") +
  scale_x_discrete(labels = c("Forward", "Backward", "Up", "Down" )) +
  geom_hline(yintercept = 1, linetype = "dashed") +
  scale_color_manual(name = "Speed", labels = c("1 m/s", "3 m/s", "5 m/s"), values = c("red", "blue", "forestgreen", "orange"))


ggplot(Data_Temp,aes(as.factor(Direction), Gain, color=as.factor(Direction))) +
  geom_boxplot() + 
  facet_grid(Task~., labeller = labeller(Task = tasklabels)) +
  labs (x = "Direction") +
  scale_x_discrete(labels = c("Forward", "Backward", "Up", "Down" )) +
  geom_hline(yintercept = 1, linetype = "dashed") +
  theme(legend.position="none") +
  scale_color_manual(values = c("red", "blue", "forestgreen", "orange"))


ggplot(Data_Temp,aes(as.factor(Speed), Gain, color=as.factor(Speed))) +
  geom_boxplot() + 
  facet_grid(Task~., labeller = labeller(Task = tasklabels)) +
  labs (x = "Speed (m/s)") +
  geom_hline(yintercept = 1, linetype = "dashed") +
  theme(legend.position="none") +
  scale_color_manual(values = c("darkblue", "#CC0066", "#CC6666"))

##find means, SDs for raw gains
Data_Temp %>%
  group_by(Speed, Task) %>%
  summarise_at(vars(Gain), list(name = mean, sd))



###linear mixed models comparisons###
#####################################

##checking for Task 1

Model1_Everything = lmer(Gain ~ as.factor(Direction) + as.factor(Speed) + as.factor(Distance_Real) + (as.factor(Speed) + as.factor(Direction)| Participant),
                         data = Data_Temp %>% filter(Task==1),
                         #control = lmerControl(optCtrl = list(maxfun = 1000000), optimizer = "bobyqa"))
                         control = lmerControl(optimizer = "bobyqa"))
#confint(Model1_Everything, method = "boot", level = 0.95)
summary(Model1_Everything)


Model1_Speed = lmer(Gain ~ as.factor(Direction) + as.factor(Speed) + as.factor(Distance_Real) + (as.factor(Speed)| Participant),
                    data = Data_Temp %>% filter(Task==1),
                    control = lmerControl(optimizer = "bobyqa"))
#confint(Model1_Speed, method = "boot", level = 0.95)
summary(Model1_Speed)

Model1_Direction = lmer(Gain ~ as.factor(Direction) + as.factor(Speed) + as.factor(Distance_Real) + (as.factor(Direction)| Participant),
                        data = Data_Temp %>% filter(Task==1),
                        #control = lmerControl(optCtrl = list(maxfun = 1000000), optimizer = "bobyqa"))
                        control = lmerControl(optimizer = "bobyqa"))
#confint(Model1_Direction, method =  "boot", level = 0.95)
summary(Model1_Direction)


# Model1_Interaction = lmer(Gain ~ as.factor(Direction) * as.factor(Speed) + (1 + as.factor(Speed) + as.factor(Direction)| Participant),
#                           data = Data_Temp,
#                           #control = lmerControl(optCtrl = list(maxfun = 1000000), optimizer = "bobyqa"))
#                           control = lmerControl(optimizer = "bobyqa"))
# #confint(Model1_Interaction, method = "boot", level = 0.95)
# summary(Model1_Interaction)
# 
# 
# #checking if the simpler model can be used (it can't)                                      
anova(Model1_Speed, Model1_Everything)
anova(Model1_Direction, Model1_Everything)
# 
# ##need to use Model1_Everything
# 
# 
# #checking for interaction
# anova(Model1_Interaction, Model1_Everything)
# 
# ##no differences with interaction


##checking for Task 2

Model1_Everything_Task2 = lmer(Gain ~ as.factor(Direction) + as.factor(Speed) + as.factor(Distance_Real) + (as.factor(Speed) + as.factor(Direction)| Participant),
                         data = Data_Temp %>% filter(Task==2),
                         #control = lmerControl(optCtrl = list(maxfun = 1000000), optimizer = "bobyqa"))
                         control = lmerControl(optimizer = "bobyqa"))
#confint(Model1_Everything, method = "boot", level = 0.95)
summary(Model1_Everything_Task2)


Model1_Speed_Task2 = lmer(Gain ~ as.factor(Direction) + as.factor(Speed) + as.factor(Distance_Real) + (as.factor(Speed)| Participant),
                    data = Data_Temp %>% filter(Task==2),
                    control = lmerControl(optimizer = "bobyqa"))
#confint(Model1_Speed, method = "boot", level = 0.95)
summary(Model1_Speed_Task2)

Model1_Direction_Task2 = lmer(Gain ~ as.factor(Direction) + as.factor(Speed) + as.factor(Distance_Real) + (as.factor(Direction)| Participant),
                        data = Data_Temp %>% filter(Task==2),
                        #control = lmerControl(optCtrl = list(maxfun = 1000000), optimizer = "bobyqa"))
                        control = lmerControl(optimizer = "bobyqa"))
#confint(Model1_Direction, method =  "boot", level = 0.95)
summary(Model1_Direction_Task2)


# Model1_Interaction = lmer(Gain ~ as.factor(Direction) * as.factor(Speed) + (1 + as.factor(Speed) + as.factor(Direction)| Participant),
#                           data = Data_Temp,
#                           #control = lmerControl(optCtrl = list(maxfun = 1000000), optimizer = "bobyqa"))
#                           control = lmerControl(optimizer = "bobyqa"))
# #confint(Model1_Interaction, method = "boot", level = 0.95)
# summary(Model1_Interaction)
# 
# 
# #checking if the simpler model can be used (it can't)                                      
anova(Model1_Speed_Task2, Model1_Everything_Task2)
anova(Model1_Direction_Task2, Model1_Everything_Task2)






##############RESULTS################

###MTT direction
require(lmerTest)
Model2_Fixed = lmer(Gain ~ as.factor(Direction2) + as.factor(Speed) + as.factor(Distance_Real) + (as.factor(Speed) + as.factor(Direction2)| Participant),
                    data = Data_Temp %>% filter(Task==1) %>%
                      mutate(Direction2 = 
                               case_when(Direction == 4 ~ "1Down",
                                         Direction == 3 ~ "3Up",
                                         Direction == 2 ~ "4Backward",
                                         Direction == 1 ~ "2Forward")),
                    control = lmerControl(optimizer = "bobyqa"))
confint(Model2_Fixed, method = "boot", level = 0.95)
summary(Model2_Fixed)


Model2_Fixed = lmer(Gain ~ as.factor(Direction2) + as.factor(Speed) + as.factor(Distance_Real) + (as.factor(Speed) + as.factor(Direction2)| Participant),
                    data = Data_Temp %>% filter(Task==1) %>%
                      mutate(Direction2 = 
                               case_when(Direction == 4 ~ "2Down",
                                         Direction == 3 ~ "1Up",
                                         Direction == 2 ~ "4Backward",
                                         Direction == 1 ~ "3Forward")),
                    control = lmerControl(optimizer = "bobyqa"))
confint(Model2_Fixed, method = "boot", level = 0.95)
summary(Model2_Fixed)

Model2_Fixed = lmer(Gain ~ as.factor(Direction2) + as.factor(Speed) + as.factor(Distance_Real) + (as.factor(Speed) + as.factor(Direction2)| Participant),
                    data = Data_Temp %>% filter(Task==1) %>%
                      mutate(Direction2 = 
                               case_when(Direction == 4 ~ "3Down",
                                         Direction == 3 ~ "4Up",
                                         Direction == 2 ~ "1Backward",
                                         Direction == 1 ~ "2Forward")), 
                    control = lmerControl(optimizer = "bobyqa"))
confint(Model2_Fixed, method = "boot", level = 0.95)
summary(Model2_Fixed)


###AT direction
require(lmerTest)
Model2_Fixed = lmer(Gain ~ as.factor(Direction2) + as.factor(Speed) + as.factor(Distance_Real) + (as.factor(Speed) + as.factor(Direction2)| Participant),
                    data = Data_Temp %>% filter(Task==2) %>%
                      mutate(Direction2 = 
                               case_when(Direction == 4 ~ "1Down",
                                         Direction == 3 ~ "3Up",
                                         Direction == 2 ~ "4Backward",
                                         Direction == 1 ~ "2Forward")),
                    control = lmerControl(optimizer = "bobyqa"))
confint(Model2_Fixed, method = "boot", level = 0.95)
summary(Model2_Fixed)


Model2_Fixed = lmer(Gain ~ as.factor(Direction2) + as.factor(Speed) + as.factor(Distance_Real) + (as.factor(Speed) + as.factor(Direction2)| Participant),
                    data = Data_Temp %>% filter(Task==2) %>%
                      mutate(Direction2 = 
                               case_when(Direction == 4 ~ "2Down",
                                         Direction == 3 ~ "1Up",
                                         Direction == 2 ~ "4Backward",
                                         Direction == 1 ~ "3Forward")),
                    control = lmerControl(optimizer = "bobyqa"))
confint(Model2_Fixed, method = "boot", level = 0.95)
summary(Model2_Fixed)

Model2_Fixed = lmer(Gain ~ as.factor(Direction2) + as.factor(Speed) + as.factor(Distance_Real) + (as.factor(Speed) + as.factor(Direction2)| Participant),
                    data = Data_Temp %>% filter(Task==2) %>%
                      mutate(Direction2 = 
                               case_when(Direction == 4 ~ "3Down",
                                         Direction == 3 ~ "4Up",
                                         Direction == 2 ~ "1Backward",
                                         Direction == 1 ~ "2Forward")),
                    control = lmerControl(optimizer = "bobyqa"))
confint(Model2_Fixed, method = "boot", level = 0.95)
summary(Model2_Fixed)


###MTT speed
Model2_Fixed = lmer(Gain ~ as.factor(Speed2) + as.factor(Direction) + as.factor(Distance_Real) + (as.factor(Direction) + as.factor(Speed2)| Participant),
                    data = Data_Temp %>% filter(Task==1) %>%
                      mutate(Speed2 = 
                               case_when(Speed == 5 ~ "1Five",
                                         Speed == 3 ~ "3three",
                                         Speed == 1 ~ "5one")),
                    control = lmerControl(optimizer = "bobyqa"))
confint(Model2_Fixed, method = "boot", level = 0.95)
summary(Model2_Fixed)

Model2_Fixed = lmer(Gain ~ as.factor(Speed2) + as.factor(Direction) + as.factor(Distance_Real) + (as.factor(Direction) + as.factor(Speed2)| Participant),
                    data = Data_Temp %>% filter(Task==1) %>%
                      mutate(Speed2 = 
                               case_when(Speed == 5 ~ "2Five",
                                         Speed == 3 ~ "1three",
                                         Speed == 1 ~ "3one")),
                    control = lmerControl(optimizer = "bobyqa"))
confint(Model2_Fixed, method = "boot", level = 0.95)
summary(Model2_Fixed)


###AT speed
Model2_Fixed = lmer(Gain ~ as.factor(Speed2) + as.factor(Direction) + as.factor(Distance_Real) + (as.factor(Direction) + as.factor(Speed2)| Participant),
                    data = Data_Temp %>% filter(Task==2) %>%
                      mutate(Speed2 = 
                               case_when(Speed == 5 ~ "1Five",
                                         Speed == 3 ~ "3three",
                                         Speed == 1 ~ "5one")),
                    control = lmerControl(optimizer = "bobyqa"))
confint(Model2_Fixed, method = "boot", level = 0.95)
summary(Model2_Fixed)

Model2_Fixed = lmer(Gain ~ as.factor(Speed2) + as.factor(Direction) + as.factor(Distance_Real) + (as.factor(Direction) + as.factor(Speed2)| Participant),
                    data = Data_Temp %>% filter(Task==2) %>%
                      mutate(Speed2 = 
                               case_when(Speed == 5 ~ "2Five",
                                         Speed == 3 ~ "1three",
                                         Speed == 1 ~ "3one")),
                    control = lmerControl(optimizer = "bobyqa"))
confint(Model2_Fixed, method = "boot", level = 0.95)
summary(Model2_Fixed)




#############MODELING##############
###################################

###Figure 3

# Data3 = Data2 %>%
#   group_by(Speed,Direction) %>%
#   mutate(LappeGain = optim(initalParam_Lappe, errfn_Lappe, VectorRealDistances=Distance_Real, 
#                            VectorResponseDistancesMTT = Data_MTT,
#                            VectorResponseDistancesAT = Data_AT)$par[1],
#          
#          LappeDecay = optim(initalParam_Lappe, errfn_Lappe, VectorRealDistances=Distance_Real, 
#                             VectorResponseDistancesMTT = Data_MTT,
#                             VectorResponseDistancesAT = Data_AT)$par[2],
#          
#          Prediction_Lappe_MTT = lappe(Distance_Real,LappeGain,LappeDecay, TypeOfTask = "MTT"),
#          Prediction_Lappe_AT = lappe(Distance_Real,LappeGain,LappeDecay, TypeOfTask = "AT"))
# save(Data3, file = paste0(dirname(rstudioapi::getSourceEditorContext()$path),"/Data3.RData"))

load(file = paste0(dirname(rstudioapi::getSourceEditorContext()$path),"/Data3.RData"))


Data3 = Data3 %>% 
  group_by(Distance_Real, Direction, Speed) %>%
  mutate(Mean_Performance_AT = mean(Data_AT, na.rm = TRUE),
         SD_Performance_AT = sd(Data_AT, na.rm = TRUE),
         Mean_Performance_MTT = mean(Data_MTT, na.rm = TRUE),
         SD_Performance_MTT = sd(Data_MTT, na.rm = TRUE),
         Speed2 = case_when(
           Speed == 1 ~ "1 m/s",
           Speed == 3 ~ "3 m/s",
           Speed == 5 ~ "5 m/s"),
         Direction2 = case_when(
           Direction == 1 ~ "Forwards",
           Direction == 2 ~ "Backwards",
           Direction == 3 ~ "Up",
           Direction == 4 ~ "Down"))


ggplot(Data3,
       aes(Mean_Performance_AT,Distance_Real)) +
  geom_point(color = "red", size = 2) +
  geom_ribbon(aes(xmin = Mean_Performance_AT - SD_Performance_AT, xmax = Mean_Performance_AT + SD_Performance_AT), 
              fill = "red",
              alpha = 0.2) +
  geom_line(color = "red", linetype = 2) +
  facet_grid(Speed2~Direction2) +
  geom_point(aes(Distance_Real,Mean_Performance_MTT), color = "blue", size = 2, shape = 15, alpha = 0.5) +
  geom_line(aes(Distance_Real,Mean_Performance_MTT), color = "blue", linetype = 2, alpha = 0.5) +
  geom_ribbon(aes(x = Distance_Real,
                  ymin = Mean_Performance_MTT - SD_Performance_MTT, 
                  ymax = Mean_Performance_MTT + SD_Performance_MTT), 
              fill = "blue",
              alpha = 0.2) +
  geom_line(aes(Prediction_Lappe_AT,Distance_Real),color = "darkred", size = 2) +
  geom_line(aes(Distance_Real,Prediction_Lappe_MTT), color = "darkblue", size = 2) +
  ylab("Actual Traveled Distance (m)") +
  xlab("Perceived Traveled Distance (m)") +
  coord_cartesian(xlim = c(0,50))
ggsave("Figure2 Lappe fits Confidence Bands (SDs).jpg", w = 12, h = 8)



#Fitting slopes
initalParam_Simple = c(0,6)
StraightLine = function(VectorRealDistances, Slope){
  Slope*VectorRealDistances
}

errfn_StraightLine = function(Slope,VectorRealDistances,VectorResponseDistances){
  mean((VectorResponseDistances[!is.na(VectorResponseDistances)]-StraightLine(VectorRealDistances, Slope))^2)
}


colnames(Data2) = c("Speed","Direction","Distance_Real","Participant", "Data_MTT","Data_AT")

############################################################
####Lappe model
############################################################

errfn_Lappe = function(p,VectorRealDistances,VectorResponseDistancesMTT, VectorResponseDistancesAT){
  MSE1 = mean((VectorResponseDistancesMTT[!is.na(VectorResponseDistancesMTT)]-lappe(VectorRealDistances, TypeOfTask = "MTT", p[1], p[2]))^2) #p(1)= gain, p(2)= decay
  MSE2 = mean((VectorResponseDistancesAT[!is.na(VectorResponseDistancesAT)]-lappe(VectorRealDistances, TypeOfTask = "AT", p[1], p[2]))^2) #p(1)= gain, p(2)= decay
  MSEtotal = MSE1 + MSE2
  MSEtotal
}

errfn_Lappe_NoAlpha = function(p,VectorRealDistances,VectorResponseDistancesMTT, VectorResponseDistancesAT){
  MSE1 = mean((VectorResponseDistancesMTT[!is.na(VectorResponseDistancesMTT)]-lappe(VectorRealDistances, TypeOfTask = "MTT", gain = p, alpha = 0.00001))^2) #p(1)= gain, p(2)= decay
  MSE2 = mean((VectorResponseDistancesAT[!is.na(VectorResponseDistancesAT)]-lappe(VectorRealDistances, TypeOfTask = "AT", gain = p, alpha = 0.00001))^2) #p(1)= gain, p(2)= decay
  MSEtotal = MSE1 + MSE2
  MSEtotal
}

initalParam_Lappe <- c( 1, 0.01)

lappe = function(d0, gain, alpha, TypeOfTask){
  if(TypeOfTask == "MTT"){
       # PredictedDistance = -(1/alpha)*log(gain/(d0*alpha+gain))
       PredictedDistance = (log(d0+gain/alpha)-log(gain/alpha))/alpha}
  else if (TypeOfTask == "AT"){
       PredictedDistance = (gain/alpha) * ( 1 - exp(-d0*alpha))}
PredictedDistance}


#univerted thing
# Data2 = Data2 %>%
#   mutate(MeanDataAcrossTasks = (Data_MTT + Data_AT)/2)


AIC_Self <- function(MSE, k, N) {
  return( (N * log(MSE)) + (2 * k) )
}
relativeLikelihood <- function(crit) {
  return( exp( ( min( crit  ) - crit  ) / 2 ) )
}

Data4 = Data2[complete.cases(Data2),]

set.seed(3)
Data5 = rbind(Data4 %>% filter(Direction == 4),
              (Data4 %>% filter(Direction == 1))[sample(1:length((Data4 %>% filter(Direction == 1))$Participant), length((Data4 %>% filter(Direction == 4))$Participant)),],
              (Data4 %>% filter(Direction == 2))[sample(1:length((Data4 %>% filter(Direction == 2))$Participant), length((Data4 %>% filter(Direction == 4))$Participant)),],
              (Data4 %>% filter(Direction == 3))[sample(1:length((Data4 %>% filter(Direction == 3))$Participant), length((Data4 %>% filter(Direction == 4))$Participant)),])
              

Data5 = Data5 %>%
  group_by(Speed,Direction,Participant) %>%
  mutate(LappeGain = optim(initalParam_Lappe, errfn_Lappe, VectorRealDistances=Distance_Real,
                                                           VectorResponseDistancesMTT = Data_MTT,
                                                           VectorResponseDistancesAT = Data_AT)$par[1],

         LappeDecay = optim(initalParam_Lappe, errfn_Lappe, VectorRealDistances=Distance_Real,
                           VectorResponseDistancesMTT = Data_MTT,
                           VectorResponseDistancesAT = Data_AT)$par[2],

         Prediction_Lappe_MTT = lappe(Distance_Real,LappeGain,LappeDecay, TypeOfTask = "MTT"),
         Prediction_Lappe_AT = lappe(Distance_Real,LappeGain,LappeDecay, TypeOfTask = "AT"),

         MSE_Lappe = optim(initalParam_Lappe, errfn_Lappe, VectorRealDistances=Distance_Real,
                           VectorResponseDistancesMTT = Data_MTT,
                           VectorResponseDistancesAT = Data_AT)$value[1],

         AIC_Lappe = AIC_Self(MSE_Lappe, 2, length(LappeGain))
         )

Data5 = Data5 %>%
  group_by(Speed,Direction,Participant) %>%
   mutate(#Slope = optimize(errfn_StraightLine,
                            # interval=initalParam_Simple, 
                            # VectorRealDistances=Perceived_Travel_Distance, 
                            # VectorResponseDistances=Actual_Travel_Distance)$minimum,

          #Prediction_Slope = Slope*Actual_Travel_Distance,
     
         LappeGain = optim(1, errfn_Lappe_NoAlpha, 
                           VectorRealDistances=Distance_Real,
                       VectorResponseDistancesMTT = Data_MTT,
                       VectorResponseDistancesAT = Data_AT, 
                       method = "Brent",
                       lower = 0,
                       upper = 15)$par,

          PredictionLappe_NoAlpha_MTT = lappe(Distance_Real, LappeGain, alpha = 0.000001, "MTT"),
          PredictionLappe_NoAlpha_AT = lappe(Distance_Real, LappeGain, alpha = 0.000001, "AT"),

          MSE_LappeNoAlpha = optim(1, errfn_Lappe_NoAlpha, 
                            VectorRealDistances=Distance_Real,
                            VectorResponseDistancesMTT = Data_MTT,
                            VectorResponseDistancesAT = Data_AT, 
                            method = "Brent",
                            lower = 0,
                            upper = 15)$value,
         
          AIC_Slope = AIC_Self(MSE_LappeNoAlpha, 1, length(Data3$LappeGain))
          )


####Testing Differences between Lappe and Slope for the Different Directions

##Direction 1
round(mean((Data5 %>% filter(Direction == 1))$MSE_Lappe),2)
round(mean((Data5 %>% filter(Direction == 1))$MSE_LappeNoAlpha),2)
NumberOfConditions = length((Data5 %>%
                               filter(Direction == 1) %>%
                               group_by(Direction,Speed,Participant) %>%
                               slice(1))$LappeGain)
AIC_Lappe_All_Forwards = AIC_Self(sum((Data5 %>% filter(Direction == 1))$MSE_Lappe),NumberOfConditions*2,length((Data5 %>% filter(Direction == 1))$MSE_Lappe))
AIC_Slope_All_Forwards = AIC_Self(sum((Data5 %>% filter(Direction == 1))$MSE_LappeNoAlpha),NumberOfConditions,length((Data5 %>% filter(Direction == 1))$MSE_Slope))
RelLikelihood_Lappe_Forwards = exp( (AIC_Lappe_All_Forwards - AIC_Slope_All_Forwards) / 2 )
RelLikelihood_Lappe_Forwards

##Direction 2
round(mean((Data5 %>% filter(Direction == 2))$MSE_Lappe),2)
round(mean((Data5 %>% filter(Direction == 2))$MSE_LappeNoAlpha),2)
NumberOfConditions = length((Data5 %>%
                               filter(Direction == 2) %>%
                               group_by(Direction,Speed,Participant) %>%
                               slice(1))$LappeGain)
AIC_Lappe_All_Backwards = AIC_Self(sum((Data5 %>% filter(Direction == 2))$MSE_Lappe),NumberOfConditions*2,length((Data5 %>% filter(Direction == 2))$MSE_Lappe))
AIC_Slope_All_Backwards = AIC_Self(sum((Data5 %>% filter(Direction == 2))$MSE_LappeNoAlpha),NumberOfConditions,length((Data5 %>% filter(Direction == 2))$MSE_Lappe))
RelLikelihood_Lappe_Backwards = exp( (AIC_Lappe_All_Backwards - AIC_Slope_All_Backwards) / 2 )
RelLikelihood_Lappe_Backwards

##Direction 3
round(mean((Data5 %>% filter(Direction == 3))$MSE_Lappe),2)
round(mean((Data5 %>% filter(Direction == 3))$MSE_LappeNoAlpha),2)
NumberOfConditions = length((Data5 %>%
                               filter(Direction == 3) %>%
                               group_by(Direction,Speed,Participant) %>%
                               slice(1))$LappeGain)
AIC_Lappe_All_Up = AIC_Self(sum((Data5 %>% filter(Direction == 3))$MSE_Lappe),NumberOfConditions*2,length((Data5 %>% filter(Direction == 3))$MSE_Lappe))
AIC_Slope_All_Up = AIC_Self(sum((Data5 %>% filter(Direction == 3))$MSE_LappeNoAlpha),NumberOfConditions,length((Data5 %>% filter(Direction == 3))$MSE_Lappe))
RelLikelihood_Lappe_Up = exp( (AIC_Lappe_All_Up - AIC_Slope_All_Up) / 2 )
RelLikelihood_Lappe_Up


##Direction 4
round(mean((Data5 %>% filter(Direction == 4))$MSE_Lappe),2)
round(mean((Data5 %>% filter(Direction == 4))$MSE_LappeNoAlpha),2)
NumberOfConditions = length((Data5 %>%
                               filter(Direction == 4) %>%
                               group_by(Direction,Speed,Participant) %>%
                               slice(1))$LappeGain)
AIC_Lappe_All_Down = AIC_Self(sum((Data5 %>% filter(Direction == 4))$MSE_Lappe),NumberOfConditions*2,length((Data5 %>% filter(Direction == 4))$MSE_Lappe))
AIC_Slope_All_Down = AIC_Self(sum((Data5 %>% filter(Direction == 4))$MSE_LappeNoAlpha),NumberOfConditions,length((Data5 %>% filter(Direction == 4))$MSE_Lappe))
RelLikelihood_Lappe_Down = exp( (AIC_Lappe_All_Down - AIC_Slope_All_Down) / 2 )
RelLikelihood_Lappe_Down

#Is Lappe a better fit for forwards or other directions?
exp((AIC_Lappe_All_Forwards - AIC_Lappe_All_Backwards) / 2)
exp((AIC_Lappe_All_Forwards - AIC_Lappe_All_Up) / 2)
exp((AIC_Lappe_All_Forwards - AIC_Lappe_All_Down) / 2)
exp((AIC_Lappe_All_Up - AIC_Lappe_All_Down) / 2)


###Lappe is a better fit for forwards, compared to the other 3 directions###





##plotting
ggplot(Data_Temp, aes(Actual_Travel_Distance,Perceived_Travel_Distance, color = Task, legend.position="none")) +
  geom_point()
ggplot(Data_Temp,aes(Distance_Real, Perceived_Travel_Distance - Prediction_Slope)) +
  geom_point()
ggplot(Data2,aes(Distance_Real, Data_MTT - Prediction_Lappe_MTT)) +
  geom_point() +
  ylim (-40,20)
ggplot(Data_Temp,aes(Distance_Real, Perceived_Travel_Distance - Prediction_Slope)) +
  geom_point()
  #ylim (-25,60)
ggplot(Data2,aes(Distance_Real, Data_AT - Prediction_Lappe_AT)) +
  geom_point() +
  ylim (-25,60)

##Figure4
ggplot(Data5,aes(as.factor(Direction), MSE_Lappe, color = as.factor(Direction))) +
  geom_boxplot() +
  labs (x = "Direction", y = "Mean Squared Error") +
  scale_x_discrete(labels = c("Forward", "Backward", "Up", "Down" )) +
  theme(legend.position="none") +
  scale_color_manual(values = c("red", "blue", "forestgreen", "orange"))

Data6 = Data5%>% 
  group_by(Distance_Real, Direction) %>%
  summarise_at(vars(MSE_Lappe), list(MSE_Lappe_mean = mean, SD = sd))

ggplot(Data6,aes(Distance_Real, MSE_Lappe_mean, color=as.factor(Direction))) +
  geom_point(size=2) +
  geom_line(size=1) +
  labs (x = "Distance (m)", y = "Mean Squared Error") +
  scale_color_manual(name = "Direction", labels = c("Forward", "Backward", "Up", "Down" ), values = c("red", "blue", "forestgreen", "orange"))
