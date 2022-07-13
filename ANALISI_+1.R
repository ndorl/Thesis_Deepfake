library(readr)
library(tidyverse)
library(devtools)
library(tidyverse)
library(hrbrthemes)
library(viridis)
library(xlsx)
library(psych)
library(car)
library(AICcmodavg)
library(rgeolocate)
library(scales)
library(dplyr)
library(ggplot2)
require(ggbiplot)
library(rworldmap)

#creating dataframe from .csv

Full_Dataset <- read_delim("~/Tesi magistrale/Research/Dataset/Updated_Data_Gathering_Final_February+7,+2022_11.56.csv", 
                           ";", escape_double = FALSE, trim_ws = TRUE)


# plot data on world map

worldmap <- getMap(resolution = "coarse")
plot(worldmap, xlim = c(-150, 150), ylim = c(-80, 100), 
     asp = 1, border = "darkgray", bg = "aliceblue", col = "black", fill = T)

# add points

points(Full_Dataset$LONGITUDE, Full_Dataset$LATITUDE, 
       col = "red", cex = 1, pch = 20)
title(main = "Respondents Map Overview")

#Substituting likert scale to numerical values for the first expositon

Full_Dataset$AD_BORING_1 <- as.integer(plyr::mapvalues(Full_Dataset$AD_BORING_1,
                                                       from = c("Strongly disagree",
                                                                "Disagree",
                                                                "Somewhat disagree",
                                                                "Neither agree nor disagree",
                                                                "Somewhat agree",
                                                                "Agree",
                                                                "Strongly agree"),
                                                       to = c(1,2,3,4,5,6,7)))
Full_Dataset$AD_IRRITATING_1 <- as.integer(plyr::mapvalues(Full_Dataset$AD_IRRITATING_1,
                                                           from = c("Strongly disagree",
                                                                    "Disagree",
                                                                    "Somewhat disagree",
                                                                    "Neither agree nor disagree",
                                                                    "Somewhat agree",
                                                                    "Agree",
                                                                    "Strongly agree"),
                                                           to = c(1,2,3,4,5,6,7)))
Full_Dataset$AD_DISTURBING_1 <- as.integer(plyr::mapvalues(Full_Dataset$AD_DISTURBING_1,
                                                           from = c("Strongly disagree",
                                                                    "Disagree",
                                                                    "Somewhat disagree",
                                                                    "Neither agree nor disagree",
                                                                    "Somewhat agree",
                                                                    "Agree",
                                                                    "Strongly agree"),
                                                           to = c(1,2,3,4,5,6,7)))
Full_Dataset$AD_CREDIBLE_1 <- as.integer(plyr::mapvalues(Full_Dataset$AD_CREDIBLE_1,
                                                         from = c("Strongly disagree",
                                                                  "Disagree",
                                                                  "Somewhat disagree",
                                                                  "Neither agree nor disagree",
                                                                  "Somewhat agree",
                                                                  "Agree",
                                                                  "Strongly agree"),
                                                         to = c(1,2,3,4,5,6,7)))
Full_Dataset$AD_GOOD_1 <- as.integer(plyr::mapvalues(Full_Dataset$AD_GOOD_1,
                                                     from = c("Strongly disagree",
                                                              "Disagree",
                                                              "Somewhat disagree",
                                                              "Neither agree nor disagree",
                                                              "Somewhat agree",
                                                              "Agree",
                                                              "Strongly agree"),
                                                     to = c(1,2,3,4,5,6,7)))
Full_Dataset$AD_HONEST_1 <- as.integer(plyr::mapvalues(Full_Dataset$AD_HONEST_1,
                                                       from = c("Strongly disagree",
                                                                "Disagree",
                                                                "Somewhat disagree",
                                                                "Neither agree nor disagree",
                                                                "Somewhat agree",
                                                                "Agree",
                                                                "Strongly agree"),
                                                       to = c(1,2,3,4,5,6,7)))
Full_Dataset$AD_TRUTHFUL_1 <- as.integer(plyr::mapvalues(Full_Dataset$AD_TRUTHFUL_1,
                                                         from = c("Strongly disagree",
                                                                  "Disagree",
                                                                  "Somewhat disagree",
                                                                  "Neither agree nor disagree",
                                                                  "Somewhat agree",
                                                                  "Agree",
                                                                  "Strongly agree"),
                                                         to = c(1,2,3,4,5,6,7)))
Full_Dataset$AD_LIKEABLE_1 <- as.integer(plyr::mapvalues(Full_Dataset$AD_LIKEABLE_1,
                                                         from = c("Strongly disagree",
                                                                  "Disagree",
                                                                  "Somewhat disagree",
                                                                  "Neither agree nor disagree",
                                                                  "Somewhat agree",
                                                                  "Agree",
                                                                  "Strongly agree"),
                                                         to = c(1,2,3,4,5,6,7)))
Full_Dataset$AD_ENJOYABLE_1 <- as.integer(plyr::mapvalues(Full_Dataset$AD_ENJOYABLE_1, from = c("Strongly disagree", "Disagree", "Somewhat disagree", "Neither agree nor disagree", "Somewhat agree", "Agree", "Strongly agree"), to = c(1,2,3,4,5,6,7)))
Full_Dataset$LIKE_1 <- as.integer(plyr::mapvalues(Full_Dataset$LIKE_1, from = c("Dislike a great deal", "Dislike a moderate amount", "Dislike a little", "Neither like nor dislike", "Like a little", "Like a moderate amount", "Like a great deal"), to = c(1,2,3,4,5,6,7)))

#Doing the same for the second eposition

Full_Dataset$AD_BORING_2 <- as.integer(plyr::mapvalues(Full_Dataset$AD_BORING_2, from = c("Strongly disagree", "Disagree", "Somewhat disagree", "Neither agree nor disagree", "Somewhat agree", "Agree", "Strongly agree"), to = c(1,2,3,4,5,6,7)))
Full_Dataset$AD_IRRITATING_2 <- as.integer(plyr::mapvalues(Full_Dataset$AD_IRRITATING_2, from = c("Strongly disagree", "Disagree", "Somewhat disagree", "Neither agree nor disagree", "Somewhat agree", "Agree", "Strongly agree"), to = c(1,2,3,4,5,6,7)))
Full_Dataset$AD_DISTURBING_2 <- as.integer(plyr::mapvalues(Full_Dataset$AD_DISTURBING_2, from = c("Strongly disagree", "Disagree", "Somewhat disagree", "Neither agree nor disagree", "Somewhat agree", "Agree", "Strongly agree"), to = c(1,2,3,4,5,6,7)))
Full_Dataset$AD_CREDIBLE_2 <- as.integer(plyr::mapvalues(Full_Dataset$AD_CREDIBLE_2, from = c("Strongly disagree", "Disagree", "Somewhat disagree", "Neither agree nor disagree", "Somewhat agree", "Agree", "Strongly agree"), to = c(1,2,3,4,5,6,7)))
Full_Dataset$AD_GOOD_2 <- as.integer(plyr::mapvalues(Full_Dataset$AD_GOOD_2, from = c("Strongly disagree", "Disagree", "Somewhat disagree", "Neither agree nor disagree", "Somewhat agree", "Agree", "Strongly agree"), to = c(1,2,3,4,5,6,7)))
Full_Dataset$AD_HONEST_2 <- as.integer(plyr::mapvalues(Full_Dataset$AD_HONEST_2, from = c("Strongly disagree", "Disagree", "Somewhat disagree", "Neither agree nor disagree", "Somewhat agree", "Agree", "Strongly agree"), to = c(1,2,3,4,5,6,7)))
Full_Dataset$AD_TRUTHFUL_2 <- as.integer(plyr::mapvalues(Full_Dataset$AD_TRUTHFUL_2, from = c("Strongly disagree", "Disagree", "Somewhat disagree", "Neither agree nor disagree", "Somewhat agree", "Agree", "Strongly agree"), to = c(1,2,3,4,5,6,7)))
Full_Dataset$AD_LIKEABLE_2 <- as.integer(plyr::mapvalues(Full_Dataset$AD_LIKEABLE_2, from = c("Strongly disagree", "Disagree", "Somewhat disagree", "Neither agree nor disagree", "Somewhat agree", "Agree", "Strongly agree"), to = c(1,2,3,4,5,6,7)))
Full_Dataset$AD_ENJOYABLE_2 <- as.integer(plyr::mapvalues(Full_Dataset$AD_ENJOYABLE_2, from = c("Strongly disagree", "Disagree", "Somewhat disagree", "Neither agree nor disagree", "Somewhat agree", "Agree", "Strongly agree"), to = c(1,2,3,4,5,6,7)))
Full_Dataset$LIKE_2 <- as.integer(plyr::mapvalues(Full_Dataset$LIKE_2, from = c("Dislike a great deal", "Dislike a moderate amount", "Dislike a little", "Neither like nor dislike", "Like a little", "Like a moderate amount", "Like a great deal"), to = c(1,2,3,4,5,6,7)))

#Deleting extra columns

Full_Dataset[67:81] <- list(NULL)

#Creating Education Levels abbreviations

Full_Dataset <- Full_Dataset %>%
  mutate(EDUCATION = if_else(EDUCATION == "2nd Level Master", "2ndL Master", EDUCATION)) %>%
  mutate(EDUCATION = if_else(EDUCATION == "High school graduate", "HighSchool", EDUCATION)) %>%
  mutate(EDUCATION = if_else(EDUCATION == "Bachelor Degree", "Bachelor", EDUCATION)) %>%
  mutate(EDUCATION = if_else(EDUCATION == "Less than high school", "No HighSchool", EDUCATION)) %>%
  mutate(EDUCATION = if_else(EDUCATION == "Postgraduate/Master/5 year Degree", "Postgraduate", EDUCATION))

#creating subset for every different scenario

###Creating PRE_TEST subset from FULL_DATASET

PRE_TEST <- Full_Dataset
PRE_TEST[1:3] <- list(NULL)
PRE_TEST[2] <- NULL
PRE_TEST <- pivot_longer(PRE_TEST, cols = 2:5, names_to = c("nothign", "noge", "N_DF", "BRAND"), names_sep = "_", values_drop_na = TRUE)
PRE_TEST[3:52] <- list(NULL)
PRE_TEST[6:10] <- NULL
PRE_TEST[8] <- NULL

#Creating Main_Test subset from FULL_DATASET

MAIN_TEST <- Full_Dataset
MAIN_TEST[1:3] <- NULL
MAIN_TEST[2:7] <- NULL
MAIN_TEST <- pivot_longer(MAIN_TEST,
                          cols = B_DF_LOR:N_DF_GILL,
                          names_to = c("DISCLOSURE_1", "TYPE_1", "BRAND_1"),
                          names_sep = "_",
                          values_drop_na = TRUE)
MAIN_TEST[13] <- NULL
MAIN_TEST[14] <- NULL
MAIN_TEST[45] <- NULL
MAIN_TEST <- pivot_longer(MAIN_TEST,
                          cols = B_DF_JAG:A_SM_TCC,
                          names_to = c("DISCLOSURE_2", "TYPE_2", "BRAND_2"),
                          names_sep = "_",
                          values_drop_na = TRUE)
MAIN_TEST[36] <- NULL
MAIN_TEST[27:29] <- NULL

#fixing the mispelled "GILL" in BRAND + convert everything in factor

MAIN_TEST <- MAIN_TEST %>%
  mutate(BRAND_1 = if_else(BRAND_1 == "GILL", "GIL", BRAND_1)) %>%
  mutate_if(is.character, factor)

PRE_TEST <- PRE_TEST %>%
  mutate(BRAND = if_else(BRAND == "GILL", "GIL", BRAND)) %>%
  mutate_if(is.character, factor)

#creating one column for KNOWLEDGE

MAIN_TEST <- pivot_longer(MAIN_TEST, cols = DF_KNOWLEDGE:SM_KNOWLEDGE, names_to = "Knowledge", values_drop_na = TRUE)

#PRE_TEST ANALYSIS

#demographics main test

PT_Demographics <- PRE_TEST[3:5]
PT_Demographics$Gender <- PRE_TEST$Gender
summary(PT_Demographics)

#gender

Gender_pre <- PRE_TEST %>% 
  group_by(Gender) %>% # Variable to be transformed
  dplyr::count() %>% 
  ungroup() %>% 
  mutate(perc = `n` / sum(`n`)) %>% 
  arrange(perc) %>%
  mutate(labels = scales::percent(perc))

ggplot(Gender_pre, aes(x = "", y = perc, fill = Gender)) +
  geom_col() +
  geom_text(aes(x=1.6, label = labels),
            position = position_stack(vjust = 0.5))+
  coord_polar(theta = "y")+
  guides(fill = guide_legend(title = "Gender"))+
  scale_fill_viridis_d(option = "A", begin = 0, end = 0.8, drop = F)+
  theme_bw()+
  theme_void()

#Education

Education_pre <- PRE_TEST %>% 
  group_by(EDUCATION) %>% # Variable to be transformed
  dplyr::count() %>% 
  ungroup() %>% 
  mutate(perc = `n` / sum(`n`)) %>% 
  arrange(perc) %>%
  mutate(labels = scales::percent(perc))

ggplot(Education_pre, aes(x = "", y = perc, fill = EDUCATION)) +
  geom_col() +
  geom_text(aes(x=1.55, label = labels),
            position = position_stack(vjust = 0.5))+
  coord_polar(theta = "y")+
  guides(fill = guide_legend(title = "Education"))+
  scale_fill_viridis_d(option = "A", begin = 0, end = 0.85, drop = F)+
  theme_bw()+
  theme_void()

#Age

Age_pre <- PRE_TEST %>% 
  group_by(AGE) %>% # Variable to be transformed
  dplyr::count() %>% 
  ungroup() %>% 
  mutate(perc = `n` / sum(`n`)) %>% 
  arrange(perc) %>%
  mutate(labels = scales::percent(perc))

ggplot(Age_pre, aes(x = "", y = perc, fill = AGE)) +
  geom_col() +
  geom_text(aes(x=1.55, label = labels),
            position = position_stack(vjust = 0.5))+
  coord_polar(theta = "y")+
  guides(fill = guide_legend(title = "Age"))+
  scale_fill_viridis_d(option = "A", begin = 0, end = 0.85, drop = F)+
  theme_bw()+
  theme_void()

#Employment

Employment_pre <- PRE_TEST %>% 
  group_by(EMPLOYMENT) %>% # Variable to be transformed
  dplyr::count() %>% 
  ungroup() %>% 
  mutate(perc = `n` / sum(`n`)) %>% 
  arrange(perc) %>%
  mutate(labels = scales::percent(perc))

ggplot(Employment_pre, aes(x = "", y = perc, fill = EMPLOYMENT)) +
  geom_col() +
  geom_text(aes(x=1.55, label = labels),
            position = position_stack(vjust = 0.5))+
  coord_polar(theta = "y")+
  guides(fill = guide_legend(title = "Employment"))+
  scale_fill_viridis_d(option = "A", begin = 0, end = 0.85, drop = F)+
  theme_bw()+
  theme_void()

#mutate variables into factor

PRE_TEST <- PRE_TEST %>%
  mutate(across(everything(), factor))

#preparing labels for plotting

likert_breaks <- c("Strongly Disagree", "Somewhat Disagree", "Slightly Disagree", "Neutral", "Slightly Agree", "Somewhat Agree", "Strongly Agree")

score_breaks <- c("10","20","30","40","50","60","70","80","90","100")

labeller_spot <- c(
  GIL = "Gillette",
  LOR = "L'Oréal",
  JAG = "Jaguar",
  TCC = "TrueCaller"
)

labeller_pre_test_cond <- c(
  DF = "DeepFake",
  N = "Not DeepFake"
)

#PLOTTING RESULTS

PRE_TEST %>%
  ggplot(aes(x = N_DF, y = 100, fill = REAL_or_FAKE)) +
  geom_bar(position = "fill", stat = "identity")+
  facet_grid(~BRAND, labeller = labeller(
    BRAND = labeller_spot
  ),
  switch = "y")+
  scale_fill_viridis_d(option = "A", begin = 0, end = 0.8, drop = F)+
  ggtitle("Deepfake recognition by Type of Ad")+
  xlab("")+
  ylab("Percentage")+
  labs(fill = "REAL OR FAKE?")+
  theme_bw()+
  theme(axis.text = element_text(size = 10))+
  scale_x_discrete(labels = labeller_pre_test_cond)

SUMMARY_PRE_TEST <- data.frame(PRE_TEST$REAL_or_FAKE)
SUMMARY_PRE_TEST$N_DF <- PRE_TEST$N_DF
SUMMARY_PRE_TEST$BRAND <- PRE_TEST$BRAND
table(SUMMARY_PRE_TEST)

#________________________PRE_TEST ANALYSED________________________________


#________________BEGINNING OF MAIN_TEST_____________________________

#demographics main test

MT_Demographics <- MAIN_TEST[22:24]
MT_Demographics$Gender <- MAIN_TEST$Gender
summary(MT_Demographics)

#gender

Gender_main <- MAIN_TEST %>% 
  group_by(Gender) %>% # Variable to be transformed
  dplyr::count() %>% 
  ungroup() %>% 
  mutate(perc = `n` / sum(`n`)) %>% 
  arrange(perc) %>%
  mutate(labels = scales::percent(perc))

ggplot(Gender_main, aes(x = "", y = perc, fill = Gender)) +
  geom_col() +
  geom_text(aes(x=1.6, label = labels),
            position = position_stack(vjust = 0.5))+
  coord_polar(theta = "y")+
  guides(fill = guide_legend(title = "Gender"))+
  scale_fill_viridis_d(option = "A", begin = 0, end = 0.8, drop = F)+
  theme_bw()+
  theme_void()

#Education


Education_main <- MAIN_TEST %>% 
  group_by(EDUCATION) %>% # Variable to be transformed
  dplyr::count() %>% 
  ungroup() %>% 
  mutate(perc = `n` / sum(`n`)) %>% 
  arrange(perc) %>%
  mutate(labels = scales::percent(perc))

ggplot(Education_main, aes(x = "", y = perc, fill = EDUCATION)) +
  geom_col() +
  geom_text(aes(x=1.55, label = labels),
            position = position_stack(vjust = 0.5))+
  coord_polar(theta = "y")+
  guides(fill = guide_legend(title = "Education"))+
  scale_fill_viridis_d(option = "A", begin = 0, end = 0.85, drop = F)+
  theme_bw()+
  theme_void()

#Age

Age_main <- MAIN_TEST %>% 
  group_by(AGE) %>% # Variable to be transformed
  dplyr::count() %>% 
  ungroup() %>% 
  mutate(perc = `n` / sum(`n`)) %>% 
  arrange(perc) %>%
  mutate(labels = scales::percent(perc))

ggplot(Age_main, aes(x = "", y = perc, fill = AGE)) +
  geom_col() +
  geom_text(aes(x=1.55, label = labels),
            position = position_stack(vjust = 0.5))+
  coord_polar(theta = "y")+
  guides(fill = guide_legend(title = "Age"))+
  scale_fill_viridis_d(option = "A", begin = 0, end = 0.85, drop = F)+
  theme_bw()+
  theme_void()

#Employment

Employment_main <- MAIN_TEST %>% 
  group_by(EMPLOYMENT) %>% # Variable to be transformed
  dplyr::count() %>% 
  ungroup() %>% 
  mutate(perc = `n` / sum(`n`)) %>% 
  arrange(perc) %>%
  mutate(labels = scales::percent(perc))

ggplot(Employment_main, aes(x = "", y = perc, fill = EMPLOYMENT)) +
  geom_col() +
  geom_text(aes(x=1.55, label = labels),
            position = position_stack(vjust = 0.5))+
  coord_polar(theta = "y")+
  guides(fill = guide_legend(title = "Employment"))+
  scale_fill_viridis_d(option = "A", begin = 0, end = 0.85, drop = F)+
  theme_bw()+
  theme_void()


#creating data.frame of likert to be analyzed for pca and cronbach's alpha

reliability_1 <- MAIN_TEST[2:11]
reliability_2 <- MAIN_TEST[12:21]

#pca test for 1 and 2

pca_1 <- princomp(reliability_1, cor = T)
part_pca_1 <- pca_1$sdev^2/sum(pca_1$sdev^2)*100
print(part_pca_1)
print(cumsum(part_pca_1))

ggbiplot(pca_1)

pca_2 <- princomp(reliability_2, cor = T)
part_pca_2 <- pca_2$sdev^2/sum(pca_2$sdev^2)*100
print(part_pca_2)
print(cumsum(part_pca_2))

ggbiplot(pca_2)


#checking scales with cronbach's alpha, I use check.keys = true as the first three items are negatively correlated with the total scale.

#reverse.coding negative related items

#recode inverted

#Checking Cronbach's alpha

reliability_1$AD_BORING_1 <- as.list(reliability_1$AD_BORING_1)
reliability_1$AD_BORING_1 <- as.integer(recode(reliability_1$AD_BORING_1, '1=7; 2=6; 3=5; 4=4; 5=3; 6=2; 7=1'))

reliability_1$AD_IRRITATING_1 <- as.list(reliability_1$AD_IRRITATING_1)
reliability_1$AD_IRRITATING_1 <- as.integer(recode(reliability_1$AD_IRRITATING_1, '1=7; 2=6; 3=5; 4=4; 5=3; 6=2; 7=1'))

reliability_1$AD_DISTURBING_1 <- as.list(reliability_1$AD_DISTURBING_1)
reliability_1$AD_DISTURBING_1 <- as.integer(recode(reliability_1$AD_DISTURBING_1, '1=7; 2=6; 3=5; 4=4; 5=3; 6=2; 7=1'))


psych::alpha(reliability_1)

reliability_2$AD_BORING_2 <- as.list(reliability_2$AD_BORING_2)
reliability_2$AD_BORING_2 <- as.integer(recode(reliability_2$AD_BORING_2, '1=7; 2=6; 3=5; 4=4; 5=3; 6=2; 7=1'))


reliability_2$AD_IRRITATING_2 <- as.list(reliability_2$AD_IRRITATING_2)
reliability_2$AD_IRRITATING_2 <- as.integer(recode(reliability_2$AD_IRRITATING_2, '1=7; 2=6; 3=5; 4=4; 5=3; 6=2; 7=1'))

reliability_2$AD_DISTURBING_2 <- as.list(reliability_2$AD_DISTURBING_2)
reliability_2$AD_DISTURBING_2 <- as.integer(recode(reliability_2$AD_DISTURBING_2, '1=7; 2=6; 3=5; 4=4; 5=3; 6=2; 7=1'))


psych::alpha(reliability_2, check.keys = TRUE)

#test passed = Cronbach alpha = 0.9 > 0.7 for exposition 1 and 0.88 > 0.7 for exposition 2.

#Creating labels

Disclosure_types <- c("AFTER", "BEFORE", "DURING", "NONE")

Labeller_terminology <- c(
  DF = "DeepFake",
  SM = "Synthetic Media"
)

#creating means for overview of attitude

MAIN_TEST$ADTRUST_1 <- apply(reliability_1, 1,mean)
MAIN_TEST$ADTRUST_2 <- apply(reliability_2, 1,mean)

#transforming ADTRUST likert scale mean into a 100 score

MAIN_TEST$ADTRUST_1 <- MAIN_TEST$ADTRUST_1*100/7
MAIN_TEST$ADTRUST_2 <- MAIN_TEST$ADTRUST_2*100/7


#Analyzing the model

#ANOVA Analysis for exposition 1

one_way_ANOVA_1 <- aov(ADTRUST_1 ~ DISCLOSURE_1,
                       data = MAIN_TEST)
summary(one_way_ANOVA_1)

two_way_ANOVA_1 <- aov(ADTRUST_1 ~ DISCLOSURE_1*TYPE_1,
                       data = MAIN_TEST)
summary(two_way_ANOVA_1)

blocking_ANOVA_1 <- aov(ADTRUST_1 ~ DISCLOSURE_1*TYPE_1 + BRAND_1,
                        data = MAIN_TEST)
summary(blocking_ANOVA_1)

value_ANOVA_1 <- aov(ADTRUST_1 ~ DISCLOSURE_1*TYPE_1*value,
                     data = MAIN_TEST)
summary(value_ANOVA_1)

value_blocking_ANOVA_1 <- aov(ADTRUST_1 ~ DISCLOSURE_1*TYPE_1*value + BRAND_1,
                              data = MAIN_TEST)
summary(value_blocking_ANOVA_1)

value_brand_ANOVA_1 <- aov(ADTRUST_1 ~ DISCLOSURE_1*TYPE_1*value*BRAND_1,
                           data = MAIN_TEST)
summary(value_brand_ANOVA_1)

#finding the best-fit model for exposition 1

model.set_1 <- list(one_way_ANOVA_1, two_way_ANOVA_1, blocking_ANOVA_1, value_ANOVA_1, value_blocking_ANOVA_1, value_brand_ANOVA_1)
model.names_1 <- c("1W_ANOVA", "2W_ANOVA", "2W_BLOCK_ANOVA", "3W_ANOVA", "3W_BLOCK_ANOVA", "4W_ANOVA")

aictab(model.set_1, modnames = model.names_1)

#testing the model for homoscedasticity in exposition 1

par(mfrow=c(2,2))
plot(blocking_ANOVA_1)
par(mfrow=c(1,1))

#post-hoc test for exposition 1

tukey.blockinganova_1 <-TukeyHSD(blocking_ANOVA_1)

tukey.blockinganova_1

#Find the groupwise differences for exposition 1

tukey.plot.aov<-aov(ADTRUST_1 ~ DISCLOSURE_1*BRAND_1, data= MAIN_TEST)
tukey.plot.test<-TukeyHSD(tukey.plot.aov)
plot(tukey.plot.test, las = 1)
par(mar=c(5,6,4,1)+.15)

#plotting first exposition interaction between DISCLOSURE_1, TYPE_1 by BRAND_1

MAIN_TEST %>%
  ggplot(aes(x = TYPE_1, group = DISCLOSURE_1, color = DISCLOSURE_1, y = ADTRUST_1))+
  stat_summary(fun = mean, geom = "point", lwd = 2)+
  stat_summary(fun = mean, geom = "line", lwd = 1.1)+
  facet_grid(~BRAND_1, labeller = labeller(
    BRAND_1 = labeller_spot, switch = "y"))+
  xlab("")+
  ylab("Score")+
  labs(fill = "Disclosure Timing")+
  ggtitle("Interaction between variables")+
  scale_x_discrete(labels = Labeller_terminology)+
  scale_fill_discrete(name = "Disclosure Timing", labels = c("After", "Before", "During", "None"))

#plotting overview_1 

MAIN_TEST %>% 
  ggplot(aes(x = DISCLOSURE_1, y = ADTRUST_1, fill = DISCLOSURE_1)) +
  geom_boxplot(stat = "boxplot", position = "dodge") +
  stat_summary(fun = mean, geom = "point", shape = 20, size = 5, color = "white", fill = "red")+
  geom_jitter(color = "black", size = 1, alpha = 0.9, width = 0.1, height = 0.1)+
  theme_ipsum()+
  theme(
    legend.position = "none",
    plot.title = element_text(size = 11)
  )+
  ggtitle("AD attitude")+
  xlab("")+
  ylab("Score")+
  labs(fill = "Disclore Timing")+
  scale_x_discrete(labels = Disclosure_types)+
  scale_y_continuous("Score", labels = score_breaks, breaks = seq(1, 100, by = 10))+
  facet_grid(BRAND_1 ~ TYPE_1, labeller = labeller(TYPE_1 = Labeller_terminology, BRAND_1 = labeller_spot)) +
  scale_fill_viridis_d(option = "B", begin = 0.3, end = 1, labels = Disclosure_types, drop = F) +
  theme_bw()

#Plot knowledge for DF and SM

MAIN_TEST %>%
  group_by(Knowledge, value) %>%
  dplyr::summarise(count = n())

Knowledge_labels <- c("Deepfake", "Synthetic Media")

MAIN_TEST %>%
  ggplot(aes(Knowledge, y= 100, fill = value))+
  ggtitle("Terminology Knowledge")+
  geom_bar(position = "fill", stat = "identity")+
  xlab("")+
  ylab("Percentage")+
  labs(fill = "Do you know what Deepfake/Synthetic Media are?")+
  scale_x_discrete(labels = Knowledge_labels)

#ANOVA Analysis for exposition 2 interaction

one_way_ANOVA_2 <- aov(ADTRUST_2 ~ DISCLOSURE_2,
                       data = MAIN_TEST)
summary(one_way_ANOVA_2)

two_way_ANOVA_2 <- aov(ADTRUST_2 ~ DISCLOSURE_2*TYPE_2,
                       data = MAIN_TEST)
summary(two_way_ANOVA_2)

blocking_ANOVA_2 <- aov(ADTRUST_2 ~ DISCLOSURE_2*TYPE_2 + BRAND_2,
                        data = MAIN_TEST)
summary(blocking_ANOVA_2)

value_ANOVA_2 <- aov(ADTRUST_2 ~ DISCLOSURE_2*TYPE_2*value,
                     data = MAIN_TEST)
summary(value_ANOVA_2)

value_blocking_ANOVA_2 <- aov(ADTRUST_2 ~ DISCLOSURE_2*TYPE_2*value + BRAND_2,
                              data = MAIN_TEST)
summary(value_blocking_ANOVA_2)

value_brand_ANOVA_2 <- aov(ADTRUST_2 ~ DISCLOSURE_2*TYPE_2*value*BRAND_2,
                           data = MAIN_TEST)
summary(value_brand_ANOVA_2)

#finding the best-fit model for exposition 2

model.set_2 <- list(one_way_ANOVA_2, two_way_ANOVA_2, blocking_ANOVA_2, value_ANOVA_2, value_blocking_ANOVA_2, value_brand_ANOVA_2)
model.names_2 <- c("1W_ANOVA", "2W_ANOVA", "2W_BLOCK_ANOVA", "3W_ANOVA", "3W_BLOCK_ANOVA", "4W_ANOVA")

aictab(model.set_2, modnames = model.names_2)

#testing the models for homoscedasticity for exposition 2

par(mfrow=c(2,2))
plot(blocking_ANOVA_2)
par(mfrow=c(1,1))

#post-hoc test for exposition 2

tukey.blockinganova_2 <-TukeyHSD(blocking_ANOVA_2)

tukey.blockinganova_2

#Find the groupwise differences for exposition 2

par(mar=c(5,9,6,1)+.2)
tukey.plot.aov_2 <-aov(ADTRUST_2 ~ DISCLOSURE_2*BRAND_2, data= MAIN_TEST)
tukey.plot.test_2 <-TukeyHSD(tukey.plot.aov_2)
plot(tukey.plot.test_2 , las = 1)

#plotting second exposition interaction between DISCLOSURE_2 and TYPE_2

MAIN_TEST %>%
  ggplot(aes(x = TYPE_2, group = DISCLOSURE_2, color = DISCLOSURE_2, y = ADTRUST_2))+
  stat_summary(fun = mean, geom = "point", lwd = 2)+
  stat_summary(fun = mean, geom = "line", lwd = 1.1)+
  facet_grid(~BRAND_2, labeller = labeller(
    BRAND_2 = labeller_spot, switch = "y"))+
  xlab("")+
  ylab("Score")+
  labs(fill = "Disclosure Timing")+
  ggtitle("Interaction between variables")+
  scale_x_discrete(labels = Labeller_terminology)+
  scale_fill_discrete(name = "Disclosure Timing", labels = c("After", "Before", "During", "None"))

#plotting overview 2

MAIN_TEST %>% 
  ggplot(aes(x = DISCLOSURE_2, y = ADTRUST_2, fill = DISCLOSURE_2)) +
  geom_boxplot(stat = "boxplot", position = "dodge") +
  stat_summary(fun = mean, geom = "point", shape = 20, size = 5, color = "white", fill = "red")+
  geom_jitter(color = "black", size = 1, alpha = 0.9, width = 0.1, height = 0.1)+
  theme_ipsum()+
  theme(
    legend.position = "none",
    plot.title = element_text(size = 11)
  )+
  ggtitle("AD attitude")+
  xlab("")+
  ylab("Score")+
  labs(fill = "Disclore Timing")+
  scale_x_discrete(labels = Disclosure_types)+
  scale_y_continuous("Score", labels = score_breaks, breaks = seq(1, 100, by = 10))+
  facet_grid(BRAND_2 ~ TYPE_2, labeller = labeller(TYPE_2 = Labeller_terminology, BRAND_2 = labeller_spot)) +
  scale_fill_viridis_d(option = "B", begin = 0.3, end = 1, labels = Disclosure_types, drop = F) +
  theme_bw()

write.xlsx(MAIN_TEST, file = "MAINTEST_2.xlsx")

