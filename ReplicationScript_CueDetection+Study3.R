################################################################################
################################################################################
###                         Replication Script for                          ###
### Constituency References in Social Media: MPs’ Usage and Voters’ Reaction ###
################################################################################
################################################################################

# Note: This script does the following:
#      1) Replicates the analyses pertaining to the third (observational) study of the paper.
#      2) Replicates the figures related to the local cues methods. 

# change the language and date formatting to English if it is not already
Sys.setenv(LANG = "EN")
Sys.setlocale("LC_TIME", "English") # key, without this conversion to POSIXct does not work
Sys.getlocale(category = "LC_ALL")


# Install packages
if('lme4' %in% rownames(installed.packages()) == FALSE) {install.packages('lme4')}
if('stargazer' %in% rownames(installed.packages()) == FALSE) {install.packages('stargazer')}
if('vtable' %in% rownames(installed.packages()) == FALSE) {install.packages('vtable')}
if('tidyr' %in% rownames(installed.packages()) == FALSE) {install.packages('tidyr')}
if('sjPlot' %in% rownames(installed.packages()) == FALSE) {install.packages('sjPlot')}
if('ggplot2' %in% rownames(installed.packages()) == FALSE) {install.packages('ggplot2')}
if('cowplot' %in% rownames(installed.packages()) == FALSE) {install.packages('cowplot')}


# Load libraries
library(lme4)
library(stargazer)
library(vtable)
library(tidyr)
library(sjPlot)
library(ggplot2)
library(cowplot)

#########################
# Set working directory #
#########################

# Set the working directory to that of the current file
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


#############
# Load data #
#############

load("./ReplicationData_CueDetection+Study3.RData")

# Notes on data frames:
# TOT_TWEET_COUNT: Counts based on all 1.3 million Tweets
# RS_TWEET_COUNT: Counts of Tweets in the random hand-coded sample
# RS_FAL_NEG: Subset of counts of random hand-coded sample that are false negatives in a very extensive definition of local cues
# CHPANACH: Swiss election data (2011-2019)


######################################################################################
# Success rate of our geospatial dictionary-based detection of local cues (Figure 2) #
######################################################################################

# Generate figure
alltweets <- ggplot(TOT_TWEET_COUNT[-which(TOT_TWEET_COUNT$local_cues == "negative"),], aes(fill=local_cues, y=pct, x=country)) + 
  geom_bar(position="stack", stat="identity", colour="black") +
  scale_fill_manual(values =  c('#d7191c',
                                '#2c7bb6'), guide = guide_legend(reverse = TRUE)) +
  theme(legend.title = element_blank(),
        legend.position = "bottom",
        legend.text = element_text(size = 12),
        axis.title.y=element_blank(),
        axis.text.y = element_text(size = 14, colour="black"), # Increase x axis label font size
        axis.title.x = element_text(size = 14, colour="black"), # Increase y axis title font size
        axis.text.x = element_text(size = 12, colour = "black"), # Increase x axis label font size
        panel.background = element_rect(fill = "white",
                                        colour = "white",
                                        size = 0.5, linetype = "solid"),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                        colour = "grey80"), 
        panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                        colour = "grey80")) +
  scale_y_continuous("Tweets (%)", position = "right") + 
  scale_x_discrete(position = "bottom", labels=c("CH" = "Switzerland", "DE" = "Germany"))+
  coord_flip()

alltweets


# Obtain the plot without the legend
alltweets_noleg <- alltweets + theme(legend.position = "none")

# Obtain the legend separately
legend <- get_plot_component(alltweets, 'guide-box-bottom', return_all = TRUE)

# Plot with cowplot
cowplot::plot_grid(alltweets_noleg, legend, nrow = 2, rel_heights = c(0.7, 0.2))
cowplot::plot_grid(alltweets_noleg, legend, nrow = 2, rel_heights = c(0.7, 0.2)) -> alltweets


# Export the figure
completefilename <- paste("./AllTweets_Fal-Tru_Positives_", as.character(format(Sys.time(), "%Y-%m-%d_%H%M")), ".png", sep = "")
png(completefilename,height = 6, width = 14.85, units = "cm", res = 1200)
alltweets
dev.off()

rm(alltweets_noleg, legend)



######################################################################################
# Comparison between dictionary-based and manual detection of local cues (Figure A1) #
######################################################################################

# Generate figure
sampletweets_classify <- ggplot(RS_TWEET_COUNT, aes(fill=local_cues, y=pct, x=country)) + 
  geom_bar(position="stack", stat="identity", colour="black") +
  # scale_fill_hue(guide = guide_legend(reverse = TRUE)) + # Flip the order of the legend
  scale_fill_manual(values =  c('#d7191c',
                                '#fdae61',
                                '#abd9e9',
                                '#2c7bb6'), guide = guide_legend(reverse = TRUE)) +
  theme(legend.title = element_blank(),
        legend.position = "bottom",
        #legend.position = c(0, 0.2),
        #legend.direction = "horizontal",
        legend.text = element_text(size = 12),
        axis.title.y=element_blank(),
        axis.text.y = element_text(size = 14, colour="black"), # Increase x axis label font size
        axis.title.x = element_text(size = 14, colour="black"), # Increase y axis title font size
        axis.text.x = element_text(size = 12, colour = "black"), # Increase x axis label font size
        panel.background = element_rect(fill = "white",
                                        colour = "white",
                                        size = 0.5, linetype = "solid"),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                        colour = "grey80"), 
        panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                        colour = "grey80")) +
  scale_y_continuous("Tweets (%)", position = "right") + 
  scale_x_discrete(position = "bottom", labels=c("CH" = "Switzerland", "DE" = "Germany"))+
  coord_flip()

sampletweets_classify

# Obtain the plot without the legend
sampletweets_classify_noleg <- sampletweets_classify + theme(legend.position = "none")

# Obtain the legend separately
legend <- get_plot_component(sampletweets_classify, 'guide-box-bottom', return_all = TRUE)

# Plot with cowplot
cowplot::plot_grid(sampletweets_classify_noleg, legend, nrow = 2, rel_heights = c(0.7, 0.2))
cowplot::plot_grid(sampletweets_classify_noleg, legend, nrow = 2, rel_heights = c(0.7, 0.2)) -> sampletweets_classify

# Export the figure
completefilename <- paste("./SampleTweets_Classification_", as.character(format(Sys.time(), "%Y-%m-%d_%H%M")), ".png", sep = "")
png(completefilename,height = 6, width = 14.85, units = "cm", res = 1200)
sampletweets_classify
dev.off()

rm(sampletweets_classify_noleg, legend)



#############################################
# Categories of false negatives (Figure A3) #
#############################################

# Generate figure
sampletweets_falneg <- ggplot(RS_FAL_NEG, aes(fill=reasoning_category, y=pct, x=country)) + 
  geom_bar(position="stack", stat="identity", colour="black") +
  # scale_fill_hue(guide = guide_legend(reverse = TRUE)) + # Flip the order of the legend
  scale_fill_manual(values =  c('#2166ac',
                                '#762a83',
                                '#af8dc3',
                                '#e7d4e8',
                                '#d9f0d3',
                                '#7fbf7b',
                                '#1b7837'),
                    guide = guide_legend(reverse = TRUE,
                                         nrow = 3, byrow = TRUE)) +
  theme(legend.title = element_blank(),
        legend.position = "bottom",
        #legend.position = c(0, 0.2),
        #legend.direction = "horizontal",
        legend.text = element_text(size = 12),
        axis.title.y=element_blank(),
        axis.text.y = element_text(size = 14, colour="black"), # Increase x axis label font size
        axis.title.x = element_text(size = 14, colour="black"), # Increase y axis title font size
        axis.text.x = element_text(size = 12, colour = "black"), # Increase x axis label font size
        panel.background = element_rect(fill = "white",
                                        colour = "white",
                                        size = 0.5, linetype = "solid"),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                        colour = "grey80"), 
        panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                        colour = "grey80")) +
  scale_y_continuous("Tweets (%)", position = "right") + 
  scale_x_discrete(position = "bottom", labels=c("CH" = "Switzerland", "DE" = "Germany"))+
  coord_flip()

sampletweets_falneg

# Obtain the plot without the legend
sampletweets_falneg_noleg <- sampletweets_falneg + theme(legend.position = "none")

# Obtain the legend separately
legend <- get_plot_component(sampletweets_falneg, 'guide-box-bottom', return_all = TRUE)

# Plot with cowplot
cowplot::plot_grid(sampletweets_falneg_noleg, legend, nrow = 2, rel_heights = c(0.3, 0.2))
cowplot::plot_grid(sampletweets_falneg_noleg, legend, nrow = 2, rel_heights = c(0.3, 0.2)) -> sampletweets_falneg

# Export the figure
completefilename <- paste("./SampleTweets_FalNeg_Categories_", as.character(format(Sys.time(), "%Y-%m-%d_%H%M")), ".png", sep = "")
png(completefilename,height = 8, width = 14.85, units = "cm", res = 1200)
sampletweets_falneg
dev.off()

rm(sampletweets_falneg_noleg, legend)





########################################################
# Summary statistics for the preference votes analysis #
########################################################

st(CHPANACH, 
   vars = c("votes_own_list_changed","cumulation_vote_pct","votes_other_lists","panachage_vote_pct","nr_of_tweets_with_localcue_campaign","total_nr_of_tweets_campaign","incumbent","tenure","candidature_types","localness","gender","votes_total"),
   labels = c("Votes own list amended (abs.)", "Votes own list amended (%)","Votes other lists (abs.)", "Votes other lists (%)","Tweets with local cues", "Total Tweets", "Incumbent MP","Tenure", "Additional Upper House candidacy", "Localness", "Female", "Total votes"),
   summ = c('notNA(x)','mean(x)','sd(x)','min(x)','max(x)'),
   summ.names = c('N','Mean','Std. Dev.','Min','Max'),
   out = "latex")



######################################################################
# The effect of Tweets with local cues on preference votes (Table 2) #
######################################################################


# Model 1: Votes own list amended (abs.)
linmod1 <- lmer(votes_own_list_changed  ~ nr_of_tweets_with_localcue_campaign_mc + total_nr_of_tweets_campaign_mc +
                   incumbent + tenure_mc + candidature_types + localness + gender + votes_total_mc + party_id_national + canton + election_date
                 + (1 | pers_id),
                 data=CHPANACH)
summary(linmod1)
tab_model(linmod1)


# Model 2: Votes own list amended (%)
linmod2 <- lmer(cumulation_vote_pct  ~ nr_of_tweets_with_localcue_campaign_mc + total_nr_of_tweets_campaign_mc +
                   incumbent + tenure_mc + candidature_types + localness + gender + votes_total_mc + party_id_national + canton + election_date
                 + (1 | pers_id),
                 data=CHPANACH)
summary(linmod2)
tab_model(linmod2)


# Model 3: Votes other lists (abs.)
linmod3 <- lmer(votes_other_lists ~ nr_of_tweets_with_localcue_campaign_mc + total_nr_of_tweets_campaign_mc +
                  incumbent + tenure_mc + candidature_types + localness + gender + votes_total_mc + party_id_national + canton + election_date
                + (1 | pers_id),
                data=CHPANACH)
summary(linmod3)
tab_model(linmod3)


# Model 4: Votes other lists (%)
linmod4 <- lmer(panachage_vote_pct ~ nr_of_tweets_with_localcue_campaign_mc + total_nr_of_tweets_campaign_mc +
                   incumbent + tenure_mc + candidature_types + localness + gender + votes_total_mc + party_id_national + canton + election_date
                 + (1 | pers_id),
                 data=CHPANACH)
summary(linmod4)
tab_model(linmod4)


# Produce a regression table with stargazer
# Note: random effects have to be added manually from tab_model output.
stargazer(linmod1,linmod2,linmod3,linmod4, title="The effect of Tweets with local cues on preference votes",
          type = "latex",
          single.row = TRUE,
          star.cutoffs = c(0.05, 0.01, 0.001),
          dep.var.caption  = "Panachage votes",
          dep.var.labels   = c("Votes own list changed (abs.)", "Votes own list changed (%)","Votes other lists (abs.)", "Votes other lists (%)"),
          omit = c("party_id","canton", "election_date"),
          omit.labels = c("Party","Canton", "Election date"),
          omit.yes.no = c("Controlled", "-"),
          covariate.labels = c("Tweets with local cues", "Total Tweets", "Incumbent MP",
                               "Tenure", "Additional Upper House candidacy", "Localness", "Female", "Total votes",
                               "Constant")
)



##################################################################################################
# The effect of Tweets with local cues on preference votes without control variables (Table A12) #
##################################################################################################


# Model 1: Votes own list amended (abs.)
linmod1_nc <- lmer(votes_own_list_changed  ~ nr_of_tweets_with_localcue_campaign_mc
                   + (1 | pers_id),
                   data=CHPANACH)
summary(linmod1_nc)
tab_model(linmod1_nc)


# Model 2: Votes own list amended (%)
linmod2_nc <- lmer(cumulation_vote_pct  ~ nr_of_tweets_with_localcue_campaign_mc
                   + (1 | pers_id),
                   data=CHPANACH)
summary(linmod2_nc)
tab_model(linmod2_nc)


# Model 3: Votes other lists (abs.)
linmod3_nc <- lmer(votes_other_lists ~ nr_of_tweets_with_localcue_campaign_mc
                   + (1 | pers_id),
                   data=CHPANACH)
summary(linmod3_nc)
tab_model(linmod3_nc)


# Model 4: Votes other lists (%)
linmod4_nc <- lmer(panachage_vote_pct ~ nr_of_tweets_with_localcue_campaign_mc
                   + (1 | pers_id),
                   data=CHPANACH)
summary(linmod4_nc)
tab_model(linmod4_nc)


# Produce a regression table with stargazer
stargazer(linmod1_nc,linmod2_nc,linmod3_nc,linmod4_nc, title="The effect of Tweets with local cues on preference votes without control variables",
          type = "latex",
          single.row = TRUE,
          star.cutoffs = c(0.05, 0.01, 0.001),
          dep.var.caption  = "Panachage votes",
          dep.var.labels   = c("Votes own list changed (abs.)", "Votes own list changed (%)","Votes other lists (abs.)", "Votes other lists (%)"),
          omit = c("party_id","canton", "election_date"),
          omit.labels = c("Party","Canton", "Election date"),
          omit.yes.no = c("Controlled", "-"),
          covariate.labels = c("Tweets with local cues")
)



###################################################################################################
# The effect of Tweets with local cues on preference votes with canton random effects (Table A13) #
###################################################################################################


# Model 1: Votes own list amended (abs.)
linmod1_cre <- lmer(votes_own_list_changed  ~ nr_of_tweets_with_localcue_campaign_mc + total_nr_of_tweets_campaign_mc +
                      incumbent + tenure_mc + candidature_types + localness + gender + votes_total_mc + party_id_national + election_date +
                      (1 | pers_id) + (1|canton),
                    data=CHPANACH)
summary(linmod1_cre)
tab_model(linmod1_cre)


# Model 2: Votes own list amended (%)
linmod2_cre <- lmer(cumulation_vote_pct  ~ nr_of_tweets_with_localcue_campaign_mc + total_nr_of_tweets_campaign_mc +
                      incumbent + tenure_mc + candidature_types + localness + gender + votes_total_mc + party_id_national + election_date +
                      (1 | pers_id) + (1|canton),
                    data=CHPANACH)
summary(linmod2_cre)
tab_model(linmod2_cre)


# Model 3: Votes other lists (abs.)
linmod3_cre <- lmer(votes_other_lists ~ nr_of_tweets_with_localcue_campaign_mc + total_nr_of_tweets_campaign_mc +
                      incumbent + tenure_mc + candidature_types + localness + gender + votes_total_mc + party_id_national + election_date +
                      (1 | pers_id) + (1|canton),
                    data=CHPANACH)
summary(linmod3_cre)
tab_model(linmod3_cre)


# Model 4: Votes other lists (%)
linmod4_cre <- lmer(panachage_vote_pct ~ nr_of_tweets_with_localcue_campaign_mc + total_nr_of_tweets_campaign_mc +
                      incumbent + tenure_mc + candidature_types + localness + gender + votes_total_mc + party_id_national + election_date +
                      (1 | pers_id) + (1|canton),
                    data=CHPANACH)
summary(linmod4_cre)
tab_model(linmod4_cre)


# Produce a regression table with stargazer
stargazer(linmod1_cre,linmod2_cre,linmod3_cre,linmod4_cre, title="The effect of Tweets with local cues on preference votes with canton random effects",
          type = "latex",
          single.row = TRUE,
          star.cutoffs = c(0.05, 0.01, 0.001),
          dep.var.caption  = "Panachage votes",
          dep.var.labels   = c("Votes own list changed (abs.)", "Votes own list changed (%)","Votes other lists (abs.)", "Votes other lists (%)"),
          omit = c("party_id", "election_date"),
          omit.labels = c("Party", "Election date"),
          omit.yes.no = c("Controlled", "-"),
          covariate.labels = c("Tweets with local cues", "Total Tweets", "Incumbent MP",
                               "Tenure", "Additional Upper House candidacy", "Localness", "Female","Total votes",
                               "Constant")
)


##################################################################################################
# The effect of Tweets with local cues on preference votes with party random effects (Table A14) #
##################################################################################################


# Model 1: Votes own list amended (abs.)
linmod1_pre <- lmer(votes_own_list_changed  ~ nr_of_tweets_with_localcue_campaign_mc + total_nr_of_tweets_campaign_mc +
                      incumbent + tenure_mc + candidature_types + localness + gender + votes_total_mc + canton + election_date +
                      (1 | pers_id) + (1|party_id_national),
                    data=CHPANACH)
summary(linmod1_pre)
tab_model(linmod1_pre)


# Model 2: Votes own list amended (%)
linmod2_pre <- lmer(cumulation_vote_pct  ~ nr_of_tweets_with_localcue_campaign_mc + total_nr_of_tweets_campaign_mc +
                      incumbent + tenure_mc + candidature_types + localness + gender + votes_total_mc + canton + election_date +
                      (1 | pers_id) + (1|party_id_national),
                    data=CHPANACH)
summary(linmod2_pre)
tab_model(linmod2_pre)


# Model 3: Votes other lists (abs.)
linmod3_pre <- lmer(votes_other_lists ~ nr_of_tweets_with_localcue_campaign_mc + total_nr_of_tweets_campaign_mc +
                      incumbent + tenure_mc + candidature_types + localness + gender + votes_total_mc + canton + election_date +
                      (1 | pers_id) + (1|party_id_national),
                    data=CHPANACH)
summary(linmod3_pre)
tab_model(linmod3_pre)


# Model 4: Votes other lists (%)
linmod4_pre <- lmer(panachage_vote_pct ~ nr_of_tweets_with_localcue_campaign_mc + total_nr_of_tweets_campaign_mc +
                      incumbent + tenure_mc + candidature_types + localness + gender + votes_total_mc + canton + election_date +
                      (1 | pers_id) + (1|party_id_national),
                    data=CHPANACH)
summary(linmod4_pre)
tab_model(linmod4_pre)


# Produce a regression table with stargazer
stargazer(linmod1_pre,linmod2_pre,linmod3_pre,linmod4_pre, title="The effect of Tweets with local cues on preference votes with canton random effects",
          type = "latex",
          single.row = TRUE,
          star.cutoffs = c(0.05, 0.01, 0.001),
          dep.var.caption  = "Panachage votes",
          dep.var.labels   = c("Votes own list changed (abs.)", "Votes own list changed (%)","Votes other lists (abs.)", "Votes other lists (%)"),
          omit = c("canton", "election_date"),
          omit.labels = c("Canton", "Election date"),
          omit.yes.no = c("Controlled", "-"),
          covariate.labels = c("Tweets with local cues", "Total Tweets", "Incumbent MP",
                               "Tenure", "Additional Upper House candidacy", "Localness", "Female","Total votes",
                               "Constant")
)



##################################################################################################
# The effect of Tweets with local cues on preference votes with party random effects (Table A11) #
##################################################################################################

# Note: This is for the observational part of the appendix discussion on nativist cues with CHES data.

# Model 1: Votes own list amended (abs.)
linmod1_nativ <- lmer(votes_own_list_changed  ~ nr_of_tweets_with_localcue_campaign_mc*nationalism  + total_nr_of_tweets_campaign_mc +
                        incumbent + tenure_mc + candidature_types + localness + gender + votes_total_mc #+ party_id_national 
                      + canton + election_date
                      + (1 | pers_id),
                      data=CHPANACH)
summary(linmod1_nativ)
tab_model(linmod1_nativ)


# Model 2: Votes own list amended (%)
linmod2_nativ <- lmer(cumulation_vote_pct  ~ nr_of_tweets_with_localcue_campaign_mc*nationalism  + total_nr_of_tweets_campaign_mc +
                        incumbent + tenure_mc + candidature_types + localness + gender + votes_total_mc #+ party_id_national 
                      + canton + election_date
                      + (1 | pers_id),
                      data=CHPANACH)
summary(linmod2_nativ)
tab_model(linmod2_nativ)


# Model 3: Votes other lists (abs.)
linmod3_nativ <- lmer(votes_other_lists ~ nr_of_tweets_with_localcue_campaign_mc*nationalism  + total_nr_of_tweets_campaign_mc +
                        incumbent + tenure_mc + candidature_types + localness + gender + votes_total_mc #+ party_id_national 
                      + canton + election_date
                      + (1 | pers_id),
                      data=CHPANACH)
summary(linmod3_nativ)
tab_model(linmod3_nativ)


# Model 4: Votes other lists (%)
linmod4_nativ <- lmer(panachage_vote_pct ~ nr_of_tweets_with_localcue_campaign_mc*nationalism  + total_nr_of_tweets_campaign_mc +
                        incumbent + tenure_mc + candidature_types + localness + gender + votes_total_mc #+ party_id_national 
                      + canton + election_date
                      + (1 | pers_id),
                      data=CHPANACH)
summary(linmod4_nativ)
tab_model(linmod4_nativ)


# Produce a regression table with stargazer
stargazer(linmod1_nativ,linmod2_nativ,linmod3_nativ,linmod4_nativ, title="The effect of Tweets with local cues on preference votes depending on party nationalism (CHES)",
          type = "latex",
          single.row = TRUE,
          star.cutoffs = c(0.05, 0.01, 0.001),
          dep.var.caption  = "Panachage votes",
          dep.var.labels   = c("Votes own list changed (abs.)", "Votes own list changed (%)","Votes other lists (abs.)", "Votes other lists (%)"),
          omit = c("party_id","canton", "election_date"),
          omit.labels = c("Party","Canton", "Election date"),
          omit.yes.no = c("Controlled", "-"),
          covariate.labels = c("Tweets with local cues", "Party-level nativism","Total Tweets", "Incumbent MP",
                               "Tenure", "Additional Upper House candidacy","Localness" ,"Female", "Total votes","Tweets with local cues$\\\times$Party-level nativism",
                               "Constant")
)

