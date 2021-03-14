#############################################################################################
# Australian and New Zealand Judicial Supervision Meta-analysis: Analysis
#############################################################################################
########################################
# I) Table of Contents 
#####################################
# I:  Table of Contents
# 1:  Packages & functions
#     1.1:    Packages
#     1.2:    Functions
# 2:  Data    cleaning and wrangling
#     2.1:    Data
#     2.2:    Make relevant variables numeric
#     2.3:    Data check
#     2.4:    Transform and create proportion variables  
# 3: Proportion who recidivated 
#     3.1:     Separate target rows 
#     3.2:     Import complex data 
#     3.3:     Meta-analysis of proportion that recidivated outcomes  
#     3.4:     Relative recidivism rate
#     3.5:     Forest plot: Proportion that recidivated 
#     3.6:     Sensitivity analysis
#                 3.6.1:     Leave-One-Out 
#                 3.6.2:     Alternative probit scaling values
#                 3.6.3:     Alternative models
#                         3.6.3.1:     Fixed effect model   
#                         3.6.3.2:     Intention-to-treat: graduates only
#                 3.7: Publication bias      
# 4: Moderator analysis: Proportion who recidivated
#     4.1:     Year of publication
#     4.2:     Jurisdiction
#                 4.2.1:     New Zealand
#                 4.2.2:     Victoria 
#                 4.2.3:     Queensland
#                 4.2.4:     Australia 
#     4.3:     Outcome  
#     4.4:     Measure
#                 4.4.1:     Offences 
#                 4.4.2:     Proven offences
#     4.5:     Time status of measure
#                 4.5.1:     Elapsed time 
#                 4.5.2:     Free time  
#                 4.5.3:     Not reported
#     4.6: Court setting
#                  4.6.1:     Community court
#                  4.6.2:     Drug court
#                  4.6.3:     Mainstream court
#     4.7:     Study groups
#                  4.7.1:     Combined groups
#                  4.7.2:     Standard
#     4.8:     Partially matched
#     4.9:     Intention-to-treat
#                  4.9.1:     Graduates
#                  4.9.2:     Graduates/Failures
#                  4.9.3:     Not reported
#     4.10:    Lifetime arrests - Treatment 
#     4.11:    Lifetime arrests - Control 
#     4.12:    Lifetime convictions - Treatment 
#     4.13:    Lifetime convictions - Control 
#     4.14:    Mean age - Treatment 
#     4.15:    Mean age - Control 
#     4.16:    Proportion male - Treatment 
#     4.17:    Proportion male - Control 
#     4.18:    Proportion ATSI or Maori - Treatment 
#     4.19:    Proportion ATSI or Maori - Control 
#     4.20:    Post-test period months 
#     4.21:    Post-test begins 
#     4.22:    Measured during or post treatment
#                  4.22.1:    During
#                  4.22.2:    Post
#     4.23:    Program duration minimum (months)
#     4.24:    Program duration average (months)
#     4.25:    Post treatment months - Treatment
#     4.26:    Post treatment months - Control
#     4.27:    Charge level
#                  4.27.1:    Summary
#                  4.27.2:    Summary/indictable
#     4.28:    Accepted violent offenders
#                  4.28.1:    No
#                  4.28.2:    Yes
#     4.29:    Reported accepting violent offenders
#                  4.29.1:    No
#                  4.29.2:    Yes
#     4.30:    Reported accepting indictable charges
#                  4.30.1:    No
#                  4.30.2:    Yes
#     4.31:    Reported expunging charges
#     4.32:    Proportion of sample that were graduates - Treatment
#     4.33:    Proportion of sample that were terminates - Treatment
#     4.34:    Proportion of sample that completed order - Treatment
#     4.35:    Proportion of sample that completed order - Control
#     4.36:    Publication type
#                  4.36.1:    Evaluation
#                  4.36.2:    Journal article
#     4.37:    Publication status
#     4.38:    Stage of disposition
#                  4.38.1:    Post-plea
#                  4.38.2:    Pre-plea
#     4.39:    Reported a single judicial officer presiding over hearings
#                  4.39.1:    No
#                  4.39.2:    Yes
#     4.40:    Number of judicial officers presiding over hearings
#     4.41:    Reported team training
#                  4.41.1:    No
#                  4.41.2:    Yes
#     4.42:    Reported judicial officer training
#     4.43:    Reported judicial officer or team training
#     4.44:    Minutes per review hearing (average)
#     4.45:    Reported frequent team meetings
#                  4.45.1:    No
#                  4.45.2:    Yes
#     4.46:    Reported using sanctions or graduated sanctions
#                  4.46.1:    Graduated
#                  4.46.2:    Sanctions
#                  4.46.3:    Sanctions or graduated sanctions
#     4.47:    Reported using rewards
#                  4.47.1:    No
#                  4.47.2:    Yes
#     4.48:    Reported using sanctions and rewards
#     4.49:    Frequency of review hearings
#     4.50:    Frequent vs monthly review hearings
#     4.51:    Reported frequent review hearings 
#                  4.51.1:    No
#                  4.51.2:    Yes
#     4.52:    Reported measuring risk - Treatment
#                  4.52.1:    No
#                  4.52.2:    Yes
#     4.53:    Reported measuring risk - Control
#                  4.53.1:    No
#                  4.53.2:    Yes
#     4.54:    Reported individualising treatment - Treatment
#     4.55:    Reported individualising treatment - Control
#     4.56:    Reported drug or alcohol treatment - Treatment
#                  4.56.1:    Offered
#                  4.56.2:    Required
#     4.57:    Type of drug or alcohol treatment - Treatment
#     4.58:    Reported drug or alcohol treatment - Control
#     4.59:    Type of drug or alcohol treatment - Control
#     4.60:    Reported drug or alcohol testing - Treatment
#                  4.60.1:    No
#                  4.60.2:    Offered or required
#     4.61:    Reported drug or alcohol testing - Control
#     4.62:    Reported detox treatment - Treatment
#                  4.62.1:    No
#                  4.62.2:    Offered
#     4.63:    Reported detox treatment - Control
#     4.64:    Reported pharmacotherapy - Treatment
#     4.65:    Reported pharmacotherapy - Control
#     4.66:    Reported relapse prevention - Treatment
#                  4.66.1:    No
#                  4.66.2:    Offered or required
#     4.67:    Reported relapse prevention - Control
#     4.68:    Reported psychiatric treatment - Treatment
#     4.69:    Reported psychiatric treatment - Control
#     4.70:    Reported mental health treatment - Treatment
#                  4.70.1:    No
#                  4.70.2:    Offered
#     4.71:    Reported mental health treatment - Control
#     4.72:    Reported cognitive-behavioral therapy - Treatment
#                  4.72.1:    No
#                  4.72.2:    Offered or required
#     4.73:    Reported cognitive-behavioral therapy - Control
#     4.74:    Reported counseling - Treatment 
#                  4.74.1:    No
#                  4.74.2:    Required
#                  4.74.3:    Offered
#     4.75:    Reported counseling - Control 
#     4.76:    Reported housing support - Treatment 
#                  4.76.1:    No
#                  4.76.2:    Offered
#     4.77:    Reported housing support - Control 
#     4.78:    Reported support groups - Treatment 
#                  4.78.1:    No
#                  4.78.2:    Offered or required
#     4.79:    Reported support groups - Control 
#     4.80:    Reported material aid - Treatment 
#                  4.80.1:    No
#                  4.80.2:    Offered
#     4.81:    Reported material aid - Control 
#     4.82:    Reported health services - Treatment 
#     4.83:    Reported health services - Control 
#     4.84:    Reported domestic violence program - Treatment
#                  4.84.1:    No
#                  4.84.2:    Offered
#     4.85:    Reported domestic violence program - Control 
#     4.86:    Reported vocational or educational training - Treatment 
#                  4.86.1:    No
#                  4.86.2:    Offered or required
#     4.87:    Reported vocational or educational services - Control 
#     4.88:    Reported anger management - Treatment 
#                  4.88.1:    No
#                  4.88.2:    Offered
#     4.89:    Reported anger management - Control 
#     4.90:    Reported life-skills training - Treatment 
#                  4.90.1:    No
#                  4.90.2:    Offered
#     4.91:    Reported life-skills training - Control 
#     4.92:    Reported family support services - Treatment 
#     4.93:    Reported family support services - Control 
#     4.94:    Reported prison intervention - Control
#                  4.94.1:    No
#                  4.94.2:    Yes
#     4.95:    Other treatments - Treatment 
#     4.96:    Other treatments - Control 
#     4.97:    Total number of treatments required - Treatment 
#     4.98:    Total number of treatments required - Control 
#     4.99:    Total number of treatments offered - Treatment 
#     4.100:   Total number of treatments offered - Control 
#                  4.100.1:   None reported
#                  4.100.2:   One reported
#     4.102:   Modified Maryland Scientific methods scale rating
#     4.103:   Modified Maryland Scientific methods scale rating - Sensitivity analysis re-code a: midway category
#     4.104:   Modified Maryland Scientific methods scale rating - Sensitivity analysis re-code b: considered 'well-matched' 
# 5: Mean recidivist events
#     5.1:     Separate target rows 
#     5.2:     Incidents and free days variables 
#     5.2:     Incidents and free days variables 
#     5.3:     Meta-analysis of recidivist events: conditional generalized linear mixed-effects model using exact likelihood
#     5.4:     Forest plot: mean recidivist events
#     5.5:     Sensitivity analysis: Mean recidivist event outcomes
#                  5.5.1:     Unconditional generalized linear mixed-effects model with fixed study effects
#                  5.5.2:     Unconditional generalized linear mixed-effects model with random study effects
#                  5.5.3:     Publication bias
# 6: Moderator analysis: Mean recidivist events
#     6.1:     Year of publication
#     6.2:     Jurisdiction
#                 6.2.1:     New Zealand
#                 6.2.2:     New South Wales
#                 6.2.3:     Queensland
#                 6.2.4:     Australia 
#     6.3:     Outcome  
#                 6.3.1:     Offences
#                 6.3.2:     Proven offences 
#     6.4:     Measure
#     6.5:     Time status of measure
#     6.6: Court setting
#     6.7:     Study groups
#     6.8:     Study groups (alternative)
#     6.9:     Intention-to-treat
#     6.10:    Lifetime arrests - Treatment 
#     6.11:    Lifetime arrests - Control 
#     6.12:    Lifetime convictions - Treatment 
#     6.13:    Lifetime convictions - Control 
#     6.14:    Mean age - Treatment 
#     6.15:    Mean age - Control 
#     6.16:    Proportion male - Treatment 
#     6.17:    Proportion male - Control 
#     6.18:    Proportion ATSI or Maori - Treatment 
#     6.19:    Proportion ATSI or Maori - Control 
#     6.20:    Pre-treatment mean - Treatment  
#     6.21:    Pre-treatment mean - Control 
#     6.22:    Post-test period months
#     6.23:    Post-test begins 
#     6.24:    Measured during or post treatment
#                  6.24.1:    During
#                  6.24.2:    Post
#     6.25:    Program duration minimum (months)
#     6.26:    Program duration average (months)
#     6.27:    Post treatment months - Treatment
#     6.28:    Post treatment months - Control
#     6.29:    Charge level
#     6.30:    Accepted violent offenders
#                  6.30.1:    No
#                  6.30.2:    Yes
#     6.31:    Reported accepting violent offenders
#     6.32:    Reported accepting indictable charges
#     6.33:    Reported expunging charges
#     6.34:    Proportion of sample that were graduates - Treatment
#     6.35:    Proportion of sample that were terminates - Treatment
#     6.36:    Proportion of sample that completed order - Treatment
#     6.37:    Proportion of sample that completed order - Control
#     6.38:    Publication type
#     6.39:    Publication status
#     6.40:    Research design
#     6.41:    Stage of disposition
#     6.42:    Reported a single judicial officer presiding over hearings
#                  6.42.1:    No
#                  6.42.2:    Yes
#     6.43:    Number of judicial officers presiding over hearings
#     6.44:    Reported team training
#     6.45:    Reported judicial officer training
#     6.46:    Reported judicial officer or team training
#     6.47:    Minutes per review hearing (average)
#     6.48:    Reported frequent team meetings
#                  6.48.1:    No
#                  6.48.2:    Yes
#     6.49:    Reported using sanctions
#     6.50:    Reported graduated sanctions
#                  6.50.1:    No
#                  6.50.2:    Yes
#     6.51:    Reported using sanctions or graduated sanctions
#     6.52:    Reported using rewards
#                  6.52.1:    No
#                  6.52.2:    Yes
#     6.53:    Reported using sanctions and rewards
#     6.54:    Frequency of review hearings
#                  6.54.1:    Fortnightly
#                  6.54.2:    Weekly
#     6.55:    Frequency duration
#     6.56:    Frequent vs monthly review hearings
#     6.57:    Reported frequent review hearings 
#                  6.57.1:    No
#                  6.57.2:    Yes
#     6.58:    Reported measuring risk - Treatment
#                  6.58.1:    No
#                  6.58.2:    Yes
#     6.59:    Reported measuring risk - Control
#     6.60:    Reported individualising treatment - Treatment
#     6.61:    Reported individualising treatment - Control
#     6.62:    Reported drug or alcohol treatment - Treatment
#     6.63:    Type of drug or alcohol treatment - Treatment
#     6.64:    Reported drug or alcohol treatment - Control
#     6.65:    Type of drug or alcohol treatment - Control
#     6.66:    Reported drug or alcohol testing - Treatment
#     6.67:    Reported drug or alcohol testing - Control
#     6.68:    Reported detox treatment - Treatment
#                  6.68.1:    No
#                  6.68.2:    Offered or required
#     6.69:    Reported detox treatment - Control
#     6.70:    Reported pharmacotherapy - Treatment
#                  6.70.1:    No
#                  6.70.2:    Offered
#     6.71:    Reported pharmacotherapy - Control
#     6.72:    Reported relapse prevention - Treatment
#                  6.72.1:    No
#                  6.72.2:    Offered
#                  6.72.3:    Required
#     6.73:    Reported relapse prevention - Control
#     6.74:    Reported psychiatric treatment - Treatment
#     6.75:    Reported psychiatric treatment - Control
#     6.76:    Reported mental health treatment - Treatment
#     6.77:    Reported mental health treatment - Control
#     6.78:    Reported cognitive-behavioral therapy - Treatment
#                  6.78.1:    No
#                  6.78.2:    Offered or required
#     6.79:    Reported cognitive-behavioral therapy - Control
#     6.80:    Reported counseling - Treatment 
#     6.81:    Reported counseling - Control 
#     6.82:    Reported housing support - Treatment 
#                  6.82.1:    No
#                  6.82.2:    Offered
#     6.83:    Reported housing support - Control 
#     6.84:    Reported support groups - Treatment 
#     6.85:    Reported support groups - Control 
#     6.86:    Reported material aid - Treatment 
#                  6.86.1:    No
#                  6.86.2:    Offered
#     6.87:    Reported material aid - Control 
#     6.88:    Reported health services - Treatment 
#     6.89:    Reported health services - Control 
#     6.90:    Reported domestic violence program - Treatment
#     6.91:    Reported domestic violence program - Control 
#     6.92:    Reported vocational or educational training - Treatment 
#                  6.92.1:    No
#                  6.92.2:    Offered or required
#     6.93:    Reported vocational or educational services - Control 
#     6.94:    Reported anger management - Treatment 
#                  6.94.1:    No
#                  6.94.2:    Offered
#     6.95:    Reported anger management - Control 
#     6.96:    Reported life-skills training - Treatment 
#                  6.96.1:    No
#                  6.96.2:    Offered or required
#     6.97:    Reported life-skills training - Control 
#     6.98:    Reported family support services - Treatment 
#     6.99:    Reported family support services - Control 
#     6.100:   Reported prison intervention - Control
#     6.101:   Other treatments - Treatment 
#     6.102:   Other treatments - Control 
#     6.103:   Total number of treatments required - Treatment 
#     6.104:   Total number of treatments required - Control 
#     6.105:   Total number of treatments offered - Treatment 
#     6.106:   Total number of treatments offered - Control 
#                  6.100.1:   None reported
#                  6.100.2:   One reported
#     6.107:   Modified Maryland Scientific methods scale rating
#     6.108:   Modified Maryland Scientific methods scale rating - Sensitivity analysis re-code a: midway category
#     6.109:   Modified Maryland Scientific methods scale rating - Sensitivity analysis re-code b: considered 'well-matched' 
# 7: Moderator analysis: Z test on subgroup differences
#     7.1:     Proportion who recidivated
#                  7.1.1:     Z test on subgroup differences
#                  7.1.2:     Benjamini-Hochberg procedure
#                  7.1.3:     Clean proportion who recidivated moderator table
#     7.2:     Mean recidivist events
#                  7.2.1:     Z test on subgroup differences
#                  7.2.2:     Benjamini-Hochberg procedure
#                  7.2.3:     Clean mean recidivist events moderator table
#     7.3:     Merge and export moderator tables 
# 8: Pooled Effects tables
#     8.1:     Proportion who recidivated
#     8.2:     Mean recidivist events
########################################
# II) References
#####################################
#   Amemiya, T. (1981). Qualitative response models: A survey. Journal of economic literature, 19(4), 1483-1536. 
#
#   Borenstein, M., Hedges, L. V., Higgins, J. P., & Rothstein, H. R. (2011). Introduction to meta-analysis. John Wiley & Sons.         
#
#   Viechtbauer, W. (n.d.). Convergence Problems with the rma() Function. http://www.metafor-project.org/doku.php/tips:convergence_problems_rma?do=
#
#   Viechtbauer, W. (n.d.). Stijnen et al. (2010). http://www.metafor-project.org/doku.php/analyses:stijnen2010
#
#   Wilson, D. B., Mitchell, O., & MacKenzie, D. L. (2006). A systematic review of drug court effects on recidivism. 
#               Journal of Experimental Criminology, 2(4), 459-487. doi:10.1007/s11292-006-9019-4
#
########################################
# 1) Packages & functions
########################################
########################################
# 1.1) Packages
#####################################
library(dplyr)
library(plyr)
library(tidyverse)
library(metafor)
library(forestplot)
library(data.table)
library(grid)
########################################
# 1.2) Functions
#####################################
# Round up from 0.5 and will need to use the below function
round2 = function(x, n) {
  posneg = sign(x)
  z = abs(x)*10^n
  z = z + 0.5
  z = trunc(z)
  z = z/10^n
  z*posneg
}

# For the purposes of exporting and converting p-values that are <.01 so that they export as <.01
pvalr <- function(pvals, sig.limit = .001, digits = 3, html = FALSE) {
  
  roundr <- function(x, digits = 1) {
    res <- sprintf(paste0('%.', digits, 'f'), x)
    zzz <- paste0('0.', paste(rep('0', digits), collapse = ''))
    res[res == paste0('-', zzz)] <- zzz
    res
  }
  
  sapply(pvals, function(x, sig.limit) {
    if (x < sig.limit)
      if (html)
        return(sprintf('&lt; %s', format(sig.limit))) else
          return(sprintf('< %s', format(sig.limit)))
    if (x > .1)
      return(roundr(x, digits = 2)) else
        return(roundr(x, digits = digits))
  }, sig.limit = sig.limit)
}

#splice levels
splice_levels <- function(w, x){
  y <- anti_join(w, x, by = c("Moderator", "Level")) %>% group_by(Moderator)
  y[, c(6:9)] <- ' - '
  y[, 5] <- NA
  z <- rbind(x,y, fill = TRUE)
  z <- arrange(z, mod_category, Moderator)
  z$k <- ifelse(is.na(z$k)== TRUE, ' - ', z$k)
  if (!exists("z$OR")){
    z$OR <- ifelse(is.na(z$OR)== TRUE, ' - ', z$OR)
  }
  if (!exists("z$IRR")){
    z$IRR <- ifelse(is.na(z$IRR)== TRUE, ' - ', z$IRR)
  }
  z<- z[,-10]
  return(z)
}
########################################
# 2) Data cleaning and wrangling
########################################
########################################
# 2.1) Data
#####################################
AUSNZ_DB <- read.csv(url("https://raw.githubusercontent.com/mtrood/effects-of-judicial-supervision-on-recidivisim-of-offenders-in-Australia-and-New-Zealand/main/AUSNZ%20JSMA.csv"))
########################################
# 2.2) Make relevant variables numeric
#####################################
AUSNZ_wrangle <- AUSNZ_DB

# Convert appropriate variables to numeric 
AUSNZ_wrangle_num <- AUSNZ_wrangle %>% select(starts_with("LT"), starts_with("mean"),starts_with("pt"), -"pt_period_months", -"pt_begins", -"pt_order_name_treat", 
                               -"pt_order_name_treat", -"pt_order_name_cont", starts_with("male"),starts_with("ATSI"),starts_with("bl"),
                                contains("program"),contains("Average"), starts_with("total")) %>% 
                                                    apply(2,function(x) as.numeric(as.character(x))) 

AUSNZ_wrangle_no_num<- AUSNZ_wrangle %>%select(-starts_with("LT"), -starts_with("mean"),-starts_with("pt"), "pt_period_months", "pt_begins", "pt_order_name_treat", 
                                     "pt_order_name_treat", "pt_order_name_cont", -starts_with("male"),-starts_with("ATSI"),-starts_with("bl"),
                                     -contains("program"),-contains("Average"), -starts_with("total")) 


AUSNZ_wrangle_num_compl <- cbind(AUSNZ_wrangle_num,AUSNZ_wrangle_no_num) #bind numeric and non-numeric variables
AUSNZ_wrangle_num_compl <- AUSNZ_wrangle_num_compl[,names(AUSNZ_wrangle)] #reset order of variables

########################################
# 2.3) Data check
#####################################
percent_check <- AUSNZ_wrangle_num_compl%>% 
      apply(.,2,function(x) as.character(x))%>%
      apply(.,2,function(x) str_detect(x,"%")) #check for percent (%) symbols 

sum(percent_check==TRUE, na.rm=T) #how many observations contain %

# Note: many of the levels of the 'measure' variable will contain % due the measure used. 
sum(percent_check==TRUE, na.rm=T)==sum(str_count(AUSNZ_wrangle_num_compl$measure, coll("%"))) #logical stating whether number of observed % signs matches the number counted in the 'measure' column 

########################################
# 2.4: Transform and create proportion variables  
#####################################
# First, remove any rows that have complex data. They will be rejoined to the data frame after have had their effect sizes calculated
AUSNZ_wrangle_non_prop <- subset(AUSNZ_wrangle_num_compl, is.na(AUSNZ_wrangle_num_compl$pt_treat_Dichotomous_. & AUSNZ_wrangle_num_compl$pt_cont_Dichotomous_.))
AUSNZ_wrangle_prop <- subset(AUSNZ_wrangle_num_compl, !(is.na(AUSNZ_wrangle_num_compl$pt_treat_Dichotomous_. & AUSNZ_wrangle_num_compl$pt_cont_Dichotomous_.)))

# add a decimal place to the dichotomous variables to make them into proportions, ensuring any single digit values remain s single digits 
AUSNZ_wrangle_prop$pt_treat_Dichotomous_.<- AUSNZ_wrangle_prop$pt_treat_Dichotomous_. %>% 
                                                                              str_pad(., 2, side="left", pad="0") %>% 
                                                                              gsub('\\.','', .) %>% 
                                                                              paste0('.', .) %>%
                                                                              as.numeric(.)
AUSNZ_wrangle_prop$pt_cont_Dichotomous_.<- AUSNZ_wrangle_prop$pt_cont_Dichotomous_. %>% 
                                                                              str_pad(., 2, side="left", pad="0") %>% 
                                                                              gsub('\\.','', .) %>% 
                                                                              paste0('.', .) %>%
                                                                              as.numeric(.)
# Create proportion variables 
AUSNZ_wrangle_prop$pt_treat_prop_recid_yes <- round2(AUSNZ_wrangle_prop$pt_treat_Dichotomous_. * AUSNZ_wrangle_prop$pt_treat_Dichotomous_n, 0)
AUSNZ_wrangle_prop$pt_treat_prop_recid_no <- round2((1-AUSNZ_wrangle_prop$pt_treat_Dichotomous_.) * AUSNZ_wrangle_prop$pt_treat_Dichotomous_n, 0)

AUSNZ_wrangle_prop$pt_cont_prop_recid_yes <- round2(AUSNZ_wrangle_prop$pt_cont_Dichotomous_. * AUSNZ_wrangle_prop$pt_cont_Dichotomous_n, 0)
AUSNZ_wrangle_prop$pt_cont_prop_recid_no <- round2((1-AUSNZ_wrangle_prop$pt_cont_Dichotomous_.) * AUSNZ_wrangle_prop$pt_cont_Dichotomous_n, 0)

# Bind AUSNZ_wrangle_non_prop together 
AUSNZ_wrangle_final <- rbind.fill(AUSNZ_wrangle_prop, AUSNZ_wrangle_non_prop)

AUSNZ_wrangle_final <- AUSNZ_wrangle_final[order(AUSNZ_wrangle_final$studyid), ] #reset order of rows
########################################
# 3: Proportion who recidivated
########################################
########################################
# 3.1: Separate target rows 
#####################################
# Separate all those studies that included a dichotomous measure of recidivism
prop_recid <- AUSNZ_wrangle_final %>% subset(., includes_complete_dichotomous== "Yes")

# Separate the measures related to proportion that recidivated  
prop_recid_meas <- prop_recid %>% subset(., measure== "% breaches"| measure== "% charges"|
                                               measure== "% convicted"| measure== "% new offence"| measure== "% new offence episode"| measure=="% offence episodes (regressed)" | 
                                               measure== "% proven offence"| measure== "% proven drug offence"| measure== "% proven family violence offence"| measure== "% proven property offence"|
                                               measure== "% proven offence (excluding breach)"| measure== "% proven offence leading to imprisonment"| 
                                               measure== "% proven serious offence"| measure== "% proven traffic offence"| measure== "% proven violent offence"|
                                               measure== "% rearrested"| measure== "% sentenced to prison"| measure== "survival") 

# Separate the preferred measures 
prop_recid_prefmeas <- subset(prop_recid_meas, subset = measure %in% 
                               c("% charges", "% convicted", "% new offence","% new offence episode","% offence episodes (regressed)","% proven offence")) #subsets the preferred measures

prop_recid_unprefmeas <- subset(prop_recid_meas, !(prop_recid_meas$measure== "% charges"
                                                  |prop_recid_meas$measure== "% convicted"
                                                  |prop_recid_meas$measure== "% new offence"
                                                  |prop_recid_meas$measure== "% new offence episode"
                                                  |prop_recid_meas$measure== "% offence episodes (regressed)"
                                                  |prop_recid_meas$measure== "% proven offence")) #subsets all other measures

prop_recid_unprefmeas <- prop_recid_unprefmeas[!(prop_recid_unprefmeas$studyid%in%prop_recid_prefmeas$studyid),] #removes unpreferred rows if their study ID matches a study ID in the preferred object. 

prop_recid_bound<-bind_rows(prop_recid_prefmeas,prop_recid_unprefmeas) #bind the two data frames together. 

sum(unique(prop_recid_bound$studyid)>0)==sum(unique(prop_recid_meas$studyid)>0) #check that the number of study IDs matches the original subset

# Separate rows with '% offence episodes (regressed)' as preferred
prop_recid_prefmeas <- subset(prop_recid_bound, subset = measure %in% c("% offence episodes (regressed)", "% proven offence"))
prop_recid_unprefmeas <- subset(prop_recid_bound, !(prop_recid_bound$measure== "% offence episodes (regressed)"
                                                    |prop_recid_bound$measure=="% proven offence")) 
prop_recid_unprefmeas <- prop_recid_unprefmeas[!(prop_recid_unprefmeas$studyid%in%prop_recid_prefmeas$studyid),] 
prop_recid_bound<-bind_rows(prop_recid_prefmeas, prop_recid_unprefmeas) 

sum(unique(prop_recid_bound$studyid)>0)==sum(unique(prop_recid_meas$studyid)>0)

# Separate preferred outcomes 
# Note: 'proven offences' are preferred to 'offences' here 
prop_recid_prefout <- subset(prop_recid_bound, subset = outcome %in% 
                               c("proven offence", "conviction", "proven offence","rearrest","theft or drug offences"))
prop_recid_unprefout <- subset(prop_recid_bound, !(prop_recid_bound$outcome=="proven offence"
                                         |prop_recid_bound$outcome== "conviction"
                                         |prop_recid_bound$outcome== "proven offence"
                                         |prop_recid_bound$outcome== "rearrest"
                                         |prop_recid_bound$outcome== "theft or drug offences")) 
prop_recid_unprefout <- prop_recid_unprefout[!(prop_recid_unprefout$studyid%in%prop_recid_prefout$studyid),] 
prop_recid_bound<-bind_rows(prop_recid_prefout,prop_recid_unprefout)

sum(unique(prop_recid_bound$studyid)>0)==sum(unique(prop_recid_meas$studyid)>0)

# Repeat with 'offences'.
prop_recid_prefout <- subset(prop_recid_bound, subset = outcome %in% 
                               c("offences", "proven offence", "conviction", "proven offence","rearrest","theft or drug offences"))
prop_recid_unprefout <- subset(prop_recid_bound, !(prop_recid_bound$outcome=="offences"
                                                       |prop_recid_bound$outcome== "proven offence"
                                                       |prop_recid_bound$outcome== "conviction"
                                                       |prop_recid_bound$outcome== "proven offence"
                                                       |prop_recid_bound$outcome== "rearrest"
                                                       |prop_recid_bound$outcome== "theft or drug offences")) 
prop_recid_unprefout <- prop_recid_unprefout[!(prop_recid_unprefout$studyid%in%prop_recid_prefout$studyid),] 
prop_recid_bound<-bind_rows(prop_recid_prefout,prop_recid_unprefout)

sum(unique(prop_recid_bound$studyid)>0)==sum(unique(prop_recid_meas$studyid)>0)

# Separate preferred rows by intention to treat
# note: firstly make an object for the graduates that will be used later. 
prop_recid_grads <- subset(prop_recid_bound, (prop_recid_bound$ITT=="Graduates"))

prop_recid_prefitt <- subset(prop_recid_bound, subset = ITT %in% 
                               c("Graduates/Failures", "Not reported"))
prop_recid_unprefitt <- subset(prop_recid_bound, !(prop_recid_bound$ITT== "Graduates/Failures"
                                                       |prop_recid_bound$ITT== "Not reported")) 
prop_recid_unprefitt <- prop_recid_unprefitt[!(prop_recid_unprefitt$studyid%in%prop_recid_prefitt$studyid),] 
prop_recid_bound<-bind_rows(prop_recid_prefitt,prop_recid_unprefitt)

sum(unique(prop_recid_bound$studyid)>0)==sum(unique(prop_recid_meas$studyid)>0)

# Separate preferred studygroups alternative giving preference to partially matched groups 
prop_recid_prefstudygroups_alt<- subset(prop_recid_bound, subset = studygroups_alternative %in% "partially matched")
prop_recid_unprefstudygroups_alt<- subset(prop_recid_bound, !(prop_recid_bound$studygroups_alternative== "partially matched")) 
prop_recid_unprefstudygroups_alt<- prop_recid_unprefstudygroups_alt[!(prop_recid_unprefstudygroups_alt$studyid%in%prop_recid_prefstudygroups_alt$studyid),] 
prop_recid_bound<-bind_rows(prop_recid_prefstudygroups_alt,prop_recid_unprefstudygroups_alt)

sum(unique(prop_recid_bound$studyid)>0)==sum(unique(prop_recid_meas$studyid)>0)

# Separate preferred studygroups
prop_recid_prefstudygroups<- subset(prop_recid_bound, subset = studygroups %in% "combined groups")
prop_recid_unprefstudygroups<- subset(prop_recid_bound, !(prop_recid_bound$studygroups== "combined groups")) 
prop_recid_unprefstudygroups<- prop_recid_unprefstudygroups[!(prop_recid_unprefstudygroups$studyid%in%prop_recid_prefstudygroups$studyid),] 
prop_recid_bound<-bind_rows(prop_recid_prefstudygroups,prop_recid_unprefstudygroups)

sum(unique(prop_recid_bound$studyid)>0)==sum(unique(prop_recid_meas$studyid)>0)

# Drop rows that do not report the follow-up period or the samples do not have approximately the same follow-up time 
prop_recid_pref_fu <- subset(prop_recid_bound, !(is.na(prop_recid_bound$pt_period_months))) 
prop_recid_unpref_fu <- subset(prop_recid_bound, is.na(prop_recid_bound$pt_period_months))

prop_recid_unpref_fu <- filter(prop_recid_unpref_fu, prop_recid_unpref_fu$pt_period_days_avg_cont - 5 <= prop_recid_unpref_fu$pt_period_days_avg_treat & 
                                                      prop_recid_unpref_fu$pt_period_days_avg_cont + 5 >= prop_recid_unpref_fu$pt_period_days_avg_treat)

prop_recid_unpref_fu<- prop_recid_unpref_fu[!(prop_recid_unpref_fu$studyid%in%prop_recid_pref_fu$studyid),] 
prop_recid_bound<-bind_rows(prop_recid_unpref_fu, prop_recid_pref_fu)

sum(unique(prop_recid_bound$studyid)>0)==sum(unique(prop_recid_meas$studyid)>0) 
# note that several study IDs  have now been dropped and this code will no longer produce 'TRUE'. Instead, monitor the number of unique study IDs from here. 
sum(unique(prop_recid_bound$studyid)>0)

# Separate rows with longer follow-up periods: 48 months
prop_recid_prefpt_period<- subset(prop_recid_bound, (prop_recid_bound$pt_period_months == "48" | is.na(prop_recid_bound$pt_period_months))) 
prop_recid_unprefpt_period<- subset(prop_recid_bound, !(prop_recid_bound$pt_period_months == "48"| is.na(prop_recid_bound$pt_period_months))) 
prop_recid_unprefpt_period<- prop_recid_unprefpt_period[!(prop_recid_unprefpt_period$studyid%in%prop_recid_prefpt_period$studyid),] 
prop_recid_bound<-bind_rows(prop_recid_prefpt_period,prop_recid_unprefpt_period)

# Separate rows with longer follow-up periods: 12 months
prop_recid_prefpt_period<- subset(prop_recid_bound, (prop_recid_bound$pt_period_months == "12"| is.na(prop_recid_bound$pt_period_months))) 
prop_recid_unprefpt_period<- subset(prop_recid_bound, !(prop_recid_bound$pt_period_months == "12"| is.na(prop_recid_bound$pt_period_months))) 
prop_recid_unprefpt_period<- prop_recid_unprefpt_period[!(prop_recid_unprefpt_period$studyid%in%prop_recid_prefpt_period$studyid),] 
prop_recid_bound<-bind_rows(prop_recid_prefpt_period,prop_recid_unprefpt_period)

sum(unique(prop_recid_bound$studyid)>0)

# Separate preferred study IDs.
# Note: Study IDs 16a and 16b report the same outcome, moderator, and methodological information. For this reason Study ID 16b will be dropped here. 
prop_recid_bound_final <- subset(prop_recid_bound, !(prop_recid_bound$studyid == "16b" | prop_recid_bound$studyid == "9b")) 
target <- c("2", "3", "6", "8", "9a", "10", "11", "12", "13", "14", "15", "16a") #reset order of rows
prop_recid_bound_final <- prop_recid_bound_final[match(target, prop_recid_bound_final$studyid),] 

# Separate rows with complex data (they will be rejoined prior to the analysis)
prop_recid_analysis_complex <- subset(prop_recid_bound_final, is.na(prop_recid_bound_final$pt_treat_Dichotomous_.))
prop_recid_bound_final <- subset(prop_recid_bound_final, !(is.na(prop_recid_bound_final$pt_treat_Dichotomous_.)))
########################################
# 3.2: Import and transform complex data 
#####################################
AUSNZ_complex_data <- read.csv(url("https://raw.githubusercontent.com/mtrood/effects-of-judicial-supervision-on-recidivisim-of-offenders-in-Australia-and-New-Zealand/main/AUSNZ%20JSMA%20complex%20data.csv"))
prop_recid_complex_data_wrangle <- AUSNZ_complex_data

# Convert appropriate variables to numeric 
prop_recid_complex_data_wrangle$Coefficient.estimate <- prop_recid_complex_data_wrangle %>% select("Coefficient.estimate") %>% 
                                                              apply(2,function(x) as.numeric(as.character(x))) 

# Separate all those studies that included a dichotomous measure of recidivism
prop_recid_complex <- prop_recid_complex_data_wrangle %>% subset(., includes_complete_dichotomous== "Yes")

# Separate the measures related to proportion that recidivated
prop_recid_complex <- prop_recid_complex %>% subset(., measure== "% convicted (regressed)" | measure== "% offence episodes (regressed)"| measure== "% rearrested") 

# Separate the preferred outcomes 
prop_recid_complex_prefout <- subset(prop_recid_complex, subset = outcome %in% "offence") 
prop_recid_complex_unprefout <- subset(prop_recid_complex, !(prop_recid_complex$outcome== "offence")) 

prop_recid_complex_unprefout <- prop_recid_complex_unprefout[!(prop_recid_complex_unprefout$studyid%in%prop_recid_complex_prefout$studyid),] 
prop_recid_complex_bound<-bind_rows(prop_recid_complex_prefout,prop_recid_complex_unprefout) 

sum(unique(prop_recid_complex_bound$studyid)>0)==sum(unique(prop_recid_complex$studyid)>0) 

# Separate preferred coefficients or estimate: recursive probit function
prop_recid_complex_probit <- subset(prop_recid_complex_bound, (prop_recid_complex_bound$technique== "recursive bivariate probit model that employs a maximum likelihood method to estimate consistent treatment effect estimates")) 

# Transform recursive probit function coefficient to logit following the rescaling factor (1.6) described by Amemiya (1981, p. 1488). Insert as variable in prop_recid_bound_complex
prop_recid_complex_probit$yi <- prop_recid_complex_probit$Coefficient.estimate*1.6

# Rescale the standard error using the same rescaling factor. Borenstein et al., (2009, p.37) note that the standard error for the LogOddsratio is obtained by taking the square root
# of the variance. Reverse this calculation on the standard error to obtain the variance. 
prop_recid_complex_probit$vi <- (prop_recid_complex_probit$SE*1.6)^2 

# Add a variable each for the standard error, z-score, and confidence interval on the z-score following Borenstein et al.'s (2009, p. 52) formulae 
prop_recid_complex_probit$sei <- prop_recid_complex_probit$SE*1.6 
prop_recid_complex_probit$zi <- prop_recid_complex_probit$yi / prop_recid_complex_probit$sei 
prop_recid_complex_probit$ci.lb <- prop_recid_complex_probit$yi-1.96*prop_recid_complex_probit$sei
prop_recid_complex_probit$ci.ub <- prop_recid_complex_probit$yi+1.96*prop_recid_complex_probit$sei

# Separate the preferred intention-to-treat
# note: firstly make an object for the graduates that will be used later. 
prop_recid_grads_complex <- subset(prop_recid_complex_probit, (prop_recid_complex_probit$ITT=="Graduates"))

prop_recid_complex_prefITT <- subset(prop_recid_complex_probit, (prop_recid_complex_probit$ITT== "Graduates/Failures")) 
prop_recid_complex_unprefITT <- subset(prop_recid_complex_probit, !(prop_recid_complex_probit$ITT== "Graduates/Failures")) 

prop_recid_complex_unprefITT <- prop_recid_complex_unprefITT[!(prop_recid_complex_unprefITT$studyid%in%prop_recid_complex_prefITT$studyid),] 
prop_recid_complex_probit_bound <- bind_rows(prop_recid_complex_prefITT,prop_recid_complex_unprefITT) 

prop_recid_analysis_complex <- cbind(prop_recid_analysis_complex, prop_recid_complex_probit_bound[, c("yi", "vi", "sei", "zi", "ci.lb", "ci.ub")])

########################################
# 3.3: Meta-analysis of proportion that recidivated outcomes  
#####################################
# Calculate the effect size and variance for the 'proportion variables' 
prop_recid_analysis_incompl <- summary.escalc(escalc(measure="OR", ai=pt_treat_prop_recid_yes, bi=pt_treat_prop_recid_no, n1i=pt_treat_Dichotomous_n, 
                                     ci=pt_cont_prop_recid_yes, di=pt_cont_prop_recid_no, n2i=pt_cont_Dichotomous_n, data=prop_recid_bound_final))

# Merge the analysis dataframe with the complex data dataframe
prop_recid_analysis <- rbind.fill(prop_recid_analysis_incompl, prop_recid_analysis_complex)

# Reorder the rows by effect size
prop_recid_analysis <- prop_recid_analysis[order(prop_recid_analysis$yi),]

# For supplementary materials only: reorder and export
# prop_recid_analysis <- prop_recid_analysis[order(prop_recid_analysis$year),] %>% select(., -studyid)
# write.csv(prop_recid_analysis, file = "Odds of recidivism outcome descriptives.csv")


# Random-effects meta-analysis (with restricted maximum-likelihood estimator)
prop_recid_analysis_re <- rma(yi=yi, vi=vi, method="REML", data = prop_recid_analysis, verbose=TRUE, control=list(stepadj=0.5))
summary(prop_recid_analysis_re)

# Create a vector with the row name, pooled odds ratio, confidence interval, z statistic, z p-value, credibility interval, Q statistic, and I^2
prop_pooled_effect_re <- c('Random-effects model', exp(prop_recid_analysis_re$b), exp(prop_recid_analysis_re$ci.lb), exp(prop_recid_analysis_re$ci.ub), prop_recid_analysis_re$zval, 
                           prop_recid_analysis_re$pval, exp(predict(prop_recid_analysis_re)$cr.lb), exp(predict(prop_recid_analysis_re)$cr.ub), prop_recid_analysis_re$QE, prop_recid_analysis_re$QEp, prop_recid_analysis_re$I2)

# Add the names for each column
names(prop_pooled_effect_re) <- c("Model", "Odds ratio", "ConL","ConU", "Z statistic", "Z p-value","CrL", "CrU", "Q","Q p-value", "I^2")
prop_pooled_effect_output <- prop_pooled_effect_re
########################################
# 3.4: Relative recidivism rate
#####################################
# Calculate the percentage change in recidivism relative to a base rate of 50% using Wilson et al.'s (2006, p. 481) formula: 
# pa = 1 - o(pb)/ (1 + o(pb)-pb)
# where: pa is the recidivism rate in the treatment group, pb is the recidivism rate in the control group, and o is the odds ratio.
# Note: We will be making this calculation with odds ratio that is less than 1. We therefore do not need to minus the result from 1. 
percentage_rate <- (exp(prop_recid_analysis_re$b)*.50)/ (1 + exp(prop_recid_analysis_re$b)*.50-.50)
percentage_rate

50-percentage_rate*100
########################################
# 3.5: Forest plot: Proportion that recidivated 
#####################################
# Create objects for the required pieces of data and round them to 2 decimal places for the forestplot structure
prop_fp.OR <- round2(exp(prop_recid_analysis$yi), 2)
prop_fp.lower.CI <- round2(exp(prop_recid_analysis$ci.lb), 2)
prop_fp.upper.CI <- round2(exp(prop_recid_analysis$ci.ub), 2)
prop_fp.pooled_OR <-  round2(exp(prop_recid_analysis_re$b), 2)
prop_fp.pooled_OR_CI_low <- round2(exp(prop_recid_analysis_re$ci.lb), 2)
prop_fp.pooled_OR_CI_upp <- round2(exp(prop_recid_analysis_re$ci.ub), 2)
prop_fp.z.p.value <- format.pval(2 * pnorm(abs(prop_recid_analysis$zi), lower.tail = FALSE), eps = .001, 2)
prop_fp.pooled_OR_z_p_value <- sprintf("%.3f",prop_recid_analysis_re$pval)
prop_fp.pooled_OR_Q <- round2(prop_recid_analysis_re$QE, 2)
prop_fp.pooled_OR_Q_p <- round2(prop_recid_analysis_re$QEp, 2)
prop_fp.pooled_OR_I2 <- round2(prop_recid_analysis_re$I2, 2)



###Create the text objects to be displayed (where different from those above). 
prop_fp.txt.OR <- format(prop_fp.OR, nsmall = 2)
prop_fp.txt.lower.CI <- paste0(format(prop_fp.lower.CI, nsmall = 2), ",")
prop_fp.txt.CI <-  paste0("[", (paste(prop_fp.txt.lower.CI, format(prop_fp.upper.CI, nsmall = 2))), "]")
prop_fp.txt.pooled_OR_CI_low <- paste0(round2(prop_fp.pooled_OR_CI_low, 2), ",")
prop_fp.txt.pooled.CI <-  paste0("[", (paste(prop_fp.txt.pooled_OR_CI_low , round2(prop_fp.pooled_OR_CI_upp, 2))), "]")

###Amend the year and author observations
prop_fp_authors <- paste0(prop_recid_analysis$authors,",")
prop_fp_author.year <- paste(prop_fp_authors, paste0("(", prop_recid_analysis$year, ")"))

### Create Forest Plot Structure and Text
prop_fp_structure <- 
    structure(list(
    mean  = c(NA, prop_fp.OR, prop_fp.pooled_OR), 
    lower = c(NA, prop_fp.lower.CI, prop_fp.pooled_OR_CI_low),
    upper = c(NA, prop_fp.upper.CI, prop_fp.pooled_OR_CI_upp)),
    .Names = c("OR", "lower", "upper"), 
    row.names = c(NA, -11L), 
    class = "data.frame")

prop_fp_tabletext<-cbind(
  c("Study", prop_fp_author.year, "Summary"),
  c("OR", prop_fp.txt.OR, sprintf("%.2f", (prop_fp.pooled_OR))),
  c("95% CI", prop_fp.txt.CI, prop_fp.txt.pooled.CI),
  c("p-Value", prop_fp.z.p.value, prop_fp.pooled_OR_z_p_value))

forestplot(prop_fp_tabletext,
           hrzl_lines = list("2" = gpar(lwd=1, col = "#000044"), 
                             "14" = gpar(lwd=1, col = "#000044"),
                             "15" = gpar(lwd=2, col = "#000044")),
           graph.pos = 4,
           graphwidth = unit(5, "cm"),
           xticks = c(0.50, 1.0, 2.0, 4.0), 
           txt_gp = fpTxtGp(label = gpar(fontfamily = "", cex=.6), ticks = gpar(cex=.6)),
           prop_fp_structure,new_page = TRUE,
           is.summary=c(TRUE, rep(FALSE,12),TRUE), 
           xlog=TRUE,
           col=fpColors(box="royalblue",line="darkblue", summary="royalblue", hrz_lines = "#444444"),
           colgap = unit(.02,"npc"),
           align = c("l", "c", "c", "r"),
           ci.vertices = TRUE)

x <- unit(0.05, 'npc')
y <- unit(.043, 'npc')
# g <- grid.locator('npc') to select on the plot view. replace x and y with g$x and g$y in grid.text below
if (prop_fp.pooled_OR_Q_p < .05){grid.text(substitute(paste('Model: Q = ', a, ', p<.05, ',b, ' = ', c ), list(a=prop_fp.pooled_OR_Q, b= ~ I^{2}, c=prop_fp.pooled_OR_I2)), # add the model fit stats to the forest plot
                                           x, y, just = "left", gp = gpar(fontsize=7, font = 1))}
if (prop_fp.pooled_OR_Q_p >= .05){grid.text(substitute(paste('Model: Q = ', a, ', p = ',b, ', ',c, ' = ', d ), list(a=prop_fp.pooled_OR_Q, b=prop_fp.pooled_OR_Q_p, c= ~ I^{2}, d=prop_fp.pooled_OR_I2)), #this line if Q_p >= .05
                                            x, y, just = "left", gp = gpar(fontsize=7, font = 1))}

########################################
# 3.6: Sensitivity analysis
########################################
########################################
# 3.6.1: Leave-One-Out 
#####################################
leave1out(prop_recid_analysis_re, transf=exp)
########################################
# 3.6.2: Alternative probit scaling values
#####################################
# Create a function that rescales the Study ID 3's probit coefficient and standard error with values between 1.6 and 1.8 to assess the effect on the overall pooled effect
iterative_rescale <- function(w, x, vector_yi, vector_vi){
  coef_vector <-  seq(from=1.6, to=1.8, by=0.01)*w # create a vector for the scaling values
  variance_vector <-  (seq(from=1.6, to=1.8, by=0.01)*x)^2
  datalist = list() # create the list that loops the other studies' yis and vis
  for (i in 1:20) {
    yivi_short <- data.frame(yi = vector_yi, vi = vector_vi)
    datalist[[i]] <- yivi_short 
  }
  yi_vi_df = do.call(cbind, datalist)
  yi_vi_df <- rbind(yi_vi_df, c(rbind(coef_vector,variance_vector))) # bind the rescaled values to the looped dataframe so that the recsaled yi and vi values alternate
  yi_vi_df <- data.frame(lapply(yi_vi_df, function(x) unlist(x))) # unlist dataframe
  
  column_combinations <-cbind(rep(seq(1,ncol(yi_vi_df),by=2), 20), sort(rep(seq(1,ncol(yi_vi_df),by=2), 20)+1)) #make a list of odd and even columns. This will be used to tell 'apply' which columns to use to achieve the desired output 
  models_list <- apply(column_combinations, 1,
                       function(y){
                         rma(yi = yi_vi_df[, y[1]], vi = yi_vi_df[, y[2]], method="REML", data = yi_vi_df, control=list(stepadj=0.5))
    }
  )
  summary_data <- lapply(models_list, `[`, c('b', 'ci.lb', 'ci.ub', 'zval', 'pval', 'QE', 'QEp','I2'))
  summary_df <- data.frame(matrix(unlist(summary_data), nrow=length(summary_data), byrow=T))
  colnames(summary_df) <- c("Odds_ratio", "ConL","ConU", "Z statistic", "Z p-value","Q","Q p-value", "I^2")
  summary_df[c(1:3)] <- lapply(summary_df [c(1:3)], exp)
  return(summary_df)
}

prop_recid_analysis_re_iteratively_rescaled  <- iterative_rescale(prop_recid_complex_probit$Coefficient.estimate[1], prop_recid_complex_probit$SE[1], 
                                                          prop_recid_analysis_incompl$yi, prop_recid_analysis_incompl$vi)
prop_recid_analysis_re_iteratively_rescaled 

# Call the row with the odds ratio closet to 0 (smallest pooled effect)
prop_recid_analysis_re_iteratively_rescaled[which.max(prop_recid_analysis_re_iteratively_rescaled$Odds_ratio), ]
# Call the row with the highest Z p-value
prop_recid_analysis_re_iteratively_rescaled[which.max(prop_recid_analysis_re_iteratively_rescaled$`Z p-value`), ]
# Logical call whether there are any I^2 values less than 45 or great than 45.7
any(prop_recid_analysis_re_iteratively_rescaled$`I^2` < 45, prop_recid_analysis_re_iteratively_rescaled$`I^2` > 45.7)
########################################
# 3.6.3: Alternative models
########################################
########################################
# 3.6.3.1: Fixed effect model 
#####################################
# Model the data using a fixed effects model
prop_recid_analysis_fe <- rma(yi=yi, vi=vi, method="FE", data = prop_recid_analysis)
summary(prop_recid_analysis_fe)

# Save the estimate, q statistic, DFs, and variance into respective objects
prop_fe_PLOR <- prop_recid_analysis_fe$b
prop_fe_PLOR_v <- 1/sum(1/prop_recid_analysis$vi)
prop_fe_Q <- prop_recid_analysis_fe$QE
prop_fe_df <- length(prop_recid_analysis_fe$yi)-1

# Estimate of tau-squared: T.sq (estimate of the variance of the true effect size)
# Borenstein et al. (2011, p. 114) note that T.sq is calculated as follows:
# T.sq = (Q-df)/C 
#   Where C = the sum of the study weights minus the sum of each study weight squared and divided by 
#   sum of the the study weights, or C = sum(1/v)-(sum((1/v)^2)/sum(1/v))

#calculate C
prop_fe_C <- sum(1/prop_recid_analysis$vi)-(sum((1/prop_recid_analysis$vi)^2)/sum(1/prop_recid_analysis$vi))

#calculate T.sq
prop_fe_T.sq <- (prop_fe_Q -prop_fe_df)/prop_fe_C
prop_fe_T.sq

###T (our estimate of the true standard deviation) is simply the square root of T.sq (Borenstein, et al, 2011, p.116)
prop_fe_T <- sqrt(prop_fe_T.sq)
prop_fe_T

# Credibility Interval 
# Borenstein et al. (2011, p. 129) cite Higgins et al.'s (year of study not reported) formulae for
# calculating the credibility interval
#    LLpred = M* - tdf * (sqrt(T.sq + Vm*))
#    ULpred = M* + tdf * (sqrt(T.sq + Vm*))
#      where M* = mean effect size in the sample, tdf is the t-value corresponding to the desired alpha level
#      with the relevant degrees of freedom, and Vm* is the variance of M*

#Note we first need to calculate tdf. We can do this with the qt function as follows
# qt(1-alpha/2, k-2)
prop_fe_tdf <- qt(1-0.05/2, (length(prop_recid_analysis$yi)-2))
prop_fe_tdf

prop_fe_LLcred <- prop_fe_PLOR - prop_fe_tdf*(sqrt(prop_fe_T.sq + prop_fe_PLOR_v))
prop_fe_ULcred <- prop_fe_PLOR + prop_fe_tdf*(sqrt(prop_fe_T.sq + prop_fe_PLOR_v))
prop_fe_CrI <- c(exp(prop_fe_LLcred), exp(prop_fe_ULcred))
prop_fe_CrI

# Note: the prediction interval on the the fixed effect model will not be reported given that we assume that the true effect is the same in all studies. We can therefore
#       use the model's confidence interval as its measure of precision (see Borenstein et al., 2009, p.127)

# Create a vector with the row name, pooled odds ratio, confidence interval, z statistic, z p-value, credibility interval, Q statistic, and I^2
prop_pooled_effect_fe <- c('Fixed effect model', exp(prop_fe_PLOR), exp(prop_recid_analysis_fe$ci.lb), exp(prop_recid_analysis_fe$ci.ub), prop_recid_analysis_fe$zval, 
                                          prop_recid_analysis_fe$pval, NA, NA, prop_fe_Q, prop_recid_analysis_fe$QEp, prop_recid_analysis_fe$I2)

# Add the names for each column
names(prop_pooled_effect_fe) <- c("Model", "Odds ratio", "ConL","ConU", "Z statistic", "Z p-value", "CrL", "CrU", "Q","Q p-value", "I^2")
prop_pooled_effect_output <-bind_rows(prop_pooled_effect_output, prop_pooled_effect_fe)

########################################
# 3.6.3.2: Intention-to-treat: graduates only
#####################################
# Follow section 3.1 and separate the target rows but select only outcomes that assessed the intervention's graduates. 
# Drop rows that do not report the follow-up period and do not have approximately the same follow-up time 
prop_recid_grads_pref_fu <- subset(prop_recid_grads, !(is.na(prop_recid_grads$pt_period_months))) 
prop_recid_grads_unpref_fu <- subset(prop_recid_grads, is.na(prop_recid_grads$pt_period_months))

prop_recid_grads_unpref_fu <- filter(prop_recid_grads_unpref_fu, prop_recid_grads_unpref_fu$pt_period_days_avg_cont - 5 <= prop_recid_grads_unpref_fu$pt_period_days_avg_treat & 
                                 prop_recid_grads_unpref_fu$pt_period_days_avg_cont + 5 >= prop_recid_grads_unpref_fu$pt_period_days_avg_treat)

prop_recid_grads_unpref_fu<- prop_recid_grads_unpref_fu[!(prop_recid_grads_unpref_fu$studyid%in%prop_recid_grads_pref_fu$studyid),] 
prop_recid_grads<-bind_rows(prop_recid_grads_unpref_fu, prop_recid_grads_pref_fu)

sum(unique(prop_recid_grads$studyid)>0)

# Drop study id 9b
prop_recid_grads <- subset(prop_recid_grads, !(prop_recid_grads$studyid == '9b')) 

# Separate preferred studygroups alternative giving preference to partially matched groups 
prop_recid_grads_prefstudygroups_alt<- subset(prop_recid_grads, subset = studygroups_alternative %in% "partially matched")
prop_recid_grads_unprefstudygroups_alt<- subset(prop_recid_grads, !(prop_recid_grads$studygroups_alternative== "partially matched")) 
prop_recid_grads_unprefstudygroups_alt<- prop_recid_grads_unprefstudygroups_alt[!(prop_recid_grads_unprefstudygroups_alt$studyid%in%prop_recid_grads_prefstudygroups_alt$studyid),] 
prop_recid_grads_bound<-bind_rows(prop_recid_grads_prefstudygroups_alt, prop_recid_grads_unprefstudygroups_alt)

sum(unique(prop_recid_grads_bound$studyid)>0)

# Separate rows with longer follow-up periods: 12 months
prop_recid_grads_prefpt_period <- subset(prop_recid_grads_bound, (prop_recid_grads_bound$pt_period_months == "12")) 
prop_recid_grads_unprefpt_period<- subset(prop_recid_grads_bound, !(prop_recid_grads_bound$pt_period_months == "12")) 
prop_recid_grads_unprefpt_period<- prop_recid_grads_unprefpt_period[!(prop_recid_grads_unprefpt_period$studyid%in%prop_recid_grads_prefpt_period$studyid),] 
prop_recid_grads_bound <- bind_rows(prop_recid_grads_prefpt_period,prop_recid_grads_unprefpt_period)

# Separate studies with complex data 
prop_recid_grads_complex_only <- subset(prop_recid_grads_bound, is.na(prop_recid_grads_bound$pt_treat_Dichotomous_.))
prop_recid_grads_bound_final <- subset(prop_recid_grads_bound, !(is.na(prop_recid_grads_bound$pt_treat_Dichotomous_.)))

# Add the adjusted variables to the complex only dataframe
prop_recid_grads_complex_only <- cbind(prop_recid_grads_complex_only, prop_recid_grads_complex[, c("yi", "vi", "sei", "zi", "ci.lb", "ci.ub")])

# Calculate the effect size and variance for the 'proportion variables' 
prop_recid_grads_analysis <- summary.escalc(escalc(measure="OR", ai=pt_treat_prop_recid_yes, bi=pt_treat_prop_recid_no, n1i=pt_treat_Dichotomous_n, 
                                             ci=pt_cont_prop_recid_yes, di=pt_cont_prop_recid_no, n2i=pt_cont_Dichotomous_n, data=prop_recid_grads_bound_final))

# Merge the analysis dataframe with the complex data dataframe
prop_recid_grads_analysis <- rbind.fill(prop_recid_grads_analysis, prop_recid_grads_complex_only)

# Reorder the rows by effect size
prop_recid_grads_analysis <- prop_recid_grads_analysis[order(prop_recid_grads_analysis$yi),]

# Random-effects meta-analysis (with restricted maximum-likelihood estimator)
prop_recid_grads_analysis_re <- rma(yi=yi, vi=vi, method="REML", data = prop_recid_grads_analysis)
summary(prop_recid_grads_analysis_re)

# Create a vector with the row name, pooled odds ratio, confidence interval, z statistic, z p-value, credibility interval, Q statistic, and I^2
prop_pooled_effect_re <- c('Random-effects model (Graduates only)', exp(prop_recid_grads_analysis_re$b), exp(prop_recid_grads_analysis_re$ci.lb), exp(prop_recid_grads_analysis_re$ci.ub), prop_recid_grads_analysis_re$zval, 
                           prop_recid_grads_analysis_re$pval, exp(predict(prop_recid_grads_analysis_re)$cr.lb), exp(predict(prop_recid_grads_analysis_re)$cr.ub), prop_recid_grads_analysis_re$QE, prop_recid_grads_analysis_re$QEp, prop_recid_grads_analysis_re$I2)

# Add the names for each column
names(prop_pooled_effect_re) <- c("Model", "Odds ratio", "ConL","ConU", "Z statistic", "Z p-value","CrL", "CrU", "Q","Q p-value", "I^2")
prop_pooled_effect_output <- bind_rows(prop_pooled_effect_output, prop_pooled_effect_re)
########################################
# 3.7: Publication bias
#####################################
# Note: publication bias will be assessed using functions from the metafor package (Viectbauer, 2010) 

# Trim and fill procedure
# Run metafor's trimfill on the rma object
prop_recid_re_trimfill_model <- trimfill(prop_recid_analysis_re, control=list(stepadj=0.5, maxiter=1000)) #note that we have decreased the step length and increased the number of iterations so that the Fisher scoring algorithm converges 

# Funnel plot including the estimated missing values
dev.new(width=22, height=22)

funnel(prop_recid_re_trimfill_model, legend = TRUE, atransf=exp, xlab = 'RIRR')

# Rosenthal's Fail Safe N
prop_recid_re_rosenthal_fsn<- fsn(yi = prop_recid_analysis$yi, vi = prop_recid_analysis$vi)
prop_recid_re_rosenthal_fsn

# Orwin's Fail Safe N 
prop_recid_re_orwin_fsn <- fsn(yi = prop_recid_analysis$yi, vi = prop_recid_analysis$vi, type = "Orwin", target = 0.05)
prop_recid_re_orwin_fsn

# Egger regression
prop_recid_re_egger_test <- regtest.rma(prop_recid_analysis_re, predictor="sei")
prop_recid_re_egger_test
########################################
# 4: Moderator analysis: proportion who recidivated
########################################
########################################
# 4.1: Year of publication
#####################################
# Insert the moderator into the model
mod_prop_recid_year <- rma(yi=yi, vi=vi, method="REML", mods = ~ year, data = prop_recid_analysis, verbose=TRUE, control=list(stepadj=0.5))

# Create an object for the desired information and create a 'moderator_output' data frame 
mod_prop_year_list<- c('Year of publication', 'methodological', 'Beta_weight', mod_prop_recid_year$k, mod_prop_recid_year$beta[2, 1], mod_prop_recid_year$se[2], mod_prop_recid_year$zval[2], mod_prop_recid_year$pval[2], 
                    mod_prop_recid_year$ci.lb[2], mod_prop_recid_year$ci.ub[2], mod_prop_recid_year$k- length(mod_prop_recid_year$b), mod_prop_recid_year$QE, mod_prop_recid_year$QEp, 
                    mod_prop_recid_year$I2)

# name the object's columns 
names(mod_prop_year_list) <- c("name", "mod_category", "level", "level_k", "est_LOR", "est_SE","est_z","est_z_p","est_cl","est_cu", "res_DF", "Qres", "Qres_p", 
                          "I2")
prop_moderator_output <- mod_prop_year_list
########################################
# 4.2: Jurisdiction
#####################################
table(prop_recid_analysis$jurisdiction, prop_recid_analysis$studyid)
########################################
# 4.2.1: Jurisdiction - New Zealand
#####################################
# Select the subgroup in the model 
mod_prop_recid_jurisdiction_nz <- rma(yi=yi, vi=vi, method="REML", subset=(jurisdiction== "NZ"), data = prop_recid_analysis)

# Create an object for the desired information  
mod_prop_jurisdiction_nz_list <- c('Jurisdiction', 'programmatic', 'New Zealand (ref)', mod_prop_recid_jurisdiction_nz$k, mod_prop_recid_jurisdiction_nz$beta[1], mod_prop_recid_jurisdiction_nz$se[1], mod_prop_recid_jurisdiction_nz$zval[1], mod_prop_recid_jurisdiction_nz$pval[1], 
                    mod_prop_recid_jurisdiction_nz$ci.lb[1], mod_prop_recid_jurisdiction_nz$ci.ub[1], mod_prop_recid_jurisdiction_nz$k- length(mod_prop_recid_jurisdiction_nz$b), mod_prop_recid_jurisdiction_nz$QE, mod_prop_recid_jurisdiction_nz$QEp, 
                    mod_prop_recid_jurisdiction_nz$I2)
# name the object's columns and bind to prop_moderator_output
names(mod_prop_jurisdiction_nz_list) <- c("name", "mod_category", "level", "level_k", "est_LOR", "est_SE","est_z","est_z_p","est_cl","est_cu", "res_DF", "Qres", "Qres_p", 
                                 "I2")
prop_moderator_output <- bind_rows(prop_moderator_output, mod_prop_jurisdiction_nz_list)

########################################
# 4.2.2: Jurisdiction - Victoria 
#####################################
# Select the subgroup in the model 
mod_prop_recid_jurisdiction_vic <- rma(yi=yi, vi=vi, method="REML", subset=(jurisdiction== "VIC"), data = prop_recid_analysis)

# Create an object for the desired information  
mod_prop_jurisdiction_vic_list <- c('Jurisdiction', 'programmatic', 'Victoria', mod_prop_recid_jurisdiction_vic$k, mod_prop_recid_jurisdiction_vic$beta[1], mod_prop_recid_jurisdiction_vic$se[1], mod_prop_recid_jurisdiction_vic$zval[1], mod_prop_recid_jurisdiction_vic$pval[1], 
                                   mod_prop_recid_jurisdiction_vic$ci.lb[1], mod_prop_recid_jurisdiction_vic$ci.ub[1], mod_prop_recid_jurisdiction_vic$k- length(mod_prop_recid_jurisdiction_vic$b), mod_prop_recid_jurisdiction_vic$QE, mod_prop_recid_jurisdiction_vic$QEp, 
                                   mod_prop_recid_jurisdiction_vic$I2)
# name the object's columns and bind to prop_moderator_output
names(mod_prop_jurisdiction_vic_list) <- c("name", "mod_category", "level", "level_k", "est_LOR", "est_SE","est_z","est_z_p","est_cl","est_cu", "res_DF", "Qres", "Qres_p", "I2")
prop_moderator_output <- bind_rows(prop_moderator_output, mod_prop_jurisdiction_vic_list)
########################################
# 4.2.3: Jurisdiction - Queensland
#####################################
# Select the subgroup in the model 
mod_prop_recid_jurisdiction_qld <- rma(yi=yi, vi=vi, method="REML", subset=(jurisdiction== "QLD"), data = prop_recid_analysis)

# Create an object for the desired information  
mod_prop_jurisdiction_qld_list <- c('Jurisdiction', 'programmatic', 'Queensland', mod_prop_recid_jurisdiction_qld$k, mod_prop_recid_jurisdiction_qld$beta[1], mod_prop_recid_jurisdiction_qld$se[1], mod_prop_recid_jurisdiction_qld$zval[1], mod_prop_recid_jurisdiction_qld$pval[1], 
                                    mod_prop_recid_jurisdiction_qld$ci.lb[1], mod_prop_recid_jurisdiction_qld$ci.ub[1], mod_prop_recid_jurisdiction_qld$k- length(mod_prop_recid_jurisdiction_qld$b), mod_prop_recid_jurisdiction_qld$QE, mod_prop_recid_jurisdiction_qld$QEp, 
                                    mod_prop_recid_jurisdiction_qld$I2)
# name the object's columns and bind to prop_moderator_output
names(mod_prop_jurisdiction_qld_list) <- c("name", "mod_category", "level", "level_k", "est_LOR", "est_SE","est_z","est_z_p","est_cl","est_cu", "res_DF", "Qres", "Qres_p", "I2")
prop_moderator_output <- bind_rows(prop_moderator_output, mod_prop_jurisdiction_qld_list)
########################################
# 4.2.4: Jurisdiction - Australia 
#####################################
# Select the subgroup in the model 
mod_prop_recid_jurisdiction_aus <- rma(yi=yi, vi=vi, method="REML", subset= !(jurisdiction== "NZ"), data = prop_recid_analysis)

# Create an object for the desired information  
mod_prop_jurisdiction_aus_list <- c('Jurisdiction', 'programmatic', 'Australia', mod_prop_recid_jurisdiction_aus$k, mod_prop_recid_jurisdiction_aus$beta[1], mod_prop_recid_jurisdiction_aus$se[1], mod_prop_recid_jurisdiction_aus$zval[1], mod_prop_recid_jurisdiction_aus$pval[1], 
                                    mod_prop_recid_jurisdiction_aus$ci.lb[1], mod_prop_recid_jurisdiction_aus$ci.ub[1], mod_prop_recid_jurisdiction_aus$k- length(mod_prop_recid_jurisdiction_aus$b), mod_prop_recid_jurisdiction_aus$QE, mod_prop_recid_jurisdiction_aus$QEp, 
                                    mod_prop_recid_jurisdiction_aus$I2)
# name the object's columns and bind to prop_moderator_output
names(mod_prop_jurisdiction_aus_list) <- c("name", "mod_category", "level", "level_k", "est_LOR", "est_SE","est_z","est_z_p","est_cl","est_cu", "res_DF", "Qres", "Qres_p","I2")
prop_moderator_output <- bind_rows(prop_moderator_output, mod_prop_jurisdiction_aus_list)
########################################
# 4.3: Outcome  
#####################################
# Note: outcome will not be included in the proportion who recidivated moderator/ subgroup analysis because differences in outcome between studies do not reflect differences in the measure used and are thereby misleading.
#       For this reason, only 'measure' will be included in the moderator/ subgroup analysis, and not 'outcome'. For example, Study ID 10's measure is 'offences' while the measure is '% convicted'. 
########################################
# 4.4: Measure  
#####################################
table(prop_recid_analysis$measure, prop_recid_analysis$studyid)
# recode the variable to account for convictions and rearrest
prop_recid_analysis$measure_recode <- recode(prop_recid_analysis$measure, "% charges" = "% new offence", "% new offence episode" = "% new offence", "% convicted" = "% proven offence")
########################################
# 4.4.1: Measure - Offences 
#####################################
# Select the subgroup in the model 
mod_prop_recid_measure_off <- rma(yi=yi, vi=vi, method="REML", subset=(measure_recode== "% new offence"), data = prop_recid_analysis)

# Create an object for the desired information  
mod_prop_measure_off_list <- c('Measure', 'methodological', 'New offence (ref)', mod_prop_recid_measure_off$k, mod_prop_recid_measure_off$beta[1], mod_prop_recid_measure_off$se[1], mod_prop_recid_measure_off$zval[1], mod_prop_recid_measure_off$pval[1], 
                                    mod_prop_recid_measure_off$ci.lb[1], mod_prop_recid_measure_off$ci.ub[1], mod_prop_recid_measure_off$k- length(mod_prop_recid_measure_off$b), mod_prop_recid_measure_off$QE, mod_prop_recid_measure_off$QEp, 
                                    mod_prop_recid_measure_off$I2)
# name the object's columns and bind to prop_moderator_output
names(mod_prop_measure_off_list) <- c("name", "mod_category", "level", "level_k", "est_LOR", "est_SE","est_z","est_z_p","est_cl","est_cu", "res_DF", "Qres", "Qres_p", "I2")
prop_moderator_output <- bind_rows(prop_moderator_output, mod_prop_measure_off_list)
########################################
# 4.4.2: Measure - Proven offences 
#####################################
# Select the subgroup in the model 
mod_prop_recid_measure_prov_off <- rma(yi=yi, vi=vi, method="REML", subset=(measure_recode== "% proven offence"), data = prop_recid_analysis)

# Create an object for the desired information  
mod_prop_measure_prov_off_list <- c('Measure', 'methodological', 'New proven offence', mod_prop_recid_measure_prov_off$k, mod_prop_recid_measure_prov_off$beta[1], mod_prop_recid_measure_prov_off$se[1], mod_prop_recid_measure_prov_off$zval[1], mod_prop_recid_measure_prov_off$pval[1], 
                               mod_prop_recid_measure_prov_off$ci.lb[1], mod_prop_recid_measure_prov_off$ci.ub[1], mod_prop_recid_measure_prov_off$k- length(mod_prop_recid_measure_prov_off$b), mod_prop_recid_measure_prov_off$QE, mod_prop_recid_measure_prov_off$QEp, 
                               mod_prop_recid_measure_prov_off$I2)
# name the object's columns and bind to prop_moderator_output
names(mod_prop_measure_prov_off_list) <- c("name", "mod_category", "level", "level_k", "est_LOR", "est_SE","est_z","est_z_p","est_cl","est_cu", "res_DF", "Qres", "Qres_p", "I2")
prop_moderator_output <- bind_rows(prop_moderator_output, mod_prop_measure_prov_off_list)
########################################
# 4.5: Time status of measure
#####################################
table(prop_recid_analysis$time_status_of_measure, prop_recid_analysis$studyid)
########################################
# 4.5.1: Time status of measure - Elapsed time 
#####################################
# Select the subgroup in the model 
mod_prop_recid_time_stat_elapsed <- rma(yi=yi, vi=vi, method="REML", subset=(time_status_of_measure== "elapsed time"), data = prop_recid_analysis)

# Create an object for the desired information  
mod_prop_time_stat_elapsed_list <- c('Time status of measure', 'methodological', 'Elapsed time (ref)', mod_prop_recid_time_stat_elapsed$k, mod_prop_recid_time_stat_elapsed$beta[1], mod_prop_recid_time_stat_elapsed$se[1], mod_prop_recid_time_stat_elapsed$zval[1], mod_prop_recid_time_stat_elapsed$pval[1], 
                                    mod_prop_recid_time_stat_elapsed$ci.lb[1], mod_prop_recid_time_stat_elapsed$ci.ub[1], mod_prop_recid_time_stat_elapsed$k- length(mod_prop_recid_time_stat_elapsed$b), mod_prop_recid_time_stat_elapsed$QE, mod_prop_recid_time_stat_elapsed$QEp, 
                                    mod_prop_recid_time_stat_elapsed$I2)
# name the object's columns and bind to prop_moderator_output
names(mod_prop_time_stat_elapsed_list) <- c("name", "mod_category", "level", "level_k", "est_LOR", "est_SE","est_z","est_z_p","est_cl","est_cu", "res_DF", "Qres", "Qres_p", "I2")
prop_moderator_output <- bind_rows(prop_moderator_output, mod_prop_time_stat_elapsed_list)
########################################
# 4.5.2: Time status of measure - Free time 
#####################################
# Select the subgroup in the model 
mod_prop_recid_time_stat_free <- rma(yi=yi, vi=vi, method="REML", subset=(time_status_of_measure== "free time"), data = prop_recid_analysis)

# Create an object for the desired information  
mod_prop_time_stat_free_list <- c('Time status of measure', 'methodological', 'Free time', mod_prop_recid_time_stat_free$k, mod_prop_recid_time_stat_free$beta[1], mod_prop_recid_time_stat_free$se[1], mod_prop_recid_time_stat_free$zval[1], mod_prop_recid_time_stat_free$pval[1], 
                                     mod_prop_recid_time_stat_free$ci.lb[1], mod_prop_recid_time_stat_free$ci.ub[1], mod_prop_recid_time_stat_free$k- length(mod_prop_recid_time_stat_free$b), mod_prop_recid_time_stat_free$QE, mod_prop_recid_time_stat_free$QEp, 
                                     mod_prop_recid_time_stat_free$I2)
# name the object's columns and bind to prop_moderator_output
names(mod_prop_time_stat_free_list) <- c("name", "mod_category", "level", "level_k", "est_LOR", "est_SE","est_z","est_z_p","est_cl","est_cu", "res_DF", "Qres", "Qres_p", "I2")
prop_moderator_output <- bind_rows(prop_moderator_output, mod_prop_time_stat_free_list)
########################################
# 4.5.3: Time status of measure - Not reported
#####################################
# Select the subgroup in the model 
mod_prop_recid_time_stat_nr <- rma(yi=yi, vi=vi, method="REML", subset=(time_status_of_measure== "Not reported"), data = prop_recid_analysis)

# Create an object for the desired information  
mod_prop_time_stat_nr_list <- c('Time status of measure', 'methodological', 'Not reported', mod_prop_recid_time_stat_nr$k, mod_prop_recid_time_stat_nr$beta[1], mod_prop_recid_time_stat_nr$se[1], mod_prop_recid_time_stat_nr$zval[1], mod_prop_recid_time_stat_nr$pval[1], 
                                  mod_prop_recid_time_stat_nr$ci.lb[1], mod_prop_recid_time_stat_nr$ci.ub[1], mod_prop_recid_time_stat_nr$k- length(mod_prop_recid_time_stat_nr$b), mod_prop_recid_time_stat_nr$QE, mod_prop_recid_time_stat_nr$QEp, 
                                  mod_prop_recid_time_stat_nr$I2)
# name the object's columns and bind to prop_moderator_output
names(mod_prop_time_stat_nr_list) <- c("name", "mod_category", "level", "level_k", "est_LOR", "est_SE","est_z","est_z_p","est_cl","est_cu", "res_DF", "Qres", "Qres_p", "I2")
prop_moderator_output <- bind_rows(prop_moderator_output, mod_prop_time_stat_nr_list)
########################################
# 4.6: Court setting
#####################################
table(prop_recid_analysis$setting, prop_recid_analysis$studyid)
# Recode 'JDC' as 'DC' and DVC as NA 
prop_recid_analysis$setting_recode <- recode(prop_recid_analysis$setting, "JDC" = "DC")
prop_recid_analysis$setting_recode <- na_if(prop_recid_analysis$setting_recode, "DVC")
########################################
# 4.6.1: Court setting - Community court
#####################################
# Select the subgroup in the model 
mod_prop_recid_setting_cc <- rma(yi=yi, vi=vi, method="REML", subset=(setting_recode== "CC"), data = prop_recid_analysis)

# Create an object for the desired information  
mod_prop_setting_cc_list <- c('Court setting', 'programmatic', 'Community court (ref)', mod_prop_recid_setting_cc$k, mod_prop_recid_setting_cc$beta[1], mod_prop_recid_setting_cc$se[1], mod_prop_recid_setting_cc$zval[1], mod_prop_recid_setting_cc$pval[1], 
                                mod_prop_recid_setting_cc$ci.lb[1], mod_prop_recid_setting_cc$ci.ub[1], mod_prop_recid_setting_cc$k- length(mod_prop_recid_setting_cc$b), mod_prop_recid_setting_cc$QE, mod_prop_recid_setting_cc$QEp, 
                                mod_prop_recid_setting_cc$I2)
# name the object's columns and bind to prop_moderator_output
names(mod_prop_setting_cc_list) <- c("name", "mod_category", "level", "level_k", "est_LOR", "est_SE","est_z","est_z_p","est_cl","est_cu", "res_DF", "Qres", "Qres_p", "I2")
prop_moderator_output <- bind_rows(prop_moderator_output, mod_prop_setting_cc_list)
########################################
# 4.6.2: Court setting - Drug court
#####################################
# Select the subgroup in the model 
mod_prop_recid_setting_dc <- rma(yi=yi, vi=vi, method="REML", subset=(setting_recode== "DC"), data = prop_recid_analysis)

# Create an object for the desired information  
mod_prop_setting_dc_list <- c('Court setting', 'programmatic', 'Drug court', mod_prop_recid_setting_dc$k, mod_prop_recid_setting_dc$beta[1], mod_prop_recid_setting_dc$se[1], mod_prop_recid_setting_dc$zval[1], mod_prop_recid_setting_dc$pval[1], 
                              mod_prop_recid_setting_dc$ci.lb[1], mod_prop_recid_setting_dc$ci.ub[1], mod_prop_recid_setting_dc$k- length(mod_prop_recid_setting_dc$b), mod_prop_recid_setting_dc$QE, mod_prop_recid_setting_dc$QEp, 
                              mod_prop_recid_setting_dc$I2)
# name the object's columns and bind to prop_moderator_output
names(mod_prop_setting_dc_list) <- c("name", "mod_category", "level", "level_k", "est_LOR", "est_SE","est_z","est_z_p","est_cl","est_cu", "res_DF", "Qres", "Qres_p", "I2")
prop_moderator_output <- bind_rows(prop_moderator_output, mod_prop_setting_dc_list)
########################################
# 4.6.3: Court setting - Mainstream court
#####################################
# Select the subgroup in the model 
mod_prop_recid_setting_mc <- rma(yi=yi, vi=vi, method="REML", subset=(setting_recode== "MC"), data = prop_recid_analysis)

# Create an object for the desired information  
mod_prop_setting_mc_list <- c('Court setting', 'programmatic', 'Mainstream court', mod_prop_recid_setting_mc$k, mod_prop_recid_setting_mc$beta[1], mod_prop_recid_setting_mc$se[1], mod_prop_recid_setting_mc$zval[1], mod_prop_recid_setting_mc$pval[1], 
                              mod_prop_recid_setting_mc$ci.lb[1], mod_prop_recid_setting_mc$ci.ub[1], mod_prop_recid_setting_mc$k- length(mod_prop_recid_setting_mc$b), mod_prop_recid_setting_mc$QE, mod_prop_recid_setting_mc$QEp, 
                              mod_prop_recid_setting_mc$I2)
# name the object's columns and bind to prop_moderator_output
names(mod_prop_setting_mc_list) <- c("name", "mod_category", "level", "level_k", "est_LOR", "est_SE","est_z","est_z_p","est_cl","est_cu", "res_DF", "Qres", "Qres_p", "I2")
prop_moderator_output <- bind_rows(prop_moderator_output, mod_prop_setting_mc_list)
########################################
# 4.7: Study groups
#####################################
table(prop_recid_analysis$studygroups, prop_recid_analysis$studyid)
# Note: this variable will be modified as follows: 1) the 'accepted vs referred' group only includes one outcome and will be recoded as standard; 2) the 'accepted vs refused' group only includes one outcome and will be recoded as standard; 
#       3) Of the the outcomes that were 'combined groups', Study ID 11 combined graduates and failures and will be recoded as  
#       as standard. The analysis will therefore compare standard and combined groups. 
# Recode variable as stated above  
prop_recid_analysis$studygroups_recode <- recode(prop_recid_analysis$studygroups, "accepted vs referred" = "standard", "accepted vs refused" = "standard")
prop_recid_analysis$studygroups_recode <- ifelse(prop_recid_analysis$studyid == '11', "standard", prop_recid_analysis$studygroups_recode)
########################################
# 4.7.1: Study groups - Combined groups
#####################################
# Select the subgroup in the model 
mod_prop_recid_stugy_groups_com <- rma(yi=yi, vi=vi, method="REML", subset=(studygroups_recode== "combined groups"), data = prop_recid_analysis)

# Create an object for the desired information  
mod_prop_stugy_groups_com_list <- c('Study groups', 'methodological', 'Combined comparison (ref)', mod_prop_recid_stugy_groups_com$k, mod_prop_recid_stugy_groups_com$beta[1], mod_prop_recid_stugy_groups_com$se[1], mod_prop_recid_stugy_groups_com$zval[1], mod_prop_recid_stugy_groups_com$pval[1], 
                              mod_prop_recid_stugy_groups_com$ci.lb[1], mod_prop_recid_stugy_groups_com$ci.ub[1], mod_prop_recid_stugy_groups_com$k- length(mod_prop_recid_stugy_groups_com$b), mod_prop_recid_stugy_groups_com$QE, mod_prop_recid_stugy_groups_com$QEp, 
                              mod_prop_recid_stugy_groups_com$I2)
# name the object's columns and bind to prop_moderator_output
names(mod_prop_stugy_groups_com_list) <- c("name", "mod_category", "level", "level_k", "est_LOR", "est_SE","est_z","est_z_p","est_cl","est_cu", "res_DF", "Qres", "Qres_p", "I2")
prop_moderator_output <- bind_rows(prop_moderator_output, mod_prop_stugy_groups_com_list)
########################################
# 4.7.2: Study groups -Standard 
#####################################
# Select the subgroup in the model 
mod_prop_recid_stugy_groups_stand <- rma(yi=yi, vi=vi, method="REML", subset=(studygroups_recode== "standard"), data = prop_recid_analysis)

# Create an object for the desired information  
mod_prop_stugy_groups_stand_list <- c('Study groups', 'methodological', 'Standard', mod_prop_recid_stugy_groups_stand$k, mod_prop_recid_stugy_groups_stand$beta[1], mod_prop_recid_stugy_groups_stand$se[1], mod_prop_recid_stugy_groups_stand$zval[1], mod_prop_recid_stugy_groups_stand$pval[1], 
                                    mod_prop_recid_stugy_groups_stand$ci.lb[1], mod_prop_recid_stugy_groups_stand$ci.ub[1], mod_prop_recid_stugy_groups_stand$k- length(mod_prop_recid_stugy_groups_stand$b), mod_prop_recid_stugy_groups_stand$QE, mod_prop_recid_stugy_groups_stand$QEp, 
                                    mod_prop_recid_stugy_groups_stand$I2)
# name the object's columns and bind to prop_moderator_output
names(mod_prop_stugy_groups_stand_list) <- c("name", "mod_category", "level", "level_k", "est_LOR", "est_SE","est_z","est_z_p","est_cl","est_cu", "res_DF", "Qres", "Qres_p", "I2")
prop_moderator_output <- bind_rows(prop_moderator_output, mod_prop_stugy_groups_stand_list)
########################################
# 4.8: Partially matched
#####################################
table(prop_recid_analysis$studygroups_alternative, prop_recid_analysis$studyid)
# Note: this variable will not be included in the proportion who recidivated moderator analysis because k = 9 outcomes did not report any information related to an alternative study group. The 'partially matched' outcomes were used to differentiate
#       among alternative studygroups within each study (i.e., to denote which, if any, groups were partially matched opposed to those that were not). There are several studies that reported using
#       sample matching which was extracted to another variable ('reported_sample_matching'), which will has it's own moderator/ subgroup analysis below. 
########################################
# 4.9: Intention-to-treat
#####################################
table(prop_recid_analysis$ITT, prop_recid_analysis$studyid)
########################################
# 4.9.1: Intention-to-treat - Graduates
#####################################
# Select the subgroup in the model 
mod_prop_recid_itt_grads <- rma(yi=yi, vi=vi, method="REML", subset=(ITT== "Graduates"), data = prop_recid_analysis)

# Create an object for the desired information  
mod_prop_itt_grads_list <- c('Intention-to-treat', 'methodological', 'Graduates (ref)', mod_prop_recid_itt_grads$k, mod_prop_recid_itt_grads$beta[1], mod_prop_recid_itt_grads$se[1], mod_prop_recid_itt_grads$zval[1], mod_prop_recid_itt_grads$pval[1], 
                                      mod_prop_recid_itt_grads$ci.lb[1], mod_prop_recid_itt_grads$ci.ub[1], mod_prop_recid_itt_grads$k- length(mod_prop_recid_itt_grads$b), mod_prop_recid_itt_grads$QE, mod_prop_recid_itt_grads$QEp, 
                                      mod_prop_recid_itt_grads$I2)
# name the object's columns and bind to prop_moderator_output
names(mod_prop_itt_grads_list) <- c("name", "mod_category", "level", "level_k", "est_LOR", "est_SE","est_z","est_z_p","est_cl","est_cu", "res_DF", "Qres", "Qres_p", "I2")
prop_moderator_output <- bind_rows(prop_moderator_output, mod_prop_itt_grads_list)
########################################
# 4.9.2: Intention-to-treat - Graduates/Failures
#####################################
# Select the subgroup in the model 
mod_prop_recid_itt_gradsfails <- rma(yi=yi, vi=vi, method="REML", subset=(ITT== "Graduates/Failures"), data = prop_recid_analysis)

# Create an object for the desired information  
mod_prop_itt_gradsfails_list <- c('Intention-to-treat', 'methodological', 'Graduates/Failures', mod_prop_recid_itt_gradsfails$k, mod_prop_recid_itt_gradsfails$beta[1], mod_prop_recid_itt_gradsfails$se[1], mod_prop_recid_itt_gradsfails$zval[1], mod_prop_recid_itt_gradsfails$pval[1], 
                             mod_prop_recid_itt_gradsfails$ci.lb[1], mod_prop_recid_itt_gradsfails$ci.ub[1], mod_prop_recid_itt_gradsfails$k- length(mod_prop_recid_itt_gradsfails$b), mod_prop_recid_itt_gradsfails$QE, mod_prop_recid_itt_gradsfails$QEp, 
                             mod_prop_recid_itt_gradsfails$I2)
# name the object's columns and bind to prop_moderator_output
names(mod_prop_itt_gradsfails_list) <- c("name", "mod_category", "level", "level_k", "est_LOR", "est_SE","est_z","est_z_p","est_cl","est_cu", "res_DF", "Qres", "Qres_p", "I2")
prop_moderator_output <- bind_rows(prop_moderator_output, mod_prop_itt_gradsfails_list)
########################################
# 4.9.3: Intention-to-treat - Not reported
#####################################
# Select the subgroup in the model 
mod_prop_recid_itt_nr <- rma(yi=yi, vi=vi, method="REML", subset=(ITT== "Not reported"), data = prop_recid_analysis)

# Create an object for the desired information  
mod_prop_itt_nr_list <- c('Intention-to-treat', 'methodological', 'Not reported', mod_prop_recid_itt_nr$k, mod_prop_recid_itt_nr$beta[1], mod_prop_recid_itt_nr$se[1], mod_prop_recid_itt_nr$zval[1], mod_prop_recid_itt_nr$pval[1], 
                                  mod_prop_recid_itt_nr$ci.lb[1], mod_prop_recid_itt_nr$ci.ub[1], mod_prop_recid_itt_nr$k- length(mod_prop_recid_itt_nr$b), mod_prop_recid_itt_nr$QE, mod_prop_recid_itt_nr$QEp, 
                                  mod_prop_recid_itt_nr$I2)
# name the object's columns and bind to prop_moderator_output
names(mod_prop_itt_nr_list) <- c("name", "mod_category", "level", "level_k", "est_LOR", "est_SE","est_z","est_z_p","est_cl","est_cu", "res_DF", "Qres", "Qres_p", "I2")
prop_moderator_output <- bind_rows(prop_moderator_output, mod_prop_itt_nr_list)

########################################
# 4.10: Lifetime arrests - Treatment 
#####################################
table(prop_recid_analysis$LT_arrests_treat, prop_recid_analysis$studyid)
# Note: there are insufficient outcomes that reported on this variable in this format to be included in a moderator/subgroup analysis. 
########################################
# 4.11: Lifetime arrests - Control 
#####################################
table(prop_recid_analysis$LT_arrests_cont, prop_recid_analysis$studyid)
# Note: there are insufficient outcomes that reported on this variable in this format to be included in a moderator/subgroup analysis. 
########################################
# 4.12: Lifetime convictions - Treatment 
#####################################
table(prop_recid_analysis$LT_convictions_treat_n, prop_recid_analysis$studyid)
# Note: there are insufficient outcomes that reported on this variable - the number of parameters to be estimated is larger than the number of observations
########################################
# 4.13: Lifetime convictions - Control 
#####################################
table(prop_recid_analysis$LT_convictions_cont_n, prop_recid_analysis$studyid)
# Note: there are insufficient outcomes that reported on this variable in this format to be included in a moderator/subgroup analysis. 
########################################
# 4.14: Mean age - Treatment 
#####################################
table(prop_recid_analysis$mean_age_treat, prop_recid_analysis$studyid)
# Insert the moderator into the model
mod_prop_recid_mean_age_tr <- rma(yi=yi, vi=vi, method="REML", mods = ~ mean_age_treat, data = prop_recid_analysis)
# Create an object for the desired information and bind it to the 'moderator_output' data frame 
mod_prop_recid_mean_age_tr_list<- c('Mean age (Tr)', 'sample characteristics', 'Beta_weight', mod_prop_recid_mean_age_tr$k, mod_prop_recid_mean_age_tr$beta[2, 1], mod_prop_recid_mean_age_tr$se[2], mod_prop_recid_mean_age_tr$zval[2], mod_prop_recid_mean_age_tr$pval[2], 
                                     mod_prop_recid_mean_age_tr$ci.lb[2], mod_prop_recid_mean_age_tr$ci.ub[2], mod_prop_recid_mean_age_tr$k- length(mod_prop_recid_mean_age_tr$b), mod_prop_recid_mean_age_tr$QE, mod_prop_recid_mean_age_tr$QEp, 
                                     mod_prop_recid_mean_age_tr$I2)
# name the object's columns 
names(mod_prop_recid_mean_age_tr_list) <- c("name", "mod_category", "level", "level_k", "est_LOR", "est_SE","est_z","est_z_p","est_cl","est_cu", "res_DF", "Qres", "Qres_p","I2")
prop_moderator_output <- bind_rows(prop_moderator_output, mod_prop_recid_mean_age_tr_list)
########################################
# 4.15: Mean age - Cont 
#####################################
table(prop_recid_analysis$mean_age_cont, prop_recid_analysis$studyid)
# Insert the moderator into the model
mod_prop_recid_mean_age_con <- rma(yi=yi, vi=vi, method="REML", mods = ~ mean_age_cont, data = prop_recid_analysis)
# Create an object for the desired information and bind it to the 'moderator_output' data frame 
mod_prop_recid_mean_age_con_list<- c('Mean age (Con)', 'sample characteristics', 'Beta_weight', mod_prop_recid_mean_age_con$k, mod_prop_recid_mean_age_con$beta[2, 1], mod_prop_recid_mean_age_con$se[2], mod_prop_recid_mean_age_con$zval[2], mod_prop_recid_mean_age_con$pval[2], 
                                    mod_prop_recid_mean_age_con$ci.lb[2], mod_prop_recid_mean_age_con$ci.ub[2], mod_prop_recid_mean_age_con$k- length(mod_prop_recid_mean_age_con$b), mod_prop_recid_mean_age_con$QE, mod_prop_recid_mean_age_con$QEp, 
                                    mod_prop_recid_mean_age_con$I2)
# name the object's columns 
names(mod_prop_recid_mean_age_con_list) <- c("name", "mod_category", "level", "level_k", "est_LOR", "est_SE","est_z","est_z_p","est_cl","est_cu", "res_DF", "Qres", "Qres_p","I2")
prop_moderator_output <- bind_rows(prop_moderator_output, mod_prop_recid_mean_age_con_list)
########################################
# 4.16: Proportion male - Treatment 
#####################################
table(prop_recid_analysis$male_._treat, prop_recid_analysis$studyid)
# Insert the moderator into the model
mod_prop_recid_prop_male_tr <- rma(yi=yi, vi=vi, method="REML", mods = ~ male_._treat, data = prop_recid_analysis)
# Create an object for the desired information and bind it to the 'moderator_output' data frame 
mod_prop_recid_prop_male_tr_list<- c('Proportion male (Tr)', 'sample characteristics', 'Beta_weight', mod_prop_recid_prop_male_tr$k, mod_prop_recid_prop_male_tr$beta[2, 1], mod_prop_recid_prop_male_tr$se[2], mod_prop_recid_prop_male_tr$zval[2], mod_prop_recid_prop_male_tr$pval[2], 
                       mod_prop_recid_prop_male_tr$ci.lb[2], mod_prop_recid_prop_male_tr$ci.ub[2], mod_prop_recid_prop_male_tr$k- length(mod_prop_recid_prop_male_tr$b), mod_prop_recid_prop_male_tr$QE, mod_prop_recid_prop_male_tr$QEp, 
                       mod_prop_recid_prop_male_tr$I2)
# name the object's columns 
names(mod_prop_recid_prop_male_tr_list) <- c("name", "mod_category", "level", "level_k", "est_LOR", "est_SE","est_z","est_z_p","est_cl","est_cu", "res_DF", "Qres", "Qres_p","I2")
prop_moderator_output <- bind_rows(prop_moderator_output, mod_prop_recid_prop_male_tr_list)
########################################
# 4.17: Proportion male - Control 
#####################################
table(prop_recid_analysis$male_._cont, prop_recid_analysis$studyid)
# Insert the moderator into the model
mod_prop_recid_prop_male_con <- rma(yi=yi, vi=vi, method="REML", mods = ~ male_._cont, data = prop_recid_analysis)
# Note: there are insufficient outcomes that reported on this variable - the number of parameters to be estimated is larger than the number of observations
# Create an object for the desired information and bind it to the 'moderator_output' data frame 
mod_prop_recid_prop_male_con_list<- c('Proportion male (Con)', 'sample characteristics', 'Beta_weight', mod_prop_recid_prop_male_con$k, mod_prop_recid_prop_male_con$beta[2, 1], mod_prop_recid_prop_male_con$se[2], mod_prop_recid_prop_male_con$zval[2], mod_prop_recid_prop_male_con$pval[2], 
                               mod_prop_recid_prop_male_con$ci.lb[2], mod_prop_recid_prop_male_con$ci.ub[2], mod_prop_recid_prop_male_con$k- length(mod_prop_recid_prop_male_con$b), mod_prop_recid_prop_male_con$QE, mod_prop_recid_prop_male_con$QEp, 
                               mod_prop_recid_prop_male_con$I2)
# name the object's columns 
names(mod_prop_recid_prop_male_con_list) <- c("name", "mod_category", "level", "level_k", "est_LOR", "est_SE","est_z","est_z_p","est_cl","est_cu", "res_DF", "Qres", "Qres_p","I2")
prop_moderator_output <- bind_rows(prop_moderator_output, mod_prop_recid_prop_male_con_list)
########################################
# 4.18: Proportion ATSI or Maori - Treatment 
#####################################
table(prop_recid_analysis$ATSI_or_MAORI_._treat, prop_recid_analysis$studyid)
# Insert the moderator into the model
mod_prop_recid_prop_ATSI_or_Maori_tr <- rma(yi=yi, vi=vi, method="REML", mods = ~ ATSI_or_MAORI_._treat, control=list(stepadj=0.5), data = prop_recid_analysis)
# Create an object for the desired information and bind it to the 'moderator_output' data frame 
mod_prop_recid_prop_ATSI_or_Maori_tr_list <- c('Proportion ATSI or Maori (Tr)', 'sample characteristics', 'Beta_weight', mod_prop_recid_prop_ATSI_or_Maori_tr$k, mod_prop_recid_prop_ATSI_or_Maori_tr$beta[2, 1], mod_prop_recid_prop_ATSI_or_Maori_tr$se[2], mod_prop_recid_prop_ATSI_or_Maori_tr$zval[2], mod_prop_recid_prop_ATSI_or_Maori_tr$pval[2], 
                               mod_prop_recid_prop_ATSI_or_Maori_tr$ci.lb[2], mod_prop_recid_prop_ATSI_or_Maori_tr$ci.ub[2], mod_prop_recid_prop_ATSI_or_Maori_tr$k- length(mod_prop_recid_prop_ATSI_or_Maori_tr$b), mod_prop_recid_prop_ATSI_or_Maori_tr$QE, mod_prop_recid_prop_ATSI_or_Maori_tr$QEp, 
                               mod_prop_recid_prop_ATSI_or_Maori_tr$I2)
# name the object's columns 
names(mod_prop_recid_prop_ATSI_or_Maori_tr_list) <- c("name", "mod_category", "level", "level_k", "est_LOR", "est_SE","est_z","est_z_p","est_cl","est_cu", "res_DF", "Qres", "Qres_p","I2")
prop_moderator_output <- bind_rows(prop_moderator_output, mod_prop_recid_prop_ATSI_or_Maori_tr_list)
########################################
# 4.19: Proportion ATSI or Maori - Control 
#####################################
table(prop_recid_analysis$ATSI_or_MAORI_._cont, prop_recid_analysis$studyid)
# Insert the moderator into the model
mod_prop_recid_prop_ATSI_or_Maori_con <- rma(yi=yi, vi=vi, method="REML", mods = ~ ATSI_or_MAORI_._cont, data = prop_recid_analysis)
# Create an object for the desired information and bind it to the 'moderator_output' data frame 
mod_prop_recid_prop_ATSI_or_Maori_con_list <- c('Proportion ATSI or Maori (Con)', 'sample characteristics', 'Beta_weight', mod_prop_recid_prop_ATSI_or_Maori_con$k, mod_prop_recid_prop_ATSI_or_Maori_con$beta[2, 1], mod_prop_recid_prop_ATSI_or_Maori_con$se[2], mod_prop_recid_prop_ATSI_or_Maori_con$zval[2], mod_prop_recid_prop_ATSI_or_Maori_con$pval[2], 
                                         mod_prop_recid_prop_ATSI_or_Maori_con$ci.lb[2], mod_prop_recid_prop_ATSI_or_Maori_con$ci.ub[2], mod_prop_recid_prop_ATSI_or_Maori_con$k- length(mod_prop_recid_prop_ATSI_or_Maori_con$b), mod_prop_recid_prop_ATSI_or_Maori_con$QE, mod_prop_recid_prop_ATSI_or_Maori_con$QEp, 
                                         mod_prop_recid_prop_ATSI_or_Maori_con$I2)
# name the object's columns 
names(mod_prop_recid_prop_ATSI_or_Maori_con_list) <- c("name", "mod_category", "level", "level_k", "est_LOR", "est_SE","est_z","est_z_p","est_cl","est_cu", "res_DF", "Qres", "Qres_p","I2")
prop_moderator_output <- bind_rows(prop_moderator_output, mod_prop_recid_prop_ATSI_or_Maori_con_list)
########################################
# 4.20: Post-test period months 
#####################################
table(prop_recid_analysis$pt_period_months, prop_recid_analysis$studyid)
# Insert the moderator into the model
mod_prop_recid_prop_pt_months <- rma(yi=yi, vi=vi, method="REML", mods = ~ pt_period_months, data = prop_recid_analysis, verbose=TRUE, control=list(stepadj=0.5))
# Create an object for the desired information and bind it to the 'moderator_output' data frame 
mod_prop_recid_pt_months_list <- c('Post-test period months', 'methodological', 'Beta_weight', mod_prop_recid_prop_pt_months$k, mod_prop_recid_prop_pt_months$beta[2, 1], mod_prop_recid_prop_pt_months$se[2], mod_prop_recid_prop_pt_months$zval[2], mod_prop_recid_prop_pt_months$pval[2], 
                                          mod_prop_recid_prop_pt_months$ci.lb[2], mod_prop_recid_prop_pt_months$ci.ub[2], mod_prop_recid_prop_pt_months$k- length(mod_prop_recid_prop_pt_months$b), mod_prop_recid_prop_pt_months$QE, mod_prop_recid_prop_pt_months$QEp, 
                                          mod_prop_recid_prop_pt_months$I2)
# name the object's columns 
names(mod_prop_recid_pt_months_list) <- c("name", "mod_category", "level", "level_k", "est_LOR", "est_SE","est_z","est_z_p","est_cl","est_cu", "res_DF", "Qres", "Qres_p","I2")
prop_moderator_output <- bind_rows(prop_moderator_output, mod_prop_recid_pt_months_list)
########################################
# 4.21: Post-test begins 
#####################################
table(prop_recid_analysis$pt_begins, prop_recid_analysis$studyid)
# Note: this variable will not be included in the proportion who recidivated moderator analysis because the it is effectively grouped (into those measured during and post enrollment) by the 'measured_during_or_post_pt' that
#       it is analysed directly below. 
########################################
# 4.22: Measured during or post treatment
#####################################
table(prop_recid_analysis$measured_during_or_post_pt, prop_recid_analysis$studyid)
########################################
# 4.22.1: Measured during or post treatment - During
#####################################
# Select the subgroup in the model 
mod_prop_recid_pt_during<- rma(yi=yi, vi=vi, method="REML", subset=(measured_during_or_post_pt== "During"), data = prop_recid_analysis)

# Create an object for the desired information  
mod_prop_recid_pt_during_list <- c('Measured during or post treatment', 'methodological', 'During (ref)', mod_prop_recid_pt_during$k, mod_prop_recid_pt_during$beta[1], mod_prop_recid_pt_during$se[1], mod_prop_recid_pt_during$zval[1], mod_prop_recid_pt_during$pval[1], 
                                      mod_prop_recid_pt_during$ci.lb[1], mod_prop_recid_pt_during$ci.ub[1], mod_prop_recid_pt_during$k- length(mod_prop_recid_pt_during$b), mod_prop_recid_pt_during$QE, mod_prop_recid_pt_during$QEp, 
                                      mod_prop_recid_pt_during$I2)
# name the object's columns and bind to prop_moderator_output
names(mod_prop_recid_pt_during_list) <- c("name", "mod_category", "level", "level_k", "est_LOR", "est_SE","est_z","est_z_p","est_cl","est_cu", "res_DF", "Qres", "Qres_p", "I2")
prop_moderator_output <- bind_rows(prop_moderator_output, mod_prop_recid_pt_during_list)
########################################
# 4.22.2: Measured during or post treatment - Post
#####################################
# Select the subgroup in the model 
mod_prop_recid_pt_post<- rma(yi=yi, vi=vi, method="REML", subset=(measured_during_or_post_pt== "Post"), data = prop_recid_analysis)

# Create an object for the desired information  
mod_prop_recid_pt_post_list <- c('Measured during or post treatment', 'methodological', 'Post', mod_prop_recid_pt_post$k, mod_prop_recid_pt_post$beta[1], mod_prop_recid_pt_post$se[1], mod_prop_recid_pt_post$zval[1], mod_prop_recid_pt_post$pval[1], 
                                  mod_prop_recid_pt_post$ci.lb[1], mod_prop_recid_pt_post$ci.ub[1], mod_prop_recid_pt_post$k- length(mod_prop_recid_pt_post$b), mod_prop_recid_pt_post$QE, mod_prop_recid_pt_post$QEp, 
                                  mod_prop_recid_pt_post$I2)
# name the object's columns and bind to prop_moderator_output
names(mod_prop_recid_pt_post_list) <- c("name", "mod_category", "level", "level_k", "est_LOR", "est_SE","est_z","est_z_p","est_cl","est_cu", "res_DF", "Qres", "Qres_p", "I2")
prop_moderator_output <- bind_rows(prop_moderator_output, mod_prop_recid_pt_post_list)
########################################
# 4.23: Program duration minimum (months)
#####################################
table(prop_recid_analysis$Program_duration_minimum_months, prop_recid_analysis$studyid)
# Insert the moderator into the model
mod_prop_recid_prog_dur <- rma(yi=yi, vi=vi, method="REML", mods = ~ Program_duration_minimum_months, data = prop_recid_analysis)
# Create an object for the desired information and bind it to the 'moderator_output' data frame 
mod_prop_recid_prog_dur_list <- c('Program duration minimum (months)', 'programmatic', 'Beta_weight', mod_prop_recid_prog_dur$k, mod_prop_recid_prog_dur$beta[2, 1], mod_prop_recid_prog_dur$se[2], mod_prop_recid_prog_dur$zval[2], mod_prop_recid_prog_dur$pval[2], 
                                          mod_prop_recid_prog_dur$ci.lb[2], mod_prop_recid_prog_dur$ci.ub[2], mod_prop_recid_prog_dur$k- length(mod_prop_recid_prog_dur$b), mod_prop_recid_prog_dur$QE, mod_prop_recid_prog_dur$QEp, 
                                          mod_prop_recid_prog_dur$I2)
# name the object's columns 
names(mod_prop_recid_prog_dur_list) <- c("name", "mod_category", "level", "level_k", "est_LOR", "est_SE","est_z","est_z_p","est_cl","est_cu", "res_DF", "Qres", "Qres_p","I2")
prop_moderator_output <- bind_rows(prop_moderator_output, mod_prop_recid_prog_dur_list)

########################################
# 4.24: Program duration average (months)
#####################################
table(prop_recid_analysis$Program_duration_average_months, prop_recid_analysis$studyid)
# Insert the moderator into the model
mod_prop_recid_prog_dur_avg <- rma(yi=yi, vi=vi, method="REML", mods = ~ Program_duration_average_months, data = prop_recid_analysis)
# Create an object for the desired information and bind it to the 'moderator_output' data frame 
mod_prop_recid_prog_dur_avg_list <- c('Program duration average (months)', 'programmatic', 'Beta_weight', mod_prop_recid_prog_dur_avg$k, mod_prop_recid_prog_dur_avg$beta[2, 1], mod_prop_recid_prog_dur_avg$se[2], mod_prop_recid_prog_dur_avg$zval[2], mod_prop_recid_prog_dur_avg$pval[2], 
                                  mod_prop_recid_prog_dur_avg$ci.lb[2], mod_prop_recid_prog_dur_avg$ci.ub[2], mod_prop_recid_prog_dur_avg$k- length(mod_prop_recid_prog_dur_avg$b), mod_prop_recid_prog_dur_avg$QE, mod_prop_recid_prog_dur_avg$QEp, 
                                  mod_prop_recid_prog_dur_avg$I2)
# name the object's columns 
names(mod_prop_recid_prog_dur_avg_list) <- c("name", "mod_category", "level", "level_k", "est_LOR", "est_SE","est_z","est_z_p","est_cl","est_cu", "res_DF", "Qres", "Qres_p","I2")
prop_moderator_output <- bind_rows(prop_moderator_output, mod_prop_recid_prog_dur_avg_list)
########################################
# 4.25: Post treatment months - Treatment
#####################################
table(prop_recid_analysis$Post_program_PT_months_treat, prop_recid_analysis$studyid)
# Insert the moderator into the model
mod_prop_recid_post_treat_months_tr <- rma(yi=yi, vi=vi, method="REML", mods = ~ Post_program_PT_months_treat, data = prop_recid_analysis)
# Create an object for the desired information and bind it to the 'moderator_output' data frame 
mod_prop_recid_post_treat_months_tr_list <- c('Post treatment months (Tr)', 'methodological', 'Beta_weight', mod_prop_recid_post_treat_months_tr$k, mod_prop_recid_post_treat_months_tr$beta[2, 1], mod_prop_recid_post_treat_months_tr$se[2], mod_prop_recid_post_treat_months_tr$zval[2], mod_prop_recid_post_treat_months_tr$pval[2], 
                                      mod_prop_recid_post_treat_months_tr$ci.lb[2], mod_prop_recid_post_treat_months_tr$ci.ub[2], mod_prop_recid_post_treat_months_tr$k- length(mod_prop_recid_post_treat_months_tr$b), mod_prop_recid_post_treat_months_tr$QE, mod_prop_recid_post_treat_months_tr$QEp, 
                                      mod_prop_recid_post_treat_months_tr$I2)
# name the object's columns 
names(mod_prop_recid_post_treat_months_tr_list) <- c("name", "mod_category", "level", "level_k", "est_LOR", "est_SE","est_z","est_z_p","est_cl","est_cu", "res_DF", "Qres", "Qres_p","I2")
prop_moderator_output <- bind_rows(prop_moderator_output, mod_prop_recid_post_treat_months_tr_list)
########################################
# 4.26: Post treatment months - Control
#####################################
table(prop_recid_analysis$Post_program_PT_months_cont, prop_recid_analysis$studyid)
# Insert the moderator into the model
mod_prop_recid_post_treat_months_con <- rma(yi=yi, vi=vi, method="REML", mods = ~ Post_program_PT_months_cont, data = prop_recid_analysis)
# Create an object for the desired information and bind it to the 'moderator_output' data frame 
mod_prop_recid_post_treat_months_con_list <- c('Post treatment months (Con)', 'methodological', 'Beta_weight', mod_prop_recid_post_treat_months_con$k, mod_prop_recid_post_treat_months_con$beta[2, 1], mod_prop_recid_post_treat_months_con$se[2], mod_prop_recid_post_treat_months_con$zval[2], mod_prop_recid_post_treat_months_con$pval[2], 
                                              mod_prop_recid_post_treat_months_con$ci.lb[2], mod_prop_recid_post_treat_months_con$ci.ub[2], mod_prop_recid_post_treat_months_con$k- length(mod_prop_recid_post_treat_months_con$b), mod_prop_recid_post_treat_months_con$QE, mod_prop_recid_post_treat_months_con$QEp, 
                                              mod_prop_recid_post_treat_months_con$I2)
# name the object's columns 
names(mod_prop_recid_post_treat_months_con_list) <- c("name", "mod_category", "level", "level_k", "est_LOR", "est_SE","est_z","est_z_p","est_cl","est_cu", "res_DF", "Qres", "Qres_p","I2")
prop_moderator_output <- bind_rows(prop_moderator_output, mod_prop_recid_post_treat_months_con_list)
########################################
# 4.27: Charge level
#####################################
table(prop_recid_analysis$charge_level, prop_recid_analysis$studyid)
# Recode 'indictable' to 'summary/indictable'
prop_recid_analysis$charge_level_recode <- prop_recid_analysis$charge_level %>% 
                                               recode(., "indictable" = "summary/indictable") %>% 
                                                        replace_na(., "NA")
########################################
# 4.27.1: Charge level - Summary
#####################################
# Select the subgroup in the model 
mod_prop_recid_charge_level_summ <- rma(yi=yi, vi=vi, method="REML", subset=(charge_level_recode== "summary"), data = prop_recid_analysis)

# Create an object for the desired information  
mod_prop_recid_charge_level_summ_list <- c('Charge level', 'programmatic', 'Summary (ref)', mod_prop_recid_charge_level_summ$k, mod_prop_recid_charge_level_summ$beta[1], mod_prop_recid_charge_level_summ$se[1], mod_prop_recid_charge_level_summ$zval[1], mod_prop_recid_charge_level_summ$pval[1], 
                                  mod_prop_recid_charge_level_summ$ci.lb[1], mod_prop_recid_charge_level_summ$ci.ub[1], mod_prop_recid_charge_level_summ$k- length(mod_prop_recid_charge_level_summ$b), mod_prop_recid_charge_level_summ$QE, mod_prop_recid_charge_level_summ$QEp, 
                                  mod_prop_recid_charge_level_summ$I2)
# name the object's columns and bind to prop_moderator_output
names(mod_prop_recid_charge_level_summ_list) <- c("name", "mod_category", "level", "level_k", "est_LOR", "est_SE","est_z","est_z_p","est_cl","est_cu", "res_DF", "Qres", "Qres_p", "I2")
prop_moderator_output <- bind_rows(prop_moderator_output, mod_prop_recid_charge_level_summ_list)
########################################
# 4.27.2: Charge level - Summary/indictable
#####################################
# Select the subgroup in the model 
mod_prop_recid_charge_level_indict<- rma(yi=yi, vi=vi, method="REML", subset=(charge_level_recode== "summary/indictable"), data = prop_recid_analysis)

# Create an object for the desired information  
mod_prop_recid_charge_level_indict_list <- c('Charge level', 'programmatic', 'Summary/indictable', mod_prop_recid_charge_level_indict$k, mod_prop_recid_charge_level_indict$beta[1], mod_prop_recid_charge_level_indict$se[1], mod_prop_recid_charge_level_indict$zval[1], mod_prop_recid_charge_level_indict$pval[1], 
                                           mod_prop_recid_charge_level_indict$ci.lb[1], mod_prop_recid_charge_level_indict$ci.ub[1], mod_prop_recid_charge_level_indict$k- length(mod_prop_recid_charge_level_indict$b), mod_prop_recid_charge_level_indict$QE, mod_prop_recid_charge_level_indict$QEp, 
                                           mod_prop_recid_charge_level_indict$I2)
# name the object's columns and bind to prop_moderator_output
names(mod_prop_recid_charge_level_indict_list) <- c("name", "mod_category", "level", "level_k", "est_LOR", "est_SE","est_z","est_z_p","est_cl","est_cu", "res_DF", "Qres", "Qres_p", "I2")
prop_moderator_output <- bind_rows(prop_moderator_output, mod_prop_recid_charge_level_indict_list)
########################################
# 4.28: Accepted violent offenders
#####################################
table(prop_recid_analysis$accepeted_violent, prop_recid_analysis$studyid)
# Transform NAs to character strings 
prop_recid_analysis$accepeted_violent_recode <- prop_recid_analysis$accepeted_violent %>% 
                                                     replace_na(., "NA")
########################################
# 4.28.1: Accepted violent offenders - No
#####################################
# Select the subgroup in the model 
mod_prop_recid_acc_vio_no <- rma(yi=yi, vi=vi, method="REML", subset=(accepeted_violent_recode== "No"), data = prop_recid_analysis)

# Create an object for the desired information  
mod_prop_recid_acc_vio_no_list <- c('Accepted violent offenders', 'programmatic', 'No (ref)', mod_prop_recid_acc_vio_no$k, mod_prop_recid_acc_vio_no$beta[1], mod_prop_recid_acc_vio_no$se[1], mod_prop_recid_acc_vio_no$zval[1], mod_prop_recid_acc_vio_no$pval[1], 
                                           mod_prop_recid_acc_vio_no$ci.lb[1], mod_prop_recid_acc_vio_no$ci.ub[1], mod_prop_recid_acc_vio_no$k- length(mod_prop_recid_acc_vio_no$b), mod_prop_recid_acc_vio_no$QE, mod_prop_recid_acc_vio_no$QEp, 
                                           mod_prop_recid_acc_vio_no$I2)
# name the object's columns and bind to prop_moderator_output
names(mod_prop_recid_acc_vio_no_list) <- c("name", "mod_category", "level", "level_k", "est_LOR", "est_SE","est_z","est_z_p","est_cl","est_cu", "res_DF", "Qres", "Qres_p", "I2")
prop_moderator_output <- bind_rows(prop_moderator_output, mod_prop_recid_acc_vio_no_list)
########################################
# 4.28.2: Accepted violent offenders - Yes
#####################################
# Select the subgroup in the model 
mod_prop_recid_acc_vio_yes <- rma(yi=yi, vi=vi, method="REML", subset=(accepeted_violent_recode== "Yes"), data = prop_recid_analysis)

# Create an object for the desired information  
mod_prop_recid_acc_vio_yes_list <- c('Accepted violent offenders', 'programmatic', 'Yes', mod_prop_recid_acc_vio_yes$k, mod_prop_recid_acc_vio_yes$beta[1], mod_prop_recid_acc_vio_yes$se[1], mod_prop_recid_acc_vio_yes$zval[1], mod_prop_recid_acc_vio_yes$pval[1], 
                                    mod_prop_recid_acc_vio_yes$ci.lb[1], mod_prop_recid_acc_vio_yes$ci.ub[1], mod_prop_recid_acc_vio_yes$k- length(mod_prop_recid_acc_vio_yes$b), mod_prop_recid_acc_vio_yes$QE, mod_prop_recid_acc_vio_yes$QEp, 
                                    mod_prop_recid_acc_vio_yes$I2)
# name the object's columns and bind to prop_moderator_output
names(mod_prop_recid_acc_vio_yes_list) <- c("name", "mod_category", "level", "level_k", "est_LOR", "est_SE","est_z","est_z_p","est_cl","est_cu", "res_DF", "Qres", "Qres_p", "I2")
prop_moderator_output <- bind_rows(prop_moderator_output, mod_prop_recid_acc_vio_yes_list)
########################################
# 4.29: Reported accepting violent offenders
#####################################
table(prop_recid_analysis$reported_accepting_violent, prop_recid_analysis$studyid)
########################################
# 4.29.1: Reported accepting violent offenders - No
#####################################
# Select the subgroup in the model 
mod_prop_recid_rep_acc_vio_no <- rma(yi=yi, vi=vi, method="REML", subset=(reported_accepting_violent== "No"), data = prop_recid_analysis)

# Create an object for the desired information  
mod_prop_recid_rep_acc_vio_no_list <- c('Reported accepting violent offenders', 'programmatic', 'No (ref)', mod_prop_recid_rep_acc_vio_no$k, mod_prop_recid_rep_acc_vio_no$beta[1], mod_prop_recid_rep_acc_vio_no$se[1], mod_prop_recid_rep_acc_vio_no$zval[1], mod_prop_recid_rep_acc_vio_no$pval[1], 
                                    mod_prop_recid_rep_acc_vio_no$ci.lb[1], mod_prop_recid_rep_acc_vio_no$ci.ub[1], mod_prop_recid_rep_acc_vio_no$k- length(mod_prop_recid_rep_acc_vio_no$b), mod_prop_recid_rep_acc_vio_no$QE, mod_prop_recid_rep_acc_vio_no$QEp, 
                                    mod_prop_recid_rep_acc_vio_no$I2)
# name the object's columns and bind to prop_moderator_output
names(mod_prop_recid_rep_acc_vio_no_list) <- c("name", "mod_category", "level", "level_k", "est_LOR", "est_SE","est_z","est_z_p","est_cl","est_cu", "res_DF", "Qres", "Qres_p", "I2")
prop_moderator_output <- bind_rows(prop_moderator_output, mod_prop_recid_rep_acc_vio_no_list)
########################################
# 4.29.2: Reported accepting violent offenders - Yes
#####################################
# Select the subgroup in the model 
mod_prop_recid_rep_acc_vio_yes <- rma(yi=yi, vi=vi, method="REML", subset=(reported_accepting_violent== "Yes"), data = prop_recid_analysis)

# Create an object for the desired information  
mod_prop_recid_rep_acc_vio_yes_list <- c('Reported accepting violent offenders', 'programmatic', 'Yes', mod_prop_recid_rep_acc_vio_yes$k, mod_prop_recid_rep_acc_vio_yes$beta[1], mod_prop_recid_rep_acc_vio_yes$se[1], mod_prop_recid_rep_acc_vio_yes$zval[1], mod_prop_recid_rep_acc_vio_yes$pval[1], 
                                        mod_prop_recid_rep_acc_vio_yes$ci.lb[1], mod_prop_recid_rep_acc_vio_yes$ci.ub[1], mod_prop_recid_rep_acc_vio_yes$k- length(mod_prop_recid_rep_acc_vio_yes$b), mod_prop_recid_rep_acc_vio_yes$QE, mod_prop_recid_rep_acc_vio_yes$QEp, 
                                        mod_prop_recid_rep_acc_vio_yes$I2)
# name the object's columns and bind to prop_moderator_output
names(mod_prop_recid_rep_acc_vio_yes_list) <- c("name", "mod_category", "level", "level_k", "est_LOR", "est_SE","est_z","est_z_p","est_cl","est_cu", "res_DF", "Qres", "Qres_p", "I2")
prop_moderator_output <- bind_rows(prop_moderator_output, mod_prop_recid_rep_acc_vio_yes_list)
########################################
# 4.30: Reported accepting indictable charges
#####################################
table(prop_recid_analysis$reported_accepting_indictable, prop_recid_analysis$studyid)
########################################
# 4.30.1: Reported accepting indictable charges - No
#####################################
# Select the subgroup in the model 
mod_prop_recid_rep_acc_indict_no <- rma(yi=yi, vi=vi, method="REML", subset=(reported_accepting_indictable== "No"), data = prop_recid_analysis)

# Create an object for the desired information  
mod_prop_recid_rep_acc_indict_no_list <- c('Reported accepting indictable charges', 'programmatic', 'No (ref)', mod_prop_recid_rep_acc_indict_no$k, mod_prop_recid_rep_acc_indict_no$beta[1], mod_prop_recid_rep_acc_indict_no$se[1], mod_prop_recid_rep_acc_indict_no$zval[1], mod_prop_recid_rep_acc_indict_no$pval[1], 
                                        mod_prop_recid_rep_acc_indict_no$ci.lb[1], mod_prop_recid_rep_acc_indict_no$ci.ub[1], mod_prop_recid_rep_acc_indict_no$k- length(mod_prop_recid_rep_acc_indict_no$b), mod_prop_recid_rep_acc_indict_no$QE, mod_prop_recid_rep_acc_indict_no$QEp, 
                                        mod_prop_recid_rep_acc_indict_no$I2)
# name the object's columns and bind to prop_moderator_output
names(mod_prop_recid_rep_acc_indict_no_list) <- c("name", "mod_category", "level", "level_k", "est_LOR", "est_SE","est_z","est_z_p","est_cl","est_cu", "res_DF", "Qres", "Qres_p", "I2")
prop_moderator_output <- bind_rows(prop_moderator_output, mod_prop_recid_rep_acc_indict_no_list)
########################################
# 4.30.2: Reported accepting indictable charges - Yes
#####################################
# Select the subgroup in the model 
mod_prop_recid_rep_acc_indict_yes <- rma(yi=yi, vi=vi, method="REML", subset=(reported_accepting_indictable== "Yes"), data = prop_recid_analysis)

# Create an object for the desired information  
mod_prop_recid_rep_acc_indict_yes_list <- c('Reported accepting indictable charges', 'programmatic', 'Yes', mod_prop_recid_rep_acc_indict_yes$k, mod_prop_recid_rep_acc_indict_yes$beta[1], mod_prop_recid_rep_acc_indict_yes$se[1], mod_prop_recid_rep_acc_indict_yes$zval[1], mod_prop_recid_rep_acc_indict_yes$pval[1], 
                                           mod_prop_recid_rep_acc_indict_yes$ci.lb[1], mod_prop_recid_rep_acc_indict_yes$ci.ub[1], mod_prop_recid_rep_acc_indict_yes$k- length(mod_prop_recid_rep_acc_indict_yes$b), mod_prop_recid_rep_acc_indict_yes$QE, mod_prop_recid_rep_acc_indict_yes$QEp, 
                                           mod_prop_recid_rep_acc_indict_yes$I2)
# name the object's columns and bind to prop_moderator_output
names(mod_prop_recid_rep_acc_indict_yes_list) <- c("name", "mod_category", "level", "level_k", "est_LOR", "est_SE","est_z","est_z_p","est_cl","est_cu", "res_DF", "Qres", "Qres_p", "I2")
prop_moderator_output <- bind_rows(prop_moderator_output, mod_prop_recid_rep_acc_indict_yes_list)


########################################
# 4.31: Reported expunging charges
#####################################
table(prop_recid_analysis$reported_charges_expunged, prop_recid_analysis$studyid)
# Note: there are insufficient outcomes that reported on this variable in this format to be included in a moderator/subgroup analysis. 
########################################
# 4.32: Proportion of sample that were graduates - Treatment
#####################################
table(prop_recid_analysis$pt_._program_graduates, prop_recid_analysis$studyid)
# Insert the moderator into the model
mod_prop_recid_prog_grad_tr <- rma(yi=yi, vi=vi, method="REML", mods = ~ pt_._program_graduates, data = prop_recid_analysis)
# Create an object for the desired information and bind it to the 'moderator_output' data frame 
mod_prop_recid_prog_grad_tr_list <- c('Proportion that were graduates (Tr)', 'sample characteristics', 'Beta_weight', mod_prop_recid_prog_grad_tr$k, mod_prop_recid_prog_grad_tr$beta[2, 1], mod_prop_recid_prog_grad_tr$se[2], mod_prop_recid_prog_grad_tr$zval[2], mod_prop_recid_prog_grad_tr$pval[2], 
                                              mod_prop_recid_prog_grad_tr$ci.lb[2], mod_prop_recid_prog_grad_tr$ci.ub[2], mod_prop_recid_prog_grad_tr$k- length(mod_prop_recid_prog_grad_tr$b), mod_prop_recid_prog_grad_tr$QE, mod_prop_recid_prog_grad_tr$QEp, 
                                              mod_prop_recid_prog_grad_tr$I2)
# name the object's columns 
names(mod_prop_recid_prog_grad_tr_list) <- c("name", "mod_category", "level", "level_k", "est_LOR", "est_SE","est_z","est_z_p","est_cl","est_cu", "res_DF", "Qres", "Qres_p","I2")
prop_moderator_output <- bind_rows(prop_moderator_output, mod_prop_recid_prog_grad_tr_list)
########################################
# 4.33: Proportion of sample that were terminates - Treatment
#####################################
table(prop_recid_analysis$pt_._program_terminates, prop_recid_analysis$studyid)
# Insert the moderator into the model
mod_prop_recid_prog_term_tr <- rma(yi=yi, vi=vi, method="REML", mods = ~ pt_._program_terminates, data = prop_recid_analysis)
# Create an object for the desired information and bind it to the 'moderator_output' data frame 
mod_prop_recid_prog_term_tr_list <- c('Proportion that were terminates (Tr)', 'sample characteristics', 'Beta_weight', mod_prop_recid_prog_term_tr$k, mod_prop_recid_prog_term_tr$beta[2, 1], mod_prop_recid_prog_term_tr$se[2], mod_prop_recid_prog_term_tr$zval[2], mod_prop_recid_prog_term_tr$pval[2], 
                                      mod_prop_recid_prog_term_tr$ci.lb[2], mod_prop_recid_prog_term_tr$ci.ub[2], mod_prop_recid_prog_term_tr$k- length(mod_prop_recid_prog_term_tr$b), mod_prop_recid_prog_term_tr$QE, mod_prop_recid_prog_term_tr$QEp, 
                                      mod_prop_recid_prog_term_tr$I2)
# name the object's columns 
names(mod_prop_recid_prog_term_tr_list) <- c("name", "mod_category", "level", "level_k", "est_LOR", "est_SE","est_z","est_z_p","est_cl","est_cu", "res_DF", "Qres", "Qres_p","I2")
prop_moderator_output <- bind_rows(prop_moderator_output, mod_prop_recid_prog_term_tr_list)
########################################
# 4.34: Proportion of sample that completed order - Treatment
#####################################
table(prop_recid_analysis$pt_order_completion_treat_., prop_recid_analysis$studyid)
# Insert the moderator into the model
mod_prop_recid_ord_compl_tr <- rma(yi=yi, vi=vi, method="REML", mods = ~ pt_order_completion_treat_., data = prop_recid_analysis)
# Create an object for the desired information and bind it to the 'moderator_output' data frame 
mod_prop_recid_ord_compl_tr_list <- c('Proportion that completed order (Tr)', 'sample characteristics', 'Beta_weight', mod_prop_recid_ord_compl_tr$k, mod_prop_recid_ord_compl_tr$beta[2, 1], mod_prop_recid_ord_compl_tr$se[2], mod_prop_recid_ord_compl_tr$zval[2], mod_prop_recid_ord_compl_tr$pval[2], 
                                      mod_prop_recid_ord_compl_tr$ci.lb[2], mod_prop_recid_ord_compl_tr$ci.ub[2], mod_prop_recid_ord_compl_tr$k- length(mod_prop_recid_ord_compl_tr$b), mod_prop_recid_ord_compl_tr$QE, mod_prop_recid_ord_compl_tr$QEp, 
                                      mod_prop_recid_ord_compl_tr$I2)
# name the object's columns 
names(mod_prop_recid_ord_compl_tr_list) <- c("name", "mod_category", "level", "level_k", "est_LOR", "est_SE","est_z","est_z_p","est_cl","est_cu", "res_DF", "Qres", "Qres_p","I2")
prop_moderator_output <- bind_rows(prop_moderator_output, mod_prop_recid_ord_compl_tr_list)
########################################
# 4.35: Proportion of sample that completed order - Control
#####################################
table(prop_recid_analysis$pt_order_completion_cont_., prop_recid_analysis$studyid)
# Note: there are insufficient outcomes that reported on this variable - the number of parameters to be estimated is larger than the number of observations
########################################
# 4.36: Publication type
#####################################
table(prop_recid_analysis$source, prop_recid_analysis$studyid)
########################################
# 4.36.1: Publication type - Evaluation
#####################################
# Select the subgroup in the model 
mod_prop_recid_pub_type_eval <- rma(yi=yi, vi=vi, method="REML", subset=(source== "Evaluation"), data = prop_recid_analysis, verbose=TRUE, digits=5, control=list(stepadj=0.5))

# Create an object for the desired information  
mod_prop_recid_pub_type_eval_list <- c('Publication type', 'methodological', 'Evaluation (ref)', mod_prop_recid_pub_type_eval$k, mod_prop_recid_pub_type_eval$beta[1], mod_prop_recid_pub_type_eval$se[1], mod_prop_recid_pub_type_eval$zval[1], mod_prop_recid_pub_type_eval$pval[1], 
                                           mod_prop_recid_pub_type_eval$ci.lb[1], mod_prop_recid_pub_type_eval$ci.ub[1], mod_prop_recid_pub_type_eval$k- length(mod_prop_recid_pub_type_eval$b), mod_prop_recid_pub_type_eval$QE, mod_prop_recid_pub_type_eval$QEp, 
                                           mod_prop_recid_pub_type_eval$I2)
# name the object's columns and bind to prop_moderator_output
names(mod_prop_recid_pub_type_eval_list) <- c("name", "mod_category", "level", "level_k", "est_LOR", "est_SE","est_z","est_z_p","est_cl","est_cu", "res_DF", "Qres", "Qres_p", "I2")
prop_moderator_output <- bind_rows(prop_moderator_output, mod_prop_recid_pub_type_eval_list)
########################################
# 4.36.2: Publication type - Journal article
#####################################
# Select the subgroup in the model 
mod_prop_recid_pub_type_ja <- rma(yi=yi, vi=vi, method="REML", subset=(source== "Journal Article"), data = prop_recid_analysis)

# Create an object for the desired information  
mod_prop_recid_pub_type_ja_list <- c('Publication type', 'methodological', 'Journal article', mod_prop_recid_pub_type_ja$k, mod_prop_recid_pub_type_ja$beta[1], mod_prop_recid_pub_type_ja$se[1], mod_prop_recid_pub_type_ja$zval[1], mod_prop_recid_pub_type_ja$pval[1], 
                                       mod_prop_recid_pub_type_ja$ci.lb[1], mod_prop_recid_pub_type_ja$ci.ub[1], mod_prop_recid_pub_type_ja$k- length(mod_prop_recid_pub_type_ja$b), mod_prop_recid_pub_type_ja$QE, mod_prop_recid_pub_type_ja$QEp, 
                                       mod_prop_recid_pub_type_ja$I2)
# name the object's columns and bind to prop_moderator_output
names(mod_prop_recid_pub_type_ja_list) <- c("name", "mod_category", "level", "level_k", "est_LOR", "est_SE","est_z","est_z_p","est_cl","est_cu", "res_DF", "Qres", "Qres_p", "I2")
prop_moderator_output <- bind_rows(prop_moderator_output, mod_prop_recid_pub_type_ja_list)

########################################
# 4.37: Publication status
#####################################
table(prop_recid_analysis$source_published, prop_recid_analysis$studyid)
# Note: this variable will not be included in the proportion who recidivated moderator/subgroup analysis because it separates outcomes into the identical two groups as '4.37 Publication type'.
########################################
# 4.38: Stage of disposition
#####################################
table(prop_recid_analysis$stage_of_disposition, prop_recid_analysis$studyid)
prop_recid_analysis$stage_of_disposition_recode <- replace_na(prop_recid_analysis$stage_of_disposition, "NA")
########################################
# 4.38.1: Stage of disposition - Post-plea
#####################################
# Select the subgroup in the model 
mod_prop_recid_stage_dispo_post <- rma(yi=yi, vi=vi, method="REML", subset=(stage_of_disposition_recode== "Post"), data = prop_recid_analysis, verbose=TRUE, digits=5, control=list(stepadj=0.5))

# Create an object for the desired information  
mod_prop_recid_stage_dispo_post_list <- c('Stage of disposition', 'programmatic', 'Post-plea (ref)', mod_prop_recid_stage_dispo_post$k, mod_prop_recid_stage_dispo_post$beta[1], mod_prop_recid_stage_dispo_post$se[1], mod_prop_recid_stage_dispo_post$zval[1], mod_prop_recid_stage_dispo_post$pval[1], 
                                       mod_prop_recid_stage_dispo_post$ci.lb[1], mod_prop_recid_stage_dispo_post$ci.ub[1], mod_prop_recid_stage_dispo_post$k- length(mod_prop_recid_stage_dispo_post$b), mod_prop_recid_stage_dispo_post$QE, mod_prop_recid_stage_dispo_post$QEp, 
                                       mod_prop_recid_stage_dispo_post$I2)
# name the object's columns and bind to prop_moderator_output
names(mod_prop_recid_stage_dispo_post_list) <- c("name", "mod_category", "level", "level_k", "est_LOR", "est_SE","est_z","est_z_p","est_cl","est_cu", "res_DF", "Qres", "Qres_p", "I2")
prop_moderator_output <- bind_rows(prop_moderator_output, mod_prop_recid_stage_dispo_post_list)

########################################
# 4.38.2: Stage of disposition - Pre-plea
#####################################
# Select the subgroup in the model 
mod_prop_recid_stage_dispo_pre <- rma(yi=yi, vi=vi, method="REML", subset=(stage_of_disposition_recode== "Pre"), data = prop_recid_analysis)

# Create an object for the desired information  
mod_prop_recid_stage_dispo_pre_list <- c('Stage of disposition', 'programmatic', 'Pre-plea', mod_prop_recid_stage_dispo_pre$k, mod_prop_recid_stage_dispo_pre$beta[1], mod_prop_recid_stage_dispo_pre$se[1], mod_prop_recid_stage_dispo_pre$zval[1], mod_prop_recid_stage_dispo_pre$pval[1], 
                                          mod_prop_recid_stage_dispo_pre$ci.lb[1], mod_prop_recid_stage_dispo_pre$ci.ub[1], mod_prop_recid_stage_dispo_pre$k- length(mod_prop_recid_stage_dispo_pre$b), mod_prop_recid_stage_dispo_pre$QE, mod_prop_recid_stage_dispo_pre$QEp, 
                                          mod_prop_recid_stage_dispo_pre$I2)
# name the object's columns and bind to prop_moderator_output
names(mod_prop_recid_stage_dispo_pre_list) <- c("name", "mod_category", "level", "level_k", "est_LOR", "est_SE","est_z","est_z_p","est_cl","est_cu", "res_DF", "Qres", "Qres_p", "I2")
prop_moderator_output <- bind_rows(prop_moderator_output, mod_prop_recid_stage_dispo_pre_list)
########################################
# 4.39: Reported a single judicial officer presiding over hearings
#####################################
table(prop_recid_analysis$reported_single_JO, prop_recid_analysis$studyid)
########################################
# 4.39.1: Reported a single judicial officer presiding over hearings - No
#####################################
table(prop_recid_analysis$reported_single_JO, prop_recid_analysis$studyid)
# Select the subgroup in the model 
mod_prop_recid_rep_sing_jo_no <- rma(yi=yi, vi=vi, method="REML", subset=(reported_single_JO == "No"), data = prop_recid_analysis,digits=5, control=list(stepadj=0.5))

# Create an object for the desired information  
mod_prop_recid_rep_sing_jo_no_list <- c('Reported single judicial officer', 'programmatic', 'No (ref)', mod_prop_recid_rep_sing_jo_no$k, mod_prop_recid_rep_sing_jo_no$beta[1], mod_prop_recid_rep_sing_jo_no$se[1], mod_prop_recid_rep_sing_jo_no$zval[1], mod_prop_recid_rep_sing_jo_no$pval[1], 
                                           mod_prop_recid_rep_sing_jo_no$ci.lb[1], mod_prop_recid_rep_sing_jo_no$ci.ub[1], mod_prop_recid_rep_sing_jo_no$k- length(mod_prop_recid_rep_sing_jo_no$b), mod_prop_recid_rep_sing_jo_no$QE, mod_prop_recid_rep_sing_jo_no$QEp, 
                                           mod_prop_recid_rep_sing_jo_no$I2)
# name the object's columns and bind to prop_moderator_output
names(mod_prop_recid_rep_sing_jo_no_list) <- c("name", "mod_category", "level", "level_k", "est_LOR", "est_SE","est_z","est_z_p","est_cl","est_cu", "res_DF", "Qres", "Qres_p", "I2")
prop_moderator_output <- bind_rows(prop_moderator_output, mod_prop_recid_rep_sing_jo_no_list)
########################################
# 4.39.2: Reported a single judicial officer presiding over hearings - Yes
#####################################
table(prop_recid_analysis$reported_single_JO, prop_recid_analysis$studyid)
# Select the subgroup in the model 
mod_prop_recid_rep_sing_jo_yes <- rma(yi=yi, vi=vi, method="REML", subset=(reported_single_JO == "Yes"), data = prop_recid_analysis)

# Create an object for the desired information  
mod_prop_recid_rep_sing_jo_yes_list <- c('Reported single judicial officer', 'programmatic', 'Yes', mod_prop_recid_rep_sing_jo_yes$k, mod_prop_recid_rep_sing_jo_yes$beta[1], mod_prop_recid_rep_sing_jo_yes$se[1], mod_prop_recid_rep_sing_jo_yes$zval[1], mod_prop_recid_rep_sing_jo_yes$pval[1], 
                                        mod_prop_recid_rep_sing_jo_yes$ci.lb[1], mod_prop_recid_rep_sing_jo_yes$ci.ub[1], mod_prop_recid_rep_sing_jo_yes$k- length(mod_prop_recid_rep_sing_jo_yes$b), mod_prop_recid_rep_sing_jo_yes$QE, mod_prop_recid_rep_sing_jo_yes$QEp, 
                                        mod_prop_recid_rep_sing_jo_yes$I2)
# name the object's columns and bind to prop_moderator_output
names(mod_prop_recid_rep_sing_jo_yes_list) <- c("name", "mod_category", "level", "level_k", "est_LOR", "est_SE","est_z","est_z_p","est_cl","est_cu", "res_DF", "Qres", "Qres_p", "I2")
prop_moderator_output <- bind_rows(prop_moderator_output, mod_prop_recid_rep_sing_jo_yes_list)
########################################
# 4.40: Number of judicial officers presiding over hearings
#####################################
table(prop_recid_analysis$JOs_presiding_over_hearings_collapsed, prop_recid_analysis$studyid)
# Note: this variable will not be included in the proportion who recidivated moderator/subgroup analysis because it separates outcomes into the identical two groups as '4.42 Reported a single judicial officer presiding over hearings'.
########################################
# 4.41: Reported team training
#####################################
table(prop_recid_analysis$reported_team_training, prop_recid_analysis$studyid)
########################################
# 4.41.1: Reported team training - No
#####################################
# Select the subgroup in the model 
mod_prop_recid_rep_team_train_no <- rma(yi=yi, vi=vi, method="REML", subset=(reported_team_training == "No"), data = prop_recid_analysis, control=list(stepadj=0.5))

# Create an object for the desired information  
mod_prop_recid_rep_team_train_no_list <- c('Reported team training', 'programmatic', 'No (ref)', mod_prop_recid_rep_team_train_no$k, mod_prop_recid_rep_team_train_no$beta[1], mod_prop_recid_rep_team_train_no$se[1], mod_prop_recid_rep_team_train_no$zval[1], mod_prop_recid_rep_team_train_no$pval[1], 
                                        mod_prop_recid_rep_team_train_no$ci.lb[1], mod_prop_recid_rep_team_train_no$ci.ub[1], mod_prop_recid_rep_team_train_no$k- length(mod_prop_recid_rep_team_train_no$b), mod_prop_recid_rep_team_train_no$QE, mod_prop_recid_rep_team_train_no$QEp, 
                                        mod_prop_recid_rep_team_train_no$I2)
# name the object's columns and bind to prop_moderator_output
names(mod_prop_recid_rep_team_train_no_list) <- c("name", "mod_category", "level", "level_k", "est_LOR", "est_SE","est_z","est_z_p","est_cl","est_cu", "res_DF", "Qres", "Qres_p", "I2")
prop_moderator_output <- bind_rows(prop_moderator_output, mod_prop_recid_rep_team_train_no_list)
########################################
# 4.41.2: Reported team training - Yes
#####################################
# Select the subgroup in the model 
mod_prop_recid_rep_team_train_yes <- rma(yi=yi, vi=vi, method="REML", subset=(reported_team_training == "Yes"), data = prop_recid_analysis)

# Create an object for the desired information  
mod_prop_recid_rep_team_train_yes_list <- c('Reported team training', 'programmatic', 'Yes', mod_prop_recid_rep_team_train_yes$k, mod_prop_recid_rep_team_train_yes$beta[1], mod_prop_recid_rep_team_train_yes$se[1], mod_prop_recid_rep_team_train_yes$zval[1], mod_prop_recid_rep_team_train_yes$pval[1], 
                                           mod_prop_recid_rep_team_train_yes$ci.lb[1], mod_prop_recid_rep_team_train_yes$ci.ub[1], mod_prop_recid_rep_team_train_yes$k- length(mod_prop_recid_rep_team_train_yes$b), mod_prop_recid_rep_team_train_yes$QE, mod_prop_recid_rep_team_train_yes$QEp, 
                                           mod_prop_recid_rep_team_train_yes$I2)
# name the object's columns and bind to prop_moderator_output
names(mod_prop_recid_rep_team_train_yes_list) <- c("name", "mod_category", "level", "level_k", "est_LOR", "est_SE","est_z","est_z_p","est_cl","est_cu", "res_DF", "Qres", "Qres_p", "I2")
prop_moderator_output <- bind_rows(prop_moderator_output, mod_prop_recid_rep_team_train_yes_list)
########################################
# 4.42: Reported judicial officer training
#####################################
table(prop_recid_analysis$reported_JO.training, prop_recid_analysis$studyid)
# Note: there are insufficient outcomes that reported on this variable - the number of parameters to be estimated is larger than the number of observations
########################################
# 4.43: Reported judicial officer or team training
#####################################
table(prop_recid_analysis$reported_judge_or_team_training, prop_recid_analysis$studyid)
# Note: this variable will not be included in the proportion who recidivated moderator/subgroup analysis because it separates outcomes into the identical two groups as '4.43: Reported team training'.
########################################
# 4.44: Minutes per review hearing (average)
#####################################
table(prop_recid_analysis$average_minutes_review_hearing, prop_recid_analysis$studyid)
# Note: there are insufficient outcomes that reported on this variable to compute a moderator/subgroup analysis
########################################
# 4.45: Reported frequent team meetings
#####################################
table(prop_recid_analysis$reported_frequent_team_meetings, prop_recid_analysis$studyid)
########################################
# 4.45.1: Reported frequent team meetings - No 
#####################################
# Select the subgroup in the model 
mod_prop_recid_rep_team_meet_no <- rma(yi=yi, vi=vi, method="REML", subset=(reported_frequent_team_meetings == "No"), data = prop_recid_analysis)

# Create an object for the desired information  
mod_prop_recid_rep_team_meet_no_list <- c('Reported frequent team meetings', 'programmatic', 'No (ref)', mod_prop_recid_rep_team_meet_no$k, mod_prop_recid_rep_team_meet_no$beta[1], mod_prop_recid_rep_team_meet_no$se[1], mod_prop_recid_rep_team_meet_no$zval[1], mod_prop_recid_rep_team_meet_no$pval[1], 
                                           mod_prop_recid_rep_team_meet_no$ci.lb[1], mod_prop_recid_rep_team_meet_no$ci.ub[1], mod_prop_recid_rep_team_meet_no$k- length(mod_prop_recid_rep_team_meet_no$b), mod_prop_recid_rep_team_meet_no$QE, mod_prop_recid_rep_team_meet_no$QEp, 
                                           mod_prop_recid_rep_team_meet_no$I2)
# name the object's columns and bind to prop_moderator_output
names(mod_prop_recid_rep_team_meet_no_list) <- c("name", "mod_category", "level", "level_k", "est_LOR", "est_SE","est_z","est_z_p","est_cl","est_cu", "res_DF", "Qres", "Qres_p", "I2")
prop_moderator_output <- bind_rows(prop_moderator_output, mod_prop_recid_rep_team_meet_no_list)
########################################
# 4.45.2: Reported frequent team meetings - Yes
#####################################
# Select the subgroup in the model 
mod_prop_recid_rep_team_meet_yes <- rma(yi=yi, vi=vi, method="REML", subset=(reported_frequent_team_meetings == "Yes"), data = prop_recid_analysis)

# Create an object for the desired information  
mod_prop_recid_rep_team_meet_yes_list <- c('Reported frequent team meetings', 'programmatic', 'Yes', mod_prop_recid_rep_team_meet_yes$k, mod_prop_recid_rep_team_meet_yes$beta[1], mod_prop_recid_rep_team_meet_yes$se[1], mod_prop_recid_rep_team_meet_yes$zval[1], mod_prop_recid_rep_team_meet_yes$pval[1], 
                                          mod_prop_recid_rep_team_meet_yes$ci.lb[1], mod_prop_recid_rep_team_meet_yes$ci.ub[1], mod_prop_recid_rep_team_meet_yes$k- length(mod_prop_recid_rep_team_meet_yes$b), mod_prop_recid_rep_team_meet_yes$QE, mod_prop_recid_rep_team_meet_yes$QEp, 
                                          mod_prop_recid_rep_team_meet_yes$I2)
# name the object's columns and bind to prop_moderator_output
names(mod_prop_recid_rep_team_meet_yes_list) <- c("name", "mod_category", "level", "level_k", "est_LOR", "est_SE","est_z","est_z_p","est_cl","est_cu", "res_DF", "Qres", "Qres_p", "I2")
prop_moderator_output <- bind_rows(prop_moderator_output, mod_prop_recid_rep_team_meet_yes_list)


########################################
# 4.46: Reported using sanctions or graduated sanctions
#####################################
table(prop_recid_analysis$reported_sanctions_or_graduated_sanctions, prop_recid_analysis$studyid)
########################################
# 4.46.1: Reported using sanctions or graduated sanctions - No
#####################################
# Select the subgroup in the model 
mod_prop_recid_rep_sancts_or_grad_no<- rma(yi=yi, vi=vi, method="REML", subset=(reported_sanctions_or_graduated_sanctions == "No"), data = prop_recid_analysis)

# Create an object for the desired information  
mod_prop_recid_rep_sancts_or_grad_no_list <- c('Reported using sanctions or graduated sanctions', 'programmatic', 'No (ref)', mod_prop_recid_rep_sancts_or_grad_no$k, mod_prop_recid_rep_sancts_or_grad_no$beta[1], mod_prop_recid_rep_sancts_or_grad_no$se[1], mod_prop_recid_rep_sancts_or_grad_no$zval[1], mod_prop_recid_rep_sancts_or_grad_no$pval[1], 
                                               mod_prop_recid_rep_sancts_or_grad_no$ci.lb[1], mod_prop_recid_rep_sancts_or_grad_no$ci.ub[1], mod_prop_recid_rep_sancts_or_grad_no$k- length(mod_prop_recid_rep_sancts_or_grad_no$b), mod_prop_recid_rep_sancts_or_grad_no$QE, mod_prop_recid_rep_sancts_or_grad_no$QEp, 
                                               mod_prop_recid_rep_sancts_or_grad_no$I2)
# name the object's columns and bind to prop_moderator_output
names(mod_prop_recid_rep_sancts_or_grad_no_list) <- c("name", "mod_category", "level", "level_k", "est_LOR", "est_SE","est_z","est_z_p","est_cl","est_cu", "res_DF", "Qres", "Qres_p", "I2")
prop_moderator_output <- bind_rows(prop_moderator_output, mod_prop_recid_rep_sancts_or_grad_no_list)
########################################
# 4.46.2: Reported using sanctions or graduated sanctions - Graduated
#####################################
# Select the subgroup in the model 
mod_prop_recid_rep_sancts_or_grad_grad<- rma(yi=yi, vi=vi, method="REML", subset=(reported_sanctions_or_graduated_sanctions == "Graduated"), data = prop_recid_analysis)

# Create an object for the desired information  
mod_prop_recid_rep_sancts_or_grad_grad_list <- c('Reported using sanctions or graduated sanctions', 'programmatic', 'Graduated', mod_prop_recid_rep_sancts_or_grad_grad$k, mod_prop_recid_rep_sancts_or_grad_grad$beta[1], mod_prop_recid_rep_sancts_or_grad_grad$se[1], mod_prop_recid_rep_sancts_or_grad_grad$zval[1], mod_prop_recid_rep_sancts_or_grad_grad$pval[1], 
                                            mod_prop_recid_rep_sancts_or_grad_grad$ci.lb[1], mod_prop_recid_rep_sancts_or_grad_grad$ci.ub[1], mod_prop_recid_rep_sancts_or_grad_grad$k- length(mod_prop_recid_rep_sancts_or_grad_grad$b), mod_prop_recid_rep_sancts_or_grad_grad$QE, mod_prop_recid_rep_sancts_or_grad_grad$QEp, 
                                            mod_prop_recid_rep_sancts_or_grad_grad$I2)
# name the object's columns and bind to prop_moderator_output
names(mod_prop_recid_rep_sancts_or_grad_grad_list) <- c("name", "mod_category", "level", "level_k", "est_LOR", "est_SE","est_z","est_z_p","est_cl","est_cu", "res_DF", "Qres", "Qres_p", "I2")
prop_moderator_output <- bind_rows(prop_moderator_output, mod_prop_recid_rep_sancts_or_grad_grad_list)
########################################
# 4.46.3: Reported using sanctions or graduated sanctions - Sanctions
#####################################
# Select the subgroup in the model 
mod_prop_recid_rep_sancts_or_grad_sancts<- rma(yi=yi, vi=vi, method="REML", subset=(reported_sanctions_or_graduated_sanctions == "sanctions"), data = prop_recid_analysis)

# Create an object for the desired information  
mod_prop_recid_rep_sancts_or_grad_sancts_list <- c('Reported using sanctions or graduated sanctions', 'programmatic', 'Sanctions', mod_prop_recid_rep_sancts_or_grad_sancts$k, mod_prop_recid_rep_sancts_or_grad_sancts$beta[1], mod_prop_recid_rep_sancts_or_grad_sancts$se[1], mod_prop_recid_rep_sancts_or_grad_sancts$zval[1], mod_prop_recid_rep_sancts_or_grad_sancts$pval[1], 
                                                 mod_prop_recid_rep_sancts_or_grad_sancts$ci.lb[1], mod_prop_recid_rep_sancts_or_grad_sancts$ci.ub[1], mod_prop_recid_rep_sancts_or_grad_sancts$k- length(mod_prop_recid_rep_sancts_or_grad_sancts$b), mod_prop_recid_rep_sancts_or_grad_sancts$QE, mod_prop_recid_rep_sancts_or_grad_sancts$QEp, 
                                                 mod_prop_recid_rep_sancts_or_grad_sancts$I2)
# name the object's columns and bind to prop_moderator_output
names(mod_prop_recid_rep_sancts_or_grad_sancts_list) <- c("name", "mod_category", "level", "level_k", "est_LOR", "est_SE","est_z","est_z_p","est_cl","est_cu", "res_DF", "Qres", "Qres_p", "I2")
prop_moderator_output <- bind_rows(prop_moderator_output, mod_prop_recid_rep_sancts_or_grad_sancts_list)
########################################
# 4.46.4: Reported using sanctions or graduated sanctions - Sanctions or graduated sanctions
#####################################
# Select the subgroup in the model 
mod_prop_recid_rep_sancts_or_grad_or<- rma(yi=yi, vi=vi, method="REML", subset=(!reported_sanctions_or_graduated_sanctions == "No"), data = prop_recid_analysis)

# Create an object for the desired information  
mod_prop_recid_rep_sancts_or_grad_or_list <- c('Reported using sanctions or graduated sanctions', 'programmatic', 'Sanctions or graduated sanctions', mod_prop_recid_rep_sancts_or_grad_or$k, mod_prop_recid_rep_sancts_or_grad_or$beta[1], mod_prop_recid_rep_sancts_or_grad_or$se[1], mod_prop_recid_rep_sancts_or_grad_or$zval[1], mod_prop_recid_rep_sancts_or_grad_or$pval[1], 
                                                   mod_prop_recid_rep_sancts_or_grad_or$ci.lb[1], mod_prop_recid_rep_sancts_or_grad_or$ci.ub[1], mod_prop_recid_rep_sancts_or_grad_or$k- length(mod_prop_recid_rep_sancts_or_grad_or$b), mod_prop_recid_rep_sancts_or_grad_or$QE, mod_prop_recid_rep_sancts_or_grad_or$QEp, 
                                                   mod_prop_recid_rep_sancts_or_grad_or$I2)
# name the object's columns and bind to prop_moderator_output
names(mod_prop_recid_rep_sancts_or_grad_or_list) <- c("name", "mod_category", "level", "level_k", "est_LOR", "est_SE","est_z","est_z_p","est_cl","est_cu", "res_DF", "Qres", "Qres_p", "I2")
prop_moderator_output <- bind_rows(prop_moderator_output, mod_prop_recid_rep_sancts_or_grad_or_list)
########################################
# 4.47: Reported using rewards
#####################################
table(prop_recid_analysis$reported_using_rewards, prop_recid_analysis$studyid)
########################################
# 4.47.1: Reported using rewards - No
#####################################
# Select the subgroup in the model 
mod_prop_recid_rep_rewards_no<- rma(yi=yi, vi=vi, method="REML", subset=(reported_using_rewards == "No"), data = prop_recid_analysis)

# Create an object for the desired information  
mod_prop_recid_rep_rewards_no_list <- c('Reported using rewards', 'programmatic', 'No (ref)', mod_prop_recid_rep_rewards_no$k, mod_prop_recid_rep_rewards_no$beta[1], mod_prop_recid_rep_rewards_no$se[1], mod_prop_recid_rep_rewards_no$zval[1], mod_prop_recid_rep_rewards_no$pval[1], 
                                               mod_prop_recid_rep_rewards_no$ci.lb[1], mod_prop_recid_rep_rewards_no$ci.ub[1], mod_prop_recid_rep_rewards_no$k- length(mod_prop_recid_rep_rewards_no$b), mod_prop_recid_rep_rewards_no$QE, mod_prop_recid_rep_rewards_no$QEp, 
                                               mod_prop_recid_rep_rewards_no$I2)
# name the object's columns and bind to prop_moderator_output
names(mod_prop_recid_rep_rewards_no_list) <- c("name", "mod_category", "level", "level_k", "est_LOR", "est_SE","est_z","est_z_p","est_cl","est_cu", "res_DF", "Qres", "Qres_p", "I2")
prop_moderator_output <- bind_rows(prop_moderator_output, mod_prop_recid_rep_rewards_no_list)

########################################
# 4.47.2: Reported using rewards - Yes
#####################################
# Select the subgroup in the model 
mod_prop_recid_rep_rewards_yes<- rma(yi=yi, vi=vi, method="REML", subset=(reported_using_rewards == "Yes"), data = prop_recid_analysis)

# Create an object for the desired information  
mod_prop_recid_rep_rewards_yes_list <- c('Reported using rewards', 'programmatic', 'Yes', mod_prop_recid_rep_rewards_yes$k, mod_prop_recid_rep_rewards_yes$beta[1], mod_prop_recid_rep_rewards_yes$se[1], mod_prop_recid_rep_rewards_yes$zval[1], mod_prop_recid_rep_rewards_yes$pval[1], 
                                        mod_prop_recid_rep_rewards_yes$ci.lb[1], mod_prop_recid_rep_rewards_yes$ci.ub[1], mod_prop_recid_rep_rewards_yes$k- length(mod_prop_recid_rep_rewards_yes$b), mod_prop_recid_rep_rewards_yes$QE, mod_prop_recid_rep_rewards_yes$QEp, 
                                        mod_prop_recid_rep_rewards_yes$I2)
# name the object's columns and bind to prop_moderator_output
names(mod_prop_recid_rep_rewards_yes_list) <- c("name", "mod_category", "level", "level_k", "est_LOR", "est_SE","est_z","est_z_p","est_cl","est_cu", "res_DF", "Qres", "Qres_p", "I2")
prop_moderator_output <- bind_rows(prop_moderator_output, mod_prop_recid_rep_rewards_yes_list)
########################################
# 4.48: Reported using sanctions and rewards
#####################################
table(prop_recid_analysis$reported_sanctions_and_rewards, prop_recid_analysis$studyid)
# Note: this variable will not be included in the proportion who recidivated moderator/subgroup analysis because it separates outcomes into the identical two groups as '4.51: Reported using rewards'.
########################################
# 4.49: Frequency of review hearings
#####################################
table(prop_recid_analysis$frequency_review_hearing, prop_recid_analysis$studyid)
# Note: there are insufficient numbers across groups to compute a moderator/subgroup analysis
########################################
# 4.50: Frequent vs monthly review hearings
#####################################
table(prop_recid_analysis$frequent_vs_monthly_rh, prop_recid_analysis$studyid)
# Note: there are insufficient numbers across groups to compute a moderator/subgroup analysis
########################################
# 4.51: Reported frequent review hearings 
#####################################
table(prop_recid_analysis$reported_frequent_rh, prop_recid_analysis$studyid)
########################################
# 4.51.1: Reported frequent review hearings - No
#####################################
# Select the subgroup in the model 
mod_prop_recid_rep_freq_rh_no <- rma(yi=yi, vi=vi, method="REML", subset=(reported_frequent_rh == "No"), data = prop_recid_analysis)

# Create an object for the desired information  
mod_prop_recid_rep_freq_rh_no_list <- c('Reported frequent review hearings', 'programmatic', 'No (ref)', mod_prop_recid_rep_freq_rh_no$k, mod_prop_recid_rep_freq_rh_no$beta[1], mod_prop_recid_rep_freq_rh_no$se[1], mod_prop_recid_rep_freq_rh_no$zval[1], mod_prop_recid_rep_freq_rh_no$pval[1], 
                                        mod_prop_recid_rep_freq_rh_no$ci.lb[1], mod_prop_recid_rep_freq_rh_no$ci.ub[1], mod_prop_recid_rep_freq_rh_no$k- length(mod_prop_recid_rep_freq_rh_no$b), mod_prop_recid_rep_freq_rh_no$QE, mod_prop_recid_rep_freq_rh_no$QEp, 
                                        mod_prop_recid_rep_freq_rh_no$I2)
# name the object's columns and bind to prop_moderator_output
names(mod_prop_recid_rep_freq_rh_no_list) <- c("name", "mod_category", "level", "level_k", "est_LOR", "est_SE","est_z","est_z_p","est_cl","est_cu", "res_DF", "Qres", "Qres_p", "I2")
prop_moderator_output <- bind_rows(prop_moderator_output, mod_prop_recid_rep_freq_rh_no_list)
########################################
# 4.51.2: Reported frequent review hearings - Yes
#####################################
# Select the subgroup in the model 
mod_prop_recid_rep_freq_rh_yes <- rma(yi=yi, vi=vi, method="REML", subset=(reported_frequent_rh == "Yes"), data = prop_recid_analysis)

# Create an object for the desired information  
mod_prop_recid_rep_freq_rh_yes_list <- c('Reported frequent review hearings', 'programmatic', 'Yes', mod_prop_recid_rep_freq_rh_yes$k, mod_prop_recid_rep_freq_rh_yes$beta[1], mod_prop_recid_rep_freq_rh_yes$se[1], mod_prop_recid_rep_freq_rh_yes$zval[1], mod_prop_recid_rep_freq_rh_yes$pval[1], 
                                        mod_prop_recid_rep_freq_rh_yes$ci.lb[1], mod_prop_recid_rep_freq_rh_yes$ci.ub[1], mod_prop_recid_rep_freq_rh_yes$k- length(mod_prop_recid_rep_freq_rh_yes$b), mod_prop_recid_rep_freq_rh_yes$QE, mod_prop_recid_rep_freq_rh_yes$QEp, 
                                        mod_prop_recid_rep_freq_rh_yes$I2)
# name the object's columns and bind to prop_moderator_output
names(mod_prop_recid_rep_freq_rh_yes_list) <- c("name", "mod_category", "level", "level_k", "est_LOR", "est_SE","est_z","est_z_p","est_cl","est_cu", "res_DF", "Qres", "Qres_p", "I2")
prop_moderator_output <- bind_rows(prop_moderator_output, mod_prop_recid_rep_freq_rh_yes_list)

########################################
# 4.52: Reported measuring risk - Treatment
#####################################
table(prop_recid_analysis$reported_measuring_risk_treat, prop_recid_analysis$studyid)
########################################
# 4.52.1: Reported measuring risk - Treatment - No
#####################################
# Select the subgroup in the model 
mod_prop_recid_rep_meas_risk_tr_no <- rma(yi=yi, vi=vi, method="REML", subset=(reported_measuring_risk_treat == "No"), data = prop_recid_analysis)

# Create an object for the desired information  
mod_prop_recid_rep_meas_risk_tr_no_list <- c('Reported measuring risk (Tr)', 'treatment', 'No (ref)', mod_prop_recid_rep_meas_risk_tr_no$k, mod_prop_recid_rep_meas_risk_tr_no$beta[1], mod_prop_recid_rep_meas_risk_tr_no$se[1], mod_prop_recid_rep_meas_risk_tr_no$zval[1], mod_prop_recid_rep_meas_risk_tr_no$pval[1], 
                                        mod_prop_recid_rep_meas_risk_tr_no$ci.lb[1], mod_prop_recid_rep_meas_risk_tr_no$ci.ub[1], mod_prop_recid_rep_meas_risk_tr_no$k- length(mod_prop_recid_rep_meas_risk_tr_no$b), mod_prop_recid_rep_meas_risk_tr_no$QE, mod_prop_recid_rep_meas_risk_tr_no$QEp, 
                                        mod_prop_recid_rep_meas_risk_tr_no$I2)
# name the object's columns and bind to prop_moderator_output
names(mod_prop_recid_rep_meas_risk_tr_no_list) <- c("name", "mod_category", "level", "level_k", "est_LOR", "est_SE","est_z","est_z_p","est_cl","est_cu", "res_DF", "Qres", "Qres_p", "I2")
prop_moderator_output <- bind_rows(prop_moderator_output, mod_prop_recid_rep_meas_risk_tr_no_list)
########################################
# 4.52.2: Reported measuring risk - Treatment - Yes
#####################################
# Select the subgroup in the model 
mod_prop_recid_rep_meas_risk_tr_yes <- rma(yi=yi, vi=vi, method="REML", subset=(reported_measuring_risk_treat == "Yes"), data = prop_recid_analysis)

# Create an object for the desired information  
mod_prop_recid_rep_meas_risk_tr_yes_list <- c('Reported measuring risk (Tr)', 'treatment', 'Yes', mod_prop_recid_rep_meas_risk_tr_yes$k, mod_prop_recid_rep_meas_risk_tr_yes$beta[1], mod_prop_recid_rep_meas_risk_tr_yes$se[1], mod_prop_recid_rep_meas_risk_tr_yes$zval[1], mod_prop_recid_rep_meas_risk_tr_yes$pval[1], 
                                             mod_prop_recid_rep_meas_risk_tr_yes$ci.lb[1], mod_prop_recid_rep_meas_risk_tr_yes$ci.ub[1], mod_prop_recid_rep_meas_risk_tr_yes$k- length(mod_prop_recid_rep_meas_risk_tr_yes$b), mod_prop_recid_rep_meas_risk_tr_yes$QE, mod_prop_recid_rep_meas_risk_tr_yes$QEp, 
                                             mod_prop_recid_rep_meas_risk_tr_yes$I2)
# name the object's columns and bind to prop_moderator_output
names(mod_prop_recid_rep_meas_risk_tr_yes_list) <- c("name", "mod_category", "level", "level_k", "est_LOR", "est_SE","est_z","est_z_p","est_cl","est_cu", "res_DF", "Qres", "Qres_p", "I2")
prop_moderator_output <- bind_rows(prop_moderator_output, mod_prop_recid_rep_meas_risk_tr_yes_list)
########################################
# 4.53: Reported measuring risk - Control
#####################################
table(prop_recid_analysis$reported_measuring_risk_cont, prop_recid_analysis$studyid)
########################################
# 4.53.1: Reported measuring risk - Control - No
#####################################
# Select the subgroup in the model 
mod_prop_recid_rep_meas_risk_con_no <- rma(yi=yi, vi=vi, method="REML", subset=(reported_measuring_risk_cont == "No"), data = prop_recid_analysis, digits=5, control=list(stepadj=0.5))

# Create an object for the desired information  
mod_prop_recid_rep_meas_risk_con_no_list <- c('Reported measuring risk (Con)', 'treatment', 'No (ref)', mod_prop_recid_rep_meas_risk_con_no$k, mod_prop_recid_rep_meas_risk_con_no$beta[1], mod_prop_recid_rep_meas_risk_con_no$se[1], mod_prop_recid_rep_meas_risk_con_no$zval[1], mod_prop_recid_rep_meas_risk_con_no$pval[1], 
                                             mod_prop_recid_rep_meas_risk_con_no$ci.lb[1], mod_prop_recid_rep_meas_risk_con_no$ci.ub[1], mod_prop_recid_rep_meas_risk_con_no$k- length(mod_prop_recid_rep_meas_risk_con_no$b), mod_prop_recid_rep_meas_risk_con_no$QE, mod_prop_recid_rep_meas_risk_con_no$QEp, 
                                             mod_prop_recid_rep_meas_risk_con_no$I2)
# name the object's columns and bind to prop_moderator_output
names(mod_prop_recid_rep_meas_risk_con_no_list) <- c("name", "mod_category", "level", "level_k", "est_LOR", "est_SE","est_z","est_z_p","est_cl","est_cu", "res_DF", "Qres", "Qres_p", "I2")
prop_moderator_output <- bind_rows(prop_moderator_output, mod_prop_recid_rep_meas_risk_con_no_list)
########################################
# 4.53.2: Reported measuring risk - Control - Yes
#####################################
# Select the subgroup in the model 
mod_prop_recid_rep_meas_risk_con_yes <- rma(yi=yi, vi=vi, method="REML", subset=(reported_measuring_risk_cont == "Yes"), data = prop_recid_analysis)

# Create an object for the desired information  
mod_prop_recid_rep_meas_risk_con_yes_list <- c('Reported measuring risk (Con)', 'treatment', 'Yes', mod_prop_recid_rep_meas_risk_con_yes$k, mod_prop_recid_rep_meas_risk_con_yes$beta[1], mod_prop_recid_rep_meas_risk_con_yes$se[1], mod_prop_recid_rep_meas_risk_con_yes$zval[1], mod_prop_recid_rep_meas_risk_con_yes$pval[1], 
                                              mod_prop_recid_rep_meas_risk_con_yes$ci.lb[1], mod_prop_recid_rep_meas_risk_con_yes$ci.ub[1], mod_prop_recid_rep_meas_risk_con_yes$k- length(mod_prop_recid_rep_meas_risk_con_yes$b), mod_prop_recid_rep_meas_risk_con_yes$QE, mod_prop_recid_rep_meas_risk_con_yes$QEp, 
                                              mod_prop_recid_rep_meas_risk_con_yes$I2)
# name the object's columns and bind to prop_moderator_output
names(mod_prop_recid_rep_meas_risk_con_yes_list) <- c("name", "mod_category", "level", "level_k", "est_LOR", "est_SE","est_z","est_z_p","est_cl","est_cu", "res_DF", "Qres", "Qres_p", "I2")
prop_moderator_output <- bind_rows(prop_moderator_output, mod_prop_recid_rep_meas_risk_con_yes_list)
########################################
# 4.54: Reported individualising treatment - Treatment
#####################################
table(prop_recid_analysis$reported_individualized_treat_treat, prop_recid_analysis$studyid)
# Note: there are insufficient numbers across groups to compute a moderator/subgroup analysis
########################################
# 4.55: Reported individualising treatment - Control
#####################################
table(prop_recid_analysis$reported_individualized_treat_cont, prop_recid_analysis$studyid)
# Note: there are insufficient numbers across groups to compute a moderator/subgroup analysis
########################################
# 4.56: Reported drug or alcohol treatment - Treatment
#####################################
table(prop_recid_analysis$reported_drug_or_alcohol_treatment_treat, prop_recid_analysis$studyid)
########################################
# 4.56.1: Reported drug or alcohol treatment - Treatment - Offered
#####################################
# Select the subgroup in the model 
mod_prop_recid_rep_drug_or_alch_treat_tr_off <- rma(yi=yi, vi=vi, method="REML", subset=(reported_drug_or_alcohol_treatment_treat == "offered"), data = prop_recid_analysis)

# Create an object for the desired information  
mod_prop_recid_rep_drug_or_alch_treat_tr_off_list <- c('Reported drug or alcohol treatment (Tr)', 'treatment', 'Offered (ref)', mod_prop_recid_rep_drug_or_alch_treat_tr_off$k, mod_prop_recid_rep_drug_or_alch_treat_tr_off$beta[1], mod_prop_recid_rep_drug_or_alch_treat_tr_off$se[1], mod_prop_recid_rep_drug_or_alch_treat_tr_off$zval[1], mod_prop_recid_rep_drug_or_alch_treat_tr_off$pval[1], 
                                              mod_prop_recid_rep_drug_or_alch_treat_tr_off$ci.lb[1], mod_prop_recid_rep_drug_or_alch_treat_tr_off$ci.ub[1], mod_prop_recid_rep_drug_or_alch_treat_tr_off$k- length(mod_prop_recid_rep_drug_or_alch_treat_tr_off$b), mod_prop_recid_rep_drug_or_alch_treat_tr_off$QE, mod_prop_recid_rep_drug_or_alch_treat_tr_off$QEp, 
                                              mod_prop_recid_rep_drug_or_alch_treat_tr_off$I2)
# name the object's columns and bind to prop_moderator_output
names(mod_prop_recid_rep_drug_or_alch_treat_tr_off_list) <- c("name", "mod_category", "level", "level_k", "est_LOR", "est_SE","est_z","est_z_p","est_cl","est_cu", "res_DF", "Qres", "Qres_p", "I2")
prop_moderator_output <- bind_rows(prop_moderator_output, mod_prop_recid_rep_drug_or_alch_treat_tr_off_list)
########################################
# 4.56.2: Reported drug or alcohol treatment - Treatment - Required
#####################################
# Select the subgroup in the model 
mod_prop_recid_rep_drug_or_alch_treat_tr_req <- rma(yi=yi, vi=vi, method="REML", subset=(reported_drug_or_alcohol_treatment_treat == "required"), data = prop_recid_analysis)

# Create an object for the desired information  
mod_prop_recid_rep_drug_or_alch_treat_tr_req_list <- c('Reported drug or alcohol treatment (Tr)', 'treatment', 'Required', mod_prop_recid_rep_drug_or_alch_treat_tr_req$k, mod_prop_recid_rep_drug_or_alch_treat_tr_req$beta[1], mod_prop_recid_rep_drug_or_alch_treat_tr_req$se[1], mod_prop_recid_rep_drug_or_alch_treat_tr_req$zval[1], mod_prop_recid_rep_drug_or_alch_treat_tr_req$pval[1], 
                                                       mod_prop_recid_rep_drug_or_alch_treat_tr_req$ci.lb[1], mod_prop_recid_rep_drug_or_alch_treat_tr_req$ci.ub[1], mod_prop_recid_rep_drug_or_alch_treat_tr_req$k- length(mod_prop_recid_rep_drug_or_alch_treat_tr_req$b), mod_prop_recid_rep_drug_or_alch_treat_tr_req$QE, mod_prop_recid_rep_drug_or_alch_treat_tr_req$QEp, 
                                                       mod_prop_recid_rep_drug_or_alch_treat_tr_req$I2)
# name the object's columns and bind to prop_moderator_output
names(mod_prop_recid_rep_drug_or_alch_treat_tr_req_list) <- c("name", "mod_category", "level", "level_k", "est_LOR", "est_SE","est_z","est_z_p","est_cl","est_cu", "res_DF", "Qres", "Qres_p", "I2")
prop_moderator_output <- bind_rows(prop_moderator_output, mod_prop_recid_rep_drug_or_alch_treat_tr_req_list)
########################################
# 4.57: Type of drug or alcohol treatment - Treatment
#####################################
table(prop_recid_analysis$type_of_drug_or_alch_treat_treat, prop_recid_analysis$studyid)
# Note: there are insufficient numbers across groups to compute a moderator/subgroup analysis
########################################
# 4.58: Reported drug or alcohol treatment - Control
#####################################
table(prop_recid_analysis$reported_drug_or_alcohol_treatment_cont, prop_recid_analysis$studyid)
# Note: there are insufficient numbers across groups to compute a moderator/subgroup analysis
########################################
# 4.59: Type of drug or alcohol treatment - Control
#####################################
table(prop_recid_analysis$type_of_drug_or_alch_treat_cont, prop_recid_analysis$studyid)
# Note: there are insufficient numbers across groups to compute a moderator/subgroup analysis
########################################
# 4.60: Reported drug or alcohol testing - Treatment
#####################################
table(prop_recid_analysis$reported_drug_or_alcohol_testing_treat, prop_recid_analysis$studyid)
# Recode 'offered' and 'required' to 'yes' 
prop_recid_analysis$reported_drug_or_alcohol_testing_treat_recode <- recode(prop_recid_analysis$reported_drug_or_alcohol_testing_treat, 'offered'='Yes', 'required' ='Yes')
########################################
# 4.60.1: Reported drug or alcohol testing  - Treatment - No
#####################################
# Select the subgroup in the model 
mod_prop_recid_rep_drug_or_alch_test_tr_no <- rma(yi=yi, vi=vi, method="REML", subset=(reported_drug_or_alcohol_testing_treat_recode == "No"), data = prop_recid_analysis)

# Create an object for the desired information  
mod_prop_recid_rep_drug_or_alch_test_tr_no_list <- c('Reported drug or alcohol testing (Tr)', 'treatment', 'No (ref)', mod_prop_recid_rep_drug_or_alch_test_tr_no$k, mod_prop_recid_rep_drug_or_alch_test_tr_no$beta[1], mod_prop_recid_rep_drug_or_alch_test_tr_no$se[1], mod_prop_recid_rep_drug_or_alch_test_tr_no$zval[1], mod_prop_recid_rep_drug_or_alch_test_tr_no$pval[1], 
                                                       mod_prop_recid_rep_drug_or_alch_test_tr_no$ci.lb[1], mod_prop_recid_rep_drug_or_alch_test_tr_no$ci.ub[1], mod_prop_recid_rep_drug_or_alch_test_tr_no$k- length(mod_prop_recid_rep_drug_or_alch_test_tr_no$b), mod_prop_recid_rep_drug_or_alch_test_tr_no$QE, mod_prop_recid_rep_drug_or_alch_test_tr_no$QEp, 
                                                       mod_prop_recid_rep_drug_or_alch_test_tr_no$I2)
# name the object's columns and bind to prop_moderator_output
names(mod_prop_recid_rep_drug_or_alch_test_tr_no_list) <- c("name", "mod_category", "level", "level_k", "est_LOR", "est_SE","est_z","est_z_p","est_cl","est_cu", "res_DF", "Qres", "Qres_p", "I2")
prop_moderator_output <- bind_rows(prop_moderator_output, mod_prop_recid_rep_drug_or_alch_test_tr_no_list)
########################################
# 4.60.2: Reported drug or alcohol testing  - Treatment - Offered or required
#####################################
# Select the subgroup in the model 
mod_prop_recid_rep_drug_or_alch_test_tr_yes <- rma(yi=yi, vi=vi, method="REML", subset=(reported_drug_or_alcohol_testing_treat_recode == "Yes"), data = prop_recid_analysis)

# Create an object for the desired information  
mod_prop_recid_rep_drug_or_alch_test_tr_yes_list <- c('Reported drug or alcohol testing (Tr)', 'treatment', 'Offered or required', mod_prop_recid_rep_drug_or_alch_test_tr_yes$k, mod_prop_recid_rep_drug_or_alch_test_tr_yes$beta[1], mod_prop_recid_rep_drug_or_alch_test_tr_yes$se[1], mod_prop_recid_rep_drug_or_alch_test_tr_yes$zval[1], mod_prop_recid_rep_drug_or_alch_test_tr_yes$pval[1], 
                                                      mod_prop_recid_rep_drug_or_alch_test_tr_yes$ci.lb[1], mod_prop_recid_rep_drug_or_alch_test_tr_yes$ci.ub[1], mod_prop_recid_rep_drug_or_alch_test_tr_yes$k- length(mod_prop_recid_rep_drug_or_alch_test_tr_yes$b), mod_prop_recid_rep_drug_or_alch_test_tr_yes$QE, mod_prop_recid_rep_drug_or_alch_test_tr_yes$QEp, 
                                                      mod_prop_recid_rep_drug_or_alch_test_tr_yes$I2)
# name the object's columns and bind to prop_moderator_output
names(mod_prop_recid_rep_drug_or_alch_test_tr_yes_list) <- c("name", "mod_category", "level", "level_k", "est_LOR", "est_SE","est_z","est_z_p","est_cl","est_cu", "res_DF", "Qres", "Qres_p", "I2")
prop_moderator_output <- bind_rows(prop_moderator_output, mod_prop_recid_rep_drug_or_alch_test_tr_yes_list)
########################################
# 4.61: Reported drug or alcohol testing - Control
#####################################
table(prop_recid_analysis$reported_drug_or_alcohol_testing_cont, prop_recid_analysis$studyid)
# Note: there are insufficient numbers across groups to compute a moderator/subgroup analysis
########################################
# 4.62: Reported detox treatment - Treatment
#####################################
table(prop_recid_analysis$reported_detox_treat, prop_recid_analysis$studyid)
########################################
# 4.62.1: Reported detox treatment - Treatment - No
#####################################
# Select the subgroup in the model 
mod_prop_recid_rep_detox_tr_no <- rma(yi=yi, vi=vi, method="REML", subset=(reported_detox_treat == "No"), data = prop_recid_analysis)

# Create an object for the desired information  
mod_prop_recid_rep_detox_tr_no_list <- c('Reported detox treatment (Tr)', 'treatment', 'No (ref)', mod_prop_recid_rep_detox_tr_no$k, mod_prop_recid_rep_detox_tr_no$beta[1], mod_prop_recid_rep_detox_tr_no$se[1], mod_prop_recid_rep_detox_tr_no$zval[1], mod_prop_recid_rep_detox_tr_no$pval[1], 
                                                     mod_prop_recid_rep_detox_tr_no$ci.lb[1], mod_prop_recid_rep_detox_tr_no$ci.ub[1], mod_prop_recid_rep_detox_tr_no$k- length(mod_prop_recid_rep_detox_tr_no$b), mod_prop_recid_rep_detox_tr_no$QE, mod_prop_recid_rep_detox_tr_no$QEp, 
                                                     mod_prop_recid_rep_detox_tr_no$I2)
# name the object's columns and bind to prop_moderator_output
names(mod_prop_recid_rep_detox_tr_no_list) <- c("name", "mod_category", "level", "level_k", "est_LOR", "est_SE","est_z","est_z_p","est_cl","est_cu", "res_DF", "Qres", "Qres_p", "I2")
prop_moderator_output <- bind_rows(prop_moderator_output, mod_prop_recid_rep_detox_tr_no_list)
        
########################################
# 4.62.2: Reported detox treatment - Treatment - Offered
#####################################
# Select the subgroup in the model 
mod_prop_recid_rep_detox_tr_off <- rma(yi=yi, vi=vi, method="REML", subset=(reported_detox_treat == "offered"), data = prop_recid_analysis)

# Create an object for the desired information  
mod_prop_recid_rep_detox_tr_off_list <- c('Reported detox treatment (Tr)', 'treatment', 'Offered', mod_prop_recid_rep_detox_tr_off$k, mod_prop_recid_rep_detox_tr_off$beta[1], mod_prop_recid_rep_detox_tr_off$se[1], mod_prop_recid_rep_detox_tr_off$zval[1], mod_prop_recid_rep_detox_tr_off$pval[1], 
                                         mod_prop_recid_rep_detox_tr_off$ci.lb[1], mod_prop_recid_rep_detox_tr_off$ci.ub[1], mod_prop_recid_rep_detox_tr_off$k- length(mod_prop_recid_rep_detox_tr_off$b), mod_prop_recid_rep_detox_tr_off$QE, mod_prop_recid_rep_detox_tr_off$QEp, 
                                         mod_prop_recid_rep_detox_tr_off$I2)
# name the object's columns and bind to prop_moderator_output
names(mod_prop_recid_rep_detox_tr_off_list) <- c("name", "mod_category", "level", "level_k", "est_LOR", "est_SE","est_z","est_z_p","est_cl","est_cu", "res_DF", "Qres", "Qres_p", "I2")
prop_moderator_output <- bind_rows(prop_moderator_output, mod_prop_recid_rep_detox_tr_off_list)
########################################
# 4.63: Reported detox treatment - Control
#####################################
table(prop_recid_analysis$reported_detox_cont, prop_recid_analysis$studyid)
# Note: there are insufficient numbers across groups to compute a moderator/subgroup analysis
########################################
# 4.64: Reported pharmacotherapy - Treatment
#####################################
table(prop_recid_analysis$reported_pharmacotherapy_treat, prop_recid_analysis$studyid)
# Note: this variable will not be included in the proportion who recidivated moderator/subgroup analysis because it separates outcomes into the identical two groups as '4.66: Reported detox treatment - Treatment'.
########################################
# 4.65: Reported pharmacotherapy - Control
#####################################
table(prop_recid_analysis$reported_pharmacotherapy_cont, prop_recid_analysis$studyid)  
# Note: there are insufficient numbers across groups to compute a moderator/subgroup analysis
########################################
# 4.66: Reported relapse prevention - Treatment
#####################################
table(prop_recid_analysis$reported_relapse_prevention_treat, prop_recid_analysis$studyid)
# Recode 'offered' and 'required' to 'yes' 
prop_recid_analysis$reported_relapse_prevention_treat_recode <- recode(prop_recid_analysis$reported_relapse_prevention_treat, 'offered'='Yes', 'required' ='Yes')  
########################################
# 4.66.1: Reported relapse prevention - Treatment - No
#####################################
# Select the subgroup in the model 
mod_prop_recid_rep_relapse_prev_tr_no <- rma(yi=yi, vi=vi, method="REML", subset=(reported_relapse_prevention_treat_recode == "No"), data = prop_recid_analysis)

# Create an object for the desired information  
mod_prop_recid_rep_relapse_prev_tr_no_list <- c('Reported relapse prevention (Tr)', 'treatment', 'No (ref)', mod_prop_recid_rep_relapse_prev_tr_no$k, mod_prop_recid_rep_relapse_prev_tr_no$beta[1], mod_prop_recid_rep_relapse_prev_tr_no$se[1], mod_prop_recid_rep_relapse_prev_tr_no$zval[1], mod_prop_recid_rep_relapse_prev_tr_no$pval[1], 
                                         mod_prop_recid_rep_relapse_prev_tr_no$ci.lb[1], mod_prop_recid_rep_relapse_prev_tr_no$ci.ub[1], mod_prop_recid_rep_relapse_prev_tr_no$k- length(mod_prop_recid_rep_relapse_prev_tr_no$b), mod_prop_recid_rep_relapse_prev_tr_no$QE, mod_prop_recid_rep_relapse_prev_tr_no$QEp, 
                                         mod_prop_recid_rep_relapse_prev_tr_no$I2)
# name the object's columns and bind to prop_moderator_output
names(mod_prop_recid_rep_relapse_prev_tr_no_list) <- c("name", "mod_category", "level", "level_k", "est_LOR", "est_SE","est_z","est_z_p","est_cl","est_cu", "res_DF", "Qres", "Qres_p", "I2")
prop_moderator_output <- bind_rows(prop_moderator_output, mod_prop_recid_rep_relapse_prev_tr_no_list)

########################################
# 4.66.2: Reported relapse prevention - Treatment - Offered or required
#####################################
# Select the subgroup in the model 
mod_prop_recid_rep_relapse_prev_tr_yes <- rma(yi=yi, vi=vi, method="REML", subset=(reported_relapse_prevention_treat_recode == "Yes"), data = prop_recid_analysis, digits=5, control=list(stepadj=0.5))

# Create an object for the desired information  
mod_prop_recid_rep_relapse_prev_tr_yes_list <- c('Reported relapse prevention (Tr)', 'treatment', 'Offered or required', mod_prop_recid_rep_relapse_prev_tr_yes$k, mod_prop_recid_rep_relapse_prev_tr_yes$beta[1], mod_prop_recid_rep_relapse_prev_tr_yes$se[1], mod_prop_recid_rep_relapse_prev_tr_yes$zval[1], mod_prop_recid_rep_relapse_prev_tr_yes$pval[1], 
                                          mod_prop_recid_rep_relapse_prev_tr_yes$ci.lb[1], mod_prop_recid_rep_relapse_prev_tr_yes$ci.ub[1], mod_prop_recid_rep_relapse_prev_tr_yes$k- length(mod_prop_recid_rep_relapse_prev_tr_yes$b), mod_prop_recid_rep_relapse_prev_tr_yes$QE, mod_prop_recid_rep_relapse_prev_tr_yes$QEp, 
                                          mod_prop_recid_rep_relapse_prev_tr_yes$I2)
# name the object's columns and bind to prop_moderator_output
names(mod_prop_recid_rep_relapse_prev_tr_yes_list) <- c("name", "mod_category", "level", "level_k", "est_LOR", "est_SE","est_z","est_z_p","est_cl","est_cu", "res_DF", "Qres", "Qres_p", "I2")
prop_moderator_output <- bind_rows(prop_moderator_output, mod_prop_recid_rep_relapse_prev_tr_yes_list)      
########################################
# 4.67: Reported relapse prevention - Control
#####################################
table(prop_recid_analysis$reported_relapse_prevention_cont, prop_recid_analysis$studyid)      
# Note: there are insufficient numbers across groups to compute a moderator/subgroup analysis
########################################
# 4.68: Reported psychiatric treatment - Treatment
#####################################
table(prop_recid_analysis$reported_psychiatric_treatment_treat, prop_recid_analysis$studyid)      
# Note: there are insufficient numbers across groups to compute a moderator/subgroup analysis
########################################
# 4.69: Reported psychiatric treatment - Control
#####################################
table(prop_recid_analysis$reported_psychiatric_treatment_cont, prop_recid_analysis$studyid)      
# Note: there are insufficient numbers across groups to compute a moderator/subgroup analysis
########################################
# 4.70: Reported mental health treatment - Treatment
#####################################
table(prop_recid_analysis$reported_mental_health_treatment_treat, prop_recid_analysis$studyid)  
########################################
# 4.70.1: Reported mental health treatment - Treatment - No
#####################################
# Select the subgroup in the model 
mod_prop_recid_rep_mh_treat_tr_no <- rma(yi=yi, vi=vi, method="REML", subset=(reported_mental_health_treatment_treat == "No"), data = prop_recid_analysis)

# Create an object for the desired information  
mod_prop_recid_rep_mh_treat_tr_no_list <- c('Reported mental health treatment (Tr)', 'treatment', 'No (ref)', mod_prop_recid_rep_mh_treat_tr_no$k, mod_prop_recid_rep_mh_treat_tr_no$beta[1], mod_prop_recid_rep_mh_treat_tr_no$se[1], mod_prop_recid_rep_mh_treat_tr_no$zval[1], mod_prop_recid_rep_mh_treat_tr_no$pval[1], 
                                                mod_prop_recid_rep_mh_treat_tr_no$ci.lb[1], mod_prop_recid_rep_mh_treat_tr_no$ci.ub[1], mod_prop_recid_rep_mh_treat_tr_no$k- length(mod_prop_recid_rep_mh_treat_tr_no$b), mod_prop_recid_rep_mh_treat_tr_no$QE, mod_prop_recid_rep_mh_treat_tr_no$QEp, 
                                                mod_prop_recid_rep_mh_treat_tr_no$I2)
# name the object's columns and bind to prop_moderator_output
names(mod_prop_recid_rep_mh_treat_tr_no_list) <- c("name", "mod_category", "level", "level_k", "est_LOR", "est_SE","est_z","est_z_p","est_cl","est_cu", "res_DF", "Qres", "Qres_p", "I2")
prop_moderator_output <- bind_rows(prop_moderator_output, mod_prop_recid_rep_mh_treat_tr_no_list)
########################################
# 4.70.2: Reported mental health treatment - Treatment - Offered
#####################################
# Select the subgroup in the model 
mod_prop_recid_rep_mh_treat_tr_off <- rma(yi=yi, vi=vi, method="REML", subset=(reported_mental_health_treatment_treat == "offered"), data = prop_recid_analysis)

# Create an object for the desired information  
mod_prop_recid_rep_mh_treat_tr_off_list <- c('Reported mental health treatment (Tr)', 'treatment', 'Offered', mod_prop_recid_rep_mh_treat_tr_off$k, mod_prop_recid_rep_mh_treat_tr_off$beta[1], mod_prop_recid_rep_mh_treat_tr_off$se[1], mod_prop_recid_rep_mh_treat_tr_off$zval[1], mod_prop_recid_rep_mh_treat_tr_off$pval[1], 
                                                mod_prop_recid_rep_mh_treat_tr_off$ci.lb[1], mod_prop_recid_rep_mh_treat_tr_off$ci.ub[1], mod_prop_recid_rep_mh_treat_tr_off$k- length(mod_prop_recid_rep_mh_treat_tr_off$b), mod_prop_recid_rep_mh_treat_tr_off$QE, mod_prop_recid_rep_mh_treat_tr_off$QEp, 
                                                mod_prop_recid_rep_mh_treat_tr_off$I2)
# name the object's columns and bind to prop_moderator_output
names(mod_prop_recid_rep_mh_treat_tr_off_list) <- c("name", "mod_category", "level", "level_k", "est_LOR", "est_SE","est_z","est_z_p","est_cl","est_cu", "res_DF", "Qres", "Qres_p", "I2")
prop_moderator_output <- bind_rows(prop_moderator_output, mod_prop_recid_rep_mh_treat_tr_off_list)
########################################
# 4.71: Reported mental health treatment - Control
#####################################
table(prop_recid_analysis$reported_mental_health_treatment_cont, prop_recid_analysis$studyid)  
# Note: there are insufficient numbers across groups to compute a moderator/subgroup analysis
########################################
# 4.72: Reported cognitive-behavioral therapy - Treatment
#####################################
table(prop_recid_analysis$reported_CBT_based_treatment_treat, prop_recid_analysis$studyid)  
# Recode offered/required to 'yes' to form the reported yes/no variable. 
prop_recid_analysis$reported_CBT_based_treatment_treat_recode <- recode(prop_recid_analysis$reported_CBT_based_treatment_treat, 'offered' = 'Yes', 'required' = 'Yes')
########################################
# 4.72.1: Reported cognitive-behavioral therapy - Treatment - No
#####################################
# Select the subgroup in the model 
mod_prop_recid_rep_cbt_treat_tr_no <- rma(yi=yi, vi=vi, method="REML", subset=(reported_CBT_based_treatment_treat_recode == "No"), data = prop_recid_analysis)

# Create an object for the desired information  
mod_prop_recid_rep_cbt_treat_tr_no_list <- c('Reported CBT treatment (Tr)', 'treatment', 'No (ref)', mod_prop_recid_rep_cbt_treat_tr_no$k, mod_prop_recid_rep_cbt_treat_tr_no$beta[1], mod_prop_recid_rep_cbt_treat_tr_no$se[1], mod_prop_recid_rep_cbt_treat_tr_no$zval[1], mod_prop_recid_rep_cbt_treat_tr_no$pval[1], 
                                                mod_prop_recid_rep_cbt_treat_tr_no$ci.lb[1], mod_prop_recid_rep_cbt_treat_tr_no$ci.ub[1], mod_prop_recid_rep_cbt_treat_tr_no$k- length(mod_prop_recid_rep_cbt_treat_tr_no$b), mod_prop_recid_rep_cbt_treat_tr_no$QE, mod_prop_recid_rep_cbt_treat_tr_no$QEp, 
                                                mod_prop_recid_rep_cbt_treat_tr_no$I2)
# name the object's columns and bind to prop_moderator_output
names(mod_prop_recid_rep_cbt_treat_tr_no_list) <- c("name", "mod_category", "level", "level_k", "est_LOR", "est_SE","est_z","est_z_p","est_cl","est_cu", "res_DF", "Qres", "Qres_p", "I2")
prop_moderator_output <- bind_rows(prop_moderator_output, mod_prop_recid_rep_cbt_treat_tr_no_list)
########################################
# 4.72.2: Reported cognitive-behavioral therapy - Treatment - Offered or required
#####################################
# Select the subgroup in the model 
mod_prop_recid_rep_cbt_treat_tr_yes <- rma(yi=yi, vi=vi, method="REML", subset=(reported_CBT_based_treatment_treat_recode == "Yes"), data = prop_recid_analysis)

# Create an object for the desired information  
mod_prop_recid_rep_cbt_treat_tr_yes_list <- c('Reported CBT treatment (Tr)', 'treatment', 'Offered or required', mod_prop_recid_rep_cbt_treat_tr_yes$k, mod_prop_recid_rep_cbt_treat_tr_yes$beta[1], mod_prop_recid_rep_cbt_treat_tr_yes$se[1], mod_prop_recid_rep_cbt_treat_tr_yes$zval[1], mod_prop_recid_rep_cbt_treat_tr_yes$pval[1], 
                                             mod_prop_recid_rep_cbt_treat_tr_yes$ci.lb[1], mod_prop_recid_rep_cbt_treat_tr_yes$ci.ub[1], mod_prop_recid_rep_cbt_treat_tr_yes$k- length(mod_prop_recid_rep_cbt_treat_tr_yes$b), mod_prop_recid_rep_cbt_treat_tr_yes$QE, mod_prop_recid_rep_cbt_treat_tr_yes$QEp, 
                                             mod_prop_recid_rep_cbt_treat_tr_yes$I2)
# name the object's columns and bind to prop_moderator_output
names(mod_prop_recid_rep_cbt_treat_tr_yes_list) <- c("name", "mod_category", "level", "level_k", "est_LOR", "est_SE","est_z","est_z_p","est_cl","est_cu", "res_DF", "Qres", "Qres_p", "I2")
prop_moderator_output <- bind_rows(prop_moderator_output, mod_prop_recid_rep_cbt_treat_tr_yes_list)
########################################
# 4.73: Reported cognitive-behavioral therapy - Control
#####################################
table(prop_recid_analysis$reported_CBT_based_treatment_cont, prop_recid_analysis$studyid)
# Note: there are insufficient numbers across groups to compute a moderator/subgroup analysis
########################################
# 4.74: Reported counseling - Treatment 
#####################################
table(prop_recid_analysis$reported_counseling_treat, prop_recid_analysis$studyid)  
########################################
# 4.74.1: Reported counseling - Treatment - No
#####################################
# Select the subgroup in the model 
mod_prop_recid_rep_counselling_tr_no <- rma(yi=yi, vi=vi, method="REML", subset=(reported_counseling_treat == "No"), data = prop_recid_analysis)

# Create an object for the desired information  
mod_prop_recid_rep_counselling_tr_no_list <- c('Reported counselling (Tr)', 'treatment', 'No (ref)', mod_prop_recid_rep_counselling_tr_no$k, mod_prop_recid_rep_counselling_tr_no$beta[1], mod_prop_recid_rep_counselling_tr_no$se[1], mod_prop_recid_rep_counselling_tr_no$zval[1], mod_prop_recid_rep_counselling_tr_no$pval[1], 
                                             mod_prop_recid_rep_counselling_tr_no$ci.lb[1], mod_prop_recid_rep_counselling_tr_no$ci.ub[1], mod_prop_recid_rep_counselling_tr_no$k- length(mod_prop_recid_rep_counselling_tr_no$b), mod_prop_recid_rep_counselling_tr_no$QE, mod_prop_recid_rep_counselling_tr_no$QEp, 
                                             mod_prop_recid_rep_counselling_tr_no$I2)
# name the object's columns and bind to prop_moderator_output
names(mod_prop_recid_rep_counselling_tr_no_list) <- c("name", "mod_category", "level", "level_k", "est_LOR", "est_SE","est_z","est_z_p","est_cl","est_cu", "res_DF", "Qres", "Qres_p", "I2")
prop_moderator_output <- bind_rows(prop_moderator_output, mod_prop_recid_rep_counselling_tr_no_list)
########################################
# 4.74.2: Reported counseling - Treatment - Required
#####################################
# Select the subgroup in the model 
mod_prop_recid_rep_counselling_tr_req <- rma(yi=yi, vi=vi, method="REML", subset=(reported_counseling_treat == "required"), data = prop_recid_analysis)

# Create an object for the desired information  
mod_prop_recid_rep_counselling_tr_req_list <- c('Reported counselling (Tr)', 'treatment', 'Required', mod_prop_recid_rep_counselling_tr_req$k, mod_prop_recid_rep_counselling_tr_req$beta[1], mod_prop_recid_rep_counselling_tr_req$se[1], mod_prop_recid_rep_counselling_tr_req$zval[1], mod_prop_recid_rep_counselling_tr_req$pval[1], 
                                               mod_prop_recid_rep_counselling_tr_req$ci.lb[1], mod_prop_recid_rep_counselling_tr_req$ci.ub[1], mod_prop_recid_rep_counselling_tr_req$k- length(mod_prop_recid_rep_counselling_tr_req$b), mod_prop_recid_rep_counselling_tr_req$QE, mod_prop_recid_rep_counselling_tr_req$QEp, 
                                               mod_prop_recid_rep_counselling_tr_req$I2)
# name the object's columns and bind to prop_moderator_output
names(mod_prop_recid_rep_counselling_tr_req_list) <- c("name", "mod_category", "level", "level_k", "est_LOR", "est_SE","est_z","est_z_p","est_cl","est_cu", "res_DF", "Qres", "Qres_p", "I2")
prop_moderator_output <- bind_rows(prop_moderator_output, mod_prop_recid_rep_counselling_tr_req_list)
########################################
# 4.74.3: Reported counseling - Treatment - Offered
#####################################
# Select the subgroup in the model 
mod_prop_recid_rep_counselling_tr_off <- rma(yi=yi, vi=vi, method="REML", subset=(reported_counseling_treat == "offered"), data = prop_recid_analysis, control=list(stepadj=0.5))

# Create an object for the desired information  
mod_prop_recid_rep_counselling_tr_off_list <- c('Reported counselling (Tr)', 'treatment', 'Offered', mod_prop_recid_rep_counselling_tr_off$k, mod_prop_recid_rep_counselling_tr_off$beta[1], mod_prop_recid_rep_counselling_tr_off$se[1], mod_prop_recid_rep_counselling_tr_off$zval[1], mod_prop_recid_rep_counselling_tr_off$pval[1], 
                                                mod_prop_recid_rep_counselling_tr_off$ci.lb[1], mod_prop_recid_rep_counselling_tr_off$ci.ub[1], mod_prop_recid_rep_counselling_tr_off$k- length(mod_prop_recid_rep_counselling_tr_off$b), mod_prop_recid_rep_counselling_tr_off$QE, mod_prop_recid_rep_counselling_tr_off$QEp, 
                                                mod_prop_recid_rep_counselling_tr_off$I2)
# name the object's columns and bind to prop_moderator_output
names(mod_prop_recid_rep_counselling_tr_off_list) <- c("name", "mod_category", "level", "level_k", "est_LOR", "est_SE","est_z","est_z_p","est_cl","est_cu", "res_DF", "Qres", "Qres_p", "I2")
prop_moderator_output <- bind_rows(prop_moderator_output, mod_prop_recid_rep_counselling_tr_off_list)
########################################
# 4.75: Reported counseling - Control 
#####################################
table(prop_recid_analysis$reported_counseling_cont, prop_recid_analysis$studyid)  
# Note: there are insufficient numbers across groups to compute a moderator/subgroup analysis
########################################
# 4.76: Reported housing support - Treatment 
#####################################
table(prop_recid_analysis$reported_housing_support_treat, prop_recid_analysis$studyid)       
########################################
# 4.76.1: Reported housing support - Treatment - No
#####################################
# Select the subgroup in the model 
mod_prop_recid_rep_house_supp_tr_no <- rma(yi=yi, vi=vi, method="REML", subset=(reported_housing_support_treat == "No"), data = prop_recid_analysis)

# Create an object for the desired information  
mod_prop_recid_rep_house_supp_tr_no_list <- c('Reported housing support (Tr)', 'treatment', 'No (ref)', mod_prop_recid_rep_house_supp_tr_no$k, mod_prop_recid_rep_house_supp_tr_no$beta[1], mod_prop_recid_rep_house_supp_tr_no$se[1], mod_prop_recid_rep_house_supp_tr_no$zval[1], mod_prop_recid_rep_house_supp_tr_no$pval[1], 
                                               mod_prop_recid_rep_house_supp_tr_no$ci.lb[1], mod_prop_recid_rep_house_supp_tr_no$ci.ub[1], mod_prop_recid_rep_house_supp_tr_no$k- length(mod_prop_recid_rep_house_supp_tr_no$b), mod_prop_recid_rep_house_supp_tr_no$QE, mod_prop_recid_rep_house_supp_tr_no$QEp, 
                                               mod_prop_recid_rep_house_supp_tr_no$I2)
# name the object's columns and bind to prop_moderator_output
names(mod_prop_recid_rep_house_supp_tr_no_list) <- c("name", "mod_category", "level", "level_k", "est_LOR", "est_SE","est_z","est_z_p","est_cl","est_cu", "res_DF", "Qres", "Qres_p", "I2")
prop_moderator_output <- bind_rows(prop_moderator_output, mod_prop_recid_rep_house_supp_tr_no_list)      
########################################
# 4.76.2: Reported housing support - Treatment - Offered
#####################################
# Select the subgroup in the model 
mod_prop_recid_rep_house_supp_tr_off <- rma(yi=yi, vi=vi, method="REML", subset=(reported_housing_support_treat == "offered"), data = prop_recid_analysis)

# Create an object for the desired information  
mod_prop_recid_rep_house_supp_tr_off_list <- c('Reported housing support (Tr)', 'treatment', 'Offered', mod_prop_recid_rep_house_supp_tr_off$k, mod_prop_recid_rep_house_supp_tr_off$beta[1], mod_prop_recid_rep_house_supp_tr_off$se[1], mod_prop_recid_rep_house_supp_tr_off$zval[1], mod_prop_recid_rep_house_supp_tr_off$pval[1], 
                                              mod_prop_recid_rep_house_supp_tr_off$ci.lb[1], mod_prop_recid_rep_house_supp_tr_off$ci.ub[1], mod_prop_recid_rep_house_supp_tr_off$k- length(mod_prop_recid_rep_house_supp_tr_off$b), mod_prop_recid_rep_house_supp_tr_off$QE, mod_prop_recid_rep_house_supp_tr_off$QEp, 
                                              mod_prop_recid_rep_house_supp_tr_off$I2)
# name the object's columns and bind to prop_moderator_output
names(mod_prop_recid_rep_house_supp_tr_off_list) <- c("name", "mod_category", "level", "level_k", "est_LOR", "est_SE","est_z","est_z_p","est_cl","est_cu", "res_DF", "Qres", "Qres_p", "I2")
prop_moderator_output <- bind_rows(prop_moderator_output, mod_prop_recid_rep_house_supp_tr_off_list)      
########################################
# 4.77: Reported housing support - Control 
#####################################
table(prop_recid_analysis$reported_housing_support_cont, prop_recid_analysis$studyid)      
# Note: there are insufficient numbers across groups to compute a moderator/subgroup analysis
########################################
# 4.78: Reported support groups - Treatment 
#####################################
table(prop_recid_analysis$reported_support_groups_treat, prop_recid_analysis$studyid) 
# Recode 'offered' and 'required' to 'yes' so that there are sufficient outcomes in each group.
prop_recid_analysis$reported_support_groups_treat_recode <- recode(prop_recid_analysis$reported_support_groups_treat, 'offered'='Yes', 'required' ='Yes') 
########################################
# 4.78.1: Reported support groups - Treatment - No
#####################################
# Select the subgroup in the model 
mod_prop_recid_rep_supp_group_tr_no <- rma(yi=yi, vi=vi, method="REML", subset=(reported_support_groups_treat_recode == "No"), data = prop_recid_analysis)

# Create an object for the desired information  
mod_prop_recid_rep_supp_group_tr_no_list <- c('Reported support groups (Tr)', 'treatment', 'No (ref)', mod_prop_recid_rep_supp_group_tr_no$k, mod_prop_recid_rep_supp_group_tr_no$beta[1], mod_prop_recid_rep_supp_group_tr_no$se[1], mod_prop_recid_rep_supp_group_tr_no$zval[1], mod_prop_recid_rep_supp_group_tr_no$pval[1], 
                                              mod_prop_recid_rep_supp_group_tr_no$ci.lb[1], mod_prop_recid_rep_supp_group_tr_no$ci.ub[1], mod_prop_recid_rep_supp_group_tr_no$k- length(mod_prop_recid_rep_supp_group_tr_no$b), mod_prop_recid_rep_supp_group_tr_no$QE, mod_prop_recid_rep_supp_group_tr_no$QEp, 
                                              mod_prop_recid_rep_supp_group_tr_no$I2)
# name the object's columns and bind to prop_moderator_output
names(mod_prop_recid_rep_supp_group_tr_no_list) <- c("name", "mod_category", "level", "level_k", "est_LOR", "est_SE","est_z","est_z_p","est_cl","est_cu", "res_DF", "Qres", "Qres_p", "I2")
prop_moderator_output <- bind_rows(prop_moderator_output, mod_prop_recid_rep_supp_group_tr_no_list)  
########################################
# 4.78.2: Reported support groups - Treatment - Offered or required
#####################################
# Select the subgroup in the model 
mod_prop_recid_rep_supp_group_tr_yes <- rma(yi=yi, vi=vi, method="REML", subset=(reported_support_groups_treat_recode == "Yes"), data = prop_recid_analysis)

# Create an object for the desired information  
mod_prop_recid_rep_supp_group_tr_yes_list <- c('Reported support groups (Tr)', 'treatment', 'Offered or required', mod_prop_recid_rep_supp_group_tr_yes$k, mod_prop_recid_rep_supp_group_tr_yes$beta[1], mod_prop_recid_rep_supp_group_tr_yes$se[1], mod_prop_recid_rep_supp_group_tr_yes$zval[1], mod_prop_recid_rep_supp_group_tr_yes$pval[1], 
                                              mod_prop_recid_rep_supp_group_tr_yes$ci.lb[1], mod_prop_recid_rep_supp_group_tr_yes$ci.ub[1], mod_prop_recid_rep_supp_group_tr_yes$k- length(mod_prop_recid_rep_supp_group_tr_yes$b), mod_prop_recid_rep_supp_group_tr_yes$QE, mod_prop_recid_rep_supp_group_tr_yes$QEp, 
                                              mod_prop_recid_rep_supp_group_tr_yes$I2)
# name the object's columns and bind to prop_moderator_output
names(mod_prop_recid_rep_supp_group_tr_yes_list) <- c("name", "mod_category", "level", "level_k", "est_LOR", "est_SE","est_z","est_z_p","est_cl","est_cu", "res_DF", "Qres", "Qres_p", "I2")
prop_moderator_output <- bind_rows(prop_moderator_output, mod_prop_recid_rep_supp_group_tr_yes_list)  
########################################
# 4.79: Reported support groups - Control 
#####################################
table(prop_recid_analysis$reported_support_groups_.cont, prop_recid_analysis$studyid)      
# Note: there are insufficient numbers across groups to compute a moderator/subgroup analysis
########################################
# 4.80: Reported material aid - Treatment 
#####################################
table(prop_recid_analysis$reported_material_aid_treat, prop_recid_analysis$studyid) 
########################################
# 4.80.1: Reported material aid - Treatment - No
#####################################
# Select the subgroup in the model 
mod_prop_recid_rep_mat_aid_tr_no <- rma(yi=yi, vi=vi, method="REML", subset=(reported_material_aid_treat == "No"), data = prop_recid_analysis)

# Create an object for the desired information  
mod_prop_recid_rep_mat_aid_tr_no_list <- c('Reported material aid (Tr)', 'treatment', 'No (ref)', mod_prop_recid_rep_mat_aid_tr_no$k, mod_prop_recid_rep_mat_aid_tr_no$beta[1], mod_prop_recid_rep_mat_aid_tr_no$se[1], mod_prop_recid_rep_mat_aid_tr_no$zval[1], mod_prop_recid_rep_mat_aid_tr_no$pval[1], 
                                              mod_prop_recid_rep_mat_aid_tr_no$ci.lb[1], mod_prop_recid_rep_mat_aid_tr_no$ci.ub[1], mod_prop_recid_rep_mat_aid_tr_no$k- length(mod_prop_recid_rep_mat_aid_tr_no$b), mod_prop_recid_rep_mat_aid_tr_no$QE, mod_prop_recid_rep_mat_aid_tr_no$QEp, 
                                              mod_prop_recid_rep_mat_aid_tr_no$I2)
# name the object's columns and bind to prop_moderator_output
names(mod_prop_recid_rep_mat_aid_tr_no_list) <- c("name", "mod_category", "level", "level_k", "est_LOR", "est_SE","est_z","est_z_p","est_cl","est_cu", "res_DF", "Qres", "Qres_p", "I2")
prop_moderator_output <- bind_rows(prop_moderator_output, mod_prop_recid_rep_mat_aid_tr_no_list) 
########################################
# 4.80.2: Reported material aid - Treatment - Offered
#####################################
# Select the subgroup in the model 
mod_prop_recid_rep_mat_aid_tr_off <- rma(yi=yi, vi=vi, method="REML", subset=(reported_material_aid_treat == "offered"), data = prop_recid_analysis)

# Create an object for the desired information  
mod_prop_recid_rep_mat_aid_tr_off_list <- c('Reported material aid (Tr)', 'treatment', 'Offered', mod_prop_recid_rep_mat_aid_tr_off$k, mod_prop_recid_rep_mat_aid_tr_off$beta[1], mod_prop_recid_rep_mat_aid_tr_off$se[1], mod_prop_recid_rep_mat_aid_tr_off$zval[1], mod_prop_recid_rep_mat_aid_tr_off$pval[1], 
                                           mod_prop_recid_rep_mat_aid_tr_off$ci.lb[1], mod_prop_recid_rep_mat_aid_tr_off$ci.ub[1], mod_prop_recid_rep_mat_aid_tr_off$k- length(mod_prop_recid_rep_mat_aid_tr_off$b), mod_prop_recid_rep_mat_aid_tr_off$QE, mod_prop_recid_rep_mat_aid_tr_off$QEp, 
                                           mod_prop_recid_rep_mat_aid_tr_off$I2)
# name the object's columns and bind to prop_moderator_output
names(mod_prop_recid_rep_mat_aid_tr_off_list) <- c("name", "mod_category", "level", "level_k", "est_LOR", "est_SE","est_z","est_z_p","est_cl","est_cu", "res_DF", "Qres", "Qres_p", "I2")
prop_moderator_output <- bind_rows(prop_moderator_output, mod_prop_recid_rep_mat_aid_tr_off_list) 
########################################
# 4.81: Reported material aid - Control 
#####################################
table(prop_recid_analysis$reported_material_aid_cont, prop_recid_analysis$studyid)      
# Note: there are insufficient numbers across groups to compute a moderator/subgroup analysis
########################################
# 4.82: Reported health services - Treatment 
#####################################
table(prop_recid_analysis$reported_health_services_treat, prop_recid_analysis$studyid) 
# Note: this variable separates the subgroups in the same way as '4.43: Reported team training' and will be excluded from the proportion that recidivated moderator analysis.
########################################
# 4.83: Reported health services - Control 
#####################################
table(prop_recid_analysis$reported_health_services_cont, prop_recid_analysis$studyid)      
# Note: there are insufficient numbers across groups to compute a moderator/subgroup analysis
########################################
# 4.84: Reported domestic violence program - Treatment 
#####################################
table(prop_recid_analysis$reported_domestic_violence_prog_treat, prop_recid_analysis$studyid) 
########################################
# 4.84.1: Reported domestic violence program - Treatment - No
#####################################
# Select the subgroup in the model 
mod_prop_recid_rep_dv_tr_no <- rma(yi=yi, vi=vi, method="REML", subset=(reported_domestic_violence_prog_treat == "No"), data = prop_recid_analysis)

# Create an object for the desired information  
mod_prop_recid_rep_dv_tr_no_list <- c('Reported domestic violence program (Tr)', 'treatment', 'No (ref)', mod_prop_recid_rep_dv_tr_no$k, mod_prop_recid_rep_dv_tr_no$beta[1], mod_prop_recid_rep_dv_tr_no$se[1], mod_prop_recid_rep_dv_tr_no$zval[1], mod_prop_recid_rep_dv_tr_no$pval[1], 
                                               mod_prop_recid_rep_dv_tr_no$ci.lb[1], mod_prop_recid_rep_dv_tr_no$ci.ub[1], mod_prop_recid_rep_dv_tr_no$k- length(mod_prop_recid_rep_dv_tr_no$b), mod_prop_recid_rep_dv_tr_no$QE, mod_prop_recid_rep_dv_tr_no$QEp, 
                                               mod_prop_recid_rep_dv_tr_no$I2)
# name the object's columns and bind to prop_moderator_output
names(mod_prop_recid_rep_dv_tr_no_list) <- c("name", "mod_category", "level", "level_k", "est_LOR", "est_SE","est_z","est_z_p","est_cl","est_cu", "res_DF", "Qres", "Qres_p", "I2")
prop_moderator_output <- bind_rows(prop_moderator_output, mod_prop_recid_rep_dv_tr_no_list) 
########################################
# 4.84.2: Reported domestic violence program - Treatment - Offered
#####################################
# Select the subgroup in the model 
mod_prop_recid_rep_dv_tr_off <- rma(yi=yi, vi=vi, method="REML", subset=(reported_domestic_violence_prog_treat == "offered"), data = prop_recid_analysis)

# Create an object for the desired information  
mod_prop_recid_rep_dv_tr_off_list <- c('Reported domestic violence program (Tr)', 'treatment', 'Offered', mod_prop_recid_rep_dv_tr_off$k, mod_prop_recid_rep_dv_tr_off$beta[1], mod_prop_recid_rep_dv_tr_off$se[1], mod_prop_recid_rep_dv_tr_off$zval[1], mod_prop_recid_rep_dv_tr_off$pval[1], 
                                      mod_prop_recid_rep_dv_tr_off$ci.lb[1], mod_prop_recid_rep_dv_tr_off$ci.ub[1], mod_prop_recid_rep_dv_tr_off$k- length(mod_prop_recid_rep_dv_tr_off$b), mod_prop_recid_rep_dv_tr_off$QE, mod_prop_recid_rep_dv_tr_off$QEp, 
                                      mod_prop_recid_rep_dv_tr_off$I2)
# name the object's columns and bind to prop_moderator_output
names(mod_prop_recid_rep_dv_tr_off_list) <- c("name", "mod_category", "level", "level_k", "est_LOR", "est_SE","est_z","est_z_p","est_cl","est_cu", "res_DF", "Qres", "Qres_p", "I2")
prop_moderator_output <- bind_rows(prop_moderator_output, mod_prop_recid_rep_dv_tr_off_list) 
########################################
# 4.85: Reported domestic violence program - Control 
#####################################
table(prop_recid_analysis$reported_domestic_violence_prog_cont, prop_recid_analysis$studyid)      
# Note: there are insufficient numbers across groups to compute a moderator/subgroup analysis
########################################
# 4.86: Reported vocational or educational training - Treatment 
#####################################
table(prop_recid_analysis$reported_edu_or_vocational_services_treat, prop_recid_analysis$studyid) 
# Recode 'offered' and 'required' to 'yes' so that there are sufficient outcomes in each group.
prop_recid_analysis$reported_edu_or_vocational_services_treat_recode <- recode(prop_recid_analysis$reported_edu_or_vocational_services_treat, 'offered'='Yes', 'required' ='Yes') 
########################################
# 4.86.1: Reported vocational or educational services - Treatment - No
#####################################
# Select the subgroup in the model 
mod_prop_recid_rep_edu_voc_serv_tr_no <- rma(yi=yi, vi=vi, method="REML", subset=(reported_edu_or_vocational_services_treat_recode == "No"), data = prop_recid_analysis)

# Create an object for the desired information  
mod_prop_recid_rep_edu_voc_serv_tr_no_list <- c('Reported vocational or educational services (Tr)', 'treatment', 'No (ref)', mod_prop_recid_rep_edu_voc_serv_tr_no$k, mod_prop_recid_rep_edu_voc_serv_tr_no$beta[1], mod_prop_recid_rep_edu_voc_serv_tr_no$se[1], mod_prop_recid_rep_edu_voc_serv_tr_no$zval[1], mod_prop_recid_rep_edu_voc_serv_tr_no$pval[1], 
                                      mod_prop_recid_rep_edu_voc_serv_tr_no$ci.lb[1], mod_prop_recid_rep_edu_voc_serv_tr_no$ci.ub[1], mod_prop_recid_rep_edu_voc_serv_tr_no$k- length(mod_prop_recid_rep_edu_voc_serv_tr_no$b), mod_prop_recid_rep_edu_voc_serv_tr_no$QE, mod_prop_recid_rep_edu_voc_serv_tr_no$QEp, 
                                      mod_prop_recid_rep_edu_voc_serv_tr_no$I2)
# name the object's columns and bind to prop_moderator_output
names(mod_prop_recid_rep_edu_voc_serv_tr_no_list) <- c("name", "mod_category", "level", "level_k", "est_LOR", "est_SE","est_z","est_z_p","est_cl","est_cu", "res_DF", "Qres", "Qres_p", "I2")
prop_moderator_output <- bind_rows(prop_moderator_output, mod_prop_recid_rep_edu_voc_serv_tr_no_list) 
########################################
# 4.86.2: Reported vocational or educational services - Treatment - Offered or required
#####################################
# Select the subgroup in the model 
mod_prop_recid_rep_edu_voc_serv_tr_yes <- rma(yi=yi, vi=vi, method="REML", subset=(reported_edu_or_vocational_services_treat_recode == "Yes"), data = prop_recid_analysis)

# Create an object for the desired information  
mod_prop_recid_rep_edu_voc_serv_tr_yes_list <- c('Reported vocational or educational services (Tr)', 'treatment', 'Offered or required', mod_prop_recid_rep_edu_voc_serv_tr_yes$k, mod_prop_recid_rep_edu_voc_serv_tr_yes$beta[1], mod_prop_recid_rep_edu_voc_serv_tr_yes$se[1], mod_prop_recid_rep_edu_voc_serv_tr_yes$zval[1], mod_prop_recid_rep_edu_voc_serv_tr_yes$pval[1], 
                                                mod_prop_recid_rep_edu_voc_serv_tr_yes$ci.lb[1], mod_prop_recid_rep_edu_voc_serv_tr_yes$ci.ub[1], mod_prop_recid_rep_edu_voc_serv_tr_yes$k- length(mod_prop_recid_rep_edu_voc_serv_tr_yes$b), mod_prop_recid_rep_edu_voc_serv_tr_yes$QE, mod_prop_recid_rep_edu_voc_serv_tr_yes$QEp, 
                                                mod_prop_recid_rep_edu_voc_serv_tr_yes$I2)
# name the object's columns and bind to prop_moderator_output
names(mod_prop_recid_rep_edu_voc_serv_tr_yes_list) <- c("name", "mod_category", "level", "level_k", "est_LOR", "est_SE","est_z","est_z_p","est_cl","est_cu", "res_DF", "Qres", "Qres_p", "I2")
prop_moderator_output <- bind_rows(prop_moderator_output, mod_prop_recid_rep_edu_voc_serv_tr_yes_list) 
########################################
# 4.87: Reported vocational or educational services - Control 
#####################################
table(prop_recid_analysis$reported_edu_or_vocational_services_cont, prop_recid_analysis$studyid)      
# Note: there are insufficient numbers across groups to compute a moderator/subgroup analysis
########################################
# 4.88: Reported anger management - Treatment 
#####################################
table(prop_recid_analysis$reported_anger_management_treat, prop_recid_analysis$studyid) 
########################################
# 4.88.1: Reported anger management - Treatment - No
#####################################
# Select the subgroup in the model 
mod_prop_recid_rep_anger_man_tr_no <- rma(yi=yi, vi=vi, method="REML", subset=(reported_anger_management_treat == "No"), data = prop_recid_analysis)

# Create an object for the desired information  
mod_prop_recid_rep_anger_man_tr_no_list <- c('Reported anger management (Tr)', 'treatment', 'No (ref)', mod_prop_recid_rep_anger_man_tr_no$k, mod_prop_recid_rep_anger_man_tr_no$beta[1], mod_prop_recid_rep_anger_man_tr_no$se[1], mod_prop_recid_rep_anger_man_tr_no$zval[1], mod_prop_recid_rep_anger_man_tr_no$pval[1], 
                                                mod_prop_recid_rep_anger_man_tr_no$ci.lb[1], mod_prop_recid_rep_anger_man_tr_no$ci.ub[1], mod_prop_recid_rep_anger_man_tr_no$k- length(mod_prop_recid_rep_anger_man_tr_no$b), mod_prop_recid_rep_anger_man_tr_no$QE, mod_prop_recid_rep_anger_man_tr_no$QEp, 
                                                mod_prop_recid_rep_anger_man_tr_no$I2)
# name the object's columns and bind to prop_moderator_output
names(mod_prop_recid_rep_anger_man_tr_no_list) <- c("name", "mod_category", "level", "level_k", "est_LOR", "est_SE","est_z","est_z_p","est_cl","est_cu", "res_DF", "Qres", "Qres_p", "I2")
prop_moderator_output <- bind_rows(prop_moderator_output, mod_prop_recid_rep_anger_man_tr_no_list) 
########################################
# 4.88.2: Reported anger management - Treatment - Offered
#####################################
# Select the subgroup in the model 
mod_prop_recid_rep_anger_man_tr_yes <- rma(yi=yi, vi=vi, method="REML", subset=(reported_anger_management_treat == "offered"), data = prop_recid_analysis)

# Create an object for the desired information  
mod_prop_recid_rep_anger_man_tr_yes_list <- c('Reported anger management (Tr)', 'treatment', 'Offered', mod_prop_recid_rep_anger_man_tr_yes$k, mod_prop_recid_rep_anger_man_tr_yes$beta[1], mod_prop_recid_rep_anger_man_tr_yes$se[1], mod_prop_recid_rep_anger_man_tr_yes$zval[1], mod_prop_recid_rep_anger_man_tr_yes$pval[1], 
                                             mod_prop_recid_rep_anger_man_tr_yes$ci.lb[1], mod_prop_recid_rep_anger_man_tr_yes$ci.ub[1], mod_prop_recid_rep_anger_man_tr_yes$k- length(mod_prop_recid_rep_anger_man_tr_yes$b), mod_prop_recid_rep_anger_man_tr_yes$QE, mod_prop_recid_rep_anger_man_tr_yes$QEp, 
                                             mod_prop_recid_rep_anger_man_tr_yes$I2)
# name the object's columns and bind to prop_moderator_output
names(mod_prop_recid_rep_anger_man_tr_yes_list) <- c("name", "mod_category", "level", "level_k", "est_LOR", "est_SE","est_z","est_z_p","est_cl","est_cu", "res_DF", "Qres", "Qres_p", "I2")
prop_moderator_output <- bind_rows(prop_moderator_output, mod_prop_recid_rep_anger_man_tr_yes_list) 
########################################
# 4.89: Reported anger management - Control 
#####################################
table(prop_recid_analysis$reported_anger_management_cont, prop_recid_analysis$studyid)      
# Note: there are insufficient numbers across groups to compute a moderator/subgroup analysis
########################################
# 4.90: Reported life-skills training - Treatment 
#####################################
table(prop_recid_analysis$reported_life_skills_treat, prop_recid_analysis$studyid) 
########################################
# 4.90.1: Reported life-skills training - Treatment - No
#####################################
# Select the subgroup in the model 
mod_prop_recid_rep_life_skill_tr_no <- rma(yi=yi, vi=vi, method="REML", subset=(reported_life_skills_treat == "No"), data = prop_recid_analysis)

# Create an object for the desired information  
mod_prop_recid_rep_life_skill_tr_no_list <- c('Reported life-skills training (Tr)', 'treatment', 'No (ref)', mod_prop_recid_rep_life_skill_tr_no$k, mod_prop_recid_rep_life_skill_tr_no$beta[1], mod_prop_recid_rep_life_skill_tr_no$se[1], mod_prop_recid_rep_life_skill_tr_no$zval[1], mod_prop_recid_rep_life_skill_tr_no$pval[1], 
                                             mod_prop_recid_rep_life_skill_tr_no$ci.lb[1], mod_prop_recid_rep_life_skill_tr_no$ci.ub[1], mod_prop_recid_rep_life_skill_tr_no$k- length(mod_prop_recid_rep_life_skill_tr_no$b), mod_prop_recid_rep_life_skill_tr_no$QE, mod_prop_recid_rep_life_skill_tr_no$QEp, 
                                             mod_prop_recid_rep_life_skill_tr_no$I2)
# name the object's columns and bind to prop_moderator_output
names(mod_prop_recid_rep_life_skill_tr_no_list) <- c("name", "mod_category", "level", "level_k", "est_LOR", "est_SE","est_z","est_z_p","est_cl","est_cu", "res_DF", "Qres", "Qres_p", "I2")
prop_moderator_output <- bind_rows(prop_moderator_output, mod_prop_recid_rep_life_skill_tr_no_list) 
########################################
# 4.90.2: Reported life-skills training - Treatment - Offered
#####################################
# Select the subgroup in the model 
mod_prop_recid_rep_life_skill_tr_off <- rma(yi=yi, vi=vi, method="REML", subset=(reported_life_skills_treat == "offered"), data = prop_recid_analysis)

# Create an object for the desired information  
mod_prop_recid_rep_life_skill_tr_off_list <- c('Reported life-skills training (Tr)', 'treatment', 'Offered', mod_prop_recid_rep_life_skill_tr_off$k, mod_prop_recid_rep_life_skill_tr_off$beta[1], mod_prop_recid_rep_life_skill_tr_off$se[1], mod_prop_recid_rep_life_skill_tr_off$zval[1], mod_prop_recid_rep_life_skill_tr_off$pval[1], 
                                              mod_prop_recid_rep_life_skill_tr_off$ci.lb[1], mod_prop_recid_rep_life_skill_tr_off$ci.ub[1], mod_prop_recid_rep_life_skill_tr_off$k- length(mod_prop_recid_rep_life_skill_tr_off$b), mod_prop_recid_rep_life_skill_tr_off$QE, mod_prop_recid_rep_life_skill_tr_off$QEp, 
                                              mod_prop_recid_rep_life_skill_tr_off$I2)
# name the object's columns and bind to prop_moderator_output
names(mod_prop_recid_rep_life_skill_tr_off_list) <- c("name", "mod_category", "level", "level_k", "est_LOR", "est_SE","est_z","est_z_p","est_cl","est_cu", "res_DF", "Qres", "Qres_p", "I2")
prop_moderator_output <- bind_rows(prop_moderator_output, mod_prop_recid_rep_life_skill_tr_off_list) 
########################################
# 4.91: Reported life-skills training - Control 
#####################################
table(prop_recid_analysis$reported_life_skills_cont, prop_recid_analysis$studyid)      
# Note: there are insufficient numbers across groups to compute a moderator/subgroup analysis
########################################
# 4.92: Reported family support services - Treatment 
#####################################
table(prop_recid_analysis$reported_family_support_service_or_training_treat, prop_recid_analysis$studyid) 
# Note: this variable separates the subgroups in the same way as 4.88: Reported domestic violence program - Treatment and will be excluded from the proportion that recidivated moderator analysis.
########################################
# 4.93: Reported family support services - Control 
#####################################
table(prop_recid_analysis$reported_family_support_service_or_training_cont, prop_recid_analysis$studyid)      
# Note: there are insufficient numbers across groups to compute a moderator/subgroup analysis
########################################
# 4.94: Reported prison intervention - Control 
#####################################
table(prop_recid_analysis$reported_prison_cont, prop_recid_analysis$studyid)  
########################################
# 4.94.1: Reported prison intervention - Control - No
#####################################
# Select the subgroup in the model 
mod_prop_recid_rep_prison_con_no <- rma(yi=yi, vi=vi, method="REML", subset=(reported_prison_cont == "No"), data = prop_recid_analysis)

# Create an object for the desired information  
mod_prop_recid_rep_prison_con_no_list <- c('Reported prison intervention (Con)', 'treatment', 'No (ref)', mod_prop_recid_rep_prison_con_no$k, mod_prop_recid_rep_prison_con_no$beta[1], mod_prop_recid_rep_prison_con_no$se[1], mod_prop_recid_rep_prison_con_no$zval[1], mod_prop_recid_rep_prison_con_no$pval[1], 
                                               mod_prop_recid_rep_prison_con_no$ci.lb[1], mod_prop_recid_rep_prison_con_no$ci.ub[1], mod_prop_recid_rep_prison_con_no$k- length(mod_prop_recid_rep_prison_con_no$b), mod_prop_recid_rep_prison_con_no$QE, mod_prop_recid_rep_prison_con_no$QEp, 
                                               mod_prop_recid_rep_prison_con_no$I2)
# name the object's columns and bind to prop_moderator_output
names(mod_prop_recid_rep_prison_con_no_list) <- c("name", "mod_category", "level", "level_k", "est_LOR", "est_SE","est_z","est_z_p","est_cl","est_cu", "res_DF", "Qres", "Qres_p", "I2")
prop_moderator_output <- bind_rows(prop_moderator_output, mod_prop_recid_rep_prison_con_no_list) 
########################################
# 4.94.2: Reported prison intervention - Control - Yes
#####################################
# Select the subgroup in the model 
mod_prop_recid_rep_prison_con_yes <- rma(yi=yi, vi=vi, method="REML", subset=(reported_prison_cont == "Yes"), data = prop_recid_analysis)

# Create an object for the desired information  
mod_prop_recid_rep_prison_con_yes_list <- c('Reported prison intervention (Con)', 'treatment', 'Yes', mod_prop_recid_rep_prison_con_yes$k, mod_prop_recid_rep_prison_con_yes$beta[1], mod_prop_recid_rep_prison_con_yes$se[1], mod_prop_recid_rep_prison_con_yes$zval[1], mod_prop_recid_rep_prison_con_yes$pval[1], 
                                           mod_prop_recid_rep_prison_con_yes$ci.lb[1], mod_prop_recid_rep_prison_con_yes$ci.ub[1], mod_prop_recid_rep_prison_con_yes$k- length(mod_prop_recid_rep_prison_con_yes$b), mod_prop_recid_rep_prison_con_yes$QE, mod_prop_recid_rep_prison_con_yes$QEp, 
                                           mod_prop_recid_rep_prison_con_yes$I2)
# name the object's columns and bind to prop_moderator_output
names(mod_prop_recid_rep_prison_con_yes_list) <- c("name", "mod_category", "level", "level_k", "est_LOR", "est_SE","est_z","est_z_p","est_cl","est_cu", "res_DF", "Qres", "Qres_p", "I2")
prop_moderator_output <- bind_rows(prop_moderator_output, mod_prop_recid_rep_prison_con_yes_list) 
########################################
# 4.95: Other treatments - Treatment 
#####################################
table(prop_recid_analysis$other_treatment_treat, prop_recid_analysis$studyid) 
# Note: there are insufficient numbers across groups to compute a moderator/subgroup analysis
########################################
# 4.96: Other treatments - Control 
#####################################
table(prop_recid_analysis$other_treatment_cont, prop_recid_analysis$studyid) 
# Note: there are insufficient numbers across groups to compute a moderator/subgroup analysis
########################################
# 4.97: Total number of treatments required - Treatment 
#####################################
table(prop_recid_analysis$total_treatments_required_treat, prop_recid_analysis$studyid) 
# Insert the moderator into the model
mod_prop_recid_treats_req_tr <- rma(yi=yi, vi=vi, method="REML", mods = ~ total_treatments_required_treat, data = prop_recid_analysis, digits=5, control=list(stepadj=0.5))
# Create an object for the desired information and bind it to the 'moderator_output' data frame 
mod_prop_recid_treats_req_tr_list<- c('Total number of treatments required (Tr)', 'treatment', 'Beta_weight', mod_prop_recid_treats_req_tr$k, mod_prop_recid_treats_req_tr$beta[2, 1], mod_prop_recid_treats_req_tr$se[2], mod_prop_recid_treats_req_tr$zval[2], mod_prop_recid_treats_req_tr$pval[2], 
                                     mod_prop_recid_treats_req_tr$ci.lb[2], mod_prop_recid_treats_req_tr$ci.ub[2], mod_prop_recid_treats_req_tr$k- length(mod_prop_recid_treats_req_tr$b), mod_prop_recid_treats_req_tr$QE, mod_prop_recid_treats_req_tr$QEp, 
                                     mod_prop_recid_treats_req_tr$I2)
# name the object's columns 
names(mod_prop_recid_treats_req_tr_list) <- c("name", "mod_category", "level", "level_k", "est_LOR", "est_SE","est_z","est_z_p","est_cl","est_cu", "res_DF", "Qres", "Qres_p","I2")
prop_moderator_output <- bind_rows(prop_moderator_output, mod_prop_recid_treats_req_tr_list)
########################################
# 4.98: Total number of treatments required - Control 
#####################################
table(prop_recid_analysis$total_treatments_required_cont, prop_recid_analysis$studyid) 
# Note: there are insufficient numbers across groups to compute a moderator/subgroup analysis
########################################
# 4.99: Total number of treatments offered - Treatment 
#####################################
table(prop_recid_analysis$total_treatments_offered_treat, prop_recid_analysis$studyid) 
# Insert the moderator into the model
mod_prop_recid_treats_off_tr <- rma(yi=yi, vi=vi, method="REML", mods = ~ total_treatments_offered_treat, data = prop_recid_analysis, digits=5, control=list(stepadj=0.5))
# Create an object for the desired information and bind it to the 'moderator_output' data frame 
mod_prop_recid_treats_off_tr_list<- c('Total number of treatments offered (Tr)', 'treatment', 'Beta_weight', mod_prop_recid_treats_off_tr$k, mod_prop_recid_treats_off_tr$beta[2, 1], mod_prop_recid_treats_off_tr$se[2], mod_prop_recid_treats_off_tr$zval[2], mod_prop_recid_treats_off_tr$pval[2], 
                                      mod_prop_recid_treats_off_tr$ci.lb[2], mod_prop_recid_treats_off_tr$ci.ub[2], mod_prop_recid_treats_off_tr$k- length(mod_prop_recid_treats_off_tr$b), mod_prop_recid_treats_off_tr$QE, mod_prop_recid_treats_off_tr$QEp, 
                                      mod_prop_recid_treats_off_tr$I2)
# name the object's columns 
names(mod_prop_recid_treats_off_tr_list) <- c("name", "mod_category", "level", "level_k", "est_LOR", "est_SE","est_z","est_z_p","est_cl","est_cu", "res_DF", "Qres", "Qres_p","I2")
prop_moderator_output <- bind_rows(prop_moderator_output, mod_prop_recid_treats_off_tr_list)
########################################
# 4.100: Total number of treatments offered - Control 
#####################################
table(prop_recid_analysis$total_treatments_offered_cont, prop_recid_analysis$studyid) 
# Recode 'NA' to 'no' so that there are sufficient outcomes in each group.
prop_recid_analysis$total_treatments_offered_cont_recode <- replace_na(prop_recid_analysis$total_treatments_offered_cont, 'No') 
########################################
# 4.100.1: Total number of treatments offered - None reported 
#####################################
# Select the subgroup in the model 
mod_prop_recid_treats_off_con_no <- rma(yi=yi, vi=vi, method="REML", subset=(total_treatments_offered_cont_recode == "No"), data = prop_recid_analysis, control=list(stepadj=0.5))

# Create an object for the desired information  
mod_prop_recid_treats_off_con_no_list <- c('Reported any treatment offered (Con)', 'treatment', 'No (ref)', mod_prop_recid_treats_off_con_no$k, mod_prop_recid_treats_off_con_no$beta[1], mod_prop_recid_treats_off_con_no$se[1], mod_prop_recid_treats_off_con_no$zval[1], mod_prop_recid_treats_off_con_no$pval[1], 
                                           mod_prop_recid_treats_off_con_no$ci.lb[1], mod_prop_recid_treats_off_con_no$ci.ub[1], mod_prop_recid_treats_off_con_no$k- length(mod_prop_recid_treats_off_con_no$b), mod_prop_recid_treats_off_con_no$QE, mod_prop_recid_treats_off_con_no$QEp, 
                                           mod_prop_recid_treats_off_con_no$I2)
# name the object's columns and bind to prop_moderator_output
names(mod_prop_recid_treats_off_con_no_list) <- c("name", "mod_category", "level", "level_k", "est_LOR", "est_SE","est_z","est_z_p","est_cl","est_cu", "res_DF", "Qres", "Qres_p", "I2")
prop_moderator_output <- bind_rows(prop_moderator_output, mod_prop_recid_treats_off_con_no_list) 
########################################
# 4.100.2: Total number of treatments offered - One reported
#####################################
# Select the subgroup in the model 
mod_prop_recid_treats_off_con_yes <- rma(yi=yi, vi=vi, method="REML", subset=(total_treatments_offered_cont_recode == "1"), data = prop_recid_analysis)

# Create an object for the desired information  
mod_prop_recid_treats_off_con_yes_list <- c('Reported any treatment offered (Con)', 'treatment', 'Yes', mod_prop_recid_treats_off_con_yes$k, mod_prop_recid_treats_off_con_yes$beta[1], mod_prop_recid_treats_off_con_yes$se[1], mod_prop_recid_treats_off_con_yes$zval[1], mod_prop_recid_treats_off_con_yes$pval[1], 
                                           mod_prop_recid_treats_off_con_yes$ci.lb[1], mod_prop_recid_treats_off_con_yes$ci.ub[1], mod_prop_recid_treats_off_con_yes$k- length(mod_prop_recid_treats_off_con_yes$b), mod_prop_recid_treats_off_con_yes$QE, mod_prop_recid_treats_off_con_yes$QEp, 
                                           mod_prop_recid_treats_off_con_yes$I2)
# name the object's columns and bind to prop_moderator_output
names(mod_prop_recid_treats_off_con_yes_list) <- c("name", "mod_category", "level", "level_k", "est_LOR", "est_SE","est_z","est_z_p","est_cl","est_cu", "res_DF", "Qres", "Qres_p", "I2")
prop_moderator_output <- bind_rows(prop_moderator_output, mod_prop_recid_treats_off_con_yes_list) 

########################################
# 4.102: Modified Maryland Scientific methods scale rating
#####################################
table(prop_recid_analysis$MSMS, prop_recid_analysis$studyid) 
# Insert the moderator into the model
mod_prop_recid_MSMS <- rma(yi=yi, vi=vi, method="REML", mods = ~ MSMS, data = prop_recid_analysis, digits=5, control=list(stepadj=0.5))
# Create an object for the desired information and bind it to the 'moderator_output' data frame 
mod_prop_recid_MSMS_list<- c('MSMS rating', 'methodological', 'Beta_weight', mod_prop_recid_MSMS$k, mod_prop_recid_MSMS$beta[2, 1], mod_prop_recid_MSMS$se[2], mod_prop_recid_MSMS$zval[2], mod_prop_recid_MSMS$pval[2], 
                                      mod_prop_recid_MSMS$ci.lb[2], mod_prop_recid_MSMS$ci.ub[2], mod_prop_recid_MSMS$k- length(mod_prop_recid_MSMS$b), mod_prop_recid_MSMS$QE, mod_prop_recid_MSMS$QEp, 
                                      mod_prop_recid_MSMS$I2)
# name the object's columns 
names(mod_prop_recid_MSMS_list) <- c("name", "mod_category", "level", "level_k", "est_LOR", "est_SE","est_z","est_z_p","est_cl","est_cu", "res_DF", "Qres", "Qres_p","I2")
prop_moderator_output <- bind_rows(prop_moderator_output, mod_prop_recid_MSMS_list)
########################################
# 4.103: Modified Maryland Scientific methods scale rating - Sensitivity analysis re-code a: midway category
#####################################
# Create a variable that recodes studyies rated as '3.5' on the MSMS for the reason that participants were matched 'Participants matched on some variables using a valid and reliable statistical technique, but unmatched on most of the important confounding variables ' as 4
prop_recid_analysis$MSMS_recode1 <- ifelse(prop_recid_analysis$MSMS == '3' & prop_recid_analysis$MSMS_reason == 'Participants matched on some variables using a valid and reliable statistical technique, but unmatched on most of the important confounding variables ',
                                           3.5, prop_recid_analysis$MSMS)
# Insert the moderator into the model
mod_prop_recid_MSMS_recode1 <- rma(yi=yi, vi=vi, method="REML", mods = ~ MSMS_recode1, data = prop_recid_analysis, digits=5, control=list(stepadj=0.5))
# Create an object for the desired information and bind it to the 'moderator_output' data frame 
mod_prop_recid_MSMS_recode1_list<- c('MSMS rating recode #a', 'methodological', 'Beta_weight', mod_prop_recid_MSMS_recode1$k, mod_prop_recid_MSMS_recode1$beta[2, 1], mod_prop_recid_MSMS_recode1$se[2], mod_prop_recid_MSMS_recode1$zval[2], mod_prop_recid_MSMS_recode1$pval[2], 
                                     mod_prop_recid_MSMS_recode1$ci.lb[2], mod_prop_recid_MSMS_recode1$ci.ub[2], mod_prop_recid_MSMS_recode1$k- length(mod_prop_recid_MSMS_recode1$b), mod_prop_recid_MSMS_recode1$QE, mod_prop_recid_MSMS_recode1$QEp, 
                                     mod_prop_recid_MSMS_recode1$I2)
# name the object's columns 
names(mod_prop_recid_MSMS_recode1_list) <- c("name", "mod_category", "level", "level_k", "est_LOR", "est_SE","est_z","est_z_p","est_cl","est_cu", "res_DF", "Qres", "Qres_p","I2")
prop_moderator_output <- bind_rows(prop_moderator_output, mod_prop_recid_MSMS_recode1_list)
########################################
# 4.104: Modified Maryland Scientific methods scale rating - Sensitivity analysis re-code b: considered 'well-matched' 
#####################################
# Create a variable that recodes studyies rated as '3' on the MSMS for the reason that participants were matched 'Participants matched on some variables using a valid and reliable statistical technique, but unmatched on most of the important confounding variables ' as 4
prop_recid_analysis$MSMS_recode2 <- ifelse(prop_recid_analysis$MSMS == '3' & prop_recid_analysis$MSMS_reason == 'Participants matched on some variables using a valid and reliable statistical technique, but unmatched on most of the important confounding variables ',
                                          4, prop_recid_analysis$MSMS)
# Insert the moderator into the model
mod_prop_recid_MSMS_recode2 <- rma(yi=yi, vi=vi, method="REML", mods = ~ MSMS_recode2, data = prop_recid_analysis, digits=5, control=list(stepadj=0.5))
# Create an object for the desired information and bind it to the 'moderator_output' data frame 
mod_prop_recid_MSMS_recode2_list<- c('MSMS rating recode #b', 'methodological', 'Beta_weight', mod_prop_recid_MSMS_recode2$k, mod_prop_recid_MSMS_recode2$beta[2, 1], mod_prop_recid_MSMS_recode2$se[2], mod_prop_recid_MSMS_recode2$zval[2], mod_prop_recid_MSMS_recode2$pval[2], 
                             mod_prop_recid_MSMS_recode2$ci.lb[2], mod_prop_recid_MSMS_recode2$ci.ub[2], mod_prop_recid_MSMS_recode2$k- length(mod_prop_recid_MSMS_recode2$b), mod_prop_recid_MSMS_recode2$QE, mod_prop_recid_MSMS_recode2$QEp, 
                             mod_prop_recid_MSMS_recode2$I2)
# name the object's columns 
names(mod_prop_recid_MSMS_recode2_list) <- c("name", "mod_category", "level", "level_k", "est_LOR", "est_SE","est_z","est_z_p","est_cl","est_cu", "res_DF", "Qres", "Qres_p","I2")
prop_moderator_output <- bind_rows(prop_moderator_output, mod_prop_recid_MSMS_recode2_list)

########################################
# 5: Mean re-offences
########################################
########################################
# 5.1: Separate target rows 
#####################################
# Separate the measures related mean offences  
mean_recid_meas <- AUSNZ_wrangle_final %>% subset(., measure== "annualized offence episodes"|measure== "annualized offences"| measure== "mean annualized arrests"| measure== "court appearances (estimated)"|
                                           measure== "mean offence episodes"| measure== "mean proven offences"| measure== "mean proven offences per 100 participants")  

# Separate preferred measures: annualized offences is preferred to mean proven offences 
mean_recid_prefmeas <- subset(mean_recid_meas, subset = measure %in% "annualized offences") 
mean_recid_unprefmeas <- subset(mean_recid_meas, !(mean_recid_meas$measure== "annualized offences")) 
mean_recid_unprefmeas <- mean_recid_unprefmeas[!(mean_recid_unprefmeas$studyid%in%mean_recid_prefmeas$studyid),] 
mean_recid_bound <- bind_rows(mean_recid_prefmeas,mean_recid_unprefmeas) 

sum(unique(mean_recid_bound$studyid)>0)==sum(unique(mean_recid_meas$studyid)>0) 

# Separate preferred outcomes
mean_recid_prefout <- subset(mean_recid_bound, subset = outcome %in% c("offences", "theft or drug offences")) 
mean_recid_unprefout <- subset(mean_recid_bound, !(mean_recid_bound$outcome== "offences" | mean_recid_bound$outcome== "theft or drug offences")) 
mean_recid_unprefout <- mean_recid_unprefout[!(mean_recid_unprefout$studyid%in%mean_recid_prefout$studyid),] 
mean_recid_bound<-bind_rows(mean_recid_prefout,mean_recid_unprefout) 

sum(unique(mean_recid_bound$studyid)>0)==sum(unique(mean_recid_meas$studyid)>0) 

# Separate by studygroups_alternative: 'partially matched' is the preferred studygroups_alternative
mean_recid_prefstudygroups_alt <- subset(mean_recid_bound, subset = studygroups_alternative %in% "partially matched") 
mean_recid_unprefstudygroups_alt <- subset(mean_recid_bound, !(mean_recid_bound$studygroups_alternative=="partially matched")) 
mean_recid_unprefstudygroups_alt <- mean_recid_unprefstudygroups_alt[!(mean_recid_unprefstudygroups_alt$studyid%in%mean_recid_prefstudygroups_alt$studyid),] 
mean_recid_bound<-bind_rows(mean_recid_prefstudygroups_alt,mean_recid_unprefstudygroups_alt) 
sum(unique(mean_recid_bound$studyid)>0)==sum(unique(mean_recid_meas$studyid)>0) 

# Separate by study group: 'combined groups' is the preferred studygroups value
mean_recid_prefstudygroups <- subset(mean_recid_bound, subset = studygroups %in% "combined groups") 
mean_recid_unprefstudygroups <- subset(mean_recid_bound, !(mean_recid_bound$studygroups=="combined groups")) 
mean_recid_unprefstudygroups <- mean_recid_unprefstudygroups[!(mean_recid_unprefstudygroups$studyid%in%mean_recid_prefstudygroups$studyid),] 
mean_recid_bound<-bind_rows(mean_recid_prefstudygroups,mean_recid_unprefstudygroups) 
sum(unique(mean_recid_bound$studyid)>0)==sum(unique(mean_recid_meas$studyid)>0) 

# Separate by Intention-to-treat: "Graduates/Failures" is the preferred ITT value 
mean_recid_prefitt <- subset(mean_recid_bound, subset = ITT %in% "Graduates/Failures") 
mean_recid_unprefitt <- subset(mean_recid_bound, !(mean_recid_bound$ITT=="Graduates/Failures")) 
mean_recid_unprefitt <- mean_recid_unprefitt[!(mean_recid_unprefitt$studyid%in%mean_recid_prefitt$studyid),] 
mean_recid_bound<-bind_rows(mean_recid_prefitt,mean_recid_unprefitt) 
sum(unique(mean_recid_bound$studyid)>0)==sum(unique(mean_recid_meas$studyid)>0) 

# Separate by time-status: "free time" is the preferred time-status 
mean_recid_preftime_stat <- subset(mean_recid_bound, subset = time_status_of_measure %in% "free time") 
mean_recid_unpreftime_stat <- subset(mean_recid_bound, !(mean_recid_bound$time_status_of_measure=="free time")) 
mean_recid_unpreftime_stat <- mean_recid_unpreftime_stat[!(mean_recid_unpreftime_stat$studyid%in%mean_recid_preftime_stat$studyid),] 
mean_recid_bound<-bind_rows(mean_recid_preftime_stat,mean_recid_unpreftime_stat) 
sum(unique(mean_recid_bound$studyid)>0)==sum(unique(mean_recid_meas$studyid)>0) 

# Separate by pt_period_months: preferred period is 12 months 
mean_recid_prefpt_period <- subset(mean_recid_bound, subset = pt_period_months %in% c("12", NA))
mean_recid_unprefpt_period <- subset(mean_recid_bound, !(mean_recid_bound$pt_period_months=="12"|
                                                         is.na(mean_recid_bound$pt_period_months))) 
mean_recid_unprefpt_period <- mean_recid_unprefpt_period[!(mean_recid_unprefpt_period$studyid%in%mean_recid_prefpt_period$studyid),] 
mean_recid_bound<-bind_rows(mean_recid_prefpt_period,mean_recid_unprefpt_period) 
sum(unique(mean_recid_bound$studyid)>0)==sum(unique(mean_recid_meas$studyid)>0) 

# Drop rows that do not report the follow-up period or average amount of follow-up for treatment and comparisons. Select each study Id's row that has the closest average follow-up days between treatment and comparison
mean_recid_pref_fu <- subset(mean_recid_bound, !(is.na(mean_recid_bound$pt_period_months))) 
mean_recid_unpref_fu <- subset(mean_recid_bound, is.na(mean_recid_bound$pt_period_months))

mean_recid_unpref_fu <- mean_recid_unpref_fu %>%
                                                group_by(studyid) %>% 
                                                   slice(which.min(abs(pt_period_days_avg_treat-pt_period_days_avg_cont)))

mean_recid_bound<-bind_rows(mean_recid_unpref_fu, mean_recid_pref_fu)
sum(unique(mean_recid_bound$studyid)>0)==sum(unique(mean_recid_meas$studyid)>0) # note Study ID '1b' has now be removed: it reports the same outcome as Study ID 1a (the original evaluation) and is preferred here.  

# #reset order of outcomes.
target <- c("1a", "4b","7", "8", "11", "12", "15") 
mean_recid_bound_final <- mean_recid_bound[match(target, mean_recid_bound$studyid),] 

########################################
# 5.2: Incidents and free days variables 
#####################################
# Calculate incident counts and free days for treatment and control conditions
# Note: Study ID 4b's follow-up period is an estimate per 1000 free days. As such we will calculate their free days as n*1000, while the other Study's will be calculated as n*365.  
mean_recid_bound_final$pt_incidents_treat <- round2(mean_recid_bound_final$pt_treat_m*mean_recid_bound_final$pt_treat_n, 0)
mean_recid_bound_final$pt_freedays_treat <- ifelse(mean_recid_bound_final$studyid == '4b',1000*mean_recid_bound_final$pt_treat_n, 
                                                   365*mean_recid_bound_final$pt_treat_n)

mean_recid_bound_final$pt_incidents_cont <- round2(mean_recid_bound_final$pt_cont_m*mean_recid_bound_final$pt_cont_n, 0)
mean_recid_bound_final$pt_freedays_cont <- ifelse(mean_recid_bound_final$studyid == '4b',1000*mean_recid_bound_final$pt_cont_n, 
                                                  365*mean_recid_bound_final$pt_cont_n)

########################################
# 5.3: Meta-analysis of recidivist events: conditional generalized linear mixed-effects model using exact likelihood
#####################################
# Perform a a meta-analysis of incident rates per 1000 free days following Viechtbauer's (n.d.) guide to Stijnen (2010). 
mean_recid_bound_final$pt_freedays_treat <- round2(mean_recid_bound_final$pt_freedays_treat/1000, 0)
mean_recid_bound_final$pt_freedays_cont <- round2(mean_recid_bound_final$pt_freedays_cont/1000, 0)

# Fit poisson-normal models to the treatment and control groups separately. 
recid_rates_tr <- rma.glmm(measure="IRLN", xi=pt_incidents_treat, ti=pt_freedays_treat, data=mean_recid_bound_final)
predict(recid_rates_tr, transf=exp, digits=3)

recid_rates_con <- rma.glmm(measure="IRLN", xi=pt_incidents_cont, ti=pt_freedays_cont, data=mean_recid_bound_final)
predict(recid_rates_con, transf=exp, digits=3)

# Calculate the effect size for each outcome
mean_recid_analysis <- summary.escalc(escalc(measure="IRR", x1i=pt_incidents_treat, t1i=pt_freedays_treat, x2i=pt_incidents_cont, t2i=pt_freedays_cont, data=mean_recid_bound_final, to="none"))

# Reorder the rows by effect size
mean_recid_analysis <- mean_recid_analysis[order(mean_recid_analysis$yi),]

# For supplementary materials only: reorder and export
# mean_recid_analysis <- mean_recid_analysis[order(mean_recid_analysis$year),] %>% select(., -studyid)
# write.csv(mean_recid_analysis, file = "Incidence of recidivism outcome descriptives.csv")


# Model the data with a mixed-effects conditional Poisson regression model (i.e., the binomial-normal model)
mean_recid_rma_glmm_CMEL  <- rma.glmm(measure="IRR", x1i=pt_incidents_treat, t1i=pt_freedays_treat, x2i=pt_incidents_cont, t2i=pt_freedays_cont, data=mean_recid_analysis, model="CM.EL")
summary(mean_recid_rma_glmm_CMEL, digits=3)
predict(mean_recid_rma_glmm_CMEL, transf=exp, digits=3)

# test the deviance for overdispersion 
mean_recid_rma_glmm_CMEL_dev_p<- pchisq((mean_recid_rma_glmm_CMEL$fit.stats[2,1]), df=mean_recid_rma_glmm_CMEL$k-1, lower.tail=FALSE)
mean_recid_rma_glmm_CMEL_dev_p

# Create a vector with the row name, pooled odds ratio, confidence interval, z statistic, z p-value, credibility interval, Q statistic, and I^2
mean_pooled_effect_CMEL <- c('GLMM conditional model with exact liklihood', exp(mean_recid_rma_glmm_CMEL$b), exp(mean_recid_rma_glmm_CMEL$ci.lb), exp(mean_recid_rma_glmm_CMEL$ci.ub), mean_recid_rma_glmm_CMEL$zval, 
                           mean_recid_rma_glmm_CMEL$pval, exp(predict(mean_recid_rma_glmm_CMEL)$cr.lb), exp(predict(mean_recid_rma_glmm_CMEL)$cr.ub), mean_recid_rma_glmm_CMEL$QE.Wld, mean_recid_rma_glmm_CMEL$QEp.Wld,
                           mean_recid_rma_glmm_CMEL$QE.LRT, mean_recid_rma_glmm_CMEL$QEp.LRT, mean_recid_rma_glmm_CMEL$k-1, mean_recid_rma_glmm_CMEL$I2, mean_recid_rma_glmm_CMEL$fit.stats[2,1], mean_recid_rma_glmm_CMEL_dev_p)

# Add the names for each column
names(mean_pooled_effect_CMEL) <- c("Model", "Incident Rate Ratio", "ConL","ConU", "Z statistic", "Z p-value","CrL", "CrU", "Wald-type test","Wald-type test p-value","Likelihood ratio test", 
                                    "Likelihood ratio test p-value","Dfs", "I^2", "Deviance", "Deviance p-value")
########################################
# 5.4: Forest plot: mean recidivist events
#####################################
# Create objects for the required pieces of data and round them to 2 decimal places for the forestplot structure
mean_fp.IRR <- round2(exp(mean_recid_analysis$yi), 2)
mean_fp.lower.CI <- round2(exp(mean_recid_analysis$ci.lb), 2)
mean_fp.upper.CI <- round2(exp(mean_recid_analysis$ci.ub), 2)
mean_fp.pooled_IRR <-  round2(exp(mean_recid_rma_glmm_CMEL$b), 2)
mean_fp.pooled_IRR_CI_low <- round2(exp(mean_recid_rma_glmm_CMEL$ci.lb), 2)
mean_fp.pooled_IRR_CI_upp <- round2(exp(mean_recid_rma_glmm_CMEL$ci.ub), 2)
mean_fp.z.p.value <- format.pval(2 * pnorm(abs(mean_recid_analysis$zi), lower.tail = FALSE), eps = .001, 2)
mean_fp.pooled_IRR_z_p_value <- round2(mean_recid_rma_glmm_CMEL$pval, 3)
mean_fp.pooled_IRR_I2 <- round2(mean_recid_rma_glmm_CMEL$I2, 2)
mean_fp.pooled_IRR_CR_low <- round2(predict(mean_recid_rma_glmm_CMEL, trans = exp)$cr.lb, 2)
mean_fp.pooled_IRR_CR_upp <- round2(predict(mean_recid_rma_glmm_CMEL, trans = exp)$cr.ub, 2)

###Create the text objects to be displayed (where different from those above). 
mean_fp.txt.IRR <- format(mean_fp.IRR, nsmall = 2)
mean_fp.txt.lower.CI <- paste0(format(mean_fp.lower.CI, nsmall = 2), ",")
mean_fp.txt.CI <-  paste0("[", (paste(mean_fp.txt.lower.CI, format(mean_fp.upper.CI, nsmall = 2))), "]")
mean_fp.txt.pooled_IRR_CI_low <- paste0(round2(mean_fp.pooled_IRR_CI_low, 2), ",")
mean_fp.txt.pooled.CI <-  paste0("[", (paste(mean_fp.txt.pooled_IRR_CI_low , round2(mean_fp.pooled_IRR_CI_upp, 2))), "]")

###Amend the year and author observations
mean_fp_authors <- paste0(mean_recid_analysis$authors,",")
mean_fp_author.year <- paste(mean_fp_authors, paste0("(", mean_recid_analysis$year, ")"))

### Create Forest Plot Structure and Text
mean_fp_structure <- 
  structure(list(
    mean  = c(NA, mean_fp.IRR, mean_fp.pooled_IRR), 
    lower = c(NA, mean_fp.lower.CI, mean_fp.pooled_IRR_CI_low),
    upper = c(NA, mean_fp.upper.CI, mean_fp.pooled_IRR_CI_upp)),
    .Names = c("IRR", "lower", "upper"), 
    row.names = c(NA, -10L), 
    class = "data.frame")

mean_fp_tabletext<-cbind(
  c("Study", mean_fp_author.year, "Summary"),
  c("IRR", mean_fp.txt.IRR, sprintf("%.2f", (mean_fp.pooled_IRR))),
  c("95% CI", mean_fp.txt.CI, mean_fp.txt.pooled.CI),
  c("p-Value", mean_fp.z.p.value, mean_fp.pooled_IRR_z_p_value))

forestplot(mean_fp_tabletext,
           hrzl_lines = list("2" = gpar(lwd=1, col = "#000044"), 
                             "9" = gpar(lwd=1, col = "#000044"),
                             "10" = gpar(lwd=2, col = "#000044")),
           graph.pos = 4,
           graphwidth = unit(5, "cm"),
           line.margin = unit(.05, "cm"),
           xticks = c(0.5, 0.75, 1.0, 1.5, 2.0), 
           txt_gp = fpTxtGp(label = gpar(fontfamily = "", cex=.6), ticks = gpar(cex=.6)),
           mean_fp_structure,new_page = TRUE,
           is.summary=c(TRUE, rep(FALSE,7),TRUE), 
           xlog=TRUE,
           col=fpColors(box="royalblue",line="darkblue", summary="royalblue", hrz_lines = "#444444"),
           colgap = unit(.02,"npc"),
           align = c("l", "c", "c", "r"),
           ci.vertices = TRUE)

x <- unit(0.052, 'npc')
y <- unit(0.048, 'npc')
grid.text(substitute(paste('Model:', a,' = ',b,', credibility interval: (',c,', ',d,')' ), list(a= ~ I^{2}, b=mean_fp.pooled_IRR_I2, c= mean_fp.pooled_IRR_CR_low, d=mean_fp.pooled_IRR_CR_upp)), # add the model fit stats to the forest plot
                            x, y, just = "left", gp = gpar(fontsize=7, font = 1))
########################################
# 5.5: Sensitivity analysis: Mean recidivist events
#####################################
########################################
# 5.5.1: Unconditional generalized linear mixed-effects model with fixed study effects
#####################################
# Model the data with a mixed-effects conditional Poisson regression model (i.e., the binomial-normal model)
mean_recid_rma_glmm_UMFS  <- rma.glmm(measure="IRR", x1i=pt_incidents_treat, t1i=pt_freedays_treat, x2i=pt_incidents_cont, t2i=pt_freedays_cont, data=mean_recid_analysis, model="UM.FS")
summary(mean_recid_rma_glmm_UMFS, digits=3)

predict(mean_recid_rma_glmm_UMFS, transf=exp, digits=3)

# test the deviance for overdispersion 
mean_recid_rma_glmm_UMFS_dev_p<- pchisq((mean_recid_rma_glmm_UMFS$fit.stats[2,1]), df=mean_recid_rma_glmm_UMFS$k-1, lower.tail=FALSE)
mean_recid_rma_glmm_UMFS_dev_p

# Create a vector with the row name, pooled odds ratio, confidence interval, z statistic, z p-value, credibility interval, Q statistic, and I^2
mean_pooled_effect_UMFS <- c('GLMM unconditional model with fixed study effects', exp(mean_recid_rma_glmm_UMFS$b), exp(mean_recid_rma_glmm_UMFS$ci.lb), exp(mean_recid_rma_glmm_UMFS$ci.ub), mean_recid_rma_glmm_UMFS$zval, 
                             mean_recid_rma_glmm_UMFS$pval, exp(predict(mean_recid_rma_glmm_UMFS)$cr.lb), exp(predict(mean_recid_rma_glmm_UMFS)$cr.ub), mean_recid_rma_glmm_UMFS$QE.Wld, mean_recid_rma_glmm_UMFS$QEp.Wld,
                             mean_recid_rma_glmm_UMFS$QE.LRT, mean_recid_rma_glmm_UMFS$QEp.LRT, mean_recid_rma_glmm_UMFS$k-1, mean_recid_rma_glmm_UMFS$I2, mean_recid_rma_glmm_UMFS$fit.stats[2,1], mean_recid_rma_glmm_UMFS_dev_p)

# Add the names for each column
names(mean_pooled_effect_UMFS) <- c("Model", "Incident Rate Ratio", "ConL","ConU", "Z statistic", "Z p-value","CrL", "CrU", "Wald-type test","Wald-type test p-value","Likelihood ratio test", 
                                    "Likelihood ratio test p-value","Dfs", "I^2", "Deviance", "Deviance p-value")
mean_pooled_effect_output <- rbind(mean_pooled_effect_CMEL, mean_pooled_effect_UMFS)
########################################
# 5.5.2: Unconditional generalized linear mixed-effects model with random study effects
#####################################
# Model the data with a mixed-effects conditional Poisson regression model (i.e., the binomial-normal model)
mean_recid_rma_glmm_UMRS  <- rma.glmm(measure="IRR", x1i=pt_incidents_treat, t1i=pt_freedays_treat, x2i=pt_incidents_cont, t2i=pt_freedays_cont, data=mean_recid_analysis, nAGQ=1, model="UM.RS")
summary(mean_recid_rma_glmm_UMRS, digits=3)
predict(mean_recid_rma_glmm_UMRS, transf=exp, digits=3)

# test the deviance for overdispersion 
mean_recid_rma_glmm_UMRS_dev_p<- pchisq((mean_recid_rma_glmm_UMRS$fit.stats[2,1]), df=mean_recid_rma_glmm_UMRS$k-1, lower.tail=FALSE)
mean_recid_rma_glmm_UMRS_dev_p

# Create a vector with the row name, pooled odds ratio, confidence interval, z statistic, z p-value, credibility interval, Q statistic, and I^2
mean_pooled_effect_UMRS <- c('GLMM unconditional model with random study effects', exp(mean_recid_rma_glmm_UMRS$b), exp(mean_recid_rma_glmm_UMRS$ci.lb), exp(mean_recid_rma_glmm_UMRS$ci.ub), mean_recid_rma_glmm_UMRS$zval, 
                             mean_recid_rma_glmm_UMRS$pval, exp(predict(mean_recid_rma_glmm_UMRS)$cr.lb), exp(predict(mean_recid_rma_glmm_UMRS)$cr.ub), mean_recid_rma_glmm_UMRS$QE.Wld, mean_recid_rma_glmm_UMRS$QEp.Wld,
                             mean_recid_rma_glmm_UMRS$QE.LRT, mean_recid_rma_glmm_UMRS$QEp.LRT, mean_recid_rma_glmm_UMRS$k-1, mean_recid_rma_glmm_UMRS$I2, mean_recid_rma_glmm_UMRS$fit.stats[2,1], mean_recid_rma_glmm_UMRS_dev_p)

# Add the names for each column
names(mean_pooled_effect_UMRS) <- c("Model", "Incident Rate Ratio", "ConL","ConU", "Z statistic", "Z p-value","CrL", "CrU", "Wald-type test","Wald-type test p-value","Likelihood ratio test", 
                                    "Likelihood ratio test p-value","Dfs", "I^2", "Deviance", "Deviance p-value")
mean_pooled_effect_output <- rbind(mean_pooled_effect_output, mean_pooled_effect_UMRS)
########################################
# 5.5.3: Publication bias
#####################################
# Rosenthal's Fail Safe N
mean_recid_rosenthal_fsn<- fsn(yi = mean_recid_analysis$yi, vi = mean_recid_analysis$vi)
mean_recid_rosenthal_fsn

# Orwin's Fail Safe N 
mean_recid_orwin_fsn <- fsn(yi = mean_recid_analysis$yi, vi = mean_recid_analysis$vi, type = "Orwin", target = 0.05)
mean_recid_orwin_fsn

########################################
# 6: Moderator analysis: Mean recidivist events
########################################
########################################
# 6.1: Year of publication
#####################################
table(mean_recid_analysis$year, mean_recid_analysis$studyid)
mean_recid_analysis$year_recode <- as.numeric(na_if(mean_recid_analysis$year, 'n.d.'))
# Insert the moderator into the model
mod_mean_recid_year <- rma.glmm(measure="IRR", x1i=pt_incidents_treat, t1i=pt_freedays_treat, x2i=pt_incidents_cont, t2i=pt_freedays_cont, mods = ~ year_recode, data=mean_recid_analysis, model="CM.EL")
# Note: a warning message will appear noting that studies with NAs were omitted and that some yi/vi values were NA. This message can be ignored here and in subsequent moderators

# Create an object for the desired information and create a 'moderator_output' data frame 
mod_mean_recid_list<- c('Year of publication', 'methodological', 'Beta_weight', mod_mean_recid_year$k, mod_mean_recid_year$beta[2, 1], mod_mean_recid_year$se[2], mod_mean_recid_year$zval[2], mod_mean_recid_year$pval[2], 
                       mod_mean_recid_year$ci.lb[2], mod_mean_recid_year$ci.ub[2], mod_mean_recid_year$k- length(mod_mean_recid_year$b), NA, NA, mod_mean_recid_year$I2)
# name the object's columns 
names(mod_mean_recid_list) <- c("name", "mod_category", "level", "level_k", "est_LIRR", "est_SE","est_z","est_z_p","est_cl","est_cu", "res_DF", "Qres", "Qres_p", "I2")
mean_moderator_output <- mod_mean_recid_list
########################################
# 6.2: Jurisdiction
#####################################
table(mean_recid_analysis$jurisdiction, mean_recid_analysis$studyid)
########################################
# 6.2.1: Jurisdiction - New Zealand 
#####################################
# Select the subgroup in the model 
mod_mean_recid_jurisdiction_nz <- rma.glmm(measure="IRR", x1i=pt_incidents_treat, t1i=pt_freedays_treat, x2i=pt_incidents_cont, t2i=pt_freedays_cont, 
                                           subset=(jurisdiction== "NZ"), data=mean_recid_analysis, model="CM.EL")
# Create an object for the desired information  
mod_mean_recid_jurisdiction_nz_list <- c('Jurisdiction', 'programmatic', 'New Zealand (ref)', mod_mean_recid_jurisdiction_nz$k, mod_mean_recid_jurisdiction_nz$beta[1], mod_mean_recid_jurisdiction_nz$se[1], mod_mean_recid_jurisdiction_nz$zval[1], mod_mean_recid_jurisdiction_nz$pval[1], 
                                   mod_mean_recid_jurisdiction_nz$ci.lb[1], mod_mean_recid_jurisdiction_nz$ci.ub[1], mod_mean_recid_jurisdiction_nz$k- length(mod_mean_recid_jurisdiction_nz$b), NA, NA, 
                                   mod_mean_recid_jurisdiction_nz$I2)
# name the object's columns and bind to mean_moderator_output
names(mod_mean_recid_jurisdiction_nz_list) <- c("name", "mod_category", "level", "level_k", "est_LIRR", "est_SE","est_z","est_z_p","est_cl","est_cu", "res_DF", "Qres", "Qres_p", "I2")
mean_moderator_output <- bind_rows(mean_moderator_output, mod_mean_recid_jurisdiction_nz_list)
########################################
# 6.2.2: Jurisdiction - New South Wales
#####################################
# Select the subgroup in the model 
mod_mean_recid_jurisdiction_nsw <- rma.glmm(measure="IRR", x1i=pt_incidents_treat, t1i=pt_freedays_treat, x2i=pt_incidents_cont, t2i=pt_freedays_cont, 
                                            subset=(jurisdiction== "NSW"), data=mean_recid_analysis, model="CM.EL")
# Create an object for the desired information  
mod_mean_recid_jurisdiction_nsw_list <- c('Jurisdiction', 'programmatic', 'New South Wales', mod_mean_recid_jurisdiction_nsw$k, mod_mean_recid_jurisdiction_nsw$beta[1], mod_mean_recid_jurisdiction_nsw$se[1], mod_mean_recid_jurisdiction_nsw$zval[1], mod_mean_recid_jurisdiction_nsw$pval[1], 
                                          mod_mean_recid_jurisdiction_nsw$ci.lb[1], mod_mean_recid_jurisdiction_nsw$ci.ub[1], mod_mean_recid_jurisdiction_nsw$k- length(mod_mean_recid_jurisdiction_nsw$b), NA, NA, 
                                          mod_mean_recid_jurisdiction_nsw$I2)
# name the object's columns and bind to mean_moderator_output
names(mod_mean_recid_jurisdiction_nsw_list) <- c("name", "mod_category", "level", "level_k", "est_LIRR", "est_SE","est_z","est_z_p","est_cl","est_cu", "res_DF", "Qres", "Qres_p", "I2")
mean_moderator_output <- bind_rows(mean_moderator_output, mod_mean_recid_jurisdiction_nsw_list)
########################################
# 6.2.3: Jurisdiction - Queensland
#####################################
# Select the subgroup in the model 
mod_mean_recid_jurisdiction_qld <- rma.glmm(measure="IRR", x1i=pt_incidents_treat, t1i=pt_freedays_treat, x2i=pt_incidents_cont, t2i=pt_freedays_cont, 
                                            subset=(jurisdiction== "QLD"), data=mean_recid_analysis, model="CM.EL")
# Create an object for the desired information  
mod_mean_recid_jurisdiction_qld_list <- c('Jurisdiction', 'programmatic', 'Queensland', mod_mean_recid_jurisdiction_qld$k, mod_mean_recid_jurisdiction_qld$beta[1], mod_mean_recid_jurisdiction_qld$se[1], mod_mean_recid_jurisdiction_qld$zval[1], mod_mean_recid_jurisdiction_qld$pval[1], 
                                   mod_mean_recid_jurisdiction_qld$ci.lb[1], mod_mean_recid_jurisdiction_qld$ci.ub[1], mod_mean_recid_jurisdiction_qld$k- length(mod_mean_recid_jurisdiction_qld$b), NA, NA, 
                                   mod_mean_recid_jurisdiction_qld$I2)
# name the object's columns and bind to mean_moderator_output
names(mod_mean_recid_jurisdiction_qld_list) <- c("name", "mod_category", "level", "level_k", "est_LIRR", "est_SE","est_z","est_z_p","est_cl","est_cu", "res_DF", "Qres", "Qres_p", "I2")
mean_moderator_output <- bind_rows(mean_moderator_output, mod_mean_recid_jurisdiction_qld_list)
########################################
# 6.2.4: Jurisdiction - Australia
#####################################
# Select the subgroup in the model 
mod_mean_recid_jurisdiction_aus <- rma.glmm(measure="IRR", x1i=pt_incidents_treat, t1i=pt_freedays_treat, x2i=pt_incidents_cont, t2i=pt_freedays_cont, 
                                            subset= !(jurisdiction== "NZ"), data=mean_recid_analysis, model="CM.EL")
# Create an object for the desired information  
mod_mean_recid_jurisdiction_aus_list <- c('Jurisdiction', 'programmatic', 'Australia', mod_mean_recid_jurisdiction_aus$k, mod_mean_recid_jurisdiction_aus$beta[1], mod_mean_recid_jurisdiction_aus$se[1], mod_mean_recid_jurisdiction_aus$zval[1], mod_mean_recid_jurisdiction_aus$pval[1], 
                                    mod_mean_recid_jurisdiction_aus$ci.lb[1], mod_mean_recid_jurisdiction_aus$ci.ub[1], mod_mean_recid_jurisdiction_aus$k- length(mod_mean_recid_jurisdiction_aus$b), NA, NA, 
                                    mod_mean_recid_jurisdiction_aus$I2)
# name the object's columns and bind to mean_moderator_output
names(mod_mean_recid_jurisdiction_aus_list) <- c("name", "mod_category", "level", "level_k", "est_LIRR", "est_SE","est_z","est_z_p","est_cl","est_cu", "res_DF", "Qres", "Qres_p", "I2")
mean_moderator_output <- bind_rows(mean_moderator_output, mod_mean_recid_jurisdiction_aus_list)
########################################
# 6.3: Outcome
#####################################
table(mean_recid_analysis$outcome, mean_recid_analysis$studyid)
# recode the variable to account split the outcomes into offences and proven offences 
mean_recid_analysis$outcome_recode <- recode(mean_recid_analysis$outcome, "offence episodes" = "offences", "theft or drug offences" = "offences", "court appearances" = "offences")
########################################
# 6.3.1: Outcome - Offences 
#####################################
# Select the subgroup in the model 
mod_mean_recid_outcome_off <- rma.glmm(measure="IRR", x1i=pt_incidents_treat, t1i=pt_freedays_treat, x2i=pt_incidents_cont, t2i=pt_freedays_cont, 
                                       subset=(outcome_recode== "offences"), data=mean_recid_analysis, model="CM.EL")
# Create an object for the desired information  
mod_mean_recid_outcome_off_list <- c('Outcome', 'methodological', 'Offences (ref)', mod_mean_recid_outcome_off$k, mod_mean_recid_outcome_off$beta[1], mod_mean_recid_outcome_off$se[1], mod_mean_recid_outcome_off$zval[1], mod_mean_recid_outcome_off$pval[1], 
                                   mod_mean_recid_outcome_off$ci.lb[1], mod_mean_recid_outcome_off$ci.ub[1], mod_mean_recid_outcome_off$k- length(mod_mean_recid_outcome_off$b), NA, NA, 
                                   mod_mean_recid_outcome_off$I2)
# name the object's columns and bind to mean_moderator_output
names(mod_mean_recid_outcome_off_list) <- c("name", "mod_category", "level", "level_k", "est_LIRR", "est_SE","est_z","est_z_p","est_cl","est_cu", "res_DF", "Qres", "Qres_p", "I2")
mean_moderator_output <- bind_rows(mean_moderator_output, mod_mean_recid_outcome_off_list)
########################################
# 6.3.1: Outcome - Proven offences 
#####################################
# Select the subgroup in the model 
mod_mean_recid_outcome_prov_off <- rma.glmm(measure="IRR", x1i=pt_incidents_treat, t1i=pt_freedays_treat, x2i=pt_incidents_cont, t2i=pt_freedays_cont, 
                                            subset=(outcome_recode== "proven offence"), data=mean_recid_analysis, model="CM.EL")
# Create an object for the desired information  
mod_mean_recid_outcome_prov_off_list <- c('Outcome', 'methodological', 'Proven offences', mod_mean_recid_outcome_prov_off$k, mod_mean_recid_outcome_prov_off$beta[1], mod_mean_recid_outcome_prov_off$se[1], mod_mean_recid_outcome_prov_off$zval[1], mod_mean_recid_outcome_prov_off$pval[1], 
                               mod_mean_recid_outcome_prov_off$ci.lb[1], mod_mean_recid_outcome_prov_off$ci.ub[1], mod_mean_recid_outcome_prov_off$k- length(mod_mean_recid_outcome_prov_off$b), NA, NA, 
                               mod_mean_recid_outcome_prov_off$I2)
# name the object's columns and bind to mean_moderator_output
names(mod_mean_recid_outcome_prov_off_list) <- c("name", "mod_category", "level", "level_k", "est_LIRR", "est_SE","est_z","est_z_p","est_cl","est_cu", "res_DF", "Qres", "Qres_p", "I2")
mean_moderator_output <- bind_rows(mean_moderator_output, mod_mean_recid_outcome_prov_off_list)
########################################
# 6.4: Measure
#####################################
table(mean_recid_analysis$measure, mean_recid_analysis$studyid)
# Note: this variable will not be included in the mean recidivism moderator/ subgroup analysis because it would need to be recoded as follows, which would be a duplicate subgroup analysis to '6.3: Outcome'
# e.g., recode(mean_recid_analysis$measure, "offences" = "annualized offence episodes", "court appearances (estimated)" = "annualized offence episodes", "mean proven offences per 100 participants" = "annualized offences", "mean offence episodes" = "annualized offence episodes", "mean annualized arrests" = "annualized offence episodes")
########################################
# 6.5: Time status of measure
#####################################
table(mean_recid_analysis$time_status_of_measure, mean_recid_analysis$studyid)
# Note: there are insufficient numbers across groups to compute a moderator/subgroup analysis
########################################
# 6.6: Court setting
#####################################
table(mean_recid_analysis$setting, mean_recid_analysis$studyid)
# Note: there are insufficient numbers across groups to compute a moderator/subgroup analysis
########################################
# 6.7: Study groups
#####################################
table(mean_recid_analysis$studygroups, mean_recid_analysis$studyid)
# Note: this variable will not be included in the mean recidivism moderator/ subgroup analysis because two of the 'combined groups' cannot be meaningfully combined: one represents 
#        combined comparison groups while the other represents combined graduates/failures. There will therefore be groups containing single outcomes which cannot be analysed. 
########################################
# 6.8: Study groups (alternative)
#####################################
table(mean_recid_analysis$studygroups_alternative, mean_recid_analysis$studyid)
# Note: there are insufficient numbers across groups to compute a moderator/subgroup analysis
########################################
# 6.9: Intention-to-treat
#####################################
table(mean_recid_analysis$ITT, mean_recid_analysis$studyid)
# Note: there are insufficient numbers across groups to compute a moderator/subgroup analysis
########################################
# 6.10: Lifetime arrests - Treatment
#####################################
table(mean_recid_analysis$LT_arrests_treat, mean_recid_analysis$studyid)
# Note: there are insufficient numbers across groups to compute a moderator/subgroup analysis
########################################
# 6.11: Lifetime arrests - Control
#####################################
table(mean_recid_analysis$LT_arrests_cont, mean_recid_analysis$studyid)
# Note: there are insufficient numbers across groups to compute a moderator/subgroup analysis
########################################
# 6.12: Lifetime convictions - Treatment
#####################################
table(mean_recid_analysis$LT_convictions_treat, mean_recid_analysis$studyid)
# Note: there are insufficient numbers across groups to compute a moderator/subgroup analysis
########################################
# 6.13: Lifetime convictions - Control
#####################################
table(mean_recid_analysis$LT_convictions_cont, mean_recid_analysis$studyid)
# Note: there are insufficient numbers across groups to compute a moderator/subgroup analysis
########################################
# 6.14: Mean age - Treatment
#####################################
table(mean_recid_analysis$mean_age_treat, mean_recid_analysis$studyid)
# Insert the moderator into the model
mod_mean_recid_mean_age_tr <- rma.glmm(measure="IRR", x1i=pt_incidents_treat, t1i=pt_freedays_treat, x2i=pt_incidents_cont, t2i=pt_freedays_cont, mods = ~ mean_age_treat, data=mean_recid_analysis, model="CM.EL")
# Create an object for the desired information and create a 'moderator_output' data frame 
mod_mean_recid_mean_age_tr_list<- c('Mean age (Tr)', 'sample characteristics', 'Beta_weight', mod_mean_recid_mean_age_tr$k, mod_mean_recid_mean_age_tr$beta[2, 1], mod_mean_recid_mean_age_tr$se[2], mod_mean_recid_mean_age_tr$zval[2], mod_mean_recid_mean_age_tr$pval[2], 
                       mod_mean_recid_mean_age_tr$ci.lb[2], mod_mean_recid_mean_age_tr$ci.ub[2], mod_mean_recid_mean_age_tr$k- length(mod_mean_recid_mean_age_tr$b), NA, NA, mod_mean_recid_mean_age_tr$I2)
# name the object's columns 
names(mod_mean_recid_mean_age_tr_list) <- c("name", "mod_category", "level", "level_k", "est_LIRR", "est_SE","est_z","est_z_p","est_cl","est_cu", "res_DF", "Qres", "Qres_p", "I2")
mean_moderator_output <- bind_rows(mean_moderator_output, mod_mean_recid_mean_age_tr_list)
########################################
# 6.15: Mean age - Control
#####################################
table(mean_recid_analysis$mean_age_cont, mean_recid_analysis$studyid)
# Insert the moderator into the model
mod_mean_recid_mean_age_con <- rma.glmm(measure="IRR", x1i=pt_incidents_treat, t1i=pt_freedays_treat, x2i=pt_incidents_cont, t2i=pt_freedays_cont, mods = ~ mean_age_cont, data=mean_recid_analysis, model="CM.EL")
# Create an object for the desired information and create a 'moderator_output' data frame 
mod_mean_recid_mean_age_con_list<- c('Mean age (Con)', 'sample characteristics', 'Beta_weight', mod_mean_recid_mean_age_con$k, mod_mean_recid_mean_age_con$beta[2, 1], mod_mean_recid_mean_age_con$se[2], mod_mean_recid_mean_age_con$zval[2], mod_mean_recid_mean_age_con$pval[2], 
                              mod_mean_recid_mean_age_con$ci.lb[2], mod_mean_recid_mean_age_con$ci.ub[2], mod_mean_recid_mean_age_con$k- length(mod_mean_recid_mean_age_con$b), NA, NA, mod_mean_recid_mean_age_con$I2)
# name the object's columns 
names(mod_mean_recid_mean_age_con_list) <- c("name", "mod_category", "level", "level_k", "est_LIRR", "est_SE","est_z","est_z_p","est_cl","est_cu", "res_DF", "Qres", "Qres_p", "I2")
mean_moderator_output <- bind_rows(mean_moderator_output, mod_mean_recid_mean_age_con_list)
########################################
# 6.16: Proportion male - Treatment
#####################################
table(mean_recid_analysis$male_._treat, mean_recid_analysis$studyid)
# Insert the moderator into the model
# note we will exclude Study ID 4b as 'sex' was controlled for and included in the database is un-adjusted. 
mod_mean_recid_prop_male_tr <- rma.glmm(measure="IRR", x1i=pt_incidents_treat, t1i=pt_freedays_treat, x2i=pt_incidents_cont, t2i=pt_freedays_cont, mods = ~ male_._treat, subset=(!studyid== "4b"), data=mean_recid_analysis, model="CM.EL")
# Create an object for the desired information and create a 'moderator_output' data frame 
mod_mean_recid_prop_male_tr_list<- c('Proportion male (Tr)', 'sample characteristics', 'Beta_weight', mod_mean_recid_prop_male_tr$k, mod_mean_recid_prop_male_tr$beta[2, 1], mod_mean_recid_prop_male_tr$se[2], mod_mean_recid_prop_male_tr$zval[2], mod_mean_recid_prop_male_tr$pval[2], 
                              mod_mean_recid_prop_male_tr$ci.lb[2], mod_mean_recid_prop_male_tr$ci.ub[2], mod_mean_recid_prop_male_tr$k- length(mod_mean_recid_prop_male_tr$b), NA, NA, mod_mean_recid_prop_male_tr$I2)
# name the object's columns 
names(mod_mean_recid_prop_male_tr_list) <- c("name", "mod_category", "level", "level_k", "est_LIRR", "est_SE","est_z","est_z_p","est_cl","est_cu", "res_DF", "Qres", "Qres_p", "I2")
mean_moderator_output <- bind_rows(mean_moderator_output, mod_mean_recid_prop_male_tr_list)
########################################
# 6.17: Proportion male - Control
#####################################
table(mean_recid_analysis$male_._cont, mean_recid_analysis$studyid)
# Insert the moderator into the model
# note we will exclude Study ID 4b as 'sex' was controlled for and included in the database is un-adjusted. 
mod_mean_recid_prop_male_con <- rma.glmm(measure="IRR", x1i=pt_incidents_treat, t1i=pt_freedays_treat, x2i=pt_incidents_cont, t2i=pt_freedays_cont, mods = ~ male_._cont, subset=(!studyid== "4b"), data=mean_recid_analysis, model="CM.EL")
# Create an object for the desired information and create a 'moderator_output' data frame 
mod_mean_recid_prop_male_con_list<- c('Proportion male (Con)', 'sample characteristics', 'Beta_weight', mod_mean_recid_prop_male_con$k, mod_mean_recid_prop_male_con$beta[2, 1], mod_mean_recid_prop_male_con$se[2], mod_mean_recid_prop_male_con$zval[2], mod_mean_recid_prop_male_con$pval[2], 
                               mod_mean_recid_prop_male_con$ci.lb[2], mod_mean_recid_prop_male_con$ci.ub[2], mod_mean_recid_prop_male_con$k- length(mod_mean_recid_prop_male_con$b), NA, NA, mod_mean_recid_prop_male_con$I2)
# name the object's columns 
names(mod_mean_recid_prop_male_con_list) <- c("name", "mod_category", "level", "level_k", "est_LIRR", "est_SE","est_z","est_z_p","est_cl","est_cu", "res_DF", "Qres", "Qres_p", "I2")
mean_moderator_output <- bind_rows(mean_moderator_output, mod_mean_recid_prop_male_con_list)
########################################
# 6.18: Proportion ATSI or Maori - Treatment 
#####################################
table(mean_recid_analysis$ATSI_or_MAORI_._treat, mean_recid_analysis$studyid)
# Insert the moderator into the model
# note we will exclude Study ID 4b as 'sex' was controlled for and included in the database is un-adjusted. 
mod_mean_recid_prop_ATSI_or_Maori_tr <- rma.glmm(measure="IRR", x1i=pt_incidents_treat, t1i=pt_freedays_treat, x2i=pt_incidents_cont, t2i=pt_freedays_cont, mods = ~ ATSI_or_MAORI_._treat, subset=(!studyid== "4b"), data=mean_recid_analysis, model="CM.EL")
# Create an object for the desired information and create a 'moderator_output' data frame 
mod_mean_recid_prop_ATSI_or_Maori_tr_list<- c('Proportion ATSI or Maori (Tr)', 'sample characteristics', 'Beta_weight', mod_mean_recid_prop_ATSI_or_Maori_tr$k, mod_mean_recid_prop_ATSI_or_Maori_tr$beta[2, 1], mod_mean_recid_prop_ATSI_or_Maori_tr$se[2], mod_mean_recid_prop_ATSI_or_Maori_tr$zval[2], mod_mean_recid_prop_ATSI_or_Maori_tr$pval[2], 
                                mod_mean_recid_prop_ATSI_or_Maori_tr$ci.lb[2], mod_mean_recid_prop_ATSI_or_Maori_tr$ci.ub[2], mod_mean_recid_prop_ATSI_or_Maori_tr$k- length(mod_mean_recid_prop_ATSI_or_Maori_tr$b), NA, NA, mod_mean_recid_prop_ATSI_or_Maori_tr$I2)
# name the object's columns 
names(mod_mean_recid_prop_ATSI_or_Maori_tr_list) <- c("name", "mod_category", "level", "level_k", "est_LIRR", "est_SE","est_z","est_z_p","est_cl","est_cu", "res_DF", "Qres", "Qres_p", "I2")
mean_moderator_output <- bind_rows(mean_moderator_output, mod_mean_recid_prop_ATSI_or_Maori_tr_list)
########################################
# 6.19: Proportion ATSI or Maori - Control 
#####################################
table(mean_recid_analysis$ATSI_or_MAORI_._cont, mean_recid_analysis$studyid)
# Insert the moderator into the model
# note we will exclude Study ID 4b as 'sex' was controlled for and included in the database is un-adjusted. 
mod_mean_recid_prop_ATSI_or_Maori_con <- rma.glmm(measure="IRR", x1i=pt_incidents_treat, t1i=pt_freedays_treat, x2i=pt_incidents_cont, t2i=pt_freedays_cont, mods = ~ ATSI_or_MAORI_._cont, subset=(!studyid== "4b"), data=mean_recid_analysis, model="CM.EL")
# Create an object for the desired information and create a 'moderator_output' data frame 
mod_mean_recid_prop_ATSI_or_Maori_con_list<- c('Proportion ATSI or Maori (Con)', 'sample characteristics', 'Beta_weight', mod_mean_recid_prop_ATSI_or_Maori_con$k, mod_mean_recid_prop_ATSI_or_Maori_con$beta[2, 1], mod_mean_recid_prop_ATSI_or_Maori_con$se[2], mod_mean_recid_prop_ATSI_or_Maori_con$zval[2], mod_mean_recid_prop_ATSI_or_Maori_con$pval[2], 
                                              mod_mean_recid_prop_ATSI_or_Maori_con$ci.lb[2], mod_mean_recid_prop_ATSI_or_Maori_con$ci.ub[2], mod_mean_recid_prop_ATSI_or_Maori_con$k- length(mod_mean_recid_prop_ATSI_or_Maori_con$b), NA, NA, mod_mean_recid_prop_ATSI_or_Maori_con$I2)
# name the object's columns 
names(mod_mean_recid_prop_ATSI_or_Maori_con_list) <- c("name", "mod_category", "level", "level_k", "est_LIRR", "est_SE","est_z","est_z_p","est_cl","est_cu", "res_DF", "Qres", "Qres_p", "I2")
mean_moderator_output <- bind_rows(mean_moderator_output, mod_mean_recid_prop_ATSI_or_Maori_con_list)
########################################
# 6.20: Pre-treatment mean - Treatment 
#####################################
table(mean_recid_analysis$bl_treat_m, mean_recid_analysis$studyid)
# Insert the moderator into the model
mod_mean_recid_pre_treat_mean_tr <- rma.glmm(measure="IRR", x1i=pt_incidents_treat, t1i=pt_freedays_treat, x2i=pt_incidents_cont, t2i=pt_freedays_cont, mods = ~ bl_treat_m, data=mean_recid_analysis, model="CM.EL")
# Create an object for the desired information and create a 'moderator_output' data frame 
mod_mean_recid_pre_treat_mean_tr_list<- c('Pre-treatment mean (Tr)', 'sample characteristics', 'Beta_weight', mod_mean_recid_pre_treat_mean_tr$k, mod_mean_recid_pre_treat_mean_tr$beta[2, 1], mod_mean_recid_pre_treat_mean_tr$se[2], mod_mean_recid_pre_treat_mean_tr$zval[2], mod_mean_recid_pre_treat_mean_tr$pval[2], 
                                              mod_mean_recid_pre_treat_mean_tr$ci.lb[2], mod_mean_recid_pre_treat_mean_tr$ci.ub[2], mod_mean_recid_pre_treat_mean_tr$k- length(mod_mean_recid_pre_treat_mean_tr$b), NA, NA, mod_mean_recid_pre_treat_mean_tr$I2)
# name the object's columns 
names(mod_mean_recid_pre_treat_mean_tr_list) <- c("name", "mod_category", "level", "level_k", "est_LIRR", "est_SE","est_z","est_z_p","est_cl","est_cu", "res_DF", "Qres", "Qres_p", "I2")
mean_moderator_output <- bind_rows(mean_moderator_output, mod_mean_recid_pre_treat_mean_tr_list)
########################################
# 6.21: Pre-treatment mean - Control 
#####################################
table(mean_recid_analysis$bl_cont_m, mean_recid_analysis$studyid)
# Insert the moderator into the model
mod_mean_recid_pre_treat_mean_con <- rma.glmm(measure="IRR", x1i=pt_incidents_treat, t1i=pt_freedays_treat, x2i=pt_incidents_cont, t2i=pt_freedays_cont, mods = ~ bl_cont_m, data=mean_recid_analysis, model="CM.EL")
# Create an object for the desired information and create a 'moderator_output' data frame 
mod_mean_recid_pre_treat_mean_con_list<- c('Pre-treatment mean (Con)', 'sample characteristics', 'Beta_weight', mod_mean_recid_pre_treat_mean_con$k, mod_mean_recid_pre_treat_mean_con$beta[2, 1], mod_mean_recid_pre_treat_mean_con$se[2], mod_mean_recid_pre_treat_mean_con$zval[2], mod_mean_recid_pre_treat_mean_con$pval[2], 
                                          mod_mean_recid_pre_treat_mean_con$ci.lb[2], mod_mean_recid_pre_treat_mean_con$ci.ub[2], mod_mean_recid_pre_treat_mean_con$k- length(mod_mean_recid_pre_treat_mean_con$b), NA, NA, mod_mean_recid_pre_treat_mean_con$I2)
# name the object's columns 
names(mod_mean_recid_pre_treat_mean_con_list) <- c("name", "mod_category", "level", "level_k", "est_LIRR", "est_SE","est_z","est_z_p","est_cl","est_cu", "res_DF", "Qres", "Qres_p", "I2")
mean_moderator_output <- bind_rows(mean_moderator_output, mod_mean_recid_pre_treat_mean_con_list)
########################################
# 6.22: Post-test period  
#####################################
table(mean_recid_analysis$pt_period_months, mean_recid_analysis$studyid)
# Note: there are insufficient numbers across groups to compute a moderator/subgroup analysis
########################################
# 6.23: Post-test begins 
#####################################
table(mean_recid_analysis$pt_begins, mean_recid_analysis$studyid)
# Note: this variable will not be included in the moderator analysis because the it is effectively grouped (into those measured during and post enrollment) by the 'measured_during_or_post_pt' that
#       it is analysed directly below. 
########################################
# 6.24: Measured during or post treatment
#####################################
table(mean_recid_analysis$measured_during_or_post_pt, mean_recid_analysis$studyid)
########################################
# 6.24.1: Measured during or post treatment - During
#####################################
# Select the subgroup in the model 
mod_mean_recid_pt_during <- rma.glmm(measure="IRR", x1i=pt_incidents_treat, t1i=pt_freedays_treat, x2i=pt_incidents_cont, t2i=pt_freedays_cont, 
                                     subset=(measured_during_or_post_pt== "During"), data=mean_recid_analysis, model="CM.EL")
# Create an object for the desired information  
mod_mean_recid_pt_during_list <- c('Measured during or post treatment', 'methodological', 'During (ref)', mod_mean_recid_pt_during$k, mod_mean_recid_pt_during$beta[1], mod_mean_recid_pt_during$se[1], mod_mean_recid_pt_during$zval[1], mod_mean_recid_pt_during$pval[1], 
                                         mod_mean_recid_pt_during$ci.lb[1], mod_mean_recid_pt_during$ci.ub[1], mod_mean_recid_pt_during$k- length(mod_mean_recid_pt_during$b), NA, NA, 
                                         mod_mean_recid_pt_during$I2)
# name the object's columns and bind to mean_moderator_output
names(mod_mean_recid_pt_during_list) <- c("name", "mod_category", "level", "level_k", "est_LIRR", "est_SE","est_z","est_z_p","est_cl","est_cu", "res_DF", "Qres", "Qres_p", "I2")
mean_moderator_output <- bind_rows(mean_moderator_output, mod_mean_recid_pt_during_list)
########################################
# 6.24.2: Measured during or post treatment - Post
#####################################
# Select the subgroup in the model 
mod_mean_recid_pt_post <- rma.glmm(measure="IRR", x1i=pt_incidents_treat, t1i=pt_freedays_treat, x2i=pt_incidents_cont, t2i=pt_freedays_cont, 
                                     subset=(measured_during_or_post_pt== "Post"), data=mean_recid_analysis, model="CM.EL")
# Create an object for the desired information  
mod_mean_recid_pt_post_list <- c('Measured during or post treatment', 'methodological', 'Post', mod_mean_recid_pt_post$k, mod_mean_recid_pt_post$beta[1], mod_mean_recid_pt_post$se[1], mod_mean_recid_pt_post$zval[1], mod_mean_recid_pt_post$pval[1], 
                                   mod_mean_recid_pt_post$ci.lb[1], mod_mean_recid_pt_post$ci.ub[1], mod_mean_recid_pt_post$k- length(mod_mean_recid_pt_post$b), NA, NA, 
                                   mod_mean_recid_pt_post$I2)
# name the object's columns and bind to mean_moderator_output
names(mod_mean_recid_pt_post_list) <- c("name", "mod_category", "level", "level_k", "est_LIRR", "est_SE","est_z","est_z_p","est_cl","est_cu", "res_DF", "Qres", "Qres_p", "I2")
mean_moderator_output <- bind_rows(mean_moderator_output, mod_mean_recid_pt_post_list)
########################################
# 6.25: Program duration minimum (months)
#####################################
table(mean_recid_analysis$Program_duration_minimum_months, mean_recid_analysis$studyid)
# Note: there are insufficient numbers across groups to compute a moderator/subgroup analysis
########################################
# 6.26: Program duration average (months)
#####################################
table(mean_recid_analysis$Program_duration_average_months, mean_recid_analysis$studyid)
# Insert the moderator into the model
mod_mean_recid_prog_dur_avg <- rma.glmm(measure="IRR", x1i=pt_incidents_treat, t1i=pt_freedays_treat, x2i=pt_incidents_cont, t2i=pt_freedays_cont, 
                                             mods = ~ Program_duration_average_months, data=mean_recid_analysis, model="CM.EL")
# Create an object for the desired information and create a 'moderator_output' data frame 
mod_mean_recid_prog_dur_avg_list<- c('Program duration average (months)', 'programmatic', 'Beta_weight', mod_mean_recid_prog_dur_avg$k, mod_mean_recid_prog_dur_avg$beta[2, 1], mod_mean_recid_prog_dur_avg$se[2], mod_mean_recid_prog_dur_avg$zval[2], mod_mean_recid_prog_dur_avg$pval[2], 
                                          mod_mean_recid_prog_dur_avg$ci.lb[2], mod_mean_recid_prog_dur_avg$ci.ub[2], mod_mean_recid_prog_dur_avg$k- length(mod_mean_recid_prog_dur_avg$b), NA, NA, mod_mean_recid_prog_dur_avg$I2)
# name the object's columns 
names(mod_mean_recid_prog_dur_avg_list) <- c("name", "mod_category", "level", "level_k", "est_LIRR", "est_SE","est_z","est_z_p","est_cl","est_cu", "res_DF", "Qres", "Qres_p", "I2")
mean_moderator_output <- bind_rows(mean_moderator_output, mod_mean_recid_prog_dur_avg_list)
########################################
# 6.27: Post treatment months - Treatment 
#####################################
table(mean_recid_analysis$Post_program_PT_months_treat, mean_recid_analysis$studyid)
# Insert the moderator into the model
mod_mean_recid_post_prog_months <- rma.glmm(measure="IRR", x1i=pt_incidents_treat, t1i=pt_freedays_treat, x2i=pt_incidents_cont, t2i=pt_freedays_cont, 
                                        mods = ~ Post_program_PT_months_treat, data=mean_recid_analysis, model="CM.EL")
# Create an object for the desired information and create a 'moderator_output' data frame 
mod_mean_recid_post_prog_months_list<- c('Post treatment months (Tr)', 'methodological', 'Beta_weight', mod_mean_recid_post_prog_months$k, mod_mean_recid_post_prog_months$beta[2, 1], mod_mean_recid_post_prog_months$se[2], mod_mean_recid_post_prog_months$zval[2], mod_mean_recid_post_prog_months$pval[2], 
                                     mod_mean_recid_post_prog_months$ci.lb[2], mod_mean_recid_post_prog_months$ci.ub[2], mod_mean_recid_post_prog_months$k- length(mod_mean_recid_post_prog_months$b), NA, NA, mod_mean_recid_post_prog_months$I2)
# name the object's columns 
names(mod_mean_recid_post_prog_months_list) <- c("name", "mod_category", "level", "level_k", "est_LIRR", "est_SE","est_z","est_z_p","est_cl","est_cu", "res_DF", "Qres", "Qres_p", "I2")
mean_moderator_output <- bind_rows(mean_moderator_output, mod_mean_recid_post_prog_months_list)
########################################
# 6.28: Post treatment months - Control 
#####################################
table(mean_recid_analysis$Post_program_PT_months_cont, mean_recid_analysis$studyid)
# Note: there are insufficient numbers across groups to compute a moderator/subgroup analysis
########################################
# 6.29: Charge level
#####################################
table(mean_recid_analysis$charge_level, mean_recid_analysis$studyid)
# Note: there are insufficient numbers across groups to compute a moderator/subgroup analysis
########################################
# 6.30: Accepted violent offenders
#####################################
table(mean_recid_analysis$accepeted_violent, mean_recid_analysis$studyid)
########################################
# 6.30.1: Accepted violent offenders - No
#####################################
# Select the subgroup in the model 
mod_mean_recid_acc_vio_no <- rma.glmm(measure="IRR", x1i=pt_incidents_treat, t1i=pt_freedays_treat, x2i=pt_incidents_cont, t2i=pt_freedays_cont, 
                                     subset=(accepeted_violent== "No"), data=mean_recid_analysis, model="CM.EL")
# Create an object for the desired information  
mod_mean_recid_acc_vio_no_list <- c('Accepted violent offenders', 'programmatic', 'No (ref)', mod_mean_recid_acc_vio_no$k, mod_mean_recid_acc_vio_no$beta[1], mod_mean_recid_acc_vio_no$se[1], mod_mean_recid_acc_vio_no$zval[1], mod_mean_recid_acc_vio_no$pval[1], 
                                   mod_mean_recid_acc_vio_no$ci.lb[1], mod_mean_recid_acc_vio_no$ci.ub[1], mod_mean_recid_acc_vio_no$k- length(mod_mean_recid_acc_vio_no$b), NA, NA, 
                                   mod_mean_recid_acc_vio_no$I2)
# name the object's columns and bind to mean_moderator_output
names(mod_mean_recid_acc_vio_no_list) <- c("name", "mod_category", "level", "level_k", "est_LIRR", "est_SE","est_z","est_z_p","est_cl","est_cu", "res_DF", "Qres", "Qres_p", "I2")
mean_moderator_output <- bind_rows(mean_moderator_output, mod_mean_recid_acc_vio_no_list)
########################################
# 6.30.2: Accepted violent offenders - Yes
#####################################
# Select the subgroup in the model 
mod_mean_recid_acc_vio_yes <- rma.glmm(measure="IRR", x1i=pt_incidents_treat, t1i=pt_freedays_treat, x2i=pt_incidents_cont, t2i=pt_freedays_cont, 
                                      subset=(accepeted_violent== "Yes"), data=mean_recid_analysis, model="CM.EL")
# Create an object for the desired information  
mod_mean_recid_acc_vio_yes_list <- c('Accepted violent offenders', 'programmatic', 'Yes', mod_mean_recid_acc_vio_yes$k, mod_mean_recid_acc_vio_yes$beta[1], mod_mean_recid_acc_vio_yes$se[1], mod_mean_recid_acc_vio_yes$zval[1], mod_mean_recid_acc_vio_yes$pval[1], 
                                    mod_mean_recid_acc_vio_yes$ci.lb[1], mod_mean_recid_acc_vio_yes$ci.ub[1], mod_mean_recid_acc_vio_yes$k- length(mod_mean_recid_acc_vio_yes$b), NA, NA, 
                                    mod_mean_recid_acc_vio_yes$I2)
# name the object's columns and bind to mean_moderator_output
names(mod_mean_recid_acc_vio_yes_list) <- c("name", "mod_category", "level", "level_k", "est_LIRR", "est_SE","est_z","est_z_p","est_cl","est_cu", "res_DF", "Qres", "Qres_p", "I2")
mean_moderator_output <- bind_rows(mean_moderator_output, mod_mean_recid_acc_vio_yes_list)
########################################
# 6.31: Reported accepting violent offenders
#####################################
table(mean_recid_analysis$reported_accepting_violent, mean_recid_analysis$studyid)
# Note: this variable will not be included in the mean recidivism moderator/ subgroup analysis because it separates the outcomes into the same groups as the variable '6.30: Accepted violent offenders',
#       which it is conceptually similar to. 
########################################
# 6.32: Reported accepting indictable charges
#####################################
table(mean_recid_analysis$reported_accepting_indictable, mean_recid_analysis$studyid)
# Note: there are insufficient numbers across groups to compute a moderator/subgroup analysis
########################################
# 6.33: Reported expunging charges
#####################################
table(mean_recid_analysis$reported_charges_expunged, mean_recid_analysis$studyid)
# Note: there are insufficient numbers across groups to compute a moderator/subgroup analysis
########################################
# 6.34: Proportion of sample that were graduates - Treatment
#####################################
table(mean_recid_analysis$pt_._program_graduates, mean_recid_analysis$studyid)
# Insert the moderator into the model
mod_mean_recid_prog_grad_tr <- rma.glmm(measure="IRR", x1i=pt_incidents_treat, t1i=pt_freedays_treat, x2i=pt_incidents_cont, t2i=pt_freedays_cont, 
                                        mods = ~ pt_._program_graduates, data=mean_recid_analysis, model="CM.EL")
# Create an object for the desired information and create a 'moderator_output' data frame 
mod_mean_recid_prog_grad_tr_list<- c('Proportion that were graduates (Tr)', 'sample characteristics', 'Beta_weight', mod_mean_recid_prog_grad_tr$k, mod_mean_recid_prog_grad_tr$beta[2, 1], mod_mean_recid_prog_grad_tr$se[2], mod_mean_recid_prog_grad_tr$zval[2], mod_mean_recid_prog_grad_tr$pval[2], 
                                     mod_mean_recid_prog_grad_tr$ci.lb[2], mod_mean_recid_prog_grad_tr$ci.ub[2], mod_mean_recid_prog_grad_tr$k- length(mod_mean_recid_prog_grad_tr$b), NA, NA, mod_mean_recid_prog_grad_tr$I2)
# name the object's columns 
names(mod_mean_recid_prog_grad_tr_list) <- c("name", "mod_category", "level", "level_k", "est_LIRR", "est_SE","est_z","est_z_p","est_cl","est_cu", "res_DF", "Qres", "Qres_p", "I2")
mean_moderator_output <- bind_rows(mean_moderator_output, mod_mean_recid_prog_grad_tr_list)
########################################
# 6.35: Proportion of sample that were terminates - Treatment
#####################################
table(mean_recid_analysis$pt_._program_terminates, mean_recid_analysis$studyid)
# Insert the moderator into the model
mod_mean_recid_prog_term_tr <- rma.glmm(measure="IRR", x1i=pt_incidents_treat, t1i=pt_freedays_treat, x2i=pt_incidents_cont, t2i=pt_freedays_cont, 
                                        mods = ~ pt_._program_terminates, data=mean_recid_analysis, model="CM.EL")
# Create an object for the desired information and create a 'moderator_output' data frame 
mod_mean_recid_prog_term_tr_list<- c('Proportion that were terminates (Tr)', 'sample characteristics', 'Beta_weight', mod_mean_recid_prog_term_tr$k, mod_mean_recid_prog_term_tr$beta[2, 1], mod_mean_recid_prog_term_tr$se[2], mod_mean_recid_prog_term_tr$zval[2], mod_mean_recid_prog_term_tr$pval[2], 
                                     mod_mean_recid_prog_term_tr$ci.lb[2], mod_mean_recid_prog_term_tr$ci.ub[2], mod_mean_recid_prog_term_tr$k- length(mod_mean_recid_prog_term_tr$b), NA, NA, mod_mean_recid_prog_term_tr$I2)
# name the object's columns 
names(mod_mean_recid_prog_term_tr_list) <- c("name", "mod_category", "level", "level_k", "est_LIRR", "est_SE","est_z","est_z_p","est_cl","est_cu", "res_DF", "Qres", "Qres_p", "I2")
mean_moderator_output <- bind_rows(mean_moderator_output, mod_mean_recid_prog_term_tr_list)
########################################
# 6.36: Proportion of sample that completed order - Treatment
#####################################
table(mean_recid_analysis$pt_order_completion_treat_., mean_recid_analysis$studyid)
# Insert the moderator into the model
mod_mean_recid_ord_compl_tr <- rma.glmm(measure="IRR", x1i=pt_incidents_treat, t1i=pt_freedays_treat, x2i=pt_incidents_cont, t2i=pt_freedays_cont, 
                                        mods = ~ pt_order_completion_treat_., data=mean_recid_analysis, model="CM.EL")
# Create an object for the desired information and create a 'moderator_output' data frame 
mod_mean_recid_ord_compl_tr_list<- c('Proportion that completed order (Tr)', 'sample characteristics', 'Beta_weight', mod_mean_recid_ord_compl_tr$k, mod_mean_recid_ord_compl_tr$beta[2, 1], mod_mean_recid_ord_compl_tr$se[2], mod_mean_recid_ord_compl_tr$zval[2], mod_mean_recid_ord_compl_tr$pval[2], 
                                     mod_mean_recid_ord_compl_tr$ci.lb[2], mod_mean_recid_ord_compl_tr$ci.ub[2], mod_mean_recid_ord_compl_tr$k- length(mod_mean_recid_ord_compl_tr$b), NA, NA, mod_mean_recid_ord_compl_tr$I2)
# name the object's columns 
names(mod_mean_recid_ord_compl_tr_list) <- c("name", "mod_category", "level", "level_k", "est_LIRR", "est_SE","est_z","est_z_p","est_cl","est_cu", "res_DF", "Qres", "Qres_p", "I2")
mean_moderator_output <- bind_rows(mean_moderator_output, mod_mean_recid_ord_compl_tr_list)
########################################
# 6.37: Proportion of sample that completed order - Control
#####################################
table(mean_recid_analysis$pt_order_completion_cont_., mean_recid_analysis$studyid)
# Note: there are insufficient outcomes that reported on this variable - the number of parameters to be estimated is larger than the number of observations
########################################
# 6.38: Publication type
#####################################
table(mean_recid_analysis$source, mean_recid_analysis$studyid)
# Note: there are insufficient outcomes that reported on this variable - the number of parameters to be estimated is larger than the number of observations
########################################
# 6.39: Publication status
#####################################
table(mean_recid_analysis$source_published, mean_recid_analysis$studyid)
# Note: there are insufficient numbers across groups to compute a moderator/subgroup analysis
########################################
# 6.40: Research design
#####################################
table(mean_recid_analysis$research_design, mean_recid_analysis$studyid)
# Note: there are insufficient numbers across groups to compute a moderator/subgroup analysis
########################################
# 6.41: Stage of disposition
#####################################
table(mean_recid_analysis$stage_of_disposition, mean_recid_analysis$studyid)
# Note: there are insufficient numbers across groups to compute a moderator/subgroup analysis
########################################
# 6.42: Reported a single judicial officer presiding over hearings
#####################################
table(mean_recid_analysis$reported_single_JO, mean_recid_analysis$studyid)
########################################
# 6.42.1: Reported a single judicial officer presiding over hearings - No
#####################################
# Select the subgroup in the model 
mod_mean_recid_rep_sing_jo_no <- rma.glmm(measure="IRR", x1i=pt_incidents_treat, t1i=pt_freedays_treat, x2i=pt_incidents_cont, t2i=pt_freedays_cont, 
                                      subset=(reported_single_JO== "No"), data=mean_recid_analysis, model="CM.EL")
# Create an object for the desired information  
mod_mean_recid_rep_sing_jo_no_list <- c('Reported single judicial officer', 'programmatic', 'No (ref)', mod_mean_recid_rep_sing_jo_no$k, mod_mean_recid_rep_sing_jo_no$beta[1], mod_mean_recid_rep_sing_jo_no$se[1], mod_mean_recid_rep_sing_jo_no$zval[1], mod_mean_recid_rep_sing_jo_no$pval[1], 
                                    mod_mean_recid_rep_sing_jo_no$ci.lb[1], mod_mean_recid_rep_sing_jo_no$ci.ub[1], mod_mean_recid_rep_sing_jo_no$k- length(mod_mean_recid_rep_sing_jo_no$b), NA, NA, 
                                    mod_mean_recid_rep_sing_jo_no$I2)
# name the object's columns and bind to mean_moderator_output
names(mod_mean_recid_rep_sing_jo_no_list) <- c("name", "mod_category", "level", "level_k", "est_LIRR", "est_SE","est_z","est_z_p","est_cl","est_cu", "res_DF", "Qres", "Qres_p", "I2")
mean_moderator_output <- bind_rows(mean_moderator_output, mod_mean_recid_rep_sing_jo_no_list)
########################################
# 6.42.2: Reported a single judicial officer presiding over hearings - Yes
#####################################
# Select the subgroup in the model 
mod_mean_recid_rep_sing_jo_yes <- rma.glmm(measure="IRR", x1i=pt_incidents_treat, t1i=pt_freedays_treat, x2i=pt_incidents_cont, t2i=pt_freedays_cont, 
                                          subset=(reported_single_JO== "Yes"), data=mean_recid_analysis, model="CM.EL")
# Create an object for the desired information  
mod_mean_recid_rep_sing_jo_yes_list <- c('Reported single judicial officer', 'programmatic', 'Yes', mod_mean_recid_rep_sing_jo_yes$k, mod_mean_recid_rep_sing_jo_yes$beta[1], mod_mean_recid_rep_sing_jo_yes$se[1], mod_mean_recid_rep_sing_jo_yes$zval[1], mod_mean_recid_rep_sing_jo_yes$pval[1], 
                                        mod_mean_recid_rep_sing_jo_yes$ci.lb[1], mod_mean_recid_rep_sing_jo_yes$ci.ub[1], mod_mean_recid_rep_sing_jo_yes$k- length(mod_mean_recid_rep_sing_jo_yes$b), NA, NA, 
                                        mod_mean_recid_rep_sing_jo_yes$I2)
# name the object's columns and bind to mean_moderator_output
names(mod_mean_recid_rep_sing_jo_yes_list) <- c("name", "mod_category", "level", "level_k", "est_LIRR", "est_SE","est_z","est_z_p","est_cl","est_cu", "res_DF", "Qres", "Qres_p", "I2")
mean_moderator_output <- bind_rows(mean_moderator_output, mod_mean_recid_rep_sing_jo_yes_list)
########################################
# 6.43: Number of judicial officer presiding over hearings
#####################################
table(mean_recid_analysis$JOs_presiding_over_hearings_collapsed, mean_recid_analysis$studyid)
# Note: this variable will not be included in the mean recidivism moderator/subgroup analysis because it needs to be recoded, and the appropriate recode 
#       separates outcomes into the identical two groups as '6.43: Reported a single judicial officer presiding over hearings'.
########################################
# 6.44: Reported team training
#####################################
table(mean_recid_analysis$reported_team_training, mean_recid_analysis$studyid)
# Note: there are insufficient numbers across groups to compute a moderator/subgroup analysis
########################################
# 6.45: Reported judicial officer training
#####################################
table(mean_recid_analysis$reported_JO.training, mean_recid_analysis$studyid)
# Note: there are insufficient numbers across groups to compute a moderator/subgroup analysis
########################################
# 6.46: Reported judicial officer or team training
#####################################
table(mean_recid_analysis$reported_judge_or_team_training, mean_recid_analysis$studyid)
# Note: there are insufficient numbers across groups to compute a moderator/subgroup analysis
########################################
# 6.47: Minutes per review hearing (average)
#####################################
table(mean_recid_analysis$average_minutes_review_hearing, mean_recid_analysis$studyid)
# Note: there are insufficient numbers across groups to compute a moderator/subgroup analysis
########################################
# 6.48: Reported frequent team meetings
#####################################
table(mean_recid_analysis$reported_frequent_team_meetings, mean_recid_analysis$studyid)
########################################
# 6.48.1: Reported frequent team meetings - No
#####################################
# Select the subgroup in the model 
mod_mean_recid_rep_team_neet_no <- rma.glmm(measure="IRR", x1i=pt_incidents_treat, t1i=pt_freedays_treat, x2i=pt_incidents_cont, t2i=pt_freedays_cont, 
                                              subset=(reported_frequent_team_meetings== "No"), data=mean_recid_analysis, model="CM.EL")
# Create an object for the desired information  
mod_mean_recid_rep_team_neet_no_list <- c('Reported frequent team meetings', 'programmatic', 'No (ref)', mod_mean_recid_rep_team_neet_no$k, mod_mean_recid_rep_team_neet_no$beta[1], mod_mean_recid_rep_team_neet_no$se[1], mod_mean_recid_rep_team_neet_no$zval[1], mod_mean_recid_rep_team_neet_no$pval[1], 
                                            mod_mean_recid_rep_team_neet_no$ci.lb[1], mod_mean_recid_rep_team_neet_no$ci.ub[1], mod_mean_recid_rep_team_neet_no$k- length(mod_mean_recid_rep_team_neet_no$b), NA, NA, 
                                            mod_mean_recid_rep_team_neet_no$I2)
# name the object's columns and bind to mean_moderator_output
names(mod_mean_recid_rep_team_neet_no_list) <- c("name", "mod_category", "level", "level_k", "est_LIRR", "est_SE","est_z","est_z_p","est_cl","est_cu", "res_DF", "Qres", "Qres_p", "I2")
mean_moderator_output <- bind_rows(mean_moderator_output, mod_mean_recid_rep_team_neet_no_list)
########################################
# 6.48.2: Reported frequent team meetings - Yes
#####################################
# Select the subgroup in the model 
mod_mean_recid_rep_team_neet_yes <- rma.glmm(measure="IRR", x1i=pt_incidents_treat, t1i=pt_freedays_treat, x2i=pt_incidents_cont, t2i=pt_freedays_cont, 
                                            subset=(reported_frequent_team_meetings== "Yes"), data=mean_recid_analysis, model="CM.EL")
# Create an object for the desired information  
mod_mean_recid_rep_team_neet_yes_list <- c('Reported frequent team meetings', 'programmatic', 'Yes', mod_mean_recid_rep_team_neet_yes$k, mod_mean_recid_rep_team_neet_yes$beta[1], mod_mean_recid_rep_team_neet_yes$se[1], mod_mean_recid_rep_team_neet_yes$zval[1], mod_mean_recid_rep_team_neet_yes$pval[1], 
                                          mod_mean_recid_rep_team_neet_yes$ci.lb[1], mod_mean_recid_rep_team_neet_yes$ci.ub[1], mod_mean_recid_rep_team_neet_yes$k- length(mod_mean_recid_rep_team_neet_yes$b), NA, NA, 
                                          mod_mean_recid_rep_team_neet_yes$I2)
# name the object's columns and bind to mean_moderator_output
names(mod_mean_recid_rep_team_neet_yes_list) <- c("name", "mod_category", "level", "level_k", "est_LIRR", "est_SE","est_z","est_z_p","est_cl","est_cu", "res_DF", "Qres", "Qres_p", "I2")
mean_moderator_output <- bind_rows(mean_moderator_output, mod_mean_recid_rep_team_neet_yes_list)
########################################
# 6.49: Reported using sanctions
#####################################
table(mean_recid_analysis$reported_sanctions, mean_recid_analysis$studyid)
# Note: there are insufficient numbers across groups to compute a moderator/subgroup analysis
########################################
# 6.50: Reported using graduated sanctions
#####################################
table(mean_recid_analysis$reported_using_graduated_sanctions, mean_recid_analysis$studyid)
########################################
# 6.50.1: Reported using graduated sanctions - No
#####################################
# Select the subgroup in the model 
mod_mean_recid_rep_grad_sancts_no <- rma.glmm(measure="IRR", x1i=pt_incidents_treat, t1i=pt_freedays_treat, x2i=pt_incidents_cont, t2i=pt_freedays_cont, 
                                      subset=(reported_using_graduated_sanctions== "No"), data=mean_recid_analysis, model="CM.EL")
# Create an object for the desired information  
mod_mean_recid_rep_grad_sancts_no_list <- c('Reported using graduated sanctions', 'programmatic', 'No (ref)', mod_mean_recid_rep_grad_sancts_no$k, mod_mean_recid_rep_grad_sancts_no$beta[1], mod_mean_recid_rep_grad_sancts_no$se[1], mod_mean_recid_rep_grad_sancts_no$zval[1], mod_mean_recid_rep_grad_sancts_no$pval[1], 
                                    mod_mean_recid_rep_grad_sancts_no$ci.lb[1], mod_mean_recid_rep_grad_sancts_no$ci.ub[1], mod_mean_recid_rep_grad_sancts_no$k- length(mod_mean_recid_rep_grad_sancts_no$b), NA, NA, 
                                    mod_mean_recid_rep_grad_sancts_no$I2)
# name the object's columns and bind to mean_moderator_output
names(mod_mean_recid_rep_grad_sancts_no_list) <- c("name", "mod_category", "level", "level_k", "est_LIRR", "est_SE","est_z","est_z_p","est_cl","est_cu", "res_DF", "Qres", "Qres_p", "I2")
mean_moderator_output <- bind_rows(mean_moderator_output, mod_mean_recid_rep_grad_sancts_no_list)
########################################
# 6.50.2: Reported using graduated sanctions - Yes
#####################################
# Select the subgroup in the model 
mod_mean_recid_rep_grad_sancts_yes <- rma.glmm(measure="IRR", x1i=pt_incidents_treat, t1i=pt_freedays_treat, x2i=pt_incidents_cont, t2i=pt_freedays_cont, 
                                              subset=(reported_using_graduated_sanctions== "Yes"), data=mean_recid_analysis, model="CM.EL")
# Create an object for the desired information  
mod_mean_recid_rep_grad_sancts_yes_list <- c('Reported using graduated sanctions', 'programmatic', 'Yes', mod_mean_recid_rep_grad_sancts_yes$k, mod_mean_recid_rep_grad_sancts_yes$beta[1], mod_mean_recid_rep_grad_sancts_yes$se[1], mod_mean_recid_rep_grad_sancts_yes$zval[1], mod_mean_recid_rep_grad_sancts_yes$pval[1], 
                                            mod_mean_recid_rep_grad_sancts_yes$ci.lb[1], mod_mean_recid_rep_grad_sancts_yes$ci.ub[1], mod_mean_recid_rep_grad_sancts_yes$k- length(mod_mean_recid_rep_grad_sancts_yes$b), NA, NA, 
                                            mod_mean_recid_rep_grad_sancts_yes$I2)
# name the object's columns and bind to mean_moderator_output
names(mod_mean_recid_rep_grad_sancts_yes_list) <- c("name", "mod_category", "level", "level_k", "est_LIRR", "est_SE","est_z","est_z_p","est_cl","est_cu", "res_DF", "Qres", "Qres_p", "I2")
mean_moderator_output <- bind_rows(mean_moderator_output, mod_mean_recid_rep_grad_sancts_yes_list)
########################################
# 6.51: Reported using sanctions or graduated sanctions
#####################################
table(mean_recid_analysis$reported_sanctions_or_graduated_sanctions, mean_recid_analysis$studyid)
# Note: this variable will not be included in the mean recidivism moderator/ subgroup analysis because it separates the outcomes into the same groups as the variable '6.51: Reported using graduated sanctions',
#       which it is conceptually similar to. 
########################################
# 6.52: Reported using rewards
#####################################
table(mean_recid_analysis$reported_using_rewards, mean_recid_analysis$studyid)
########################################
# 6.52.1: Reported using rewards - No
#####################################
# Select the subgroup in the model 
mod_mean_recid_rep_rewards_no <- rma.glmm(measure="IRR", x1i=pt_incidents_treat, t1i=pt_freedays_treat, x2i=pt_incidents_cont, t2i=pt_freedays_cont, 
                                          subset=(reported_using_rewards== "No"), data=mean_recid_analysis, model="CM.EL")
# Create an object for the desired information  
mod_mean_recid_rep_rewards_no_list <- c('Reported using rewards', 'programmatic', 'No (ref)', mod_mean_recid_rep_rewards_no$k, mod_mean_recid_rep_rewards_no$beta[1], mod_mean_recid_rep_rewards_no$se[1], mod_mean_recid_rep_rewards_no$zval[1], mod_mean_recid_rep_rewards_no$pval[1], 
                                        mod_mean_recid_rep_rewards_no$ci.lb[1], mod_mean_recid_rep_rewards_no$ci.ub[1], mod_mean_recid_rep_rewards_no$k- length(mod_mean_recid_rep_rewards_no$b), NA, NA, 
                                        mod_mean_recid_rep_rewards_no$I2)
# name the object's columns and bind to mean_moderator_output
names(mod_mean_recid_rep_rewards_no_list) <- c("name", "mod_category", "level", "level_k", "est_LIRR", "est_SE","est_z","est_z_p","est_cl","est_cu", "res_DF", "Qres", "Qres_p", "I2")
mean_moderator_output <- bind_rows(mean_moderator_output, mod_mean_recid_rep_rewards_no_list)
########################################
# 6.52.2: Reported using rewards - Yes
#####################################
# Select the subgroup in the model 
mod_mean_recid_rep_rewards_yes <- rma.glmm(measure="IRR", x1i=pt_incidents_treat, t1i=pt_freedays_treat, x2i=pt_incidents_cont, t2i=pt_freedays_cont, 
                                          subset=(reported_using_rewards== "Yes"), data=mean_recid_analysis, model="CM.EL")
# Create an object for the desired information  
mod_mean_recid_rep_rewards_yes_list <- c('Reported using rewards', 'programmatic', 'Yes', mod_mean_recid_rep_rewards_yes$k, mod_mean_recid_rep_rewards_yes$beta[1], mod_mean_recid_rep_rewards_yes$se[1], mod_mean_recid_rep_rewards_yes$zval[1], mod_mean_recid_rep_rewards_yes$pval[1], 
                                        mod_mean_recid_rep_rewards_yes$ci.lb[1], mod_mean_recid_rep_rewards_yes$ci.ub[1], mod_mean_recid_rep_rewards_yes$k- length(mod_mean_recid_rep_rewards_yes$b), NA, NA, 
                                        mod_mean_recid_rep_rewards_yes$I2)
# name the object's columns and bind to mean_moderator_output
names(mod_mean_recid_rep_rewards_yes_list) <- c("name", "mod_category", "level", "level_k", "est_LIRR", "est_SE","est_z","est_z_p","est_cl","est_cu", "res_DF", "Qres", "Qres_p", "I2")
mean_moderator_output <- bind_rows(mean_moderator_output, mod_mean_recid_rep_rewards_yes_list)
########################################
# 6.53: Reported using sanctions and rewards
#####################################
table(mean_recid_analysis$reported_sanctions_and_rewards, mean_recid_analysis$studyid)
# Note: this variable will not be included in the moderator/subgroup analysis because it separates outcomes into the identical two groups as '6.53: Reported using rewards'.
########################################
# 6.54: Frequency of review hearings
#####################################
table(mean_recid_analysis$frequency_review_hearing, mean_recid_analysis$studyid)
# recode 'NAs' they can ignored in the analysis 
mean_recid_analysis$frequency_review_hearing_recode <- replace_na(mean_recid_analysis$frequency_review_hearing, "NA")
########################################
# 6.54.1: Frequency of review hearings - Fortnightly
#####################################
# Select the subgroup in the model 
mod_mean_recid_freq_rh_fort <- rma.glmm(measure="IRR", x1i=pt_incidents_treat, t1i=pt_freedays_treat, x2i=pt_incidents_cont, t2i=pt_freedays_cont, 
                                          subset=(frequency_review_hearing_recode== "fortnightly"), data=mean_recid_analysis, model="CM.EL")
# Create an object for the desired information  
mod_mean_recid_freq_rh_fort_list <- c('Frequency of review hearings', 'programmatic', 'Fortnightly (ref)', mod_mean_recid_freq_rh_fort$k, mod_mean_recid_freq_rh_fort$beta[1], mod_mean_recid_freq_rh_fort$se[1], mod_mean_recid_freq_rh_fort$zval[1], mod_mean_recid_freq_rh_fort$pval[1], 
                                        mod_mean_recid_freq_rh_fort$ci.lb[1], mod_mean_recid_freq_rh_fort$ci.ub[1], mod_mean_recid_freq_rh_fort$k- length(mod_mean_recid_freq_rh_fort$b), NA, NA, 
                                        mod_mean_recid_freq_rh_fort$I2)
# name the object's columns and bind to mean_moderator_output
names(mod_mean_recid_freq_rh_fort_list) <- c("name", "mod_category", "level", "level_k", "est_LIRR", "est_SE","est_z","est_z_p","est_cl","est_cu", "res_DF", "Qres", "Qres_p", "I2")
mean_moderator_output <- bind_rows(mean_moderator_output, mod_mean_recid_freq_rh_fort_list)
########################################
# 6.54.1: Frequency of review hearings - Weekly
#####################################
# Select the subgroup in the model 
mod_mean_recid_freq_rh_week <- rma.glmm(measure="IRR", x1i=pt_incidents_treat, t1i=pt_freedays_treat, x2i=pt_incidents_cont, t2i=pt_freedays_cont, 
                                        subset=(frequency_review_hearing_recode== "weekly"), data=mean_recid_analysis, model="CM.EL")
# Create an object for the desired information  
mod_mean_recid_freq_rh_week_list <- c('Frequency of review hearings', 'programmatic', 'Weekly', mod_mean_recid_freq_rh_week$k, mod_mean_recid_freq_rh_week$beta[1], mod_mean_recid_freq_rh_week$se[1], mod_mean_recid_freq_rh_week$zval[1], mod_mean_recid_freq_rh_week$pval[1], 
                                      mod_mean_recid_freq_rh_week$ci.lb[1], mod_mean_recid_freq_rh_week$ci.ub[1], mod_mean_recid_freq_rh_week$k- length(mod_mean_recid_freq_rh_week$b), NA, NA, 
                                      mod_mean_recid_freq_rh_week$I2)
# name the object's columns and bind to mean_moderator_output
names(mod_mean_recid_freq_rh_week_list) <- c("name", "mod_category", "level", "level_k", "est_LIRR", "est_SE","est_z","est_z_p","est_cl","est_cu", "res_DF", "Qres", "Qres_p", "I2")
mean_moderator_output <- bind_rows(mean_moderator_output, mod_mean_recid_freq_rh_week_list)
########################################
# 6.55: Frequency duration
#####################################
table(mean_recid_analysis$frequency_duration, mean_recid_analysis$studyid)
# Note: there are insufficient numbers across groups to compute a moderator/subgroup analysis
########################################
# 6.56: Frequent vs monthly review hearings
#####################################
table(mean_recid_analysis$frequent_vs_monthly_rh, mean_recid_analysis$studyid)
# Note: there are insufficient numbers across groups to compute a moderator/subgroup analysis
########################################
# 6.57: Reported frequent review hearings
#####################################
table(mean_recid_analysis$reported_frequent_rh, mean_recid_analysis$studyid)
########################################
# 6.57.1: Reported frequent review hearings - No
#####################################
# Select the subgroup in the model 
mod_mean_recid_rep_freq_rh_no <- rma.glmm(measure="IRR", x1i=pt_incidents_treat, t1i=pt_freedays_treat, x2i=pt_incidents_cont, t2i=pt_freedays_cont, 
                                              subset=(reported_frequent_rh== "No"), data=mean_recid_analysis, model="CM.EL")
# Create an object for the desired information  
mod_mean_recid_rep_freq_rh_no_list <- c('Reported frequent review hearings', 'programmatic', 'No (ref)', mod_mean_recid_rep_freq_rh_no$k, mod_mean_recid_rep_freq_rh_no$beta[1], mod_mean_recid_rep_freq_rh_no$se[1], mod_mean_recid_rep_freq_rh_no$zval[1], mod_mean_recid_rep_freq_rh_no$pval[1], 
                                            mod_mean_recid_rep_freq_rh_no$ci.lb[1], mod_mean_recid_rep_freq_rh_no$ci.ub[1], mod_mean_recid_rep_freq_rh_no$k- length(mod_mean_recid_rep_freq_rh_no$b), NA, NA, 
                                            mod_mean_recid_rep_freq_rh_no$I2)
# name the object's columns and bind to mean_moderator_output
names(mod_mean_recid_rep_freq_rh_no_list) <- c("name", "mod_category", "level", "level_k", "est_LIRR", "est_SE","est_z","est_z_p","est_cl","est_cu", "res_DF", "Qres", "Qres_p", "I2")
mean_moderator_output <- bind_rows(mean_moderator_output, mod_mean_recid_rep_freq_rh_no_list)
########################################
# 6.57.2: Reported frequent review hearings - Yes
#####################################
# Select the subgroup in the model 
mod_mean_recid_rep_freq_rh_yes <- rma.glmm(measure="IRR", x1i=pt_incidents_treat, t1i=pt_freedays_treat, x2i=pt_incidents_cont, t2i=pt_freedays_cont, 
                                          subset=(reported_frequent_rh== "Yes"), data=mean_recid_analysis, model="CM.EL")
# Create an object for the desired information  
mod_mean_recid_rep_freq_rh_yes_list <- c('Reported frequent review hearings', 'programmatic', 'Yes', mod_mean_recid_rep_freq_rh_yes$k, mod_mean_recid_rep_freq_rh_yes$beta[1], mod_mean_recid_rep_freq_rh_yes$se[1], mod_mean_recid_rep_freq_rh_yes$zval[1], mod_mean_recid_rep_freq_rh_yes$pval[1], 
                                        mod_mean_recid_rep_freq_rh_yes$ci.lb[1], mod_mean_recid_rep_freq_rh_yes$ci.ub[1], mod_mean_recid_rep_freq_rh_yes$k- length(mod_mean_recid_rep_freq_rh_yes$b), NA, NA, 
                                        mod_mean_recid_rep_freq_rh_yes$I2)
# name the object's columns and bind to mean_moderator_output
names(mod_mean_recid_rep_freq_rh_yes_list) <- c("name", "mod_category", "level", "level_k", "est_LIRR", "est_SE","est_z","est_z_p","est_cl","est_cu", "res_DF", "Qres", "Qres_p", "I2")
mean_moderator_output <- bind_rows(mean_moderator_output, mod_mean_recid_rep_freq_rh_yes_list)
########################################
# 6.58: Reported measuring risk - Treatment
#####################################
table(mean_recid_analysis$reported_measuring_risk_treat, mean_recid_analysis$studyid)
########################################
# 6.58.1: Reported measuring risk - Treatment - No
#####################################
# Select the subgroup in the model 
mod_mean_recid_rep_meas_risk_tr_no <- rma.glmm(measure="IRR", x1i=pt_incidents_treat, t1i=pt_freedays_treat, x2i=pt_incidents_cont, t2i=pt_freedays_cont, 
                                          subset=(reported_measuring_risk_treat== "No"), data=mean_recid_analysis, model="CM.EL")
# Create an object for the desired information  
mod_mean_recid_rep_meas_risk_tr_no_list <- c('Reported measuring risk (Tr)', 'treatment', 'No (ref)', mod_mean_recid_rep_meas_risk_tr_no$k, mod_mean_recid_rep_meas_risk_tr_no$beta[1], mod_mean_recid_rep_meas_risk_tr_no$se[1], mod_mean_recid_rep_meas_risk_tr_no$zval[1], mod_mean_recid_rep_meas_risk_tr_no$pval[1], 
                                        mod_mean_recid_rep_meas_risk_tr_no$ci.lb[1], mod_mean_recid_rep_meas_risk_tr_no$ci.ub[1], mod_mean_recid_rep_meas_risk_tr_no$k- length(mod_mean_recid_rep_meas_risk_tr_no$b), NA, NA, 
                                        mod_mean_recid_rep_meas_risk_tr_no$I2)
# name the object's columns and bind to mean_moderator_output
names(mod_mean_recid_rep_meas_risk_tr_no_list) <- c("name", "mod_category", "level", "level_k", "est_LIRR", "est_SE","est_z","est_z_p","est_cl","est_cu", "res_DF", "Qres", "Qres_p", "I2")
mean_moderator_output <- bind_rows(mean_moderator_output, mod_mean_recid_rep_meas_risk_tr_no_list)
########################################
# 6.58.2: Reported measuring risk - Treatment - Yes
#####################################
# Select the subgroup in the model 
mod_mean_recid_rep_meas_risk_tr_yes <- rma.glmm(measure="IRR", x1i=pt_incidents_treat, t1i=pt_freedays_treat, x2i=pt_incidents_cont, t2i=pt_freedays_cont, 
                                               subset=(reported_measuring_risk_treat== "Yes"), data=mean_recid_analysis, model="CM.EL")
# Create an object for the desired information  
mod_mean_recid_rep_meas_risk_tr_yes_list <- c('Reported measuring risk (Tr)', 'treatment', 'Yes', mod_mean_recid_rep_meas_risk_tr_yes$k, mod_mean_recid_rep_meas_risk_tr_yes$beta[1], mod_mean_recid_rep_meas_risk_tr_yes$se[1], mod_mean_recid_rep_meas_risk_tr_yes$zval[1], mod_mean_recid_rep_meas_risk_tr_yes$pval[1], 
                                             mod_mean_recid_rep_meas_risk_tr_yes$ci.lb[1], mod_mean_recid_rep_meas_risk_tr_yes$ci.ub[1], mod_mean_recid_rep_meas_risk_tr_yes$k- length(mod_mean_recid_rep_meas_risk_tr_yes$b), NA, NA, 
                                             mod_mean_recid_rep_meas_risk_tr_yes$I2)
# name the object's columns and bind to mean_moderator_output
names(mod_mean_recid_rep_meas_risk_tr_yes_list) <- c("name", "mod_category", "level", "level_k", "est_LIRR", "est_SE","est_z","est_z_p","est_cl","est_cu", "res_DF", "Qres", "Qres_p", "I2")
mean_moderator_output <- bind_rows(mean_moderator_output, mod_mean_recid_rep_meas_risk_tr_yes_list)
########################################
# 6.59: Reported measuring risk - Control
#####################################
table(mean_recid_analysis$reported_measuring_risk_cont, mean_recid_analysis$studyid)
# Note: there are insufficient numbers across groups to compute a moderator/subgroup analysis
########################################
# 6.60: Reported individualising treatment - Treatment
#####################################
table(mean_recid_analysis$reported_individualized_treat_treat, mean_recid_analysis$studyid)
# Note: there are insufficient numbers across groups to compute a moderator/subgroup analysis
########################################
# 6.61: Reported individualising treatment - Control
#####################################
table(mean_recid_analysis$reported_individualized_treat_cont, mean_recid_analysis$studyid)
# Note: there are insufficient numbers across groups to compute a moderator/subgroup analysis
########################################
# 6.62: Reported drug or alcohol treatment - Treatment 
#####################################
table(mean_recid_analysis$reported_drug_or_alcohol_treatment_treat, mean_recid_analysis$studyid)
# Note: there are insufficient numbers across groups to compute a moderator/subgroup analysis
########################################
# 6.63: Type of drug or alcohol treatment - Treatment
#####################################
table(mean_recid_analysis$type_of_drug_or_alch_treat_treat, mean_recid_analysis$studyid)
# Note: there are insufficient numbers across groups to compute a moderator/subgroup analysis
########################################
# 6.64: Reported drug or alcohol treatment - Control
#####################################
table(mean_recid_analysis$reported_drug_or_alcohol_treatment_cont, mean_recid_analysis$studyid)
# Note: there are insufficient numbers across groups to compute a moderator/subgroup analysis
########################################
# 6.65: Type of drug or alcohol treatment - Control
#####################################
table(mean_recid_analysis$type_of_drug_or_alch_treat_cont, mean_recid_analysis$studyid)
# Note: there are insufficient numbers across groups to compute a moderator/subgroup analysis
########################################
# 6.66: Reported drug or alcohol testing - Treatment 
#####################################
table(mean_recid_analysis$reported_drug_or_alcohol_testing_treat, mean_recid_analysis$studyid)
# Note: there are insufficient numbers across groups to compute a moderator/subgroup analysis
########################################
# 6.67: Reported drug or alcohol testing - Control 
#####################################
table(mean_recid_analysis$reported_drug_or_alcohol_testing_cont, mean_recid_analysis$studyid)
# Note: there are insufficient numbers across groups to compute a moderator/subgroup analysis
########################################
# 6.68: Reported detox treatment - Treatment 
#####################################
table(mean_recid_analysis$reported_detox_treat, mean_recid_analysis$studyid)
# Recode 'offered' and 'required' to 'yes' 
mean_recid_analysis$reported_detox_treat_recode <- recode(mean_recid_analysis$reported_detox_treat, 'offered'='Yes', 'required' ='Yes')  
########################################
# 6.68.1: Reported detox treatment - Treatment - No
#####################################
# Select the subgroup in the model 
mod_mean_recid_rep_detox_tr_no <- rma.glmm(measure="IRR", x1i=pt_incidents_treat, t1i=pt_freedays_treat, x2i=pt_incidents_cont, t2i=pt_freedays_cont, 
                                               subset=(reported_detox_treat_recode== "No"), data=mean_recid_analysis, model="CM.EL")
# Create an object for the desired information  
mod_mean_recid_rep_detox_tr_no_list <- c('Reported detox treatment (Tr)', 'treatment', 'No (ref)', mod_mean_recid_rep_detox_tr_no$k, mod_mean_recid_rep_detox_tr_no$beta[1], mod_mean_recid_rep_detox_tr_no$se[1], mod_mean_recid_rep_detox_tr_no$zval[1], mod_mean_recid_rep_detox_tr_no$pval[1], 
                                             mod_mean_recid_rep_detox_tr_no$ci.lb[1], mod_mean_recid_rep_detox_tr_no$ci.ub[1], mod_mean_recid_rep_detox_tr_no$k- length(mod_mean_recid_rep_detox_tr_no$b), NA, NA, 
                                             mod_mean_recid_rep_detox_tr_no$I2)
# name the object's columns and bind to mean_moderator_output
names(mod_mean_recid_rep_detox_tr_no_list) <- c("name", "mod_category", "level", "level_k", "est_LIRR", "est_SE","est_z","est_z_p","est_cl","est_cu", "res_DF", "Qres", "Qres_p", "I2")
mean_moderator_output <- bind_rows(mean_moderator_output, mod_mean_recid_rep_detox_tr_no_list)
########################################
# 6.68.2: Reported detox treatment- Treatment - Offered or required
#####################################
# Select the subgroup in the model 
mod_mean_recid_rep_detox_tr_yes <- rma.glmm(measure="IRR", x1i=pt_incidents_treat, t1i=pt_freedays_treat, x2i=pt_incidents_cont, t2i=pt_freedays_cont, 
                                           subset=(reported_detox_treat_recode== "Yes"), data=mean_recid_analysis, model="CM.EL")
# Create an object for the desired information  
mod_mean_recid_rep_detox_tr_yes_list <- c('Reported detox treatment (Tr)', 'treatment', 'Offered or required', mod_mean_recid_rep_detox_tr_yes$k, mod_mean_recid_rep_detox_tr_yes$beta[1], mod_mean_recid_rep_detox_tr_yes$se[1], mod_mean_recid_rep_detox_tr_yes$zval[1], mod_mean_recid_rep_detox_tr_yes$pval[1], 
                                         mod_mean_recid_rep_detox_tr_yes$ci.lb[1], mod_mean_recid_rep_detox_tr_yes$ci.ub[1], mod_mean_recid_rep_detox_tr_yes$k- length(mod_mean_recid_rep_detox_tr_yes$b), NA, NA, 
                                         mod_mean_recid_rep_detox_tr_yes$I2)
# name the object's columns and bind to mean_moderator_output
names(mod_mean_recid_rep_detox_tr_yes_list) <- c("name", "mod_category", "level", "level_k", "est_LIRR", "est_SE","est_z","est_z_p","est_cl","est_cu", "res_DF", "Qres", "Qres_p", "I2")
mean_moderator_output <- bind_rows(mean_moderator_output, mod_mean_recid_rep_detox_tr_yes_list)
########################################
# 6.69: Reported detox treatment - Control 
#####################################
table(mean_recid_analysis$reported_detox_cont, mean_recid_analysis$studyid)
# Note: there are insufficient numbers across groups to compute a moderator/subgroup analysis
########################################
# 6.70: Reported pharmacotherapy treatment - Treatment 
#####################################
table(mean_recid_analysis$reported_pharmacotherapy_treat, mean_recid_analysis$studyid)
########################################
# 6.70.1: Reported pharmacotherapy - Treatment - No
#####################################
# Select the subgroup in the model 
mod_mean_recid_rep_pharm_tr_no <- rma.glmm(measure="IRR", x1i=pt_incidents_treat, t1i=pt_freedays_treat, x2i=pt_incidents_cont, t2i=pt_freedays_cont, 
                                           subset=(reported_pharmacotherapy_treat== "No"), data=mean_recid_analysis, model="CM.EL")
# Create an object for the desired information  
mod_mean_recid_rep_pharm_tr_no_list <- c('Reported pharmacotherapy (Tr)', 'treatment', 'No (ref)', mod_mean_recid_rep_pharm_tr_no$k, mod_mean_recid_rep_pharm_tr_no$beta[1], mod_mean_recid_rep_pharm_tr_no$se[1], mod_mean_recid_rep_pharm_tr_no$zval[1], mod_mean_recid_rep_pharm_tr_no$pval[1], 
                                         mod_mean_recid_rep_pharm_tr_no$ci.lb[1], mod_mean_recid_rep_pharm_tr_no$ci.ub[1], mod_mean_recid_rep_pharm_tr_no$k- length(mod_mean_recid_rep_pharm_tr_no$b), NA, NA, 
                                         mod_mean_recid_rep_pharm_tr_no$I2)
# name the object's columns and bind to mean_moderator_output
names(mod_mean_recid_rep_pharm_tr_no_list) <- c("name", "mod_category", "level", "level_k", "est_LIRR", "est_SE","est_z","est_z_p","est_cl","est_cu", "res_DF", "Qres", "Qres_p", "I2")
mean_moderator_output <- bind_rows(mean_moderator_output, mod_mean_recid_rep_pharm_tr_no_list)

########################################
# 6.70.2: Reported pharmacotherapy - Treatment - Offered
#####################################
# Select the subgroup in the model 
mod_mean_recid_rep_pharm_tr_off <- rma.glmm(measure="IRR", x1i=pt_incidents_treat, t1i=pt_freedays_treat, x2i=pt_incidents_cont, t2i=pt_freedays_cont, 
                                           subset=(reported_pharmacotherapy_treat== "offered"), data=mean_recid_analysis, model="CM.EL")
# Create an object for the desired information  
mod_mean_recid_rep_pharm_tr_off_list <- c('Reported pharmacotherapy (Tr)', 'treatment', 'Offered', mod_mean_recid_rep_pharm_tr_off$k, mod_mean_recid_rep_pharm_tr_off$beta[1], mod_mean_recid_rep_pharm_tr_off$se[1], mod_mean_recid_rep_pharm_tr_off$zval[1], mod_mean_recid_rep_pharm_tr_off$pval[1], 
                                         mod_mean_recid_rep_pharm_tr_off$ci.lb[1], mod_mean_recid_rep_pharm_tr_off$ci.ub[1], mod_mean_recid_rep_pharm_tr_off$k- length(mod_mean_recid_rep_pharm_tr_off$b), NA, NA, 
                                         mod_mean_recid_rep_pharm_tr_off$I2)
# name the object's columns and bind to mean_moderator_output
names(mod_mean_recid_rep_pharm_tr_off_list) <- c("name", "mod_category", "level", "level_k", "est_LIRR", "est_SE","est_z","est_z_p","est_cl","est_cu", "res_DF", "Qres", "Qres_p", "I2")
mean_moderator_output <- bind_rows(mean_moderator_output, mod_mean_recid_rep_pharm_tr_off_list)
########################################
# 6.71: Reported pharmacotherapy treatment - Control 
#####################################
table(mean_recid_analysis$reported_pharmacotherapy_cont, mean_recid_analysis$studyid)
# Note: there are insufficient numbers across groups to compute a moderator/subgroup analysis
########################################
# 6.72: Reported relapse prevention - Treatment 
#####################################
table(mean_recid_analysis$reported_relapse_prevention_treat, mean_recid_analysis$studyid)
########################################
# 6.72.1: Reported relapse prevention - Treatment - No
#####################################
mod_mean_recid_rep_relapse_prev_tr_no <- rma.glmm(measure="IRR", x1i=pt_incidents_treat, t1i=pt_freedays_treat, x2i=pt_incidents_cont, t2i=pt_freedays_cont, 
                                                   subset=(reported_relapse_prevention_treat== "No"), data=mean_recid_analysis, model="CM.EL")
# Create an object for the desired information  
mod_mean_recid_rep_relapse_prev_tr_no_list <- c('Reported relapse prevention (Tr)', 'treatment', 'No (ref)', mod_mean_recid_rep_relapse_prev_tr_no$k, mod_mean_recid_rep_relapse_prev_tr_no$beta[1], mod_mean_recid_rep_relapse_prev_tr_no$se[1], mod_mean_recid_rep_relapse_prev_tr_no$zval[1], mod_mean_recid_rep_relapse_prev_tr_no$pval[1], 
                                                 mod_mean_recid_rep_relapse_prev_tr_no$ci.lb[1], mod_mean_recid_rep_relapse_prev_tr_no$ci.ub[1], mod_mean_recid_rep_relapse_prev_tr_no$k- length(mod_mean_recid_rep_relapse_prev_tr_no$b), NA, NA, 
                                                 mod_mean_recid_rep_relapse_prev_tr_no$I2)
# name the object's columns and bind to mean_moderator_output
names(mod_mean_recid_rep_relapse_prev_tr_no_list) <- c("name", "mod_category", "level", "level_k", "est_LIRR", "est_SE","est_z","est_z_p","est_cl","est_cu", "res_DF", "Qres", "Qres_p", "I2")
mean_moderator_output <- bind_rows(mean_moderator_output, mod_mean_recid_rep_relapse_prev_tr_no_list)
########################################
# 6.72.2: Reported relapse prevention - Treatment  - Offered
#####################################
# Select the subgroup in the model 
mod_mean_recid_rep_relapse_prev_tr_off <- rma.glmm(measure="IRR", x1i=pt_incidents_treat, t1i=pt_freedays_treat, x2i=pt_incidents_cont, t2i=pt_freedays_cont, 
                                            subset=(reported_relapse_prevention_treat== "offered"), data=mean_recid_analysis, model="CM.EL")
# Create an object for the desired information  
mod_mean_recid_rep_relapse_prev_tr_off_list <- c('Reported relapse prevention (Tr)', 'treatment', 'Offered', mod_mean_recid_rep_relapse_prev_tr_off$k, mod_mean_recid_rep_relapse_prev_tr_off$beta[1], mod_mean_recid_rep_relapse_prev_tr_off$se[1], mod_mean_recid_rep_relapse_prev_tr_off$zval[1], mod_mean_recid_rep_relapse_prev_tr_off$pval[1], 
                                          mod_mean_recid_rep_relapse_prev_tr_off$ci.lb[1], mod_mean_recid_rep_relapse_prev_tr_off$ci.ub[1], mod_mean_recid_rep_relapse_prev_tr_off$k- length(mod_mean_recid_rep_relapse_prev_tr_off$b), NA, NA, 
                                          mod_mean_recid_rep_relapse_prev_tr_off$I2)
# name the object's columns and bind to mean_moderator_output
names(mod_mean_recid_rep_relapse_prev_tr_off_list) <- c("name", "mod_category", "level", "level_k", "est_LIRR", "est_SE","est_z","est_z_p","est_cl","est_cu", "res_DF", "Qres", "Qres_p", "I2")
mean_moderator_output <- bind_rows(mean_moderator_output, mod_mean_recid_rep_relapse_prev_tr_off_list)
########################################
# 6.72.3: Reported relapse prevention - Treatment  - Required
#####################################
# Select the subgroup in the model 
mod_mean_recid_rep_relapse_prev_tr_req <- rma.glmm(measure="IRR", x1i=pt_incidents_treat, t1i=pt_freedays_treat, x2i=pt_incidents_cont, t2i=pt_freedays_cont, 
                                                   subset=(reported_relapse_prevention_treat== "required"), data=mean_recid_analysis, model="CM.EL")
# Create an object for the desired information  
mod_mean_recid_rep_relapse_prev_tr_req_list <- c('Reported relapse prevention (Tr)', 'treatment', 'Required', mod_mean_recid_rep_relapse_prev_tr_req$k, mod_mean_recid_rep_relapse_prev_tr_req$beta[1], mod_mean_recid_rep_relapse_prev_tr_req$se[1], mod_mean_recid_rep_relapse_prev_tr_req$zval[1], mod_mean_recid_rep_relapse_prev_tr_req$pval[1], 
                                                 mod_mean_recid_rep_relapse_prev_tr_req$ci.lb[1], mod_mean_recid_rep_relapse_prev_tr_req$ci.ub[1], mod_mean_recid_rep_relapse_prev_tr_req$k- length(mod_mean_recid_rep_relapse_prev_tr_req$b), NA, NA, 
                                                 mod_mean_recid_rep_relapse_prev_tr_req$I2)
# name the object's columns and bind to mean_moderator_output
names(mod_mean_recid_rep_relapse_prev_tr_req_list) <- c("name", "mod_category", "level", "level_k", "est_LIRR", "est_SE","est_z","est_z_p","est_cl","est_cu", "res_DF", "Qres", "Qres_p", "I2")
mean_moderator_output <- bind_rows(mean_moderator_output, mod_mean_recid_rep_relapse_prev_tr_req_list)
########################################
# 6.73: Reported relapse prevention - Control 
#####################################
table(mean_recid_analysis$reported_relapse_prevention_cont, mean_recid_analysis$studyid)
# Note: there are insufficient numbers across groups to compute a moderator/subgroup analysis
########################################
# 6.74: Reported psychiatric treatment - Treatment 
#####################################
table(mean_recid_analysis$reported_psychiatric_treatment_treat, mean_recid_analysis$studyid)
# Note: there are insufficient numbers across groups to compute a moderator/subgroup analysis
########################################
# 6.75: Reported psychiatric treatment - Control 
#####################################
table(mean_recid_analysis$reported_psychiatric_treatment_cont, mean_recid_analysis$studyid)
# Note: there are insufficient numbers across groups to compute a moderator/subgroup analysis
########################################
# 6.76: Reported mental health treatment - Treatment 
#####################################
table(mean_recid_analysis$reported_mental_health_treatment_treat, mean_recid_analysis$studyid)
# Note: there are insufficient numbers across groups to compute a moderator/subgroup analysis
########################################
# 6.77: Reported mental health treatment - Control 
#####################################
table(mean_recid_analysis$reported_mental_health_treatment_cont, mean_recid_analysis$studyid)
# Note: there are insufficient numbers across groups to compute a moderator/subgroup analysis
########################################
# 6.78: Reported cognitive-behavioral therapy - Treatment 
#####################################
table(mean_recid_analysis$reported_CBT_based_treatment_treat, mean_recid_analysis$studyid)
# Recode 'offered' and 'required' to 'yes' 
mean_recid_analysis$reported_CBT_based_treatment_treat_recode <- recode(mean_recid_analysis$reported_CBT_based_treatment_treat, 'offered'='Yes', 'required' ='Yes')  
########################################
# 6.78.1: Reported cognitive-behavioral therapy - Treatment - No
#####################################
# Select the subgroup in the model 
mod_mean_recid_rep_cbt_treat_tr_no <- rma.glmm(measure="IRR", x1i=pt_incidents_treat, t1i=pt_freedays_treat, x2i=pt_incidents_cont, t2i=pt_freedays_cont, 
                                                   subset=(reported_CBT_based_treatment_treat_recode== "No"), data=mean_recid_analysis, model="CM.EL")
# Create an object for the desired information  
mod_mean_recid_rep_cbt_treat_tr_no_list <- c('Reported CBT treatment (Tr)', 'treatment', 'No (ref)', mod_mean_recid_rep_cbt_treat_tr_no$k, mod_mean_recid_rep_cbt_treat_tr_no$beta[1], mod_mean_recid_rep_cbt_treat_tr_no$se[1], mod_mean_recid_rep_cbt_treat_tr_no$zval[1], mod_mean_recid_rep_cbt_treat_tr_no$pval[1], 
                                                 mod_mean_recid_rep_cbt_treat_tr_no$ci.lb[1], mod_mean_recid_rep_cbt_treat_tr_no$ci.ub[1], mod_mean_recid_rep_cbt_treat_tr_no$k- length(mod_mean_recid_rep_cbt_treat_tr_no$b), NA, NA, 
                                                 mod_mean_recid_rep_cbt_treat_tr_no$I2)
# name the object's columns and bind to mean_moderator_output
names(mod_mean_recid_rep_cbt_treat_tr_no_list) <- c("name", "mod_category", "level", "level_k", "est_LIRR", "est_SE","est_z","est_z_p","est_cl","est_cu", "res_DF", "Qres", "Qres_p", "I2")
mean_moderator_output <- bind_rows(mean_moderator_output, mod_mean_recid_rep_cbt_treat_tr_no_list)
########################################
# 6.78.2: Reported cognitive-behavioral therapy - Treatment - Offered or required
#####################################
# Select the subgroup in the model 
mod_mean_recid_rep_cbt_treat_tr_off <- rma.glmm(measure="IRR", x1i=pt_incidents_treat, t1i=pt_freedays_treat, x2i=pt_incidents_cont, t2i=pt_freedays_cont, 
                                               subset=(reported_CBT_based_treatment_treat_recode== "Yes"), data=mean_recid_analysis, model="CM.EL")
# Create an object for the desired information  
mod_mean_recid_rep_cbt_treat_tr_off_list <- c('Reported CBT treatment (Tr)', 'treatment', 'Offered or required', mod_mean_recid_rep_cbt_treat_tr_off$k, mod_mean_recid_rep_cbt_treat_tr_off$beta[1], mod_mean_recid_rep_cbt_treat_tr_off$se[1], mod_mean_recid_rep_cbt_treat_tr_off$zval[1], mod_mean_recid_rep_cbt_treat_tr_off$pval[1], 
                                             mod_mean_recid_rep_cbt_treat_tr_off$ci.lb[1], mod_mean_recid_rep_cbt_treat_tr_off$ci.ub[1], mod_mean_recid_rep_cbt_treat_tr_off$k- length(mod_mean_recid_rep_cbt_treat_tr_off$b), NA, NA, 
                                             mod_mean_recid_rep_cbt_treat_tr_off$I2)
# name the object's columns and bind to mean_moderator_output
names(mod_mean_recid_rep_cbt_treat_tr_off_list) <- c("name", "mod_category", "level", "level_k", "est_LIRR", "est_SE","est_z","est_z_p","est_cl","est_cu", "res_DF", "Qres", "Qres_p", "I2")
mean_moderator_output <- bind_rows(mean_moderator_output, mod_mean_recid_rep_cbt_treat_tr_off_list)
########################################
# 6.79: Reported cognitive-behavioral therapy - Control 
#####################################
table(mean_recid_analysis$reported_CBT_based_treatment_cont, mean_recid_analysis$studyid)
# Note: there are insufficient numbers across groups to compute a moderator/subgroup analysis
########################################
# 6.80: Reported counseling - Treatment 
#####################################
table(mean_recid_analysis$reported_counseling_treat, mean_recid_analysis$studyid)
# Note: this variable separates the subgroups in the same way as 6.73: Reported relapse prevention - Treatment and will be excluded from the mean recidivist moderator analysis.
########################################
# 6.81: Reported counseling - Control 
#####################################
table(mean_recid_analysis$reported_counseling_cont, mean_recid_analysis$studyid)
# Note: there are insufficient numbers across groups to compute a moderator/subgroup analysis
########################################
# 6.82: Reported housing support - Treatment 
#####################################
table(mean_recid_analysis$reported_housing_support_treat, mean_recid_analysis$studyid)
########################################
# 6.82.1: Reported housing support - Treatment - No
#####################################
# Select the subgroup in the model 
mod_mean_recid_rep_house_supp_tr_no <- rma.glmm(measure="IRR", x1i=pt_incidents_treat, t1i=pt_freedays_treat, x2i=pt_incidents_cont, t2i=pt_freedays_cont, 
                                               subset=(reported_housing_support_treat== "No"), data=mean_recid_analysis, model="CM.EL")
# Create an object for the desired information  
mod_mean_recid_rep_house_supp_tr_no_list <- c('Reported housing support (Tr)', 'treatment', 'No (ref)', mod_mean_recid_rep_house_supp_tr_no$k, mod_mean_recid_rep_house_supp_tr_no$beta[1], mod_mean_recid_rep_house_supp_tr_no$se[1], mod_mean_recid_rep_house_supp_tr_no$zval[1], mod_mean_recid_rep_house_supp_tr_no$pval[1], 
                                             mod_mean_recid_rep_house_supp_tr_no$ci.lb[1], mod_mean_recid_rep_house_supp_tr_no$ci.ub[1], mod_mean_recid_rep_house_supp_tr_no$k- length(mod_mean_recid_rep_house_supp_tr_no$b), NA, NA, 
                                             mod_mean_recid_rep_house_supp_tr_no$I2)
# name the object's columns and bind to mean_moderator_output
names(mod_mean_recid_rep_house_supp_tr_no_list) <- c("name", "mod_category", "level", "level_k", "est_LIRR", "est_SE","est_z","est_z_p","est_cl","est_cu", "res_DF", "Qres", "Qres_p", "I2")
mean_moderator_output <- bind_rows(mean_moderator_output, mod_mean_recid_rep_house_supp_tr_no_list)
########################################
# 6.82.2: Reported housing support - Treatment - Offered
#####################################
# Select the subgroup in the model 
mod_mean_recid_rep_house_supp_tr_off <- rma.glmm(measure="IRR", x1i=pt_incidents_treat, t1i=pt_freedays_treat, x2i=pt_incidents_cont, t2i=pt_freedays_cont, 
                                                subset=(reported_housing_support_treat== "offered"), data=mean_recid_analysis, model="CM.EL")
# Create an object for the desired information  
mod_mean_recid_rep_house_supp_tr_off_list <- c('Reported housing support (Tr)', 'treatment', 'Offered', mod_mean_recid_rep_house_supp_tr_off$k, mod_mean_recid_rep_house_supp_tr_off$beta[1], mod_mean_recid_rep_house_supp_tr_off$se[1], mod_mean_recid_rep_house_supp_tr_off$zval[1], mod_mean_recid_rep_house_supp_tr_off$pval[1], 
                                              mod_mean_recid_rep_house_supp_tr_off$ci.lb[1], mod_mean_recid_rep_house_supp_tr_off$ci.ub[1], mod_mean_recid_rep_house_supp_tr_off$k- length(mod_mean_recid_rep_house_supp_tr_off$b), NA, NA, 
                                              mod_mean_recid_rep_house_supp_tr_off$I2)
# name the object's columns and bind to mean_moderator_output
names(mod_mean_recid_rep_house_supp_tr_off_list) <- c("name", "mod_category", "level", "level_k", "est_LIRR", "est_SE","est_z","est_z_p","est_cl","est_cu", "res_DF", "Qres", "Qres_p", "I2")
mean_moderator_output <- bind_rows(mean_moderator_output, mod_mean_recid_rep_house_supp_tr_off_list)

########################################
# 6.83: Reported housing support - Control 
#####################################
table(mean_recid_analysis$reported_housing_support_cont, mean_recid_analysis$studyid)
# Note: there are insufficient numbers across groups to compute a moderator/subgroup analysis
########################################
# 6.84: Reported support groups - Treatment 
#####################################
table(mean_recid_analysis$reported_support_groups_treat, mean_recid_analysis$studyid)
# Note: to be included this variable would need to be recoded in such a way that the subgroups would be the same as those split in  6.71:Reported pharmacotherapy treatment - Treatment.
#       it so will be excluded from the mean revidivist moderator analysis.
########################################
# 6.85: Reported support groups - Control 
#####################################
table(mean_recid_analysis$reported_support_groups_.cont, mean_recid_analysis$studyid)
# Note: there are insufficient numbers across groups to compute a moderator/subgroup analysis
########################################
# 6.86: Reported material aid - Treatment 
#####################################
table(mean_recid_analysis$reported_material_aid_treat, mean_recid_analysis$studyid)
########################################
# 6.86.1: Reported material aid - Treatment - No
#####################################
# Select the subgroup in the model 
mod_mean_recid_rep_mat_aid_tr_no <- rma.glmm(measure="IRR", x1i=pt_incidents_treat, t1i=pt_freedays_treat, x2i=pt_incidents_cont, t2i=pt_freedays_cont, 
                                                subset=(reported_support_groups_treat== "No"), data=mean_recid_analysis, model="CM.EL")
# Create an object for the desired information  
mod_mean_recid_rep_mat_aid_tr_no_list <- c('Reported material aid (Tr)', 'treatment', 'No (ref)', mod_mean_recid_rep_mat_aid_tr_no$k, mod_mean_recid_rep_mat_aid_tr_no$beta[1], mod_mean_recid_rep_mat_aid_tr_no$se[1], mod_mean_recid_rep_mat_aid_tr_no$zval[1], mod_mean_recid_rep_mat_aid_tr_no$pval[1], 
                                              mod_mean_recid_rep_mat_aid_tr_no$ci.lb[1], mod_mean_recid_rep_mat_aid_tr_no$ci.ub[1], mod_mean_recid_rep_mat_aid_tr_no$k- length(mod_mean_recid_rep_mat_aid_tr_no$b), NA, NA, 
                                              mod_mean_recid_rep_mat_aid_tr_no$I2)
# name the object's columns and bind to mean_moderator_output
names(mod_mean_recid_rep_mat_aid_tr_no_list) <- c("name", "mod_category", "level", "level_k", "est_LIRR", "est_SE","est_z","est_z_p","est_cl","est_cu", "res_DF", "Qres", "Qres_p", "I2")
mean_moderator_output <- bind_rows(mean_moderator_output, mod_mean_recid_rep_mat_aid_tr_no_list)
########################################
# 6.86.2: Reported material aid - Treatment - Offered
#####################################
# Select the subgroup in the model 
mod_mean_recid_rep_mat_aid_tr_off <- rma.glmm(measure="IRR", x1i=pt_incidents_treat, t1i=pt_freedays_treat, x2i=pt_incidents_cont, t2i=pt_freedays_cont, 
                                             subset=(reported_support_groups_treat== "offered"), data=mean_recid_analysis, model="CM.EL")
# Create an object for the desired information  
mod_mean_recid_rep_mat_aid_tr_off_list <- c('Reported material aid (Tr)', 'treatment', 'Offered', mod_mean_recid_rep_mat_aid_tr_off$k, mod_mean_recid_rep_mat_aid_tr_off$beta[1], mod_mean_recid_rep_mat_aid_tr_off$se[1], mod_mean_recid_rep_mat_aid_tr_off$zval[1], mod_mean_recid_rep_mat_aid_tr_off$pval[1], 
                                           mod_mean_recid_rep_mat_aid_tr_off$ci.lb[1], mod_mean_recid_rep_mat_aid_tr_off$ci.ub[1], mod_mean_recid_rep_mat_aid_tr_off$k- length(mod_mean_recid_rep_mat_aid_tr_off$b), NA, NA, 
                                           mod_mean_recid_rep_mat_aid_tr_off$I2)
# name the object's columns and bind to mean_moderator_output
names(mod_mean_recid_rep_mat_aid_tr_off_list) <- c("name", "mod_category", "level", "level_k", "est_LIRR", "est_SE","est_z","est_z_p","est_cl","est_cu", "res_DF", "Qres", "Qres_p", "I2")
mean_moderator_output <- bind_rows(mean_moderator_output, mod_mean_recid_rep_mat_aid_tr_off_list)
########################################
# 6.87: Reported material aid - Control 
#####################################
table(mean_recid_analysis$reported_material_aid_cont, mean_recid_analysis$studyid)
# Note: there are insufficient numbers across groups to compute a moderator/subgroup analysis
########################################
# 6.88: Reported health services- Treatment 
#####################################
table(mean_recid_analysis$reported_health_services_treat, mean_recid_analysis$studyid)
# Note: there are insufficient numbers across groups to compute a moderator/subgroup analysis
########################################
# 6.89: Reported health services- Control 
#####################################
table(mean_recid_analysis$reported_health_services_cont, mean_recid_analysis$studyid)
# Note: there are insufficient numbers across groups to compute a moderator/subgroup analysis
########################################
# 6.90: Reported domestic violence program - Treatment 
#####################################
table(mean_recid_analysis$reported_domestic_violence_prog_treat, mean_recid_analysis$studyid)
# Note: there are insufficient numbers across groups to compute a moderator/subgroup analysis
########################################
# 6.91: Reported domestic violence program - Control 
#####################################
table(mean_recid_analysis$reported_domestic_violence_prog_cont, mean_recid_analysis$studyid)
# Note: there are insufficient numbers across groups to compute a moderator/subgroup analysis
########################################
# 6.92: Reported vocational or educational training - Treatment 
#####################################
table(mean_recid_analysis$reported_edu_or_vocational_services_treat, mean_recid_analysis$studyid)
########################################
# 6.92.1: Reported vocational or educational training - Treatment - No
#####################################
# Select the subgroup in the model 
mod_mean_recid_rep_edu_voc_serv_tr_no <- rma.glmm(measure="IRR", x1i=pt_incidents_treat, t1i=pt_freedays_treat, x2i=pt_incidents_cont, t2i=pt_freedays_cont, 
                                                subset=(reported_edu_or_vocational_services_treat== "No"), data=mean_recid_analysis, model="CM.EL")
# Create an object for the desired information  
mod_mean_recid_rep_edu_voc_serv_tr_no_list <- c('Reported vocational or educational services (Tr)', 'treatment', 'No (ref)', mod_mean_recid_rep_edu_voc_serv_tr_no$k, mod_mean_recid_rep_edu_voc_serv_tr_no$beta[1], mod_mean_recid_rep_edu_voc_serv_tr_no$se[1], mod_mean_recid_rep_edu_voc_serv_tr_no$zval[1], mod_mean_recid_rep_edu_voc_serv_tr_no$pval[1], 
                                              mod_mean_recid_rep_edu_voc_serv_tr_no$ci.lb[1], mod_mean_recid_rep_edu_voc_serv_tr_no$ci.ub[1], mod_mean_recid_rep_edu_voc_serv_tr_no$k- length(mod_mean_recid_rep_edu_voc_serv_tr_no$b), NA, NA, 
                                              mod_mean_recid_rep_edu_voc_serv_tr_no$I2)
# name the object's columns and bind to mean_moderator_output
names(mod_mean_recid_rep_edu_voc_serv_tr_no_list) <- c("name", "mod_category", "level", "level_k", "est_LIRR", "est_SE","est_z","est_z_p","est_cl","est_cu", "res_DF", "Qres", "Qres_p", "I2")
mean_moderator_output <- bind_rows(mean_moderator_output, mod_mean_recid_rep_edu_voc_serv_tr_no_list)
########################################
# 6.92.2: Reported vocational or educational training - Treatment - Offered or required
#####################################
# Select the subgroup in the model 
mod_mean_recid_rep_edu_voc_serv_tr_yes <- rma.glmm(measure="IRR", x1i=pt_incidents_treat, t1i=pt_freedays_treat, x2i=pt_incidents_cont, t2i=pt_freedays_cont, 
                                                   subset=!(reported_edu_or_vocational_services_treat== "No"), data=mean_recid_analysis, model="CM.EL")
# Create an object for the desired information  
mod_mean_recid_rep_edu_voc_serv_tr_yes_list <- c('Reported vocational or educational services (Tr)', 'treatment', 'Offered or required', mod_mean_recid_rep_edu_voc_serv_tr_yes$k, mod_mean_recid_rep_edu_voc_serv_tr_yes$beta[1], mod_mean_recid_rep_edu_voc_serv_tr_yes$se[1], mod_mean_recid_rep_edu_voc_serv_tr_yes$zval[1], mod_mean_recid_rep_edu_voc_serv_tr_yes$pval[1], 
                                                 mod_mean_recid_rep_edu_voc_serv_tr_yes$ci.lb[1], mod_mean_recid_rep_edu_voc_serv_tr_yes$ci.ub[1], mod_mean_recid_rep_edu_voc_serv_tr_yes$k- length(mod_mean_recid_rep_edu_voc_serv_tr_yes$b), NA, NA, 
                                                 mod_mean_recid_rep_edu_voc_serv_tr_yes$I2)
# name the object's columns and bind to mean_moderator_output
names(mod_mean_recid_rep_edu_voc_serv_tr_yes_list) <- c("name", "mod_category", "level", "level_k", "est_LIRR", "est_SE","est_z","est_z_p","est_cl","est_cu", "res_DF", "Qres", "Qres_p", "I2")
mean_moderator_output <- bind_rows(mean_moderator_output, mod_mean_recid_rep_edu_voc_serv_tr_yes_list)
########################################
# 6.93: Reported vocational or educational training - Control 
#####################################
table(mean_recid_analysis$reported_edu_or_vocational_services_cont, mean_recid_analysis$studyid)
# Note: there are insufficient numbers across groups to compute a moderator/subgroup analysis
########################################
# 6.94: Reported anger management - Treatment 
#####################################
table(mean_recid_analysis$reported_anger_management_treat, mean_recid_analysis$studyid)
########################################
# 6.94.1: Reported anger management - Treatment - No
#####################################
# Select the subgroup in the model 
mod_mean_recid_rep_anger_man_tr_no <- rma.glmm(measure="IRR", x1i=pt_incidents_treat, t1i=pt_freedays_treat, x2i=pt_incidents_cont, t2i=pt_freedays_cont, 
                                                  subset=(reported_anger_management_treat== "No"), data=mean_recid_analysis, model="CM.EL")
# Create an object for the desired information  
mod_mean_recid_rep_anger_man_tr_no_list <- c('Reported anger management (Tr)', 'treatment', 'No (ref)', mod_mean_recid_rep_anger_man_tr_no$k, mod_mean_recid_rep_anger_man_tr_no$beta[1], mod_mean_recid_rep_anger_man_tr_no$se[1], mod_mean_recid_rep_anger_man_tr_no$zval[1], mod_mean_recid_rep_anger_man_tr_no$pval[1], 
                                                mod_mean_recid_rep_anger_man_tr_no$ci.lb[1], mod_mean_recid_rep_anger_man_tr_no$ci.ub[1], mod_mean_recid_rep_anger_man_tr_no$k- length(mod_mean_recid_rep_anger_man_tr_no$b), NA, NA, 
                                                mod_mean_recid_rep_anger_man_tr_no$I2)
# name the object's columns and bind to mean_moderator_output
names(mod_mean_recid_rep_anger_man_tr_no_list) <- c("name", "mod_category", "level", "level_k", "est_LIRR", "est_SE","est_z","est_z_p","est_cl","est_cu", "res_DF", "Qres", "Qres_p", "I2")
mean_moderator_output <- bind_rows(mean_moderator_output, mod_mean_recid_rep_anger_man_tr_no_list)
########################################
# 6.94.1: Reported anger management - Treatment - Offered
#####################################
# Select the subgroup in the model 
mod_mean_recid_rep_anger_man_tr_off <- rma.glmm(measure="IRR", x1i=pt_incidents_treat, t1i=pt_freedays_treat, x2i=pt_incidents_cont, t2i=pt_freedays_cont, 
                                               subset=(reported_anger_management_treat== "offered"), data=mean_recid_analysis, model="CM.EL")
# Create an object for the desired information  
mod_mean_recid_rep_anger_man_tr_off_list <- c('Reported anger management (Tr)', 'treatment', 'Offered', mod_mean_recid_rep_anger_man_tr_off$k, mod_mean_recid_rep_anger_man_tr_off$beta[1], mod_mean_recid_rep_anger_man_tr_off$se[1], mod_mean_recid_rep_anger_man_tr_off$zval[1], mod_mean_recid_rep_anger_man_tr_off$pval[1], 
                                             mod_mean_recid_rep_anger_man_tr_off$ci.lb[1], mod_mean_recid_rep_anger_man_tr_off$ci.ub[1], mod_mean_recid_rep_anger_man_tr_off$k- length(mod_mean_recid_rep_anger_man_tr_off$b), NA, NA, 
                                             mod_mean_recid_rep_anger_man_tr_off$I2)
# name the object's columns and bind to mean_moderator_output
names(mod_mean_recid_rep_anger_man_tr_off_list) <- c("name", "mod_category", "level", "level_k", "est_LIRR", "est_SE","est_z","est_z_p","est_cl","est_cu", "res_DF", "Qres", "Qres_p", "I2")
mean_moderator_output <- bind_rows(mean_moderator_output, mod_mean_recid_rep_anger_man_tr_off_list)
########################################
# 6.95: Reported anger management - Control 
#####################################
table(mean_recid_analysis$reported_anger_management_cont, mean_recid_analysis$studyid)
# Note: there are insufficient numbers across groups to compute a moderator/subgroup analysis
########################################
# 6.96: Reported life-skills training - Treatment 
#####################################
table(mean_recid_analysis$reported_life_skills_treat, mean_recid_analysis$studyid)
########################################
# 6.96.1: Reported life-skills training - Treatment - No
#####################################
# Select the subgroup in the model 
mod_mean_recid_rep_life_skill_tr_no <- rma.glmm(measure="IRR", x1i=pt_incidents_treat, t1i=pt_freedays_treat, x2i=pt_incidents_cont, t2i=pt_freedays_cont, 
                                               subset=(reported_life_skills_treat== "No"), data=mean_recid_analysis, model="CM.EL")
# Create an object for the desired information  
mod_mean_recid_rep_life_skill_tr_no_list <- c('Reported life-skills training (Tr)', 'treatment', 'No (ref)', mod_mean_recid_rep_life_skill_tr_no$k, mod_mean_recid_rep_life_skill_tr_no$beta[1], mod_mean_recid_rep_life_skill_tr_no$se[1], mod_mean_recid_rep_life_skill_tr_no$zval[1], mod_mean_recid_rep_life_skill_tr_no$pval[1], 
                                             mod_mean_recid_rep_life_skill_tr_no$ci.lb[1], mod_mean_recid_rep_life_skill_tr_no$ci.ub[1], mod_mean_recid_rep_life_skill_tr_no$k- length(mod_mean_recid_rep_life_skill_tr_no$b), NA, NA, 
                                             mod_mean_recid_rep_life_skill_tr_no$I2)
# name the object's columns and bind to mean_moderator_output
names(mod_mean_recid_rep_life_skill_tr_no_list) <- c("name", "mod_category", "level", "level_k", "est_LIRR", "est_SE","est_z","est_z_p","est_cl","est_cu", "res_DF", "Qres", "Qres_p", "I2")
mean_moderator_output <- bind_rows(mean_moderator_output, mod_mean_recid_rep_life_skill_tr_no_list)
########################################
# 6.96.2: Reported life-skills training - Treatment - Offered or required
#####################################
# Select the subgroup in the model 
mod_mean_recid_rep_life_skill_tr_yes <- rma.glmm(measure="IRR", x1i=pt_incidents_treat, t1i=pt_freedays_treat, x2i=pt_incidents_cont, t2i=pt_freedays_cont, 
                                                 subset=!(reported_life_skills_treat== "no"), data=mean_recid_analysis, model="CM.EL")
# Create an object for the desired information  
mod_mean_recid_rep_life_skill_tr_yes_list <- c('Reported life-skills training (Tr)', 'treatment', 'Offered or required', mod_mean_recid_rep_life_skill_tr_yes$k, mod_mean_recid_rep_life_skill_tr_yes$beta[1], mod_mean_recid_rep_life_skill_tr_yes$se[1], mod_mean_recid_rep_life_skill_tr_yes$zval[1], mod_mean_recid_rep_life_skill_tr_yes$pval[1], 
                                               mod_mean_recid_rep_life_skill_tr_yes$ci.lb[1], mod_mean_recid_rep_life_skill_tr_yes$ci.ub[1], mod_mean_recid_rep_life_skill_tr_yes$k- length(mod_mean_recid_rep_life_skill_tr_yes$b), NA, NA, 
                                               mod_mean_recid_rep_life_skill_tr_yes$I2)
# name the object's columns and bind to mean_moderator_output
names(mod_mean_recid_rep_life_skill_tr_yes_list) <- c("name", "mod_category", "level", "level_k", "est_LIRR", "est_SE","est_z","est_z_p","est_cl","est_cu", "res_DF", "Qres", "Qres_p", "I2")
mean_moderator_output <- bind_rows(mean_moderator_output, mod_mean_recid_rep_life_skill_tr_yes_list)
########################################
# 6.97: Reported life-skills training - Control 
#####################################
table(mean_recid_analysis$reported_life_skills_cont, mean_recid_analysis$studyid)
# Note: there are insufficient numbers across groups to compute a moderator/subgroup analysis
########################################
# 6.98: Reported family support services - Treatment 
#####################################
table(mean_recid_analysis$reported_family_support_service_or_training_treat, mean_recid_analysis$studyid)
# Note: there are insufficient numbers across groups to compute a moderator/subgroup analysis
########################################
# 6.99: Reported family support services - Control 
#####################################
table(mean_recid_analysis$reported_family_support_service_or_training_cont, mean_recid_analysis$studyid)
# Note: there are insufficient numbers across groups to compute a moderator/subgroup analysis
########################################
# 6.100: Reported prison intervention - Control 
#####################################
table(mean_recid_analysis$reported_prison_cont, mean_recid_analysis$studyid)
# Note: this variable separates the subgroups in the same way as 6.59: Reported measuring risk - Treatment and will be excluded from the mean revidivist moderator analysis.
########################################
# 6.101: Other treatments - Treatment 
#####################################
table(mean_recid_analysis$other_treatment_treat, mean_recid_analysis$studyid)
# Note: there are insufficient numbers across groups to compute a moderator/subgroup analysis
########################################
# 6.102: Other treatments - Control 
#####################################
table(mean_recid_analysis$other_treatment_cont, mean_recid_analysis$studyid)
# Note: there are insufficient numbers across groups to compute a moderator/subgroup analysis
########################################
# 6.103: Total number of treatments required - Treatment 
#####################################
table(mean_recid_analysis$total_treatments_required_treat, mean_recid_analysis$studyid)
# Insert the moderator into the model
mod_mean_recid_post_treats_req_tr <- rma.glmm(measure="IRR", x1i=pt_incidents_treat, t1i=pt_freedays_treat, x2i=pt_incidents_cont, t2i=pt_freedays_cont, 
                                                mods = ~ total_treatments_required_treat, data=mean_recid_analysis, model="CM.EL")
# Create an object for the desired information and create a 'moderator_output' data frame 
mod_mean_recid_post_treats_req_tr_list<- c('Total number of treatments required (Tr)', 'treatment', 'Beta_weight', mod_mean_recid_post_treats_req_tr$k, mod_mean_recid_post_treats_req_tr$beta[2, 1], mod_mean_recid_post_treats_req_tr$se[2], mod_mean_recid_post_treats_req_tr$zval[2], mod_mean_recid_post_treats_req_tr$pval[2], 
                                             mod_mean_recid_post_treats_req_tr$ci.lb[2], mod_mean_recid_post_treats_req_tr$ci.ub[2], mod_mean_recid_post_treats_req_tr$k- length(mod_mean_recid_post_treats_req_tr$b), NA, NA, mod_mean_recid_post_treats_req_tr$I2)
# name the object's columns 
names(mod_mean_recid_post_treats_req_tr_list) <- c("name", "mod_category", "level", "level_k", "est_LIRR", "est_SE","est_z","est_z_p","est_cl","est_cu", "res_DF", "Qres", "Qres_p", "I2")
mean_moderator_output <- bind_rows(mean_moderator_output, mod_mean_recid_post_treats_req_tr_list)
########################################
# 6.104: Total number of treatments required - Control 
#####################################
table(mean_recid_analysis$total_treatments_required_cont, mean_recid_analysis$studyid)
# Note: there are insufficient numbers across groups to compute a moderator/subgroup analysis
########################################
# 6.105: Total number of treatments offered - Treatment 
#####################################
table(mean_recid_analysis$total_treatments_offered_treat, mean_recid_analysis$studyid)
# Insert the moderator into the model
mod_mean_recid_post_treats_off_tr <- rma.glmm(measure="IRR", x1i=pt_incidents_treat, t1i=pt_freedays_treat, x2i=pt_incidents_cont, t2i=pt_freedays_cont, 
                                              mods = ~ total_treatments_offered_treat, data=mean_recid_analysis, model="CM.EL")
# Create an object for the desired information and create a 'moderator_output' data frame 
mod_mean_recid_post_treats_off_tr_list<- c('Total number of treatments offered (Tr)', 'treatment', 'Beta_weight', mod_mean_recid_post_treats_off_tr$k, mod_mean_recid_post_treats_off_tr$beta[2, 1], mod_mean_recid_post_treats_off_tr$se[2], mod_mean_recid_post_treats_off_tr$zval[2], mod_mean_recid_post_treats_off_tr$pval[2], 
                                           mod_mean_recid_post_treats_off_tr$ci.lb[2], mod_mean_recid_post_treats_off_tr$ci.ub[2], mod_mean_recid_post_treats_off_tr$k- length(mod_mean_recid_post_treats_off_tr$b), NA, NA, mod_mean_recid_post_treats_off_tr$I2)
# name the object's columns 
names(mod_mean_recid_post_treats_off_tr_list) <- c("name", "mod_category", "level", "level_k", "est_LIRR", "est_SE","est_z","est_z_p","est_cl","est_cu", "res_DF", "Qres", "Qres_p", "I2")
mean_moderator_output <- bind_rows(mean_moderator_output, mod_mean_recid_post_treats_off_tr_list)
########################################
# 6.106: Total number of treatments offered - Control 
#####################################
table(mean_recid_analysis$total_treatments_offered_cont, mean_recid_analysis$studyid)
# Note: there are insufficient numbers across groups to compute a moderator/subgroup analysis
########################################
# 6.107: Modified Maryland Scientific methods scale rating
#####################################
table(mean_recid_analysis$MSMS, mean_recid_analysis$studyid)
# Insert the moderator into the model
mod_mean_recid_MSMS_rate <- rma.glmm(measure="IRR", x1i=pt_incidents_treat, t1i=pt_freedays_treat, x2i=pt_incidents_cont, t2i=pt_freedays_cont, 
                                              mods = ~ MSMS, data=mean_recid_analysis, model="CM.EL")
# Create an object for the desired information and create a 'moderator_output' data frame 
mod_mean_recid_MSMS_rate_list<- c('MSMS rating', 'methodological', 'Beta_weight', mod_mean_recid_MSMS_rate$k, mod_mean_recid_MSMS_rate$beta[2, 1], mod_mean_recid_MSMS_rate$se[2], mod_mean_recid_MSMS_rate$zval[2], mod_mean_recid_MSMS_rate$pval[2], 
                                           mod_mean_recid_MSMS_rate$ci.lb[2], mod_mean_recid_MSMS_rate$ci.ub[2], mod_mean_recid_MSMS_rate$k- length(mod_mean_recid_MSMS_rate$b), NA, NA, mod_mean_recid_MSMS_rate$I2)
# name the object's columns 
names(mod_mean_recid_MSMS_rate_list) <- c("name", "mod_category", "level", "level_k", "est_LIRR", "est_SE","est_z","est_z_p","est_cl","est_cu", "res_DF", "Qres", "Qres_p", "I2")
mean_moderator_output <- bind_rows(mean_moderator_output, mod_mean_recid_MSMS_rate_list)
########################################
# 6.108: Modified Maryland Scientific methods scale rating - Sensitivity analysis re-code a: midway category
#####################################
# Create a variable that recodes studyies rated as '3' on the MSMS for the reason that participants were matched 'Participants matched on some variables using a valid and reliable statistical technique, but unmatched on most of the important confounding variables ' as 4
mean_recid_analysis$MSMS_recode1 <- ifelse(mean_recid_analysis$MSMS == '3' & mean_recid_analysis$MSMS_reason == 'Participants matched on some variables using a valid and reliable statistical technique, but unmatched on most of the important confounding variables ',
                                           3.5, mean_recid_analysis$MSMS)
# Insert the moderator into the model
mod_mean_recid_MSMS_recode1 <- rma.glmm(measure="IRR", x1i=pt_incidents_treat, t1i=pt_freedays_treat, x2i=pt_incidents_cont, t2i=pt_freedays_cont, 
                                             mods = ~ MSMS_recode1, data=mean_recid_analysis, model="CM.EL")
# Create an object for the desired information and create a 'moderator_output' data frame 
mod_mean_recid_MSMS_recode1_list<- c('MSMS rating recode #a', 'methodological', 'Beta_weight', mod_mean_recid_MSMS_recode1$k, mod_mean_recid_MSMS_recode1$beta[2, 1], mod_mean_recid_MSMS_recode1$se[2], mod_mean_recid_MSMS_recode1$zval[2], mod_mean_recid_MSMS_recode1$pval[2], 
                                          mod_mean_recid_MSMS_recode1$ci.lb[2], mod_mean_recid_MSMS_recode1$ci.ub[2], mod_mean_recid_MSMS_recode1$k- length(mod_mean_recid_MSMS_recode1$b), NA, NA, mod_mean_recid_MSMS_recode1$I2)
# name the object's columns 
names(mod_mean_recid_MSMS_recode1_list) <- c("name", "mod_category", "level", "level_k", "est_LIRR", "est_SE","est_z","est_z_p","est_cl","est_cu", "res_DF", "Qres", "Qres_p", "I2")
mean_moderator_output <- bind_rows(mean_moderator_output, mod_mean_recid_MSMS_recode1_list)
########################################
# 6.109: Modified Maryland Scientific methods scale rating - Sensitivity analysis re-code b: considered 'well-matched' 
#####################################
# Create a variable that recodes studyies rated as '3' on the MSMS for the reason that participants were matched 'Participants matched on some variables using a valid and reliable statistical technique, but unmatched on most of the important confounding variables ' as 4
mean_recid_analysis$MSMS_recode2 <- ifelse(mean_recid_analysis$MSMS == '3' & mean_recid_analysis$MSMS_reason == 'Participants matched on some variables using a valid and reliable statistical technique, but unmatched on most of the important confounding variables ',
                                          4, mean_recid_analysis$MSMS)
# Insert the moderator into the model
mod_mean_recid_MSMS_recode2 <- rma.glmm(measure="IRR", x1i=pt_incidents_treat, t1i=pt_freedays_treat, x2i=pt_incidents_cont, t2i=pt_freedays_cont, 
                                     mods = ~ MSMS_recode2, data=mean_recid_analysis, model="CM.EL")
# Create an object for the desired information and create a 'moderator_output' data frame 
mod_mean_recid_MSMS_recode2_list<- c('MSMS rating recode #b', 'methodological', 'Beta_weight', mod_mean_recid_MSMS_recode2$k, mod_mean_recid_MSMS_recode2$beta[2, 1], mod_mean_recid_MSMS_recode2$se[2], mod_mean_recid_MSMS_recode2$zval[2], mod_mean_recid_MSMS_recode2$pval[2], 
                                  mod_mean_recid_MSMS_recode2$ci.lb[2], mod_mean_recid_MSMS_recode2$ci.ub[2], mod_mean_recid_MSMS_recode2$k- length(mod_mean_recid_MSMS_recode2$b), NA, NA, mod_mean_recid_MSMS_recode2$I2)
# name the object's columns 
names(mod_mean_recid_MSMS_recode2_list) <- c("name", "mod_category", "level", "level_k", "est_LIRR", "est_SE","est_z","est_z_p","est_cl","est_cu", "res_DF", "Qres", "Qres_p", "I2")
mean_moderator_output <- bind_rows(mean_moderator_output, mod_mean_recid_MSMS_recode2_list)
########################################
# 7: Moderator analysis: Z test on subgroup differences
#####################################
########################################
# 7.1: Proportion who recidivated
#####################################
########################################
# 7.1.1: Z test on subgroup differences: Proportion who recidivated
#####################################
#Organise the rows into their moderator category, add an id to each row, add a variable for ref rows, and the ID variable the first variable. 
prop_moderator_output <- arrange(prop_moderator_output, mod_category)
prop_moderator_output$mod_id <- 1:nrow(prop_moderator_output)
prop_moderator_output <- prop_moderator_output %>%
                                            select(mod_id, everything())
###Convert relevant variables to numeric
prop_moderator_output_num<- prop_moderator_output %>% select(-"name", -"mod_category",-"level") %>%
                                                apply(2,function(x) as.numeric(as.character(x))) 

prop_moderator_output_non_num <- prop_moderator_output %>% select("name", "mod_category","level") 

prop_moderator_output_numbered <-cbind(prop_moderator_output_non_num,prop_moderator_output_num) #bind numeric and character variables
prop_moderator_output_numbered <-prop_moderator_output_numbered[,names(prop_moderator_output)]
prop_moderator_output_numbered$reference <- ifelse(grepl("ref", prop_moderator_output_numbered$level) == TRUE, 'REF', 
                                                   ifelse(prop_moderator_output_numbered$level == 'Beta_weight', 'BETA', 'ALT'))

# Perform the 'Z-test method' for differences between groups described by Borenstein et al. (2009, p156):
# Diff = Mb - Ma
# Zdiff = Diff/ SEdiff
# SEdiff = sqrt(Vma + Vmb) 
#                       Where Mb and Ma are the two pooled effects, 
#                       and Vma and Vmb are the variance of the two pooled effects 

# Grouping by 'name', calculate Diff, SEdiff, and ZDiff.

# First, split the dataframe into to subgroups and Betas
prop_moderator_output_subgroups <- prop_moderator_output_numbered %>% 
                                            subset(., reference== "REF"|
                                                     reference== "ALT")
prop_moderator_output_betas <- prop_moderator_output_numbered %>% 
                                              subset(., reference== "BETA")

# Subtract the reference estimate LOR from the alternative level's estimate LOR
setDT(prop_moderator_output_subgroups)[, Diff:=  est_LOR - est_LOR[match('REF', reference)], .(mod_category , name)]

prop_moderator_output_subgroups$Diff[prop_moderator_output_subgroups$reference== 'REF'] <- NA
# Create a variable for the variances                                                                                                                     
prop_moderator_output_subgroups$Vm <- prop_moderator_output_subgroups$est_SE^2 
# Calculate SEdiff
setDT(prop_moderator_output_subgroups)[, SEdiff:= sqrt(Vm + Vm[match('REF', reference)]), .(mod_category , name)]
prop_moderator_output_subgroups$SEdiff[prop_moderator_output_subgroups$reference== 'REF'] <- NA
# Calculate Zdiff                                                                                               
prop_moderator_output_subgroups$Zdiff <- prop_moderator_output_subgroups$Diff/prop_moderator_output_subgroups$SEdiff

### A a two tailed p-value can then be calculated on the ZDiff using a normal distribution (Borenstein et al., 2009, p. 156)
prop_moderator_output_subgroups$Zdiff_p <- 2 * pnorm(abs(prop_moderator_output_subgroups$Zdiff), lower.tail = FALSE)

# bind the betas and subgroups dataframes back together
prop_moderator_output_bound <- bind_rows(prop_moderator_output_subgroups, prop_moderator_output_betas) 
prop_moderator_output_bound <- prop_moderator_output_bound[order(prop_moderator_output_bound$mod_id), ]

# create a new variable that contains the Beta's p values along with Zdiff_p
prop_moderator_output_bound$pval <- ifelse(prop_moderator_output_bound$reference=='BETA', prop_moderator_output_bound$est_z_p, 
                                           ifelse(prop_moderator_output_bound$reference=='ALT', prop_moderator_output_bound$Zdiff_p, NA))
########################################
# 7.1.2: Benjamini-Hochberg procedure: Proportion who recidivated
#####################################
# Separate a category from moderator analysis
prop_mod_category_methodological <-prop_moderator_output_bound%>%filter(mod_category=='methodological')
# Create a new variable that for BH adjusted p-values 
prop_mod_category_methodological$pval_adjusted <- p.adjust(prop_mod_category_methodological$pval, 
                                                         method = "BH")
# Repeat for the remaining moderator categories 
# sample_characteristics 
prop_mod_category_sample_characteristics <-prop_moderator_output_bound%>%filter(mod_category=='sample characteristics')
prop_mod_category_sample_characteristics$pval_adjusted <-p.adjust(prop_mod_category_sample_characteristics$pval, 
                                                                method = "BH")
# programmatic
prop_mod_category_programmatic<-prop_moderator_output_bound%>%filter(mod_category=='programmatic')
prop_mod_category_programmatic$pval_adjusted <-p.adjust(prop_mod_category_programmatic$pval, 
                                                      method = "BH")
# treatment
prop_mod_category_treatment<-prop_moderator_output_bound%>%filter(mod_category=='treatment')
prop_mod_category_treatment$pval_adjusted <-p.adjust(prop_mod_category_treatment$pval, 
                                                   method = "BH")
# bind the dataframes together 
prop_moderator_output_adjusted <-rbind(prop_mod_category_methodological,
                                      prop_mod_category_sample_characteristics,
                                      prop_mod_category_programmatic,
                                      prop_mod_category_treatment)
# Re-order by mod_id
prop_moderator_output_adjusted <- arrange(prop_moderator_output_adjusted, mod_id)
# check that the values from prop_moderator_output_bound and _prop_moderator_output_adjusted are the same 
prop_moderator_output_bound$mod_id==prop_moderator_output_adjusted$mod_id
prop_moderator_output_bound$pval==prop_moderator_output_adjusted$pval
# add a column for rounded p-values 
prop_moderator_output_adjusted$pval_adjusted_rounded <-round2(prop_moderator_output_adjusted$pval_adjusted, 3)
########################################
# 7.1.3: Clean proportion who recidivated moderator table
#####################################
# Remove irrelevant variables 
prop_moderator_export <- prop_moderator_output_adjusted %>% select(-est_z, -est_z_p, -res_DF, -Qres, -Qres_p, -Vm, -SEdiff,-Zdiff, -pval_adjusted_rounded)
                                                                   
# Exponentiate relevant columns and change variable names accordingly
prop_moderator_export <-prop_moderator_export %>% mutate_at(vars(est_LOR, est_cl, est_cu,Diff), list(~exp(.)))

# Format the p-values - split data frame to apply pvalr function 
prop_moderator_export_refs <- subset(prop_moderator_export, prop_moderator_export$reference == 'REF')
prop_moderator_export_bs_alts <- subset(prop_moderator_export, !prop_moderator_export$reference == 'REF')


prop_moderator_export_bs_alts$pval <- ifelse(prop_moderator_export_bs_alts$pval <.01, pvalr(prop_moderator_export_bs_alts$pval, digits = 3), pvalr(prop_moderator_export_bs_alts$pval, digits = 2))
prop_moderator_export_refs$pval <- as.character(prop_moderator_export_refs$pval)
prop_moderator_export <- rbind(prop_moderator_export_bs_alts, prop_moderator_export_refs)
prop_moderator_export <- prop_moderator_export[order(prop_moderator_export$mod_id),]

# Round relevant columns. Note: any value >.99 and <1 will be rounded to .99 so that rounding does not change the direction of the effect.
prop_moderator_export <- prop_moderator_export %>% mutate_at(vars(est_LOR, est_cl, est_cu, Diff), 
                                                             list(~ifelse(.<1 & .>.99, floor(.*100)/100, round2(., 2))))

# Add trailing zeros where necessary
prop_moderator_export <- prop_moderator_export %>% mutate_at(vars(est_LOR, est_cl, est_cu, Diff), 
                                                             list(~sprintf("%.2f",. )))

prop_moderator_export$est_SE <- ifelse(prop_moderator_export$est_SE <.01, sprintf("%.3f", round2(prop_moderator_export$est_SE, 3)),
                                       sprintf("%.2f",round2(prop_moderator_export$est_SE, 2)))

prop_moderator_export$I2 <- ifelse(prop_moderator_export$I2 >.001, sprintf("%.2f", round2(prop_moderator_export$I2, 2)),"<.001")

# Collapse confidence interval into one variable and add punctuation 
prop_moderator_export <-unite(prop_moderator_export, '95% CI', c(est_cl, est_cu), remove=TRUE)
prop_moderator_export$`95% CI` <- paste0("(", (prop_moderator_export$`95% CI`), ")")
prop_moderator_export$`95% CI` <-gsub("_", ", ", prop_moderator_export$`95% CI`)

# Add '-' for the Beta weight's diff values 
prop_moderator_export$Diff <- ifelse(prop_moderator_export$level == 'Beta_weight', ' - ', prop_moderator_export$Diff)

# Remove underscore from level
prop_moderator_export$level  <-gsub("_", " ", prop_moderator_export$level)

# Add significance levels to the unadjusted pval based on the adjusted pval
prop_moderator_export$pval<- ifelse(prop_moderator_export$pval_adjusted > 0.01 & prop_moderator_export$pval_adjusted<0.05, paste0(prop_moderator_export$pval,"*"),
                                            ifelse(prop_moderator_export$pval_adjusted>0.001 & prop_moderator_export$pval_adjusted<0.01, paste0(prop_moderator_export$pval,"**"),
                                                   ifelse(prop_moderator_export$pval_adjusted< 0.001, paste0(prop_moderator_export$pval,"***"), 
                                                          prop_moderator_export$pval)))

# Reorder each moderator data frame alphabetically within each moderator category
prop_moderator_export <- arrange(prop_moderator_export,mod_category, name)

# Insert 'blank' into name and Diff so that we can replace them with blank space
#prop_moderator_export$name <- ifelse(prop_moderator_export$reference == 'ALT', 'blank', prop_moderator_export$name)
prop_moderator_export$Diff <- ifelse(prop_moderator_export$Diff == 'NA', 'blank', prop_moderator_export$Diff)
prop_moderator_export$pval <- as.character(replace_na(prop_moderator_export$pval,'blank'))

# Replace NAs with blank space
prop_moderator_export <- prop_moderator_export %>% mutate_at(vars(name,Diff,pval), list(~gsub('blank', "", .)))

# Rename columns for output 
prop_moderator_export <- plyr::rename(prop_moderator_export , c("name" = "Moderator","level" = "Level", "est_LOR" = "OR", "level_k" = "k", "est_SE" = "SE", "pval" = "Zdiff/beta p-value", "reference"))

# Drop irrelevant columns and export 
prop_moderator_export_final<- prop_moderator_export[, c("mod_category", "Moderator", "reference", "Level","k", "OR", "95% CI", "I2", "Zdiff/beta p-value")]

# export table to CSV 
# write.csv(prop_moderator_export_final, file = "Proportion that recidivated moderator table.csv")

########################################
# 7.2: Mean recidivist events
#####################################
########################################
# 7.2.1: Z test on subgroup differences: Mean recidivist events 
#####################################
# Organise the rows into their moderator category, add an id to each row, add a variable for ref rows, and the ID variable the first variable. 
mean_moderator_output_z <- arrange(mean_moderator_output, mod_category)
mean_moderator_output_z$mod_id <- 1:nrow(mean_moderator_output_z)
mean_moderator_output_z <- mean_moderator_output_z %>%
                                                select(mod_id, everything())
# Convert relevant variables to numeric
mean_moderator_output_num<- mean_moderator_output_z %>% select(-"name", -"mod_category",-"level") %>%
                                        apply(2,function(x) as.numeric(as.character(x))) 

mean_moderator_output_non_num <- mean_moderator_output_z %>% select("name", "mod_category","level") 

mean_moderator_output_numbered <-cbind(mean_moderator_output_non_num,mean_moderator_output_num) #bind numeric and character variables
mean_moderator_output_numbered <-mean_moderator_output_numbered[,names(mean_moderator_output_z)]
mean_moderator_output_numbered$reference <- ifelse(grepl("ref", mean_moderator_output_numbered$level) == TRUE, 'REF', 
                                                   ifelse(mean_moderator_output_numbered$level == 'Beta_weight', 'BETA', 'ALT'))

# Perform the 'Z-test method' for differences between groups described by Borenstein et al. (2009, p156):
# Diff = Mb - Ma
# Zdiff = Diff/ SEdiff
# SEdiff = sqrt(Vma + Vmb) 
#                       Where Mb and Ma are the two pooled effects, 
#                       and Vma and Vmb are the variance of the two pooled effects 

# Grouping by 'name', calculate Diff, SEdiff, and ZDiff.
# First, split the dataframe into to subgroups and Betas
mean_moderator_output_subgroups <- mean_moderator_output_numbered %>% 
                                              subset(., reference== "REF"| reference== "ALT")
mean_moderator_output_betas <- mean_moderator_output_numbered %>% 
                                                                   subset(., reference== "BETA")

# Subtract the reference estimate LOR from the alternative level's estimate LOR
setDT(mean_moderator_output_subgroups)[, Diff:=  est_LIRR - est_LIRR[match('REF', reference)], .(mod_category , name)]

mean_moderator_output_subgroups$Diff[mean_moderator_output_subgroups$reference== 'REF'] <- NA
# Create a variable for the variances                                                                                                                     
mean_moderator_output_subgroups$Vm <- mean_moderator_output_subgroups$est_SE^2 
# Calculate SEdiff
setDT(mean_moderator_output_subgroups)[, SEdiff:= sqrt(Vm + Vm[match('REF', reference)]), .(mod_category , name)]
mean_moderator_output_subgroups$SEdiff[mean_moderator_output_subgroups$reference== 'REF'] <- NA
# Calculate Zdiff                                                                                               
mean_moderator_output_subgroups$Zdiff <- mean_moderator_output_subgroups$Diff/mean_moderator_output_subgroups$SEdiff

### A a two tailed p-value can then be calculated on the ZDiff using a normal distribution (Borenstein et al., 2009, p. 156)
mean_moderator_output_subgroups$Zdiff_p <- 2 * pnorm(abs(mean_moderator_output_subgroups$Zdiff), lower.tail = FALSE)

# bind the betas and subgroups dataframes back together
mean_moderator_output_bound <- bind_rows(mean_moderator_output_subgroups, mean_moderator_output_betas) 
mean_moderator_output_bound <- mean_moderator_output_bound[order(mean_moderator_output_bound$mod_id), ]

# create a new variable that contains the Beta's p values along with Zdiff_p
mean_moderator_output_bound$pval <- ifelse(mean_moderator_output_bound$reference=='BETA', mean_moderator_output_bound$est_z_p, 
                                           ifelse(mean_moderator_output_bound$reference=='ALT', mean_moderator_output_bound$Zdiff_p, NA))

########################################
# 7.2.2: Benjamini-Hochberg procedure: Mean recidivist events 
#####################################
###Separate a category from moderator analysis
mean_mod_category_methodological <-mean_moderator_output_bound%>%filter(mod_category=='methodological')
###Create a new variable that for BH adjusted p-values 
mean_mod_category_methodological$pval_adjusted <- p.adjust(mean_mod_category_methodological$pval, 
                                                           method = "BH")
###Repeat for the remaining moderator categories 
###sample_characteristics 
mean_mod_category_sample_characteristics <-mean_moderator_output_bound%>%filter(mod_category=='sample characteristics')
mean_mod_category_sample_characteristics$pval_adjusted <-p.adjust(mean_mod_category_sample_characteristics$pval, 
                                                                  method = "BH")
###programmatic
mean_mod_category_programmatic<-mean_moderator_output_bound%>%filter(mod_category=='programmatic')
mean_mod_category_programmatic$pval_adjusted <-p.adjust(mean_mod_category_programmatic$pval, 
                                                        method = "BH")
###treatment
mean_mod_category_treatment<-mean_moderator_output_bound%>%filter(mod_category=='treatment')
mean_mod_category_treatment$pval_adjusted <-p.adjust(mean_mod_category_treatment$pval, 
                                                     method = "BH")
###bind the dataframes together 
mean_moderator_output_adjusted <-rbind(mean_mod_category_methodological,
                                       mean_mod_category_sample_characteristics,
                                       mean_mod_category_programmatic,
                                       mean_mod_category_treatment)
###Re-order by mod_id
mean_moderator_output_adjusted <- arrange(mean_moderator_output_adjusted, mod_id)
###check that the values from mean_moderator_output_bound and mean_moderator_output_adjusted are the same 
mean_moderator_output_bound$mod_id==mean_moderator_output_adjusted$mod_id
mean_moderator_output_bound$pval==mean_moderator_output_adjusted$pval
###add a column for rounded p-values 

mean_moderator_output_adjusted$pval_adjusted_rounded <-round2(mean_moderator_output_adjusted$pval_adjusted, 3)
mean_moderator_output_adjusted$pval_adjusted_rounded <-round(mean_moderator_output_adjusted$pval_adjusted, digits = 3)
########################################
# 7.2.3: Clean mean recidivist events moderator table
#####################################
# Remove irrelevant variables 
mean_moderator_export <- mean_moderator_output_adjusted %>% select(-est_z, -est_z_p, -res_DF, -Qres, -Qres_p, -Vm, -SEdiff,-Zdiff, -Zdiff_p, -pval_adjusted_rounded)

# Exponentiate relevant columns
mean_moderator_export <-mean_moderator_export %>% mutate_at(vars(est_LIRR, est_cl, est_cu, Diff), list(~exp(.)))

# Format the p-values - split data frame to apply pvalr function 
mean_moderator_export_refs <- subset(mean_moderator_export, mean_moderator_export$reference == 'REF')
mean_moderator_export_bs_alts <- subset(mean_moderator_export, !mean_moderator_export$reference == 'REF')

# mean_moderator_export_bs_alts$pval <- pvalr(mean_moderator_export_bs_alts$pval, digits = 2)
mean_moderator_export_bs_alts$pval <- ifelse(mean_moderator_export_bs_alts$pval <.01, pvalr(mean_moderator_export_bs_alts$pval, digits = 3), pvalr(mean_moderator_export_bs_alts$pval, digits = 2))
mean_moderator_export_refs$pval <- as.character(mean_moderator_export_refs$pval)
mean_moderator_export <- rbind(mean_moderator_export_bs_alts, mean_moderator_export_refs)
mean_moderator_export <- mean_moderator_export[order(mean_moderator_export$mod_id),]

# Round relevant columns. Note: any value >.99 and <1 will be rounded to .99 so that rounding does not change the direction of the effect.
mean_moderator_export <- mean_moderator_export %>% mutate_at(vars(est_LIRR, est_SE, est_cl, est_cu, Diff), 
                                                             list(~ifelse(.<1 & .>.99, floor(.*100)/100, round2(., 2))))
# Add trailing zeros where necessary
mean_moderator_export <- mean_moderator_export %>% mutate_at(vars(est_LIRR, est_SE, est_cl, est_cu, Diff), 
                                                             list(~sprintf("%.2f",. )))

mean_moderator_export$I2 <- ifelse(mean_moderator_export$I2 >.001, sprintf("%.2f", round2(mean_moderator_export$I2, 2)),"<.001")

# Add '-' for the Beta weight's diff values 
mean_moderator_export$Diff <- ifelse(mean_moderator_export$level == 'Beta_weight', ' - ', mean_moderator_export$Diff)

# Collapse confidence interval into one variable and add punctuation 
mean_moderator_export <-unite(mean_moderator_export, '95% CI', c(est_cl, est_cu), remove=TRUE)
mean_moderator_export$`95% CI` <- paste0("(", (mean_moderator_export$`95% CI`), ")")
mean_moderator_export$`95% CI` <-gsub("_", ", ", mean_moderator_export$`95% CI`)

# Remove underscore from level
mean_moderator_export$level  <-gsub("_", " ", mean_moderator_export$level)

# Add significance levels to the unadjusted pval based on the adjusted pval
mean_moderator_export$pval<- ifelse(mean_moderator_export$pval_adjusted > 0.01 & mean_moderator_export$pval_adjusted<0.05, paste0(mean_moderator_export$pval,"*"),
                                    ifelse(mean_moderator_export$pval_adjusted>0.001 & mean_moderator_export$pval_adjusted<0.01, paste0(mean_moderator_export$pval,"**"),
                                           ifelse(mean_moderator_export$pval_adjusted< 0.001, paste0(mean_moderator_export$pval,"***"), 
                                                  mean_moderator_export$pval)))

# Reorder each moderator data frame alphabetically within each moderator category
mean_moderator_export <- arrange(mean_moderator_export,mod_category, name)

# Insert 'blank' into name and Diff so that we can replace them with blank space
# mean_moderator_export$name <- ifelse(mean_moderator_export$reference == 'ALT', 'blank', mean_moderator_export$name)
mean_moderator_export$Diff <- ifelse(mean_moderator_export$Diff == 'NA', 'blank', mean_moderator_export$Diff)
mean_moderator_export$pval <- as.character(replace_na(mean_moderator_export$pval,'blank'))

# Replace NAs with blank space
mean_moderator_export <- mean_moderator_export %>% mutate_at(vars(name,Diff,pval), list(~gsub('blank', "", .)))

# Rename columns for output 
mean_moderator_export <- plyr::rename(mean_moderator_export , c("name" = "Moderator","level" = "Level", "est_LIRR" = "IRR","95% CI" = "95% CI", "level_k" = "k", "est_SE" = "SE","I2" ="I2", "pval" = "Zdiff/beta p-value"))

# Drop irrelevant columns and export 
mean_moderator_export_final<- mean_moderator_export[, c("mod_category", "Moderator", "reference","Level","k", "IRR", "95% CI", "I2", "Zdiff/beta p-value")]

# export table to CSV 
# write.csv(mean_moderator_export_final, file = "Recidivism incidents moderator table.csv")
########################################
# 7.3: Merge and export moderator tables 
#####################################
# Splice the levels from the two moderator dataframes
prop_moderator_merge <- splice_levels(mean_moderator_export_final, prop_moderator_export_final)
mean_moderator_merge <- splice_levels(prop_moderator_export_final, mean_moderator_export_final)
# reorder so they are the same 
mean_moderator_merge <- mean_moderator_merge[match(paste(prop_moderator_merge$Level,prop_moderator_merge$Moderator), paste(mean_moderator_merge$Level,mean_moderator_merge$Moderator)),]
mean_moderator_merge <- mean_moderator_merge %>% select(-"mod_category", -"Moderator", -"Level") %>% 
                                                          rename(., c("reference" = "reference.1","k" = "k.1","95% CI" = "95% CI.1","I2" = "I2.1", "Zdiff/beta p-value" = "Zdiff/beta p-value.1"))

# bind moderator dataframes 
moderator_export <- cbind(prop_moderator_merge, mean_moderator_merge)

# Clear Moderator unwanted values
moderator_export$Moderator <- ifelse(moderator_export$reference == 'ALT', "", moderator_export$Moderator) # add a dash to alt levels
moderator_export$`Zdiff/beta p-value` <- ifelse(moderator_export$reference == 'REF', '-' , moderator_export$`Zdiff/beta p-value`)
moderator_export$`Zdiff/beta p-value.1` <- ifelse(moderator_export$reference.1 == 'REF', '-' , moderator_export$`Zdiff/beta p-value.1`)

moderator_export <- moderator_export %>% select(-"reference", -"reference.1") 
# view(moderator_export)
# write.csv(moderator_export, file = "Combined moderator tables.csv")

########################################
# 8: Pooled Effects tables
#####################################
########################################
# 8.1: Proportion that recidivated: pooled effects table
#####################################
prop_pooled_effect_output_table_num <- prop_pooled_effect_output %>% select(-'Model') %>% 
                                                                                        apply(2,function(x) as.numeric(as.character(x))) %>% 
                                                                                                                                     as.data.frame(.)
prop_pooled_effect_output_table_nonum <- prop_pooled_effect_output %>% select('Model')

# Round appropriate values
prop_pooled_effect_output_table_num <- prop_pooled_effect_output_table_num %>% 
                                                                        mutate_at(vars(`Odds ratio`, ConL, ConU,CrL, CrU, Q, `I^2`), 
                                                                                  list(~sprintf("%.2f", round2(., 2))))

prop_pooled_effect_output_table_num$`Z p-value` <- round(prop_pooled_effect_output_table_num$`Z p-value`, digits = 2)

# Add asterisks to significant p-values for Z and Q 
prop_pooled_effect_output_table_num$`Z p-value` <- ifelse(prop_pooled_effect_output_table_num$`Z p-value`>= 0.01 & prop_pooled_effect_output_table_num$`Z p-value`<0.05, paste0(prop_pooled_effect_output_table_num$`Z p-value`,"*"),
                                                          ifelse(prop_pooled_effect_output_table_num$`Z p-value`>0.001 & prop_pooled_effect_output_table_num$`Z p-value`<0.01, paste0(prop_pooled_effect_output_table_num$`Z p-value`,"**"),
                                                               ifelse(prop_pooled_effect_output_table_num$`Z p-value`< 0.001, paste0(prop_pooled_effect_output_table_num$`Z p-value`,"***"), prop_pooled_effect_output_table_num$`Z p-value`)))

prop_pooled_effect_output_table_num$Q <- ifelse(prop_pooled_effect_output_table_num$`Q p-value`> 0.01 & prop_pooled_effect_output_table_num$`Q p-value`<0.05, paste0(prop_pooled_effect_output_table_num$Q,"*"),
                                                            ifelse(prop_pooled_effect_output_table_num$`Q p-value`>0.001 & prop_pooled_effect_output_table_num$`Q p-value`<0.01, paste0(prop_pooled_effect_output_table_num$Q,"**"),
                                                                   ifelse(prop_pooled_effect_output_table_num$`Q p-value`< 0.001, paste0(prop_pooled_effect_output_table_num$Q,"***"), prop_pooled_effect_output_table_num$Q)))

# Merge confidence interval and add punctuation 
prop_pooled_effect_output_table_num <- unite(prop_pooled_effect_output_table_num, 'Confidence interval', c(ConL, ConU), remove=TRUE)
prop_pooled_effect_output_table_num$`Confidence interval` <- prop_pooled_effect_output_table_num$`Confidence interval` %>% 
                                                                                                            {gsub('_', ", ", .)} %>% 
                                                                                                                   {paste0("(", (.), ")")}
# Merge credibility interval variables and replace NAs with '-'
prop_pooled_effect_output_table_num <- unite(prop_pooled_effect_output_table_num, 'Credibility interval', c(CrL, CrU), remove=TRUE)

prop_pooled_effect_output_table_num$`Credibility interval` <- ifelse(prop_pooled_effect_output_table_num$`Credibility interval`!= 'NA_NA', 
                                                                     paste0("(", (prop_pooled_effect_output_table_num$`Credibility interval`), ")"), 
                                                                         prop_pooled_effect_output_table_num$`Credibility interval`)
                                                                     
prop_pooled_effect_output_table_num$`Credibility interval` <- ifelse(prop_pooled_effect_output_table_num$`Credibility interval`== 'NA_NA',  
                                                                     gsub('NA_NA', " - ", prop_pooled_effect_output_table_num$`Credibility interval`),
                                                                                  gsub('_', ", ", prop_pooled_effect_output_table_num$`Credibility interval`))

# Bind DF, and export 
prop_pooled_effect_output_table <- cbind(prop_pooled_effect_output_table_nonum, prop_pooled_effect_output_table_num)
# remove unwanted columns 
prop_pooled_effect_output_table <- prop_pooled_effect_output_table %>%select(!c(`Z statistic`, `Q p-value`))
# write.csv(prop_pooled_effect_output_table, file = "Proportion that recidivated pooled effects table.csv")
########################################
# 8.2: Mean recidivist events: pooled effects table
#####################################
mean_pooled_effect_output <- mean_pooled_effect_output %>% as.data.frame(.)
mean_pooled_effect_output_num <- mean_pooled_effect_output %>%
                                                          select('Incident Rate Ratio', 'ConL', 'ConU', 'Z statistic', 'Z p-value', 'CrL', 'CrU', 'Wald-type test','Wald-type test p-value',
                                                                'Likelihood ratio test','Likelihood ratio test p-value', 'Dfs','I^2', 'Deviance','Deviance p-value') %>% 
                                                                                                                            apply(2,function(x) as.numeric(as.character(x))) %>% as.data.frame(.)

mean_pooled_effect_output_nonum <- mean_pooled_effect_output %>%
                                                              select('Model')

# Round values and add trailing 0s
mean_pooled_effect_output_num <- mean_pooled_effect_output_num %>% 
                                                              mutate_at(vars(`Incident Rate Ratio`, ConL, ConU, `Z statistic`,CrL, CrU, `Wald-type test`, `Likelihood ratio test`,
                                                                             `I^2`, Deviance), 
                                                                                              list(~sprintf("%.2f", round2(., 2))))

mean_pooled_effect_output_num$`Wald-type test` <- ifelse(mean_pooled_effect_output_num$`Wald-type test p-value`> 0.01 & mean_pooled_effect_output_num$`Wald-type test p-value`<0.05, paste0(mean_pooled_effect_output_num$`Wald-type test`,"*"),
                                                      ifelse(mean_pooled_effect_output_num$`Wald-type test p-value`>0.001 & mean_pooled_effect_output_num$`Wald-type test p-value`<0.01, paste0(mean_pooled_effect_output_num$`Wald-type test`,"**"),
                                                             ifelse(mean_pooled_effect_output_num$`Wald-type test p-value`< 0.001, paste0(mean_pooled_effect_output_num$`Wald-type test`,"***"), mean_pooled_effect_output_num$`Wald-type test`)))

mean_pooled_effect_output_num$`Likelihood ratio test` <- ifelse(mean_pooled_effect_output_num$`Likelihood ratio test p-value`> 0.01 & mean_pooled_effect_output_num$`Likelihood ratio test p-value`<0.05, paste0(mean_pooled_effect_output_num$`Likelihood ratio test`,"*"),
                                                         ifelse(mean_pooled_effect_output_num$`Likelihood ratio test p-value`>0.001 & mean_pooled_effect_output_num$`Likelihood ratio test p-value`<0.01, paste0(mean_pooled_effect_output_num$`Likelihood ratio test`,"**"),
                                                                ifelse(mean_pooled_effect_output_num$`Likelihood ratio test p-value`< 0.001, paste0(mean_pooled_effect_output_num$`Likelihood ratio test`,"***"), mean_pooled_effect_output_num$`Likelihood ratio test`)))


mean_pooled_effect_output_num <- mean_pooled_effect_output_num %>% 
                                                              mutate_at(vars(`Z p-value`, `Wald-type test p-value`, `Likelihood ratio test p-value`, `Deviance p-value`),
                                                                        list(~ifelse(.<0.001, paste0(pvalr(.),"***"),  sprintf("%.3f",round2(., 3)))))

# Join the confidence and credibility interval variables and add punctuation
mean_pooled_effect_output_num <- unite(mean_pooled_effect_output_num, 'Confidence interval', c(ConL, ConU), remove=TRUE)
mean_pooled_effect_output_num <- unite(mean_pooled_effect_output_num, 'Credibility interval', c(CrL, CrU), remove=TRUE)
mean_pooled_effect_output_num <- mean_pooled_effect_output_num %>% mutate_at(vars(`Confidence interval`, `Credibility interval`), 
                                                                    list(~paste0("(", (gsub('_', ", ", .)), ")"))) 
# Bind DF, and export 
mean_pooled_effect_output_table <- cbind(mean_pooled_effect_output_nonum, mean_pooled_effect_output_num)
# Remove unwanted variables
mean_pooled_effect_output_table <- mean_pooled_effect_output_table %>%select(!c(`Z statistic`, `Wald-type test p-value`, `Likelihood ratio test p-value`))
# write.csv(mean_pooled_effect_output_table, file = "Recidivist events pooled effects table.csv")
                                                                           