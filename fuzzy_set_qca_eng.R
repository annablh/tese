# Load packages
library(readr)
library(dplyr)
library(QCA)
library(SetMethods)


# Load the dataset
df = read.csv('preprocessed_dataset.csv')

# Write the dataset to an Excel file
write_excel_csv(df, 'preprocessed_dataset.xls')

# Select columns for QCA
df = df[,c('country','cooperated','prior_asylum_application','agree_help_refugees', 'days_left_party_left')]

# Rename countries as row labels
rownames(df) = df$country

#===============================
# CALIBRATION
#===============================
# Calibration is essentially a process of forming and defining concepts that interact with your theory.
# Conceptual and theoretical criteria are used for the crossover point, avoiding purely empirical criteria such as descriptive statistics.

# Germany
df$agree_help_refugees[df$country == 'Germany']
df$cooperated[df$country == 'Germany']
df$prior_asylum_application[df$country == 'Germany']

#========== Target Cooperation ============

# By cutting the percentage from Germany
df = mutate(df, cooperated = ifelse(cooperated > 37, 1, 0))

df

#========== Left Government Party ============

# Calibrate
df$days_left_party_left_cal = round(calibrate(df$days_left_party_left, 
                                               type = "fuzzy", 
                                               thresholds = "e=0, c=365, i=547.5", 
                                               logistic=TRUE), digits=2)

# Visualize the calibration graph
plot(df$days_left_party_left, df$days_left_party_left_cal, pch=18, col="black", main='',
     xlab='Raw Value',
     ylab='Fuzzy Value') +
  abline(h=0.5, col="black") +
  abline(v=547.5, col="black") +
  text(df$days_left_party_left, df$days_left_party_left_cal, df$country, cex=0.65, pos=3, col="black") 

#========== Number of Refugees ============

# Calibrate
df$prior_asylum_application_cal = round(calibrate(df$prior_asylum_application, 
                                                  type = "fuzzy", 
                                                  thresholds = "e=1, c=2, i=4.1", 
                                                  logistic=TRUE), digits=2)

# Visualize the calibration graph
plot(df$prior_asylum_application, df$prior_asylum_application_cal, pch=18, col="black", main='',
     xlab='Raw Value',
     ylab='Fuzzy Value') +
  abline(h=0.5, col="black") +
  abline(v=4.1, col="black") +
  text(df$prior_asylum_application, df$prior_asylum_application_cal, df$country, cex=0.65, pos=3, col="black") 

#========== Agree to Help Refugees ============

df$agree_help_refugees_cal = round(calibrate(df$agree_help_refugees, 
                                              type = "fuzzy", 
                                              thresholds = "e=0.3, c=0.5, i=0.85", 
                                              logistic=TRUE), digits=2)

plot(df$agree_help_refugees, df$agree_help_refugees_cal, pch=18, col="black", main='',
     xlab='Raw Value',
     ylab='Fuzzy Value') +
  abline(h=0.5, col="black") +
  abline(v=0.85, col="black") +
  text(df$agree_help_refugees, df$agree_help_refugees_cal, df$country, cex=0.65, pos=3, col="black") 

#================== Select calibrated subset
subset = df[,c('prior_asylum_application_cal','agree_help_refugees_cal', 'days_left_party_left_cal')]

#================================
# NECESSARY CONDITIONS
#================================

# Necessary conditions individually
pof(subset, 'cooperated', df, relation = "nec")

# Necessary conditions in combination
super = superSubset(df, outcome = "cooperated",
                    conditions = "prior_asylum_application_cal, agree_help_refugees_cal, days_left_party_left_cal",
                    incl.cut = 0.7, 
                    ro = 0.4, 
                    cov.cut = 0.4)

super

#==============================
# SUFFICIENT CONDITIONS
#=============================

# Sufficient conditions individually
pof(subset, 'cooperated', df, relation = "suf")

#==========================
# TRUTH TABLE
#==========================

##====================== Parsimonious Solution  ======================##

ttSURV <- truthTable(data=df, 
                     outcome = "cooperated", 
                     conditions = "prior_asylum_application_cal, agree_help_refugees_cal, days_left_party_left_cal",
                     incl.cut=0.7, 
                     sort.by="incl, n", 
                     complete=FALSE, 
                     show.cases=TRUE) 
ttSURV 

# Minimization of the truth table
minimize(ttSURV, details=TRUE, show.cases=TRUE, row.dom=TRUE, all.sol=FALSE, use.tilde=FALSE)


##====================== Complete Solution  ======================##

ttSURV <- truthTable(data=df, 
                     outcome = "cooperated", 
                     conditions = "prior_asylum_application_cal, agree_help_refugees_cal",
                     sort.by="incl, n", 
                     complete=FALSE, 
                     show.cases=TRUE) 
ttSURV 

# minimization of truth table
minimize(ttSURV, details=TRUE, show.cases=TRUE, row.dom=TRUE, all.sol=FALSE, use.tilde=FALSE)
