

# Data ####

  source("scripts/data.R")

# Basic data reading in and cleaning.
# The baseline data are in a wide format.
# The outcome data are in a long format, and include calcuated different scores.
# The main dataset merges these, retaining the long format. However, as
# ANCOVA is the primary model of analysis, later scripts need to flip the data
# to wide.

# Beyond this initial data, several other datasets were added over time

# The rest of the scripts were set up to produce the outputs that went into the
# presenatations and papers.

# Table 1 ####

  source("scripts/table.1.R")

# This relies on data.R to produce the descriptive table that was table 1
# in the paper.


# Figure 1 ####

  source("scripts/figure.1.R")

# This relies on data.R to produce a plot of the GLVEF result that was fig 1
# in the paper.

# Results table ####

  source("scripts/results.table.R")

# This is script to run all the ANCOVAs as well as the
# time-specific outcome summaries into a single table, which is table 2 in
# the paper


# Events ####

  source("scripts/events.R")

# Regression model of event risk by arms and simple binomial tests.



# EXTRA ####

# Unadjusted Results table ####

source("scripts/results.unadjusted.R")

# This is an insane, hacky script to run all the ANCOVAs and put the results in
# a single table. These models are adjusted for stratifiers, but not the
# baseline outcome values.

# TIMI adjusted ####

  source("scripts/results.table.timi.R")

# ANCOVA results with models adjusted for TIMI


# Time to ischemia adjusted ####

  source("scripts/results.table.ichtime.R")

# ANCOVA results with models adjusted for time to ischemia


# Multiplicity ####

# An adjustment for multiple outcomes was assessed using BH. The script is
# stand-alone, with the p-values from the paper and associcated outcomes
# typed in.

source("scripts/bh.R")

# Differences by missing late CE ####

source("scripts/missing_latece.R")

# This groups by whether they are missing late CE or not, describes variables
# by group, and tests for differences

# Differences by complete case ####

source("scripts/missing.R")

# This groups by whether they are missing any key data, describes variables
# by group, and tests for differences. Missing flag calculated in results.table

# Log example ####

  source("scripts/extra/log_example.R")

# This is just looking at what happens when we use the log transformed outcomes