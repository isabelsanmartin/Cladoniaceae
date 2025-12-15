### CoMET with rjMCMC (Reversible Jump MCMC algorithm) to estimate simultaneously changes in diversification rates and mass extinction events.


#library(TESS) loads packages ape and coda and desolve as dependencies
library(TESS)

treefile <- "Cladonia"

analysis_name <- sprintf("CoMET_%s",treefile)

#Conditioning on survival of all lineages in empirical phylogeny
CDT <- "survival"


#This assigns the prior for changes in speciation and extinction (or net diversification and turnover) as Poisson process priors with 0.5 probability assigned to 0 MEE or 0 rate shifts events.
EXPECTED_NUM_EVENTS <- 2


MCMC_ITERATIONS <- 1000000


#Read prunned tree containing only 1 tip per genus in phylogeny (remove specific sampling)

tree <- read.tree(file=sprintf("~/Documents/Raquel_Pino_Cladonia/TESS/Cladonia_RB_nowides.tre",treefile) )

#Assign sampling value at present
# Total is number of species within genus Cladonia. tree$Nnode+1 = number of tips in the tree 


# rho <- (tree$Nnode+1)/total

rho <- 0.47

#Set prior distributions for rate parameters. Shifts and MEEs are modeled throgh a CPP process as in TreePar.

priorForms <- c("lognormal","normal","gamma")



########## RUN ANALYSIS ##############################################################


# Run two different analysis, one with MEE sampling events (integrating our rate shifts) and one without MEEs, estimating posterior of rate shifts.
 
ALLOW_MASS_EXTINCTION <- !TRUE

if ( ALLOW_MASS_EXTINCTION == TRUE ) {
   analysis_name <- sprintf("CoMET_%s_ME",treefile)
} else {
   analysis_name <- sprintf("CoMET_%s",treefile)
}


#To run an analysis with no MEEs, use the code [1] below. This will produce an output called "CoMET_genera" containing the files with parameter values. It will also produce at the end a PDF figure named "CoMET_genera.pdf" containing vignettes for 4 figures. This function will create a directory with the name of "analysis_name" given above in the current working directory getwd()

[1]
tess.analysis(tree=tree, numExpectedRateChanges=EXPECTED_NUM_EVENTS, numExpectedMassExtinctions=EXPECTED_NUM_EVENTS, initialSpeciationRate=2.0, initialExtinctionRate=1.0, empiricalHyperPriorInflation = 10.0, empiricalHyperPriorForm = priorForms, samplingProbability=rho, estimateMassExtinctionTimes = ALLOW_MASS_EXTINCTION, estimateNumberMassExtinctions = ALLOW_MASS_EXTINCTION, MAX_ITERATIONS = MCMC_ITERATIONS, THINNING = 100,  MAX_TIME = Inf, MIN_ESS = 1000, CONDITION=CDT, dir = analysis_name)

########### PROCESS ANALYSIS OUTPUT ########################################################
out <- tess.process.output(analysis_name, tree, numExpectedRateChanges=EXPECTED_NUM_EVENTS)


################### PLOT RESULTS IN VIGNETTES #######################################################
##Results will be plotted as vignettes within a PDF: nrows = 2, ncolumns= FIG_TYPES/2
##Figures will be plotted as vignettes: number of figures depends on Fig_Types. Rate shift times and mass extinction times are plotted together with Bayes Factors (2lnBF) significance levels at 2,6,10 on the right y axis (this can be assigned a name with argument "yaxt")

NUM_FIGS <- 6
FIG_TYPES <- c("speciation rates", "extinction rates","net-diversification rates","relative-extinction rates", "speciation shift times","extinction shift times")

pdf(sprintf("%s.pdf",analysis_name))
layout.mat <- matrix(1:NUM_FIGS,nrow=2,ncol=NUM_FIGS / 2)
layout(layout.mat)
tess.plot.output(out,fig.types=FIG_TYPES,las=1)
dev.off()

##########  OPTIONS ###########

#To run an analysis with MEEs, use the code [2] below. This will produce an output folder called "CoMET_ME_genera" containing the files with parameter posterior MCMC values. It will also produce at the end a PDF figure named "CoMET_ME_genra.pdf" containing vignettes for six figures, including MEE times.

[2]

ALLOW_MASS_EXTINCTION <- TRUE

tess.analysis(tree=tree, numExpectedRateChanges=EXPECTED_NUM_EVENTS, numExpectedMassExtinctions=EXPECTED_NUM_EVENTS, initialSpeciationRate=2.0, initialExtinctionRate=1.0, empiricalHyperPriorInflation = 10.0, empiricalHyperPriorForm = priorForms, samplingProbability=rho, estimateMassExtinctionTimes = ALLOW_MASS_EXTINCTION, estimateNumberMassExtinctions = ALLOW_MASS_EXTINCTION, MAX_ITERATIONS = MCMC_ITERATIONS, THINNING = 100,  MAX_TIME = Inf, MIN_ESS = 1000, CONDITION=CDT, dir = analysis_name)

outMEE <- tess.process.output(analysis_name, tree, numExpectedRateChanges=EXPECTED_NUM_EVENTS)

NUM_FIGS <- 6
FIG_TYPES <- c("net-diversification rates","relative-extinction rates", "speciation shift times","extinction shift times","mass extinction Bayes factors","mass extinction times")

pdf(sprintf("%s.pdf",analysis_name))
layout.mat <- matrix(1:NUM_FIGS,nrow=2,ncol=NUM_FIGS / 2)
layout(layout.mat)
tess.plot.output(outMEE,fig.types=FIG_TYPES,las=1)
dev.off()

# Now, we run the anaysis for the family Cladoniaceae using sampling strategy "diversified"

######## CHANGE SAMPLING STRATEGY FROM UNIFORM TO DIVERSIFIED ##########################
# INCOMPLETE TAXON SAMPLING strategy. Default in tess.analysis function is "samplingStrategy=uniform" (species are sampled uniformly at random), which means that each species has the same probability (rho) of being sampled in the phylogeny.
# Fo the Cladonieaceae tree, at family level, it makes more sense to apply a "diversified strategy" assuming that we used overdispersed sampling, maximizing representation of genera, one species per genus.

# To apply alternative: "samplingStrategy=diversified" (species are sampled to maximize the diversity sampled in the phylogeny: i.e., only the oldest 25% of divergence events are included in the reconstructed phylogeny with sampling probability rho, and all later divergence events are excluded), we can use the function "tess.analysis.diversified.R" created by modifying the argument "samplingStrategy=diversified" in the "tess.likelihood" function part of the code. First, source it:

source("tess.analysis.diversified.R")

# Change file name

treefile <- "Cladoniaceae"

analysis_name <- sprintf("CoMET-Cladoniaceae_%s",treefile)

#Conditioning on survival of all lineages in empirical phylogeny
CDT <- "survival"


#This assigns the prior for changes in speciation and extinction (or net diversification and turnover) as Poisson process priors with 0.5 probability assigned to 0 MEE or 0 rate shifts events.
EXPECTED_NUM_EVENTS <- 2


MCMC_ITERATIONS <- 1000000


#Read prunned tree containing only 1 tip per genus in phylogeny (remove specific sampling)

tree <- read.tree(file=sprintf("~/Documents/Raquel_Pino_Cladonia/TESS/Cladoniaceae.tre",treefile) )

#Assign sampling value at present
# Total is number of species within Cladoniaceae (500 sp). tree$Nnode+1 = number of tips in the phylogeny (62)

total <- 500 


rho <- (tree$Nnode+1)/total

# rho <- 0.126

#Set prior distributions for rate parameters. Shifts and MEEs are modeled throgh a CPP process as in TreePar.

priorForms <- c("lognormal","normal","gamma")



ALLOW_MASS_EXTINCTION <- TRUE

tess.analysis(tree=tree, numExpectedRateChanges=EXPECTED_NUM_EVENTS, numExpectedMassExtinctions=EXPECTED_NUM_EVENTS, initialSpeciationRate=2.0, initialExtinctionRate=1.0, empiricalHyperPriorInflation = 10.0, empiricalHyperPriorForm = priorForms, samplingProbability=rho, estimateMassExtinctionTimes = ALLOW_MASS_EXTINCTION, estimateNumberMassExtinctions = ALLOW_MASS_EXTINCTION, MAX_ITERATIONS = MCMC_ITERATIONS, THINNING = 100,  MAX_TIME = Inf, MIN_ESS = 1000, CONDITION=CDT, dir = analysis_name)

outMEE_fam <- tess.process.output(analysis_name, tree, numExpectedRateChanges=EXPECTED_NUM_EVENTS)

NUM_FIGS <- 6
FIG_TYPES <- c("net-diversification rates","relative-extinction rates", "speciation shift times","extinction shift times","mass extinction Bayes factors","mass extinction times")

pdf(sprintf("%s.pdf",analysis_name))
layout.mat <- matrix(1:NUM_FIGS,nrow=2,ncol=NUM_FIGS / 2)
layout(layout.mat)
tess.plot.output(outMEE_fam,fig.types=FIG_TYPES,las=1)
dev.off()
