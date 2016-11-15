# Functions for summary survival data -------------------------------------
setwd("P:/FranWork/LupusDeath")
fn.env <- new.env()
source('P:/FranWork/LupusDeath/lib/R/truncFunctions.R', local=fn.env)
load('lib/reload.R')
reload()

#try
summDat<-read.csv(file="try.csv", header=TRUE, sep=",")
weibull_estimation(summDat[10:11,])

rweibull(107, -.000000000000003, scale =0)

