adult.death<-read.csv(file="double-checked_mw3.csv", header=T, sep=",")
adult.death<-adult.death[1:187,1:43]
names(adult.death)
dim(adult.death)
adult.death[,1]
adult.death.included<-adult.death[adult.death$Arm=="",]
dim(adult.death.included)

adult.death.included1<-rbind(adult.death.included,
                            adult.death[95,],#Folomeev
                            adult.death[104,],#Sthoeger
                            adult.death[127:128,],#Uramoto b,c
                            adult.death[146,],#Kaufman L,(Hashimoto deliminated,wrong death%)
                            adult.death[163,],#reveille JD >=20
                            adult.death[168:169,],#Urman jd
                            adult.death[176,],#Wu G >=20
                            adult.death[185,])#Chang
                          
                            
dim(adult.death.included1) #dim(131,43) 
#names(adult.death.included1[,37:40]) #"sle.relat." "infect."    "CVD."       "malign." 

#eliminate rows with all NA
adult.death.included2<-adult.death.included1[rowSums(is.na(adult.death.included1[,37:40]))!=4,]
adult.death.included2[,37:40]#92 43

#reset na=0 to perform calculation
adult.death.included2[,37:40][is.na(adult.death.included2[,37:40])]<-0

#create other cause of death pct
adult.death.included2$other<-100-rowSums(adult.death.included2[,37:40])#92 44

#eliminate Hashimoto and Hersh,% cause of death larger than 1
adult.death.included2<-adult.death.included2[!adult.death.included2$Author %in% c("Hashimoto","Hersh"),]

#create stratify var from start of enrollment
adult.death.included2$strat<-0
adult.death.included2$strat[adult.death.included2$start.enrollment<1980]<-1
adult.death.included2$strat[adult.death.included2$start.enrollment>=1980
                            & adult.death.included2$start.enrollment<=1989]<-2
adult.death.included2$strat[adult.death.included2$start.enrollment>=1990
                            & adult.death.included2$start.enrollment<=1999]<-3
adult.death.included2$strat[adult.death.included2$start.enrollment>=2000]<-4


#create weight var  
adult.death.included2$wt<-adult.death.included2$number*adult.death.included2$deaths...
adult.death.included2[is.na(adult.death.included2$wt),]#Feng,Reveille has no data in death%

aggregate(adult.death.included2[c(37:40,44)],by=list(adult.death.included2$strat),
          function(x) round((sum(x*adult.death.included2$wt,na.rm=T))/(sum(adult.death.included2$wt,na.rm=T)),digits=2))

#Group.1 sle.relat. infect.  CVD. malign. other
#1       0      45.13   16.43 10.80    0.00 27.64
#2       1      34.38   31.84 15.32    5.39 13.06
#3       2      29.94   26.77 18.31    4.33 20.65
#4       3      24.44   41.56 14.00    3.91 16.09
#5       4      31.63   36.36 12.39    5.22 14.40
#==================================================================================================================================




#study info
adDeath<-read.csv(file="StudyInfo.AD.csv", header=T, sep=",") 
dim(adDeath)#188  52
names(adDeath)

adDeath.included<-adDeath[adDeath$Arm=="",]
dim(adDeath.included)#122  52
adDeath.included[,3]
adDeath.included1<-rbind(adDeath.included,
                             adDeath[adDeath$Author=="Folomeev",],#Folomeev
                             adDeath[adDeath$Author=="Sthoeger",],#Sthoeger
                             adDeath[adDeath$Author=="Uramoto",],#Uramoto b,c
                             adDeath[adDeath$Author=="Kaufman",],#Kaufman L,(Hashimoto deliminated,wrong death%)
                             adDeath[adDeath$Author=="Reveille" & adDeath$Arm=="20 and older",],#reveille JD >=20
                             adDeath[adDeath$Author=="Urman",],#Urman jd
                             adDeath[adDeath$Author=="Wu" & adDeath$Arm=="20 and older",],#Wu G >=20
                             adDeath[adDeath$Author=="Chang",])#Chang

dim(adDeath.included1)#132  52,Urman jd in original data
dim(adult.death.included1)#131  43

#compare the equivalence
x1<-gsub('[[:space:]]',"",adult.death.included1$Author)
cbind(adDeath.included1$Author[order(adDeath.included1$Author)],x1[order(x1)])#adult.Death has no Hashimoto
      

#names(adDeath.included1[,37:40]) #"sle.relat." "infect."    "CVD."       "malign." 

#eliminate rows with all NA
adDeath.included2<-adDeath.included1[rowSums(is.na(adDeath.included1[,37:40]))!=4,]
adDeath.included2[,37:40]#92 52

#reset na=0 to perform calculation
adDeath.included2[,37:40][is.na(adDeath.included2[,37:40])]<-0

#create other cause of death pct
adDeath.included2$other<-100-rowSums(adDeath.included2[,37:40])#92 53

#eliminate Hashimoto and Hersh,% cause of death larger than 1
adDeath.included2<-adDeath.included2[!adDeath.included2$Author %in% c("Hashimoto","Hersh"),]#91 53,same with adult death

#create stratify var from start of enrollment
adDeath.included2$strat<-0
adDeath.included2$strat[adDeath.included2$start_of_study<1980]<-1
adDeath.included2$strat[adDeath.included2$start_of_study>=1980
                            & adDeath.included2$start_of_study<=1989]<-2
adDeath.included2$strat[adDeath.included2$start_of_study>=1990
                            & adDeath.included2$start_of_study<=1999]<-3
adDeath.included2$strat[adDeath.included2$start_of_study>=2000]<-4


#create weight var  
adDeath.included2$wt<-adDeath.included2$number*adDeath.included2$deaths
adDeath.included2[is.na(adDeath.included2$wt),]#Feng,Reveille has no data in death%

aggregate(adDeath.included2[,c('sle.relat', 'infect',  'CVD', 'malign','other')],by=list(adDeath.included2$strat),
          function(x) round((sum(x*adDeath.included2$wt,na.rm=T))/(sum(adDeath.included2$wt,na.rm=T)),digits=2))

#Group.1 sle.relat infect   CVD malign other=====start_of_study result
#1       1     25.35  34.25 17.77   7.21 15.42
#2       2     33.43  25.54 19.24   5.25 16.54
#3       3     24.54  32.32 14.40   4.56 24.17
#4       4     31.74  27.18 11.56   5.69 23.83





#Group.1 sle.relat. infect.  CVD. malign. other======start.enrollment result
#1       0      45.13   16.43 10.80    0.00 27.64
#2       1      34.38   31.84 15.32    5.39 13.06
#3       2      29.94   26.77 18.31    4.33 20.65
#4       3      24.44   41.56 14.00    3.91 16.09
#5       4      31.63   36.36 12.39    5.22 14.40
write.csv(adDeath.included2,file="CauseDeathDat.csv")
table(d2$strat)

#==================================compare resutls======no use since results based on start_of_study=====================================
table(adult.death.included2$strat);adult.death.included2[adult.death.included2$strat==0,3]
#"Rua-Figueroa" "Halberg"      "estes d"      "Wu G,"                                                 
table(adDeath.included2$strat);adDeath.included2[adDeath.included2$strat==0,3]
#"Halberg"      "Rua-Figueroa" "Wu" 

#check estes start.enrollment
adDeath.included2[adDeath.included2$Author=='Estes',]#1961
adult.death.included2[adult.death.included2$Author=='estes d',]#NA, coded 0
#=============================================================================================




#developed vs developing
names(adDeath.included2)
d1<-adDeath.included2[adDeath.included2$Developed=='Developed',]
aggregate(d1[,c('sle.relat', 'infect',  'CVD', 'malign','other')],by=list(d1$strat),
          function(x) round((sum(x*d1$wt,na.rm=T))/(sum(d1$wt,na.rm=T)),digits=2))

d2<-adDeath.included2[adDeath.included2$Developed=='Developing',]
aggregate(d2[,c('sle.relat', 'infect',  'CVD', 'malign','other')],by=list(d2$strat),
          function(x) round((sum(x*d2$wt,na.rm=T))/(sum(d2$wt,na.rm=T)),digits=2))


#inception cohorts
I<-adDeath.included2[adDeath.included2$inception==1,]
aggregate(I[,c('sle.relat', 'infect',  'CVD', 'malign','other')],by=list(I$strat),
          function(x) round((sum(x*I$wt,na.rm=T))/(sum(I$wt,na.rm=T)),digits=2))


I1<-I[I$Developed=='Developed',]
aggregate(I1[,c('sle.relat', 'infect',  'CVD', 'malign','other')],by=list(I1$strat),
          function(x) round((sum(x*I1$wt,na.rm=T))/(sum(I1$wt,na.rm=T)),digits=2))
#Group.1 sle.relat infect   CVD malign other
#1       1     18.92  21.92 32.51  12.71 13.94
#2       2     32.29  24.05 26.93   8.67  8.06
#3       3      7.70  42.32 32.18   4.93 12.88

I2<-I[I$Developed=='Developing',]
aggregate(I2[,c('sle.relat', 'infect',  'CVD', 'malign','other')],by=list(I2$strat),
          function(x) round((sum(x*I2$wt,na.rm=T))/(sum(I2$wt,na.rm=T)),digits=2))
#Group.1    sle.relat infect   CVD malign other
#1       1     37.50  31.00 25.00   0.00  6.50
#2       2     38.20  26.50 16.20   0.00 19.10
#3       3     16.91  65.36  6.39   2.74  8.61
#4       4      0.00  50.00  0.00   0.00 50.00





#===================trend by decade=========================
with(adDeath.included2,lm(sle.relat~strat))
lm(adDeath.included2$sle.relat~adDeath.included2$strat)
require(MASS)
with(adDeath.included2,cor.test(sle.relat,start_of_study))#r=-.28, p = 0.007102
with(adDeath.included2,cor.test(inception,start_of_study))#r=.18, p= 0.07798
with(adDeath.included2,cor.test(CVD,start_of_study))#r=.007, p = 0.9486
with(adDeath.included2,cor.test(malign,start_of_study))#r=-.005, p = 0.9622
with(adDeath.included2,cor.test(other,start_of_study))#r=.09,p = 0.3932

with(adDeath.included2,plot(start_of_study,nephritis))
with(adDeath.included2,plot(start_of_study,CNS))

