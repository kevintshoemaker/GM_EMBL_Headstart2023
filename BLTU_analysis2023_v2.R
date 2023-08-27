# Blandings Turtle Survival and abundance analysis
#  Great Meadows, Concord MA


# Questions -------------

# Should we remove carlisle and EST individuals?
# How to tease apart telemetered and non-telemetered individuals?
# Some dead individuals recaptured? 
# "official" ids? What to do with re-used codes?


# TODO ------------------
# Make a fifth age class and model detection prob differently for males 
       # and females in that final age class due to nesting
       # model survival equivalently for these two age classes.. 
# Note that the age-class 1 detection probability is not used. 
   # maybe make separate age arrays for survival and detection probability?

# Get ready ----------------

rm(list=ls())

# load packages ------------------------

library(jagsUI)
library(Hmisc)
library(lubridate)
library(readxl)

# load data ---------------------

df1_allcaps <- read_excel("GM EMBL Manuscript Cap_Telem and Analyses.xlsx",1,col_types = "text")

df3_adult_telem <- read_excel("Kaplan survival calculations (BLTU, EMBL) Radiotelemetry2.xlsx",1,col_types = "text")
df4_HS_telem <- read_excel("Kaplan survival calculations (BLTU, EMBL) Radiotelemetry2.xlsx",2,col_types = "text")

names(df1_allcaps)   # 61 columns. Keep only important ones
table(df1_allcaps$Species)  # all blandings turtle. ignore

keep1 <- c("Notch","JoinID","Date","Year","Sex","Age Class: Adult, Juvenile",
           "Headstart (Y/N)","IF HEADSTARTED, Hatch Year",
           "Is this an observation of a headstart in captivity",
           "Turtle Status: L (live), D (dead), U (unknown)",
           "Previously Unmarked Turtle (Y/N)",
           "Initial Release of Headstart (Y/N)",
           "R=radiotracked and caught, IR=Initial Release of Headstart, T=trapped, H=hand capture, N=nesting, E=telemetry estimate, HC= headstart in captivity",
           "General Location (wetland or property name)",
           "Latitude Decimal Degrees",
           "Longitude Decimal Degrees",
           "# Annuli",
           "CL (mm)",
           "PL (mm)",
           "No radio when found (NR), Found with disfunctional radio (DR), found with functioning radio (FR), functioning radio removed (RE), Radio found but no turtle (JR) Antenna Missing (AM)",
           "New radio attached (Y/N)",
           "Radio Type (if new) and  expected life",
           "General Notes (Shell Wear, Health, Behaviour details etc.)"
           )
keep1
names(df1_allcaps)
keep1%in%names(df1_allcaps)
df1_allcaps <- df1_allcaps[,keep1]
names(df1_allcaps) <- c("Notch","ID","Date","Year","Sex",
                        "Age","isHS","HS_hatchyr","HS_prerel","Status",
                        "NewTurtle","HS_initrel","CapType","Caploc","Lat",
                        "Long","Annuli","CL","PL","RT_type","RT_new","RT_new_type",
                        "Notes")
df1_lookup <- cbind(names(df1_allcaps),keep1)

df1_allcaps$row <- 1:nrow(df1_allcaps)

# names(df2_all_hsrel)
# table(df2_all_hsrel$Species)
# keep2 <- c("Year","Notch ID#","JoinID","Sex",
#            "Nest/ Mom ID","Hatch Date","Weight at Hatch","Release Weight","Did the turtle die during HS?",
#            "Was this turtle released prematurely?")
# 
# df2_all_hsrel <- df2_all_hsrel[,keep2]
# names(df2_all_hsrel) <- c("Year","Notch","ID","Sex","Nest_MomID",
#                           "HatchDate","HatchWt","ReleaseWt","NotRel","PremRel")
# 
# df2_lookup <- cbind(names(df2_all_hsrel),keep2)
# rm(keep2)
rm(keep1)

# Initial data processing ----------------------

df1_allcaps$Date <- as.Date(as.numeric(df1_allcaps$Date), origin = "1899-12-30")

df1_allcaps$Year <- year(df1_allcaps$Date)

df1_allcaps$CapType <- toupper(df1_allcaps$CapType)

df3_adult_telem$FirstDate <-  as.Date(as.numeric(df3_adult_telem$`First Date Tracked`), origin = "1899-12-30")
df3_adult_telem$LastDate <- as.Date(as.numeric(df3_adult_telem$`Last Date Tracked`), origin = "1899-12-30")

ndx <- grepl("/",df4_HS_telem$`Release Date`)
df4_HS_telem$FirstDate <-  as.Date(as.numeric(df4_HS_telem$`Release Date`), origin = "1899-12-30")
df4_HS_telem$FirstDate[ndx] <- mdy(df4_HS_telem$`Release Date`[ndx])

ndx <- grepl("/",df4_HS_telem$`Last Date Radiotracked (also: date found dead or with missing radio)`)
df4_HS_telem$LastDate <- as.Date(as.numeric(df4_HS_telem$`Last Date Radiotracked (also: date found dead or with missing radio)`), origin = "1899-12-30")
df4_HS_telem$LastDate[ndx] <- mdy(df4_HS_telem$`Last Date Radiotracked (also: date found dead or with missing radio)`[ndx])

# df2_all_hsrel$HatchDate <- as.Date(as.numeric(df2_all_hsrel$HatchDate), origin = "1899-12-30")
# df2_all_hsrel$HatchYear <- year(df2_all_hsrel$HatchDate)
# 
# df2_all_hsrel$HatchWt <- as.numeric(df2_all_hsrel$HatchWt )
# df2_all_hsrel$ReleaseWt <- as.numeric(df2_all_hsrel$ReleaseWt)
# df2_all_hsrel$Year <- as.numeric(df2_all_hsrel$Year)

# Summaries and data checks  ----------------------

allIDs <- sort(unique(c(df1_allcaps$ID)))   # df2_all_hsrel$ID
allIDs <- allIDs[!grepl("Unmarked",allIDs)]
allIDs <- allIDs[!grepl("\\?",allIDs)]
allIDs <- allIDs[!grepl("UNK",allIDs)]
allIDs <- allIDs[!grepl("Carlisle",allIDs)]    # remove Carlisle individuals
allIDs <- allIDs[!grepl("EST",allIDs)]    # remove "EST" individuals
ninds <- length(allIDs)    #  // 744 individuals after removing HS dataset

df1_allcaps <- subset(df1_allcaps,ID%in%allIDs)

# remove any individuals released on the same year they were born
allIDs_hs <- allIDs[grepl("HS",allIDs)]
torm=c()
i=1
for(i in 1:length(allIDs_hs)){
  thisid <- allIDs_hs[i]
  temp1 <- subset(df1_allcaps,ID==thisid)
  birthyear <- as.numeric(gsub("HS","",strsplit(thisid,"\\|")[[1]][2]))
  relyear <- birthyear+1 # year released.
  firstyear <- min(temp1$Year,na.rm=T)   
  if(birthyear==firstyear) torm = c(torm,temp1$row[temp1$Year==birthyear])
}

df1_allcaps <- subset(df1_allcaps,row%in%setdiff(df1_allcaps$row,torm))

allIDs <- sort(unique(c(df1_allcaps$ID)))   # df2_all_hsrel$ID
ninds <- length(allIDs)    # 725 individuals remaining

df1_allcaps <- subset(df1_allcaps,ID%in%allIDs)

allyears <- sort(unique(c(df1_allcaps$Year)))  # df2_all_hsrel$Year
allyears <- min(allyears):max(allyears)
nyears <- length(allyears)  # 21 years!

thisID <- allIDs[sample(1:ninds,1)]
temp1 <- subset(df1_allcaps,ID==thisID)
# temp2 <- subset(df2_all_hsrel,ID==thisID)

# allIDs
# thisID <- "2|"
# temp1 <- subset(df1_allcaps,ID==thisID)
# temp1 <- temp1[order(temp1$Date),]

# note: some headstarts missing from df2. Maybe just use df1 for everything??

# note: seems like some RT individuals are counted as "hand captured?"?  M
   # maybe just use trap captures for mark-recap estimates

# what is captype "v"?
vcap <- subset(df1_allcaps,CapType=="v")  # still not sure. seems to include telemetry and trap captures and incidentals... 


table(df1_allcaps$CapType)   # 530 "V" observations... 


hs_ids <- allIDs[grepl("HS",allIDs)]
nhs  <- length(hs_ids)   # 688 unique headstarted turtles in dataset

other_ids <- allIDs[!grepl("HS",allIDs)]
nother <- length(other_ids)   # 100 unique turtles that were not headstarted in the dataset


# loop through all known telemetry individuals and place them in a separate dataframe
telem_df <- NULL
capture_df <- NULL

df3_adult_telem$ID <- ifelse(is.na(df3_adult_telem$Notch),NA,paste0(df3_adult_telem$Notch,"|") ) 
adult_telem_ids <- sort(unique(df3_adult_telem$ID))

df4_HS_telem$ID <- paste0(df4_HS_telem$`Turtle ID`,"|HS",df4_HS_telem$`Year Class`)
hs_telem_ids <- sort(unique(df4_HS_telem$ID))
hs_telem_ids <- hs_telem_ids[!grepl("NA",hs_telem_ids)]

# hs_telem_notches <- sort(unique(df4_HS_telem$`Turtle ID`))

i=18  
for(i in 1:ninds){
  thisID <- allIDs[i]
  temp1 <- subset(df1_allcaps, ID==thisID)
  thisnotch <- strsplit(thisID,"\\|")[[1]][1]   # note this is not unique even for headstarts
  isHS <- grepl("HS",thisID)
  if(isHS){
    istelem <- thisID%in%hs_telem_ids
    # istelem <- thisnotch%in%hs_telem_notches
    if(istelem){
      temp2 <- subset(df4_HS_telem,ID==thisID)
      r=1
      for(r in 1:nrow(temp2)){   # account for individuals tracked more than once
        temp3 <- temp2[r,]
        telem_interval <- interval(temp3$FirstDate,temp3$LastDate)
        toadd <- subset(temp1,Date%within%telem_interval)
        telem_df <- rbind(telem_df,toadd)
        # toadd <- subset(temp1,!Date%within%telem_interval)
        # capture_df <- rbind(capture_df,toadd)
      }
      
    } #else{ # if not a telemetered individual...
    #   capture_df <- rbind(capture_df,temp1)
    # }
  }else{   # if adult
    istelem <- thisID%in%adult_telem_ids
    if(istelem){
      temp2 <- subset(df3_adult_telem,ID==thisID)
      r=1
      for(r in 1:nrow(temp2)){   # account for individuals tracked more than once
        temp3 <- temp2[r,]
        telem_interval <- interval(temp3$FirstDate,temp3$LastDate)
        toadd <- subset(temp1,Date%within%telem_interval)
        telem_df <- rbind(telem_df,toadd)
        # toadd <- subset(temp1,!Date%within%telem_interval)
        # capture_df <- rbind(capture_df,toadd)
      }
      
    }   #else{  # if not a telemetered individual...
    #   capture_df <- rbind(capture_df,temp1)
    # }
  }
}

ndx <- setdiff(df1_allcaps$row,telem_df$row)
capture_df <- df1_allcaps[ndx,]
capture_df <- subset(capture_df,!CapType%in%c("R","E","HC"))    # all non-telemetry observations


## remove additional observations Cara flagged as telemetry points (or just add to telemetry data?)

fdf <- read.csv("nontelem_df_McElroy.csv")
fdf$Date <- mdy(fdf$Date)
fdf$flag <- nchar(fdf$CLM.Suggest)>0
fdf <- subset(fdf,flag)
nrow(fdf)
names(fdf)

# note: move turtles marked 'delete' to telemetry data

newtelem <- subset(fdf,CLM.Suggest=="delete")
ntndx <- numeric(nrow(newtelem))
i=1
for(i in 1:nrow(newtelem)){
  this <- subset(df1_allcaps,Date==newtelem$Date[i]&ID==newtelem$ID[i]&CapType==newtelem$CapType[i])
  if(nrow(this)==1){
    ntndx[i] <- this$row
  }else{
    stop()
  }
}

# remove these individuals from capture data and move to telemetry... 

temp <- capture_df[capture_df$row%in%ntndx,]
nrow(temp)

temp2 <- telem_df[telem_df$row%in%ntndx,]  # note: many are already in the telemetry dataset!

nrow(telem_df)
telem_df <- rbind(telem_df,capture_df[capture_df$row%in%ntndx,])
  
nrow(capture_df)
capture_df <- capture_df[!capture_df$row%in%ntndx,]
  
rm(fdf,temp,temp1,temp2,temp3,this,vcap,newtelem,ntndx)

# telem_df


# Make capture histories ----------------------

# first make master capture history

master_ch <- matrix(0,nrow=ninds,ncol=nyears)
master_alive <- matrix(NA,nrow=ninds,ncol=nyears)
master_telem <- matrix(0,nrow=ninds,ncol=nyears)
master_firsts <- numeric(ninds)
master_age <- matrix(NA,nrow=ninds,ncol=nyears)   # KTS: change this per emails with Bryan 

colnames(master_ch) <- allyears  
colnames(master_age) <- allyears
colnames(master_alive) <- allyears
colnames(master_telem) <- allyears  


# look briefly at growth and age

# tapply(as.numeric(df1_allcaps$CL),df1_allcaps$Age,mean,na.rm=T)
# tapply(as.numeric(df1_allcaps$Annuli),df1_allcaps$Age,mean,na.rm=T)

# NOTE: Bryan wants to classify individuals as adult according to size, 
 #   but that will require a multi-state model that is much more sophisticated 
 #   than I have time for right now- for now we need to make an age-equivalent to define stage classes.


# first, loop through all hs individuals and "enter" them in the capture history when they were released into the GM population
allIDs_hs <- allIDs[grepl("HS",allIDs)]

# age classes: 
 #  1: newly release hs
 #  2: 2nd year after release
 #  3: young juvenile under age 10
 #  4: subadult (non-reproductive) under age 20
 #  5: reproductive adult age 20 and over

relseq <- c(1,2,rep(3,5),rep(4,9),rep(5,20))   # stage/age sequence for released individuals
i=481
for(i in 1:length(allIDs_hs)){
  thisid <- allIDs_hs[i]
  idndx <- match(thisid,allIDs)
  birthyear <- as.numeric(gsub("HS","",strsplit(thisid,"\\|")[[1]][2]))
  relndx <- match(birthyear+1,allyears)   # year released. 
  master_ch[idndx,relndx] <- 1    # enter it into the population at its birth year
  master_alive[idndx,relndx] <- 1
  master_age[idndx,relndx:nyears] <- relseq[1:length(relndx:nyears)] 
  master_firsts[idndx] <- relndx
}

# compute the number of hs releases in the dataset

hs_releases_byyear <- apply(master_ch,2,sum)

# make an analog to 'relseq' for dealing with individuals captured as juveniles

ageseq <- list()
ageseq[[1]] <-  c(rep(3,3),rep(4,9),rep(5,30))     # first captured at under 110
ageseq[[2]] <-  c(rep(4,9),rep(5,30)) # first captured from 110 to 125
ageseq[[3]] <-  c(rep(4,7),rep(5,30))# first captured between 125 and 150
ageseq[[4]] <-  c(rep(4,5),rep(5,30)) # first captured between 150 and 175
ageseq[[5]] <-  c(rep(4,3),rep(5,30)) # first captured between 175 and 185
ageseq[[6]] <-  c(rep(5,35)) # first captured above 185
ageseq 

agebreaks <- c(-Inf,110,125,150,175,185,Inf)

# then, loop through all non-hs individuals and "enter them in the capture history when they were first captured
allIDs_other <- setdiff(allIDs,allIDs_hs)
i=38
for(i in 1:length(allIDs_other)){
  thisid <- allIDs_other[i]
  idndx <- match(thisid,allIDs)
  temp1 <- subset(df1_allcaps,ID==thisid)
  fyear <- min(temp1$Year)
  fndx <- match(fyear,allyears)   # year entered study
  master_ch[idndx,fndx] <- 1    # enter it into the population
  master_alive[idndx,fndx] <- 1
  master_firsts[idndx] <- fndx
  theseyears <- sort(unique(temp1$Year))
  thesendx <- match(theseyears,allyears)
  temp <- tapply(temp1$Age,temp1$Year,function(t) names(which.max(table(t)))  )
  temp2 <- tapply(temp1$CL,temp1$Year, function(t) mean(as.numeric(t),na.rm=T)   )
  firstCL <- temp2[as.character(fyear)]   # size and class at first capture
  if(is.na(firstCL)){
    if(all(temp=="Adult")){
      firstCL = 190  # force to be adult
    }else if(any(temp=="Adult")){
      firsta <- min(as.numeric(names(which(temp=="Adult"))))
      firstandx <- match(firsta,allyears)
      yearstoad <- firstandx-fndx  # 6 years til known to become adult
      firstCL <- ifelse(yearstoad<5,180,ifelse(yearstoad<10,140,120) )
    }else{
      firstCL <- 120
    }
  } 
  agebin <- as.numeric(cut(firstCL,breaks = agebreaks)) 
  thisseq <- ageseq[[agebin]]
  if(any(temp=="Adult")){    # if it's known to be an adult sometime within the study period
    firsta <- min(as.numeric(names(which(temp=="Adult"))))
    firstandx <- match(firsta,allyears)
    if(firsta==theseyears[1]){  # if adult when first captured, then it's easy
      master_age[idndx,fndx:nyears] <- 5  # set to the reproductive adult class (5) 
    }else{  # if it has a known transition to adult sometime during the study period
      master_age[idndx,fndx:nyears] <- thisseq[1:length(fndx:nyears)] 
      master_age[idndx,firstandx:nyears] <- 5  # force it to be adult if seen as adult... 
    }
  }else{   # if the individual is capured as juvenile and doesn't transition to adult
    master_age[idndx,fndx:nyears] <- thisseq[1:length(fndx:nyears)]
  }
}

# thisid <- allIDs_other[14]
# temp1 <- subset(df1_allcaps,ID==thisid)

## next, loop through all telemetry data and continue filling in capture histories
telem_ids <- sort(unique(telem_df$ID))
telem_ninds <- length(telem_ids)   # 153 individuals
# telem_years <- sort(unique(telem_df$Year))

telem_hsids <- telem_ids[grepl("HS",telem_ids)]
telem_hsninds <- length(telem_hsids)
telem_otherids <- setdiff(telem_ids,telem_hsids)   # these should be mostly adults...
telem_otherninds <- length(telem_otherids)

  ## loop through telemetered headstarts
i=87
for(i in 1:telem_hsninds){
  thisid <- telem_hsids[i]
  idndx <- match(thisid,allIDs)
  birthyear <- as.numeric(gsub("HS","",strsplit(thisid,"\\|")[[1]][2]))
  relndx <- match(birthyear+1,allyears)   # year released. 
  temp1 <- subset(telem_df,ID==thisid)
  temp1 <- temp1[order(temp1$Date),]
  theseyears <- sort(unique(temp1$Year))
  thesendx <- match(theseyears,allyears)
  master_telem[idndx,thesendx] <- 1
  master_ch[idndx,thesendx] <- 1
  firstyear <- theseyears[1] 
  firstndx <- thesendx[1]
  lastyearalive <- max(temp1$Year[temp1$Status=="L"],na.rm=T)
  landx <- match(lastyearalive,allyears)
  lastndx <- max(thesendx)
  master_alive[idndx,relndx:landx] <- 1    # alive when first released up to last year known alive
  thislast <- min(nyears,lastndx+1)    #not needed?
  temp1$Status[which(is.na(temp1$Status))] <- "N"
  finalstatus <- ifelse(temp1$Status[nrow(temp1)]=="L",1,ifelse(temp1$Status[nrow(temp1)]=="D",0,2))
  startndx <- max(landx+1,relndx+1)
  if(finalstatus==0){
    if(startndx<nyears) master_alive[idndx,startndx:nyears] <- 0
    master_telem[idndx,min(nyears,landx+1)] <- 1   # make sure first year of being dead is recorded as having been noted...
    master_ch[idndx,min(nyears,landx+1)] <- 1
  }else{
    if(startndx<nyears) master_alive[idndx,startndx:nyears] <- NA
  } 
}

## loop through telemetered adults
i=10
for(i in 1:telem_otherninds){
  thisid <- telem_otherids[i]
  idndx <- match(thisid,allIDs)
  
  temp1 <- subset(df1_allcaps,ID==thisid)
  fyear <- min(temp1$Year)
  fndx <- match(fyear,allyears)   # year entered study
  
  temp1 <- subset(telem_df,ID==thisid)
  temp1 <- temp1[order(temp1$Date),]
  theseyears <- sort(unique(temp1$Year))
  thesendx <- match(theseyears,allyears)
  master_telem[idndx,thesendx] <- 1
  master_ch[idndx,thesendx] <- 1
  firstyear <- theseyears[1] 
  firstndx <- thesendx[1]
  lastyearalive <- max(temp1$Year[temp1$Status=="L"],na.rm=T)
  landx <- match(lastyearalive,allyears)
  lastndx <- max(thesendx)
  master_alive[idndx,fndx:landx] <- 1    # alive when first released up to last year known alive
  temp1$Status[which(is.na(temp1$Status))] <- "N"
  finalstatus <- ifelse(temp1$Status[nrow(temp1)]=="L",1,ifelse(temp1$Status[nrow(temp1)]=="D",0,2))
  startndx <- max(landx+1,relndx+1)
  if(finalstatus==0){
    if(startndx<nyears) master_alive[idndx,startndx:nyears] <- 0
    master_telem[idndx,min(nyears,landx+1)] <- 1   # make sure first year of being dead is recorded as having been noted...
    master_ch[idndx,min(nyears,landx+1)] <- 1
  }else{
    if(startndx<nyears) master_alive[idndx,startndx:nyears] <- NA
  } 
}
# 
# thisid=allIDs[249]
# temp1=subset(df1_allcaps,ID==thisid)


# then loop through the regular captures (not telemetry)

cap_ids <- sort(unique(capture_df$ID))
cap_ninds <- length(cap_ids)

i=231
for(i in 1:cap_ninds){
  thisid <- cap_ids[i]
  idndx <- match(thisid,allIDs)
  temp1 <- subset(capture_df,ID==thisid)
  theseyears <- sort(unique(temp1$Year))
  thesendx <- match(theseyears,allyears)
  lastyr <- theseyears[length(theseyears)]
  lyndx <- thesendx[length(theseyears)]
  lydf <- subset(temp1,Year==lastyr)
  lydf$Status[which(is.na(lydf$Status))] <- "N"
  lydead <- any(lydf$Status=="D")
  if(grepl("HS",thisid)){
    birthyear <- as.numeric(gsub("HS","",strsplit(thisid,"\\|")[[1]][2]))
    relndx <- match(birthyear+1,allyears)   # year released. 
    master_ch[idndx,thesendx] <- 1
    if(lydead){
      master_alive[idndx,relndx:max(lyndx-1,relndx)] <- 1
      master_alive[idndx,lyndx:nyears] <- 0
    }else{
      master_alive[idndx,relndx:max(thesendx)] <- 1 
    }
  }else{
    temp2 <- subset(df1_allcaps,ID==thisid)
    fyear <- min(temp2$Year)
    fndx <- match(fyear,allyears)   # year entered study
    master_ch[idndx,thesendx] <- 1
    if(lydead){
      master_alive[idndx,fndx:max(lyndx-1,fndx)] <- 1
      master_alive[idndx,lyndx:nyears] <- 0
    }else{
      master_alive[idndx,fndx:max(thesendx)] <- 1 
    }
  }
}

# determine sex for each individual

master_ismale <- numeric(ninds)
master_ismale[] <- NA
i=1
for(i in 1:ninds){
  thisid <- allIDs[i]
  temp1 <- subset(df1_allcaps,ID==thisid)
  # idndx <- match(thisid,allIDs) 
  if(any(!is.na(temp1$Sex))){
    temp2 <- table(temp1$Sex)
    if(grepl("M",names(which.max(temp2)))) master_ismale[i] <- 1
    if(grepl("U",names(which.max(temp2)))) master_ismale[i] <- NA
    if(grepl("F",names(which.max(temp2)))) master_ismale[i] <- 0
  }
}


# export df of non-telemetry observations...

# nontelem_df <- df1_allcaps[df1_allcaps$row%in%setdiff(df1_allcaps$row, telem_df$row),]
write.csv(capture_df,"nontelem_df2.csv",row.names = F)

master_ishs <- as.numeric(grepl("HS",allIDs))

# JAGS code --------------

cat("
model{

# interpolate covariates

pmale ~ dunif(0,1) 
for(i in 1:ninds){
  ismale[i] ~ dbern(pmale)
}

for(i in 1:ninds){
  for(t in firsts[i]:nyears){
    isyj[i,t] <- ifelse(age[i,t]==3,1,0)
    islj[i,t] <- ifelse(age[i,t]==4,1,0)
  }
  for(t in 1:(firsts[i]-1)){
    islj[i,t] <- 0
  }
}

# survival

for(a in 1:4){
  phi0[a] ~ dunif(0,1)
  phi0.l[a] <- log(phi0[a]/(1-phi0[a]))
}
phi0[5] <- phi0[4]      # force subadult and adult survival to be equal
phi0.l[5] <- phi0.l[4]

# phi.maleeff ~ dnorm(0,0.1)
phi.hseff ~ dnorm(0,1)
for(i in 1:ninds){
  for(t in firsts[i]:(nyears-1)){
    logit(phi[i,t]) <- phi0.l[age[i,t]] + isyj[i,t]*phi.hseff*ishs[i]  # add headstart effect for juveniles...  #+ step(age[i,t]-4)*phi.maleeff*ismale[i]
  }
}

# detection probability (probability of encountering non-telemetered individuals each year)

for(a in 1:nages){    # separate baseline detection prob for all age classes
  p0[a] ~ dunif(0,1)
  p0.l[a] <- log(p0[a]/(1-p0[a]))
}

p.sdyr ~ dunif(0,5)     # random year effect...
p.precyr <- pow(p.sdyr,-2)
for(t in 1:4){
  p.yeareff[t] ~ dnorm(0,p.precyr)
}
p.yeareff[5] <- -99  # no captures this year
for(t in 6:nyears){
  p.yeareff[t] ~ dnorm(0,p.precyr)
}
# p.sdind ~ dunif(0,5)
# p.precind <- pow(p.sdind,-2)
# for(i in 1:ninds){
#   p.indeff[i] ~ dnorm(0,p.precind)
#   p.indeff2[i] <- p.indeff[i]*isnative[i]
# }

p.dead ~ dunif(0,0.5)
#p.maleeff ~ dnorm(0,1)   
p.adfemeff ~ dnorm(0,1)   # reproductive adult females are likely to have higher detection prob
#p.hseff ~ dnorm(0,1)  # headstarts may have different detectability even after the first couple years? (only for juvenile stage...)

for(i in 1:ninds){
  for(t in (firsts[i]+1):(nyears)){
    logit(p[i,t]) <- p0.l[age[i,t]] + #step(age[i,t]-4)*p.maleeff*ismale[i] +   # effect of being male for subadults and adults
                                      # isyj[i,t]*p.hseff*ishs[i] +       # effect of being headstarted after first 2 years on detection prob as young juvenile
                                      step(age[i,t]-5)*p.adfemeff*(1-ismale[i]) +   # effect of being an adult female
                              p.yeareff[t]        # step(age[i,t]-3)*p.indeff2[i]   # year effect (and possibly individual effect...)
  }
  for(t in 1:firsts[i]){
    invp[i,t] <- 0
  }
}

# likelihood

for(i in 1:ninds){
  alive[i,firsts[i]] ~ dbern(1)    # must be alive at first capture occasion (or upon release)
  for(t in (firsts[i]+1):nyears){
    palive[i,t] <- alive[i,t-1] * phi[i,t-1]    # prob of still being alive and in the study this year
    alive[i,t] ~ dbern(palive[i,t])             # data/latent node
    pcap[i,t] <- telem[i,t-1]*telem[i,t]*1 + (1-telem[i,t-1]*telem[i,t]) * (alive[i,t]*p[i,t] + ((1-alive[i,t])*alive[i,t-1])*p.dead  )   # account for dead captures
    invp[i,t] <- 1/(pcap[i,t]+0.000001)  # for computing abundance using H-T...
    y[i,t] ~ dbern(pcap[i,t])
  } 
  for(t in 1:(firsts[i]-1)){
    alive[i,t] <- 0
  }
}

# derived terms
  # compute H-T estimate of abundance with and without headstarts?
  
for(i in 1:ninds){
  isnative[i] <- 1-ishs[i]
}

for(i in 1:ninds){
  for(t in 1:nyears){
    totN_HSs[i,t] <- alive[i,t]*ishs[i]
    totN_resHTs[i,t] <- y[i,t]*alive[i,t]*invp[i,t]*isnative[i]
    totN_resKs[i,t] <- alive[i,t]*isnative[i]   # non-headstarts in study known or suspected alive
    totN_HSljs[i,t] <- alive[i,t]*ishs[i]*fraclg[ysr[i,t]+1]
    # totN_HSljs[i,t] <- alive[i,t]*ishs[i]*islj[i,t]  # headstarts that made it to the large juvenile stage
    # totNs[i,t] <- y[i,t]*alive[i,t]*invp[i,t]
  }
}
for(t in 1:nyears){
  # totN[t] <- sum(totNs[1:ninds,t])
  totN_HS[t] <- sum(totN_HSs[1:ninds,t])
  totN_resHT[t] <- sum(totN_resHTs[1:ninds,t])
  totN_resK[t] <- sum(totN_resKs[1:ninds,t])
  # frac_HS[t] <- totN_HS[t]/totN[t]
  totN_HSlj[t] <- sum(totN_HSljs[1:ninds,t])
}
  
}    
",file="EMBL_JAGS3.txt")



# Package data -------------------

## from cara's calculations

fraclg <- c(0,0.08,0.1,0.43,0.64,0.85,0.96,rep(0.99,50))

ysr <- matrix(0,nrow=ninds,ncol=nyears)
i=1
for(i in 1:ninds){
  thishs <- master_ishs[i] 
  if(thishs ==1)  ysr[i,master_firsts[i]:nyears] <- 1:length(master_firsts[i]:nyears)
}

dfj <- list(
  ninds=ninds,
  nyears=nyears,
  nages=5,
  firsts=master_firsts,
  age=master_age,
  telem=master_telem,
  alive=master_alive,
  y=master_ch,
  ishs=master_ishs,
  ismale=master_ismale,
  fraclg=fraclg,
  ysr=ysr
)

alive2 <- master_alive
alive2[!is.na(master_alive)] <- NA
alive2[is.na(master_alive)] <- 1
temp <- sapply(1:ninds,function(t) alive2[t,1:master_firsts[t]] <<- NA   )

ifj <- function(){
  list(
    pmale=runif(1,0.4,0.6),
    # phi0=runif(4,c(0.5,0.6,0.6,0.8),c(0.6,0.7,0.8,0.9)),
    p0=runif(5,c(0.1,0.2,0.3,0.4,0.4),c(0.2,0.3,0.4,0.5,0.5)),
    # phi.maleeff=runif(1,-0.1,0.1),
    phi.hseff=runif(1,-0.1,0.1),
    # p.maleeff=runif(1,-0.1,0.1),
    p.adfemeff=runif(1,-0.1,0.1),
    # p.hseff=runif(1,-0.1,0.1),
    p.dead=runif(1,0.05,0.1),
    p.sdyr=runif(1,0.02,0.05),
    # p.sdind=runif(1,0.02,0.05),
    alive=alive2
  )
}
# ifj()

params <- c(
  "pmale",
  "phi0",
  "p0",
  # "phi.maleeff",
  "phi.hseff",
  # "p.maleeff",
  "p.adfemeff",
  # "p.hseff",
  "p.dead",
  "p.sdyr",
  # "p.sdind",
  "p.yeareff",  
  # "totN",
  "totN_HS",
  "totN_resHT",  # horvitz thompson
  "totN_resK",   # all non-hs known to be in the study
  "totN_HSlj"     # hss that make it to large juvenile stage
  # "frac_HS"
)
jagsfile="EMBL_JAGS3.txt"

# Run analysis --------------------

ni=20000
nb=10000
nt=20
nc=3
# ?jags
mod = jags(dfj, ifj, params, jagsfile,
     n.chains=nc, n.adapt=1000, n.iter=ni, n.burnin=nb, n.thin=nt,
     parallel=T, n.cores=nc)
saveRDS(mod,file="output/latestrun6.rds")

# Visualize results -------------------

# mod <- readRDS("output/latestrun6.rds")
# thisid=allIDs[211]
# temp1=subset(df1_allcaps,ID==thisid)

sims <- mod$sims.list

# plot(sims$pmale,type="l")
traceplot(mod,"pmale")
jagsUI::densityplot(mod,"pmale")

# plot(sims$phi0[,5],type="l")
traceplot(mod,"phi0[5]")
jagsUI::densityplot(mod,"phi0[5]")

quantile(sims$phi0[,2],c(0.5,0.025,0.975))
quantile(plogis(qlogis(sims$phi0[,3])+sims$phi.hseff),c(0.5,0.025,0.975))

quantile(sims$totN_HSlj[,nyears],c(0.5,0.025,0.975))
quantile(sims$totN_resK[,nyears],c(0.5,0.025,0.975))

totAd <- sims$totN_HSlj[,nyears] + sims$totN_resK[,nyears]
quantile(totAd,c(0.5,0.025,0.975))


# plot(sims$p0[,3],type="l")
traceplot(mod,"p0[5]")
jagsUI::densityplot(mod,"p0[4]")

jagsUI::densityplot(mod,"p.adfemeff")  # adult females are much more likely to be captured
 
# jagsUI::densityplot(mod,"p.hseff")  # headstarts are more likely to be captured adult females are much more likely to be captured

jagsUI::densityplot(mod,"phi.hseff") 

t1 <- t(apply(sims$phi0,2,function(t) quantile(t,c(0.025,0.5,0.975))))
t2 <- quantile(plogis(qlogis(sims$phi0[,3])+sims$phi.hseff),c(0.025,0.5,0.975))

png("figs/surv_by_age6.png",4.5,4,units="in",res=600)
errbar(1:5+c(0,0,-0.2,0,0),t1[1:5,2],t1[1:5,3],t1[1:5,1],ylab="Apparent survival",
       xaxt="n",xlab="Stage class",ylim=c(0,1),
       xlim=c(0.5,5.5),col=c(gray(0),gray(0),gray(0.5),gray(0.5),gray(0.5)),
       errbar.col=c(gray(0),gray(0),gray(0.5),gray(0.5),gray(0.5)))
errbar(3.2,t2[2],t2[3],t2[1],col=gray(0),errbar.col=gray(0),add=T)
axis(1,at=1:5,labels=c("HS1","HS2","SJ","LJ","Adult"))
legend("bottomleft",bty="n",pch=20,lty=1,col=c(gray(0.5),gray(0)),legend=c("general population","headstart"))
dev.off()

# t1 <- t(apply(sims$p0,2,function(t) quantile(t,c(0.025,0.5,0.975))))

p2_hs <- plogis(qlogis(sims$p0[,2]))   # sims$p.hseff
# p3_hs <- plogis(qlogis(sims$p0[,3])+sims$p.hseff)
p3 <- plogis(qlogis(sims$p0[,3]))  # probability of capturing a resident juvenile
# p4_fem_hs <- plogis(qlogis(sims$p0[,4]))
p4 <- plogis(qlogis(sims$p0[,4]))
# p4_male <- plogis(qlogis(sims$p0[,4])+sims$p.hseff+sims$p.maleeff)
# p4_male <- plogis(qlogis(sims$p0[,4])+sims$p.maleeff)
p5_fem <- plogis(qlogis(sims$p0[,5])+sims$p.adfemeff)
p5_male <- plogis(qlogis(sims$p0[,5]))  # +sims$p.maleeff

hss <- cbind(p2_hs,p3)  # headstart series
t1 <-  t(apply(hss,2,function(t) quantile(t,c(0.025,0.5,0.975))))

resmales <- cbind(p3,p4,p5_male)  # male series
t2 <-  t(apply(resmales,2,function(t) quantile(t,c(0.025,0.5,0.975))))

resfems <- cbind(p5_fem)
t3 <-  t(apply(resfems,2,function(t) quantile(t,c(0.025,0.5,0.975))))
  
png("figs/p_by_age5.png",5,4,units="in",res=600)
errbar(2:2-c(0),t1[1:1,2],t1[1:1,3],t1[1:1,1],ylab="Detection probability",
       xaxt="n",xlab="Stage class",ylim=c(0,0.4),xlim=c(0.7,5.5),
       col="darkred",errbar.col = "darkred")
errbar(3:5-c(0,0,0.2),t2[1:3,2],t2[1:3,3],t2[1:3,1],ylim=c(0,0.6),xlim=c(1,5.5),
       col="darkblue",errbar.col = "darkblue",add=T)
errbar(5:5+c(0.2),t3[1:1,2],t3[1:1,3],t3[1:1,1],ylim=c(0,0.6),xlim=c(1,5.5),
       col="darkgreen",errbar.col = "darkgreen",add=T)
axis(1,at=1:5,labels=c("HS1","HS2","SJ","LJ","Adult"))
legend("topleft",bty="n",col=c("darkblue","darkred","darkgreen"),lty=c(1,1,1),
       pch=20,legend=c("General Pop.","Headstart","Female"))
dev.off()

# plot(sims$p.maleeff,type="l")

# plot(sims$p0[,4],type="l")

# plot(sims$phi.maleeff,type="l")  # needs more time to fully converge? # TODO: remove this from analysis?

plot(sims$p.dead,type="l")

# plot(sims$p.sdyr,type="l")
# plot(sims$p.sdind,type="l")    # not converged yet (maybe remove term?)

plot(sims$p.yeareff[,16],type="l")

# plot(sims$totN[,10],type="l")
plot(sims$totN_HS[,10],type="l")
# plot(sims$totN_native[,10],type="l")

totN <- sims$totN_HS + sims$totN_resK

t1 <- t(apply(totN,2,function(t) quantile(t,c(0.025,0.5,0.975))))
t2 <- t(apply(sims$totN_HS,2,function(t) quantile(t,c(0.025,0.5,0.975))))
t3 <- t(apply(sims$totN_resK,2,function(t) quantile(t,c(0.025,0.5,0.975))))
t4 <- t(apply(sims$totN_resHT,2,function(t) quantile(t,c(0.025,0.5,0.975))))

t5 <- t4
t5[] <- sapply(1:length(t4),function(t) max(t3[t],t4[t])  )
  
thisndx <- 2:nyears
theseyears <- allyears[thisndx]
png("figs/N_by_time6.png",4.5,6.5,units="in",res=600)

par(mfrow=c(2,1))
par(mai=c(0.5,0.8,0.1,0.1))
errbar(theseyears-0.2,t1[thisndx,2],t1[thisndx,3],t1[thisndx,1],
       col=gray(0),errbar.col = gray(0), #,cex=0.5,  # pch=1,
       ylab="# Turtles",xlab="",ylim=c(0,400))
# errbar(theseyears+0,t2[thisndx,2],t2[thisndx,3],t2[thisndx,1],
       # col="brown",errbar.col = "brown",add=T)
errbar(theseyears+0.2,t5[thisndx,2],t5[thisndx,3],t5[thisndx,1],
       col=gray(0.5),errbar.col = gray(0.5),add=T)
legend("topleft",bty="n",pch=c(20,20),col=c(gray(0),gray(0.5)),
       lty=c(1,1),legend=c("Total N","Resident"))
# dev.off()


# plot total releases vs living releases

relyears <- c(2003,2007:2022)+1
releases2 <- c(10,10,20,35,58,28,70,64,60,32,6,16,32,57,56,76,68)  # note: added 2023
names(releases2) <- relyears
releases <- numeric(nyears)
releases[match(relyears,allyears)] <- releases2
names(releases) <- allyears
releases
hs_releases_byyear

cumsum(releases)

# png("figs/Rel_by_time3.png",5,4,units="in",res=300)
errbar(theseyears,t2[thisndx,2],t2[thisndx,3],t2[thisndx,1],
       col=gray(0.5),errbar.col = gray(0.5),
       ylab="# Turtles",xlab="",ylim=c(0,710))
# errbar(theseyears+0,t2[thisndx,2],t2[thisndx,3],t2[thisndx,1],
# col="brown",errbar.col = "brown",add=T)
# errbar(theseyears+0.2,t3[thisndx,2],t3[thisndx,3],t3[thisndx,1],
#        col="darkblue",errbar.col = "darkblue",add=T)
lines(theseyears,cumsum(releases[-1]),col=gray(0),lwd=2,lty=2)
legend("topleft",bty="n",pch=c(20,NA),col=c(gray(0.5),gray(0)),
       lty=c(1,2),lwd=c(1,2),legend=c("Surviving HS","Cumulative releases"))
dev.off()














