#**************************************************************************
#**** File: Calculate Mixed Trophic Impact From Ecosystem Model Output ****
#****   Project: Ecosystem Modelling of the Oceanic Gulf of Mexico     ****
#****                    Developer: Matt Woodstock                     ****
#**************************************************************************

## Citation ##
#* Woodstock, M.S., T.T. Sutton, T. Frank, Y. Zhang. (2021). 
#*   An early warning sign: trophic structure changes in the 
#*   oceanic Gulf of Mexico from 2011-2018. Ecological Modelling.

## Contact Email ##
#* fishesofthedeep@gmail.com - Matt Woodstock


## Notes ##
#* Most of this has to do with taking Ecopath with Ecosim model output and calculating MTI (Part 1)
#* This methodology could be used for any ecosystem where you have both diet and predation mortality matrices (Part 2)


## Clear Working Directory
rm(list=ls())

## Load Packages ##
packages = c("MASS","xlsx")

package.check <- lapply(packages,FUN = function(x) {
  if (!require(x, character.only = TRUE)) {
    install.packages(x, dependencies = TRUE)
    library(x, character.only = TRUE)
  }
})

#Set working directory#
#* For EwE output, @indir is your mc_Ecosimscenario directory
indir<-"DIRECTORY FOR MODEL OUTPUT/INPUT FOR THIS PROCESS"
outdir <-"DIRECTORY TO DUMP EXCEL FILES" #*N files created = N iterations:: Best to make a separate directory
setwd(indir)

## Load Species names ##
## You need to create these files ##
#* I provide an example. Take note of the formatting issues with importing special characters in R
load(file="Species Information.RData")

species<-as.matrix(read.csv("FILE NAME.csv",header=F)) #List of functional groups in the model
species_adj<-as.matrix(read.csv("FILE NAME.csv",header=F)) #Functional groups spelled as R will input them (i.e., special characters become periods)

## Global Parameters ##
n_sim<- #Number of iterations
n_spec<-length(species[,2]) #Number of functional groups
n_fisher<- #Number of fisheries in the model
year_start<- #Model Start Year
year_end<- #Model End Year
n_year<- #Number of years of the simulation
n_year_concern<- #Could be different/implemented if you include a spin up to the years of concern


#### Part 1:  Gather Ecopath with Ecosim Output and Calculate MTI For Each Year and Iteration####
#* If you already have a diet matrix, skip this
mti_array<-array(0,dim=c(n_spec+n_fisher,n_spec+n_fisher,n_year_concern,n_sim)) #* Placeholder for future calculation in loop
MTI<-array(0,dim=c(n_spec+n_fisher,n_spec+n_fisher,n_year_concern,n_sim)) #* Actual result that includes indirect impacts
n_count<-0 #Use a counter to control simulations

##Load Information##
for (file_count in 1:n_sim){
  if (file_count < 10){
    foldername<-paste0(indir,"/mc_output_trial000",file_count,sep="") #Working Directory where output is located
    if ((dir.exists(foldername))){
      setwd(foldername)
      n_count<-n_count+1
    }
  }
  if (file_count < 100){
    foldername<-paste0(indir,"/mc_output_trial00",file_count,sep="")
    if ((dir.exists(foldername))){
      setwd(foldername)
      n_count<-n_count+1
    }
  }
  if (file_count < 1000){
    foldername<-paste0(indir,"/mc_output_trial0",file_count,sep="")
    if ((dir.exists(foldername))){
      setwd(foldername)
      n_count<-n_count+1
    }
  }
  if (file_count < 10000){
    foldername<-paste0(indir,"/mc_output_trial",file_count,sep="")
    if ((dir.exists(foldername))){
      setwd(foldername)
      n_count<-n_count+1
    }
  }
  
  #Input diet composition#
  diet_array<-array(0,dim=c(n_spec+n_fisher,n_spec+n_fisher,n_year_concern))
  fish_contrib<-array(0,dim=c(n_spec,n_fisher,n_year_concern))
  for (a in 1:length(species[,2])){
    filename<-paste("prey_",species[a,2],"_annual.csv",sep="")
    if (file.exists(filename)){
      diet<-read.csv(filename,skip=9,header=T) #Necessary data always starts on row 10
      prey_taxa<-colnames(diet) #Collected prey items of predator
      for (b in 1:length(diet[,1])){
        for (c in 1:length(species[,2])){
          for (d in 2:length(diet[1,])){
            if (species_adj[c,2]==prey_taxa[d]){
              diet_array[c,a,b]<-diet[b,d] #You are creating a diet matrix by skipping all non-preys
            }
          }
        }
      }
    }
  }
  
  filename<-"catch-fleet-group_annual.csv" #Fishery Catches
  
  #** If you have alot of fisheries or a long model, creat a subset to reduce run time **
  if (file.exists(filename)){
    fishery<-read.csv(filename,skip=9,header=T)
    for (z in 1:length(fishery[,1])){
      for (y in year_start:year_end){ #Input years your model starts and ends
        for (x in 1:n_fisher){
          for (w in 1:n_spec){
            if (fishery[z,1]==y && fishery[z,2]==x && fishery[z,3]==w){
              fish_contrib[w,x,y-(year_start-1)]<-fishery[z,4]
            }
          }
        }
      }
    }
  }
  
  ## Calculate "Diet Composition" for fishery and append to diet_array
  for (a in ((n_spec+1):(n_spec+n_fisher))){
    for (y in year_start:year_end){
      for (x in 1:n_fisher){
        for (w in 1:n_spec){
          if (mean(fish_contrib[,x,-(year_start-1)])>0)
            diet_array[w,a,y-(year_start-1)]<-fish_contrib[w,x,y-(year_start-1)]/sum(fish_contrib[,x,y-(year_start-1)]) #Calculate proporitional contribution of a species to fishery
        }
      }
    }
  }
  
  #Input predation and fishing mortalities#
  #Predation Mortality#
  pred_array<-array(0,dim=c(n_spec+n_fisher,n_spec+n_fisher,n_year_concern))
  
  for (a in 1:length(species[,2])){ #a = prey species
    filename<-paste("predation_",species[a,2],"_annual.csv",sep="")
    if (file.exists(filename)){
      pred<-read.csv(filename,skip=9,header=T)
      z<-c(dim(pred))
      pred<-pred[,-z[2]]
      pred_taxa<-colnames(pred)
      for (b in 1:length(pred[,1])){ #b = year
        for (c in 1:length(species[,2])){ #c is potential pred species
          for (d in 2:length(pred[1,])){ #d is predator number
            if (species_adj[c,2]==pred_taxa[d]){
              pred_array[a,c,b]<-pred[b,d]
            }
          }
        }
      }
    }
  }
  
  #Fishing Mortality#
  filename<-paste("mort-fleet-group_annual.csv",sep="")
  if (file.exists(filename)){
    fish_mort<-read.csv(filename,skip=9,header=T)
    
    for (z in 1:length(fish_mort[,1])){
      for (a in (year_start:year_end)){ #Input years your model starts and ends
        for (b in 1:length(species[,2])){
          for (c in 1:n_fisher){
            if (fish_mort[z,1]==a && fish_mort[z,3]==b && fish_mort[z,2]==c){
              pred_array[b,c+n_spec,a-(year_start-1)]<-fish_mort[z,4] 
            }
          }
        }
      }
    }
  }
  
  #Calculate Proportion of Predation from predator#
  prop_pred<-array(0,dim=c(n_spec+n_fisher,n_spec+n_fisher,n_year_concern))
  
  for (a in 1:(n_spec+n_fisher)){ #Prey
    for (b in 1:(n_spec+n_fisher)){ #Predator
      for (c in 1:n_year_concern){
        prop_pred[a,b,c]<-pred_array[a,b,c]/sum(pred_array[a,,c]) #Need to know the proportion of total mortality made up by predation from each predator on each prey.
      }
    }
  }
  prop_pred[is.na(prop_pred)]<-0
  
  #Calculate MTI For Direct Relationships###
  for (a in 1:(n_spec+n_fisher)){
    for (b in 1:(n_spec+n_fisher)){
      for (c in 1:n_year_concern){
        mti_array[a,b,c,n_count]<-diet_array[a,b,c]-prop_pred[a,b,c] #Calculation of direct trophic impact
      }
    }
  }

  setwd(outdir)
  
  #* If you are not interested in having excel files, comment those lines out
  #* Will significantly increase run time, but may be wise to have in case the loop cannot finish for any reason
  for (year in 1:n_year_concern){
    filename<-paste("MTI simulation ", n_count,".xlsx",sep="")
    identity.matrix <- diag(ncol(mti_array[,,year,n_count]))
    MTI[,,year,n_count] <- MASS::ginv(identity.matrix - mti_array[,,year,n_count]) - identity.matrix #Calculation of total MTI for each interaction
    if (year == 1){ #Year one creates the file
      sheet_name<-paste("MTI Year ",year,sep="")
      write.xlsx(MTI[,,year,n_count],file=filename,
                 sheetName = sheet_name,append=F)
    }
    if (year > 1){ #Years 2+ add sheets onto existing file
      sheet_name<-paste("MTI Year ",year,sep="")
      write.xlsx(MTI[,,year,n_count],file=filename,
                 sheetName = sheet_name,append=T)
    }
  }
  print(c(file_count)) #Will want to keep track of your loop#
  Sys.sleep(0.01)
}

#* Save all data to reload later
#* Will save you time instead of reloading all files
save(MTI,file="Mixed Trophic Impact Input.RData")

#****************************Done****************************************



#### Part 2: If you already have diet and predation matrices and just want to calculate MTI ####

#Calculate MTI For Direct Relationships###
for (n_count in 1:n_sim){
  for (a in 1:(n_spec+n_fisher)){
    for (b in 1:(n_spec+n_fisher)){
      for (c in 1:n_year_concern){
        mti_array[a,b,c,n_count]<-diet_array[a,b,c]-prop_pred[a,b,c] #Calculation of direct trophic impact
      }
    }
  }

  setwd(outdir)

  #* If you are not interested in having excel files, comment those lines out
  #* Will significantly increase run time, but may be wise to have in case the loop cannot finish for any reason
  for (year in 1:n_year_concern){
    filename<-paste("MTI simulation ", n_count,".xlsx",sep="")
    identity.matrix <- diag(ncol(mti_array[,,year,n_count]))
    MTI[,,year,n_count] <- MASS::ginv(identity.matrix - mti_array[,,year,n_count]) - identity.matrix #Calculation of total MTI for each interaction
    if (year == 1){ #Year one creates the file
      sheet_name<-paste("MTI Year ",year,sep="")
      write.xlsx(MTI[,,year,n_count],file=filename,
               sheetName = sheet_name,append=F)
    } else { #Years 2+ add sheets onto existing file
      sheet_name<-paste("MTI Year ",year,sep="")
      write.xlsx(MTI[,,year,n_count],file=filename,
               sheetName = sheet_name,append=T)
    }
  }
  print(c(file_count)) #Will want to keep track of your loop#
  Sys.sleep(0.01)
}