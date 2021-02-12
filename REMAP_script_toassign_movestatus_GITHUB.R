#Mary Regina Boland
#Jessica R Meeker
#Script to clean addresses and determine if someone moved
#Originally Written: April 11, 2019
#Updated and Made for github on: February 11-12, 2021

library(stringdist)
library(RCurl)


###READING IN SAMPLE DATA
#reading file straight from github
moved_data <- read.csv("https://raw.githubusercontent.com/bolandlab/REMAP/main/REMAP_sample_data_UPDATED.csv")

#reading file from local (need to set filepath)
#fldr="/SCRIPTS_FOR_GITHUB/"
#moved_data = read.csv(paste(fldr, "REMAP_sample_data.csv", sep=""))
head(moved_data)
length(unique(moved_data$PATIENT_ID)) #17 patients
###END READING IN SAMPLE DATA


###REMAP ALGORITHM BELOW
#make all addresses uppercase
moved_data$ADDR_HX_LINE1_atdeliv =toupper(moved_data$ADDR_HX_LINE1_atdeliv)
moved_data$ADDR_HX_LINE1_1yearprior =toupper(moved_data$ADDR_HX_LINE1_1yearprior) 

#converting street to st for string checking
moved_data$ADDR_HX_LINE1_atdeliv=gsub("STREET", "ST", moved_data$ADDR_HX_LINE1_atdeliv)
moved_data$ADDR_HX_LINE1_1yearprior=gsub("STREET", "ST", moved_data$ADDR_HX_LINE1_1yearprior)

#converting place to pl
moved_data$ADDR_HX_LINE1_atdeliv=gsub("PLACE", "PL", moved_data$ADDR_HX_LINE1_atdeliv)
moved_data$ADDR_HX_LINE1_1yearprior=gsub("PLACE", "PL", moved_data$ADDR_HX_LINE1_1yearprior)

#computing the number of differences
moved_data$num_differences_in_address=stringdist(moved_data$ADDR_HX_LINE1_1yearprior, moved_data$ADDR_HX_LINE1_atdeliv, method="dl")


#if less then or equal to 5 character changes in the address line  occurred then say no changes have occurred
moved_data$moved="YES"
moved_data$num_differences_in_address_woaptnum=0
for(i in 1:dim(moved_data)[1]){
  data_it=moved_data[i, ]
  data_it$ADDR_HX_LINE1_1yearprior=gsub(" UNIT [A-Z]{0,2}[0-9]{0,5}[A-Z]{0,3}", "", data_it$ADDR_HX_LINE1_1yearprior)
  data_it$ADDR_HX_LINE1_1yearprior=gsub(" APT [A-Z]{0,2}[0-9]{0,5}[A-Z]{0,3}", "", data_it$ADDR_HX_LINE1_1yearprior)
  data_it$ADDR_HX_LINE1_1yearprior=gsub("\\.", "", data_it$ADDR_HX_LINE1_1yearprior)  
  data_it$ADDR_HX_LINE1_atdeliv=gsub(" UNIT [A-Z]{0,2}[0-9]{0,5}[A-Z]{0,3}", "", data_it$ADDR_HX_LINE1_atdeliv)
  data_it$ADDR_HX_LINE1_atdeliv=gsub(" APT [A-Z]{0,2}[0-9]{0,5}[A-Z]{0,3}", "", data_it$ADDR_HX_LINE1_atdeliv)
  data_it$ADDR_HX_LINE1_atdeliv=gsub("\\.", "", data_it$ADDR_HX_LINE1_atdeliv)
  if(stringdist(data_it$ADDR_HX_LINE1_1yearprior, data_it$ADDR_HX_LINE1_atdeliv, method="dl")<=5){
    moved_data$moved[i]="NO"
  }
  
  #if changed occurred in street number address then list the patient as moved
  deliv_num = strsplit(data_it$ADDR_HX_LINE1_atdeliv, " ")
  deliv_num=deliv_num[[1]][1]
  oneyear_num=strsplit(data_it$ADDR_HX_LINE1_1yearprior, " ")
  oneyear_num=oneyear_num[[1]][1]
  if((is.na(deliv_num)==F) && is.na(oneyear_num)==F){
    if(deliv_num!=oneyear_num){
      moved_data$moved[i]="YES"
    }
    if(deliv_num==oneyear_num){
      moved_data$moved[i]="NO"
    }
  }
  moved_data$num_differences_in_address_woaptnum[i]=stringdist(data_it$ADDR_HX_LINE1_1yearprior, data_it$ADDR_HX_LINE1_atdeliv, method="dl")
}
#View(moved_data)

#number of patients that moved
length(unique(moved_data$PATIENT_ID[which(moved_data$moved=="YES")]))  #7
length(unique(moved_data$PATIENT_ID[which(moved_data$moved=="NO")]))  #10


#this determines if patients moved zip code
moved_data$moved_zipcode="YES"
for(i in 1:dim(moved_data)[1]){
  data_it=moved_data[i, ]
  data_it$ZIP_HX_1yearprior=gsub("\\-[0-9]{4}", "", tolower(data_it$ZIP_HX_1yearprior))
  data_it$ZIP_HX_atdeliv=gsub("\\-[0-9]{4}", "", tolower(data_it$ZIP_HX_atdeliv))
  if(data_it$ZIP_HX_1yearprior==data_it$ZIP_HX_atdeliv){
    moved_data$moved_zipcode[i]="NO"
  }
}
head(moved_data)
table(moved_data$moved_zipcode)

#this calculates if the moved zip code variable is concordant
#with the moved address variable
moved_data$concordant_move="NO"
for(i in 1:dim(moved_data)[1]){
  if(moved_data$moved[i]=="YES" && moved_data$moved_zipcode[i]=="YES"){
    moved_data$concordant_move[i]="YES"  
  }
}
table(moved_data$concordant_move)
table(moved_data$moved_zipcode)
table(moved_data$concordant_move)
head(moved_data)

moved_data$moved_within_zipcode="NO"
for(i in 1:dim(moved_data)[1]){
  if(moved_data$moved[i]=="YES" && moved_data$moved_zipcode[i]=="NO"){
    moved_data$moved_within_zipcode[i]="YES"  
  }
}
table(moved_data$moved_within_zipcode)

