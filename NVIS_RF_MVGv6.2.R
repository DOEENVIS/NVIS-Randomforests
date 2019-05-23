#RF taking NVIS flatfile in and predicting the MVG

#TO DO 
#Test to do all MVGs - 
library(rpart)
library(randomForest) 
library(MASS)
library(rfPermute)

start_time <- Sys.time()

#########################################
output.predict.name <- "NVIS_Predict_MVG.csv"
output.wd <- "./outputs"
input.file <- "./VC_II_FINAL_MVG_clean.csv"

# Variables
#Data size to use
Data_size.p = 50000                          #datasize for predicted data
#Data_size = 5000                            #size you want the data, used for testing or so computer doesn't fall over.
Data_size.sub = 1500                        #Subsample to make the veg groups more equal (instead of 1000 vs 10)
rf_trees <- 200                            #the number of trees to be done by RF
DataDiv <- 2                                #test data division to test the model accurately before running on other data, 2 is dividing it by half (1000/2)

#data names
prefixdsp1 <- "VC1"
#col_to_binary <-c("MVG", "NVIS_ID", "MAPUNT_IDE")
#prefixd <- col_to_binary

col_to_binary <- c("sp1", "sp2", "sp3")
prefixd <- col_to_binary
predictorVari <- "NVIS_Eucs.s$MVG_VICTA_II"             #list of the predictor varliables 
data_names <- c(col_to_binary, "PredictorVari")


#########################################
##############
NVIS_Eucs.raw <- read.csv(input.file,header=T)
NVIS_Eucs.df <- data.frame(NVIS_Eucs.raw)
nrow(NVIS_Eucs.raw)

###################################################
#function - create binary dataframe from rows
###################################################
Binary_df <- function(Veg_data, prefixd){
  #remove junk
  upper_sp1 <- unique(Veg_data[Veg_data != ""]) #pulling out only unique and remove blank
  upper_sp1 <- na.exclude(upper_sp1)  #remove NA
  upper_sp1 <- (upper_sp1[upper_sp1 != "-9999"])
  upper_sp1 <- (upper_sp1[upper_sp1 != "unknown"])
  #upper_sp1 <- (upper_sp1[upper_sp1 != " "])

  count <- 0
  for (sp in upper_sp1){
    count <- 1 + count
    message(count,": ",sp)              #simple print function
    preSp <- paste(prefixd, sp)         #gives column unique name
    if (count == 1) {binary2 = (preSp)
    } else {
      binary2 = c(binary2,preSp)       #need to change sp to Usp_1_ passed as a variable paste(prefixd, sp)  prefix = "Usp1 "  sp = "Euc"
    }
  }
  #create df with unique column names from list
  df <- data.frame(matrix(ncol = length(binary2), nrow = length(Veg_data)))
  colnames(df) <- binary2

#   produce binary vector and puts in dataframe for each row (eg community/species)  
  count1 = 0
  for(i in colnames(df)){
    col1 <- numeric()
    count1 <- count1 + 1
    message(count1,": ",i, "\t\t")
    for (SpTot in Veg_data){
      i2 <- substr(i,nchar(prefixd)+2,nchar(i))   #extracts raw name for if statement
      if (is.na(SpTot)){col1 = c(col1,0)}
      else if (SpTot == -9999){col1 = c(col1,0)}
      else if (SpTot == 'unknown'){col1 = c(col1,0)}
      else if(SpTot == i2)
      {col1 = c(col1,1)
      }
      else{col1 = c(col1,0)}
    }
    df[count1] <- col1
  }
return(df)
}

#################################################
#function - loops through the vari: prefixd and col_to_binary
#################################################
#puts it all in a dataframe
#col_to_binary <- c("DOMINANT_STRATUM", "U_DOMINANT_GROWTH_FORM", "U_HEIGHT_CLASS", "U_COVER_CODE")
#prefixd <- col_to_binary
#df.input <- data.frame(NVIS_Eucs_other)
#colnames(df.input) <- col_to_binary
#df.inputT[DOMINANT_STRATUM]
#class(df.input)

Binary.df.loop <- function(df.input, col_to_binary, prefixd){
  df.input <- data.frame(df.input)
  precount = 1  
  for (VariB in col_to_binary){
    print(VariB)
    VEGvari1 <- Binary_df(df.input[[VariB]], prefixd[precount])
    if (precount == 1){
      Nvis_analysis <- VEGvari1}
    else {Nvis_analysis <- cbind(Nvis_analysis, VEGvari1)}
    precount = precount + 1
  }
  return(Nvis_analysis)
} 
###################################################
#function - subsample down by TASVEG$VEGCODE_D
###################################################

Veg_subsample <- function(Veg_data, Data_size.sub, subfield){
  
  NVIS_Eucs.temp <- NVIS_Eucs.subsampled <- NVIS_Eucs.df[FALSE,]  #Create empty df shell from df
  subfield.U <- sort(unique(subfield))
  message("datasize is: ", Data_size.sub)
  for(u in subfield.U){
    mvg.sub <- subset(Veg_data, subfield == u)
    subset_length <- (nrow(mvg.sub))
    if(subset_length >= Data_size.sub){
      message("TasVeg: ", u)
      message("total polygons: ", subset_length)
      set.seed(343)
      NVIS_Eucs.temp <- mvg.sub[sample(nrow(mvg.sub), size = Data_size.sub),]
      NVIS_Eucs.subsampled <- rbind(NVIS_Eucs.subsampled, NVIS_Eucs.temp)
    }else{
      if(u == ""){message("BLANk not added")
      }else{
        message("Veg group: ", u)
        message("total polygons: ", subset_length, "\t\t\t\t\t\tLower than datasize")
        set.seed(3)
        NVIS_Eucs.temp <- mvg.sub[sample(nrow(mvg.sub), size = nrow(mvg.sub)),]}
      NVIS_Eucs.subsampled <- rbind(NVIS_Eucs.subsampled, NVIS_Eucs.temp)
    }
  }
  return(NVIS_Eucs.subsampled)
}
#################################################
#data exploration

colnames(NVIS_Eucs.raw)

NVIS_Eucs.raw.S <- subset(NVIS_Eucs.raw, MVG_VICTA_II < 10)
NVIS_Eucs.raw.St <- subset(NVIS_Eucs.raw.S, NUMBER_OF_L6_STRATA != -9999)
NVIS_Eucs.raw.St <- subset(NVIS_Eucs.raw.S, U_HEIGHT_CLASS != -9999)
NVIS_Eucs.raw.St <- subset(NVIS_Eucs.raw.S, U_COVER_CODE != "unknown")
colnames(NVIS_Eucs.raw.S)
nrow(NVIS_Eucs.raw.St)

allhist <- hist(NVIS_Eucs.raw.St$MVG_VICTA_II)
table(NVIS_Eucs.raw.St$MVG_VICTA_II)

colnames(NVIS_Eucs.raw.St)
summary(NVIS_Eucs.raw.St$MVG_VICTA_II)

#Pulls out the covariates that are needed
#################################################
#Model 1. checking - data preparation
#################################################
#call function - subsample down by TASVEG$VEGCODE_D

#subsample down 

subfield <- NVIS_Eucs.raw.St$MVG_VICTA_II                                                #need to put this as variable
NVIS_Eucs.s <- Veg_subsample(NVIS_Eucs.raw.St, Data_size.sub, subfield)
barplot(table(NVIS_Eucs.s$MVG_VICTA_II))                                                  ##need to put this as variable
TasVeg.freq.post <- table(NVIS_Eucs.s$MVG_VICTA_II)

#Pulls out the covariates that are needed
#NVIS_Eucs = data.frame(NVIS_Eucs.s[7], NVIS_Eucs.s[8],NVIS_Eucs.s[9])                #NEEDS WORK AS THERE MIGHT BE MORE OR LESS PREDICTOR VARIABLES
NVIS_Eucs = data.frame(NVIS_Eucs.s$U_SPECIES1, NVIS_Eucs.s$U_SPECIES2,NVIS_Eucs.s$U_SPECIES3) 
colnames(NVIS_Eucs) <- col_to_binary

col_to_binary_other <- c("DOMINANT_STRATUM", "U_GENUS1_DOMINANT", "U_DOMINANT_GROWTH_FORM", "U_HEIGHT_CLASS", "U_COVER_CODE")
prefixd_other <- col_to_binary_other
NVIS_Eucs_other <- data.frame(NVIS_Eucs.s$DOMINANT_STRATUM, NVIS_Eucs.s$U_GENUS1_DOMINANT, NVIS_Eucs.s$U_DOMINANT_GROWTH_FORM, NVIS_Eucs.s$U_HEIGHT_CLASS, NVIS_Eucs.s$U_COVER_CODE)  #adds the predictor variable
colnames(NVIS_Eucs_other) <- col_to_binary_other


#creates final dataframe for analaysis
Nvis_analysis.all <- Binary.df.loop(NVIS_Eucs, col_to_binary, prefixd)
Nvis_analysis.other <- Binary.df.loop(NVIS_Eucs_other, col_to_binary_other, prefixd_other)

PredictorVari <- NVIS_Eucs.s$MVG_VICTA_II
NVIS_ID <- NVIS_Eucs.s$NVIS_ID

Nvis_analysis <- data.frame(Nvis_analysis.all, Nvis_analysis.other, PredictorVari, NVIS_ID)  #adds the predictor variable

#call function - create binary dataframe from rows
#VEGCODE_D1 <- Binary_df(NVIS_Eucs$VEGCODE_D, prefixdsp1)

#creates final dataframe for analaysis
#Nvis_analysis <- data.frame(VEGCODE_D1, NVIS_Eucs$MVG_FINAL, NVIS_Eucs$NVIS_ID)

###########################################################
#split train and test data - "Nvis_analysis"

Data_size <- round(nrow(Nvis_analysis))
set.seed(124)
Nvis_analysis.rd <- Nvis_analysis[sample(1:nrow(Nvis_analysis)),] # Randomize data
data_break <- round(nrow(Nvis_analysis.rd)/DataDiv)

train <- Nvis_analysis.rd[0:(Data_size/DataDiv),] # Select first half of data   ########################
names(train) <- make.names(names(train))
train <- within(train, rm(NVIS_ID)) #removes the NVIS id

test <- Nvis_analysis.rd[((Data_size/DataDiv)):(nrow(Nvis_analysis.rd)),] # Select Second half
names(test) <- make.names(names(test))
test <- within(test, rm(PredictorVari)) #removes the NVIS id

summary(Nvis_analysis.rd$PredictorVari)
summary(train$PredictorVari)
length(Nvis_analysis.rd$PredictorVari)
#############################################################################################
#Random forests - training model rf_trees = 100
nrow(train)
train.rf <- randomForest(as.factor(PredictorVari) ~ ., ntree=rf_trees, data=train)
train.rf
variable.imp <- round(importance(train.rf),2)                                   #varible importance - which variables are driving the model
varImpPlot(train.rf, sort = T, n.var = 50)                                      #variable importance plot
plot(margin(train.rf))                                                          #model run

###################################################
#Random forests - TESTING - predict with split data from the training data

P.class <- predict(train.rf, newdata = test,type="class")
P.prob <- predict(train.rf, newdata = test,type="prob")
#P.vote <- predict(train.rf, newdata = test,type="vote")

plot(P.class)
table(P.class)

p.class.nvis <- data.frame(P.class, test$NVIS_ID)
Prob.class.NVIS <- data.frame(P.prob, P.class, test$NVIS_ID)

end_time <- Sys.time()
end_time - start_time

#############################################################################################
#Model 2 Seperate data
#############################################################################################
#import and predict strata
Predict_NVIS <- "./VC_II_FINAL_MVG_clean.csv"              #used as test, ideally new dataset would be imported

Predict_NVIS_Eucs <- read.csv(Predict_NVIS,header=T)
NVIS_Predict.df = data.frame(Predict_NVIS_Eucs)                #NEEDS WORK AS THERE MIGHT BE MORE OR LESS PREDICTOR VARIABLES
summary(NVIS_Predict.df)

NVIS_Eucs.p = data.frame(NVIS_Predict.df$U_SPECIES1, NVIS_Predict.df$U_SPECIES2,NVIS_Predict.df$U_SPECIES3) 
colnames(NVIS_Eucs.p) <- col_to_binary
NVIS_Eucs_otherP <- data.frame(NVIS_Predict.df$DOMINANT_STRATUM, NVIS_Predict.df$U_GENUS1_DOMINANT, NVIS_Predict.df$U_DOMINANT_GROWTH_FORM, NVIS_Predict.df$U_HEIGHT_CLASS, NVIS_Predict.df$U_COVER_CODE)  #adds the predictor variable
colnames(NVIS_Eucs_otherP) <- col_to_binary_other

#creates final dataframe for analaysis
Nvis_analysis.all.p <- Binary.df.loop(NVIS_Eucs.p, col_to_binary, prefixd)
Nvis_analysis.other.p <- Binary.df.loop(NVIS_Eucs_otherP, col_to_binary_other, prefixd_other)

#subsample to speed up analysis, possibly not need
NVIS_ID.p <- NVIS_Predict.df$NVIS_ID

Nvis_pred <- data.frame(Nvis_analysis.all.p, Nvis_analysis.other.p,  NVIS_ID.p)  #adds the predictor variable

#NVIS_Predict <- NVIS_Predict.df
#nrow(NVIS_Predict)
#################################################
###############
#call function

#creates final dataframe for analaysis

train.Nvis_pred.subset <- train[,which(names(train) %in% names(Nvis_pred))]
train.Nvis_pred.subset.p <- cbind(train.Nvis_pred.subset, train$PredictorVari)

# Fit model to train.Nvis_pred.subset, use model to predict to Nvis_pred
names(train.Nvis_pred.subset.p) <- make.names(names(train.Nvis_pred.subset.p))
names(Nvis_pred) <- make.names(names(Nvis_pred))
Nvis_pred <- data.frame(Nvis_analysis.all)                    
attach(Nvis_pred)

#Random forests - training model
train.NVIS <- randomForest(as.factor(train.PredictorVari) ~ ., ntree=rf_trees, data=train.Nvis_pred.subset.p)
train.NVIS

#Random forests - prediction models
P.class <- predict(train.NVIS, newdata = Nvis_analysis.p,type="class")
P.prob <- predict(train.NVIS, newdata = Nvis_analysis.p,type="prob")
#P.vote <- predict(train.NVIS, newdata = VEGCODE_pred,type="vote")

#put output into a dataframe
p.class.nvis <- data.frame(P.class, NVIS_ID.p)
p.class.nvis <- unique(p.class.nvis)
colnames(p.class.nvis) <- c("MVG", "NVIS_ID")

#show probablility/possible errors
#Prob.class.NVIS <- data.frame(P.prob, P.class, Nvis_analysis.all$NVIS_Predict.NVIS_ID)
#Prob.class.NVIS <- unique(Prob.class.NVIS)
###################################################################
#putting the predicted output together
colnames(NVIS_Eucs.df)

#creating df with TasVeg names to join up to veg group prediction
Predict_NVIS_Eucs2 <- data.frame(NVIS_Eucs.df$MVG_FINAL, NVIS_Eucs.df$NVIS_ID)               #####
colnames(Predict_NVIS_Eucs2) <- c("MVG", "NVIS_ID")
#Predict_NVIS_Eucs3[order(Predict_NVIS_Eucs3$Predict_NVIS_Eucs.VEGCODE_D),]
#colnames(Predict_NVIS_Eucs3) <- c("TasVEG", "NVIS_ID", "MAPUNT")
#nrow(Predict_NVIS_Eucs3)

#merge the veg group prediction and the tasveg class
p.class.nvis2 <- merge.data.frame(p.class.nvis, Predict_NVIS_Eucs2, by.x = "NVIS_ID", by.y = "NVIS_ID")
colnames(p.class.nvis2) <- c( "NVIS_ID", "MVG_predict", "MVG_Final")

##################
#checking against training/test data, pull out nvis_id and MVG
NVIS_Eucs.raw2 <- data.frame(NVIS_Eucs.raw$NVIS_ID, NVIS_Eucs.raw$MVG_FINAL)
NVIS_Eucs.raw3 <- unique(NVIS_Eucs.raw2)
colnames(NVIS_Eucs.raw3) <- c( "NVIS_ID", "MVG_final")
#NVIS_Eucs.raw3[order(NVIS_Eucs.raw3$MVG_final),]
#nrow(NVIS_Eucs.raw3)

################################
#put predicted and raw veg groups together for comparison
p.class.nvis5 <- merge.data.frame(p.class.nvis2, NVIS_Eucs.raw3, by.x = "NVIS_ID", by.y = "NVIS_ID")
colnames(p.class.nvis5) <- c( "NVIS_ID", "MVG_predict", "MVG_Final", "MVG_final")

#pull out what is of interest - no double ups
p.class.nvis6 <- data.frame(p.class.nvis5$NVIS_ID, p.class.nvis5$MVG_predict, p.class.nvis5$MVG_final, p.class.nvis5$MVG_final)
colnames(p.class.nvis6) <- c( "NVIS_ID", "MVG_predict", "MVG_final", "MVG_final")
p.class.nvis6[order(p.class.nvis6$MVG_final),]

#look at distribtution of communities
barplot(table(NVIS_Eucs.raw$MVG_FINAL))
TasVeg.freq <- table(NVIS_Eucs.raw$MVG_FINAL)

######################
#error checking 

#calC the diff between predicted and NVIS (Should be 0)
DIFF <- as.numeric(as.character(p.class.nvis6$MVG_predict)) - as.numeric(as.character(p.class.nvis6$MVG_final))
p.class.nvis7 <- data.frame(p.class.nvis6, DIFF)
#p.class.nvis7[order(p.class.nvis7$TasVeg),]

#only shows the TasVeg with errors
subs.nvis <- subset(p.class.nvis7, DIFF != 0)
subs.nvis <- subs.nvis[order(subs.nvis$MVG_final),]
if (dim(subs.nvis)[1] != 0){message(subs.nvis)
  }else{message("All good no mismatch Errors")}

subs.nvis.pos <- subset(p.class.nvis7, DIFF == 0)
subs.nvis.pos <- subs.nvis.pos[order(subs.nvis.pos$MVG_final),]
if (dim(subs.nvis.pos)[1] != 0){message(subs.nvis.pos)
}else{message("All good no mismatch Errors")}

nrow(p.class.nvis7)
length(subs.nvis.pos$MVG_final)/nrow(p.class.nvis7)
length(subs.nvis$MVG_final)/nrow(p.class.nvis7)
###################################################################
#output results to CSV
#setwd(output.wd)
#write.csv(p.class.nvis, file = output.predict.name, row.names=FALSE)

###################################################################
print('\n\nfinnished')
end_time <- Sys.time()
end_time - start_time

