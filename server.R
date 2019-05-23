#library(ExplAPSIM)
library(grid)
library(ggplot2)
library(randomForest)
library(MASS)
library(rfPermute)

#################################  #CHANGE SOME TO INPUTS
# Variables
#Data size to use
Data_size.p = 20000                          #datasize for predicted data
Data_size = 5000                            #size you want the data, used for testing or so computer doesn't fall over.
Data_size.sub = 50
DataDiv <- 2                                #test data division to test the model accurately before running on other data, 2 is dividing it by half (1000/2)

#data names
#output.predict.name <- "Tas_Predict.csv"
prefixdsp1 <- "VC1"
col_to_binary <-c("VEGCODE_D", "NVIS_ID", "MAPUNT_IDE")
prefixd <- col_to_binary

###################################################
#function - create binary dataframe from rows
###################################################

Binary_df <- function(Veg_data, prefixd){
  #remove junk
  upper_sp1 <- unique(Veg_data[Veg_data != ""]) #pulling out only unique and remove blank
  upper_sp1 <- na.exclude(upper_sp1)  #remove NA
  upper_sp1 <- (upper_sp1[upper_sp1 != "-9999"])
  upper_sp1 <- (upper_sp1[upper_sp1 != "unknown"])
  
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

###################################################
#function - subsample down by TASVEG$VEGCODE_D
###################################################

TasVeg_subsample <- function(Veg_data, Data_size.sub){
  NVIS_Eucs.df <- data.frame(Veg_data)
  NVIS_Eucs.raw <- Veg_data
  NVIS_Eucs.temp <- NVIS_Eucs.subsampled <- NVIS_Eucs.df[FALSE,]  #Create empty df shell from df
  VEGCODE.U <- sort(unique(NVIS_Eucs.raw$VEGCODE_D))
  message("datasize is: ", Data_size.sub)
  for(u in VEGCODE.U){
    mvg.sub <- subset(NVIS_Eucs.raw, VEGCODE_D == u)
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
        message("TasVeg: ", u)
        message("total polygons: ", subset_length, "\t\t\t\t\t\tLower than datasize")
        set.seed(3)
        NVIS_Eucs.temp <- mvg.sub[sample(nrow(mvg.sub), size = nrow(mvg.sub)),]}
      NVIS_Eucs.subsampled <- rbind(NVIS_Eucs.subsampled, NVIS_Eucs.temp)
    }
  }
  return(NVIS_Eucs.subsampled)
}

#################################
#################################

server <- shinyServer(function(input, output, session) {
  shinyOptions(shiny.maxRequestSize = 30*1024^2)
#################################
  #MAIN - INPUT PAGE
#################################  
  
  #read in file (csv) 
  filedata <- reactive({
    
    infile <- input$file1
    if(is.null(infile)){
      return(NULL)
    }
    read.csv(infile$datapath)
    })
  
  filedata2 <- reactive({
    infile2 <- input$file2
    if(is.null(infile2)){
      return(NULL)
    }
    read.csv(infile2$datapath)
  })   

  
  output$train_data <- renderTable({
    trips <- filedata()
    if(is.null(trips)) 
      return(NULL)
    trips
  },striped = TRUE, hover = TRUE, bordered = TRUE) 
  
  output$predict_data <- renderTable({
    trips1 <- filedata2()
    if(is.null(trips1)) 
      return(NULL)
    trips1
  },striped = TRUE, hover = TRUE, bordered = TRUE)   
  
  output$table <- renderTable
    
#################################
  #some desc stats
  
#training data: number of rows
    output$Nrows <- renderPrint({  
    datr <- nrow(filedata())           
    cat(paste("training rows: ", datr))         #can make the colour dynamic by making a function if
  })
    
#training data: number of columns
  output$Crows <- renderPrint({  
    datc <- ncol(filedata())
    cat(paste("training cols: ", datc))         #can make the colour dynamic by making a function if
  })
  
######
  #predict data: number of rows
  output$Prows <- renderPrint({  
    datrp <- nrow(filedata2())           
    cat(paste("Predict rows: ", datrp))         #can make the colour dynamic by making a function if
  })
  
  #predict data: number of columns
  output$Pcols <- renderPrint({  
    datcp <- ncol(filedata2())
    cat(paste("Predict cols: ", datcp))         #can make the colour dynamic by making a function if
  })  
#################################
  #2. SETUP - histogram of veg group frequency
#################################    
  
#produces a histgram
  output$hist <- renderPlot({
    dat <- filedata()
    hist(dat$MVG, breaks = input$bins)                #NEEDS WORK - make the hist do MVG or MVS
  })

  
#################################    
  #3.0 RESULTS - calling rf functions and then display output
  #note: to see other output look in Data checking (No. 5)
#################################      
  
#function TRAINING - subsample down by TASVEG$VEGCODE_D

TasVeg_subsample2 <- reactive({
  NVIS_Eucs.raw <- filedata() 
  Data_size.sub <- Data_size.sub
  NVIS_Eucs <- TasVeg_subsample(NVIS_Eucs.raw, Data_size.sub)
  })  
  
###################################################
#3.1 function Training - create binary dataframe from rows
#########

  freq_table <- reactive({
  dat <- TasVeg_subsample2()
  Veg_data <- dat$VEGCODE_D
  prefixd <- prefixdsp1
  VEGCODE_D1 <- Binary_df(Veg_data, prefixd)
  VEGCODE_D1
})

###################################################                                       ###################################################
  #3.2 Random forests training
  #########

fit_tree <- reactive({
    withProgress(message = 'Fitting Random Forest model. This may take a few moments ...', value = 0.5, {
      #Setting up the data
      dat <- TasVeg_subsample2()
      matrix <- freq_table()
      NVIS_ID <- dat$NVIS_ID
      Veg_group <- dat$MVG                                                                 #Need to change for dynamic MVG/MVS
      train <- data.frame(matrix, Veg_group)
    
      #training model
      RF_train <- randomForest(as.factor(train$Veg_group) ~ ., ntree=as.numeric(input$ntree), data=train)
      RF_train                                                                                   #returns 'fit'

    })
  })   
  
###################################################                                                          ###################################################  
  #3.3 function PREDICT - create binary dataframe from rows
  #########
  
  freq_tableP <- reactive({
    withProgress(message = 'changing to binary Random Forest model. This may take a few moments ...', value = 0.5, {
      dat2 <- filedata2()
      Veg_data2 <- dat2$VEGCODE_D
      prefixd <- prefixdsp1
  
      VEGCODE_D2 <- Binary_df(Veg_data2, prefixd)
      VEGCODE_D2
    })})

  ###################################################
  #3.4 FINAL PREDICTION FROM THE TRAINING DATA 
  #########
  
  PredictRF <- reactive({
    #PREDICT 
    VEGCODE_pred <- freq_tableP()  #pred binary table
    Nvis_pred <- data.frame(VEGCODE_pred)  #pred as df
    dat2 <- filedata2()         #pred input data
    #PREDICT creates final dataframe for analaysis
    Nvis_analysis.all <- data.frame(VEGCODE_pred, dat2$NVIS_ID, dat2$MAPUNT_IDE)
    
    #TRAINING
    NVIS_Eucs2.raw <- filedata()
    dat <- TasVeg_subsample2()
    matrix <- freq_table()
    NVIS_ID <- dat$NVIS_ID
    Veg_group <- dat$MVG                                                                 #Need to change for dynamic MVG/MVS
    #TRAINING creates final dataframe for analaysis
    train <- data.frame(matrix, Veg_group)  
    
    train.Nvis_pred.subset <- train[,which(names(train) %in% names(Nvis_pred))]
    train.Nvis_pred.subset.p <- cbind(train.Nvis_pred.subset, Veg_group)
    
    # Fit model to train.Nvis_pred.subset, use model to predict to Nvis_pred
    names(train.Nvis_pred.subset.p) <- make.names(names(train.Nvis_pred.subset.p))
    names(Nvis_pred) <- make.names(names(Nvis_pred))
    Nvis_pred <- data.frame(Nvis_analysis.all)                    
    attach(Nvis_pred)
    
    train.NVIS <- randomForest(as.factor(Veg_group) ~ ., ntree=as.numeric(input$ntree), data=train.Nvis_pred.subset.p)
    #train.NVIS
    
    P.class <- predict(train.NVIS, newdata = VEGCODE_pred,type="class")
    P.prob <- predict(train.NVIS, newdata = VEGCODE_pred,type="prob")
    
    
    #put output into a dataframe
    p.class.nvis <- data.frame(P.class, dat2$NVIS_ID, dat2$MAPUNT_IDE)
    p.class.nvis <- unique(p.class.nvis)
    colnames(p.class.nvis) <- c("MVG", "NVIS_ID", "MAPUNT_IDE")
    
    #creating df with TasVeg names to join up to veg group prediction
    Predict_NVIS_Eucs2 <- data.frame(dat2$VEGCODE_D, dat2$NVIS_ID, dat2$MAPUNT_IDE)
    colnames(Predict_NVIS_Eucs2) <- c("TasVEG", "NVIS_ID", "MAPUNT")
    Predict_NVIS_Eucs3 <- unique(Predict_NVIS_Eucs2)
    
    p.class.nvis2 <- merge.data.frame(p.class.nvis, Predict_NVIS_Eucs3, by.x = "NVIS_ID", by.y = "NVIS_ID")
    p.class.nvis3 <- data.frame(p.class.nvis2$NVIS_ID, p.class.nvis2$MVG, p.class.nvis2$TasVEG)
    colnames(p.class.nvis3) <- c( "NVIS_ID", "MVG_predict", "TasVEG")
    
    ##################
    #checking against training/test data, pull out nvis_id and MVG
    NVIS_Eucs.raw2 <- data.frame(NVIS_Eucs2.raw$NVIS_ID, NVIS_Eucs2.raw$MVG)
    NVIS_Eucs.raw3 <- unique(NVIS_Eucs.raw2)
    colnames(NVIS_Eucs.raw3) <- c( "NVIS_ID", "MVG_final")
    
    p.class.nvis5 <- merge.data.frame(p.class.nvis2, NVIS_Eucs.raw3, by.x = "NVIS_ID", by.y = "NVIS_ID")
    colnames(p.class.nvis5) <- c( "NVIS_ID", "MVG_predict", "MAPUNT_IDE", "TasVEG", "MAPUNT_IDE", "MVG_final")

    #pull out what is of interest - no double ups
    p.class.nvis6 <- data.frame(p.class.nvis5$NVIS_ID, p.class.nvis5$MVG_predict, p.class.nvis5$TasVEG)
    colnames(p.class.nvis6) <- c( "NVIS_ID", "MVG_predict", "TasVeg")
    p.class.nvis6[order(p.class.nvis5$MVG_predict),]
    
  })

  ###################################################
  #3.5 OUTPUT FINAL TABLE 
  #########
    
  #table from results                                         ################ RESULTS
  output$RF_pred <- renderTable({
    df.table2 <- (PredictRF())
    if(is.null(df.table2)) 
      return(NULL)
    df.table2
  })

  ###################################################
  #4, DOWNLOAD data
  ###################################################  
  
  # Downloadable csv of selected dataset ----
  output$downloadData <- downloadHandler(
    filename = function() {
      paste(input$dataset, ".csv", sep = ",")
    },
    content = function(file) {
      write.csv(PredictRF(), file, row.names = FALSE)
    })
  
###################################################
  #5. Data checking
###################################################  
  ##########
  ## processing outputs to know what is happening

##########  
  #5.0 No rows and columns of of the training data
  output$RFsummary <- renderPrint({
    h3("Summary stats")
    Nrow_TasVegS <- nrow(freq_table())
    cat(paste("(T) Number of rows in sub divided data:", Nrow_TasVegS, "\n"))
    
    Ncol_TasVegSub <- ncol(freq_table())
    cat(paste("(T) Number of columns in RF maxtrix:", Ncol_TasVegSub, "\n"))
  })
  
##########
#5.4 print out the results: Variable importance ranking
    
    #No of splits
    output$rf_out <- renderPrint({
      fit <- fit_tree()
      cat(paste("No. of variables tried at each split:", fit$mtry, "\n"))
    })

      #output plot
    output$var_imp <- renderPlot({
      fit <- fit_tree()
      randomForest::varImpPlot(fit, main = "Variable Importance Ranking", pch = 16, col = "blue", sort = T, n.var = 50)
    })

#TABS ###################################
#5.2 (T) Raw input data headings
  output$TASvegS <- renderTable({
    df.table1 <- colnames(TasVeg_subsample2())
    df.table1
  })   
    
#5.3 (T) binary sp output colnames
    output$BinaryNames <- renderTable({
      df.table2 <- colnames(freq_table())  #calling the function to get the output
      df.table2
    })    
    
#5.6 (P) Raw input data headings
    output$TASvegP <- renderTable({
      df.table1 <- colnames(filedata2())
      df.table1
    })  
    
#5.7 (P) PREDICT binary sp output colnames
    output$BinaryNamesP <- renderTable({
      df.table3 <- colnames(freq_tableP())  #calling the function to get the output
      df.table3
    }) 

#training matrix                                              #needs more work........put in matrix ideally RF.confusion_table <- table(predict(train.rf,test,type="class"),test$NVIS_Eucs.MVG_FINAL)  
# output$Tmatrix <- renderPlot({
#    fit <- fit_tree()
#   randomForest::varImpPlot(fit)  #calling the function to get the output
#  })
##########


}) 


