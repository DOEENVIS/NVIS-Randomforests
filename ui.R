library(shiny)
library(shinydashboard)

shinyUI(
  dashboardPage(title = "NVIS Random Forests", skin = "green",
                dashboardHeader(title = "NVIS Random Forests" 
                ),
                
                #### SIDE BAR
                dashboardSidebar(
                  sidebarMenu(
                    sidebarSearchForm("searchText", "buttonSearch", "Search"),                             #puts in a search tool button
                    h4("Model Setup and Run"),
                    menuItem("Input data",    tabName = "dashboard"),             #Dashboard links up with the tabItems below, icon put in little pic (https://fontawesome.com/icons?d=gallery)
                    menuItem("Setup", tabName = "Setup"),
                    menuItem("Results", tabName = "Results"),
                    menuItem("Download Ouputs", tabName = "Download"),
                    menuItem("Data checking", tabName = "Pprocess")
                  )),

############
                #1. MAIN - INPUT PAGE
                dashboardBody(
                  tabItems(
                    tabItem(tabName = "dashboard",                                                        #defines what will happen under the defined tab
                            fluidRow(
                              #Basic info boxes to show the training data went in
                              column(width = 6,                                                           #column width
                                     infoBox(h5(textOutput("Nrows")), icon = icon("align-justify"), textOutput("Tree_type")),                       #Tree_type put in tabs in the pain with values
                                     infoBox(h5(textOutput("Crows")), icon = icon("asterisk")))),
                            fluidRow(
                              #Basic info boxes to show the training data went in
                              column(width = 6,                            
                                     infoBox(h5(textOutput("Prows")), icon = icon("align-justify")),
                                     infoBox(h5(textOutput("Pcols")), icon = icon("asterisk")))),

                            fluidRow(
                              #training data input
                              tabBox(
                              tabItem(tabName = "Data Input", 
                                      h3("Read in TRAINING data"), 
                                      p("Please upload a CSV formatted file with your data."),
                                      p("Headings need to be: NVIS_ID, MVG/MVS, and VEGCODE_D"),
                                                  fileInput("file1", label = "Choose CSV File",
                                                             accept = c(
                                                               "text/csv",
                                                               "text/comma-separated-values,text/plain",
                                                               ".csv")))),
                              
                              #predict data input
                              tabBox(
                                tabItem(tabName = "Data Input", 
                                        h3("Read in PREDICT data"), 
                                        p("Please upload a CSV formatted file with your data."),
                                        p("Headings need to be: NVIS_ID, MVG/MVS, and VEGCODE_D"),
                                        fileInput("file2", label = "Choose CSV File",
                                                  accept = c(
                                                    "text/csv",
                                                    "text/comma-separated-values,text/plain",
                                                    ".csv")))

                  ))),
#########                
                  #2. SETUP - VARIABLE TO CHANGE FOR MODEL     
                  tabItem(tabName = "Setup",
                      h1("Setting up the data"),
                      mainPanel(
                        #Select if you are doing MVG or MVS
                        selectInput("y", "Response:", choices = c("MVG", "MVS")),
                        #choose the number of rf runs
                        radioButtons("ntree", "No. of Random Forest Trees:", c("10 (test)" = "10",
                                                                               "100" = "100",
                                                                               "500" = "500",
                                                                               "1000" = "1000")),
                        #histogram of frequency of vegetation groups from training data.
                        tabPanel(title = "Native Species Histogram", status = "primary", solidHeader = T, background = "blue",
                                 sliderInput("bins", "Number of Breaks", 0, 32, 2), 
                                 plotOutput("hist"))
                      )),
 
########                
                #3. RESULTS - outputs tabular results from the RF model
                tabItem(tabName = "Results",
                    h1("Results from Random Forests"),
                    tableOutput("RF_pred")
                    ),


########                
                #4. DOWNLOAD OUTPUTS - download button to download the data
                tabItem(tabName = "Download",
                        h1("Download Random Forests Outputs"),
                        helpText(" Select the download format"),
                        
                        #radioButtons("type", "Formate type:", 
                        #             choices = c("Excel (CSV)", "text (TSV)", "Text (Space Separated)", "Doc")), 
                        br(), 
                        h5(" Click on the download button to download the model output"),
                        downloadButton("downloadData", "Download")    
                
                ),
########                
                #5. DATA CHECKING: some outputs so you can look at the data
                tabItem(tabName = "Pprocess",
                        h1("Pre processing output"),
                        h3("Summary statistics"),
                        h5("(T) Training data;   (P) Predition data"),
                        textOutput("RFsummary", container = pre),
                        
                        mainPanel(
                            tabsetPanel(type="tab",
                                        tabPanel("(T) data table", tableOutput("train_data")),#1
                                        tabPanel("(T) Raw input data headings", tableOutput("TASvegS")),#2
                                        tabPanel("(T) RF matrix headings", tableOutput("BinaryNames")),#3
                                        tabPanel("(T) results", #4
                                                 h1("Results from Random Forests for:", textOutput("response")),
                                                 textOutput("rf_out", container = pre),
                                                 plotOutput("var_imp", width = "1000px", height = "700px")),
                                        tabPanel("(P) data table", tableOutput("predict_data")),#5
                                        tabPanel("(P) Raw input data headings", tableOutput("TASvegP")),#6
                                        tabPanel("(P) RF matrix headings", tableOutput("BinaryNamesP"))#7
)))))))


