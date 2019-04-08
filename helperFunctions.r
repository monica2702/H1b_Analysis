loadData <- function(){
  if(file.exists("./cleanedData/h1bData.Rds")){
    print("Cleaned file exists")
    h1b<<- readRDS("./cleanedData/h1bData.Rds")
  }
  else{
    print("Clean file does not exist, reading data file..")
    h1b<<- read_csv("h1b_kaggle.csv")
    h1b<<- cleanData(h1b)
    print("Saving cleaned file")
    saveRDS(h1b,file="./cleanedData/h1bData.Rds")
    print("Save cleaned file")
  }
  
  
}
initData <-function(){
  emp <<- unique(h1b$EMPLOYER_NAME)
  yearwise <<- data.frame(h1b$YEAR)
  caseStatus <<- unique(h1b$CASE_STATUS)
  jobSoc <<- unique(h1b$SOC_NAME)
  # h1bSplit <- separate(h1b, WORKSITE, c("WORKSITE", "STATES"), 
  #                 sep = ", ", remove = FALSE)
  # h1bSplit <- h1bSplit %>% filter(STATES != "NA")
  stateList <<- unique(h1b$STATES)
  employerNames <<- unique(h1b$EMPLOYER_NAME)
  minH1bSal <<- min(h1b$PREVAILING_WAGE, na.rm = T)
  maxH1bSal<<- max(h1b$PREVAILING_WAGE, na.rm = T)
}

computeData <- function(h1data, h1status=caseStatus, empName=NULL, empSalMin=minH1bSal, empSalMax=maxH1bSal){
  if(is.null(empName)){
    return(filter(h1data, CASE_STATUS %in% c(h1status)
                  & PREVAILING_WAGE >= empSalMin 
                  & PREVAILING_WAGE<= empSalMax  ))  
  }
  else{
    return(filter(h1data, CASE_STATUS %in% c(h1status)
                  & EMPLOYER_NAME== empName 
                  & PREVAILING_WAGE >= empSalMin 
                  & PREVAILING_WAGE<= empSalMax  ))  
  }
  
  
}

plotData <- function(h1data, h1status=caseStatus, empName=null, empSalMin=minH1bSal, empSalMax=maxH1bSal){
  filteredData <- computeData(h1data,h1status,empName,empSalMin, empSalMax)
  return(table(filteredData$YEAR))
}

jobPieData <- function(h1Data,byYear = NULL){
  result <- h1Data
  if(!is.null(byYear)){
    result <- filter(h1Data,YEAR == byYear)
  }
  # cat("Result size: ", dim(result))
  return(table(result$SOC_NAME))
}

dataForUIFrame <- function(h1Data,byYear=NULL){
  result <- h1Data
  # cat("resultSize :", dim(result))
  if(!is.null(byYear)){
    result <- filter(h1Data,YEAR == byYear)
  }
  result <- result[c("CASE_STATUS","SOC_NAME","JOB_TITLE","PREVAILING_WAGE","WORKSITE")]
  return(result)
  
}

h1bPlot <- function(h1bData= h1b,status =caseStatus,employers=NULL,minSal=minH1bSal,maxSal=maxH1bSal){
  return (barplot(plotData(h1b,status,employer,minSal, maxSal)))
}

saveData <- function(data) {
  data <- t(data)
  # Create a unique file name
  fileName <- "h1bData.csv"
  # Write the file to the local system
  write.csv(
    x = data,
    file = file.path("cleanedData", fileName), 
    row.names = FALSE, quote = TRUE
  )
}

cleanData <- function(dataToClean){
  cat("Cleaning data")
  dataToClean$SOC_NAME <- str_to_upper(dataToClean$SOC_NAME)
  cat(".")
  dataToClean$SOC_NAME <- gsub("^[0-9].*", "NA", dataToClean$SOC_NAME)
  cat(".")
  dataToClean$SOC_NAME <- gsub(".*ANALYST.*|.*ANALYSIS.*|.*ANALSYSTS.*|.*ANAL.*", 
                               "ANALYST", dataToClean$SOC_NAME)
  cat(".")
  dataToClean$SOC_NAME <- gsub(".*TEACH.*|.*EDUCATION.*", "EDUCATION", dataToClean$SOC_NAME)
  cat(".")
  dataToClean$SOC_NAME <- gsub(".*BIO.*", "BIO-RELATED SOCs", dataToClean$SOC_NAME)
  cat(".")
  dataToClean$SOC_NAME <- gsub(".*CIVIL.*", "CIVIL ENGG./PLANNERS", dataToClean$SOC_NAME)
  cat(".")
  dataToClean$SOC_NAME <- gsub(".*RESEARCH.*", "RESEARCHERS", dataToClean$SOC_NAME)
  cat(".")
  dataToClean$SOC_NAME <- gsub(".*SCIENTIST.*", "SCIENTISTS", dataToClean$SOC_NAME)
  cat(".")
  dataToClean$SOC_NAME <- gsub(".*MANAGER.*", "MANAGER", dataToClean$SOC_NAME)
  cat(".")
  dataToClean$SOC_NAME <- gsub(".*INDUSTRIAL.*", "INDUSTRIAL RELATED", dataToClean$SOC_NAME)
  cat(".")
  dataToClean$SOC_NAME <- gsub(".*TECHNICIAN.*|.*OPERATOR.*|.*MECHANIC.*", 
                               "TECHNICIANS", dataToClean$SOC_NAME)
  cat(".")
  dataToClean$SOC_NAME <- gsub(".*ART.*", "ART & RELATED", dataToClean$SOC_NAME)
  cat(".")
  dataToClean$SOC_NAME <- gsub(".*AUTOMOTIVE.*", "AUTOMOTIVE INDUSTRY", dataToClean$SOC_NAME)
  cat(".")
  dataToClean$SOC_NAME <- gsub(".*SUPERVISOR.*", "SUPERVISORS", dataToClean$SOC_NAME)
  cat(".")
  dataToClean$SOC_NAME <- gsub("^ACCOUNTANT.*|^BILL.*", "ACCOUNTANT", dataToClean$SOC_NAME)
  cat(".")
  dataToClean$SOC_NAME <- gsub("^AGRICULTURAL.*", "AGRICULTURE", dataToClean$SOC_NAME)
  cat(".")
  dataToClean$SOC_NAME <- gsub("^APPLICATION.*|.*PROGRAMMERS.*|.*PROGRAMMER.*|
                               .*PROGRAMERS.*|.*PROGRAMER.*|.*PROGRAMMGER.*|
                               .*POGRAMMERS.*|.*POGRAMMERS.*|.*SOFTWARE.*|
                               .*DEVELOPER.*", 
                       "SOFTWARE ENGINEER", dataToClean$SOC_NAME)
  cat(".")
  dataToClean$SOC_NAME <- gsub(".*DATA.*|.*DATEBASE.*", "DATA RELATED", dataToClean$SOC_NAME)
  cat(".")
  dataToClean$SOC_NAME <- gsub(".*COMPUTER.*", "COMPUTER OCCUPATIONS", dataToClean$SOC_NAME)
  cat(".")
  dataToClean$SOC_NAME <- gsub(".*SALE.*", "SALES REPRESENTATIVE", dataToClean$SOC_NAME)
  cat(".")
  dataToClean$SOC_NAME <- gsub(".*LAW.*|.*ATTORNEY.*", "LAW & ORDER", dataToClean$SOC_NAME)
  cat(".")
  dataToClean$SOC_NAME <- gsub(".*ELECTRIC.*|.*HEATING.*|.*ELETRICAL.*", 
                       "ELECTRICAL OCCUPATIONS", dataToClean$SOC_NAME)
  cat(".")
  dataToClean$SOC_NAME <- gsub(".*PLANNER.*", "PLANNERS", dataToClean$SOC_NAME)
  cat(".")
  dataToClean$SOC_NAME <- gsub(".*NURSE.*|.*NURSING.*", "NURSING", dataToClean$SOC_NAME)
  cat(".")
  dataToClean$SOC_NAME <- gsub(".*PHYSICIAN.*|.*NEUROLOGISTS.*|.*SURGEON.*|
                               .*PRACTIONER.*|.*PRACTICIONER.*|.*PRACTITIONER.*",
                       "HEALTHCARE PRACTIONER", dataToClean$SOC_NAME)
  cat(".")
  dataToClean$SOC_NAME <- gsub(".*COUNSEL.*|.*THERAP.*", "THERAPIST", dataToClean$SOC_NAME)
  cat(".")
  dataToClean$SOC_NAME <- gsub(".*DENTAL.*|.*DENTIST.*", "DENTAL PRACTIONER", dataToClean$SOC_NAME)
  cat(".")
  dataToClean$SOC_NAME <- gsub(".*COOK.*|.*CHEF.*|.*FOOD.*","FOOD INDUSTRY",dataToClean$SOC_NAME)
  cat(".")
  dataToClean$SOC_NAME <- gsub(".*AUDIO.*|.*VIDEO.*|.*MEDIA.*",
                       "MULTIMEDIA", dataToClean$SOC_NAME)
  dataToClean <- separate(dataToClean, WORKSITE, c("WORKSITE", "STATES"), 
                  sep = ", ", remove = FALSE)
  
  print("done cleaning")
  return(dataToClean)
  
}

