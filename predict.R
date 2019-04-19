
loadFitWithEMp <- function(h1bData){
  if(file.exists("./predictionData/fitE.Rds")){
    print("Fitness model with employer found")
    fitE <<- readRDS("./predictionData/fitE.Rds")
    print("FInished loading fitness model with employer")
  }
  else{
    print("Generating model for fitness with employer")
    fitE <<- Fitting_w_Emp(h1b)
    print("Finished generating model")
    saveFittingModel(fitE,"fitE")
    print("Saving generated model")
    print("Saved.")
  }
}

loadFitNoEMp <- function(h1bData){
  if(file.exists("./predictionData/fit.Rds")){
    print("Fitness model without employer found")
    fit <<- readRDS("./predictionData/fit.Rds")
    print("Finished loading fitness model without employer")
  }
  else{
    print("Generating model for fitness without employer")
    fit <<- Fitting_wo_Emp(h1b)
    print("Finished generating model")
    saveFittingModel(fit,"fit")
    print("Saving generated model")
    print("Saved.")
  }
}

saveFittingModel <- function(fitModel , name){
  filelOC <- paste0("./predictionData/",name,".Rds")
  saveRDS(fitModel,filelOC)
}


# 
Fitting_w_Emp <- function(H1data){ 
  h1data <- H1data
  names(h1data) <- c("ID",
                   "Case_Status",
                   "Employer_Name",
                   "SOC_Code",
                   "Job_Title",
                   "Full_Time",
                   "Wage_Year",
                   "Year",
                   "Longitude",
                   "City",
                   "State",
                   "Latitude")
  
  dataNew <-  h1data[,c(2,3,4,7,11)]
  
  dataNew <-  h1data[,c(2,3,4,7,11)] %>%
    mutate(Case_Status=ifelse(
      Case_Status %in% c("CERTIFIED"),1,0)) %>%
    filter(!is.na('Wage_Year'))
  
  
  fit <-  rpart(Case_Status~.,data = dataNew)
  return(fit)

}




Fitting_wo_Emp <- function(h1data){ 
  names(h1data) <- c("ID",
                   "Case_Status",
                   "Employer_Name",
                   "SOC_Code",
                   "Job_Title",
                   "Full_Time",
                   "Wage_Year",
                   "Year",
                   "Longitude",
                   "City",
                   "State",
                   "Latitude")

  dataNew <-  h1data[,c(2,4,7,11)] %>%
    mutate(Case_Status=ifelse(
      Case_Status %in% c("CERTIFIED"),1,0)) %>%
    filter(!is.na('Wage_Year'))
  
  
  fit <-  rpart(Case_Status~.,data = dataNew)
  return(fit)

}

predict_chances <- function(emp=NULL,soc,wage,state){
  if(is.null(emp) | emp =="NA"){
    return(Pred_wo_Emp(soc,wage,state))
  }else{
    return(Pred_w_Emp(emp,soc,wage,state))
  }
}

Pred_w_Emp <- function(emp,soc,wage,state){#Fn for prediction WITH Employer
  # cat("in pred w emp")
  df <- data.frame("Employer_Name"=emp,"SOC_Code"=soc,"Wage_Year"=wage,"State"=state)
  prediction <- predict(fitE,df)
  cat("There is a ",prediction*100,"% chances of your Visa getting Approved")
  return(prediction*100)
}
# 
# 
Pred_wo_Emp <- function(soc,wage,state){#Fn for prediction WITHOUT Employer
  # cat("in pred wo emp")
  df <- data.frame("SOC_Code"=soc,"Wage_Year"=wage,"State"=state)
  prediction <- predict(fit,df)
  cat("There is a ",prediction*100,"% chances of your Visa getting Approved")
  return(prediction*100)
}


