######################################################################################################################################
############################################################# Lasso function #########################################################
######################################################################################################################################

lasso_fun <- function(data, outcome, nboot=1000){
  ### data    = name of dataset. dataset should includ only all independant variables and the dependent variable (in the last column)
  ### outcome = name of outcome variable (as character)
  data1=data
  data1=data1[rowSums(is.na(data1))==0,] # dataset without missing values
  y=data1[,outcome]
  resultat=NULL
  for(m in 1:nboot){  #for each bootstrap sample      
    print(c(m/nboot))
    ind=sample(nrow(data1),nrow(data1),T) # get indices of bootstrap sample which has same dimension as original datset
    databoot=data1[ind,] # create bootstrap sample using the indices ind
    yboot=y[ind] # vector of order changed variable outcome
    xboot=model.matrix(as.formula(paste0(outcome, "~.")),data=databoot) # variables need to be in matrix format (categorical variables are transformed to dummy/ binary variables)
    xboot=xboot[,-1]
    # standardize = TRUE is default!
    glmboot=cv.glmnet(x=xboot,y=yboot,alpha=1,family="binomial", standardize = TRUE) # cross validation to select lambda. alpha=1 is the lasso penalty
    # family = "gaussian" for quantitative outcomes
    coefboot=coef(glmboot,s="lambda.1se") #extracts model coefficients from objects returned by modeling functions
    resultat=rbind(resultat,as.vector(t(coefboot)[-1])) # summarise results
  }
  
  #Percentage of apparitions and means of coefficients on 1000 iterations
  percent=NULL
  mean=NULL
  
  for (j in 1:(dim(resultat)[2])) {
    compteur=sum(resultat[,j]!=0)
    moyenne=mean(resultat[which(resultat[,j]!=0),j])
    mean=cbind(mean,moyenne)
    percent=cbind(percent,compteur*100/nboot)                               
  }
  resultat2=rbind(mean,resultat)
  resultat2=rbind(percent,resultat2)
  resultat_fin=data.frame(resultat2)
  
  # names of covariables that will be investigated 
  names(resultat_fin)= colnames(xboot)
  resultat_fin[1:2,]
  
  return(resultat_fin)
}

##################################################################################################################################################
##################################################################################################################################################
######################################################### Apply lasso function ###################################################################
##################################################################################################################################################
# input the name of the dataset and the name (as character) of the outcome variable
#### preselected crude data ####

lasso1 <- lasso_fun(data = lungWide_preSel, outcome = "Type", nboot= 100000)

head(lasso1)
ind <- which(lasso1[1,] > 80) # select variables that appeared more than 80% in all bootstrap samples
colnames(lasso1)[ind]


# most selected model
model1 <- data.frame("boot" = 1:100000, "model" = NA)
for(i in 3:100002){
  nr<- which(lasso1[i,] != 0)
  nr_char1 <- paste0(nr, collapse = "-")
  model1$model[i-2] <- nr_char1
}

table1 <-  data.frame(table(model1$model))
table(table1$Freq)

rownr1 <- which(table1$Freq == 2)
table1$Var1[rownr1]