####################################################################################################################################
##### Author: Anna Schritz                                                                                                  ########
##### Creation Date: 14/05/2018                                                                                             ########
##### Last updated: 14/01/2020                                                                                              ########
##### File Name: FunctionDescriptiveStatisticsTable.R                                                                       ########
##### Path: P:\UsefulRSASFunctions                                                                                             ########
##### Purpose: Function that creates common table for continuous and categorised/binary variables for total population and  ########
#####          stratified by a grouping variable with only 2 categories (so far)                                                                            ########
##### Reason for update: include IQR; levels names of categorised variables as variable names in descrBy2; 
#####                    statistical test to compare between groups in descrBy2()
####################################################################################################################################
Sys.setenv(LANG = "en")

library(psych)


####################################################################################################################################
#### function to get summary descriptive statistics for continuous and categorical variables ####

descr <- function(var, name=var, database, print = TRUE){
  # var (character): name of variable
  # name (character): english name of variable
  # database: name of dataset
  # print (logical): print variable name per iteration

  database = as.data.frame(database)
  
  # for continuous variables
  if(is.numeric(database[,var]) & all(is.na(database[,var])) == FALSE){
    if(print == TRUE){
      print(var)
    }


    mat <- describe(database[,var], IQR = TRUE)[,c(2,3,4,5,14,8,9)]

    row.names(mat) = name
    colnames(mat) = c("N", "Mean/Perc", "SD", "Median", "IQR", "Min.", "Max.")

    tot <- mat

  }

  ##################################################################################################################################
  #### for categorical or binary variables ####
  if((is.factor(database[,var]) | is.character(database[,var])) & all(is.na(database[,var])) == FALSE){
    if(print == TRUE){
      print(var)
    }

    nb <- table(database[,var])

    perc <- prop.table(table(database[,var]))


    nb3 <- matrix(nb, ncol = 1)
    perc3 <- matrix(perc, ncol = 1)

    tot <- cbind("N" = nb3[,1], "Mean/Perc" = perc3[,1], "SD" = NA, "Median" = NA, "IQR" = NA, "Min." = NA, "Max." = NA)
    row.names(tot) <- levels(database[,var])

    head <- matrix(data = NA, nrow = 1, ncol = 7)
    row.names(head) <- name

    tot <- rbind(head, tot)
  }
  return(tot)
}



####################################################################################################################################
#### function to get summary descriptive statistics for continuous and categorical variables ####
#### stratified by grouping variable with 2 categories
descrBy2 <- function(var, name=var, catVar, database, contTest = "wilcox", print = TRUE){
  # var (character): name of variable
  # name (character): english name of variable
  # catVar (character): categorical variable to define subgroups
  # database: name of dataset
  # contTest (character): "wilcox" if two-sample Wilcoxon test (non-parametic) should be used or
  #                       "ttest" if Student's t-test should be used (parametric)
  # print (logical): print variable name per iteration
  
  database = as.data.frame(database)
  
  ################################
  groups <- length(levels(database[,catVar]))
  gnames <- levels(database[,catVar])
  #################################
  
  
  ##################################################################################################################################
  #### for continuous variables #####
  if(is.numeric(database[,var])){
    if(print == TRUE){
      print(var)
    }
    
    #print(describeBy(database[,var], group = database[,catVar], digits= 2, mat = TRUE)[c(4,5,15,7,10,11)])
    
    mat <- describeBy(database[,var], group = database[,catVar], digits= 2, mat = TRUE, IQR = TRUE)[c(4,5,6,7,16,10,11)]
    
    
    if(contTest == "wilcox"){
      # wilcoxon rank sum test to compare between two groups
      pval_cont <- wilcox.test(database[,var] ~ database[,catVar])$p.value
    }else if(contTest == "ttest"){
      pval_cont <- t.test(database[,var] ~ database[,catVar])$p.value
    }

    

    
    
    
    row <- cbind(mat[1,], mat[2,], pval_cont)
    colnames(row) <- c(paste("N", gnames[1]), paste("Mean/Perc", gnames[1]), paste("SD", gnames[1]),
                       paste("Median", gnames[1]), paste("IQR", gnames[1]), paste("Min", gnames[1]),
                       paste("Max", gnames[1]),
                       paste("N", gnames[2]), paste("Mean/Perc", gnames[2]), paste("SD", gnames[2]),
                       paste("Median", gnames[2]), paste("IQR", gnames[2]), paste("Min", gnames[2]),
                       paste("Max", gnames[2])
                       ,"p-value"
    )
    row.names(row) <- c(name)
    
    
    
    
    
    return(row)
  }
  
  ##################################################################################################################################
  #### for categorical or binary variables ####
  if(is.factor(database[,var]) | is.character(database[,var])){
    if(print == TRUE){
      print(var)
    }
    
    nb <- table(database[,var], database[,catVar])
    
    perc <- prop.table(table(database[,var], database[,catVar]),2)
    
    
    if(length(levels(database[,var])) > 1){
      chi <- chisq.test(database[,var], database[,catVar])$p.value
    }else{
      chi <- NA
    }
    
    
    nb3 <- matrix(nb, ncol = 2)
    perc3 <- matrix(perc, ncol = 2)
    tot <- cbind("n_g1" = nb3[,1], "mean_perc_g1" = perc3[,1],"sd_g1" =NA, "median_g1"=NA, "IQR_g1" = NA, "min_g1"=NA, "max_g1"=NA,
                 "n_g2" = nb3[,2], "mean_perc_g2" = perc3[,2], "sd_g2" =NA, "median_g2"=NA, "IQR_g2" = NA, "min_g2"=NA, "max_g2"=NA
                 ,"p-value" = NA
    )
    row.names(tot) <- levels(database[,var])
    
    head <- matrix(data = NA, nrow = 1, ncol = 15)
    row.names(head) <- name
    head[1,15] <- chi
    
    
    
    catBy_summary <- rbind(head, tot)
    
    colnames(catBy_summary) <- c(paste("N", gnames[1]), paste("Mean/Perc", gnames[1]), paste("SD", gnames[1]),
                                 paste("Median", gnames[1]), paste("IQR", gnames[1]), paste("Min", gnames[1]),
                                 paste("Max", gnames[1]),
                                 paste("N", gnames[2]), paste("Mean/Perc", gnames[2]), paste("SD", gnames[2]),
                                 paste("Median", gnames[2]), paste("IQR", gnames[2]), paste("Min", gnames[2]),
                                 paste("Max", gnames[2]),
                                 "p-value")
    
    return(catBy_summary)
  }
}




####################################################################################################################################
