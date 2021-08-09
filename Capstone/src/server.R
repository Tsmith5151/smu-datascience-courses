library(simstudy)
library(ggplot2)
library(GGally)
library(randomForest)
library(ROCR)
library(caTools)
library(corrplot)
library(gplots)
library(dplyr)

set.seed(1)

linear_eq <- function(n_ev,weights,y_int){
  
  # Explanatory Variables
  ev <- c()
  if(weights == 1) {
    for (i in (1:n_ev)){
      ev[[i]] <- (paste0('0.5*EV',i,sample(c('-','+'),1)))
    }
  } else if(weights == 2) {
    for (i in (1:n_ev)){
      if(i <= n_ev / 2) {
        ev[[i]] <- (paste0('0.3*EV',i,sample(c('-','+'),1)))
      }
      else {
        ev[[i]] <- (paste0('0.7*EV',i,sample(c('-','+'),1)))
      }
    }
  }else {
    for (i in (1:n_ev)){
      if(i <= n_ev / 3) {
        ev[[i]] <- (paste0('0.2*EV',i,sample(c('-','+'),1)))
      }
      else if(i <= 2 * n_ev / 3){
        ev[[i]] <- (paste0('0.5*EV',i,sample(c('-','+'),1)))
      }
      else {
        ev[[i]] <- (paste0('0.8*EV',i,sample(c('-','+'),1)))
      }
    }
  }
  
  # Build Equation
  x <- (paste(unlist(ev), collapse=' '))
  formula <- paste0(y_int,' + ',x)
  return(substring(formula,1,nchar(formula)-1))
  
}

sim_data <- function(n_obs,n_noise,cat,ndist,nvar,n_ev,weights,y_int){
  
  data <- c()
  
  # Noise Variables
  for (i in 1:n_noise){
    data<- defData(data,varname=paste0('N',i), dist=ndist, formula = "0", variance = nvar, link = "identity")
  }
  
  # Explanatory Variables
  for (i in 1:n_ev){
    data<- defData(data,varname=paste0('EV',i), dist="normal", formula = "0", variance = nvar, link = "identity")
  }
  
  #Categorical Variables
  if(cat != 0){
    for (i in 1:cat){
      data<- defData(data,varname=paste0('Cat',i), dist="categorical", formula = "0.3;0.2;0.5")
    }
  }
  
  # Response Variable
  data <- defDataAdd(data,varname="y", dist="binary", formula=linear_eq(n_ev,weights,y_int), link = "logit")
  
  # Build Simulated Data 
  data <- as.data.frame.matrix(genData(n_obs,data))
  
  # Convert Categorical Variables to Factors
  cols =grep('Cat', names(data), value=TRUE) 
  data[cols] <- lapply(data[cols], factor)
  
  return(data[,2:(ncol(data))])
}


# Split Training/Testing Sets
split_data <- function(data, r_split) {
  set.seed(123)
  sample = sample.split(data$y,SplitRatio = r_split)
  TRAIN <<- subset(data,sample ==TRUE)
  TEST <<- subset(data, sample==FALSE)
}

# Random Forest
do_randomforest <- function(train,test,n_tree){
  
  # RF Model
  test$y <- as.factor(test$y)
  model.train <- randomForest(as.factor(y)~.,train,ntree=n_tree,importance=TRUE)
  pred <- predict(model.train, type="prob", newdata=test)[,2]
  pred <- prediction(pred,test$y)
  return(pred)
  
}

# Logistic Regression
do_logisticregression <- function(train,test,varselect){
  
  # Fit Model
  model.train = step(lm(y ~., data = train), direction = varselect)
  pred <- predict(model.train, test[, !names(test) %in% c("y")], type = 'response')
  pred <- prediction(pred, test$y)
  return(pred)
}

# Logistic Regression/ Random Forest performance
get_results_models <- function(model,upper_prob,algorithm){
  
  # TPR and FPR
  perf <- performance(model, "tpr", "fpr",cutoffs=seq(0,1,0.01))
  cutoffs<- data.frame(cut=perf@alpha.values[[1]], fpr=perf@x.values[[1]],tpr=perf@y.values[[1]])
  perf1 <- subset(cutoffs[order(cutoffs$cut, decreasing=TRUE),], (cut < upper_prob))
  perf1 <- perf1[1,]

  
  # Recal Precision
  perf <- performance(model, "rec", "prec")
  cutoffs <- data.frame(cut=perf@alpha.values[[1]], rec=perf@x.values[[1]],prec=perf@y.values[[1]])
  perf2 <- subset(cutoffs[order(cutoffs$cut, decreasing=TRUE),], (cut < upper_prob ))
  perf2 <- perf2[1,]
  
  
  #Accuracy
  perf <- performance(model, measure = "acc")
  cutoffs <- data.frame(cut=perf@x.values[[1]], acc=perf@y.values[[1]])
  perf3 <- subset(cutoffs[order(cutoffs$cut, decreasing=TRUE),], (cut < upper_prob))
  perf3 <- perf3[1,]
  
  
  #AUC
  perf <- performance(model, measure = "auc")
  perf4 <- data.frame(auc=perf@y.values[[1]])
  perf4 <- perf4[1,]
  
  # Combine DataFrames
  df_iter <- merge(perf1,perf2)
  df_iter <- merge(df_iter,perf3)
  df_iter <- merge(df_iter,perf4)
  df_iter$algorithm <- algorithm

  df_iter <- df_iter[,!names(df_iter) %in% c("cut")]
  names(df_iter) <- c("fpr","tpr","rec","prec","acc","auc","algorithm")
  
  return(df_iter)
  
}

# Compute Two Sample TTest
get_ttest_results <- function(df){
  df <- df %>%
    summarise_each(funs(t.test(.[algorithm == "RandomForest"], .[algorithm == "Logistic Regression"],pair = TRUE,conf.int=0.95)$p.value), vars = fpr:auc)
  
  results <- as.data.frame(df)
  names(results) <- c('fpr','tpr','rec','prec','acc','auc')
  df_results <- data.frame(results)
  df_results$metric <- "p-value"
  
  df_results <- format(df_results, digits=3)
  return(df_results)
}

# CASE 1: Run Logistic Regression and Random Forest Simulations sweeping variance of variables
simulation_nvar <- function(n_sim,split,ncat,nrows,noise,ndist,nvar,ev,weights,yint,varselect,ntrees,prob_thresh){
  
  # Initialize dataframe used to hold results
  df_case1_raw <- data.frame(cut = numeric(), fpr = numeric(), tpr = numeric(), rec = numeric(), prec = numeric(), acc = numeric(), auc = numeric(), nvar = numeric(),algorithm=character())
  df_case1_agg <- data.frame(algorithm = character(),cut = numeric(), fpr = numeric(), tpr = numeric(), rec = numeric(), prec = numeric(), acc = numeric(), auc = numeric(), nvar = numeric())
  
  # Iterate over variance 
  for (nvar in seq(from=0.50,to=5.0,by=0.50)){
    
    df_sim_case <- data.frame(fpr = numeric(), tpr = numeric(), rec = numeric(), prec = numeric(), acc = numeric(),algorithm = character())
    
    # Iterate of number of simulations
    for (n in 1:n_sim){
      
      # Regenerate Data
      data<-sim_data(nrows,noise,ncat,ndist,nvar,ev,weights,yint)
      
      # Split Train/Testing
      split_data(data,split)
      
      # Do RandomForest
      rf_pred <- do_randomforest(TRAIN,TEST,ntrees)
     
      # Do Logistic Regression
      lr_pred <- do_logisticregression(TRAIN,TEST,varselect)
      
      # Get Simulation Results --> LR & RF
      df_lr <- get_results_models(lr_pred,prob_thresh,"Logistic Regression")
      df_rf <- get_results_models(rf_pred,prob_thresh,"RandomForest")
      
      #Combine Aggregated DataFrames
      df_iter <- rbind(df_lr,df_rf)
      
      #Append Data
      df_sim_case <- rbind(df_sim_case,df_iter)
    }
    
    # Raw Results
    df_raw <- df_sim_case
    
    # Aggregate (i.e. Mean of Simulation Results)
    df_agg <- aggregate(df_sim_case, by=list(df_sim_case$algorithm), FUN=mean)
    df_agg <- df_agg[, !(colnames(df_agg) %in% c("algorithm"))]
    
    #Rename 
    colnames(df_agg)[colnames(df_agg)=="Group.1"] <- "algorithm"
    
    # Add "Num Variance"
    df_agg$nvar <- nvar
    df_raw$nvar <- nvar
    
    # Merge Results
    df_case1_agg <- rbind(df_case1_agg,df_agg)
    df_case1_raw <- rbind(df_case1_raw,df_raw)
  }
  
  DF_CASE1_RAW <<- df_case1_raw
  
  #Statistical Results
  DF_CASE1_RAW_TTEST <<- get_ttest_results(DF_CASE1_RAW)
  return(df_case1_agg)
}    


# CASE 2: Run Logistic Regression and Random Forest Simulations sweeping number of noise variables
simulation_num_nvar <- function(n_sim,split,ncat,nrows,noise,ndist,nvar,ev,weights,yint,varselect,ntrees,prob_thresh){
  
  # Initialize dataframe used to hold results
  df_case2_raw <- data.frame(cut = numeric(), fpr = numeric(), tpr = numeric(), rec = numeric(), prec = numeric(), acc = numeric(), auc = numeric(), nvar = numeric(),algorithm=character())
  df_case2_agg <- data.frame(algorithm = character(),cut = numeric(), fpr = numeric(), tpr = numeric(), rec = numeric(), prec = numeric(), acc = numeric(), auc = numeric(), nvar = numeric())
  
  # Iterate over number of noise variables 
  for (noise in c(1,5,10,20,50)){
    
    df_sim_case <- data.frame(fpr = numeric(), tpr = numeric(), rec = numeric(), prec = numeric(), acc = numeric(),algorithm = character())
    
    # Number of Simulations
    for (n in 1:n_sim){
      
      # Regenerate Data
      data<-sim_data(nrows,noise,ncat,ndist,nvar,ev,weights,yint)
      
      # Split Train/Testing
      split_data(data,split)
      
      # Do RandomForest
      rf_pred <- do_randomforest(TRAIN,TEST,ntrees)
      
      # Do Logistic Regression
      lr_pred <- do_logisticregression(TRAIN,TEST,varselect)
      
      # Get Simulation Results --> LR & RF
      df_lr <- get_results_models(lr_pred,prob_thresh,"Logistic Regression")
      df_rf <- get_results_models(rf_pred,prob_thresh,"RandomForest")
      
      #Combine Aggregated DataFrames
      df_iter <- rbind(df_lr,df_rf)
      
      #Append Data
      df_sim_case <- rbind(df_sim_case,df_iter)
    }
    
    # Raw Results
    df_raw <- df_sim_case
    
    # Aggregate (i.e. Mean of Simulation Results)
    df_agg <- aggregate(df_sim_case, by=list(df_sim_case$algorithm), FUN=mean)
    df_agg <- df_agg[, !(colnames(df_agg) %in% c("algorithm"))]
    
    #Rename 
    colnames(df_agg)[colnames(df_agg)=="Group.1"] <- "algorithm"
    
    # Add noise number
    df_agg$num_noise <- noise
    df_raw$num_noise <- noise
    
    # Merge Results
    df_case2_agg <- rbind(df_case2_agg,df_agg)
    df_case2_raw <- rbind(df_case2_raw,df_raw)
  }
  
  DF_CASE2_RAW <<- df_case2_raw
  
  
  #Statistical Results
  DF_CASE2_RAW_TTEST <<- get_ttest_results(DF_CASE2_RAW)
  return(df_case2_agg)  
}


# CASE 3: Run Logistic Regression and Random Forest Simulations sweeping num ex. variables 
simulation_num_evar <- function(n_sim,split,ncat,nrows,noise,ndist,nvar,ev,weights,yint,varselect,ntrees,prob_thresh){
  
  # Initialize dataframe used to hold results
  df_case3_raw <- data.frame(cut = numeric(), fpr = numeric(), tpr = numeric(), rec = numeric(), prec = numeric(), acc = numeric(), auc = numeric(), nvar = numeric(),algorithm=character())
  df_case3_agg <- data.frame(algorithm = character(),cut = numeric(), fpr = numeric(), tpr = numeric(), rec = numeric(), prec = numeric(), acc = numeric(), auc = numeric(), nvar = numeric())
  
  # Iterate over number of noise variables 
  for (num_ev in c(1,5,10,20,50)){
    
    df_sim_case <- data.frame(fpr = numeric(), tpr = numeric(), rec = numeric(), prec = numeric(), acc = numeric(),algorithm = character())
    
    # Number of Simulations
    for (n in 1:n_sim){
      
      # Regenerate Data
      data<-sim_data(nrows,noise,ncat,ndist,nvar,num_ev,weights,yint)
      
      # Split Train/Testing
      split_data(data,split)
      
      # Do RandomForest
      rf_pred <- do_randomforest(TRAIN,TEST,ntrees)
      
      # Do Logistic Regression
      lr_pred <- do_logisticregression(TRAIN,TEST,varselect)
      
      # Get Simulation Results --> LR & RF
      df_lr <- get_results_models(lr_pred,prob_thresh,"Logistic Regression")
      df_rf <- get_results_models(rf_pred,prob_thresh,"RandomForest")
      
      #Combine Aggregated DataFrames
      df_iter <- rbind(df_lr,df_rf)
      
      #Append Data
      df_sim_case <- rbind(df_sim_case,df_iter)
    }
    
    # Raw Results
    df_raw <- df_sim_case
    
    # Aggregate (i.e. Mean of Simulation Results)
    df_agg <- aggregate(df_sim_case, by=list(df_sim_case$algorithm), FUN=mean)
    df_agg <- df_agg[, !(colnames(df_agg) %in% c("algorithm"))]
    
    #Rename 
    colnames(df_agg)[colnames(df_agg)=="Group.1"] <- "algorithm"
    
    # Add ev number
    df_agg$num_ev <- num_ev
    df_raw$num_ev <- num_ev
    
    # Merge Results
    df_case3_agg <- rbind(df_case3_agg,df_agg)
    df_case3_raw <- rbind(df_case3_raw,df_raw)
  }
  
  DF_CASE3_RAW <<- df_case3_raw
  
  #Statistical Results
  DF_CASE3_RAW_TTEST <<- get_ttest_results(DF_CASE3_RAW)
  return(df_case3_agg)
}


# CASE 4:  Run Logistic Regression and Random Forest Simulations sweeping num observations and Variables
simulation_num_obs <- function(n_sim,split,ncat,nrows,noise,ndist,nvar,ev,weights,yint,varselect,ntrees,prob_thresh){
  
  # Initialize dataframe used to hold results
  df_case4_raw <- data.frame(cut = numeric(), fpr = numeric(), tpr = numeric(), rec = numeric(), prec = numeric(), acc = numeric(), 
                             auc = numeric(), algorithm=character(),num_ev = numeric(),obs = numeric())
  
  # Iterate over number of observations
  for (nrows in seq(from=100,to=10000,by=100)){
    
    df_agg <- data.frame(fpr = numeric(), tpr = numeric(), rec = numeric(), prec = numeric(), acc = numeric(),algorithm = character(),num_ev = numeric())
    
    # Number of EV
    for (num_ev in c(1,10,20,50)){
      
      df_sim_case <- data.frame(fpr = numeric(), tpr = numeric(), rec = numeric(), prec = numeric(), acc = numeric(),algorithm = character(),num_ev = numeric())

      for (n in 1:n_sim){
        # Regenerate Data
        data<-sim_data(nrows,noise,ncat,ndist,nvar,num_ev,weights,yint)
      
        # Split Train/Testing
        split_data(data,split)
        
        # Do RandomForest
        rf_pred <- do_randomforest(TRAIN,TEST,ntrees)
        
        # Do Logistic Regression
        lr_pred <- do_logisticregression(TRAIN,TEST,varselect)
        
        # Get Simulation Results --> LR & RF
        df_lr <- get_results_models(lr_pred,prob_thresh,"Logistic Regression")
        df_rf <- get_results_models(rf_pred,prob_thresh,"RandomForest")
        
        #Combine Aggregated DataFrames
        df_iter <- rbind(df_lr,df_rf)
        df_iter$num_ev <- num_ev
        
        #Append Data
        df_sim_case <- rbind(df_sim_case,df_iter)
      }
      
      # Aggregate (i.e. Mean of Simulation Results)
      df_sim_case <- aggregate(df_sim_case, by=list(df_sim_case$algorithm), FUN=mean)
      df_sim_case <- df_sim_case[, !(colnames(df_sim_case) %in% c("algorithm"))]
      
      #Rename 
      colnames(df_sim_case)[colnames(df_sim_case)=="Group.1"] <- "algorithm"
      
      # Add ev number
      df_sim_case$num_ev <- num_ev
      df_agg <- rbind(df_agg,df_sim_case)
    }
    
    # Add number of rows
    df_agg$obs <- nrows
    
    # Merge Results
    df_case4_raw <- rbind(df_case4_raw,df_agg)
  }
  
  #Statistical Results
  DF_CASE4_RAW_TTEST <<- get_ttest_results(df_case4_raw)
  return(df_case4_raw)
}


###################### PLOTS ##############################

get_line_plots <- function(lr,rf,xvar,yvar){
  
  # Logistic Regression
  plot(lr[,xvar],lr[,yvar],xlab=xvar,ylab=yvar,main=paste0('Simulation Results: ',xvar,' vs ',yvar),
       col='sienna1',type='b',pch=19,lwd=3,cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5,ylim=c(0,1))
  
  # Random Forest
  lines(rf[,xvar],rf[,yvar],col='lightseagreen',type='b',pch=18,lwd=3)
  
  #grid
  grid(nx = NULL, ny = NULL, col = "white", lty = "solid")
  
  # Legend
  legend("bottomright", legend=c("RF", "LR"),
         col=c("lightseagreen" , "sienna1"), lty=1:2, cex=0.8,text.font=4,box.lty=0)
}


get_line_plots_case4 <- function(df,y_axis,ev){
  
  # Filter For Number of Explanatory Variables
  df<- subset(df, num_ev == ev)
  ggplot(df, aes(x=df[,"obs"],y=df[,y_axis],colour=factor(df$algorithm)),group=factor(df$algorithm)) + 
    ggtitle(paste('Logistic Regression vs Random Forest',': Total EV --',ev)) + 
    geom_line(size=1.0) + scale_color_manual(values=c("sienna1", "lightseagreen")) + xlab("Observations") + ylab(y_axis) + labs(colour="Algorithms") + 
    theme_bw() + theme(plot.title = element_text(hjust = 0.5)) + theme(plot.title = element_text(size=20)) + scale_fill_discrete(name = "Algorithms") +
     coord_cartesian(ylim=c(0,1))
}


get_boxplots <- function(df,x_axis,y_axis) {
  ggplot(df, aes(x=as.factor(df[,x_axis]), y=df[,y_axis], fill=factor(df$algorithm))) + xlab(x_axis) + ylab(y_axis) + geom_boxplot()  + 
    ggtitle(paste("Logistic Regression vs Random Forest:",sapply(y_axis, toupper)))+ theme_bw() + theme(plot.title = element_text(hjust = 0.5)) + theme(plot.title = element_text(size=20)) +
    scale_fill_discrete(name = "Algorithms") +  coord_cartesian(ylim=c(0.50,1))
}

get_histogram <- function(df,x_axis) {
  ggplot(df, aes(df[,x_axis], fill = df$algorithm)) + geom_histogram(alpha = 0.6) + xlab(x_axis) + ylab("Count") + 
    ggtitle(paste("Logistic Regression vs Random Forest:",sapply(x_axis, toupper))) + theme_bw() + theme(plot.title = element_text(hjust = 0.5)) + theme(plot.title = element_text(size=20)) +
    scale_fill_discrete(name = "Algorithms") 
}

####################### Varying Variance ####################### 
server <- function(input, output) {
  
  # Return the requested dataset ----
  simdata <- reactive({
    SIM_DATA <<- sim_data(input$nrows,input$noise,input$cat,input$ndist,input$nvar,input$ev,input$weights,input$yint)
  })
  
  equation <- reactive({
    rm(list = ls())
    gc()
    par(mfrow=c(1,1))
    
    linear_eq(input$ev,input$weights,input$yint)
  })
  
  # Show Table
  output$table <- renderTable({
    head(simdata(), n = 10)
  })
  
  output$cplot <- renderPlot({
    corrplot(cor(simdata()[,1:length(names(simdata()))-1]), method="shade",type='lower',tl.col = "black", tl.srt = 45)
  })
  
  # Print Equation
  output$equation <- renderText({
    paste0("y = ",equation())
  })
  
  # Logistic and Random forest simulation
  lr_rf_nvar <- reactive({
    withProgress(message = 'Training Logistic Regression and Random Forest Models', value = 0,
                 run_model <- simulation_nvar(input$n_sim,
                                              input$split,
                                              input$cat,
                                              input$nrows,
                                              input$noise,
                                              input$ndist,
                                              nvar,input$ev,
                                              input$weights,
                                              input$yint,
                                              input$varselect, # Logistic Regression
                                              input$ntree, # Random Forest
                                              input$prob_thresh
                 )
    )
  })
  
  # Titles
  output$lr_title_nvar <- renderText({
    paste0( "The table below is the average of running ", input$n_sim, " simulations of logistic regression at 10 levels of variance (variance = 0.5 to 5.0 in increments of 0.5) in the generated data.")
  })
  
  output$rf_title_nvar <- renderText({
    paste0( "The table below is the average of running ", input$n_sim, " simulations of random forest at 10 levels of variance (variance = 0.5 to 5.0 in increments of 0.5) in the generated data.")
  })
  
  # Print LR Matrix
  output$lr_sim_nvar <- renderTable({
    LR_RF <<- lr_rf_nvar()
    LR1 <<- LR_RF[LR_RF$algorithm == 'Logistic Regression',]
    RF1 <<- LR_RF[LR_RF$algorithm == 'RandomForest',]
    LR1
  })
  
  # Print RF Matrix
  output$rf_sim_nvar <- renderTable({
    RF1
  })
  
  # Print TTEST
  output$ttest1 <- renderTable({
    DF_CASE1_RAW_TTEST
  })
  
  # CASE1 Plots
  output$case1_chart1 <- renderPlot({
    get_line_plots(LR1,RF1,'nvar','tpr')
  })
  
  output$case1_chart2 <- renderPlot({
    get_line_plots(LR1,RF1,'nvar','fpr')
  })
  
  output$case1_chart3 <- renderPlot({
    get_line_plots(LR1,RF1,'nvar','acc')
  })
  
  # CASE1 BOX PLOT
  output$case1_chart4 <- renderPlot({
    get_boxplots(DF_CASE1_RAW,'nvar','acc')
  })
  
  # CASE1 BOX PLOT
  output$case1_chart5 <- renderPlot({
    get_boxplots(DF_CASE1_RAW,'nvar','auc')
  })
  
  # CASE1 Histogam
  output$case1_chart6 <- renderPlot({
    get_histogram(DF_CASE1_RAW,'tpr')
  })
  

  
  ####################### Number of noise variables ####################### 
  output$lr_title_num_nvar <- renderText({
    paste0( "The table below displays the results of running logistic regression ", input$n_sim, " times with ", input$ev, " explanatory variables and 1, 5, 10, 20, and 50 noise variables in the dataset.")
  })
  
  output$rf_title_num_nvar <- renderText({
    paste0( "The table below displays the results of running random forest ", input$n_sim, " times with ", input$ev, " explanatory variables and 1, 5, 10, 20, and 50 noise variables in the dataset.")
  })
  
  lr_rf_num_nvar <- reactive({
    withProgress(message = 'Training Logistic Regression and Random Forest Models', value = 0,
                 run_model <- simulation_num_nvar(input$n_sim,
                                                  input$split,
                                                  input$cat,
                                                  input$nrows,
                                                  inpur$noise,
                                                  input$ndist,
                                                  input$nvar,
                                                  input$ev,
                                                  input$weights,
                                                  input$yint,
                                                  input$varselect, # Logistic Regression
                                                  input$ntree, # Random Forest
                                                  input$prob_thresh
                 )
    )
  })
  
  
  # Print LR Matrix
  output$lr_sim_num_nvar <- renderTable({
    LR_RF2 <<- lr_rf_num_nvar()
    LR2 <<- LR_RF2[LR_RF2$algorithm == 'Logistic Regression',]
    RF2 <<- LR_RF2[LR_RF2$algorithm == 'RandomForest',]
    LR2
  })
  
  # Print RF Matrix
  output$rf_sim_num_nvar <- renderTable({
    RF2
  })
  
  # Print TTEST
  output$ttest2 <- renderTable({
    DF_CASE2_RAW_TTEST
  })
  
  # CASE2 Plots
  output$case2_chart1 <- renderPlot({
    get_line_plots(LR2,RF2,'num_noise','tpr')
  })
  
  output$case2_chart2 <- renderPlot({
    get_line_plots(LR2,RF2,'num_noise','fpr')
  })
  
  output$case2_chart3 <- renderPlot({
    get_line_plots(LR2,RF2,'num_noise','acc')
  })
  
  output$case2_chart4 <- renderPlot({
    get_boxplots(DF_CASE2_RAW,'num_noise','acc')
  })
  
  output$case2_chart5 <- renderPlot({
    get_boxplots(DF_CASE2_RAW,'num_noise','auc')
  })
  
  
  ####################### Number of explanatory variables ####################### 
  output$lr_title_num_nevar <- renderText({
    paste0( "The table below displays the results of running logistic regression ", input$n_sim, " times with ", input$noise, " noise variables and 1, 5, 10, 20, and 50 explanatory variables in the dataset.")
  })
  
  output$rf_title_num_nevar <- renderText({
    paste0( "The table below displays the results of running random forest ", input$n_sim, " times with ", input$noise, " noise variables and 1, 5, 10, 20, and 50 explanatory variables in the dataset.")
  })
  
  lr_rf_num_ev <- reactive({
    withProgress(message = 'Training Logistic Regression and Random Forest Models', value = 0,
                 run_model <- simulation_num_evar(input$n_sim,
                                                  input$split,
                                                  input$cat,
                                                  input$nrows,
                                                  input$noise,
                                                  input$ndist,
                                                  input$nvar,
                                                  input$ev,
                                                  input$weights,
                                                  input$yint,
                                                  input$varselect, # Logistic Regression
                                                  input$ntree, # Random Forest
                                                  input$prob_thresh
                 )
    )
  })
  
  
  # Print LR Matrix
  output$lr_sim_num_evar <- renderTable({
    LR_RF3 <<- lr_rf_num_ev()
    LR3 <<- LR_RF3[LR_RF3$algorithm == 'Logistic Regression',]
    RF3 <<- LR_RF3[LR_RF3$algorithm == 'RandomForest',]
    LR3
  })
  
  # Print RF Matrix
  output$rf_sim_num_evar <- renderTable({
    RF3
  })
  
  # Print TTEST
  output$ttest3 <- renderTable({
    DF_CASE3_RAW_TTEST
  })
  
  # CASE3 Plots
  output$case3_chart1 <- renderPlot({
    get_line_plots(LR3,RF3,'num_ev','tpr')
  })
  
  output$case3_chart2 <- renderPlot({
    get_line_plots(LR3,RF3,'num_ev','fpr')
  })
  
  output$case3_chart3 <- renderPlot({
    get_line_plots(LR3,RF3,'num_ev','acc')
  })
  
  output$case3_chart4 <- renderPlot({
    get_boxplots(DF_CASE3_RAW,'num_ev','acc')
  })
  
  output$case3_chart5 <- renderPlot({
    get_boxplots(DF_CASE3_RAW,'num_ev','auc')
  })
  
  ####################### Varying Number of explanatory variables ####################### 
  output$lr_title_num_obs <- renderText({
    paste0( "The table below displays a sample of the results of running logistic regression and random forest with varying number of observations")
  })
  
  lr_rf_num_obs <- reactive({
    withProgress(message = 'Training Logistic Regression and Random Forest Models', value = 0,
                 run_model <- simulation_num_obs(input$n_sim,
                                                 input$split,
                                                 input$cat,
                                                 input$nrows,
                                                 input$noise,
                                                 input$ndist,
                                                 input$nvar,
                                                 input$ev,
                                                 input$weights,
                                                 input$yint,
                                                 input$varselect, # Logistic Regression
                                                 input$ntree, # Random Forest
                                                 input$prob_thresh
                 )
    )
  })
  
  
  # Print Matrix
  output$lr_rf_sim_num_nobs <- renderTable({
    DF_CASE4_RAW <<- lr_rf_num_obs()
    head(DF_CASE4_RAW,10)
  })
  
  # Print TTEST
  output$ttest4 <- renderTable({
    DF_CASE4_RAW_TTEST
  })
  
  # Plot
  output$case4_chart1 <- renderPlot({
    get_line_plots_case4(DF_CASE4_RAW,'acc',1)
  })
  
  output$case4_chart2 <- renderPlot({
    get_line_plots_case4(DF_CASE4_RAW,'acc',10)
  })
  
  output$case4_chart3 <- renderPlot({
    get_line_plots_case4(DF_CASE4_RAW,'acc',20)
  })
  
  output$case4_chart4 <- renderPlot({
    get_line_plots_case4(DF_CASE4_RAW,'acc',50)
  })
  
  output$case4_chart5 <- renderPlot({
    get_line_plots_case4(DF_CASE4_RAW,'tpr',1)
  })
  
  output$case4_chart6 <- renderPlot({
    get_line_plots_case4(DF_CASE4_RAW,'tpr',10)
  })
  
  output$case4_chart7 <- renderPlot({
    get_line_plots_case4(DF_CASE4_RAW,'tpr',20)
  })
  
  output$case4_chart8 <- renderPlot({
    get_line_plots_case4(DF_CASE4_RAW,'tpr',50)
  })
  
  output$case4_chart9 <- renderPlot({
    get_line_plots_case4(DF_CASE4_RAW,'auc',1)
  })
  
  output$case4_chart10 <- renderPlot({
    get_line_plots_case4(DF_CASE4_RAW,'auc',10)
  })
  
  output$case4_chart11 <- renderPlot({
    get_line_plots_case4(DF_CASE4_RAW,'auc',20)
  })
  
  output$case4_chart12 <- renderPlot({
    get_line_plots_case4(DF_CASE4_RAW,'auc',50)
  })

}