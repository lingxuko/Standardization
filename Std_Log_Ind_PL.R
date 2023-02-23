#' A Function Provides Line Plots Comparing Raw and Indirect Standardized Outcomes Under Logistic Model
#'
#' This function provides ggplot2 based line plots comparing raw and indirect standardized outcomes under logistic model.
#' @param Data: Input Dataset
#' @param Var: variables included in model
#' @param Outcome: Line Plot
#' @param Std_by: variables used to divide subgroup
#' @param Model: model used to fit between Var and Outcome
#' @keywords Line Plots, Indirect standardization under logistic model
#' @export
#' @examples
#' Std_Log_Ind_PL(data,"sex",c("BMI"),Std_by = "white", Ref = "median")

Std_Log_Ind_PL <- function(Data, Outcome, Predictor, Std_by, Ref="median"){
  Data_Var <- names(Data)
  if(sum(Predictor%in%Data_Var)!=length(Predictor)){break}
  if(!(Outcome%in%Data_Var)){break}
  if(!(Std_by%in%Data_Var)){break}
  if(!(Ref%in%c("median","SMR"))){break}

  Test_Raw <- data.frame(Group=names(table(Data[,c(Std_by)])),
                         Tag="Raw",
                         Value=c(NA))

  Test_Adj <- data.frame(Group=names(table(Data[,c(Std_by)])),
                         Tag="Adjusted",
                         Value=c(NA))

  Test_Raw$Value <- apply(Test_Raw,1,function(X){
    Data_ref <- Data[,unique(c(Std_by, Outcome))]
    Data_ref$Std_var <- Data[,c(Std_by)]
    Data_ref2 <- Data_ref[which(Data_ref$Std_var==as.character(X["Group"])),]
    return(mean(Data_ref2[,c(Outcome)]))
  })
  Test_Adj$Value <- apply(Test_Adj,1,function(X){
    return(Std_Log_Ind(Data, Outcome, Predictor, Std_by, Group=X["Group"], Ref="median"))
  })

  Test <- rbind(Test_Raw,Test_Adj)
  Test2 <- data.frame(Group=names(table(Data[,c(Std_by)])),Value=Test[which(Test$Tag=="Adjusted"),"Value"]-Test[which(Test$Tag=="Raw"),"Value"])

  ggplot() +
    geom_line( data=Test, aes(x=Group, y=Value, group=Tag, colour=Tag),linewidth=1.5) +
    scale_color_manual(values=c('Black','#FFA500'))+
    geom_point( data=Test, aes(x=Group, y=Value, group=Tag),size=2) +
    geom_segment(data=Test2, aes(x=Group, y=Value, xend=Group),yend=0,colour="blue")+
    geom_point( data=Test2, aes(x=Group, y=Value),size=2) +
    scale_y_continuous(
      name = paste(Outcome),
      sec.axis = sec_axis(~.*1, name=paste(Outcome, "Adjusted vs Raw Difference",sep=" "))
    )+
    geom_hline(yintercept=0)+
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(), axis.line = element_line(colour = "black"))
}
