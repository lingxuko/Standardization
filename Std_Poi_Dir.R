#' A Direct Standardization Function Under Poisson Regression
#'
#' This function provides Direct standardized outcome divided by a reference level outcome.
#' @param Data: Input Dataset
#' @param Var: variables included in model
#' @param Outcome: Standardized value divided by a reference level value
#' @param Std_by: variables used to divide subgroup
#' @param Ref: Reference measure
#' @param Model: model used to fit between Var and Outcome
#' @keywords Direct standardization under logistic model
#' @export
#' @examples
#' Std_Poi_Dir(data,"sex",c("BMI"),Std_by = "white", Group = 1,Ref = "median")

Std_Poi_Dir <- function(Data, Outcome, Predictor, Std_by, Group, Ref="median"){
  Data_Var <- names(Data)
  if(sum(Predictor%in%Data_Var)!=length(Predictor)){break}
  if(!(Outcome%in%Data_Var)){break}
  if(!(Std_by%in%Data_Var)){break}
  if(!(Ref%in%c("median","SMR"))){break}

  Data[,c(paste(Std_by))] <- as.factor(Data[,c(paste(Std_by))])
  Data[,c(paste(Outcome))] <- as.numeric(Data[,c(paste(Outcome))])

  Data_ref <- Data[,unique(c(Std_by, Predictor))]
  Data_ref[,Std_by] <- as.character(Group)

  Index <- names(table(Data[,c(Std_by)]))
  k <- length(Index)

  Model <- paste(Outcome, "~", Std_by, "+", paste(Predictor,collapse = "+"),"-1",sep = "")

  if(Ref=="median"){
    Mod <- glm(paste(Model),data=Data,family = poisson(link = log))
    Coef <- summary(Mod)$coefficients
    Coef2 <- c(median(Coef[(1:k),1]),Coef[(k+1):nrow(Coef),1])
    Numerator <- mean(predict(Mod, Data_ref, type = "response"), na.rm=TRUE)
    Data_ref[,Std_by] <- as.numeric(Data_ref[,Std_by])
    Pred_Denom <- as.matrix(Data_ref)%*%Coef2
    Denominator <- mean(1/(1 + exp(-as.numeric(Pred_Denom))),na.rm=TRUE)
  }

  if(Ref=="SMR"){
    Mod <- glm(paste(Model),data=Data,family = poisson(link = log))
    Coef <- summary(Mod)$coefficients
    Numerator <- mean(predict(Mod, Data_ref, type = "response"), na.rm=TRUE)
    Data_ref[,Std_by] <- as.numeric(Data_ref[,Std_by])
    Denominator <- c()
    for (i in 1:k) {
      Data_ref[,Std_by] <- as.numeric(Index[i])
      Coef2 <- c(Coef[i],Coef[(k+1):nrow(Coef),1])
      Pred_Denom <- as.matrix(Data_ref)%*%Coef2
      Denominator[k] <- mean(1/(1 + exp(-as.numeric(Pred_Denom))),na.rm=TRUE)
    }
    Denominator <- mean(Denominator,na.rm=TRUE)
  }
  return(Numerator/Denominator)
}
