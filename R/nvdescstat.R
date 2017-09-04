#' Numerical Variable Descriptive Statistics returns a data frame containing descriptive statistics for numerical variable.
#' @param    df - Input Data frame.
#' @return Returns a data frame containing descriptive statistics for numerical variable.
#' @examples
#' numv1<-c(8000,200,323)
#' numv2<-c(400,533,633)
#' numv3<-c(100,534,734)
#' numv4<-c(1,25,34)
#' chrv6<-c("a","b","c")
#' numv5<-c(50,10000,34000)
#' chrv7<-as.factor(c("male","female","unknown"))
#' numv6<-c(NA,300,340)
#' df<-data.frame(numv1,numv2,chrv6,numv3,numv4,numv5,chrv7,numv6)
#' nvdescstatdf<-nvdescstat(df)
#' @description Returns a data frame containing following descriptive statistics for numerical variables in a data frame.
#'              % Missing Value       -Percent of missing values in the variable. 
#'              Min Value             -Minimum value of the variable.
#'              Max Value             -Maximum value of the variable.
#'              Mean                  -Mean of the variable.
#'              Median                -Median of the variable.
#'              Variance              -Variance of the variable.
#'              Standard Deviation    -Standard Deviation of the variable.
#'              Lower Outlier Cutoff  -This helps to detect Lower outliers.
#'              Upper Outlier Cutoff  -This helps to detect Upper outliers.
#' @author "Sandip Kumar Gupta", "sandip_nitj@yahoo.co.in"
#' @import "stats"
#' @export
nvdescstat=function(df){
  options(scipen = 999)
  numvarsubset<-subbytype(df)[[2]]
  for(ivar in 1:length(names(numvarsubset))){
    if(ivar==1){
      sumdff<-data.frame(names(numvarsubset)[ivar],(sum(is.na(numvarsubset[,ivar]))/length(numvarsubset[,ivar]))*100,min(numvarsubset[,ivar],na.rm = T),max(numvarsubset[,ivar],na.rm = T),mean(numvarsubset[,ivar],na.rm = T),median(numvarsubset[,ivar],na.rm = T),var(numvarsubset[,ivar],na.rm = T),sd(numvarsubset[,ivar],na.rm = T),t(quantile(numvarsubset[,ivar],.25,na.rm = T))[1]-(1.5*IQR(numvarsubset[,ivar],na.rm = T)),t(quantile(numvarsubset[,ivar],.75,na.rm = T))[1]+(1.5*IQR(numvarsubset[,ivar],na.rm = T)))     
    } else {
      sumdff<-rbind(sumdff,data.frame(names(numvarsubset)[ivar],(sum(is.na(numvarsubset[,ivar]))/length(numvarsubset[,ivar]))*100,min(numvarsubset[,ivar],na.rm = T),max(numvarsubset[,ivar],na.rm = T),mean(numvarsubset[,ivar],na.rm = T),median(numvarsubset[,ivar],na.rm = T),var(numvarsubset[,ivar],na.rm = T),sd(numvarsubset[,ivar],na.rm = T),t(quantile(numvarsubset[,ivar],.25,na.rm = T))[1]-(1.5*IQR(numvarsubset[,ivar],na.rm = T)),t(quantile(numvarsubset[,ivar],.75,na.rm = T))[1]+(1.5*IQR(numvarsubset[,ivar],na.rm = T)))) 
    }
  }
  names(sumdff)<-c("Variable Name","% Missing Value","Min Value","Max Value","Mean","Median","Variance","Standard Deviation","Lower Outlier Cutoff","Upper Outlier Cutoff")
  return(sumdff)
}