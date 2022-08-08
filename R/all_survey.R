# ********************************************************************
# Survey of Professional forecasters Functions
# Doc: https://github.com/joergrieger/Survey#readme

# Aggregated by me into one source file




# ********************************************************************

# *******************downloadSPF function **********************************

#' Download survey data
#' @param survey The survey you wish to download
#' @param type The type you wish you to download, i.e. mean, median or annualized growth rates
#' @return downloadSPF returns an object of class "survey".
#'
#' An object of class "survey" is a list containing the following components
#' \item{survey}{The name of the survey}
#' \item{type}{Type of the survey, i.e. mean, median, growth, or individual (see individualSPF)}
#' \item{variable}{Name of the variable indicating the forecast horizon, e.g. "NGDP1", or "NGDP2", downloaded. If "all", all forecast horizons were downloaded}
#' \item{sseries}{An object of the class ts containing the downloaded series}
#' @examples
#' tmp <- downloadSPF(survey = "CPI", type = "growth")

downloadSPF <- function(survey="NGDP", type="mean"){

  # Make sure the survey name is in lower case
  survey <- tolower(survey)

  # Create URLs
  if(type == "mean"){
    dlURL <- paste("https://www.philadelphiafed.org/-/media/research-and-data/real-time-center/survey-of-professional-forecasters/data-files/files/mean_",survey,"_level.xlsx",sep="")

  }
  else if(type == "median"){
    dlURL <- paste("https://www.philadelphiafed.org/-/media/research-and-data/real-time-center/survey-of-professional-forecasters/data-files/files/median_",survey,"_level.xlsx",sep="")

  }
  else if(type == "growth"){
    dlURL <- paste("https://www.philadelphiafed.org/-/media/research-and-data/real-time-center/survey-of-professional-forecasters/data-files/files/mean_",survey,"_growth.xlsx",sep="")
  }

  # Download File
  tf = tempfile(fileext=".xlsx")
  download.file(url = dlURL,destfile = tf, mode = "wb")
  fi <- readxl::read_excel(tf)
  forecasts <- fi[,-c(1:2)]

  # Create Time Series Object of data
  startYear <- as.numeric(fi[1,1])
  startQuarter <- as.numeric(fi[1,2])
  forecasts <- ts(forecasts,start = c(startYear,startQuarter),frequency = 4)

  # Delete temporary files
  file.remove(tf)

  # create object of class "survey" and return it
  retList = structure(list(
    survey   = survey,
    type     = type,
    variable = "all",
    series   = forecasts
  ),class="survey")
  return(retList)

}


# ********************************************************************

#' @title Download individual mean survey data
#' @param survey The survey you wish to download
#' @param variable The variable name indicating the forecast horizon, e.g. "NGDP1" or "CPI2".
#' @return individual SPF returns an object of class "survey".
#'
#' An object of class "survey" is a list containing the following components
#' \item{survey}{The name of the survey}
#' \item{type}{Type of the survey, i.e. mean, median, growth, or individual (see individualSPF)}
#' \item{variable}{Name of the variable indicating the forecast horizon, e.g. "NGDP1", or "NGDP2", downloaded. If "all", all forecast horizons were downloaded}
#' \item{series}{An object of the class ts containing the downloaded series}
#' @details This function downloads an individual survey
#' @examples
#' \dontrun{
#' # download the growth rate forecasts for the consumer price index
#' tmp <- downloadSPF(survey = "CPI", type = "growth")
#' }
#' @export

individualSPF <- function(survey = "NGDP",variable="NGDP1"){

  # Make sure the survey name is in lower case for filename
  survey = tolower(survey)


  # Ditto with variable but in upper case
  variable = toupper(variable)

  # Download Data
  dlURL <- paste("https://www.philadelphiafed.org/-/media/frbp/assets/surveys-and-data/survey-of-professional-forecasters/data-files/files/individual_",survey,".xlsx",sep="")
  tf = tempfile(fileext=".xlsx")
  download.file(url = dlURL,destfile = tf, mode = "wb")
  fi <- readxl::read_excel(tf)


  # unstack data
  individualFcs   <- unstck(fi,variable)
  startYear       <- as.numeric(fi[1,1])
  startQuarter    <- as.numeric(fi[1,2])
  tsIndividualFcs <- ts(individualFcs,start=c(startYear,startQuarter),frequency = 4)

  # Convert into tsibble
  colnames(tsIndividualFcs) <- paste("Forecaster",c(1:588))
  tblIndividualFcs <- tsibble::as_tsibble(tsIndividualFcs)


  # Delete temporary files
  file.remove(tf)



  # Create object of class survey and return it
  retList = structure(list(
    survey   = toupper(survey),
    type     = "individual",
    variable = variable,
    series = tsIndividualFcs
  ),class="survey")
  return(retList)

}

# unstack data
unstck <- function(df,variable){


  maxID <- max(df$ID) # How many individual forecaster
  individualForecasts <- matrix(NA,nrow=0,ncol=maxID) # Initial matrix

  # Loop over all years in df
  for(ii in min(df$YEAR):max(df$YEAR)){

    tmpFilter <- dplyr::filter(df,YEAR == ii)

    minQuarter <- min(tmpFilter$QUARTER)
    maxQuarter <- max(tmpFilter$QUARTER)

    # Loop over all quarters in a year
    for(jj in minQuarter:maxQuarter){

      tmpFilter2 <- dplyr::filter(tmpFilter,QUARTER == jj)
      values <- matrix(NA,nrow=1,ncol=maxID)

      # Loop over all IDs and store forecast appropriate place
      for(kk in tmpFilter2$ID){

        value <- dplyr::filter(tmpFilter2, ID == kk)
        values[1,kk] <- suppressWarnings(as.numeric(dplyr::select(value,variable)))

      }
      individualForecasts <- rbind(individualForecasts, values)
    }
  }
  return(individualForecasts)

}

#' @title download probability forecasts
#' @param survey The survey data to download
#' @return Returns an S3 object of the class probsurvey
#' @export
probabilitySPF <- function(survey ="prgdp"){
  survey = tolower(survey)

  # download file
  dlURL=paste("https://www.philadelphiafed.org/-/media/frbp/assets/surveys-and-data/survey-of-professional-forecasters/data-files/files/individual_",survey,".xlsx",sep="")
  tf = tempfile(fileext=".xlsx")
  download.file(url = dlURL,destfile = tf, mode = "wb")
  fi <- readxl::read_excel(tf,na="#N/A")

  # remove industry
  fi <- fi[,-c(4)]

  # return(fi)
  retlist <- structure(list(forecasts = fi, type=survey),class="probsurvey")
  return(retlist)
}

# ********************************************************************

#' @export
#' @title  Calculating cross-sectional measures of forecast dispersion
#' @param surveyObj an object of class survey
#' @param method the dispersion measure
#' @details Computes dispersion measures for forecasters.
#' \itemize{
#' \item{1}{Interquartile Range}
#' \item{2}{Standard deviation}
#' \item{3}{Interquartile Range on Q/Q-growth}
#' }
#' @return The function returns an object of class ts
#' @examples
#' tmp <- individualSPF(survey = "CPI", variable = "CPI3")
#' dsp <- dispersion(tmp,method = 2)
#' @rdname dispersion
dispersion <- function(obj,...) UseMethod("dispersion")

#' @export
#' @rdname dispersion
dispersion.survey <- function(surveyObj, method = 1){

  # Check if survey object contains individual forecasts
  if(surveyObj$type != "individual"){

    stop("The survey object does not contain individual forecasts")

  }
  # Calculate dispersion

  if(method == 1){

    # Interquartile range

    nr <- nrow(surveyObj$series)


    tmp <- t(apply(surveyObj$series,1,quantile,na.rm=TRUE))
    disp <- tmp[,4] - tmp[,2]

    startYear    <- start(surveyObj$series)[1]
    startQuarter <- start(surveyObj$series)[2]

  }
  else if(method == 2){

    # Standard deviation
    disp <- apply(surveyObj$series,1,sd,na.rm=TRUE)

    startYear    <- start(surveyObj$series)[1]
    startQuarter <- start(surveyObj$series)[2]

  }
  else if(method == 3){
    nc <- ncol(surveyObj$series)
    nr <- nrow(surveyObj$series)
    disp <- array(0,dim=c(nr-1))

    tstmp <- matrix(NA,ncol=nc,nrow=(nr-1))

    # Calculate Q/Q growth rates
    for(ii in 1:nc){
      tstmp[,ii] <- tis::growth.rate(surveyObj$series[,ii],simple=TRUE,lag = 1) / 400
    }

    # Calculate disperion measure

    tmp <- t(apply(tstmp,1,quantile,na.rm=TRUE))
    disp <- tmp[,4] - tmp[,2]

    # Calculate starting year and starting quarter of the time series.
    startYear    <- start(surveyObj$series)[1]
    startQuarter <- start(surveyObj$series)[2]

    if(startQuarter == 4){

      startYear <- startYear + 1
      startQuarter <- 1

    }
    else{

      startQuarter <- startQuarter + 1

    }
  }

  # Create time series object

  tsDisp <- ts(disp, start = c(startYear,startQuarter),frequency = 4)
  return(tsDisp)

}



# ********************************************************************

#' @title fit survey to distribution
#' @param srvObj survey
#' @param year year of forecast
#' @param quarter quarter of forecast
#' @param id forecaster
#' @param distr probability (cu)
#' @export

fit_distribution <- function(srvObj,year,quarter,id,distr="Beta",...) UseMethod("fit_distribution")

#' @export
fit_distribution.probsurvey <- function(srvObj,year,quarter,id,distr="Beta"){
  # Prepare %>% for use in function
  `%>%` <- magrittr::`%>%`

  # Filter out survey data
  filtsrv <- srvObj$forecasts %>%
    dplyr::filter(ID == id) %>%
    dplyr::filter(YEAR == year) %>%
    dplyr::filter(QUARTER == quarter)

  if(dim(filtsrv)[1] == 0){
    stop("Invalid inputs")
  }
  filtsrv <- filtsrv[,-c(1:3)]

  # get bins
  if(year < 1973 | (year == 1973 && quarter == 1)){
    #bins <- rev(c("10+","9-9.9","8-8.9","7-7.9","6-6.9","5-5.9","4-4.9","3-3.9","2-2.9","1-1.9","0-0.9","-1- -0.1","-2 - -1.1","-3 - -2.1","< -3"))
    binleft  <- c(-Inf,-3,-2,-1,0,1,2,3,4,5,6,7,8,9,10) # left side of the bin
    binright <- c(-3,-2.1,-1.1,-0.1,0.9,1.9,2.9,3.9,4.9,5.9,6.9,7.9,8.9,9.8,Inf) # right side of the bin

    # get empirical CDF
    srvey  <- rev(as.vector(unlist(filtsrv[,c(1:15)])))
    empcdf <- cumsum(srvey)



  }
  else if(year == 1973 && quarter > 1 | year == 1974 && quarter < 4){
    binleft  <- c(-Inf, -1, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12)
    binright <- c(  -1, -0.1, 0.9, 1.9, 2.9, 3.9, 4.9, 5.9, 6.9, 7.9, 8.9, 9.9, 10.9, 11.9, Inf)

    # get empirical CDF
    srvey  <- rev(as.vector(unlist(filtsrv[,c(1:15)])))
    empcdf <- cumsum(srvey)
  }
  else if(year == 1974 && quarter == 4 | year > 1974 && year < 1981 | year == 1981 && quarter < 3){

    binleft  <- c(-Inf, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16)
    binright <- c(3, 3.9, 4.9, 5.9, 6.9, 7.9, 8.9, 9.9, 10.9, 11.9, 12.9, 13.9, 14.9, 15.9, Inf)

    # get empirical CDF
    srvey  <- rev(as.vector(unlist(filtsrv[,c(1:15)])))
    empcdf <- cumsum(srvey)
  }
  else if(year == 1981 && quarter > 2 | year > 1981 && year < 1985 | year == 1985 && quarter == 1){

    binleft  <- c(-Inf, 4, 6, 8, 10, 12)
    binright <- c(4, 5.9, 7.9, 9.9, 11.9, Inf)

    # get empirical CDF
    srvey  <- rev(as.vector(unlist(filtsrv[,c(1:6)])))
    empcdf <- cumsum(srvey)
  }
  else if(year == 1985 && quarter > 1 | year > 1985 && year < 1992){
    binleft  <- c(-Inf, 2, 4, 6, 8, 10)
    binright <- c(2, 3.9, 5.9, 7.9, 9.9, Inf)

    # get empirical CDF
    srvey  <- rev(as.vector(unlist(filtsrv[,c(1:6)])))
    empcdf <- cumsum(srvey)
  }
  else if(year > 1991 && year < 2014){

    binleft  <- c(-Inf, 0, 1, 2, 3, 4, 5, 6, 7, 8)
    binright <- c(0, 0.9, 1.9, 2.9, 3.9, 4.9, 5.9, 6.9, 7.9, Inf)

    # get empirical CDF
    srvey  <- rev(as.vector(unlist(filtsrv[,c(1:10)])))
    empcdf <- cumsum(srvey)
  }
  if(distr == "Norm"){

    fitted <- optimx::optimx(par=c(0,1),fn=fitnorm,lower=c(-Inf,0.001),method="L-BFGS-B",empcdf=empcdf/100,binright=binright)

  }
  else if(distr == "Beta"){

    fitted <- optimx::optimx(par=c(1,1),fn=fitbeta,lower=c(0.001,0.001),method="L-BFGS-B",empcdf=empcdf/100,binright=binright)

  }


  return(fitted)

}

fitbeta <- function(x=c(1,1),empcdf = NULL, binright = NULL){

  nlength <- length(empcdf)
  fit <- 0

  for(ii in 1:nlength){

    betacdf <- pbeta(binright[ii],x[1],x[2])
    fit <- fit + (empcdf[ii] - betacdf)^2

  }
  return(fit)
}

fitnorm <- function(x=c(1,1),empcdf = NULL, binright = NULL){
  nlength <- length(empcdf)
  fit <- 0

  for(ii in 1:nlength){

    normcdf <- pnorm(binright[ii],mean=x[1],sd=x[2])
    fit <- fit + (empcdf[ii] - normcdf)^2
    print(fit)

  }
  return(fit)


}


# ********************************************************************

#' @title histogram for probabiliy forecasts
#' @param srvObj a S3-object created by probabilitySPF()
#' @param year Year of forecast
#' @param quarter quarter of forecast
#' @param id forecaster id
#' @export
#' @rdname plot_histogram

plot_histogram <- function(srvObj,year = NULL,quarter = NULL,...) UseMethod("plot_histogram")

#' @export
#' @rdname plot_histogram

plot_histogram.probsurvey <- function(srvObj,year = NULL,quarter = NULL, id = NULL){
  # Prepare %>% for use in function
  `%>%` <- magrittr::`%>%`

  # Filter out survey data
  filtsrv <- srvObj$forecasts %>%
    dplyr::filter(ID == id) %>%
    dplyr::filter(YEAR == year) %>%
    dplyr::filter(QUARTER == quarter)

  if(dim(filtsrv)[1] == 0){
    stop("Invalid inputs")
  }

  # Create bins
  filtsrv <- filtsrv[,-c(1:3)]
  if(srvObj$type=="prgdp"){

    binned_survey <- getbins_prgdp(filtsrv,year,quarter)

  }
  print(binned_survey)


  # To do: make graph nicer
  ggplot2::ggplot(data=binned_survey,mapping=ggplot2::aes_(x=~bins,y=~probability)) +
    ggplot2::geom_bar(stat="identity") + ggplot2::scale_x_discrete(limits = binned_survey$bins)

}

getbins_prgdp <- function(filtsrv,year,quarter){

  if(year < 1973 | (year == 1973 && quarter == 1)){
    srvey <- rev(as.vector(unlist(filtsrv[,c(1:15)])))
    bins <- rev(c("10+","9-9.9","8-8.9","7-7.9","6-6.9","5-5.9","4-4.9","3-3.9","2-2.9","1-1.9","0-0.9","-1- -0.1","-2 - -1.1","-3 - -2.1","< -3"))
    srvey_df <- data.frame(probability = srvey,bins=bins)
  }
  else if((year == 1973 && quarter >1) | (year == 1974 && quarter < 4)){
    srvey <- rev(as.vector(unlist(filtsrv[,c(1:15)])))
    bins <- rev(c("12+","11-11.9","10-10.9","9-9.9","8-8.9","7-7.9","6-6.9","5-5.9","4-4.9","3-3.9","2-2.9","1-1.9","0-0.9","-1 - 0.1","<-1"))
    srvey_df <- data.frame(probability = srvey,bins=bins)

  }
  else if( (year == 1974 && quarter == 4) | (year > 1974 && year < 1981) | (year == 1981 && quarter < 3)){

    srvey <- rev(as.vector(unlist(filtsrv[,c(1:15)])))
    bins <- rev(c("16+","15-15.9","14-14.9","13-13.9","12-12.9","11-11.9","10-10.9","9-9.9","8-8.9","7-7.9","6-6.9","5-5.9","4-4.9","3-3.9","<3"))
    srvey_df <- data.frame(probability = srvey,bins=bins)

  }
  else if( (year == 1981 && quarter >2 ) | (year > 1981 && year < 1985) | (year == 1985 && quarter == 1)){

    srvey <- rev(as.vector(unlist(filtsrv[,c(1:6)])))
    bins <- rev(c("12+","10-11.9","8-9.9","6-7.9","4-5.9","<4"))
    srvey_df <- data.frame(probability = srvey,bins=bins)


  }
  else if( (year == 1985 && quarter > 1) | (year > 1985 && year < 1992) ){

    srvey <- rev(as.vector(unlist(filtsrv[,c(1:6)])))
    bins <- rev(c("10+","8-9.9","6-7.9","4-5.9","2-3.9","<2"))
    srvey_df <- data.frame(probability = srvey,bins=bins)

  }
  else if (year > 1991 && year < 2014){

    srvey <- rev(as.vector(unlist(filtsrv[,c(1:10)])))
    bins <- rev(c("8+","7-7.9","6-6.9","5-5.9","4-4.9","3-3.9","2-2.9","1-1.9","0-0.9","<0"))
    srvey_df <- data.frame(probability = srvey,bins=bins)

  }
  else if( year > 2013){

    srvey <- rev(as.vector(unlist(filtsrv[,c(1:10)])))
    bins <- rev(c("4+","3.5-3.9","3.0-3.4","2.5-2.9","2.0-2.4","1.5-1.9","1.0-1.4","0.5-0.9","0-0.4","<0"))
    srvey_df <- data.frame(probability = srvey,bins=bins)

  }

  return(srvey_df)

}


# ********************************************************************
