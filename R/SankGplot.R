#' Read input csv file to generate the Sankey Diagram
#'
#' This function loads a file directory to plot the sankey diagram using ggplot library.
#' It assumes the input file is a csv file with the columns being the risk factor names and
#' the corresponding years in which the measurements are recorded.
#' In order to have a standard form, the column names are changed into Variable, Year and Measurement
#' kepted.
#' It accepts a White_Option boolean variable indicating either we would like to print
#' the vertical and horizontal white spaces
#' It gets an input "OutputDir" for the directory of generating the output generated Sankey diagram
#'
#'@import ggplot2
#'@import ggalluvial
#'@import readxl
#'@import reshape
#' @param filDir Path to the input csv file
#' @keywords SankeyGPlot
#' @return A Sankey Diagram of input file
#' @export
#' @example

SankeyGplot <- function(InputDir, White_Space, OutputDir){

  if (White_Space) {
    HorizontalSize = 0.03 #HorizontalSize will determine either we would like to have vertical WhiteSpace
    VerticalSize = 0.8 #VerticalSize will determine either we would like to have vertical WhiteSpace
  } else {
    HorizontalSize = 0.0
    VerticalSize = 0.0
  }


  sampleData <- read.csv(InputDir,as.is=TRUE, check.names = FALSE)
  sampleData <- melt(sampleData, id=c(colnames(sampleData)[1]))

  # in order to be standard, we change the columns into Risk Factor , Year and Measurement!
  newColNames<-c("Variable","Year","Measurement")
  colnames(sampleData)<-newColNames



  #Assumes sampleData has the properly formatted columns
  sampleData.ext=NULL
  verticalYearMargin = 2

  sampleData$Year = as.numeric(as.character(sampleData$Year))
  verticalDivider = c(unique(sampleData$Year)+verticalYearMargin,unique(sampleData$Year)-verticalYearMargin)

  for(uy in unique(sampleData$Year)){
    yearMask = sampleData[,"Year"]==uy
    sampleDataYear = sampleData[yearMask,]
    sampleDataYear = sampleDataYear[c(order(sampleDataYear$Measurement)),]

    isFirst=TRUE
    yMax.Rolling=0;
    sampleDataYear$yMin = rep(-1,nrow(sampleDataYear))
    sampleDataYear$yMax = rep(-1,nrow(sampleDataYear))
    sampleDataYear$labels = rep("",nrow(sampleDataYear))
    sampleDataYear$labelYPos = rep(-1,nrow(sampleDataYear))
    for(i in 1:nrow(sampleDataYear)){
      if(isFirst){
        yVal.min = 0
        isFirst=FALSE
      }else{
        yVal.min=yMax.Rolling+HorizontalSize
      }

      yVal = sampleDataYear[i,"Measurement"]

      yVal.max=yVal.min+yVal
      yMax.Rolling=yVal.max

      sampleDataYear[i,"yMin"]=yVal.min
      sampleDataYear[i,"yMax"]=yVal.max

      sampleDataYear[i,"labelYPos"]=mean(c(yVal.min,yVal.max))
      sampleBefore <- sampleDataYear[i,]
      sampleBefore$Year <- sampleBefore[,"Year"]-verticalYearMargin
      sampleAfter <- sampleDataYear[i,]
      sampleAfter$Year <- sampleAfter[,"Year"]+verticalYearMargin
      sampleDataYear = rbind(sampleDataYear,sampleBefore);
      sampleDataYear = rbind(sampleDataYear,sampleAfter);

      sampleDataYear[i,"labels"]=yVal
    }
    sampleData.ext  = rbind(sampleData.ext,sampleDataYear)
  }

  ggplot(data=sampleData.ext, aes_string(x = "Year", y = "Measurement", group = "Variable")) +
    geom_ribbon(aes(ymin=yMin,
                    ymax=yMax,
                    fill = factor(Variable)), alpha = 0.5)+
    geom_vline(xintercept =verticalDivider ,
               colour = "white", size=VerticalSize)+
    theme_bw() + #remove gray background and show black border
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+ #remove gridlines
    geom_text(aes(y=labelYPos,label = labels), colour="white",size=2.5,fontface = "bold")+
    labs(fill=colnames(sampleData)[1])+
    xlab(colnames(sampleData)[2])+
    ylab(colnames(sampleData)[3])
   ggsave(
    filename=paste0(OutputDir,'Sankey.pdf'),
  )

}


