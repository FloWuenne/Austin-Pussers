library(shiny)
library(Biobase)
library(openxlsx)
Sys.setenv("R_ZIPCMD"="zip")


authorized_emails <- c("lassefolkersen@gmail.com","per.eriksson@ki.se","lei.du@ki.se","jesper.gadin@gmail.com")

path_expressionset<-"/home/ubuntu/data/2015-02-22_ASAP_expressionset/2011-03-24 ASAP core meta probe set.rdata"
path_annotation<-"/home/ubuntu/data/2015-02-22_ASAP_expressionset/2012-04-26 simple annotation of HuEx core.rData"
path_clinical_data<-"/home/ubuntu/data/2015-02-22_ASAP_expressionset/2013-09-09 asap_clinical_database.Rdata"
path_illegal_access<-"/home/ubuntu/log/illegal_access_log.txt"
path_log<-"/home/ubuntu/logs/log.txt"
path_functions<-"/home/ubuntu/srv/ask_ASAP/expression_browser/function.R"

# #for local testing
# path_expressionset<-"C:/Users/FOLK/Documents/Work/Bioinformatics/Important R-images and cel files/2012-04-26 simple annotation of HuEx core.rData"
# path_annotation<-"C:/Users/FOLK/Documents/Work/Bioinformatics/Important R-images and cel files/2011-03-24 ASAP core meta probe set.rdata"
# path_clinical_data<-"C:/Users/FOLK/Documents/Work/Bioinformatics/Important R-images and cel files/2013-09-09 asap_clinical_database.Rdata"
# path_illegal_access<-"C:/Users/FOLK/Documents/Work/Bioinformatics/illegal_access_log.txt"
# path_log<-"C:/Users/FOLK/Documents/Work/Bioinformatics/asap_expression_log.txt"
# path_functions<-"C:/Users/FOLK/Documents/Work/Bioinformatics/2016-01-25_ask_ASAP/expression_browser/function.R"
# 
# 
# 
# #loading data and functions
load(path_expressionset)
load(path_annotation)
load(path_clinical_data)
source(path_functions)


#insert updated clinical data (Lei, we may want to update this - I'm not really sure what the current state of ASAP clinical data is)
for(expressionsetName in c("ASAP_AMed","ASAP_AAdv","ASAP_MMed","ASAP_L","ASAP_H")){
  expressionset<-get(expressionsetName)
  old_pdata_to_save <-pData(expressionset)[,!colnames(pData(expressionset))%in%colnames(clinical_data)]
  pData(expressionset)<-cbind(old_pdata_to_save, clinical_data[sampleNames(expressionset),])
  assign(expressionsetName,expressionset)
}

#what can be choosen as covariates - Lei we might want to trim in this and/or make nicer names
choices_for_covariate<-colnames(pData(expressionset))


# 
# # 
# # #test-values
# gene1 <- "CDKN2A"
# gene2 <- "CDKN2B"
# focus <- "gene"
# # focus <- "dil_and_cusp"
# colourBy <- "none"
# covariate <- "none"
# pchBy <- "none"
# email<-"lassefolkersen@gmail.com"
# expressionsetName	<- "ASAP_L"
# # 
# # 

shinyServer(function(input, output) {
  
  
  
  
  #this function serves to get the data, both ready for plotting and ready for download. It also performs logging and permission and error controls.
  get_data <- reactive({
    if(input$goButton == 0){
      stop("")
    }
    
    gene1 <- toupper(isolate(input$gene1))
    gene2 <- toupper(isolate(input$gene2))
    focus <- isolate(input$focus)
    pchBy <- isolate(input$pchBy)
    expressionsetName <- isolate(input$expressionsetName)
    covariate <- isolate(input$covariate)
    colourBy <- isolate(input$colourBy)
    pchBy <- isolate(input$pchBy)
    if(is.null(gene1) | gene1=="")return(NULL)
    email<-isolate(input$email)
    
    #log and register	
    if(!tolower(email) %in% authorized_emails){
      m<-c(format(Sys.time(),"%Y-%m-%d-%H-%M-%S"),"plot",email)
      m<-paste(m,collapse="\t")
      write(m,file=path_illegal_access,append=TRUE)
      Sys.sleep(2)
      stop("Non-allowed user. Send an email to Lasse or Lei to gain access")
    }else{
      m<-c(format(Sys.time(),"%Y-%m-%d-%H-%M-%S"),"expression browser",email,gene1,gene2,focus,colourBy,pchBy,covariate)
      m<-paste(m,collapse="\t")
      write(m,file=path_log,append=TRUE)
    }
    
    #Checking genes are actually present
    if(!gene1%in%annotation_HuEx[,"genesymbol"])stop(paste("Gene",gene1,"not found"))
    if(focus == "gene"){
      if(!gene2%in%annotation_HuEx[,"genesymbol"])stop(paste("Gene",gene2,"not found"))
    }
    
    
    #getting data set
    expressionset<-get(expressionsetName)
    
    
    #getting probesets		
    probesets1<-rownames(annotation_HuEx)[annotation_HuEx[,"genesymbol"]%in%gene1]
    if(focus == "gene"){
      probesets2<-rownames(annotation_HuEx)[annotation_HuEx[,"genesymbol"]%in%gene2]
    }else{
      probesets2<-vector()	
    }
    
    #subset to genes of interest, and re-label so they can be read easily on download.
    expressionset <- expressionset[c(probesets1,probesets2),]
    featureNames(expressionset) <- paste(annotation_HuEx[featureNames(expressionset),"genesymbol"],featureNames(expressionset),sep="_")
    
    #This blocks checks which clinical variates are actually of importance in the query and retain them in the expressionset
    var_of_interest<-vector()
    if(focus == "dil_and_cusp") var_of_interest <- c(var_of_interest, "dilation","cuspidity")
    if(focus =="covar")var_of_interest <- c(var_of_interest, covariate)
    if(colourBy!="none")var_of_interest <- c(var_of_interest, colourBy)
    if(pchBy!="none")var_of_interest <- c(var_of_interest, pchBy)
    pData(expressionset)<-pData(expressionset)[,unique(var_of_interest),drop=FALSE]
    
    #return data set for both the plotting and download functions		
    return(expressionset)
  })
  
  
  
  
  output$plotGenes <- renderPlot({
    expressionset<-get_data()
    
    gene1 <- toupper(isolate(input$gene1))
    gene2 <- toupper(isolate(input$gene2))
    focus <- isolate(input$focus)
    pchBy <- isolate(input$pchBy)
    expressionsetName <- isolate(input$expressionsetName)
    covariate <- isolate(input$covariate)
    colourBy <- isolate(input$colourBy)
    pchBy <- isolate(input$pchBy)
    
    #This block is just a small hack to make the plotting funciton behave. Otherwise it acts weird when it only gets a colourBy
    if(colourBy == "none" & pchBy == "none"){
      colourBy<-NULL
      pchBy<-NULL
    }else if(colourBy !="none" & pchBy == "none"){
      colourBy<-colourBy
      pchBy<-19
    }else if(colourBy =="none" & pchBy != "none"){
      colourBy<-NULL
      pchBy<-pchBy
    }else if(colourBy !="none" & pchBy != "none"){
      colourBy<-colourBy
      pchBy<-pchBy
    }
    
    
    
    
    
    if(focus =="dil_and_cusp"){
      #have to create special variable for this
      group_labels<-paste(as.character(expressionset[["dilation"]]),as.character(expressionset[["cuspidity"]]))
      expressionset[["dil_and_cusp"]]<-as.factor(group_labels)
      
      
      #doing linear regression statistics
      expression<-exprs(expressionset)[featureNames(expressionset)[1],]
      cuspidity<-expressionset[["cuspidity"]]
      dilation<-expressionset[["dilation"]]
      model1<-lm(expression ~ cuspidity*dilation)
      p_cusp <- signif(summary(model1)[["coefficients"]]["cuspidityTAV", "Pr(>|t|)"],3)
      p_dil <- signif(summary(model1)[["coefficients"]]["dilationnon-dilated", "Pr(>|t|)"],3)
      p_interaction <- signif(summary(model1)[["coefficients"]]["cuspidityTAV:dilationnon-dilated", "Pr(>|t|)"],3)
      
      
      #main plotting function
      fun_plot_genes_vs_clinical_20110209(
        expressionset=expressionset, 
        probesets=featureNames(expressionset)[1],
        label="dil_and_cusp",
        mtexts=c("X-labels", "n"),
        ylab=sub("_.+$","",featureNames(expressionset)[1]),
        horizontalScatterSpacing=0.05,pchBy=pchBy,colourBy=colourBy,plotSummary=NULL
      )
      
      #writing the P-values
      mtext(paste("Cuspidity: P=",p_cusp,"\nDilation: P=",p_dil,"\nInteraction: P=",p_interaction,sep=""),cex=0.7,adj=0)
      
      #Adding a particularly transparent-looking box plot on top:
      plot_data<-data.frame(dil_and_cusp=expressionset[["dil_and_cusp"]], exprs=expression)
      boxplot(exprs~dil_and_cusp,data=plot_data,add=T,at=1:4,col=rgb(30,144,255,100,maxColorValue=256),outline=FALSE,xaxt="n")	
      
      
    }else if(focus =="covar"){
      
      fun_plot_genes_vs_clinical_20110209(
        expressionset=expressionset, 
        probesets=featureNames(expressionset)[1],
        label=covariate,
        mtexts=c("X-labels", "n"),
        ylab=sub("_.+$","",featureNames(expressionset)[1]),
        horizontalScatterSpacing=0.05,pchBy=pchBy,colourBy=colourBy,
        doStatistics=TRUE
      )
      
    }else if(focus =="gene"){
      probeset1<-featureNames(expressionset)[sub("_.+$","",featureNames(expressionset))%in%gene1][1]
      probeset2<-featureNames(expressionset)[sub("_.+$","",featureNames(expressionset))%in%gene2][1]
      
      fun_plot_gene_vs_gene_20100416(expressionset,x_gene=probeset1,y_gene=probeset2)
      
    }else{stop("focus not recognized")}
    
  })
  
  
  
  output$downloadData <- downloadHandler(
    filename = paste(format(Sys.time(),"%Y-%m-%d-%H-%M-%S"),"data.xls",sep="_"),
    content = function(file){
      expressionset<-get_data()
      out<-cbind(t(exprs(expressionset)),pData(expressionset))
      write.table(out,file,col.names=NA,sep="\t")
    }	)
  
  
})

