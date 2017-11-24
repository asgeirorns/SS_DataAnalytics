library(shiny)
library(ggplot2)
library(plotly)
library(lubridate)
library(rhandsontable)
library(readxl)
#library(gganimate)
#library(gapminder)

load('laeknar.Rdata')
load('legudeild.Rdata')
load('gjorgaesla.Rdata')
load('op_count.Rdata')

MyData <- read.csv(file="MyData.csv", header=TRUE, sep=";")
#print(MyData)

instruments <- readRDS('instruments.rds', refhook = NULL)
#print(instruments)


#This table is kinda strange still 
DF = data.frame(
                #Must be an even number not odd.
                Aðgerðarkóði=SurgeryDataFrame$C[1:6],
                Aldursbil = c('Velja'),
                Raða = rep(FALSE),
                Legudeild = rep(FALSE),
                Gjörgæsla = rep(FALSE),
                Dagsetning = seq(from = Sys.Date(), by = "days",length.out = 1),
                Skurðstofa = c('Velja'),
                Aðalskurðlæknir=c('Velja'),
                stringsAsFactors = FALSE)

#Planning 
df2 = data.frame(
  #Must get the list of doctors
  Aðalskurðlæknir=ustaff,
  Vika_1_Mánudagur=rep(FALSE),
  Vika_1_Þriðjudagur=rep(FALSE),
  Vika_1_Miðvikudagur=rep(FALSE),
  Vika_1_Fimmtudagur=rep(FALSE),
  Vika_1_Föstudagur=rep(FALSE),
  Vika_2_Mánudagur=rep(FALSE),
  Vika_2_Þriðjudagur=rep(FALSE),
  Vika_2_Miðvikudagur=rep(FALSE),
  Vika_2_Fimmtudagur=rep(FALSE),
  Vika_2_Föstudagur=rep(FALSE),
  #Dagsetning = seq(from = Sys.Date(), by = "days",length.out = 1),
  stringsAsFactors = FALSE
)

#Lets plot the shifts of doctors
#THIS IS NOT WORKING
df3= data.frame(
  Aðgerðarkóði=SurgeryDataFrame$C[1:4],
  Aðfang = c('Skrá') ,
  Now = c('Til stadar'),
  stringsAsFactors = FALSE
)
  

df4= data.frame(
  Aðgerðardagur=c('Mánudagur','Þriðjudagur','Miðvikudagur','Fimmtudagur', 'Föstudagur'),
  Stofa_1 = c('-'),
  Stofa_2 = c('-'),
  Stofa_3 = c('-'),
  Stofa_4 = c('Heill dagur'),
  Stofa_5 = c('Heill dagur'),
  Stofa_6 = c('-'),
  Stofa_7 = c('-'),
  Stofa_8 = c('-'),
  stringsAsFactors = FALSE
)

df5 = data.frame(
  Aðgerðardagur = c('Mánudagur','Þriðjudagur', 'Miðvikudagur', 'Fimmtudagur','Föstudagur'),
  ParturDags = c('Ekki Nota'),
  stringsAsFactors = FALSE
)

df6 = data.frame(
  Dagur = c('Mánudagur','Þriðjudagur', 'Miðvikudagur', 'Fimmtudagur','Föstudagur'),
  Fjöldi_legurýma = c('Skrá legurými')
)

  


# Define server logic required to draw a histogram
shinyServer(function(input, output) {

 # inFile <- input$file1
  
#  if (is.null(inFile))
 #   return(NULL)
  
  #read.csv(inFile$datapath, header = input$header)
  # Expression that generates a histogram. The expression is
  # wrapped in a call to renderPlot to indicate that:
  #
  #  1) It is "reactive" and therefore should re-execute automatically
  #     when inputs change
  #  2) Its output type is a plot
  
  values <- reactiveValues()
  
 
  
  
  ## Handsontable
  observe({
    if (!is.null(input$hot)) {
      DF = hot_to_r(input$hot)
    } else {
      if (is.null(values[["DF"]]))
        DF <- DF
      else
        DF <- values[["DF"]]
    }
    values[["DF"]] <- DF
  })
  
  output$hot <- renderRHandsontable({
    DF <- values[["DF"]]
    if (!is.null(DF))
      rhandsontable(DF, useTypes = TRUE, stretchH = "all", readOnly = FALSE,rowHeaders = NULL)%>%
    hot_col(col="Aðgerðarkóði", type='dropdown', source=SurgeryDataFrame$C,allowInvalid = FALSE)%>%
    hot_col(col="Aðalskurðlæknir", type ='dropdown', source=ustaff,allowInvalid = FALSE)%>%
    hot_col(col="Skurðstofa", type='dropdown', source=c('Stofa 4','Stofa 5','Stofa 1',
            'Stofa 2','Stofa 3','Stofa 6','Stofa 7','Stofa 8'),default=TRUE, allowInvalid=FALSE)%>%
    hot_col(col="Aldursbil", type='dropdown', source=c('0-9','10-19','20-29','30-39','40-49','50-59','60-69','70-79','80-89','90-99','100+'))
  })
  
#Save scheduling table
  observeEvent(input$save, {
    finalDF <- isolate(values[["DF"]])
    saveRDS(finalDF, file=file.path('./', sprintf("%s.rds", 'hermun_tafla')))
    write.csv(finalDF, file = "MyData.csv",row.names=FALSE)
  })
  
#Save workscheduling
  observeEvent(input$save2, {
    finalDF2 <- isolate(values[["df2"]])
    saveRDS(finalDF2, file=file.path('./', sprintf("%s.rds", 'hermun_tafla_2')))
    write.csv(finalDF2, file = "Vaktarplan.csv",row.names=FALSE)
  })
  
#Save MSS
  observeEvent(input$SaveMaster, {
    finalDF4 <- isolate(values[["df4"]])
    saveRDS(finalDF4, file=file.path('./', sprintf("%s.rds", 'MSS')))
    write.csv(finalDF4, file = "MSS.csv",row.names=FALSE, fileEncoding = "UTF-8")
  })
  
#Save instruments
  observeEvent(input$SaveInstru, {
    finalDF3 <- isolate(values[["df3"]])
    saveRDS(finalDF3, file=file.path('./', sprintf("%s.rds", 'instruments')))
    write.csv(finalDF3, file = "instruments.csv",row.names=FALSE, fileEncoding = "UTF-8")
  })
  #Dont know what this does?
  output$surgeryselected <- renderUI({
    str1 <- surgeryname[as.numeric(input$select)]
    str2 <- paste("...")
    HTML(paste(str1, str2, sep = '<br/>'))
    
  })
  
#Lets make a reactive table so shifts of doctors can be inserted manually
  observe({
    if (!is.null(input$hotsd)) {
      df2 = hot_to_r(input$hotsd)
    } else {
      if (is.null(values[["df2"]]))
        df2<- df2
      else
        df2 <- values[["df2"]]
    }
    values[["df2"]] <- df2
  })
  
  output$hotsd <- renderRHandsontable({
    df2 <- values[["df2"]]
    if (!is.null(df2))
      rhandsontable(df2, useTypes = TRUE, stretchH = "all", readOnly = FALSE)#%>%
  }) 
  
  
  observe({
    if (!is.null(input$hot)) {
      DF = hot_to_r(input$hot)
    } else {
      if (is.null(values[["DF"]]))
        DF <- DF
      else
        DF <- values[["DF"]]
    }
    values[["DF"]] <- DF
  })
  
  
  observe({
    if (!is.null(input$Adfong)) {
      df3 = hot_to_r(input$Adfong)
    } else {
      if (is.null(values[["df3"]]))
        df3<- df3
      else
        df3 <- values[["df3"]]
    }
    values[["df3"]] <- df3
  })
  
  output$Adfong <- renderRHandsontable({
    df3 <- values[["df3"]]
    if (!is.null(df3))
      rhandsontable(df3, useTypes = TRUE, stretchH = "all", readOnly = FALSE)%>%
      hot_col(col="Aðgerðarkóði", type='dropdown', source=SurgeryDataFrame$C,allowInvalid = TRUE)%>%
      hot_col(col="Now", type='dropdown', source=instruements$Aðfang, allowInvalid=TRUE)
  }) 
  

  #Lets make a reactive table so shifts of doctors can be inserted manually
  observe({
    if (!is.null(input$Stofur)) {
      df4 = hot_to_r(input$Stofur)
    } else {
      if (is.null(values[["df4"]]))
        df4<- df4
      else
        df4 <- values[["df4"]]
    }
    values[["df4"]] <- df4
  })
  
  output$Stofur <- renderRHandsontable({
    df4 <- values[["df4"]]
    if (!is.null(df4))
      rhandsontable(df4, useTypes = TRUE, stretchH = "all", readOnly = FALSE,overflow = 'visible')%>%
      hot_col(col='Aðgerðardagur', allowInvalid=FALSE)%>%
      hot_col(col='Stofa_1', type='dropdown', source=c('Fyrir hádegi','Eftir hádegi','Heill dagur','-'))%>%
      hot_col(col='Stofa_2', type='dropdown', source=c('Fyrir hádegi','Eftir hádegi','Heill dagur','-'))%>%
      hot_col(col='Stofa_3', type='dropdown', source=c('Fyrir hádegi','Eftir hádegi','Heill dagur','-'))%>%
      hot_col(col='Stofa_4', type='dropdown', source=c('Fyrir hádegi','Eftir hádegi','Heill dagur','-'))%>%
      hot_col(col='Stofa_5', type='dropdown', source=c('Fyrir hádegi','Eftir hádegi','Heill dagur','-'))%>%
      hot_col(col='Stofa_6', type='dropdown', source=c('Fyrir hádegi','Eftir hádegi','Heill dagur','-'))%>%
      hot_col(col='Stofa_7', type='dropdown', source=c('Fyrir hádegi','Eftir hádegi','Heill dagur','-'))%>%
      hot_col(col='Stofa_8', type='dropdown', source=c('Fyrir hádegi','Eftir hádegi','Heill dagur','Ekki nota'))
  }) 

  
  

  

#--------------------Elective Tab----------------------#
  
  output$acuteElectivePlot <- renderPlot({
    
    case = surgerycode[as.numeric(input$select)]
    agegroup = as.numeric(input$aldur)
    
    sex = "both"
    # the value 505 correponds to 3 weeks 21*24
    idx = PatientTimeInLegudeild >= 0 & SurgicalCaseCode == case & AcuteElective == "Acute" & Gender != sex
    idx0 = PatientTimeInLegudeild == 0 & SurgicalCaseCode == case & AcuteElective == "Acute" & Gender != sex
    LeguVnotleguA = c(sum(idx),sum(idx0))
    PTa = PatientDateInLegudeild[idx]
    n = length(PTa)
    Ma = matrix(ncol=5, nrow = n)
    Ma[,1] = Age[idx]
    Ma[,2] = PatientTimeInLegudeild[idx]
    Ma[,3] = PatientDateInGjorgaesla[idx]
    Ma[,4] = op_count[idx]
    OrStartsA = ymd_hms(OperationBegins[idx])
    
    Ga = Gender[idx]
    
    # data split up into 4 age groups [0 240]
    genderRatioA = matrix(nrow = 10, ncol = 2)
    for (i in c(0:9)) {
      id = Ma[,1]>=10*i & Ma[,1]<10*(i+1)
      iid = which(idx)
      genderRatioA[i+1,1] = sum(Gender[iid[id]] == 'Male')
      genderRatioA[i+1,2] = sum(Gender[iid[id]] == 'Female')
    }
    # Elective cases in Legudeild
    
    
    idx = PatientTimeInLegudeild >= 0 & SurgicalCaseCode == case & AcuteElective == "Elective" & Gender != sex
    idx0 = PatientTimeInLegudeild == 0 & SurgicalCaseCode == case & AcuteElective == "Acute" & Gender != sex
    LeguVnotleguE = c(sum(idx),sum(idx0))
    PTe = PatientDateInLegudeild[idx]
    n = length(PTe)
    Me = matrix(ncol = 5, nrow = n)
    Me[,1] = Age[idx]
    Me[,2] = PatientTimeInLegudeild[idx]
    Me[,3] = PatientTimeInGjorgaesla[idx]
    Me[,4] = op_count[idx]
    OrStartsE = ymd_hms(OperationBegins[idx])
    
    Ge = Gender[idx]
    genderRatioE = matrix(nrow = 10, ncol = 2)
    
    for (i in c(0:9)) {
      id = Me[,1]>=10*i & Me[,1]<10*(i+1)
      iid = which(idx)
      genderRatioE[i+1,1] = sum(Gender[iid[id]] == 'Male')
      genderRatioE[i+1,2] = sum(Gender[iid[id]] == 'Female')
    }
    M = rbind(Ma,Me)
    DayGroup = cut(M[,2]/24,breaks=seq(0,21), labels = as.character(seq(1,21)))
    AgeGroup = cut(M[,1],breaks=seq(0,100,10))
    PT = c(PTa,PTe)
    ORStarts = c(OrStartsA,OrStartsE)

    Mdat = data.frame(Vikudagur = weekdays(ORStarts), ORStartTime = ORStarts, Age=M[,1],AgeGroup = AgeGroup, NumOR = M[,4], ICUHours=M[,3], WardHours=M[,2],Type=c(rep("Acute",nrow(Ma)),rep("Elective",nrow(Me))),Gender = as.factor(c(as.character(Ga),as.character(Ge))), WardDay = DayGroup, Date = PT)
    
    vikudagar = c("sunnudagur","mánudagur","þriðjudagur","miðvikudagur","fimmtudagur","föstudagur","laugardagur")
    vikudagurcountE = rep(0,7)
    vikudagurcountA = rep(0,7)
    for (i in c(1:7)) {
      vikudagurcountA[i] = sum(i==wday(Mdat$ORStartTime[Mdat$Type=="Acute"]))
      vikudagurcountE[i] = sum(i==wday(Mdat$ORStartTime[Mdat$Type=="Elective"]))  
    }
    #    #barplot(vikudagurcountE,main=c("fjöldi aðgerða (Elective)"),names.arg = vikudagar)
    #    #barplot(vikudagurcountA,main=c("fjöldi aðgerða (Acute)"),names.arg = vikudagar)
    Tegund = (c(rep("Acute",7),rep("Elective",7)))
    gognA = data.frame(Dagur=vikudagar, Fjoldi = vikudagurcountA, Tegund = Tegund[1:7])
    gognE = data.frame(Dagur=vikudagar, Fjoldi = vikudagurcountE, Tegund = Tegund[8:14])
    gogn = rbind(gognA,gognE)

    ggplot(gogn,aes(Dagur,Fjoldi,fill=Tegund)) + geom_bar(stat="identity", width=.5, position = "dodge") + scale_x_discrete(limits = vikudagar)

  })
  
  
  #-------------------------WARD STAY-----------------------------------------#
  output$leguPlot <- renderPlot({
    
    case = surgerycode[as.numeric(input$select)]
    agegroup = as.numeric(input$aldur)
    legulengd = 24*21
    idx = PatientTimeInLegudeild > 0 & PatientTimeInLegudeild<=legulengd & SurgicalCaseCode == case
    idx0 = PatientTimeInLegudeild == 0 & SurgicalCaseCode == case
    OrStarts = ymd_hms(OperationBegins[idx])
    
    leguratio = c(sum(idx0),sum(SurgicalCaseCode == case))
    Ma = matrix(ncol=2, nrow = sum(idx))
    Ma[,1] = Age[idx]
    Ma[,2] = PatientTimeInLegudeild[idx]
    
    # data split up into 4 age groups [0 240]
    if (agegroup > 1) {
      id = Ma[,1]>=10*(agegroup-0) & Ma[,1]<25*(agegroup-1)
    }
    else {
      id = c(1:nrow(Ma));
    }
    #     iid = which(idx)
    #      if (nrow(Ma) > 0) {
    #        hist(Ma[id,2]/24,breaks=seq(0,legulengd/24),main=paste('Acute Age[', 10*i,',',10*(i+1),')'))
    #      }
    #    }
    if (nrow(Ma) > 0) {
      hist(Ma[id,2]/24,breaks=seq(0,21),main=sprintf("%.1f%% aðgerða fara á legudeild", (1-leguratio[1]/leguratio[2])*100), xlab = "Dagar", ylab = "Fjöldi") 
    }
    
  })
  
  
  #---------------------------Intensive Care Unit-------------------------------#
  output$gjorPlot <- renderPlot({
    
    case = surgerycode[as.numeric(input$select)]
    agegroup = as.numeric(input$aldur)
    legulengd = 24*21
    idx = PatientTimeInGjorgaesla > 0 & PatientTimeInGjorgaesla<=legulengd & SurgicalCaseCode == case
    if (sum(idx)>0) {
      idx0 = PatientTimeInGjorgaesla == 0 & SurgicalCaseCode == case
      OrStarts = ymd_hms(OperationBegins[idx])
      gjorratio = c(sum(idx0),sum(SurgicalCaseCode == case))
      
      LeguVnotgjorA = c(sum(idx),sum(idx0))
      Ma = matrix(ncol=2, nrow = sum(idx))
      Ma[,1] = Age[idx]
      Ma[,2] = PatientTimeInGjorgaesla[idx]
      
      # data split up into 4 age groups [0 240]
      #    for (i in c(0:9)) {
      if (agegroup > 1) {
        id = Ma[,1]>=10*(agegroup-0) & Ma[,1]<25*(agegroup-1)
      }
      else {
        id = c(1:nrow(Ma));
      }
      #     iid = which(idx)
      #      if (nrow(Ma) > 0) {
      #        hist(Ma[id,2]/24,breaks=seq(0,legulengd/24),main=paste('Acute Age[', 10*i,',',10*(i+1),')'))
      #      }
      #    }
      if (nrow(Ma) > 0) {
        hist(Ma[id,2]/24,breaks=seq(0,21), main=sprintf("%.1f%% aðgerða fara á gjörgæslu", (1-gjorratio[1]/gjorratio[2])*100), xlab = "Dagar", ylab = "Fjöldi") 
      }
    }
  })
  
  output$skurdPlot <- renderPlot({
    
    case = surgerycode[as.numeric(input$select)]
     
    idxA = SurgicalCaseCode == case & AcuteElective == "Acute" 
    idxE = SurgicalCaseCode == case & AcuteElective == "Elective"
    
    OrStartsA = hour(ymd_hms(OperationBegins[idxA]))
    OrStartsE = hour(ymd_hms(OperationBegins[idxE]))
    NumA = rep(0,24)
    NumE = rep(0,24)
    for (i in c(1:24)) {
      NumA[i] = sum(OrStartsA==(i-1))
      NumE[i] = sum(OrStartsE==(i-1))
    }
    NumA = NumA/sum(NumA)
    NumE = NumE/sum(NumE)
    
    gogn = data.frame(Klukkustund=c(seq(0,23),seq(0,23)), Tíðni = c(NumA,NumE), Tegund = c(rep('Acute',24),rep('Elective',24)))

    ggplot(gogn,aes(Klukkustund,Tíðni,fill=Tegund)) +ggtitle('Klukkustund þar sem aðgerð hefst') + geom_bar(stat="identity", width=.5, position = "dodge") + scale_x_discrete(limits = seq(0,23))
    
   
  })
  
  output$fjoldiadgerdaPlot <- renderPlot({
    
    case = surgerycode[as.numeric(input$select)]
    idxE = SurgicalCaseCode == case & AcuteElective == "Elective"
    idxA = SurgicalCaseCode == case & AcuteElective == "Acute"
    OrStartsE = sort(ymd_hms(OperationBegins[idxE]))
    OrStartsA = sort(ymd_hms(OperationBegins[idxA]))
    
    plot(OrStartsE,seq(1,length(OrStartsE)),ylim=c(0,max(length(OrStartsE),length(OrStartsA))),col="blue", main="Uppsafnaður fjöldi aðgerða frá 2007", xlab="Ár", ylab="Fjöldi")
    if (sum(idxA)>0) {
      points(OrStartsA,seq(1,length(OrStartsA)),col="red")
    }
  })
  
  
  
  output$legutimePlot <- renderPlot({
    
    case = surgerycode[as.numeric(input$select)]
    legulengd = 24*21
    idxE = PatientTimeInLegudeild > 0 & PatientTimeInLegudeild<=legulengd & SurgicalCaseCode == case & AcuteElective == "Elective"
    idxA = PatientTimeInLegudeild > 0 & PatientTimeInLegudeild<=legulengd & SurgicalCaseCode == case & AcuteElective == "Acute"
    
    plot(ymd_hms(OperationBegins[idxE]),PatientTimeInLegudeild[idxE]/24,ylim=c(0, 7),col="blue",main="Legutími - Blátt er Elective, Rautt er Acute", xlab="Ár", ylab="klukkustundir")
    points(ymd_hms(OperationBegins[idxA]),PatientTimeInLegudeild[idxA]/24,ylim=c(0, 7),col="red")
    
  })
  
  output$gjortimePlot <- renderPlot({
    
    case = surgerycode[as.numeric(input$select)]
    legulengd = 24*21
    idxE = PatientTimeInGjorgaesla > 0 & PatientTimeInGjorgaesla<=legulengd & SurgicalCaseCode == case & AcuteElective == "Elective"
    idxA = PatientTimeInGjorgaesla > 0 & PatientTimeInGjorgaesla<=legulengd & SurgicalCaseCode == case & AcuteElective == "Acute"
    
    plot(ymd_hms(OperationBegins[idxE]),PatientTimeInGjorgaesla[idxE]/24,ylim=c(0, 7),col="blue",main="Gjörgæslutími - Blátt er Elective, Rautt er Acute", xlab="Ár", ylab="klukkustundir")
    points(ymd_hms(OperationBegins[idxA]),PatientTimeInGjorgaesla[idxA]/24,ylim=c(0, 7),col="red")
    
  })
  
  
  output$click <- renderPrint({
    d <- event_data("plotly_click")
    if (is.null(d)) "Click on a state to view event data" else c(d$x,d$y)
    
  })
  
  output$laekPlot <- renderPlotly({
    selected = surgerycode[as.numeric(input$select)]
    dat <- LaeknirAdgerd[LaeknirAdgerd[,selected] > 0]
    ops <- names(LaeknirAdgerd)
    laek = character(0)
    surg = character(0)
    size = numeric(0)
    for (l in rownames(dat)) {
      s <- which(LaeknirAdgerd[l,]>0)
      if (LaeknirAdgerd[l,selected]) {
        laek <- c(laek,rep(l,length(s)))
        surg <- c(surg,ops[s])
        size <- c(size,as.numeric(LaeknirAdgerd[l,s]))
      }
    }
    d <- NULL
    d$Laeknir <- (as.factor(laek))
    d$Adgerd <- (as.factor(surg))
    d$Fjoldi <- size
    d$Valið = rep("Annað",length(surg))
    d$Valið[surg==selected] = selected
    d$Valið = as.factor(d$Valið)
    d <- as.data.frame(d)
    fill = c("steelblue","violetred1")
    p <- ggplot(d, aes(x = Adgerd, y = Laeknir, size = Fjoldi, fill = Valið)) +
      geom_point(shape = 21) + scale_fill_manual(values = fill) + 
      xlab('Aðgerð') + ylab('Aðalskurðlæknir') + 
      theme(text = element_text(family = "Calibri", size =10),
            axis.text.x=element_blank(),
            axis.ticks.x=element_blank(),legend.position="none")
    
    pp <- ggplotly(p,tooltip = c("Adgerd", "Laeknir", "Fjoldi")) %>% layout(height = 800, width = 1200)
    pp
  })
})

