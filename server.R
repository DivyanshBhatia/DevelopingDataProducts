library(shiny)



shinyServer(function(input,output){
 
 SexCount_y <- reactive({
 TitanicFrame <- as.data.frame(Titanic)
 i <- 0
 YesCount <- 0
 while(i < nrow(TitanicFrame)){
	if(as.logical(paste(TitanicFrame[i,2],"")==paste(input$sex,"")))
	{
	if(as.logical(paste(TitanicFrame[i,4],"")==paste("Yes","")))
	YesCount <- YesCount+as.numeric(paste(TitanicFrame[i,5],""))
	
	}
	#print(i)
	i <- i+1
	}
 return(data.frame(YesCount,YesCount))
 })
 
 
 SexCount_n <- reactive({
 TitanicFrame <- as.data.frame(Titanic)
 i <- 0
 NoCount <- 0
 while(i < nrow(TitanicFrame)){
	if(as.logical(paste(TitanicFrame[i,2],"")==paste(input$sex,"")))
	{
	if(as.logical(paste(TitanicFrame[i,4],"")==paste("No","")))
	NoCount <- NoCount+as.numeric(paste(TitanicFrame[i,5],""))
	
	}
	#print(i)
	i <- i+1
	}
 return(data.frame(NoCount,NoCount))
 })
 
 
 ClassCount_y <- reactive({
 TitanicFrame <- as.data.frame(Titanic)
 i <- 0
 YesCount <- 0
 while(i < nrow(TitanicFrame)){
	if(as.logical((paste(TitanicFrame[i,1],"")==paste(input$class,""))))
	{
	if(as.logical(paste(TitanicFrame[i,4],"")==paste("Yes","")))
	YesCount <- YesCount+as.numeric(TitanicFrame[i,5])
	
	}
	#print(i)
	i <- i+1
	}
 return(data.frame(YesCount,YesCount))
 })
 
 ClassCount_n <- reactive({
 TitanicFrame <- as.data.frame(Titanic)
 i <- 0
 NoCount <- 0
 while(i < nrow(TitanicFrame)){
	if(as.logical(paste(TitanicFrame[i,1],"")==paste(input$class,"")))
	{
	if(as.logical(paste(TitanicFrame[i,4],"")==paste("No","")))
	NoCount <- NoCount+as.numeric(paste(TitanicFrame[i,5],""))
	
	}
	#print(i)
	i <- i+1
	}
 return(data.frame(NoCount,NoCount))
 })
 
 AgeCount_y <- reactive({
 TitanicFrame <- as.data.frame(Titanic)
 i <- 0
 YesCount <- 0
 while(i < nrow(TitanicFrame)){
	if(as.logical(paste(TitanicFrame[i,3],"")==paste(input$age,"")))
	{
	if(as.logical(paste(TitanicFrame[i,4],"")==paste("Yes","")))
	YesCount <- YesCount+as.numeric(paste(TitanicFrame[i,5],""))
	
	}
	#print(i)
	i <- i+1
	}
 return(data.frame(YesCount,YesCount))
 })
 
 AgeCount_n <- reactive({
 TitanicFrame <- as.data.frame(Titanic)
 i <- 0
 NoCount <- 0
 while(i < nrow(TitanicFrame)){
	if(as.logical(paste(TitanicFrame[i,3],"")==paste(input$age,"")))
	{
	if(as.logical(paste(TitanicFrame[i,4],"")==paste("No","")))
	NoCount <- NoCount+as.numeric(paste(TitanicFrame[i,5],""))
	}
	#print(i)
	i <- i+1
	}
 return(data.frame(NoCount,NoCount))
 })
 output$class <- renderPrint({input$class})
 output$class_y <- renderPrint(print(ClassCount_y()$YesCount))
 output$class_n <- renderPrint(ClassCount_n()$NoCount[1])
 
 output$sex <- renderPrint({input$sex})
 output$sex_y <- renderPrint(print(SexCount_y()$YesCount))
 output$sex_n <- renderPrint(SexCount_y()$NoCount[1])
 
 output$age <- renderPrint({input$age})
 output$age_y <- renderPrint(print(AgeCount_y()$YesCount))
 output$age_n <- renderPrint(AgeCount_y()$NoCount[1])
 
 Table_Frame <- reactive({
 #data1 <- data.frame()
 #data1 <- names(c("Variables","YesCount","NoCount"))
 #if(input$class!=NA)
 data1 <- cbind("Class",input$class,ClassCount_y()$YesCount[1],ClassCount_n()$NoCount[1])
 #if(input$sex!=NA)
 data1 <- rbind(data1,cbind("Sex",input$sex,SexCount_y()$YesCount[1],SexCount_n()$NoCount[1]))
 #if(input$age!=NA)
 data1 <- rbind(data1,cbind("Age",input$age,AgeCount_y()$YesCount[1],AgeCount_n()$NoCount[1]))
 data1 <- data.frame(data1)
 print(colnames(data1))
 colnames(data1) <- c('Variable','Value','YesCount','NoCount')
 return(list(data1 = data1))
 })
 
 
 computeProbability <- reactive({
	yes_final <- 1.0
	no_final <- 1.0
	if(as.numeric(ClassCount_y()$YesCount[1]) > 0 || as.numeric(ClassCount_n()$NoCount[1]) > 0)
	{
	yes_class <- as.numeric(ClassCount_y()$YesCount[1])/(as.numeric(ClassCount_y()$YesCount[1])+as.numeric(ClassCount_n()$NoCount[1]))
	no_class <- 1 - yes_class
	if(yes_class == 0){
	yes_class <- 1/(1+as.numeric(ClassCount_n()$NoCount[1]))
	}
	if(no_class == 0){
	no_class <- 1/(1+as.numeric(ClassCount_y()$YesCount[1]))
	}
	yes_final <- yes_final*yes_class
	no_final <- no_final*no_class
	}
	#print(yes_final)
	if(as.numeric(SexCount_y()$YesCount[1]) > 0 || as.numeric(SexCount_n()$NoCount[1]) > 0)
	{
	yes_sex <- as.numeric(SexCount_y()$YesCount[1])/(as.numeric(SexCount_y()$YesCount[1])+as.numeric(SexCount_n()$NoCount[1]))
	no_sex <- 1 - yes_sex
	if(yes_sex == 0){
	yes_sex <- 1/(1+as.numeric(SexCount_n()$NoCount[1]))
	}
	if(no_sex == 0){
	no_sex <- 1/(1+as.numeric(SexCount_y()$YesCount[1]))
	}
	yes_final <- yes_final*yes_sex
	no_final <- no_final*no_sex
	}
	#print(yes_final)
	if(as.numeric(AgeCount_y()$YesCount[1]) > 0 || as.numeric(AgeCount_n()$NoCount[1]) > 0)
	{
	yes_age <- as.numeric(AgeCount_y()$YesCount[1])/(as.numeric(AgeCount_y()$YesCount[1])+as.numeric(AgeCount_n()$NoCount[1]))
	no_age <- as.numeric(AgeCount_n()$NoCount[1])/(as.numeric(AgeCount_y()$YesCount[1])+as.numeric(AgeCount_n()$NoCount[1]))
	
	if(yes_age == 0){
	yes_age <- 1/(1+as.numeric(AgeCount_n()$NoCount[1]))
	}
	if(no_age == 0){
	no_age <- 1/(1+as.numeric(AgeCount_y()$AgeCount[1]))
	}
	yes_final <- yes_final*yes_age
	no_final <- no_final*no_age
	}
	#print(yes_final)
	
	result_data <- data.frame(cbind(c(yes_final),c(no_final)))
	colnames(result_data) <- c("Chances of Survival","Chances that Person has Not Survived")
	return(list(result_data=result_data))
 })
 output$computetable <- renderDataTable({Table_Frame()$data1})
 output$YesProbability <- renderPrint({computeProbability()$result_data[1]})
 output$NoProbability <- renderPrint({computeProbability()$result_data[2]})
 
 })