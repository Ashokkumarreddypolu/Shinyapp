
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)

shinyServer(function(input, output) {
    
    data <- reactive({
        file1 <- input$file1
        if(is.null(file1)){return()} 
        data <- read.csv(file=file1$datapath,header=T,stringsAsFactors = F) 
    })
    data_pred <- reactive({
        file2 <- input$file2
        if(is.null(file2)){return()} 
        data <- read.csv(file=file2$datapath,header=T,stringsAsFactors = F) 
    })
    var <- reactive({
        file1 <- input$file1
        if(is.null(file1)){return()} 
        data <- read.csv(file=file1$datapath)
        x=ncol(data)
        x
    })
    
    var_names <- reactive({
        file1 <- input$file1
        if(is.null(file1)){return()} 
        data <- read.csv(file=file1$datapath)
        x=names(data)
        x
    })
    output$topN <- renderUI({
        file1 <- input$file1
        if(is.null(file1)){return()} 
        y=var()
        sliderInput("topN","select number Top N predictors",min=1,max=y,value=2,step=1)
    })
  
    output$var1 <- renderUI({ 
        file1 <- input$file1
        if(is.null(file1)){return()} 
        selectInput("varn","select the variable to be predicted",choices=var_names(),selected = NaN,multiple = F)
        })
    
    test<-eventReactive(input$go,{
     
        if(is.null(data())){return()} 
        z=data()
        y=input$varn
        x=colnames(z)
        train=z
        p=numeric(ncol(train))
        for (i in 1:ncol(train))
        {
            if(x[i]!=y)
            {
                model=lm(data=train,train[,y]~train[,i])
                p[i]=summary(model)$coeff[2,4]
            }
            else
            {
                p[i]=100
            }
        }
        order1=order(p)
        x1=order1[1:input$topN]
        finaln=x[x1]
        final_data=train[,c(finaln,y)]
        final_model<-lm(final_data[,ncol(final_data)]~.,data=final_data)
       
        final_model
    })
    
    output$Rl <-renderTable({
        
        if(is.null(test())){return()} 
        z=data()
        y=input$varn
        x=colnames(z)
        train=z
        p=numeric(ncol(train))
        for (i in 1:ncol(train))
        {
            if(x[i]!=y)
            {
                model=lm(data=train,train[,y]~train[,i])
                p[i]=summary(model)$coeff[2,4]
            }
            else
            {
                p[i]=100
            }
        }
        order1=order(p)
        x1=order1[1:input$topN]
        finaln=x[x1]
      finaln
        
    })
    
 


output$pred1<-renderTable({
    if(is.null(test())){return()} 
    model=test()
    z=data()
    y=input$varn
    x=colnames(z)
    train=z
    p=numeric(ncol(train))
    for (i in 1:ncol(train))
    {
        if(x[i]!=y)
        {
            model=lm(data=train,train[,y]~train[,i])
            p[i]=summary(model)$coeff[2,4]
        }
        else
        {
            p[i]=100
        }
    }
    order1=order(p)
    x1=order1[1:input$topN]
    finaln=x[x1]
    final_data=train[,c(finaln,y)]
    data=data_pred()
    data1=data[,colnames(finaln)]
    z=predict(model,data1)
    final=cbind(data,z)
    colnames(final)[ncol(final)]=c("Predicted")
    final
    
    
})

})
