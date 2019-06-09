#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(plyr)
library(corrplot)
library(ggplot2)
library(gridExtra)
library(ggthemes)
library(caret)
library(MASS)
library(randomForest)
library(party)
library(DT)

churn <- read.csv('Telco-Customer-Churn.csv')

# Usamos sapply para verificar o número de valores ausentes (missing) em cada coluna. 
# Descobrimos que há 11 valores ausentes nas colunas "TotalCharges". 
# Então, vamos remover todas as linhas com valores ausentes.
sapply(churn, function(x) sum(is.na(x)))
churn <- churn[complete.cases(churn), ]

cols_recode1 <- c(10:15)
for(i in 1:ncol(churn[,cols_recode1])) {
  churn[,cols_recode1][,i] <- as.factor(mapvalues
                                        (churn[,cols_recode1][,i], from =c("No internet service"),to=c("No")))
}

# 2. Vamos mudar "No phone service" para "No" para a coluna “MultipleLines”
churn$MultipleLines <- as.factor(mapvalues(churn$MultipleLines, 
                                           from=c("No phone service"),
                                           to=c("No")))

group_tenure <- function(tenure){
  if (tenure >= 0 & tenure <= 12){
    return('0-12 Month')
  }else if(tenure > 12 & tenure <= 24){
    return('12-24 Month')
  }else if (tenure > 24 & tenure <= 48){
    return('24-48 Month')
  }else if (tenure > 48 & tenure <=60){
    return('48-60 Month')
  }else if (tenure > 60){
    return('> 60 Month')
  }
}

churn$tenure_group <- sapply(churn$tenure,group_tenure)
churn$tenure_group <- as.factor(churn$tenure_group)

# 4. Alteramos os valores na coluna “SeniorCitizen” de 0 ou 1 para “No” ou “Yes”.
churn$SeniorCitizen <- as.factor(mapvalues(churn$SeniorCitizen,
                                           from=c("0","1"),
                                           to=c("No", "Yes")))

# 5. Removemos as colunas que não precisamos para a análise.
churn$customerID <- NULL
churn$tenure <- NULL


################################################

ui <- fluidPage(
  titlePanel("Analise de Churn preditivo - TELCO"),
  sidebarLayout(
    sidebarPanel(
      label_value("Filtros"),
      checkboxGroupInput("sexoInput", "Sexo",
                   choices = unique(churn$gender),
                   selected = unique(churn$gender)),
      checkboxGroupInput("internetServiceInput", "Servico de Internet",
                         choices = unique(churn$InternetService),
                         selected = unique(churn$InternetService)),
      checkboxGroupInput("tenureGroupInput", "Prazo Contrato",
                         choices = unique(churn$tenure_group),
                         selected = unique(churn$tenure_group))
      
      ),
      mainPanel(
        
        tabsetPanel(type = "tabs",
                    tabPanel("Dados", br(),DTOutput('tbl')),
                    tabPanel("Corr (var numericas)", plotOutput("corplot")),
                    tabPanel("Dist (var categoricas)", 
                             plotOutput("barplot1"),
                             br(), br(),
                             plotOutput("barplot2"),
                             br(), br(),
                             plotOutput("barplot3"),
                             br(), br(),
                             plotOutput("barplot4")
                             ),
                    tabPanel("Sumario", verbatimTextOutput("sumario")),
                    tabPanel("Dados Treino e Teste",
                             br(),
                             tabsetPanel(type = "tabs",
                                         tabPanel("Treino", DTOutput('treino')),
                                         tabPanel("Teste", DTOutput('teste'))
                                         )
                             ),
                    tabPanel("Machine Learning", verbatimTextOutput("glm"),
                             br(),
                             verbatimTextOutput("anova")),
                    tabPanel("Arvore de Decisao", plotOutput("arvore"))
                    
        )        
      )
  )
)

server <- function(input, output) {
  output$corplot <- renderPlot({
    
    churn_filtro <- subset(churn,churn$gender == unlist(input$sexoInput) & 
                             churn$InternetService == unlist(input$internetServiceInput) &
                             churn$tenure_group == unlist(input$tenureGroupInput))
    
    numeric.var <- sapply(churn_filtro, is.numeric)
    corr.matrix <- cor(churn_filtro[,numeric.var])
    
    corrplot(corr.matrix, main="\n\nCorrelação entre as Variáveis Numéricas", method="number")
    churn_filtro$TotalCharges <- NULL
    
  })
  output$barplot1 <- renderPlot({

    # Gráficos de barra de variáveis categóricas
    p1 <- ggplot(churn, aes(x=gender)) + ggtitle("Gender") + xlab("Sexo") +
      geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentual") + coord_flip() + theme_minimal()
    p2 <- ggplot(churn, aes(x=SeniorCitizen)) + ggtitle("Senior Citizen") + xlab("Senior Citizen") + 
      geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentual") + coord_flip() + theme_minimal()
    p3 <- ggplot(churn, aes(x=Partner)) + ggtitle("Partner") + xlab("Parceiros") + 
      geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentual") + coord_flip() + theme_minimal()
    p4 <- ggplot(churn, aes(x=Dependents)) + ggtitle("Dependents") + xlab("Dependentes") +
      geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentual") + coord_flip() + theme_minimal()
    
    
    grid.arrange(p1, p2, p3, p4, ncol=2)
    
  })
  
  output$arvore <- renderPlot({
    
    plot(ctree(Churn ~ Contract+tenure_group+PaperlessBilling, 
               subset(churn,churn$gender == unlist(input$sexoInput) & 
                        churn$InternetService == unlist(input$internetServiceInput) &
                        churn$tenure_group == unlist(input$tenureGroupInput))[createDataPartition(subset(churn,churn$gender == unlist(input$sexoInput) & 
                                                                                                           churn$InternetService == unlist(input$internetServiceInput) &
                                                                                                           churn$tenure_group == unlist(input$tenureGroupInput))$Churn,p=0.7,list=FALSE),]               
               ), type='simple')
  })  
  
  output$barplot2 <- renderPlot({
    
    p5 <- ggplot(churn, aes(x=PhoneService)) + ggtitle("Phone Service") + xlab("Telefonia") +
      geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentual") + coord_flip() + theme_minimal()
    p6 <- ggplot(churn, aes(x=MultipleLines)) + ggtitle("Multiple Lines") + xlab("Múltiplas Linhas") + 
      geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentual") + coord_flip() + theme_minimal()
    p7 <- ggplot(churn, aes(x=InternetService)) + ggtitle("Internet Service") + xlab("Internet Service") + 
      geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentual") + coord_flip() + theme_minimal()
    p8 <- ggplot(churn, aes(x=OnlineSecurity)) + ggtitle("Online Security") + xlab("Online Security") +
      geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentual") + coord_flip() + theme_minimal()
    grid.arrange(p5, p6, p7, p8, ncol=2)
    
  })
  
  output$barplot3 <- renderPlot({
    
    p9 <- ggplot(churn, aes(x=OnlineBackup)) + ggtitle("Online Backup") + xlab("Online Backup") +
      geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentual") + coord_flip() + theme_minimal()
    p10 <- ggplot(churn, aes(x=DeviceProtection)) + ggtitle("Device Protection") + xlab("Device Protection") + 
      geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentual") + coord_flip() + theme_minimal()
    p11 <- ggplot(churn, aes(x=TechSupport)) + ggtitle("Tech Support") + xlab("Tech Support") + 
      geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentual") + coord_flip() + theme_minimal()
    p12 <- ggplot(churn, aes(x=StreamingTV)) + ggtitle("Streaming TV") + xlab("Streaming TV") +
      geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentual") + coord_flip() + theme_minimal()
    grid.arrange(p9, p10, p11, p12, ncol=2)
    
  })
  
  output$barplot4 <- renderPlot({
    
    p13 <- ggplot(churn, aes(x=StreamingMovies)) + ggtitle("Streaming Movies") + xlab("Streaming Movies") +
      geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentual") + coord_flip() + theme_minimal()
    p14 <- ggplot(churn, aes(x=Contract)) + ggtitle("Contract") + xlab("Contract") + 
      geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentual") + coord_flip() + theme_minimal()
    p15 <- ggplot(churn, aes(x=PaperlessBilling)) + ggtitle("Paperless Billing") + xlab("Paperless Billing") + 
      geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentual") + coord_flip() + theme_minimal()
    p16 <- ggplot(churn, aes(x=PaymentMethod)) + ggtitle("Payment Method") + xlab("Payment Method") +
      geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentual") + coord_flip() + theme_minimal()
    p17 <- ggplot(churn, aes(x=tenure_group)) + ggtitle("Tenure Group") + xlab("Tenure Group") +
      geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentual") + coord_flip() + theme_minimal()
    grid.arrange(p13, p14, p15, p16, p17, ncol=2)
    
  })
  output$tbl = renderDT(
    subset(churn,churn$gender == unlist(input$sexoInput) & 
                             churn$InternetService == unlist(input$internetServiceInput) &
                             churn$tenure_group == unlist(input$tenureGroupInput))
    , 
    colnames = c(ID = 1),
    style = 'bootstrap',
    class = 'table table-condensed table-hover',
    options = list(
                      pageLength = 50,
                      dom = 'Bfrtip', 
                      buttons = c('copy', 'csv', 'excel','pdf','print', I('colvis')), 
                      colReorder = TRUE,
                      scrollX = TRUE,
                      keys = TRUE,
                      rowReorder = TRUE,
                      deferRender = TRUE,
                      scroller = TRUE,
                      scrollY = 300,
                      order = list(c(0, 'asc')),
                      fixedColumns = list(leftColumns = 2, rightColuns = 1)
    ), 
    extensions = c('Buttons', 'ColReorder','FixedColumns','KeyTable','RowReorder','Scroller')
  )
  
  output$treino = renderDT(
    
    subset(churn,churn$gender == unlist(input$sexoInput) & 
             churn$InternetService == unlist(input$internetServiceInput) &
             churn$tenure_group == unlist(input$tenureGroupInput))[createDataPartition(subset(churn,churn$gender == unlist(input$sexoInput) & 
                                            churn$InternetService == unlist(input$internetServiceInput) &
                                            churn$tenure_group == unlist(input$tenureGroupInput))$Churn,p=0.7,list=FALSE),],
    colnames = c(ID = 1),
    style = 'bootstrap',
    class = 'table table-condensed table-hover',
    options = list(
      pageLength = 50,
      dom = 'Bfrtip', 
      buttons = c('copy', 'csv', 'excel','pdf','print', I('colvis')), 
      colReorder = TRUE,
      scrollX = TRUE,
      keys = TRUE,
      rowReorder = TRUE,
      deferRender = TRUE,
      scroller = TRUE,
      scrollY = 300,
      order = list(c(0, 'asc')),
      fixedColumns = list(leftColumns = 2, rightColuns = 1)
    ), 
    extensions = c('Buttons', 'ColReorder','FixedColumns','KeyTable','RowReorder','Scroller')
  )  
  
  output$teste = renderDT(
    
    subset(churn,churn$gender == unlist(input$sexoInput) & 
             churn$InternetService == unlist(input$internetServiceInput) &
             churn$tenure_group == unlist(input$tenureGroupInput))[-createDataPartition(subset(churn,churn$gender == unlist(input$sexoInput) & 
                                       churn$InternetService == unlist(input$internetServiceInput) &
                                       churn$tenure_group == unlist(input$tenureGroupInput))$Churn,p=0.7,list=FALSE),],
    colnames = c(ID = 1),
    style = 'bootstrap',
    class = 'table table-condensed table-hover',
    options = list(
      pageLength = 50,
      dom = 'Bfrtip', 
      buttons = c('copy', 'csv', 'excel','pdf','print', I('colvis')), 
      colReorder = TRUE,
      scrollX = TRUE,
      keys = TRUE,
      rowReorder = TRUE,
      deferRender = TRUE,
      scroller = TRUE,
      scrollY = 300,
      order = list(c(0, 'asc')),
      fixedColumns = list(leftColumns = 2, rightColuns = 1)
    ), 
    extensions = c('Buttons', 'ColReorder','FixedColumns','KeyTable','RowReorder','Scroller')
  )    

  output$sumario <- renderPrint({
    str(subset(churn,churn$gender == unlist(input$sexoInput) & 
                 churn$InternetService == unlist(input$internetServiceInput) &
                 churn$tenure_group == unlist(input$tenureGroupInput)))
  })
  
  output$glm <- renderPrint({
    summary(
      glm(Churn ~ ., family=binomial(link="logit"), data=    subset(churn,churn$gender == unlist(input$sexoInput) & 
                                                                            churn$InternetService == unlist(input$internetServiceInput) &
                                                                            churn$tenure_group == unlist(input$tenureGroupInput))[createDataPartition(subset(churn,churn$gender == unlist(input$sexoInput) & 
                                                                                                                                                               churn$InternetService == unlist(input$internetServiceInput) &
                                                                                                                                                               churn$tenure_group == unlist(input$tenureGroupInput))$Churn,p=0.7,list=FALSE),]))
  })

  output$anova <- renderPrint({
    anova(
    glm(Churn ~ ., family=binomial(link="logit"), data=    subset(churn,churn$gender == unlist(input$sexoInput) & 
                                                                    churn$InternetService == unlist(input$internetServiceInput) &
                                                                    churn$tenure_group == unlist(input$tenureGroupInput))[createDataPartition(subset(churn,churn$gender == unlist(input$sexoInput) & 
                                                                                                                                                       churn$InternetService == unlist(input$internetServiceInput) &
                                                                                                                                                       churn$tenure_group == unlist(input$tenureGroupInput))$Churn,p=0.7,list=FALSE),])
    ,test="Chisq")  
  })
  
}

shinyApp(ui = ui, server = server)