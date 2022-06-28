library(shiny)
library(ggplot2)
library(shinyWidgets)
library(knitr)
library(haven)
library(shinythemes)

base_filter=read_dta("D:\\COURS_ENSAE\\ISEP2\\Semestre 4\\R\\Projet ISEP2 2022\\Section 14\\base_filter.dta")
l=as.list(unique(base_filter[,2]))
ui <- fluidPage (theme = shinytheme('united'),
                 navbarPage(
  tabPanel("Home","Bienvenue sur l'application Shiny d'Alex et Ousseynou"),

  tabPanel("ASPECTS DE LA BASE",
  textOutput(outputId = "Text2"),
  
  numericInput(
    inputId = "rows_n",
    label="S'il vous plait entrez le nombre de lignes à voir",
    value=5,
    min=1,
    max=nrow(base_1_ut)),
  
 
  textOutput(outputId = "Text"),
  tableOutput(outputId = "tbl1")),
  
  tabPanel("INFORMATIONS CAPITALES",
           mainPanel(
  textOutput(outputId = "Text3"),
  
  h4(" 1- Nombre de personnes par problèmes survenues"),
  selectInput(
    inputId = "Colors",
    label = "Please select a color",
    choices = colors(),
    selected = "blue"
  ),
  
  plotOutput(outputId = "plt1"),
  
  tableOutput(outputId = "tbl2"),
  h4(" 2-Solutions par problème"),
  
selectInput(
  inputId = "prob",
  label = "Veuillez choisir un problème",
  choices = l,
  selected = "Malade de covid"),
tableOutput(outputId = "tbl3"),
plotOutput(outputId = "plt4"))),
tabPanel("PUBLICATION DE L'INDICATEUR",
         mainPanel(
textOutput(outputId = "Text4"),

selectInput(
  inputId = "prob1",
  label = "Veuillez choisir un problème",
  choices = l,
  selected = "Malade de covid"),

textOutput(outputId = "Text5")
))))


server <- function (input,output,session){
  output$plt4 <- renderPlot({
    base_filter_1=filter(base_filter,impact_covid==input$prob)
    tab=table(base_filter_1$impact_covid,base_filter_1$solution_ou_pas)
    tab
    barplot(tab)
    })
  
  output$Text5 <- renderText ({
    base_filter_1=filter(base_filter,impact_covid==input$prob1)
    base_filter_2=filter(base_filter_1,solution_ou_pas=="Solution trouvée")
    t=mean(base_filter_2$nbres_mois_si_Oui)
    paste0("La solution pour le problème ",input$prob1," a été trouvée en environ : ",t," mois" )
  }
  
  )
  
  output$Text4 <- renderText("Ici vous trouverez selon le problème que vous désirez, le temps moyen (en mois) pour trouver la solution")
  
  output$tbl3 <- renderTable ({
    base_filter_1=filter(base_filter,impact_covid==input$prob)
    tab=table(base_filter_1$impact_covid,base_filter_1$solution_ou_pas)
    tab
  }
    
  )
  
  
  output$tbl2 <- renderTable (
    table(base_filter$impact_covid),align="c"
  )
  
  output$Text3 <- renderText("Cette section est dédiée à vous présenter les informations clés obtenues à l'issue de notre rapport
                             ")
  
  output$Text2 <- renderText("Vous trouverez les informations clés de notre rapport dont le thème est l'impact de
                             la covid sur les ménages. Ci dessous la base apurée dont vous pouvez choisir le nombre de lignes à afficher")
  
  output$plt1 <- renderPlot({
    barplot(table(base_filter$impact_covid),col=input$Colors,las=3)
  })
  output$tbl1 <- renderTable({
    
    
    if (is.na(input$rows_n)){
      
      return(NULL)
    }
    
    if (input$rows_n < 1){
      
      
      sendSweetAlert(
        session = session,
        title="Format Incorrect",
        text="Sélectionnez au moins une ligne",
        type = "warning",
        btn_labels = "Ok"
      )
      
      
    }
    
    output$Text <- renderText(
      if (is.na(input$rows_n)){
        "Please enter a number"
      }
      else{
        paste0("First ",input$rows_n," Rows")
      }
      
    )
    head(base_1_ut,input$rows_n)
    
    
  })
}
shinyApp(ui=ui,server=server)


