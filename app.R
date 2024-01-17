library(shiny)
library(rhandsontable)
#library(shinyjs)
library(reticulate)
os<-import('os')
source('modarith.r')
source('ECmodp.r')
source('ECplotting.r')
source_python('allzerosB.py', envir=globalenv(), convert=TRUE)

ui<-fluidPage(
   withMathJax(),
   tags$style(type = "text/css",
             "h1 {color: blue; font-weight: bold; font-size: 24px;}",
             "h4 {color: blue; font-weight: bold; font-size: 18px;}",
             "h2 {color: blue; font-weight: bold; font-size: 14px;}",
             "body {background-color: skyblue;}",
             "label {font-size: 16px; color: blue;
                   font-weight: bold;}"
              ),                   
   tags$head(tags$style(HTML(
            "#text1 {font-size: 17px; color: red; font-weight: bold;}",
            "#text2 {font-size: 17px;color: red; font-weight: bold;}",
            "#text3 {font-size: 17px;color: red; font-weight: bold;}",
            "#text4 {font-size: 17px;color: red; font-weight: bold;}",
            "#text5 {font-size: 17px;color: red; font-weight: bold;}",
            "#text6 {font-size: 17px;color: red; font-weight: bold;}",
            "#text7 {font-size: 17px;color: red; font-weight: bold;}",
            "#text7 {font-size: 17px;color: red; font-weight: bold;}",
            "#text8 {font-size: 17px;color: red; font-weight: bold;}",
            "#text9 {font-size: 17px;color: red; font-weight: bold;}",
            "#textEC {font-size: 17px; color: red; font-weight: bold;}",
            "#textECSUM {font-size: 17px; color: red; font-weight: bold;}"))),
            
   tags$h1('Modular Arithmatic/Number Theory Utilities'),
   navlistPanel(
   tabPanel(tags$h4("Residues, Euclidean Algorithm/GCD"),
   fluidRow(
     column(5,
       tags$h4("Reduces a mod n"),
       numericInput('a1','a', value='', width='100%'),
       numericInput('n1','n', value='', width='100%'),
       actionButton(inputId='go1', label='Find Value',
          style="color: blue; font-size: 14px; font-weight: bold; 
                   background-color: white;"),
       hr(),
       verbatimTextOutput('text1')
            ),
     column(7,
       tags$h4("Extended Euclidean Algorithm, GCD"),
       tags$h2("For g=gcd(a,b) uses the Extended Euclidean Algorithm to write g=ka+mb for integers k and m with k>0."),
       numericInput('a2','a', value='', width='100%'),
       numericInput('b','b', value='', width='100%'),
       actionButton(inputId='go2', label='Find g, k, and m',
          style="color: blue; font-size: 14px; font-weight: bold; 
                   background-color: white;"),
       hr(),
       verbatimTextOutput('text2')
           ))), 
    tabPanel(tags$h4("RSA Decryption, Modular Exponentiation"),
    fluidRow(
     column(5,
       tags$h4("RSA Decryption Key"),
       tags$h2("For composite N=pq (p and q primes) and encryption exponent
          e with gcd(e, \\(\\phi\\)(N))=1, finds the smallest positive 
          decryption exponent d."),
       numericInput('p','p', value='', width='30%'),
       numericInput('q','q', value='', width='30%'),
       numericInput('e','e', value='', width='30%'),
       actionButton(inputId='go3', label='Find d',
          style="color: blue; font-size: 14px; font-weight: bold; 
                   background-color: white;"),
       hr(),
       verbatimTextOutput('text3')
           ),
     column(7,
       tags$h4("Modular Exponentiation"),
       tags$h2("For \\(b,n\\in \\Bbb Z\\) and \\(m\\in\\Bbb N\\), 
                  finds \\(b^m\\) (mod n)"),
       numericInput('B','b', value='', width='100%'),
       numericInput('M','m', value='', width='100%'),
       numericInput('n4','n', value='', width='100%'),
       actionButton(inputId='go4', label='Find \\(b^m\\) (mod n)',
          style="color: blue; font-size: 14px; font-weight: bold; 
                   background-color: white;"),
       hr(),
       verbatimTextOutput('text4')
           )
           )),
           
      tabPanel(tags$h4('Relatively Prime Integers'),
      fluidRow(
      column(1,''),
      column(11,
      tags$h4("Relatively Prime Integers"),
      tags$h2("Given \\(n\\in\\Bbb Z\\) finds values in list 
               relatively  prime to n."),
      hr(),
      tags$h2("For a given \\(n\\in\\Bbb N\\), you may find all 
         \\(0\\le a<n\\) with \\(gcd(a,n)=1\\) or only those values 
          from a list that you enter."),
     selectInput("As", "All/List",
             choice=list(All = "a", List= "l")),
             #open only if list is to be entered
     conditionalPanel(
     condition = "input.As == 'l'",    
          textInput('x', 'Integer List (Comma Separated)', 
                                      value='', width='80%')),                               
     numericInput('n5', 'n', value=2, width='30%'),
     actionButton(inputId='go5', label='Find Co-Prime Integers',
          style="color: blue; font-size: 14px; font-weight: bold; 
                   background-color: white;"),
       hr(),
       tags$style(type="text/css", "#text5 {white-space: pre-wrap;}"),
       verbatimTextOutput('text5') 
               )
              )),
           
                 
       tabPanel(tags$h4('Square Roots Mod n'),
        fluidRow(
        column(1,''),
        column(11,
        tags$h4("Square Roots Modulo n"),
         tags$h2("For natural number n and integer a, finds a's 
                  modulo n square roots if it has any."),
          numericInput('a7', 'a', value='', width='30%'),
          numericInput('n7', 'n', value='', width='30%'),
              actionButton(inputId='go7', label='Find Square Roots',
          style="color: blue; font-size: 14px; font-weight: bold; 
                   background-color: white;"),
       hr(),
       verbatimTextOutput('text7') 
               )
              )),
       tabPanel(tags$h4('Cyclic Subgroups of \\((\\Bbb Z/n\\Bbb Z)^*\\)'),
         fluidRow(
          column(1,''),
          column(11,
          tags$h4("Cyclic Subgroup by Generated by 
                    Element \\(a\\in(\\Bbb Z/n\\Bbb Z)^*\\)"),
          tags$h2("For integers a, n with gcd(a, n)=1, 
               finds the cyclic subgroup \\(H=<a>\\) 
                  for \\(a\\in (\\Bbb Z/n\\Bbb Z)^*\\)."),
          numericInput('a6', 'a', value='', width='20%'),
          numericInput('n6', 'n', value='', width='20%'),
              actionButton(inputId='go6', label='Find Cyclic Subgroup',
          style="color: blue; font-size: 14px; font-weight: bold; 
                   background-color: white;"),
       hr(),
       tags$style(type="text/css", "#text6 {white-space: pre-wrap;}"),
       verbatimTextOutput('text6') 
                ))),
                
       tabPanel(tags$h4('Chinese Remainer Theorem'),
          fluidRow(
           column(1,''),
           column(11,
             tags$h4("Chinese Remainder Theorem"),
             tags$h2("Solve simultaneous congruences using 
                     the Chinese Remainder Theorem."),
            numericInput('No', 'Number of Congruences (<=12)',
                       value='2', width='60%', min=0, max=12),
            hr(),
            actionButton(inputId='go8', label='Save n',
               style="color: blue; font-size: 14px; font-weight: bold; 
                   background-color: white;"),
            tags$h2("For \\(x\\equiv\ c_i\ (mod\ n_i)\\), first row
                      enter \\(c_i\\)'s, second enter 
                      corresponding \\(n_i\\)'s."),
            hr(),
            rHandsontableOutput('table'),
            hr(),
            actionButton(inputId='go9', label='Solve Congruences',
               style="color: blue; font-size: 14px; font-weight: bold; 
                   background-color: white;"),
            hr(),
            verbatimTextOutput('text8') 
                  )
                    )
                  ),
                  
        tabPanel(tags$h4("Factoring Algorithms"),
        fluidRow(
        column(1,''),
        column(11,
        tags$h4("Pollard's Algorithm"),
        tags$h2("For natural number n, the algorithm attempts to factor n.
          It can fail to factor composities which have no relatively small
          prime factors and, as it involves two random selections, will not
          always produce the same output for a given imput."),
        numericInput('n8', 'n', value='', width='30%'),
        actionButton(inputId='go10', label='Go',
          style="color: blue; font-size: 14px; font-weight: bold; 
                   background-color: white;"),
        hr(),
        verbatimTextOutput('text9')
               )
              )),
              
      tabPanel(tags$h4("Elliptic Curves Mod p"),
      hr(),
      column(1,''),
      column(11,
      tags$h4("Specify an elliptic curve 
               \\( E:\\ y^2=x^3+Ax+B\\ (mod\\ p) \\)."),
      fluidRow(
      column(3,                              
      numericInput('a', 'A', value=1, width='50%')),
      column(3,                             
      numericInput('ba', 'B', value=1, width='50%')),
      column(3,                               
      numericInput('pa', 'p', value=3, width='50%')),
      actionButton(inputId='go12', label='Save Values',
          style="color: blue; font-size: 14px; font-weight: bold; 
                   background-color: white;")
                   ),
       fluidRow(
       radioButtons('GOA', 'Generate All Points or Add Two Points?',
                          choices=list(
                             'Generate All'="l", 'Add Two'="a")),
      conditionalPanel(
       condition = "input.GOA=='l'",
       actionButton(inputId='go13', label='Generate Points',
          style="color: blue; font-size: 14px; font-weight: bold; 
                   background-color: white;"),
       hr(),
       tags$style(type="text/css", "#textEC {white-space: pre-wrap;}"),
       verbatimTextOutput('textEC') 
                       ),
      conditionalPanel(
       condition = "input.GOA=='a'", 
       tags$h4("For points \\( P, Q\\in E \\), find \\(P\\oplus Q\\). Enter
            the point at infinity as (0,0)."),
       hr(),
       column(3,
       tags$h2("Input \\(P\\)"),
       numericInput('Px', 'x-Coord', value=5, width='80%'),
       numericInput('Py', 'y-Coord', value=5, width='80%')),
       column(3,
       tags$h2("Input \\(Q\\)"),
       numericInput('Qx', 'x-Coord', value=5, width='80%'),
       numericInput('Qy', 'y-Coord', value=5, width='80%')),
       column(6,
       actionButton(inputId='go14', label='Find \\(P\\oplus Q\\)',
          style="color: blue; font-size: 14px; font-weight: bold; 
                   background-color: white;"),
       hr(),
       tags$style(type="text/css", "#textECSUM {white-space: pre-wrap;}"),
       verbatimTextOutput('textECSUM'))
                        ))
       
                ) 
               ),
      tabPanel(tags$h4('Elliptic Curve Plotting/Arithmetic over \\(\\Bbb R\\)'),
        column(4,
          tags$h4("Specify an elliptic curve:"),
          tags$h4("\\( E:\\ y^2=x^3+Ax+B \\)."), 
          fluidRow(
             column (5,                             
               numericInput('ECA', 'A:', value=-6, width='100%')), 
             column(5,                           
             numericInput('ECB', 'B:', value=9, width='100%'))
                  ),
          fluidRow(
             column(5,                                
               actionButton(inputId='goECP1', label='Save Values',
                style="color: blue; font-size: 14px; font-weight: bold; 
                   background-color: white;")), 
             column(1,''),
             column(5,                              
                actionButton(inputId='goECP2', label='Draw Curve',
                  style="color: blue; font-size: 14px; font-weight: bold; 
                   background-color: white;"))
                    ),
          hr(),
          tags$h4("Click once to add P to the plot, a second time to add Q."),
          hr(),
          fluidRow( 
             column(5,
               actionButton(inputId='goADD', label='Find \\(P\\oplus Q\\)',
                  style="color: blue; font-size: 14px; font-weight: bold; 
                   background-color: white;"))
                  ),
           hr(),
           fluidRow(
              column(5,
                actionButton(inputId='RESETPTS', label='Reset Points',
                  style="color: blue; font-size: 14px; font-weight: bold; 
                   background-color: white;")),
              column(1, ''),
              column(5,
                actionButton(inputId='RESET', label='Reset All',
                  style="color: blue; font-size: 14px; font-weight: bold; 
                   background-color: white;"))
                )),
                   
         column(8,
            hr(),
            plotOutput('plot', click='plot_click',
                         height='550px', width='100%')),
            hr()   
                 ))  
              )

        
server<-function(input, output){

       v<-reactiveValues(df=CRTtable(2), NO=2,X=c(), 
                   N5=2, AS='l', GO1=FALSE, GO2=FALSE,A=1, BA=1, 
                   PA=3,GO3=FALSE, PX=0, PY=0, QX=0, QY=0,GO10=FALSE,
                   GO4=FALSE,AA=-6, B=9, q=c(1, 0,-6, 9),  
                   clickX=NULL,clickY=NULL, GODRAW=FALSE, 
                   GOADD=FALSE,P=NULL, Q=NULL)   
       observeEvent(input$go5,
                 {v$X<-as.numeric(unlist(strsplit(input$x,",")))
                  v$N5<-input$n5
                  v$AS<-input$As
                  v$GO1<-TRUE})
                   
       
       observeEvent(input$go8, {v$NO<-input$No
                                v$df<-CRTtable(v$NO)
                                })
       observeEvent(input$go9, {v$GO2<-TRUE
                                }) 
       output$table<-renderRHandsontable({rhandsontable(v$df)})
       observeEvent(input$go9,{v$df<-hot_to_r(input$table)})
       
       observeEvent(input$go12,
                 {v$A<-input$a
                  v$BA<-input$ba
                  v$PA<-input$pa})
       observeEvent(input$go13,{v$GO3<-TRUE}) 
       observeEvent(input$go14,
                 {v$PX<-input$Px
                  v$PY<-input$Py
                  v$QX<-input$Qx
                  v$QY<-input$Qy
                  v$GO4<-TRUE})
                  
       observeEvent(input$go10, {v$GO10<-TRUE})
       
       A1<-eventReactive(input$go1,{input$a1})
       N1<-eventReactive(input$go1,{input$n1})
       A2<-eventReactive(input$go2,{input$a2})
       B2<-eventReactive(input$go2,{input$b})
       P<-eventReactive(input$go3,{input$p})
       Q<-eventReactive(input$go3,{input$q})
       E<-eventReactive(input$go3,{input$e})
       B4<-eventReactive(input$go4,{input$B})
       M<-eventReactive(input$go4,{input$M})
       N4<-eventReactive(input$go4,{input$n4})
       A6<-eventReactive(input$go6,{input$a6})
       N6<-eventReactive(input$go6,{input$n6})
       A7<-eventReactive(input$go7,{input$a7})
       N7<-eventReactive(input$go7,{input$n7})
       N8<-eventReactive(input$go10,{input$n8})
       
       output$text1<-renderPrint({MODPRINT(A1(),N1())})
       output$text2<-renderPrint({EEAPRINT(A2(),B2())})
       output$text3<-renderPrint({DEKEYPRINT(P(),Q(),E())})
       output$text4<-renderPrint({PWRMODPRINT(B4(), M(), N4())})
       output$text5<-renderPrint({
                     if(v$GO1==FALSE)
                        {'Select Values'}
                      else
                        {RPList(v$AS,v$X, v$N5)}
                                 })
       output$text6<-renderPrint({GRPPRINT(A6(), N6())})
       output$text7<-renderPrint({SQRPRINT(A7(), N7())})
       output$text8<-renderPrint({
                     if(v$GO2==FALSE)
                        {'Select Values'}
                      else
                        {CRTPRINT(v$df)}
                                 })
       output$text9<-renderPrint({
                  if(v$GO10==FALSE)
                        {print('Enter n')}
                  else  
                        {PLLRD(N8())}
                                 })
       output$textEC<-renderPrint({ECPrint(v$A, v$BA, v$PA, v$GO3)})
       output$textECSUM<-renderPrint({
       ECAddPrint(c(v$PX, v$PY),c(v$QX, v$QY), v$A, v$BA, v$PA, v$GO4)
                                     })
                                     
       observeEvent(input$goECP1,{v$AA<-input$ECA
                                  v$B<-input$ECB}) 
       observeEvent(input$goECP2,{v$q<-c(1,0, v$AA, v$B)
                                  v$GODRAW<-TRUE})
                                  
      observeEvent(input$plot_click,{
           if (is.null(v$P) & is.null(v$Q)){
             v$P<-input$plot_click}
           else if  (!is.null(v$P) & is.null(v$Q)){
             v$Q<-input$plot_click}
                                    })
             
     
      observeEvent(input$goADD,{v$GOADD<-TRUE})
      
      observeEvent(input$RESETPTS,{v$GOADD<-FALSE
                                v$P<-NULL
                                v$Q<-NULL})
      
      observeEvent(input$RESET,{v$GODRAW<-FALSE
                                v$GOADD<-FALSE
                                v$P<-NULL
                                v$Q<-NULL
                                v$A<--6
                                v$B<-9
                                v$q<-c(1,0,-6,9)})
                                
      
      
      output$plot<-renderPlot({
            if (v$GODRAW==FALSE)
                 {GetPlot()}
            else if (v$GODRAW==TRUE & v$GOADD==FALSE & is.null(v$P) &
                   is.null(v$Q))
                {ECPlot(v$q)}
            else if (v$GODRAW==TRUE & v$GOADD==FALSE & !is.null(v$P) & 
                   is.null(v$Q))
                {ECPlot(v$q)
                points(as.numeric(v$P[1]),as.numeric(v$P[2]), pch=19,
                    col='blue')
                x1<-as.numeric(v$P[1])
                y1<-as.numeric(v$P[2])
                if (y1>0){Y1<-y1+2.5} else{Y1<-y1-2.5}
                text(x1, Y1, 'P', cex=1.25, font=2, col='blue')}
            else if (v$GODRAW==TRUE & v$GOADD==FALSE & !is.null(v$P) &
                 !is.null(v$Q)){
                  ECPlot(v$q)
                  eps<-max((par('usr')[4]-par('usr')[3])/50, 
                          (par('usr')[2]-par('usr')[1])/50)
                  points(c(as.numeric(v$P[1]),as.numeric(v$Q[1])),
                      c(as.numeric(v$P[2]),as.numeric(v$Q[2])),
                      pch=19, col='blue')
                  x1<-as.numeric(v$P[1])
                  y1<-as.numeric(v$P[2])
                  x2<-as.numeric(v$Q[1])
                  y2<-as.numeric(v$Q[2])
                  if (y1>0){Y1<-y1+2.5} else{Y1<-y1-2.5}
                  if (y2>0){Y2<-y2+2.5} else{Y2<-y2-2.5}
                  if (abs(x1-x2)<eps & abs(y2-y1)<eps){
                    text(x1, Y1, 'P=Q', cex=1.25, font=2, col='blue')}
                  else{
                  text(x1, Y1, 'P', cex=1.25, font=2, col='blue')
                  text(x2, Y2, 'Q', cex=1.25, font=2, col='blue')}
                      }
            else if (v$GODRAW==TRUE & v$GOADD==TRUE & !is.null(v$P) &
                   !is.null(v$Q)){
                ECPlot(v$q)
                points(c(as.numeric(v$P[1]),as.numeric(v$Q[1])),
                      c(as.numeric(v$P[2]),as.numeric(v$Q[2])),
                      pch=19, col='blue')
                AddLine(v$P, v$Q, v$q)
                                 }
                          })
        }
                                
shinyApp(ui=ui, server=server)



