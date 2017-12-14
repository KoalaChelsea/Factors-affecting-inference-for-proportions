library(shiny)
library(ggplot2)
library(mosaic)
library(dtplyr)
library(data.table)
library(scales)
library(Hmisc)


# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  #Null hypothesis
  output$nullhypo = renderUI({
    
    h3("Ho: p =", input$prop)
  })
  
  output$design = renderUI({
    if(input$designcheckbox)
    {
    h4("A researcher plans to take a random sample of size n students to do a survey about their experiences in studying at the University Park campus of Penn State University. However, she worries that sample results could be biased because the students who agree to participate might be different from those who don't (this would be an example of non-response bias). The researcher makes a confidence interval for the percentage of Penn State Students who are Pennsylvania residents based on her study and compares it to the mean of 59.5% for the population of all Penn State University Park students. This app shows  how confidence intervals of that type would come out when there is no bias.")
    }
  })
  
  #population plot with true prop
  output$popMean  = renderPlot({
    my_vector=c(input$prop, 1-input$prop)
    names(my_vector)=c("Pennsylvania Students at University Park","Out-of-state Students at University Park")
    ggplot() + geom_bar(aes(x=names(my_vector), y=my_vector), stat='identity',width=0.3, fill = "steelblue")+
      lims(y = c(0,1))+
      geom_hline(yintercept = 0.595, color = "forestgreen", size = 1.2)+
      labs(
        title = paste0("Proportion for Pennsylvania Students at University Park is ", input$prop, ". (true proportion for 2016 in green color)"),
        x = "whether the student is Pennsylvania Resident",
        y = "Enrollment by Residency")
    #barplot(my_vector,col=rgb(0.2,0.4,0.6,0.6),ylim=c(0,1), ylab="precentage")
    
  })
  
  
  
  #Calculating alpha by the confidence level input
  alpha <- reactive({
    (1 - input$level) / 2
  })
  
  #Updating Sample Size
  N <- reactive({
    as.integer(input$nsamp)
  })
  
  
  #generate 50 new sample
  Data <- reactive({
    input$new
    data.frame(
      x =
        do.call(
          paste0("rbinom"),
          c(list(n = as.integer(input$nsamp) * 50), list(1,input$prop)))
    ) %>%
      mutate(idx = rep(1:50, each = input$nsamp))
  })
  
  #calculate the interval
  Intervals <- reactive({
    Data() %>%
      group_by(idx) %>%
      summarise(
        Count = sum(x),
        sampleProp = binconf(Count, N(), alpha=alpha())[1],
        lowerbound = binconf(Count, N(), alpha=alpha())[2],
        upperbound = binconf(Count, N(), alpha=alpha())[3],
        cover = (lowerbound < input$prop) & (input$prop < upperbound)) %>%
      ungroup()
  })
  
  
  #default as all the samples are selected
  selected_sample <- 50
  selectedSample <- reactive({
    if (! is.null(input$plot_click)) {
      selected_sample <<- round(input$plot_click$y)
      if (selected_sample < 1) selected_sample <<- 1
      if (selected_sample > 50) selected_sample <<- 50
    }
    selected_sample
  })
  
  OneSample <- reactive({
    Data() %>%
      filter( idx == selectedSample() )
  })
  
  OneSampleColor <- reactive({
    colors <- c("TRUE" = "navy", "FALSE" = "red")
    covers <- (Intervals() %>% filter(idx == selectedSample()) )$cover
    colors[ as.character(covers) ]
  })
  
  #print the CIplot
  output$CIplot <- renderPlot({
    validate(
      need(is.numeric(input$nsamp),
           message = "Please input samle size")
    )
    
    # validate(
    #   need(input$nsamp >=30,
    #        message = "Please input samle size larger than 30")
    # )
    
    ggplot(data = Intervals()) +
      geom_pointrange(
        aes(x=idx, ymin = lowerbound, ymax = upperbound, y = sampleProp, colour = cover,
            alpha = idx == selectedSample(),
            size = idx == selectedSample()
        )) +
      geom_hline(yintercept = input$prop, size = 2, colour = "black", alpha = 0.5) +
      coord_flip() +
      scale_size_manual(values = c("TRUE" = 1.5, "FALSE" = .7), guide = FALSE) +
      scale_color_manual(values = c("TRUE" = "navy", "FALSE" = "red"), guide = FALSE) +
      scale_alpha_manual(values = c("TRUE" = 1, "FALSE" = .5), guide = FALSE) +
      lims(y = c(-0.01,1.01)) +
      labs(title = paste0(100 * input$level, "% Confidence Intervals for the proportion"),
           x = "50 samples are generated every time",y="vertical line shows null proportion") +
      theme(legend.position = "none",
            axis.text.y = element_blank(),
            axis.ticks.y = element_blank())
  })
  
  output$sampProp<- renderPlot({
    validate(
      need(is.numeric(input$nsamp),
           message = "Please input samle size")
    )
    
    ggplot( data = OneSample()) +
      geom_histogram( aes(x = x), bins = 15,
                      fill = OneSampleColor(), alpha=0.5) +
      geom_hline(yintercept = input$prop*N(), color = "forestgreen", size = 1) +
      labs(title = paste("Sample proportion for residency in UP = ",
                         round(mean(OneSample()$x), 2)),
           x="")
    
  
  })
  
  rate <- reactiveValues(cover = 0, total = 0)
  observeEvent(input$more, {
    rate$cover <- sum(Intervals()$cover)
    rate$total <- nrow(Intervals())
  })
  
  
  observeEvent(c(input$A, input$B, input$n, input$level),
                {rate$cover <- sum(Intervals()$cover); rate$total <- nrow(Intervals())}
  )
  
  # text messages
  output$CoverageRate <- renderText({
    validate(
      need(is.numeric(input$nsamp),
           message = "Please input samle size")
    )
    
    paste(sum(Intervals()$cover), "of these",
          nrow(Intervals()), "intervals cover the parameter value. And coverage rate is ",
          round(100 *  sum(Intervals()$cover)/ nrow(Intervals()), 2),
          "% (",  rate$total, " samples)")
  })
  ############################################################
  ############################################################
  
  #Calculating alpha
  zalpha <- reactive({
    (1 - input$zlevel) / 2
  })
  
  zlowerbound <- reactive({
    qnorm(zalpha())
  })
  
  zupperbound <- reactive({
    -qnorm(zalpha())
  })
  
  
  output$zplot = renderPlot({
    
    # draw the normal curve
    curve(dnorm(x, mean = 0, sd = 1), xlim=c(-3,3),xaxt = "n", main="Normal Distribution Plot (Mean = 0, StDev = 1)")
    cord.x <- c(zlowerbound(),seq(zlowerbound(),zupperbound(),0.01),zupperbound())
    cord.y <- c(0,dnorm(seq(zlowerbound(),zupperbound(),0.01)),0)
    
    polygon(cord.x, cord.y, col='skyblue')
    axis(side=1,at=round(c(zlowerbound(),zupperbound()),3))
    
  })
  
  output$feedback <- renderText({
    input$submit
    isolate({
      validate(
        need(input$question1 == 1.645 || input$question1 == 1.65 || input$question1 == 1.6 || input$question1 == 2, 'Question 1 is not correctly answered.'),
        need(input$question2 == '1.960' || input$question2 == 1.96 || input$question2 == 2.0, 'Question 2 is not correctly answered.'),
        need(input$question3 == '2.576' || input$question3 == 2.58 || input$question3 == 2.6 || input$question3 == 3, 'Question 3 is not correctly answered.'),
        need(input$question4 == 'y', 'Question 4 is not correctly answered.')
      )
      paste("All correct. Great Job!")
    })
  })
  
  ####################################################################
  #####################################################################
  
  # output$matrixScore <- renderTable({
  #   
  #  
  #   testtakers <- c(762247, 875342)
  #   meanScore <- c(524,494)
  #   sdScore <- c(126,116)
  #   groupScore <- c(testtakers, meanScore, sdScore)
  #   
  #   matrixfinal <- matrix(groupScore, nrow = 3, ncol = 2, byrow = TRUE, dimnames = list(rownames = c("n", "μ", "σ"), c("Male","Female")))
  #   matrixfinal
  # })
  
  output$testdesign = renderUI({
    if(input$testdesigncheckbox)
    {
      h4("A researcher wants to sample a group of n University Park students and n students from other Penn State campuses to ask them about their experiences in college.  Although the percentage of Pennsylvania residents is 24.9% lower at University Park, a critic believes her sampling technique would provide a sample of students with a proportion (p) that does not depend on the campus (the null hypothesis). The researcher uses her samples to conduct a test of that null hypothesis and this app shows how that test would behave when the sampling is really unbiased and the University Park campus has a proportion that is 0.249 lower lower. ")
    }
  })
  
  
  #Calculating alpha by the confidence level input
  dalpha <- reactive({
    (1 - input$dlevel) / 2
  })
  
  #Updating Sample Size
  dN <- reactive({
    as.integer(input$nSamp)
  })
  
  
  standardError <- reactive({
    sqrt(0.595*0.405/dN() + 0.844*0.156/dN())
  })
    
  #population mean plot with true diffmean
  output$dpopMean  = renderPlot({
    # dat <- read.table(text = "University_Park Other_Campuses
    #                           Pennsylvania_Students 0.595 0.844
    #                           Out-of-State_Students 0.405	 0.156",
    #                   sep = "",header = TRUE)
  
    dfPop <- data.frame(types = rep(c("Pennsylvania_Students", "Out-of-State_Students"), each=2),
                           location=rep(c("University Park", "Other Campuses"),2),
                           samplepercent=c(0.595,0.844,0.405,0.156))
                           
    ggplot(dfPop,aes(x = location,y = samplepercent, fill = types)) +
      geom_bar(position = position_fill(),stat="identity", width=0.3) +
      scale_y_continuous(labels = percent_format()) +
      scale_fill_brewer(palette="Paired")+
      labs(
        title = paste0("population proportion(diff) = -24.9%, σ(p(UP)-p(Others)) = ",round(sqrt(0.595*0.405 + 0.844*0.156),3)),
        y = "Enrollment by Percentage")

  })
  
  UPS <- reactive({
    input$newSample
    rbinom(n=dN(), 1, 0.595)
  })
  
  UWS <- reactive({
    input$newSample
    rbinom(n=dN(), 1, 0.844)
  })
  
  Diff <- reactive({
    mean(UPS()) - mean(UWS())
  })
  
  output$sampleDiff  = renderPlot({
    validate(
      need(is.numeric(input$nSamp),
           message = "Please input samle size")
    )
    input$newSample
    
    dfSample <- data.frame(types = rep(c("Pennsylvania_Students", "Out-of-State_Students"), each=2),
                          location=rep(c("University Park", "Other Campuses"),2),
                          samplepercent=c(mean(UPS()),mean(UWS()),1-mean(UPS()),1-mean(UWS())),
                          ref=c(0.595,0.844), 2)
    

    ggplot(dfSample,aes(x = location,y = samplepercent, fill = types)) +
      geom_bar(position = position_fill(),stat="identity", width=0.3) +
      scale_y_continuous(labels = percent_format()) +
      scale_fill_brewer(palette="Set2")+
      geom_errorbar(aes(ymin = ref, ymax = ref, col = "True proportion"), width = 0.3, colour = "#191919", size = 1) + 
      labs(
        title = paste0("phat(diff) = ", percent(Diff()),", σ(phat(UP)-phat(Others) = ",round(standardError(),3),", UP sample = ",dN(),", others sample = ",dN()),
        x ="")
  })

  
  dlowerbound <- reactive({
    Diff() + qnorm(dalpha()) * standardError()
  })
  dupperbound <- reactive({
    Diff() - qnorm(dalpha()) * standardError()
  })
  
  output$CItable = renderTable({
    validate(
      need(is.numeric(input$nSamp),
           message = "Please input samle size")
    )
    if(input$CIcheckbox)
    {
      ctable = matrix(c(percent(dlowerbound()), percent(dupperbound())),nrow=1)
      colnames(ctable) = c("Lower bound","Upper bound")
      ctable
    }
  })
  
  pvalue <- reactive({
    2*(1-pnorm(abs(zstatistic())))
  })
  
  zstatistic <- reactive({
    Diff()/standardError()
    
  })
  
  output$sampleinfotable = renderTable({
    validate(
      need(is.numeric(input$nSamp),
           message = "Please input samle size")
    )
    ctable = matrix(c(percent(mean(UPS())), percent(mean(UWS()))), nrow=1)
    colnames(ctable) = c("University Park","Other Campuses")
    ctable
  })
  
  output$Diffinfo = renderUI({
    validate(
      need(is.numeric(input$nSamp),
           message = "")
    )
    paste("The difference between UP and other campuses sample (UP-other) is ", percent(Diff()))
  })
  
  output$testtable = renderTable({
    validate(
      need(is.numeric(input$nSamp),
           message = "Please input samle size")
    )
    if(input$testcheckbox)
    {
      ctable = matrix(c(zstatistic(),pvalue()),nrow=1)
      colnames(ctable) = c("z-statistic","p-value")
      #rownames(ctable) = paste((input$dlevel),"% CI",sep="")
      ctable
    }
  })
  
  zstandard <- reactive({
    -qnorm(dalpha())
  })
  
  output$decisionZ = renderText({
    validate(
      need(is.numeric(input$nSamp),
           message = "Please input samle size")
    )
    if(input$decisioncheckbox)
    {
      if(abs(zstatistic()) <= zstandard()){
        paste("Since it is observed that |z| = ",abs(round(zstatistic(),3))," is less than z*score = ",round(zstandard(),3),", the null hypothesis provides a reasonable explanation of the data so we can NOT conclude that University Park campus has a different proportion of Pennsylvania residents  when student's are chosen by the researcher's sampling procedure.")
        
      }else{
        paste("Since it is observed that |z| = ",abs(round(zstatistic(),3))," is larger than z*score = ",round(zstandard(),3),", the null hypothesis is not a reasonable explanation of the data so we have evidence that there is a difference between the percentage of Pennsylvania residents at the University Park campus and the percentage at other campuses when students are chosen by the researcher's sampling procedure.")
      }
    }
    
  })
  
  output$decisionP = renderText({
    validate(
      need(is.numeric(input$nSamp),
           message = "Please input samle size")
    )
    if(input$decisioncheckbox)
    {
      if(pvalue() >= (2*dalpha())){
        paste("Since it is observed that p-value = ",round(pvalue(),3)," is larger than ",round(2*dalpha(),3),", the null hypothesis provides a reasonable explanation of the data so we can NOT conclude that University Park campus has a different proportion of Pennsylvania residents  when student's are chosen by the researcher's sampling procedure.")
      }else{
        paste("Since it is observed that p-value = ",round(pvalue(),3)," is less than ",round(2*dalpha(),3),", the null hypothesis is not a reasonable explanation of the data so we have evidence that there is a difference between the percentage of Pennsylvania residents at the University Park campus and the percentage at other campuses when students are chosen by the researcher's sampling procedure.")
      }
    }
  })
  
})



