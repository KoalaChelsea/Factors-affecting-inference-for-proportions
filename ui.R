library(shiny)
library(shinydashboard)
library(ggplot2)
library(shinyBS)


dashboardPage(skin="blue",
              
              #Title
              dashboardHeader(title="Confidence Interval with PSU students",titleWidth=600),
              
              #Sidebar
              dashboardSidebar(
                width = 280,
                sidebarMenu(
                  menuItem("Overview", tabName = "over", icon = icon("university")),
                  menuItem("UP Residency Percentage", tabName = "UPRes", icon = icon("pencil-square")),
                  menuItem("Finding the z∗ Multiplier", tabName = "findz", icon = icon("pencil-square")),
                  menuItem("Tests for Differences of Two Proportions", tabName = "popdiff", icon = icon("pencil-square"))
                  #menuItem("Z-score Table", tabName = "table", icon = icon("flag"))
                  
                )),
              
              #Content within the tabs
              dashboardBody(tags$head(
                tags$link(rel = "stylesheet", type = "text/css", href = "Feature.css")
              ),
                
                
                tabItems(
                  
                  tabItem(tabName = "over",
                          fluidRow(
                            #column of length 12 which is the whole width
                            #I include everthing in a column though because this way there are margins and it looks better
                            column(8,
                                   h3(strong("About the App")),
                                   h4("This app represents an interactive supplementary module embedded in statistics lessons with two features. The first feature visualizes how the variations in confidence levels and sample size affect the outcome confidence interval in a single mean. The second feature tests the difference between two means by adjusting confidence levels and sample size and generating calculation results with explanations. The app requires students to engage in the interaction with the scenarios provided in context."),
                                   h4(""),
                                   h4("This lesson explores the behavior of confidence intervals for a single proportion and two sample tests for the difference in proportions as the level and sample size changes."),
                                   
                                   h3("Instructions:"),
                                   h4("a. Move the sample size and level sliders to see how they affect  confidence intervals for the proportion of Penn State students who are residents of Pennsylvania  or two-sample tests for differences in the proportion of Pennsylvania residents between the University Park campus and the other campuses."),
                                   h4("b. Click on the generate buttons to draw new samples and - for the confidence interval app - click on the center of an interval to show data for that sample."),
                                   h3("Acknowledgements:"),
                                   h4("This app was developed and coded by Yingjie(Chelsea) Wang. The homepage on github: https://github.com/KoalaChelsea."),
                                   h4("Information about confidence interval graph was drawn from Randall Pruim's shiny app. Here is the link to his homepage on github. https://github.com/rpruim"),
                                   h3(strong("About the data")),
                                   h4("The University Budget Office provided this common data set on facts about Penn State’s different campuses for Fall 2016."),
                                   # actionButton(inputId='weblink', label="Click Here to view original official page", 
                                   #              icon = icon("info"), 
                                   #              style="color: #fff; background-color: #337ab7; border-color: #2e6da4",
                                   #              onclick ="window.open('http://admissions.psu.edu/apply/statistics/', '_blank')"),
                                   helpText(a("Click Here to view original official page", href="https://budget.psu.edu/FactBook/StudentDynamic/PANonPASummary.aspx?YearCode=2016Enr&FBPlusIndc=N", target="blank")),
                                   h3("Data Summary:"),
                                   h4("This is the summary of Enrollment by Residency in Fall 2016."),
                                   img(src="2016Residency.png",height = "100%",width = "100%", algin = "middle"),
                                   br()
                                   )
                            
                            ) 
                          
                  ),
                  
                  
                  tabItem(tabName = "UPRes",
                          fluidPage(
                            titlePanel("Confidence Intervals for Enrollment by Residency in 2016 (p = 59.5%)"),
                            sidebarLayout(
                              sidebarPanel(
                                # h3(strong("Confidence Intervals for a population mean (n > 30): ")),
                                # h4("For large random samples a confidence interval for a population mean is given by"),
                                # h3("sample mean ± z* s / sqrt(n)"),
                                # h4("where z* is a multiplier number that comes form the normal curve and determines the level of confidence."),
                                h3(strong("Design: ")),
                                checkboxInput("designcheckbox","Show design info:", TRUE),
                                uiOutput("design"),
                                #h4("A researcher plans to take a random sample of size n students to do a survey about their experiences in studying at the University Park campus of Penn State University. However, she worries that sample results could be biased because the students who agree to participate might be different from those who don't (this would be an example of non-response bias). The researcher makes a confidence interval for the percentage of Penn State Students who are Pennsylvania residents based on her study and compares it to the mean of 59.5% for the population of all Penn State University Park students. This app shows  how confidence intervals of that type would come out when there is no bias."),
                                #h4("TIP: Click on an interval to show a histogram for the underlying sample."),
                                h3(strong("Hypothesis: ")),
                                uiOutput("nullhypo"),
                                
                                sliderInput("prop", "Proportion for Pennsylvania Students in UP",
                                            min=0.01, max = 0.99, value = 0.595, step = 0.01),
                                
                                sliderInput("level", "Confidence Level",
                                            min=.50, max = 0.99, value = 0.90, step = 0.01),
                                sliderInput("nsamp", "Sample Size (n > 30)",
                                            min= 30, max = 500, value = 30, step = 5),
                                
                                actionButton("new", "Generate 50 New Samples",icon("paper-plane"),style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                                bsPopover("new","Note","By clicking on this button, new 50 sample with the size you have input in each sample will be generated.",
                                          trigger="hover",placement="right"),br(),
                                br(),
                                wellPanel(
                                  plotOutput("sampProp",height = "250px"),
                                  bsPopover("sampProp","Sample Histogram Plot","This is the histogram plot of the sample you selected on Confidence Interval Plot. The green line is the true percentage that you input.",
                                            trigger="hover",placement="top"),br()
                                )
                                
                                
                              ),
                              
                              mainPanel(
                                plotOutput("popMean",height = "300px"),
                                bsPopover("popMean","Population Bar Graph","This is the bar plot based on percentage you input. The green line is the true percentage in 2016.",
                                          trigger="hover",placement="bottom"),br(),
                                plotOutput("CIplot",height = "600px", click = "plot_click"),
                                bsPopover("CIplot","Confidence Interval Plot","The blue lines indicate a confidence interval covers the population proportion and the red lines indicate that the population proportion is outside of the confidence interval. Click on an interval to show a histogram for the underlying sample.",
                                          trigger="hover",placement="top"),br(),
                                br(),
                                textOutput("CoverageRate"),
                                tags$head(tags$style("#CoverageRate{color: black;
                                 font-size: 40px;
                                                     font-style: italic;
                                                     }"
                         )
                                )
                              )
                            )
                          )
                  ),
                  tabItem(tabName = "findz",
                          fluidPage(
                            titlePanel("Confidence Intervals for a population mean (μ = 0 and σ = 1)"),
                            sidebarLayout(
                              sidebarPanel(
                                
                                h3(strong("Finding the z∗ Multiplier")),
                                h4("The value of the z∗ multiplier is dependent on the level of confidence."),
                                sliderInput("zlevel", "Confidence Level",
                                            min=.50, max = 0.99, value = 0.90, step = 0.01),
                                br(),
                                
                                wellPanel(
                                  
                                  style = "background-color: #A9A9A9;",
                                  
                                  h3("Quiz"),
                                  textInput("question1", "What is z∗  Multiplier for 90% confidence level?", "input answer with 3 decimals"),
                                  textInput("question2", "What is z∗  Multiplier for 95% confidence level?", "input answer with 3 decimals"),
                                  textInput("question3", "What is z∗  Multiplier for 99% confidence level?", "input answer with 3 decimals"),
                                  selectInput("question4", "Increasing the confidence level makes confidence interval getting larger?",
                                              c("Yes" = "y",
                                                "No" = "n",
                                                " " = "null"),selected = "null"),
                                  #textInput("question4", "Increasing the confidence level makes confidence interval getting larger? (YES or NO)", "input 'YES' or 'NO'"),
                                  br(),
                                  actionButton("submit", "Submit Answers")
                                )
                                
                              ),
                              
                              mainPanel(
                                plotOutput("zplot"),
                                bsPopover("zplot","Z Score Plot","This is the confidence interval plot for standard normal distribution. Multiplier Number (z*) is the absolute value of the boundary value. Use the value showed on this graph for following questions",
                                          trigger="hover",placement="bottom"),
                                br(),
                                br(),
                                h3("Feedback: "),
                                verbatimTextOutput("feedback")
                                
                              )
                            )
                          )
                  ),
                  tabItem(tabName = "popdiff",
                          titlePanel("Tests for Enrollment by Residency between University Park and Commonwealth Campuses"),
                          sidebarLayout(
                            sidebarPanel(
                              h3(strong("Design:")),
                              checkboxInput("testdesigncheckbox","Show design info:", TRUE),
                              uiOutput("testdesign"),
                              #h4("A researcher wants to sample a group of n University Park students and n students from other Penn State campuses to ask them about their experiences in college.  Although the percentage of Pennsylvania residents is 24.9% lower at University Park, a critic believes her sampling technique would provide a sample of students with a proportion (p) that does not depend on the campus (the null hypothesis). The researcher uses her samples to conduct a test of that null hypothesis and this app shows how that test would behave when the sampling is really unbiased and the University Park campus has a proportion that is 0.249 lower lower. "),
                              # h3(strong("For large random samples a confidence interval for the difference between two population means is given by")),
                              # h3("Difference Between the Sample Means ± z ∗ (Standard Error for Difference) "),
                              # h4("When two samples are independent of each other, Standard Error for a Difference between two sample summaries ="),
                              # img(src="2sample.png",height = 50,width = 350,algin = "middle"),
                              # h4("sqrt((standard error in first sample)^2+(standard error in second sample)^2)"),
                              #div(tableOutput('matrixScore'),style = "font-size:80%"), 
                              h3(strong("Population info:")),
                              img(src="2016Diff.png",height = "100%", width = "100%",algin = "middle"),
                              #img(src="2016Diff.png",height = 100,width = 350,algin = "middle"),
                              # h4("difference between twp μ = 524 - 494 = 30"),
                              # h4("difference between two σ = sqrt(0.021+0.015) = 0.190"),
                              h3(strong("Hypothesis: ")),
                              h4("Assume there is no difference of penn residency percentage between University Park and Other Campuses"),
                              h4("Ho: p(University Park) = p(Other Campuses)"),
                              h4("Ha: p(University Park) ≠ p(Other Campuses)"),
        
                              wellPanel(
                                h3(strong("New Sample Info")),
                                h4("Penn student percentage in two locations are shown below"),
                                tableOutput("sampleinfotable"),
                                uiOutput("Diffinfo")
                              ),
                               sliderInput("dlevel", "Confidence Level",
                                          min=.10, max = 0.99, value = 0.90, step = 0.01),
                               sliderInput("nSamp", "Sample Size for two groups",
                                          min=30, max = 150, value = 50, step = 5)
                            ),
                            
                            mainPanel(
                              plotOutput("dpopMean",height = "300px"),
                              bsPopover("dpopMean","Population Stacked Bar Graph","The two bars show precentage of Enrollment by Residency in University Park and Other Campuses.",
                                        trigger="hover",placement="bottom"),br(),
                              plotOutput("sampleDiff",height = "300px"),
                              bsPopover("sampleDiff","Sample Stacked Bar Graphs","These stacked bar graphs show the generated sample proportions of penn residency in University Park and Other Campuses. The horizontal lines on each bar indicate the population proportions for the two groups.",
                                        trigger="hover",placement="bottom"),
                              actionButton("newSample", "Generate New Samples",icon("paper-plane"),style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                              bsPopover("newSample","Note","By clicking on this button, new sample with the size you input will be generated on the Sample Histogram.",
                                        trigger="hover",placement="right"),
                              
                              splitLayout(
                                wellPanel(
                                  checkboxInput("CIcheckbox","Show Confidence Interval:", FALSE), 
                                  tableOutput("CItable")
                                ),
                                wellPanel(
                                  checkboxInput("testcheckbox","Show Test Output:", FALSE), 
                                  tableOutput("testtable")
                                )
                              ),
                             
                              wellPanel(
                                checkboxInput("decisioncheckbox","Decision about the null hypothesis:", FALSE), 
                                textOutput("decisionZ"),
                                br(),
                                textOutput("decisionP")
                              )
                            )
                          )
                          
                          
                  ),
                  
                  
                  
                  tabItem(tabName = "table",
                          fluidRow(
                            column(8,
                                   img(src='NormalTable.png')
                            )
                          )
                  )
                  
                )#end of tabItem
              )#end of dashboardBody
              
)

