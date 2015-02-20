library(shiny)

shinyUI(navbarPage("Shiny Project",
                   tabPanel("Documentation",
                            withMathJax(), h3("Why is the Variance Estimator \\(S^2\\) divided by \\(n-1?\\)"),
                            p("Variance is a statistical measure of spread of a given distribution.
               For a discrete variable, \\(X\\), with \\(n\\) equally likely events, the ",
                              strong("population variance"),
                              " is calculated as $$\\sigma^2 = \\frac{1}{n}\\sum_{i=1}^n (X_i - \\bar X)^2$$"),
                            p("However, we ", em("rarely"),
                              " know the population and often are only provided a sample."), br(),
                            p("The ", strong("sample variance")," can be calculated in ", strong(em("two")),
                              " different ways:",
                              "$$S^2 \\mbox{(unbiased)} = \\frac{\\sum_{i=1}^n (X_i - \\bar X)^2}{n-1}
                              ~~~\\mbox{and}~~~S^2\\mbox{(biased)} = \\frac{\\sum_{i=1}^n (X_i - \\bar X)^2}{n}$$",
                              "The unbiased calculation is most often used, as it provides a ",
                              strong(em("more accurate")), " estimate of population variance"),
                            br(),
                            p("The reason that \\(S^2\\) is divided by \\(n-1\\) instead of \\(n\\) is to adjust
                              for the fact that picking a sample out of a population tends to ",
                              strong("underestimate")," the variance."),
                            br(), p("To show this empirically, we simulated the following in the ",
                                    strong("Simulation Experiment"), " tab: "), br(),
                            tags$ol(
                                tags$li("Create a population by drawing a number of observations from values 1 to 20."),
                                tags$li("Draw a number of samples of specified size from the population"),
                                tags$li("Plot the difference between individual sample variance and the true population variance"),
                                tags$li("Show the effects of sample size vs accuracy of variance estimated")
                            ), br(),
                            p("The user is able to specify the ", strong(em("Population Size, Number of Samples,")),
                              "and ", strong(em("Sample Size,")), " so that the corresponding plots will respond reactively.")),

                   tabPanel("Simulation Experiment",
                            fluidRow(
                                column(4, div(style = "height: 150px")),
                                column(4, div(style = "height: 150px")),
                                column(4, div(style = "height: 150px"))),
                            fluidRow(
                                column(12,h4("We start by generating a population of ",
                                             span(textOutput("population", inline = TRUE), style = "color: red; font-size: 20px"),
                                             " observations from values 1 to 20:"),
                                       tags$hr(),htmlOutput("popHist"),
                                       h4("Next, we generate ",
                                          span(textOutput("numSample", inline = TRUE), style = "color: red; font-size: 20px"),
                                          " samples of size ",
                                          span(textOutput("sampleSize", inline = TRUE), style = "color: red; font-size: 20px"),
                                          " from the population and plot their biased variances:"),
                                       tags$hr(),htmlOutput("sampBiaVarHist"),htmlOutput("sampUnbiaVarHist"),
                                       h4("From below, we can see the biased calculation almost always ",
                                          span("underestimates", style = "color: red; font-size: 20px"),
                                          " the population variance,"),
                                       tags$hr(), plotOutput("varPlot", height = "300px"),
                                       h4("while the unbiased calculation is almost always a",
                                          span("more accurate", style = "color: red; font-size: 20px"),
                                          " estimate."),
                                       tags$hr(), plotOutput("unbiaVarPlot", height = "300px"),
                                       h4("As the sample sizes decreases, the biased variance estimate becomes ",
                                          span("more inaccurate", style = "color: red; font-size: 20px")),
                                       tags$hr(), htmlOutput("varDiffPlot"),
                                       h4("but the unbiased calculation remains ",
                                          span("consistent", style = "color: red; font-size: 20px")),
                                       tags$hr(), htmlOutput("unbiaVarDiffPlot"),
                                       style = "padding-left: 20px"
                                )
                            ),
                            absolutePanel(
                                top = 50, left = 0, right =0,
                                fixed = TRUE,
                                wellPanel(
                                    fluidRow(
                                        column(4, sliderInput("population",
                                                              "Size of Population:",
                                                              min = 100,
                                                              max = 500,
                                                              value = 250),
                                               p(strong("Population Variance: "), textOutput("popVar", inline = TRUE))),
                                        column(4, sliderInput("numSample",
                                                              "Number of Samples:",
                                                              min = 100,
                                                              max = 500,
                                                              value = 300),
                                               p(strong("Sample Variance (biased): "), textOutput("biaVar", inline = TRUE))),
                                        column(4, sliderInput("sampleSize",
                                                              "Size of Samples:",
                                                              min = 2,
                                                              max = 15,
                                                              value = 10),
                                               p(strong("Sample Variance (unbiased): "), textOutput("unbiaVar", inline = TRUE)))),

                                    style = "opacity: 0.92; z-index: 100;"
                                ))



                   )
))
