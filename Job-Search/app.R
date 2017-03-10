source("functions_def.R")
source("preprocessing.R")

# ui.R
shinyApp(
  ui = fluidPage(
    # Change the theme of shiny app
    theme = shinythemes::shinytheme("united"),

    # Change the style of the title of the app
    tags$head(
      tags$style(HTML("@import url('//fonts.googleapis.com/css?family=Lobster|Cabin:400,700');"))
    ),

    headerPanel(
      h1("Data-related Job Search with Indeed",
         style = "font-family: 'Lobster', cursive;
         font-weight: 1000; line-height: 2.1;
         color: #4d3a7d;")),

    # Sidebar with a slider input for sock parameters
    sidebarPanel(
      # insert a job seaerch related image for decoration:
      HTML('<img src="http://stat.duke.edu/sites/stat.duke.edu/files/documents/dukestatsci.jpg" width="250" height="160" border="0"> </a>'),
      hr(),

      # input the job category to search
      selectInput("category", "Select a Job Category:", choices = job_category, selected = FALSE),
      hr(),
      # input the location to search
      selectInput("state", "Select a State:", choices = states$State, selected = FALSE),
      hr(),
      p('Shiny App made by Team Standard Deviants in R, using the jobbR, wordcloud, googleVis and other packages.'),
      p('Special thanks to Prof. Colin Rundel and the amazing course STA 523 at Duke University:'),
      HTML('<a href="http://www2.stat.duke.edu/~cr173/Sta523_Fa16">
           <img src="http://chem.duke.edu/sites/chem.duke.edu/themes/dukechem/images/duke-footer-logo.png" width="150" height="60" border="0" alt="View the STA 523 Course Website"> </a>'),
      tags$div(
        HTML('<span id=indeed_at><a href="http://www.indeed.com/">jobs</a> powered by <a
             href="http://www.indeed.com/" title="Job Search"><img
             src="http://www.indeed.com/p/jobsearch.gif" style="border: 0;
             vertical-align: middle;" width="80" height="25" alt="Indeed job search"></a></span>'
        )
      )
    ),

    # main panel with tabbed interface
    mainPanel(

      h4("Example Job Titles and Employees for the Selected Category:"),
      # output a table to explain the meaning of category:
      tableOutput("category"),
      hr(),
      # two tabsets to show the word cloud plot.
      tabsetPanel(
        tabPanel("Word Cloud", plotOutput("plot")),
        tabPanel("Job Map", htmlOutput("view"))
      )
    )
  ),

  # server.R
  server = function(input, output) {
    # the outputs for table:
    output$category = renderTable({
      job.title = category %>%
        select(JobTitle, Cluster) %>%
        filter(Cluster == input$category) %>%
        group_by(JobTitle) %>%
        count() %>%
        arrange(desc(n)) %>%
        select(JobTitle) %>%
        slice(1:3)
      company = category %>%
        select(Company, Cluster) %>%
        filter(Cluster == input$category) %>%
        group_by(Company) %>%
        count() %>%
        arrange(desc(n)) %>%
        select(Company) %>%
        slice(1:3)
      data.frame(job.title,company) %>%
        setNames(c("Job Title", "Company"))
    })

    # render plots for word cloud:
    output$plot = renderPlot({
      df = data()
      corpus = clean_corpus(data2corpus(df))

      if(nrow(df)==0) {
        plot(0, main = "No jobs found in the category in this state", cex = 0, axes = FALSE, xlab = NA, ylab = NA)
      }
      else{
        WordCloud(corpus)
      }
    })

    # update the scraping process each time when a different state is seleceted:
    jobs = reactive({
      ab = states$Abbreviation[states$State==input$state]
      job_train %>% filter(results.state == ab) # subsetting jobs from the training dataframe
    })

    # transform the dataframe to document term matrix and do some cleaning:
    data = reactive({
      dat = jobs()

      # clean the dataframe:
      job_dat = data2corpus(dat)
      clean_dat = clean_corpus(job_dat)
      stem_clean_dat = clean_dat %>% tm_map(stemDocument)
      dtm = DocumentTermMatrix(stem_clean_dat) # transform into document term matrix

      # remove terms that are in greater than 80% of documents
      dtm_reduced = removeCommonTerms(dtm, 0.8)
      dtm_dense = removeSparseTerms(dtm_reduced, 0.9)
      dat$cluster = LDA_predict(lda_model, dtm_dense)
      dat = dat %>% filter(cluster==input$category)
    })

    # plot the locations on Google Map:
    output$view = renderGvis({
      map_jobs(data())
    })
  }
)



