library(shiny)
library(dplyr)
library(parallel)

shinyApp(
  ui = fluidPage(
    titlePanel("Socks"),
    # input number of simulations
    sidebarPanel(
      numericInput(
        "n_sims",
        "Number of simulations to run:",
        value = 10000,
        min = 1000,
        max = 1e6
      ),
      hr(),
      h4("Observed Data"),
      # inform users that the total number of picked socks is restricted to be 11
      h6("Number of picked socks is 11"),
      # only let users to change the number of singleton socks that are picked
      sliderInput(
        "n_odds",
        "Number of singleton socks picked",
        value = 11,
        min = 1,
        max = 11
      ),
      h6("Number of paired socks picked"),
      verbatimTextOutput("n_pairs"),
      hr(),
      h4("Prior on Total Number of Socks"),
      # specify three priors
      selectInput(
        "t_socks",
        "Prior Distribution for total number of socks",
        c(
          "Negative Binomial" = "nbinom",
          "Discrete Uniform" = "dunif",
          "Poisson" = "pois"
        )
      ),
      hr(),
      h4("Hyper Parameters"),
      # specify prior parameters
      conditionalPanel(
        condition = "input.t_socks == 'nbinom'",
        sliderInput(
          "nbinom_mu",
          HTML("Mean (expected total number of socks you have) - &mu;"),
          value = 30,
          min = 10,
          max = 200
        ),
        sliderInput(
          "nbinom_sd",
          HTML(
            "Standard Deviation (expected pair of socks in the laundry) - &sigma;"
          ),
          value = 15,
          min = 0,
          max = 100
        )
      ),
      conditionalPanel(
        condition = "input.t_socks == 'dunif'",
        sliderInput(
          "dunif_range",
          "Range:",
          value = c(10, 100),
          min = 10,
          max = 100
        )
      ),
      conditionalPanel(
        condition = "input.t_socks == 'pois'",
        sliderInput(
          "pois_lambda",
          HTML("Total prior - &lambda;:"),
          value = 35,
          min = 10,
          max = 100
        )
      ),
      hr(),
      h4("Prior on Proportion of Pairs"),
      h6("Beta Distribution"),
      sliderInput(
        "beta_a",
        HTML("Number of paired socks - &alpha;"),
        value = 15,
        min = 0,
        max = 50
      ),
      sliderInput(
        "beta_b",
        HTML("Number of singleton socks - &beta;"),
        value = 2,
        min = 0,
        max = 100
      ),
      hr(),
      checkboxInput("median", label = "Show medians", value = FALSE),
      checkboxInput("sd", label = "Show 95% credible intervals", value = FALSE),
      checkboxInput("true", label = "Show the true value", value = FALSE),
      checkboxInput("pos_table", label = "Show posterior summary statistics", value = FALSE),
      checkboxInput("pro_table", label = "Show prior summary statistics", value = FALSE),
      checkboxInput("answer", label = "Show what Karl Broman truly had!", value = FALSE)
    ),
    mainPanel(
      h4("Results"),
      plotOutput("t_socks"),
      br(),
      plotOutput("t_pairs"),
      br(),
      plotOutput("t_odds"),
      br(),
      plotOutput("prior_pairs"),
      br(),
      tableOutput("summary_pos_table"),
      br(),
      tableOutput("summary_pro_table"),
      br(),
      textOutput("messages"),
      br()
    )
  ),
  
  server = function(input, output, session) {
    # fix the number of pairs after observing the number of singletons
    n_pairs = reactive({
      (11 - input$n_odds)/2
    })
    
    # calculate and output the prior number of pairs picked by the user
    output$n_pairs = renderPrint({
      cat((11 - input$n_odds) / 2)
    })
    
    observe({
      updateSliderInput(
        session,
        inputId = "n_odds",
        min = 1,
        max = 11,
        step = 2
      )
    })
    # restrict on the negative binomial parameter sd
    # it shouldn't exceed sqrt(sd) + 1
    # explained in the write-up
    observe({
      updateSliderInput(
        session,
        inputId = "nbinom_sd",
        min = max(5, ceiling(sqrt(
          input$nbinom_mu
        ) + 1)),
        max = floor(input$nbinom_mu / 2)
      )
    })
    # negative binomial miu should be larger than the number of socks picked
    # explained in the write-up
    observe({
      updateSliderInput(session,
                        inputId = "nbinom_mu",
                        min = 11)
    })
    
    # the simulation function
    sock_sim = reactive({
      # redefine the input paramters outside the parallel function
      dunif_min = input$dunif_range[1]
      dunif_max = input$dunif_range[2]
      pois_lambda = input$pois_lambda
      nbinom_mu = input$nbinom_mu
      nbinom_sd = input$nbinom_sd
      beta_a = input$beta_a
      beta_b = input$beta_b
      t_socks = input$t_socks
      
      # defining the parallel function
      sock_simulation = function(i) {
        # simulate total number of socks t_socks
        if (t_socks == "dunif") {
          t_socks = sample(dunif_min:dunif_max, 1, replace = TRUE)
        }
        else if (t_socks == "pois") {
          t_socks = rpois(1, pois_lambda)
        }
        else if (t_socks == "nbinom") {
          prior_size_param = -nbinom_mu ^ 2 / (nbinom_mu - nbinom_sd ^ 2)
          t_socks = rnbinom(1, mu = input$nbinom_mu, size = prior_size_param)
        }
        else{
          stop()
        }
        # simulate proportion of pairs
        prior_pairs = rbeta(1, shape1 = beta_a, shape2 = beta_b)
        # total number of pairs
        t_pairs = round(floor(t_socks / 2) * prior_pairs)
        # total number of odds 
        t_odds = t_socks - 2 * t_pairs
        # calculate total number of picked socks from user input
        n_picked = 2 * n_pairs() + input$n_odds
        socks = rep(seq_len(t_pairs + t_odds), rep(c(2, 1), c(t_pairs, t_odds)))
        # sample socks with the specified proportions
        picked_socks = sample(socks, size =  min(n_picked, t_socks))
        sock_counts = table(picked_socks)
        
        c(sim_odds = sum(sock_counts == 1), sim_pairs = sum(sock_counts == 2),
          t_socks = t_socks, t_pairs = t_pairs, t_odds = t_odds, prior_pairs = prior_pairs)
      }
      n_sims = input$n_sims
      # use 4 cores at the same time
      # replicate it n_sims times
      # will return a matrix
      t(mcmapply(sock_simulation, seq_len(n_sims), mc.cores = 4))
    })
    
    # select the simulated values if they match the observed data
    post_draws = reactive({
      post_draws = sock_sim()[sock_sim()[, "sim_odds"] == input$n_odds & sock_sim()[, "sim_pairs" ] == n_pairs(), ]
      return(post_draws)
    })
    
    # Plot the posterior/prior total number of socks in the same plot
    output$t_socks = renderPlot({
      # save the densities separately 
      # for the convenience of setting ylim on the plot
      post_sock_draw = density(post_draws()[, "t_socks"])
      pror_sock_draw = density(sock_sim()[, "t_socks"])
      # plot posterior
      plot(post_sock_draw, ylim = c(0, 1.2*max(post_sock_draw$y, pror_sock_draw$y)),
           xlab = NA, type = 'l',
           main = "Density Plot of Posterior/Prior Total Number of Socks",
           col = "blue")
      # add prior
      lines(pror_sock_draw,col = "green")
      legend("topright",
             c("posterior total number of socks","prior total number of socks"),
             lty = c(1, 1),
             # gives the legend appropriate symbols (lines)
             lwd = c(2.5, 2.5),
             col = c("blue", "green")
      )
      # add medians
      if (input$median) {
        abline(v = median(post_draws()[, "t_socks"]),
               lty = 1,
               col = "blue")
      }
      # add 95% credible interval limits
      if (input$sd) {
        abline(v = quantile(post_draws()[, "t_socks"], c(0.025)), 
               lty = 2,
               col = "blue")
        abline(v = quantile(post_draws()[, "t_socks"], c(0.975)), 
               lty = 2,
               col = "blue")
      }
      # add true total number of socks (45)
      if (input$true) {
        abline(v = 21 * 2 + 3,
               lty = 1,
               col = "red")
      }
    })
    
    # Plot the posterior/prior number of paired socks in the same plot
    output$t_pairs = renderPlot({
      post_pairs_draw = density(post_draws()[, "t_pairs"])
      pror_pairs_draw = density(sock_sim()[, "t_pairs"])
      plot(post_pairs_draw, xlab = NA, ylim = c(0, 1.2*max(post_pairs_draw$y, pror_pairs_draw$y)),
           main = "Density Plot of Posterior/Prior Number of Paired Socks",
           col = "blue", type = 'l')
      lines(pror_pairs_draw,col = "green")
      legend("topright",
             c("posterior number of paired socks","prior number of paired socks"),
             lty = c(1, 1),
             # gives the legend appropriate symbols (lines)
             lwd = c(2.5, 2.5),
             col = c("blue", "green")
      )
      if (input$median) {
        abline(v = median(post_draws()[, "t_pairs"]),
               lty = 1,
               col = "blue")
      }
      if (input$sd) {
        abline(v = quantile(post_draws()[, "t_pairs"], c(0.025)), 
               lty = 2,
               col = "blue")
        abline(v = quantile(post_draws()[, "t_pairs"], c(0.975)), 
               lty = 2,
               col = "blue")
      }
      if (input$true) {
        abline(v = 21,
               lty = 1,
               col = "red")
      }
    })
    
    # Plot the posterior/prior number of odd socks in the same plot
    output$t_odds = renderPlot({
      post_odd_draw = density(post_draws()[, "t_odds"])
      pror_odd_draw = density(sock_sim()[, "t_odds"], adjust = 1.8)
      plot(post_odd_draw, ylim = c(0, 1.2*max(post_odd_draw$y, pror_odd_draw$y)),
           xlab = NA, type = 'l', 
           main = "Density Plot of Posterior/Prior Number of odd Socks",
           col = "blue")
      lines(pror_odd_draw,col = "green")
      legend("topright",
             c("posterior number of Odd socks","prior number of odd socks"),
             lty = c(1, 1),
             # gives the legend appropriate symbols (lines)
             lwd = c(2.5, 2.5),
             col = c("blue", "green")
      )
      if (input$median) {
        abline(v = median(post_draws()[, "t_odds"]),
               lty = 1,
               col = "blue")
      }
      if (input$sd) {
        abline(v = quantile(post_draws()[, "t_odds"], c(0.025)), 
               lty = 2,
               col = "blue")
        abline(v = quantile(post_draws()[, "t_odds"], c(0.975)), 
               lty = 2,
               col = "blue")
      }
      if (input$true) {
        abline(v = 3,
               lty = 1,
               col = "red")
      }
    })
    
    # Plot the posterior/prior proportion of pairs in the same plot
    output$prior_pairs = renderPlot({
      post_prop_draw = density(post_draws()[, "prior_pairs"])
      pror_prop_draw = density(sock_sim()[, "prior_pairs"])
      plot(post_prop_draw, ylim = c(0, 1.2*max(post_prop_draw$y, pror_prop_draw$y)),
           xlab = NA, type = 'l', 
           main = "Density Plot of Posterior/Prior Proportion of Paired Socks",
           col = "blue")
      lines(pror_prop_draw, col = "green")
      legend("topright",
             c("posterior proportion of paired socks","prior proportion of paired socks"),
             lty = c(1, 1),
             # gives the legend appropriate symbols (lines)
             lwd = c(2.5, 2.5),
             col = c("blue", "green")
      )
      if (input$median) {
        abline(v = median(post_draws()[, "prior_pairs"]),
               lty = 1,
               col = "blue")
      }
      if (input$sd) {
        abline(v = quantile(post_draws()[, "prior_pairs"], c(0.025)), 
               lty = 2,
               col = "blue")
        abline(v = quantile(post_draws()[, "prior_pairs"], c(0.975)), 
               lty = 2,
               col = "blue")
      }
      if (input$true) {
        abline(v = 21*2/45,
               lty = 1,
               col = "red")
      }
    })
    
    # print the summary tables
    output$summary_pos_table = renderTable({
      # if true
      if (input$pos_table){
        # initialize empty vectors
        pos1 = c()
        pos2 = c()
        pos3 = c()
        pos4 = c()
        for(i in 3:6){
          # append the mean of each statistics
          pos1 = c(pos1, round(mean(post_draws()[,i])))
          pos2 = c(pos2, round(median(post_draws()[,i])))
          pos3 = c(pos3, round(quantile(post_draws()[,i], 0.025)))
          pos4 = c(pos4, round(quantile(post_draws()[,i], 0.975)))
        }
        name_vec = c("total number of socks", 
                      "number of pairs","number of odds", 
                      "proportion of pairs")
        d1 = data.frame(name_vec, pos1, pos2, pos3, pos4)
        colnames(d1) = c("names", "mean", "median", "95% lower limit", "95% upper limit")
        d1
      }
    }, caption = "Poesterior Summary Statistics Table",
    caption.placement = getOption("top")
    )
    
    output$summary_pro_table = renderTable({
      if (input$pro_table){
        pro1 = c()
        pro2 = c()
        pro3 = c()
        pro4 = c()
        for(i in 3:6){
          pro1 = c(pro1, round(mean(sock_sim()[,i])))
          pro2 = c(pro2, round(median(sock_sim()[,i])))
          pro3 = c(pro3, round(quantile(sock_sim()[,i], 0.025)))
          pro4 = c(pro4, round(quantile(sock_sim()[,i], 0.975)))
        }
        name_vec = c("total number of socks", 
                     "number of pairs","number of odds", 
                     "proportion of pairs")
        d2 = data.frame(name_vec, pro1, pro2, pro3, pro4)
        colnames(d2) = c("names", "mean", "median", "95% lower limit", "95% upper limit")
        d2
      }
    }, caption = "Prior Summary Statistics Table",
    caption.placement = getOption("top")
    )
    
    # show or hide the true answer
    output$messages = renderText({
      if (input$answer){
        paste("Aha! Karl Broman actually had 21 pairs and 3 singletons!")
      }
    })
  }
)
