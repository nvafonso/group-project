# PROTOTYPE v2 - added comments + we are focusing on teamwork quality
#app.R
# Main file for the HEXACO & Teamwork Quality Explorer Shiny application.
# HEXACO & Teamwork Quality Explorer
# Run with: shiny::runApp("hexaco_teamwork_shiny_v2")
#
# Observed-data exploration is separate from hypothetical simulation.
# Simulation scores are illustrative assumptions, not empirical predictions.

# Load the packages used to build the interactive app and its plots.
library(shiny)
library(ggplot2)

# URL used to download the dataset if no local data.csv file is available.
data_url <- "https://osf.io/download/k4syv/"

# Define which questionnaire columns belong to each of the six HEXACO traits.
# The names ending in 'r' indicate reverse-keyed items; see reverse_hexaco below.
hexaco_items <- list(
  honesty_humility = c("F06","F30r","F54","F12r","F36","F60r","F18","F42r","F24r","F48r"),
  emotionality = c("F05","F29","F53r","F11","F35r","F17","F41r","F23","F47","F59r"),
  extraversion = c("F04","F28r","F52r","F10r","F34","F58","F16","F40","F22","F46r"),
  agreeableness = c("F03","F27","F09r","F33","F51","F15r","F39","F57r","F21r","F45"),
  conscientiousness = c("F02","F26r","F08","F32r","F14r","F38","F50","F20r","F44r","F56r"),
  openness = c("F01r","F25","F07","F31r","F13","F37","F49r","F19","F43","F55")
)

# Define which questionnaire columns belong to each teamwork-quality (TWQ) dimension.
# Items ending in 'r' are reverse-keyed and are handled below.
twq_items <- list(
  communication = c("CMNC01","CMNC02","CMNC03","CMNC04r","CMNC05","CMNC06r","CMNC07r","CMNC08","CMNC09","CMNC10"),
  coordination = c("CRDN1","CRDN2","CRDN3","CRDN4r"),
  balance = c("BLNC1","BLNC2","BLNC3r"),
  support = c("SPRT1","SPRT2","SPRT3","SPRT4","SPRT5","SPRT6"),
  commitment = c("COMT1","COMT2","COMT3","COMT4r"),
  cohesion = c("COHS01","COHS02r","COHS03","COHS04","COHS05","COHS06r","COHS07","COHS08","COHS09","COHS10")
)

# List the HEXACO item columns that must be reverse-scored.
# For a 1-to-5 response scale, reversing uses 6 - response.
reverse_hexaco <- c("F12r","F24r","F30r","F42r","F48r","F60r",
                    "F35r","F41r","F53r","F59r","F10r","F28r","F46r","F52r",
                    "F09r","F15r","F21r","F57r","F14r","F20r","F26r","F32r","F44r","F56r",
                    "F01r","F31r","F49r")
reverse_twq <- c("CMNC04r","CMNC06r","CMNC07r","CRDN4r","BLNC3r","COMT4r","COHS02r","COHS06r")

# Friendly labels shown in the interface and plot axes instead of raw variable names.
nice_names <- c(
  honesty_humility = "Honesty–Humility", emotionality = "Emotionality",
  extraversion = "Extraversion", agreeableness = "Agreeableness",
  conscientiousness = "Conscientiousness", openness = "Openness",
  communication = "Communication", coordination = "Coordination",
  balance = "Balance of contributions", support = "Support",
  commitment = "Commitment", cohesion = "Cohesion"
)

# Load the dataset. Prefer a local data.csv file; otherwise try downloading it.
# read.csv2 and read.csv handle different common CSV separator conventions.
load_data <- function() {
  if (file.exists("data.csv")) {
    d <- read.csv("data.csv", check.names = FALSE)
  } else {
    d <- tryCatch(read.csv2(data_url, check.names = FALSE), error = function(e) NULL)
    if (is.null(d) || !nrow(d)) {
      d <- tryCatch(read.csv(data_url, check.names = FALSE), error = function(e) NULL)
    }
  }
  validate(need(!is.null(d) && nrow(d) > 0, paste(
    "Could not load the dataset. Check your internet connection or place a copy",
    "of the dataset in this folder as data.csv."
  )))
  d
}

# Prepare the data by reverse-scoring specified items and computing scale averages.
# The function returns the original data plus the derived HEXACO and TWQ scale columns.
score_scales <- function(d) {
  for (nm in intersect(reverse_hexaco, names(d))) d[[nm]] <- 6 - as.numeric(d[[nm]])
  for (nm in intersect(reverse_twq, names(d))) d[[nm]] <- 6 - as.numeric(d[[nm]])
  for (scale in names(hexaco_items)) {
    items <- hexaco_items[[scale]]
    if (all(items %in% names(d))) d[[scale]] <- rowMeans(d[, items, drop = FALSE], na.rm = FALSE)
  }
  for (scale in names(twq_items)) {
    items <- twq_items[[scale]]
    if (all(items %in% names(d))) d[[scale]] <- rowMeans(d[, items, drop = FALSE], na.rm = FALSE)
  }
  d
}

# Define a shared ggplot theme so plots use consistent typography and colours.
theme_app <- function() {
  theme_minimal(base_size = 13) +
    theme(
      plot.title = element_text(face = "bold", size = 15, colour = "#183153"),
      plot.subtitle = element_text(colour = "#52647A"),
      axis.title = element_text(face = "bold", colour = "#334155"),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(colour = "#E5EAF1"),
      plot.background = element_rect(fill = "white", colour = NA),
      panel.background = element_rect(fill = "white", colour = NA)
    )
}

# USER INTERFACE (UI)
# This section defines the visible layout, controls, tabs, and output placeholders.
ui <- fluidPage(
  # CSS controls the app-wide visual design: background, cards, tabs, fonts, buttons, and sliders.
  tags$head(tags$style(HTML("
    @import url('https://fonts.googleapis.com/css2?family=DM+Sans:wght@400;500;600;700&family=Manrope:wght@500;600;700;800&display=swap');
    body { background:#F4F7FB; color:#243247; font-family:'DM Sans',sans-serif; }
    .container-fluid { max-width:1450px; padding:22px 30px 36px; }
    .hero { background:linear-gradient(120deg,#162D50,#245C78); color:white; padding:28px 32px; border-radius:20px; margin-bottom:22px; box-shadow:0 10px 26px rgba(22,45,80,.13); }
    .hero h1 { font-family:'Manrope',sans-serif; font-weight:800; margin:0 0 8px; font-size:30px; }
    .hero p { color:#DCEAF5; margin:0; font-size:15px; }
    .nav-tabs { border-bottom:1px solid #DDE5EF; margin-bottom:20px; }
    .nav-tabs>li>a { border:0!important; border-radius:10px 10px 0 0; color:#64748B; font-weight:700; padding:13px 18px; }
    .nav-tabs>li.active>a,.nav-tabs>li.active>a:hover { color:#174A68; background:white; border-bottom:3px solid #2B7A91!important; }
    .well,.panel-card { background:white; border:1px solid #E1E8F0; border-radius:16px; box-shadow:0 4px 14px rgba(30,55,90,.04); }
    .panel-card { padding:22px; margin-bottom:18px; }
    h3,h4 { font-family:'Manrope',sans-serif; color:#183153; font-weight:800; }
    .btn-primary { background:#216A80; border-color:#216A80; border-radius:10px; font-weight:700; padding:10px 18px; }
    .btn-primary:hover { background:#174A68; border-color:#174A68; }
    .form-control { border-radius:9px; border-color:#D5DFEA; box-shadow:none; }
    .help-block,.text-muted,.section-intro { color:#718096; }
    .section-intro { margin-top:-4px; margin-bottom:18px; }
    .metric { background:#F1F7FA; border:1px solid #DCEAF0; border-radius:13px; padding:14px 16px; margin-bottom:10px; }
    .metric-label { text-transform:uppercase; font-size:11px; letter-spacing:.08em; color:#64748B; font-weight:800; }
    .metric-value { font-size:25px; color:#174A68; font-family:'Manrope',sans-serif; font-weight:800; }
    .notice { background:#FFF8E8; border-left:4px solid #E4AD42; border-radius:8px; padding:12px 14px; color:#755A22; }
    .irs--shiny .irs-bar,.irs--shiny .irs-single,.irs--shiny .irs-from,.irs--shiny .irs-to { background:#28778D; border-top-color:#28778D; border-bottom-color:#28778D; }
    .irs--shiny .irs-handle { border-color:#28778D; }
  "))),
  # Header banner shown at the top of the app.
  div(class="hero",
      h1("Personality & Teamwork Quality Explorer"),
      p("Explore observed questionnaire data and test transparent, hypothetical team-formation scenarios.")
  ),
  # Organize the app into three tabs: observed data, simulations, and background information.
  tabsetPanel(
    # TAB 1: EXPLORE OBSERVED DATA
    # Users select a HEXACO trait and a teamwork-quality dimension to inspect their association.
    tabPanel("Explore the data",
      br(),
      sidebarLayout(
        sidebarPanel(
          h4("Choose your variables"),
          p(class="section-intro","Compare a personality trait with one teamwork-quality dimension."),
          selectInput("trait","HEXACO trait",choices=nice_names[names(hexaco_items)],selected="agreeableness"),
          selectInput("outcome","Teamwork-quality dimension",choices=nice_names[names(twq_items)],selected="communication"),
          checkboxInput("show_lm","Show trend line and confidence band",TRUE),
          helpText("Each point represents one participant.")
        ),
        mainPanel(
          div(class="panel-card",h3("Observed relationship"),p(class="section-intro","Participant-level questionnaire scores."),plotOutput("scatter",height=390)),
          div(class="panel-card",h4("Exploratory linear regression"),verbatimTextOutput("model_summary"),
              p(class="text-muted","Interpret cautiously: the sample is small and participants are nested within teams. Association does not imply causation."))
        )
      )
    ),
    # TAB 2: SIMULATE TEAMS
    # Users set team sizes, select the research question, and specify hypothetical model effects.
    tabPanel("Simulate teams",
      br(),
      p(class="section-intro","Compare team-composition assumptions and grouping strategies. All simulated outcomes are hypothetical."),
      sidebarLayout(
        sidebarPanel(
          # Simulation controls: number of teams, team size, trait, outcome, effects, noise, and repetitions.
          h4("1 · Build the scenario"),
          sliderInput("n_teams","Number of teams",min=2,max=30,value=8,step=1,sep=""),
          sliderInput("team_size","People per team",min=2,max=10,value=4,step=1,sep=""),
          div(class="metric",div(class="metric-label","Total simulated participants"),div(class="metric-value",textOutput("total_people",inline=TRUE))),
          tags$hr(),
          radioButtons("research_question","What would you like to investigate?",
             choices=c("Composition effect"="composition","Grouping-strategy effect"="strategy","Both effects"="both"),
             selected="both"),
          selectInput("sim_trait","Personality trait",choices=nice_names[names(hexaco_items)],selected="agreeableness"),
          selectInput("sim_outcome","Teamwork-quality dimension",choices=nice_names[names(twq_items)],selected="communication"),
          sliderInput("composition_effect","Assumed effect of team mean trait score",min=0,max=1,value=0.3,step=0.05),
          sliderInput("diversity_effect","Assumed effect of within-team trait diversity",min=-1,max=1,value=0,step=0.05),
          sliderInput("noise_sd","Random noise (SD)",min=0,max=1.5,value=0.4,step=0.05),
          sliderInput("repetitions","Repeated simulation runs",min=100,max=1000,value=300,step=100,sep=""),
          actionButton("run_sim","Run comparison",class="btn-primary",icon=icon("play"),width="100%")
        ),
        mainPanel(
          div(class="panel-card",
            # Results panel: displays the simulation comparison plot and summary table.
          h3("2 · Compare outcomes"),
            p(class="section-intro","Each point in the distribution represents the average outcome from one simulated run."),
            fluidRow(
              column(4,div(class="metric",div(class="metric-label","Teams per run"),div(class="metric-value",textOutput("metric_teams",inline=TRUE)))),
              column(4,div(class="metric",div(class="metric-label","People per team"),div(class="metric-value",textOutput("metric_size",inline=TRUE)))),
              column(4,div(class="metric",div(class="metric-label","Simulation runs"),div(class="metric-value",textOutput("metric_reps",inline=TRUE))))
            ),
            plotOutput("strategy_plot",height=340),
            h4("Summary by strategy"),
            tableOutput("summary_table")
          ),
          # Additional plot showing team trait composition versus simulated teamwork quality.
          div(class="panel-card",h3("3 · Understand the assumptions"),
            plotOutput("composition_plot",height=300),
            p(class="text-muted","The model uses observed trait and teamwork scores to set a baseline, but the composition and diversity effects are user-specified assumptions. They are not estimated causal effects.")
          ),
          div(class="notice",strong("Interpretation: "),
              "A strategy may reduce variation between teams without increasing the overall average. Results describe the model you specified, not proven effects in real teams.")
        )
      )
    ),
    # TAB 3: ABOUT
    # Briefly explains what the app does and highlights checks needed before submission.
    tabPanel("About",
      br(),
      div(class="panel-card",
        h3("About this prototype"),
        p("The app links observed questionnaire relationships with a transparent simulation exercise."),
        tags$ul(
          tags$li("The observed-data tab explores the six HEXACO traits and six teamwork-quality dimensions."),
          tags$li("The simulation compares random assignment, balancing a trait across teams, and grouping similar trait scores."),
          tags$li("Composition effects and within-team diversity effects are separate assumptions that can be varied independently."),
          tags$li("Repeated simulations show how much results vary under the specified model.")
        ),
        div(class="notice",strong("Before final submission: "),
          "verify reverse-keying and scale construction against the original study documentation, and review the simulation assumptions and assignment logic as a group.")
      )
    )
  )
)

# SERVER LOGIC
# This section reacts to user inputs, processes data, runs simulations, and creates outputs.
server <- function(input,output,session) {
  # Load and score the data reactively so the processed dataset is available to outputs.
  raw_data <- reactive(score_scales(load_data()))
  output$total_people <- renderText(input$n_teams*input$team_size)
  output$metric_teams <- renderText(input$n_teams)
  output$metric_size <- renderText(input$team_size)
  output$metric_reps <- renderText(input$repetitions)

  # OBSERVED-DATA SCATTERPLOT
  # Draw one point per participant and optionally add a fitted linear trend line.
  output$scatter <- renderPlot({
    d <- raw_data()
    validate(need(input$trait %in% names(d) && input$outcome %in% names(d),"Selected scores are unavailable. Check the dataset column names."))
    p <- ggplot(d,aes(x=.data[[input$trait]],y=.data[[input$outcome]]))+
      geom_point(alpha=.75,size=2.4,colour="#28778D")+
      labs(x=unname(nice_names[input$trait]),y=unname(nice_names[input$outcome]),
           title=paste(unname(nice_names[input$trait]),"and",unname(nice_names[input$outcome])))+theme_app()
    if (isTRUE(input$show_lm)) p <- p+geom_smooth(method="lm",se=TRUE,colour="#D18A35",fill="#F4D7A6",linewidth=1)
    p
  })

  # OBSERVED-DATA LINEAR REGRESSION
  # Fit a simple linear model predicting the selected teamwork score from the selected trait.
  # This is exploratory and does not account for participants being nested in teams.
  output$model_summary <- renderPrint({
    d <- raw_data()
    validate(need(input$trait %in% names(d) && input$outcome %in% names(d),"Scores unavailable."))
    summary(lm(reformulate(input$trait,response=input$outcome),data=d))
  })

  # SIMULATION ENGINE
  # Recalculate the simulation when the user clicks the Run comparison button.
  # The generated scores are hypothetical and depend on user-selected assumptions.
  simulations <- eventReactive(input$run_sim, {
    d <- raw_data()
    validate(need(input$sim_trait %in% names(d) && input$sim_outcome %in% names(d),"Selected scores unavailable."))
    x <- d[[input$sim_trait]]
    y <- d[[input$sim_outcome]]
    ok <- is.finite(x)&is.finite(y)
    x <- x[ok]; y <- y[ok]
    validate(need(length(x)>=4,"Not enough complete observations for simulation."))

    n_teams <- input$n_teams
    team_size <- input$team_size
    reps <- input$repetitions
    trait_name <- input$sim_trait
    strategy_names <- c("Random","Balanced","Similar")
    all_results <- vector("list", reps*length(strategy_names))
    idx <- 1L

    # Repeat the complete sampling and team-formation process for each simulation run.
    for (r in seq_len(reps)) {
      sampled <- sample(seq_along(x), size=n_teams*team_size, replace=TRUE)
      person_trait <- x[sampled]
      person_outcome <- y[sampled]
      # STRATEGY 1: randomly assign participants while preserving equal team sizes.
      random_team <- sample(rep(seq_len(n_teams), each=team_size))
      ord <- order(person_trait)
      # STRATEGY 3: group participants with similar trait scores together.
      sim_team <- integer(length(ord))
      sim_team[ord] <- rep(seq_len(n_teams),each=team_size)
      # STRATEGY 2: distribute low and high scores across teams using a back-and-forth assignment.
      bal_team <- integer(length(ord))
      for (k in seq_along(ord)) {
        sorted_position <- k
        row <- (sorted_position-1L) %/% n_teams
        col <- (sorted_position-1L) %% n_teams
        if (row %% 2L == 1L) col <- n_teams-1L-col
        bal_team[ord[k]] <- col+1L
      }
      strategies <- list(Random=random_team, Balanced=bal_team, Similar=sim_team)

      # Calculate team statistics and simulated outcomes separately for each strategy.
      for (sn in names(strategies)) {
        team_id <- strategies[[sn]]
        team_trait <- as.numeric(tapply(person_trait,team_id,mean))
        team_diversity <- as.numeric(tapply(person_trait,team_id,sd))
        team_base <- as.numeric(tapply(person_outcome,team_id,mean))
        # Standardize team means and diversity so the effect sliders act on comparable z-score scales.
        z_mean <- as.numeric(scale(team_trait))
        z_div <- as.numeric(scale(team_diversity))
        if (all(!is.finite(z_mean))) z_mean <- rep(0,length(team_trait))
        if (all(!is.finite(z_div))) z_div <- rep(0,length(team_diversity))
        # Build hypothetical scores from a baseline, user-chosen effects, and random noise.
        # These coefficients are assumptions, not effects estimated from the observed data.
        score <- mean(y,na.rm=TRUE) +
          input$composition_effect*z_mean +
          input$diversity_effect*z_div +
          rnorm(n_teams,mean=0,sd=input$noise_sd)
        score <- pmin(5,pmax(1,score))
        # Save one row per team, including its run, strategy, trait composition, and simulated score.
        all_results[[idx]] <- data.frame(
          run=r,strategy=sn,team=seq_len(n_teams),
          mean_trait=team_trait,within_team_sd=team_diversity,
          baseline_quality=team_base,simulated_quality=score
        )
        idx <- idx+1L
      }
    }
    # Combine the saved rows from all repetitions and strategies into one data frame.
    do.call(rbind,all_results)
  },ignoreInit=TRUE)

  # FILTER RESULTS FOR THE CURRENT RESEARCH QUESTION
  # For the composition-only view, keep the Random strategy; otherwise retain all strategies.
  selected_results <- reactive({
    req(simulations())
    z <- simulations()
    if (input$research_question=="composition") z <- z[z$strategy=="Random",,drop=FALSE]
    z
  })

  # PLOT 1: Depending on the selected question, show a composition relationship
  # or compare run-level average scores across the three grouping strategies.
  output$strategy_plot <- renderPlot({
    z <- selected_results()
    validate(need(nrow(z)>0,"Run the simulation to see results."))
    z$run_average <- ave(z$simulated_quality,z$run, FUN=mean)
    if (input$research_question=="composition") {
      ggplot(z,aes(x=mean_trait,y=simulated_quality))+
        geom_point(alpha=.15,colour="#28778D")+
        geom_smooth(method="lm",se=TRUE,colour="#D18A35",fill="#F4D7A6")+
        labs(x=paste("Team mean",unname(nice_names[input$sim_trait])),y="Simulated teamwork-quality score",
             title="Composition and simulated quality",subtitle="Randomly formed teams; each point is a team in a repeated run")+
        theme_app()
    } else {
      avg <- aggregate(run_average~run+strategy,data=z,FUN=mean)
      ggplot(avg,aes(x=strategy,y=run_average,fill=strategy))+
        geom_boxplot(alpha=.8,outlier.alpha=.25)+
        scale_fill_manual(values=c(Random="#28778D",Balanced="#D18A35",Similar="#7396B8"))+
        labs(x=NULL,y="Mean simulated teamwork-quality score per run",
             title="Compare grouping strategies",subtitle="Distribution of run-level average scores")+
        theme_app()+theme(legend.position="none")
    }
  })

  # PLOT 2: Show team mean trait score against simulated quality, with a line per strategy.
  output$composition_plot <- renderPlot({
    z <- simulations()
    validate(need(nrow(z)>0,"Run the simulation to see results."))

    ggplot(z,aes(x=mean_trait,y=simulated_quality,colour=strategy))+
      geom_point(alpha=.16,size=1.8)+
      geom_smooth(method="lm",se=FALSE,linewidth=.9)+
      scale_colour_manual(values=c(Random="#28778D",Balanced="#D18A35",Similar="#7396B8"))+
      labs(
        x=paste("Team mean",unname(nice_names[input$sim_trait])),
        y="Simulated teamwork-quality score",
        title="Team trait composition and simulated quality",
        subtitle="Lines show the modelled association within each grouping strategy",
        colour="Grouping strategy"
      )+
      theme_app()+theme(legend.position="bottom")
  })

  # SUMMARY TABLE
  # Summarize run-level mean quality for each strategy across all repeated runs.
  output$summary_table <- renderTable({
    z <- selected_results()
    validate(need(nrow(z)>0,"Run the simulation to see results."))
    run_avg <- aggregate(simulated_quality~run+strategy,data=z,FUN=mean)
    out <- aggregate(simulated_quality~strategy,data=run_avg,
      FUN=function(v)c(mean=mean(v),sd=sd(v),q025=unname(quantile(v,.025)),q975=unname(quantile(v,.975))))
    vals <- do.call(rbind,out$simulated_quality)
    data.frame(
      Strategy=out$strategy,
      `Mean across runs`=round(vals[,"mean"],3),
      `SD across runs`=round(vals[,"sd"],3),
      `2.5th percentile`=round(vals[,"q025"],3),
      `97.5th percentile`=round(vals[,"q975"],3),
      check.names=FALSE
    )
  },striped=TRUE,bordered=FALSE,spacing="m")
}

# Launch the application by connecting the user interface and server logic.
shinyApp(ui,server)
