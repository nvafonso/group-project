# HEXACO & Teamwork Quality Explorer
# Version: Prototype v2
# Author: Adéla Kovářová (2026)
# Purpose: Review questionnaire data and explore hypothetical team-formation scenarios.
# Run from the project folder with: shiny::runApp("outputs")
#
# Observed-data exploration is separate from hypothetical simulation.
# Simulation scores are illustrative assumptions, not empirical predictions.

# Load the packages used to build the interactive app and its plots.
library(shiny)
library(ggplot2)

simulation_core_candidates <- c(
  "simulation_core.R",
  file.path("src", "simulation_core.R"),
  file.path("outputs", "simulation_core.R"),
  file.path("..", "src", "simulation_core.R")
)
simulation_core_path <- simulation_core_candidates[file.exists(simulation_core_candidates)]
if (!length(simulation_core_path)) {
  stop("Shared simulation_core.R was not found beside app.R, in src/, or in outputs/.", call.=FALSE)
}
source(simulation_core_path[[1]], local=TRUE)

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

# Shiny expects choice labels as names and internal values as vector values.
# Keep the readable labels in the UI while passing the dataset column names.
named_choices <- function(ids) {
  setNames(ids, unname(nice_names[ids]))
}

find_team_id_column <- function(column_names) {
  normalized <- tolower(gsub("[^[:alnum:]]", "", column_names))
  for (candidate in c("teamid", "team", "teamnumber", "teamno", "groupid", "group")) {
    hit <- which(normalized == candidate)
    if (length(hit)) return(column_names[hit[[1]]])
  }
  NULL
}

# Load either the synthetic output produced by data_simulation_fixed.R or the
# observed questionnaire dataset. Simulated data are never silently replaced
# with observed data if the requested simulation file is unavailable.
read_delimited_data <- function(path) {
  content_lines <- tryCatch(readLines(path, warn=FALSE, encoding="UTF-8"),
                            error=function(e) NULL)
  if (is.null(content_lines) || !length(content_lines)) {
    stop("Could not read the selected CSV file.", call.=FALSE)
  }
  content <- paste(content_lines,collapse="\n")
  bom <- intToUtf8(0xFEFF)
  if (startsWith(content,bom)) content <- substring(content,nchar(bom)+1L)
  comma_data <- tryCatch(
    read.csv(text=content, check.names=FALSE, stringsAsFactors=FALSE),
    error=function(e) NULL
  )
  semicolon_data <- tryCatch(
    read.csv2(text=content, check.names=FALSE, stringsAsFactors=FALSE),
    error=function(e) NULL
  )
  candidates <- Filter(function(x) !is.null(x) && nrow(x)>0,
                       list(comma_data,semicolon_data))
  if (!length(candidates)) {
    stop("Could not read the file as a comma- or semicolon-separated CSV with a header row.", call.=FALSE)
  }
  d <- candidates[[which.max(vapply(candidates,ncol,integer(1)))]]
  if (ncol(d)<2) {
    stop("The file was read as one column. Check that it is a comma- or semicolon-separated CSV with column headers.", call.=FALSE)
  }
  d
}

load_data <- function(source = c("observed", "simulated", "uploaded"), upload_path = NULL) {
  source <- match.arg(source)
  if (source == "uploaded") {
    if (is.null(upload_path) || !nzchar(upload_path) || !file.exists(upload_path)) {
      stop("Upload a CSV file to review your observed data.", call. = FALSE)
    }
    return(read_delimited_data(upload_path))
  }
  if (source == "simulated") {
    candidates <- c(
      file.path("simulated_data", "combined_individual_level_results.csv"),
      file.path("outputs", "simulated_data", "combined_individual_level_results.csv"),
      file.path("..", "simulated_data", "combined_individual_level_results.csv")
    )
    existing <- candidates[file.exists(candidates)]
    if (!length(existing)) {
      stop(paste(
        "The simulated dataset was not found. Run data_simulation_fixed.R first.",
        "Expected simulated_data/combined_individual_level_results.csv."
      ), call. = FALSE)
    }
    return(read_delimited_data(existing[[1]]))
  }

  # For observed data, prefer a local data.csv; otherwise try the OSF download.
  if (file.exists("data.csv")) {
    d <- read_delimited_data("data.csv")
  } else {
    d <- tryCatch(read_delimited_data(data_url), error = function(e) NULL)
  }
  if (is.null(d) || !nrow(d)) {
    stop(paste(
      "Could not load the observed dataset. Check your internet connection",
      "or place a copy of the dataset in this folder as data.csv."
    ), call. = FALSE)
  }
  d
}

# Prepare the data by reverse-scoring specified items and computing scale averages.
# The function returns the original data plus the derived HEXACO and TWQ scale columns.
score_scales <- function(d) {
  for (nm in intersect(reverse_hexaco, names(d))) d[[nm]] <- 6 - as.numeric(d[[nm]])
  for (nm in intersect(reverse_twq, names(d))) d[[nm]] <- 6 - as.numeric(d[[nm]])
  for (scale in names(hexaco_items)) {
    items <- hexaco_items[[scale]]
    if (!scale %in% names(d) && all(items %in% names(d))) {
      d[[scale]] <- rowMeans(d[, items, drop = FALSE], na.rm = FALSE)
    }
  }
  for (scale in names(twq_items)) {
    items <- twq_items[[scale]]
    if (!scale %in% names(d) && all(items %in% names(d))) {
      d[[scale]] <- rowMeans(d[, items, drop = FALSE], na.rm = FALSE)
    }
  }
  d
}

# Define a shared ggplot theme so plots use consistent typography and colours.
theme_app <- function() {
  theme_minimal(base_size = 13) +
    theme(
      plot.title = element_text(face = "bold", size = 15, colour = "#63F3FF"),
      plot.subtitle = element_text(colour = "#B1C7E8"),
      axis.title = element_text(face = "bold", colour = "#D9E9FF"),
      axis.text = element_text(colour = "#AFC2E2"),
      axis.line = element_line(colour = "#34415F"),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(colour = "#26324E"),
      plot.background = element_rect(fill = "#0B1020", colour = NA),
      panel.background = element_rect(fill = "#0B1020", colour = NA),
      legend.background = element_rect(fill = "#0B1020", colour = NA),
      legend.text = element_text(colour = "#D9E9FF"),
      legend.title = element_text(colour = "#63F3FF", face = "bold")
    )
}

# USER INTERFACE (UI)
# This section defines the visible layout, controls, tabs, and output placeholders.
ui <- bslib::page_fluid(
  theme=bslib::bs_theme(version=5),
  # CSS controls the app-wide visual design: background, cards, tabs, fonts, buttons, and sliders.
  tags$head(tags$style(HTML("
    @import url('https://fonts.googleapis.com/css2?family=DM+Sans:wght@400;500;600;700&family=Manrope:wght@500;600;700;800&display=swap');
    body { background:#070B16; color:#DDEBFF; font-family:'DM Sans',sans-serif; }
    .container-fluid { max-width:1450px; padding:24px 30px 40px; }
    .hero { background:linear-gradient(115deg,#10152B 0%,#171638 52%,#10283A 100%); color:#F4F7FF; padding:30px 34px; border:1px solid #30365E; border-left:4px solid #FF4FD8; border-radius:18px; margin-bottom:24px; box-shadow:0 0 28px rgba(255,79,216,.12),0 12px 36px rgba(0,0,0,.35); }
    .hero h1 { font-family:'Manrope',sans-serif; font-weight:800; margin:0 0 8px; font-size:30px; }
    .hero p { color:#B8C9EA; margin:0; font-size:15px; }
    .nav-tabs { border-bottom:1px solid #303954; margin-bottom:20px; }
    .nav-tabs .nav-link { border:0!important; border-radius:9px 9px 0 0; color:#9DABCB; font-weight:700; padding:13px 18px; background:transparent!important; }
    .nav-tabs .nav-link:hover { color:#63F3FF; background:#11182B!important; }
    .nav-tabs .nav-link.active,.nav-tabs .nav-link.active:hover { color:#63F3FF; background:#101629!important; border-bottom:3px solid #FF4FD8!important; }
    .well,.panel-card { background:linear-gradient(145deg,#101629,#0C1222); color:#DDEBFF; border:1px solid #283450; border-radius:15px; box-shadow:0 8px 28px rgba(0,0,0,.22); }
    .panel-card { padding:22px; margin-bottom:18px; }
    .panel-card code.file-name { color:#FF4FD8!important; background:#24162E!important; padding:3px 7px; border:1px solid #66345E; border-radius:6px; font-weight:700; }
    .start-card { height:100%; min-height:250px; display:flex; flex-direction:column; }
    .start-card .start-action { margin-top:auto; }
    .start-illustration { height:118px; margin:0 0 16px; display:flex; align-items:center; justify-content:center; overflow:hidden; border:1px solid #263A58; border-radius:12px; background:radial-gradient(ellipse at 50% 100%,rgba(99,243,255,.10),rgba(17,27,49,.35) 65%); }
    .start-illustration svg { width:100%; height:100%; }
    h2,h3,h4 { font-family:'Manrope',sans-serif; color:#63F3FF!important; font-weight:800; }
    .btn-primary { color:#07111B; background:#63F3FF; border-color:#63F3FF; border-radius:9px; font-weight:800; padding:10px 18px; box-shadow:0 0 16px rgba(99,243,255,.2); }
    .btn-primary:hover,.btn-primary:focus { color:#07111B; background:#8DF8FF; border-color:#8DF8FF; }
    .form-control,.selectize-input { color:#EAF3FF!important; background:#0B1120!important; border-radius:8px!important; border:1px solid #35415F!important; box-shadow:none!important; }
    .selectize-input { min-height:48px; padding:11px 38px 11px 13px!important; cursor:pointer!important; }
    .selectize-input:hover { border-color:#63F3FF!important; }
    .selectize-input.focus { border-color:#63F3FF!important; box-shadow:0 0 0 2px rgba(99,243,255,.18)!important; }
    .selectize-control.single .selectize-input:after { border-color:#63F3FF transparent transparent!important; border-width:7px 6px 0!important; }
    .selectize-control.single .selectize-input.dropdown-active:after { border-color:transparent transparent #FF4FD8!important; border-width:0 6px 7px!important; }
    .selectize-dropdown,.selectize-dropdown .option { color:#EAF3FF; background:#10182B; }
    .selectize-dropdown .active { color:#07111B; background:#63F3FF; }
    label { color:#C6D5F0; }
    .help-block,.form-text,.text-muted,.section-intro { color:#A0B0CF!important; opacity:1!important; }
    .section-intro { margin-top:-4px; margin-bottom:18px; }
    .panel-card pre,.panel-card pre.shiny-text-output { color:#DDEBFF!important; background:#0B1120!important; border-color:#35415F!important; }
    .metric { background:linear-gradient(135deg,#111B31,#11142A); border:1px solid #2C4260; border-radius:12px; padding:14px 16px; margin-bottom:10px; }
    .metric-label { text-transform:uppercase; font-size:11px; letter-spacing:.09em; color:#9EAFD1; font-weight:800; }
    .metric-value { font-size:25px; color:#63F3FF; font-family:'Manrope',sans-serif; font-weight:800; text-shadow:0 0 14px rgba(99,243,255,.2); }
    .notice { background:#21162B; border-left:4px solid #FF4FD8; border-radius:8px; padding:12px 14px; color:#F2C9EC; }
    .download-actions { display:flex; flex-wrap:wrap; gap:10px; margin-top:14px; }
    .download-actions .btn { color:#DDEBFF; background:#151D32; border:1px solid #35415F; border-radius:8px; font-weight:700; }
    .download-actions .btn:hover { color:#63F3FF; border-color:#63F3FF; }
    .accordion-item { color:#DDEBFF; background:#101629; border:1px solid #3B4D70; border-radius:10px!important; overflow:hidden; margin-bottom:9px; }
    .accordion-button { position:relative; color:#63F3FF; background:linear-gradient(100deg,#16223A,#111B31); font-weight:800; box-shadow:none!important; cursor:pointer; padding:16px 48px 16px 18px; transition:background .15s ease,border-color .15s ease; }
    .accordion-button:not(.collapsed) { color:#FF8BE5; background:linear-gradient(100deg,#211B38,#17172E); }
    .accordion-button:hover { color:#FFFFFF; background:linear-gradient(100deg,#1E3150,#1A2340); }
    .accordion-button:focus-visible { outline:3px solid #63F3FF; outline-offset:-3px; z-index:2; }
    .accordion-button::after { filter:brightness(0) saturate(100%) invert(76%) sepia(49%) saturate(1200%) hue-rotate(141deg) brightness(103%) contrast(103%); }
    .accordion-body { color:#DDEBFF; background:#0C1222; }
    .btn-secondary { color:#DDEBFF; background:#25213A; border-color:#55406C; border-radius:8px; font-weight:700; }
    .btn-secondary:hover { color:#FFFFFF; background:#393054; border-color:#FF4FD8; }
    table,.table { --bs-table-bg:#101629; --bs-table-color:#E3EDFF; --bs-table-striped-bg:#182239; --bs-table-striped-color:#E3EDFF; --bs-table-hover-bg:#202D48; --bs-table-hover-color:#FFFFFF; color:#E3EDFF!important; background-color:#101629!important; }
    .table>:not(caption)>*>* { color:#E3EDFF!important; background-color:#101629!important; background-image:none!important; border-bottom-color:#35415F!important; box-shadow:none!important; }
    .table>thead>tr>th { color:#63F3FF!important; background-color:#151D32!important; border-bottom:1px solid #35415F!important; }
    .table-striped>tbody>tr:nth-of-type(odd)>td { background-color:#182239!important; color:#E3EDFF!important; }
    .table-striped>tbody>tr:nth-of-type(even)>td { background-color:#0E1527!important; color:#E3EDFF!important; }
    .table-striped>tbody>tr:hover>td { background-color:#202D48!important; color:#FFFFFF!important; }
    .irs--shiny .irs-bar,.irs--shiny .irs-single,.irs--shiny .irs-from,.irs--shiny .irs-to { background:#63F3FF; border-top-color:#63F3FF; border-bottom-color:#63F3FF; color:#07111B; }
    .irs--shiny .irs-line { background:#293550; border-color:#293550; }
    .irs--shiny .irs-handle { border-color:#FF4FD8; background:#FF4FD8; }
    .irs--shiny .irs-grid-text { color:#AFC2E2!important; }
    .irs--shiny .irs-min,.irs--shiny .irs-max { color:#DDEBFF!important; background:#151D32!important; }
  "))),
  # Header banner shown at the top of the app.
  div(class="hero",
      h1("Personality & Teamwork Quality Explorer"),
      p("Explore questionnaire data and test transparent, hypothetical team-formation scenarios.")
  ),
  # Separate reviewing dataset analysis from trying simulation assumptions.
  tabsetPanel(id="main_tabs", selected="Start",
    tabPanel("Start",
      br(),
      div(class="panel-card",
        h2("Welcome to the Personality & Teamwork Quality Explorer"),
        p(class="section-intro","Use this app to review questionnaire data or explore how different team-forming strategies behave under clearly stated assumptions.")
      ),
      fluidRow(
        column(6,
          div(class="panel-card start-card",
            div(class="start-illustration",
              tags$svg(viewBox="0 0 420 120", role="img", `aria-label`="Illustration of questionnaire data points and a trend line",
                tags$line(x1="48",y1="18",x2="48",y2="98",stroke="#667A9D",`stroke-width`="2"),
                tags$line(x1="48",y1="98",x2="376",y2="98",stroke="#667A9D",`stroke-width`="2"),
                tags$line(x1="48",y1="72",x2="376",y2="72",stroke="#293A59",`stroke-width`="1"),
                tags$line(x1="48",y1="46",x2="376",y2="46",stroke="#293A59",`stroke-width`="1"),
                tags$path(d="M66 88 C112 79, 130 72, 168 68 S232 52, 270 48 S330 34, 358 26",fill="none",stroke="#FF4FD8",`stroke-width`="3",`stroke-linecap`="round"),
                tags$circle(cx="92",cy="80",r="5",fill="#63F3FF"),
                tags$circle(cx="125",cy="69",r="5",fill="#63F3FF"),
                tags$circle(cx="153",cy="78",r="5",fill="#63F3FF"),
                tags$circle(cx="190",cy="56",r="5",fill="#63F3FF"),
                tags$circle(cx="222",cy="62",r="5",fill="#63F3FF"),
                tags$circle(cx="256",cy="43",r="5",fill="#63F3FF"),
                tags$circle(cx="294",cy="49",r="5",fill="#63F3FF"),
                tags$circle(cx="332",cy="31",r="5",fill="#63F3FF")
              )
            ),
            h3("Review analysis results"),
            p("Explore HEXACO personality traits and teamwork-quality scores in observed data, an uploaded questionnaire file, or the generated simulation dataset."),
            p(class="text-muted","This view summarizes the data and shows exploratory relationships. It does not establish cause and effect."),
            actionButton("go_analysis","Open analysis results",class="btn-primary start-action",width="100%")
          )
        ),
        column(6,
          div(class="panel-card start-card",
            div(class="start-illustration",
              tags$svg(viewBox="0 0 420 120", role="img", `aria-label`="Illustration of people grouped into three teams",
                tags$line(x1="66",y1="60",x2="110",y2="36",stroke="#63F3FF",`stroke-width`="2",opacity=".45"),
                tags$line(x1="66",y1="60",x2="112",y2="83",stroke="#63F3FF",`stroke-width`="2",opacity=".45"),
                tags$line(x1="110",y1="36",x2="112",y2="83",stroke="#63F3FF",`stroke-width`="2",opacity=".45"),
                tags$circle(cx="66",cy="60",r="11",fill="#63F3FF"), tags$circle(cx="110",cy="36",r="9",fill="#63F3FF"), tags$circle(cx="112",cy="83",r="9",fill="#63F3FF"),
                tags$line(x1="190",y1="60",x2="234",y2="36",stroke="#FF4FD8",`stroke-width`="2",opacity=".5"),
                tags$line(x1="190",y1="60",x2="236",y2="83",stroke="#FF4FD8",`stroke-width`="2",opacity=".5"),
                tags$line(x1="234",y1="36",x2="236",y2="83",stroke="#FF4FD8",`stroke-width`="2",opacity=".5"),
                tags$circle(cx="190",cy="60",r="11",fill="#FF4FD8"), tags$circle(cx="234",cy="36",r="9",fill="#FF4FD8"), tags$circle(cx="236",cy="83",r="9",fill="#FF4FD8"),
                tags$line(x1="310",y1="60",x2="354",y2="36",stroke="#B7FF5A",`stroke-width`="2",opacity=".5"),
                tags$line(x1="310",y1="60",x2="356",y2="83",stroke="#B7FF5A",`stroke-width`="2",opacity=".5"),
                tags$line(x1="354",y1="36",x2="356",y2="83",stroke="#B7FF5A",`stroke-width`="2",opacity=".5"),
                tags$circle(cx="310",cy="60",r="11",fill="#B7FF5A"), tags$circle(cx="354",cy="36",r="9",fill="#B7FF5A"), tags$circle(cx="356",cy="83",r="9",fill="#B7FF5A")
              )
            ),
            h3("Explore the simulation"),
            p("Try Random, Balanced, and Similar team assignments. Adjust hypothetical personality and diversity effects, then compare predicted teamwork scores."),
            p(class="text-muted","The simulation uses generated participant data and assumptions you choose; its results are not observed findings."),
            actionButton("go_simulation","Open simulation",class="btn-primary start-action",width="100%")
          )
        )
      ),
      div(class="notice",strong("Not sure where to begin? "),
          "Choose Analysis results to inspect a dataset. Choose the Simulation to try what-if scenarios.")
    ),
    # TAB 1: ANALYSIS RESULTS
    # Review trait/outcome relationships in either observed data or generated simulation output.
    tabPanel("Analysis results",
      br(),
      sidebarLayout(
        sidebarPanel(
          h4("Choose results to review"),
          selectInput(
            "data_source", "Dataset",
            choices = c("Observed questionnaire data" = "observed",
                        "Upload observed data (CSV)" = "uploaded",
                        "Simulated data output" = "simulated"),
            selected = "observed"
          ),
          helpText("Click the dataset field or cyan arrow to choose an option."),
          conditionalPanel(
            condition = "input.data_source == 'uploaded'",
            fileInput("observed_file", "Choose a CSV file", accept = c(".csv", ".txt")),
            helpText("Upload a CSV with a header row. Comma- and semicolon-separated files are supported; use HEXACO/TWQ item codes or scale columns.")
          ),
          conditionalPanel(
            condition = "input.data_source == 'simulated'",
            selectInput(
              "source_scenario", "Simulated scenario",
              choices = c("Low diversity", "High diversity"),
              selected = "Low diversity"
            ),
            helpText("These are hypothetical generated records, not observed respondents.")
          ),
          textOutput("data_source_note"),
          tags$hr(),
          h4("Choose your variables"),
          p(class="section-intro","Review the relationship between a personality trait and a teamwork-quality dimension. These controls select the analysis view; they do not change the simulation assumptions."),
          selectInput("trait","HEXACO trait",choices=named_choices(names(hexaco_items)),selected="agreeableness"),
          selectInput("outcome","Teamwork-quality dimension",choices=named_choices(names(twq_items)),selected="communication"),
          checkboxInput("show_lm","Show trend line and confidence band",TRUE),
          helpText("Each point represents one participant.")
        ),
        mainPanel(
          div(class="panel-card",
            h3("Teamwork score summary"),
            p(class="section-intro","Descriptive results for the selected dataset and scenario."),
            tableOutput("analysis_summary")
          ),
          div(class="panel-card",h3("Analysis results"),p(class="section-intro","Participant-level scores from the selected source."),plotOutput("scatter",height=390)),
          div(class="panel-card",h4("Exploratory regression"),verbatimTextOutput("model_summary"),
              p(class="text-muted","The model reports a standardized beta and 95% confidence interval. When a team ID is available, standard errors are clustered by team. The plot's trend band is descriptive; association does not imply causation."))
        )
      )
    ),
    # TAB 2: PLAY WITH SIMULATED DATA
    # Users set team sizes, choose grouping strategies, and adjust hypothetical model effects.
    tabPanel("Play with simulated data",
      br(),
      p(class="section-intro","Change the assumptions and rerun the comparison using generated participant records. Choose the low- or high-diversity source scenario below; these records are hypothetical."),
      sidebarLayout(
        sidebarPanel(
          # Simulation controls: number of teams, team size, trait, outcome, effects, noise, and repetitions.
          h4("1 · Build the scenario"),
          selectInput(
            "sim_scenario", "Generated participant pool",
            choices = c("Low diversity", "High diversity"),
            selected = "Low diversity"
          ),
          sliderInput("n_teams","Number of teams",min=2,max=30,value=8,step=1,sep=""),
          sliderInput("team_size","People per team",min=2,max=10,value=4,step=1,sep=""),
          div(class="metric",div(class="metric-label","Total simulated participants"),div(class="metric-value",textOutput("total_people",inline=TRUE))),
          tags$hr(),
          radioButtons("research_question","What would you like to investigate?",
             choices=c("Composition effect"="composition","Grouping-strategy effect"="strategy","Both effects"="both"),
             selected="both"),
          selectInput("sim_trait","Personality trait",choices=named_choices(names(hexaco_items)),selected="agreeableness"),
          helpText("This trait determines how Balanced and Similar teams are formed. The effect sliders below separately set how each HEXACO trait is assumed to relate to the teamwork score."),
          selectInput("sim_outcome","Teamwork-quality dimension",choices=named_choices(names(twq_items)),selected="communication"),
          bslib::accordion(
            bslib::accordion_panel(
              "Composition effects",
              p(class="text-muted","Each slider is an assumed effect size, not a personality score. It sets how many teamwork-score points (on the 1–5 scale) the model adds or subtracts for a one-standard-deviation increase in that trait's team average, compared with random grouping. For example, +0.45 means the model adds 0.45 points. Zero means no assumed effect. These values are not estimated from your data."),
              lapply(names(hexaco_items), function(tr) {
                sliderInput(inputId=paste0("comp_",tr),
                            label=paste("Assumed effect of team-average", unname(nice_names[tr])),
                            min=-1,max=1,value=0,step=0.05)
              }),
              value="composition_effects"
            ),
            bslib::accordion_panel(
              "Diversity effects",
              p(class="text-muted","Each slider sets the assumed change in teamwork-score points (on the 1–5 scale) for a one-standard-deviation increase in that trait's within-team diversity. Positive values mean more diversity raises the predicted score; negative values mean it lowers the score. Zero means no assumed effect. These values are not estimated from your data."),
              lapply(names(hexaco_items), function(tr) {
                sliderInput(inputId=paste0("div_",tr),
                            label=paste("Assumed effect of within-team", unname(nice_names[tr]), "diversity"),
                            min=-1,max=1,value=0,step=0.05)
              }),
              value="diversity_effects"
            ),
            multiple=TRUE
          ),
          actionButton("reset_effects","Reset effects to zero",class="btn-secondary",width="100%"),
          sliderInput("noise_sd","Outcome noise (SD score points)",min=0,max=1.5,value=0.4,step=0.05),
          sliderInput("repetitions","Repeated simulation runs",min=100,max=1000,value=300,step=100,sep=""),
          numericInput("seed","Random seed",value=2026,min=0,max=2147483647,step=1),
          helpText("The same seed and settings reproduce the same comparison."),
          actionButton("run_sim","Run comparison",class="btn-primary",icon=icon("play"),width="100%")
        ),
        mainPanel(
          div(class="panel-card",
            # Results panel: displays the simulation comparison plot and summary table.
          h3("2 · Compare outcomes"),
            p(class="section-intro","Compare both the average predicted score and the spread of scores between teams. Strategies reuse the same people and the same run-level noise, so a linear composition effect may change between-team spread more than the overall mean. Error bars show paired 95% intervals versus Random."),
            fluidRow(
              column(4,div(class="metric",div(class="metric-label","Teams per run"),div(class="metric-value",textOutput("metric_teams",inline=TRUE)))),
              column(4,div(class="metric",div(class="metric-label","People per team"),div(class="metric-value",textOutput("metric_size",inline=TRUE)))),
              column(4,div(class="metric",div(class="metric-label","Simulation runs"),div(class="metric-value",textOutput("metric_reps",inline=TRUE))))
            ),
            plotOutput("strategy_plot",height=450),
            h4("Summary by strategy"),
            tableOutput("summary_table"),
            div(class="download-actions",
                downloadButton("download_summary","Download summary CSV"),
                downloadButton("download_strategy_plot","Download comparison plot"),
                downloadButton("download_team_profile","Download team profile"),
                downloadButton("download_composition_plot","Download composition plot"))
          ),
          div(class="panel-card",
            h3("Team profile preview"),
            p(class="section-intro","Inspect one run. Each dot is a team; the upper panel shows its average selected trait score and the lower panel shows within-team variation. The dashed line marks the participant-pool average."),
            sliderInput("preview_run","Simulation run to inspect",min=1,max=300,value=1,step=1),
            plotOutput("team_profile_plot",height=430)
          ),
          # Additional plot showing team trait composition versus simulated teamwork quality.
          conditionalPanel(condition="input.research_question == 'both'",
          div(class="panel-card",h3("3 · Understand the assumptions"),
            plotOutput("composition_plot",height=300),
            p(class="text-muted","The model resamples complete participant records from the selected generated scenario to preserve correlations among HEXACO traits. It uses that scenario's mean teamwork score as a baseline; all 12 composition/diversity coefficients are user-specified assumptions, not estimated causal effects. Run the standalone data simulation first so its CSV output is available, then click Run comparison here.")
          )),
          div(class="notice",strong("Interpretation: "),
              "A strategy can change how much team scores vary even when the overall average stays similar. Outcomes are kept on the 1–5 scale by clipping predictions at the limits; the summary reports the share clipped, because boundary clipping can affect mean differences. Results describe the model you specified, not proven effects in real teams.")
        )
      )
    ),
    # TAB 4: ABOUT
    # Explains the app's workflows, data, and interpretation limits.
    tabPanel("About",
      br(),
      div(class="panel-card",
        h2("About the app"),
        p(class="section-intro","This app brings questionnaire-data exploration and team-formation simulations into one place. Use the Start tab to choose a workflow; the two workflows answer different questions.")
      ),
      div(class="panel-card",
        h3("What is HEXACO?"),
        p("HEXACO is a personality framework with six broad dimensions. Questionnaire items are combined into scores for these dimensions. They describe patterns in how people tend to think, feel, and behave; they are not measures of teamwork ability, diagnoses, or labels of a person's worth."),
        fluidRow(
          column(4,div(class="metric",div(class="metric-label","H · Honesty–Humility"),p("Sincerity, fairness, modesty, and less interest in exploiting others for personal gain."))),
          column(4,div(class="metric",div(class="metric-label","E · Emotionality"),p("Emotional sensitivity, including fear and anxiety, seeking support, and emotional attachment to others."))),
          column(4,div(class="metric",div(class="metric-label","X · Extraversion"),p("Social confidence, enjoyment of social interaction, and positive energy.")))
        ),
        fluidRow(
          column(4,div(class="metric",div(class="metric-label","A · Agreeableness"),p("Patience, forgiveness, flexibility, and a tendency to manage anger."))),
          column(4,div(class="metric",div(class="metric-label","C · Conscientiousness"),p("Organization, diligence, attention to detail, and careful decision-making."))),
          column(4,div(class="metric",div(class="metric-label","O · Openness to Experience"),p("Curiosity, imagination, appreciation of art and nature, and interest in new or unusual ideas.")))
        ),
        p(class="text-muted","These are brief summaries. The official HEXACO site provides the full scale descriptions: ",
          tags$a(href="https://hexaco.org/scaledescriptions&lang=en",target="_blank",rel="noopener noreferrer","HEXACO scale descriptions"),". The app's built-in item mapping is based on the 60-item version; check the questionnaire and scoring key that match your data.")
      ),
      div(class="panel-card",
        h3("Raw data and source"),
        p(tags$code(class="file-name","HEXACO_TWQ_raw-data.csv")," is an unchanged copy of the file uploaded for this group project. It contains 54 rows and 105 columns, is semicolon-separated, and includes participant and team identifiers."),
        p(tags$strong("Source: "),"Zähl, P. (2023). ",tags$em("The Influence of HEXACO Personality Traits on the Teamwork Quality in Student Software Teams — Supplementary Material"),". OSF."),
        tags$ul(
          tags$li(tags$a(href="https://osf.io/jb94w/",target="_blank",rel="noopener noreferrer","OSF project page")),
          tags$li(tags$a(href="https://osf.io/download/k4syv/",target="_blank",rel="noopener noreferrer","Original OSF download")),
          tags$li("Catalogue record and attribution: ",tags$a(href="https://itemresponsewarehouse.org/tables/zaehl2023_hexaco/",target="_blank",rel="noopener noreferrer","Item Response Warehouse — zaehl2023_hexaco")),
          tags$li("Licence: ",tags$a(href="https://creativecommons.org/licenses/by/4.0/",target="_blank",rel="noopener noreferrer","Creative Commons Attribution 4.0 International (CC BY 4.0)"))
        ),
        p(class="text-muted","The app may load a local file named data.csv or, if it is not present, try the original OSF download. The observed-data controls are separate from the generated participant pool used in the simulation tab.")
      ),
      fluidRow(
        column(6,
          div(class="panel-card",
            h3("Analysis results: explore data"),
            p("Use this tab to review observed questionnaire data, upload a CSV, or inspect the standalone generated simulation dataset."),
            p("The observed-data option uses a local data.csv when available and otherwise tries the configured online dataset. The simulated-data option reads the CSV created by the standalone simulation script."),
            tags$ul(
              tags$li("CSV uploads may contain recognized HEXACO and teamwork-quality questionnaire items or already calculated scale columns. Comma- and semicolon-separated files are supported."),
              tags$li("The app shows descriptive summaries and a plot of one selected HEXACO trait against one teamwork-quality score."),
              tags$li("The exploratory regression reports a standardized association and a 95% confidence interval. If a team ID is detected, uncertainty is adjusted for clustering by team.")
            ),
            p(class="text-muted","These analyses describe associations in the selected data. They do not show that personality causes teamwork outcomes.")
          )
        ),
        column(6,
          div(class="panel-card",
            h3("Simulation: explore what-if scenarios"),
            p("Use this tab to compare three ways of forming teams from the generated participant pool:"),
            tags$ul(
              tags$li(tags$strong("Random:")," assign people randomly."),
              tags$li(tags$strong("Balanced:")," spread people across teams based on the selected trait."),
              tags$li(tags$strong("Similar:")," place people with similar scores on that trait together.")
            ),
            p("The effect sliders are assumptions about how team-average traits and within-team diversity relate to the predicted teamwork score. A coefficient is the number of score points added or subtracted per one reference standard-deviation increase; zero means no assumed effect. These values are not estimated from the uploaded or observed data. The simulation uses its generated participant pool, separate from the dataset selected in Analysis results. The seed makes a run reproducible when the settings stay the same."),
            p("The standalone generator and this interactive comparison now use shared code for calculating team trait averages, diversity effects, and latent score components. The standalone generator additionally produces multiple correlated teamwork outcomes and ordinal questionnaire-item responses; the interactive tab focuses on one selected outcome and paired comparisons across grouping strategies."),
            p(class="text-muted","Predictions are kept on the 1–5 scale, so boundary clipping can affect results. The summary reports how often predictions would otherwise fall outside those limits.")
          )
        )
      ),
      div(class="panel-card",
        h3("How to interpret the outputs"),
        tags$ul(
          tags$li("The analysis tab reports patterns in a dataset; the simulation tab reports outcomes generated under chosen assumptions."),
          tags$li("Simulation comparisons show changes relative to Random assignment as well as average team scores and between-team variation."),
          tags$li("A near-zero difference means the selected assumptions produced little difference between strategies. It does not mean the teamwork score itself is zero."),
          tags$li("Simulation results can change with the participant pool, team size, random seed, and effect settings. Treat them as exploratory illustrations, not evidence about real teams.")
        )
      ),
      div(class="notice",
        strong("Scoring note: "),
        "Before using results in a report, verify the reverse-keyed items, scale construction, and data source against the original questionnaire and study documentation."
      ),
      div(class="panel-card",
        h4("Author"),
        p("Adéla Kovářová · 2026")
      )
    )
  )
)

# SERVER LOGIC
# This section reacts to user inputs, processes data, runs simulations, and creates outputs.
server <- function(input,output,session) {
  observeEvent(input$go_analysis, {
    updateTabsetPanel(session,"main_tabs",selected="Analysis results")
  })
  observeEvent(input$go_simulation, {
    updateTabsetPanel(session,"main_tabs",selected="Play with simulated data")
  })

  observeEvent(input$reset_effects, {
    for (trait in names(hexaco_items)) {
      updateSliderInput(session,paste0("comp_",trait),value=0)
      updateSliderInput(session,paste0("div_",trait),value=0)
    }
  })

  observed_data_cache <- reactiveVal(NULL)
  load_cached_observed <- function() {
    cached <- isolate(observed_data_cache())
    if (!is.null(cached)) {
      if (inherits(cached, "error")) stop(conditionMessage(cached), call.=FALSE)
      return(cached)
    }
    loaded <- tryCatch(load_data("observed"), error=function(e) e)
    observed_data_cache(loaded)
    if (inherits(loaded, "error")) stop(conditionMessage(loaded), call.=FALSE)
    loaded
  }

  # Use observed records or generated simulation output. Filter to one
  # simulated condition so paired participants are not duplicated in the pool.
  raw_data <- reactive({
    upload_path <- if (!is.null(input$observed_file)) input$observed_file$datapath else NULL
    loaded <- tryCatch({
      if (identical(input$data_source,"observed")) load_cached_observed()
      else load_data(input$data_source, upload_path)
    }, error=function(e) e)
    if (inherits(loaded, "error")) {
      validate(need(FALSE, conditionMessage(loaded)))
    }
    if (identical(input$data_source, "simulated") && "Scenario" %in% names(loaded)) {
      loaded <- loaded[as.character(loaded$Scenario) == input$source_scenario, , drop=FALSE]
      validate(need(nrow(loaded) > 0, "No rows were found for the selected simulated scenario."))
    }
    score_scales(loaded)
  })
  output$data_source_note <- renderText({
    if (identical(input$data_source, "uploaded")) {
      "Source: uploaded observed questionnaire data."
    } else if (identical(input$data_source, "simulated")) {
      paste("Source: hypothetical simulation output —", input$source_scenario,
            ". The app will reassign these participant profiles into new teams.")
    } else {
      "Source: observed questionnaire data."
    }
  })
  output$analysis_summary <- renderTable({
    d <- raw_data()
    available <- intersect(names(twq_items), names(d))
    validate(need(length(available) > 0, "No teamwork-quality scores are available in this dataset."))
    do.call(rbind, lapply(available, function(score_name) {
      values <- d[[score_name]]
      values <- values[is.finite(values)]
      data.frame(
        Outcome = unname(nice_names[score_name]),
        `Valid participants` = length(values),
        Mean = if (length(values)) round(mean(values), 3) else NA_real_,
        SD = if (length(values) > 1) round(stats::sd(values), 3) else NA_real_,
        Minimum = if (length(values)) round(min(values), 3) else NA_real_,
        Maximum = if (length(values)) round(max(values), 3) else NA_real_,
        check.names = FALSE
      )
    }))
  }, striped=TRUE, bordered=FALSE, spacing="m")
  output$total_people <- renderText({
    req(simulations())
    settings <- simulations()$settings
    settings$n_teams * settings$team_size
  })
  output$metric_teams <- renderText({ req(simulations()); simulations()$settings$n_teams })
  output$metric_size <- renderText({ req(simulations()); simulations()$settings$team_size })
  output$metric_reps <- renderText({ req(simulations()); simulations()$settings$reps })
  observeEvent(input$repetitions, {
    updateSliderInput(session, "preview_run", max=input$repetitions,
                      value=min(input$preview_run, input$repetitions))
  }, ignoreInit=TRUE)

  # OBSERVED-DATA SCATTERPLOT
  # Draw one point per participant and optionally add a fitted linear trend line.
  output$scatter <- renderPlot({
    d <- raw_data()
    validate(need(input$trait %in% names(d) && input$outcome %in% names(d),"Selected scores are unavailable. Check the dataset column names."))
    p <- ggplot(d,aes(x=.data[[input$trait]],y=.data[[input$outcome]]))+
      geom_point(alpha=.82,size=2.4,colour="#63F3FF")+
      labs(x=unname(nice_names[input$trait]),y=unname(nice_names[input$outcome]),
           title=paste(unname(nice_names[input$trait]),"and",unname(nice_names[input$outcome])))+theme_app()
    if (isTRUE(input$show_lm)) p <- p+geom_smooth(method="lm",formula=y~x,se=TRUE,colour="#FF4FD8",fill="#692E68",linewidth=1)
    p
  })

  # OBSERVED-DATA REGRESSION
  # Report a standardized beta and confidence interval, using team-clustered
  # standard errors when a usable team identifier is present.
  output$model_summary <- renderPrint({
    d <- raw_data()
    validate(need(input$trait %in% names(d) && input$outcome %in% names(d),"Scores unavailable."))
    team_col <- find_team_id_column(names(d))
    x <- suppressWarnings(as.numeric(as.character(d[[input$trait]])))
    y <- suppressWarnings(as.numeric(as.character(d[[input$outcome]])))
    valid <- is.finite(x) & is.finite(y)
    if (!is.null(team_col)) {
      team_values <- as.character(d[[team_col]])
      valid <- valid & !is.na(team_values) & nzchar(trimws(team_values))
    }
    validate(need(sum(valid)>=3,"At least three complete participants are needed for regression."))
    x <- x[valid]
    y <- y[valid]
    validate(need(stats::sd(x)>0 && stats::sd(y)>0,"The selected scores need variation to estimate a relationship."))
    z_trait <- as.numeric(scale(x))
    z_outcome <- as.numeric(scale(y))
    fit <- lm(z_outcome~z_trait)
    beta <- unname(coef(fit)[["z_trait"]])
    team_count <- NA_integer_

    if (!is.null(team_col)) {
      teams <- droplevels(factor(as.character(d[[team_col]][valid])))
      team_count <- nlevels(teams)
    }

    if (!is.null(team_col) && team_count>=2) {
      model_matrix <- model.matrix(fit)
      residuals <- residuals(fit)
      cluster_scores <- lapply(split(seq_along(residuals),teams),function(indices) {
        colSums(sweep(model_matrix[indices,,drop=FALSE],1,residuals[indices],"*"))
      })
      meat <- Reduce(`+`,lapply(cluster_scores,tcrossprod))
      bread <- solve(crossprod(model_matrix))
      correction <- (team_count/(team_count-1))*((length(y)-1)/(length(y)-ncol(model_matrix)))
      clustered_vcov <- correction*bread%*%meat%*%bread
      beta_se <- sqrt(max(0,clustered_vcov["z_trait","z_trait"]))
      ci_critical <- stats::qt(.975,df=team_count-1)
      method_label <- paste("Linear regression with team-clustered standard errors (team ID:",team_col,")")
    } else {
      coefficient_table <- coef(summary(fit))
      beta_se <- coefficient_table["z_trait","Std. Error"]
      ci_critical <- stats::qt(.975,df=stats::df.residual(fit))
      method_label <- if (!is.null(team_col)) {
        paste("Standardized linear regression; only one team found in",team_col)
      } else {
        "Standardized linear regression; no team ID column found"
      }
    }
    ci <- beta+c(-1,1)*ci_critical*beta_se
    cat("Model:",method_label,"\n")
    cat("Complete participants (n):",length(y),"\n")
    if (!is.null(team_col)) cat("Teams:",team_count,"\n")
    cat(sprintf("Standardized beta: %.3f\n95%% CI: [%.3f, %.3f]\n",beta,ci[[1]],ci[[2]]))
    cat("Beta is the expected SD change in teamwork score per 1 SD increase in the selected HEXACO trait.\n")
    if (!is.null(team_col) && team_count<20) {
      cat("Caution: fewer than 20 team clusters; the cluster-robust interval may be unstable.\n")
    }
  })

  # SIMULATION ENGINE
  # Always use the standalone simulation output here, independent of the dataset
  # selected in Analysis results. Sample complete rows to preserve HEXACO correlations.
  simulations <- eventReactive(input$run_sim, {
    d <- tryCatch(load_data("simulated"), error=function(e) e)
    if (inherits(d, "error")) {
      validate(need(FALSE, conditionMessage(d)))
    }
    validate(need("Scenario" %in% names(d), "The generated file has no Scenario column. Rerun data_simulation_fixed.R."))
    d <- d[as.character(d$Scenario) == input$sim_scenario, , drop=FALSE]
    validate(need(nrow(d) > 0, paste("No generated rows found for", input$sim_scenario, "scenario.")))
    d <- score_scales(d)
    trait_names <- names(hexaco_items)
    validate(
      need(all(trait_names %in% names(d)), "Some HEXACO scale columns are missing. Check the data and scale scoring."),
      need(input$sim_outcome %in% names(d), paste(
        "Selected teamwork-quality score", input$sim_outcome,
        "is unavailable in the generated file. Available outcomes:",
        paste(intersect(c(names(twq_items), "twq_overall"), names(d)), collapse=", ")
      ))
    )

    needed <- c(trait_names, input$sim_outcome)
    d_sim <- d[complete.cases(d[, needed, drop=FALSE]), needed, drop=FALSE]
    validate(need(nrow(d_sim) >= 4, "Not enough complete observations for simulation."))

    n_teams <- input$n_teams
    team_size <- input$team_size
    reps <- input$repetitions
    seed <- as.integer(input$seed)
    trait_name <- input$sim_trait
    outcome_name <- input$sim_outcome
    baseline <- mean(d_sim[[outcome_name]], na.rm=TRUE)

    comp_effects <- setNames(
      vapply(trait_names, function(tr) input[[paste0("comp_", tr)]], numeric(1)),
      trait_names
    )
    div_effects <- setNames(
      vapply(trait_names, function(tr) input[[paste0("div_", tr)]], numeric(1)),
      trait_names
    )

    strategy_names <- c("Random", "Balanced", "Similar")
    total_rows <- reps * length(strategy_names) * n_teams
    all_results <- data.frame(
      run=integer(total_rows), strategy=character(total_rows), team=integer(total_rows),
      mean_trait=numeric(total_rows), within_team_sd=numeric(total_rows),
      population_trait_mean=numeric(total_rows), baseline_quality=numeric(total_rows),
      composition_component=numeric(total_rows), diversity_component=numeric(total_rows),
      simulated_quality=numeric(total_rows), clipped_at_boundary=logical(total_rows),
      stringsAsFactors=FALSE
    )
    idx <- 1L

    # Reset the random stream for each explicit run so the same controls reproduce results.
    set.seed(seed)
    withProgress(message="Running paired simulation", value=0, {
    progress_step <- max(1L, floor(reps / 100L))

    for (r in seq_len(reps)) {
      # Resample whole rows to retain the observed correlations among HEXACO traits.
      sampled <- sample(seq_len(nrow(d_sim)), size=n_teams * team_size, replace=TRUE)
      people <- d_sim[sampled, trait_names, drop=FALSE]
      person_group_trait <- people[[trait_name]]

      # Random assignment.
      random_team <- sample(rep(seq_len(n_teams), each=team_size))

      # Similar-score assignment: adjacent sorted scores go into the same team.
      ord <- order(person_group_trait)
      sim_team <- integer(length(ord))
      sim_team[ord] <- rep(seq_len(n_teams), each=team_size)

      # Balanced assignment: alternate the sorted scores across teams in a snake pattern.
      bal_team <- integer(length(ord))
      for (k in seq_along(ord)) {
        row <- (k - 1L) %/% n_teams
        col <- (k - 1L) %% n_teams
        if (row %% 2L == 1L) col <- n_teams - 1L - col
        bal_team[ord[k]] <- col + 1L
      }

      strategies <- list(Random=random_team, Balanced=bal_team, Similar=sim_team)

      population_trait_means <- vapply(trait_names, function(tr) mean(d_sim[[tr]], na.rm=TRUE), numeric(1))
      population_trait_sds <- vapply(trait_names, function(tr) stats::sd(d_sim[[tr]], na.rm=TRUE), numeric(1))
      unit_reference <- setNames(rep(1, length(trait_names)), trait_names)
      random_components <- hexaco_team_trait_components(
        people=people, team_ids=random_team, traits=trait_names,
        trait_reference_means=population_trait_means,
        trait_reference_sds=unit_reference,
        diversity_reference_means=population_trait_means,
        diversity_reference_sds=unit_reference
      )
      composition_reference_means <- colMeans(random_components$team_means)
      composition_reference_sds <- apply(random_components$team_means, 2, stats::sd)
      invalid_composition_reference <- !is.finite(composition_reference_sds) | composition_reference_sds <= 0
      composition_reference_sds[invalid_composition_reference] <-
        population_trait_sds[invalid_composition_reference] / sqrt(team_size)
      composition_reference_sds[!is.finite(composition_reference_sds) |
                                  composition_reference_sds <= 0] <- 1

      # Compute team SDs for all strategies first, then use the Random teams
      # as a fixed within-run reference for every strategy's diversity effect.
      random_team_sds <- random_components$team_sds
      diversity_reference_means <- colMeans(random_team_sds, na.rm=TRUE)
      diversity_reference_sds <- apply(random_team_sds, 2, stats::sd, na.rm=TRUE)
      diversity_reference_sds[!is.finite(diversity_reference_sds) |
                                diversity_reference_sds <= 0] <- 1
      team_components_by_strategy <- lapply(strategies, function(team_id) {
        hexaco_team_trait_components(
          people=people, team_ids=team_id, traits=trait_names,
          trait_reference_means=composition_reference_means,
          trait_reference_sds=composition_reference_sds,
          diversity_reference_means=diversity_reference_means,
          diversity_reference_sds=diversity_reference_sds
        )
      })

      # Reuse identical outcome noise across strategies within this run so
      # paired contrasts reflect grouping differences instead of fresh noise.
      shared_outcome_noise <- rnorm(n_teams, mean=0, sd=input$noise_sd)

      for (sn in names(strategies)) {
        team_components <- team_components_by_strategy[[sn]]
        composition_component <- as.numeric(team_components$team_mean_z %*% comp_effects)
        diversity_component <- as.numeric(team_components$diversity_z %*% div_effects)
        raw_score <- hexaco_compose_latent_score(
          intercept=baseline,
          composition_component=composition_component,
          diversity_component=diversity_component,
          residual=shared_outcome_noise
        )
        score_clipped_low <- raw_score < 1
        score_clipped_high <- raw_score > 5
        score <- pmin(5, pmax(1, raw_score))

        group_mean <- team_components$team_means[, trait_name]
        group_sd <- team_components$team_sds[, trait_name]
        rows <- idx:(idx + n_teams - 1L)
        all_results$run[rows] <- r
        all_results$strategy[rows] <- sn
        all_results$team[rows] <- seq_len(n_teams)
        all_results$mean_trait[rows] <- group_mean
        all_results$within_team_sd[rows] <- group_sd
        all_results$population_trait_mean[rows] <- population_trait_means[[trait_name]]
        all_results$baseline_quality[rows] <- baseline
        all_results$composition_component[rows] <- composition_component
        all_results$diversity_component[rows] <- diversity_component
        all_results$simulated_quality[rows] <- score
        all_results$clipped_at_boundary[rows] <- score_clipped_low | score_clipped_high
        idx <- idx + n_teams
      }
      if (r %% progress_step == 0L || r == reps) {
        setProgress(value=r/reps, detail=paste("Run", r, "of", reps))
      }
    }
    })
    list(
      data = all_results,
      settings = list(
        trait = trait_name,
        outcome = outcome_name,
        scenario = input$sim_scenario,
        seed = seed,
        n_teams = n_teams,
        team_size = team_size,
        reps = reps
      )
    )
  }, ignoreInit=TRUE)

  # FILTER RESULTS FOR THE CURRENT RESEARCH QUESTION
  # For the composition-only view, keep the Random strategy; otherwise retain all strategies.
  selected_results <- reactive({
    req(simulations())
    z <- simulations()$data
    if (input$research_question=="composition") z <- z[z$strategy=="Random",,drop=FALSE]
    z
  })

  # PLOT 1: Show composition relationships or paired differences in team means and spread.
  strategy_plot_object <- reactive({
    z <- selected_results()
    settings <- simulations()$settings
    validate(need(nrow(z)>0,"Run the simulation to see results."))
    if (input$research_question=="composition") {
      ggplot(z,aes(x=mean_trait,y=simulated_quality))+
        geom_point(alpha=.2,colour="#63F3FF")+
        geom_smooth(method="lm",formula=y~x,se=TRUE,colour="#FF4FD8",fill="#692E68")+
        labs(x=paste("Team mean",unname(nice_names[settings$trait])),y="Simulated teamwork-quality score",
             title="Composition and simulated quality",subtitle="Randomly formed teams; each point is a team in a repeated run")+
        theme_app()
    } else {
      run_mean <- aggregate(simulated_quality~run+strategy,data=z,FUN=mean)
      names(run_mean)[3] <- "mean_score"
      run_sd <- aggregate(simulated_quality~run+strategy,data=z,FUN=stats::sd)
      names(run_sd)[3] <- "between_team_sd"
      run_metrics <- merge(run_mean,run_sd,by=c("run","strategy"))
      make_paired_differences <- function(metric_name, metric_label) {
        random <- run_metrics[run_metrics$strategy=="Random",c("run",metric_name),drop=FALSE]
        names(random)[2] <- "random_value"
        alternatives <- run_metrics[run_metrics$strategy!="Random",c("run","strategy",metric_name),drop=FALSE]
        paired <- merge(alternatives,random,by="run")
        data.frame(run=paired$run,strategy=paired$strategy,
                   metric=metric_label,difference=paired[[metric_name]]-paired$random_value)
      }
      paired <- rbind(
        make_paired_differences("mean_score","Mean team score"),
        make_paired_differences("between_team_sd","Between-team SD")
      )
      strategy_order <- c("Balanced","Similar")
      metric_order <- c("Mean team score","Between-team SD")
      combinations <- expand.grid(strategy=strategy_order,metric=metric_order,stringsAsFactors=FALSE)
      plot_summary <- do.call(rbind,lapply(seq_len(nrow(combinations)),function(i) {
        strategy_name <- combinations$strategy[i]
        metric_name <- combinations$metric[i]
        values <- paired$difference[paired$strategy==strategy_name & paired$metric==metric_name]
        n <- length(values)
        if (!n) return(NULL)
        mean_difference <- mean(values)
        margin <- stats::qt(.975,df=n-1)*stats::sd(values)/sqrt(n)
        data.frame(strategy=strategy_name,metric=metric_name,mean_difference=mean_difference,
                   ci_low=mean_difference-margin,ci_high=mean_difference+margin)
      }))
      plot_summary$strategy <- factor(plot_summary$strategy,levels=strategy_order)
      plot_summary$metric <- factor(plot_summary$metric,levels=metric_order)
      ggplot(plot_summary,aes(x=strategy,y=mean_difference,colour=strategy))+
        geom_hline(yintercept=0,colour="#8492AD",linetype="dashed",linewidth=.7)+
        geom_linerange(aes(ymin=ci_low,ymax=ci_high),linewidth=1.5)+
        geom_point(size=4)+
        facet_wrap(~metric,scales="free_y",ncol=1)+
        scale_colour_manual(values=c(Balanced="#FF4FD8",Similar="#B7FF5A"))+
        labs(x=NULL,y="Change vs Random",
             title="How grouping changes average score and team-to-team spread",
             subtitle="Paired differences across runs; intervals crossing zero indicate an uncertain difference")+
        theme_app()+theme(legend.position="none",strip.text=element_text(colour="#63F3FF",face="bold"))
    }
  })

  output$strategy_plot <- renderPlot(strategy_plot_object())

  # PLOT 2: Show team mean trait score against simulated quality, with a line per strategy.
  composition_plot_object <- reactive({
    result <- simulations()
    z <- result$data
    settings <- result$settings
    validate(need(nrow(z)>0,"Run the simulation to see results."))

    ggplot(z,aes(x=mean_trait,y=simulated_quality,colour=strategy))+
      geom_point(alpha=.16,size=1.8)+
      geom_smooth(method="lm",formula=y~x,se=FALSE,linewidth=.9)+
      scale_colour_manual(values=c(Random="#63F3FF",Balanced="#FF4FD8",Similar="#B7FF5A"))+
      labs(
        x=paste("Team mean",unname(nice_names[settings$trait])),
        y="Simulated teamwork-quality score",
        title="Team trait composition and simulated quality",
        subtitle="Lines show the modelled association within each grouping strategy",
        colour="Grouping strategy"
      )+
      theme_app()+theme(legend.position="bottom")
  })

  output$composition_plot <- renderPlot(composition_plot_object())

  # TEAM PROFILE PREVIEW
  # Show each strategy's team means and within-team SDs for one shared resample.
  team_profile_plot_object <- reactive({
    result <- simulations()
    all_runs <- result$data
    settings <- result$settings
    validate(need(nrow(all_runs)>0,"Run the simulation to preview team profiles."))
    preview_id <- min(input$preview_run, max(all_runs$run))
    preview <- all_runs[all_runs$run==preview_id,,drop=FALSE]
    trait_label <- unname(nice_names[settings$trait])
    profile <- rbind(
      data.frame(strategy=preview$strategy, team=preview$team,
                 metric="Team mean", value=preview$mean_trait),
      data.frame(strategy=preview$strategy, team=preview$team,
                 metric="Within-team SD", value=preview$within_team_sd)
    )
    profile$strategy <- factor(profile$strategy, levels=c("Random","Balanced","Similar"))
    profile$metric <- factor(profile$metric, levels=c("Team mean","Within-team SD"))
    reference <- data.frame(
      metric="Team mean",
      reference=unique(preview$population_trait_mean)[1]
    )

    ggplot(profile,aes(x=strategy,y=value,colour=strategy))+
      geom_hline(data=reference,aes(yintercept=reference),inherit.aes=FALSE,
                 colour="#FF4FD8",linetype="dashed",linewidth=.8)+
      geom_boxplot(aes(group=strategy,fill=strategy),width=.52,alpha=.18,outlier.shape=NA,
                   linewidth=.65)+
      geom_point(position=position_jitter(width=.07,height=0),size=2.8,alpha=.9)+
      facet_wrap(~metric,scales="free_y",ncol=1)+
      scale_colour_manual(values=c(Random="#63F3FF",Balanced="#FF4FD8",Similar="#B7FF5A"))+
      scale_fill_manual(values=c(Random="#63F3FF",Balanced="#FF4FD8",Similar="#B7FF5A"))+
      labs(x=NULL,y=trait_label,
           title=paste("How teams differ on",trait_label),
           subtitle=paste("Run",preview_id,"· dashed line is the participant-pool mean"))+
      theme_app()+theme(legend.position="none",strip.text=element_text(colour="#63F3FF",face="bold"))
  })
  output$team_profile_plot <- renderPlot(team_profile_plot_object())

  # SUMMARY TABLE
  # Report the average team score, between-team SD, and paired changes versus Random.
  summary_table_data <- reactive({
    z <- selected_results()
    validate(need(nrow(z)>0,"Run the simulation to see results."))
    run_mean <- aggregate(simulated_quality~run+strategy,data=z,FUN=mean)
    names(run_mean)[3] <- "mean_score"
    run_sd <- aggregate(simulated_quality~run+strategy,data=z,FUN=stats::sd)
    names(run_sd)[3] <- "between_team_sd"
    run_clip <- aggregate(clipped_at_boundary~run+strategy,data=z,FUN=mean)
    run_metrics <- merge(merge(run_mean,run_sd,by=c("run","strategy")),
                         run_clip,by=c("run","strategy"))
    random_metrics <- run_metrics[run_metrics$strategy=="Random",,drop=FALSE]
    strategy_order <- c("Random","Balanced","Similar")
    format_difference <- function(rows, metric_name) {
      paired <- merge(rows[,c("run",metric_name),drop=FALSE],
                      random_metrics[,c("run",metric_name),drop=FALSE],
                      by="run",suffixes=c("","_random"))
      differences <- paired[[metric_name]]-paired[[paste0(metric_name,"_random")]]
      n_difference <- length(differences)
      difference_mean <- mean(differences)
      difference_margin <- if (n_difference > 1) stats::qt(.975,df=n_difference-1)*stats::sd(differences)/sqrt(n_difference) else NA_real_
      sprintf("%.3f [%.3f, %.3f]",difference_mean,
              difference_mean-difference_margin,difference_mean+difference_margin)
    }
    out <- do.call(rbind, lapply(strategy_order[strategy_order %in% as.character(run_metrics$strategy)], function(strategy_name) {
      rows <- run_metrics[as.character(run_metrics$strategy)==strategy_name,,drop=FALSE]
      mean_change <- if (strategy_name=="Random") "Reference" else format_difference(rows,"mean_score")
      sd_change <- if (strategy_name=="Random") "Reference" else format_difference(rows,"between_team_sd")
      data.frame(
        Strategy=strategy_name,
        `Mean team score`=round(mean(rows$mean_score),3),
        `Between-team SD`=round(mean(rows$between_team_sd),3),
        `Clipped at bounds (%)`=round(100*mean(rows$clipped_at_boundary),1),
        `Mean change vs Random (95% CI)`=mean_change,
        `SD change vs Random (95% CI)`=sd_change,
        check.names=FALSE
      )
    }))
    rownames(out) <- NULL
    out
  })
  output$summary_table <- renderTable(summary_table_data(),striped=TRUE,bordered=FALSE,spacing="m")

  output$download_summary <- downloadHandler(
    filename=function() {
      settings <- simulations()$settings
      paste0("simulation_summary_", settings$scenario, "_", settings$trait,
             "_seed_", settings$seed, ".csv")
    },
    content=function(file) {
      settings <- simulations()$settings
      summary <- summary_table_data()
      summary$Seed <- settings$seed
      summary$Scenario <- settings$scenario
      summary$Trait <- settings$trait
      summary$Outcome <- settings$outcome
      write.csv(summary,file,row.names=FALSE,na="")
    }
  )
  save_plot <- function(plot_reactive,file_label,width,height) {
    downloadHandler(
      filename=function() {
        settings <- simulations()$settings
        paste0("simulation_", file_label, "_", settings$scenario,
               "_", settings$trait, "_seed_", settings$seed, ".png")
      },
      content=function(file) ggplot2::ggsave(file,plot=plot_reactive(),width=width,height=height,dpi=300,bg="#0B1020")
    )
  }
  output$download_strategy_plot <- save_plot(strategy_plot_object,"strategy_comparison",9,7)
  output$download_team_profile <- save_plot(team_profile_plot_object,"team_profile",9,7)
  output$download_composition_plot <- save_plot(composition_plot_object,"composition",10,6)
}

# Launch the application by connecting the user interface and server logic.
shinyApp(ui,server)
