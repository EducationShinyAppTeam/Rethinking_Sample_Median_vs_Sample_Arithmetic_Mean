# Load Packages ----
library(shiny)
library(shinyBS)
library(shinydashboard)
library(boastUtils)
library(ggplot2)

# Load additional dependencies and setup functions----
# source("global.R")

# Define global constants, functions, and data ----
freedManDiaconis <- function(x) {
  2 * IQR(x) / length(x)^(1/3)
}

# Define UI for App ----
ui <- list(
  ## Create the app page ----
  dashboardPage(
    skin = "yellow",
    ### Create the app header ----
    dashboardHeader(
      title = "Median vs. Mean", # You may use a shortened form of the title here
      tags$li(class = "dropdown", actionLink("info", icon("info"))),
      tags$li(
        class = "dropdown",
        tags$a(
          target = "_blank", icon("comments"),
          href = "https://pennstate.qualtrics.com/jfe/form/SV_7TLIkFtJEJ7fEPz?appName=Rethinking_Sample_Median_vs_Sample_Arithmetic_Mean"
        )
      ),
      tags$li(class = "dropdown",
              tags$a(href='https://shinyapps.science.psu.edu/',
                     icon("home")))
    ),
    ### Create the sidebar/left navigation menu ----
    dashboardSidebar(
      sidebarMenu(
        id = "pages",
        menuItem("Overview", tabName = "Overview", icon = icon("dashboard")),
        menuItem("Sample Median", tabName = "median", icon = icon("wpexplorer")),
        menuItem("Sample Arithmetic Mean", tabName = "sam", icon = icon("wpexplorer")),
        menuItem("Skewness", tabName = "skewness", icon = icon("gears")),
        menuItem("References", tabName = "References", icon = icon("leanpub"))
      ),
      tags$div(
        class = "sidebar-logo",
        boastUtils::psu_eberly_logo("reversed")
      )
    ),
    ### Create the content ----
    dashboardBody(
      tabItems(
        #### Set up the Overview Page ----
        tabItem(
          tabName = "Overview",
          withMathJax(),
          h1("Rethinking the Sample Median vs Sample Arithmetic Mean"),
          p("An app for exploring and highlighting the differences between the
            Sample Median and Sample Arithmetic Mean."),
          p("More text to come"),
          h2("Instructions"),
          p("To get the most out of this app, we recommend that do the following:"),
          tags$ol(
            tags$li("(Re-) Familiarize yourself with the ",
                    em("sample median"),
                    "from a quantitative reasoning perspective."),
            tags$li("(Re-) Familiarize yourself with the ",
                    em("sample arithmetic mean"),
                    "from a quantitative reasoning perspective."),
            tags$li("Investigate the role that skewness has on both statistics.")
          ),
          ##### Go Button--location will depend on your goals ----
          div(
            style = "text-align: center;",
            bsButton(
              inputId = "go1",
              label = "GO!",
              size = "large",
              icon = icon("bolt"),
              style = "default"
            )
          ),
          ##### Create two lines of space ----
          br(),
          br(),
          h2("Acknowledgements"),
          p(
            "This version of the app was developed and coded by Neil J.
            Hatfield.",
            br(),
            br(),
            br(),
            div(class = "updated", "Last Update: 11/5/2020 by NJH.")
          )
        ),
        #### Median Page ----
        tabItem(
          tabName = "median",
          withMathJax(),
          h2("Explore the ", em("Sample Median")),
          p("More information to come."),
          p("Focus on the median from a QR perspective."),
        ),
        tabItem(
          tabName = "sam",
          withMathJax(),
          h2("Explore the ", em("Sample Arithmetic Mean")),
          p("More information to come."),
          p("Focus on the median from a QR prespective."),
          p("Spend some time on the Fallacy perspective.")
        ),
        #### Set up a Challenge Page ----
        tabItem(
          tabName = "skewness",
          withMathJax(),
          h2("The Role of Skewness"),
          p("Skewness is an aspect of the underlying stochastic process that
            generated our data which has an important role. We can think about
            skewness as telling us in which direction to anticipate outliers;
            towards values which are larger than the Expected Value, towards
            values which are smaller than the Expected Value, or in both directions.
            Over the years, we have come up with ways to measure skewness with
            positive values indicating to look towards values larger while negative
            values send us looking at smaller values. A skewness of zero indicates
            that we should check out both directions."),
          box(
            title = "A Note about Left/Right Skewness (They're bad!)",
            collapsible = TRUE,
            collapsed = TRUE,
            width = 12,
            p("We want to take brief moment and talk about the language we use
              to characterize skewness, especially as related to data
              visualizations. A lot of people will often use the terms 'left
              skewed'/'long left tail' and 'right skewed'/'long right tail'. ",
              strong("You should avoid these terms like they are carriers of the
                     corona virius and you're about to go see a beloved, ederly
                     family member/friend. ")),
            p("There are several problems with these phrases which often lead
              people to making mistakes (e.g., 'left skewed' means the mound is
              to the left). Two important issues are 1) 'left'/'right' don't
              connect to any of our measures of skewness (what's a 'left' number?),
              and 2) 'left'/'right' don't work in all cases. Here is a histogram
              that is positively skewed (outliers appear to be larger than the
              main modal clump's values) but there's nothing 'right skewed' about
              the graph."),
            div(
              style = "text-align: center;",
              plotOutput(
                outputId = "leftRightBad",
                width = "50%",
                height = "250px"
              ),
              tags$script(HTML(
                "$(document).ready(function() {
                document.getElementById('leftRightBad').setAttribute('aria-label',
                `This plot highlights that using the terms 'left' and 'right' to
                describe skewness fail to work in many cases. Use the terms
                'negative' and 'positive' instead.`)
                })"
              ))
            ),
            p("We will only use the terms 'negative', 'positive', or 'no'/
                'symmetric' when referring to skewness.")
          ),
          p(
            "When people set up the false competition between the ",
            em("sample median"), " and ", em("sample arithmetic mean"),
            " they often bring up the following refrain:"
          ),
          tags$blockquote(
          "If the data are skewed right (positively), the mean is bigger
          than the median. If the data are left (negatively) skewed, the mean is
          less than the median."
          ),
          p(
            "Many times textbook authors and instructors make this statement and
            people treat the statement as a fact of law. The problem is that this
            statement is...", strong("WRONG! "), "Think of the above statement
            as more of a loose, general guideline that is meant to be broken like
            the i-before-e-except-after-c 'rule' in English grammar, which more
            words break than follow."
          ),
          p("We've complied a set of examples--some real, some simulated--which
            all break this false rule. Explore these examples below."),
          br(),
          fluidRow(
            column(
              width = 4,
              wellPanel(
                h3("Data Set"),
                selectInput(
                  inputId = "dataSet",
                  label = "Select a data set",
                  choices = c(
                    "Tooth agenisis",
                    "Gestational age",
                    "others"
                  )
                ),
                p("context"),
                uiOutput("dataContext")
              )
            ),
            column(
              width = 8,
              p("histogram"),
              plotOutput("dataPlots")
            )
          ),
          hr(),
          box(
            title = "Textbooks that get this wrong",
            collapsible = TRUE,
            collapsed = TRUE,
            width = 6,
            p("Here are few textbooks that get this wrong; that is, present this
              as a true law of Statistics."),
            tags$ul(
              tags$li(
                tags$cite("Statistics: Unlocking the Power of Data"),
                " (2nd ed.) by Lock, Lock, Lock Morgan, Lock, and Lock"
              ),
              tags$li(
                tags$cite("Introduction to Statistical Investigations"),
                " by Tintle, Chance, Cobb, Rossman, Roy, Swanson, and VanderStoep"
              ),
              tags$li(
                tags$cite("Workshop Statistics: Discovery with Data"),
                " (3rd ed.) by Rossman and Chance"
              ),
              tags$li(
                tags$cite("Introduction to the Practice of Statitics"),
                " (7th ed.) by Moore, McCabe, and Craig"
              )
            )
          ),
          box(
            title = "Textbooks that essentially get this wrong",
            collapsible = TRUE,
            collapsed = TRUE,
            width = 6,
            p("The following textbooks include the word 'generally' when they
              first introduce this false law. However, they do not provide any
              counter-examples and they often continue on as if 'generally' was
              never there."),
            tags$ul(
              tags$li(
                tags$cite("Introduction to Statistics & Data Analysis"),
                " (5th ed.) by Peck, Olsen, and Devore"
              ),
              tags$li(
                tags$cite("Statistics: The Art and Science of Learning from Data"),
                " (4th ed.) by Agresti, Franklin, and Klingenberg"
              )
            )
          ),
          p("If you have more data sets which serve as counter-examples or know
            of any textbooks which get this issue correct, please feel free to
            send the information on to us.")
        ),
        #### Set up the References Page ----
        tabItem(
          tabName = "References",
          withMathJax(),
          h2("References"),
          p(
            class = "hangingindent",
            "Bailey, E. (2015). shinyBS: Twitter bootstrap components for shiny.
            (v0.61). [R package]. Available from
            https://CRAN.R-project.org/package=shinyBS"
          ),
        )
      )
    )
  )
)

# Define server logic ----
server <- function(input, output, session) {

  ## Left-Right Skew Plot ----
  output$leftRightBad <- renderPlot({
    ggplot(
      data = data.frame(
        val = rchisq(500, 5)
      )
    ) +
      geom_histogram(
        mapping = aes(y = val),
        fill = boastPalette[6],
        col = "black",
        closed = "left",
        binwidth = freedManDiaconis
      ) +
      labs(
        title = "Positive Skewed Histogram",
        x = "Frequency",
        y = "Value"
      ) +
      theme_bw() +
      theme(
        plot.caption = element_text(size = 18),
        text = element_text(size = 18),
        axis.title = element_text(size = 16)
      )
  })

}

# Boast App Call ----
boastUtils::boastApp(ui = ui, server = server)
