library(shiny)
library(tidyverse)

get_wep <- function(x) {
    wep <- case_when(x > .99 ~ "virtually certain to be to the ",
                     x > .90 ~ "very likely to be to the ",
                     x > .66 ~ "likely to be to the ",
                     x >= .50 ~ "more likely than not to be to the ",
                     x >= .45 ~ "only barely more likely than not to be to the ",
                     TRUE ~ " error! "
                     )
    return(wep)
}
        
### Read in the data we'll need
### We'll need to think about how to make this maximally efficient

dat <- readRDS("partyonly_theta_iters.rds")

dat <- dat |>
    mutate(iter = rep(1L:500L, times = nrow(dat)/500))

### Scale them 0-100
dat <- dat |>
    group_by(iter) |>
    mutate(value.sc = value - min(value),
           value.sc = value.sc / max(value.sc),
           value.sc = value.sc * 100)

dat$value.sc <- round(dat$value.sc)
dat$value.sc <- as.integer(dat$value.sc)

dat <- dat |>
    dplyr::select(Name, Constituency, Party, value, value.sc) |>
    mutate(DisplayName = paste0(Name, " (", Constituency, ")")) |>
    ungroup()

### Also read in cutoffs
cutoffs <- readRDS("mean_cutoff.rds")


### ################################################################
### User interface
### ################################################################

ui <- fluidPage(
    theme = bslib::bs_theme(
                       ## to customize if necessary
                       
                   ),
    tags$link(rel = "stylesheet", type="text/css", href="https://www.survation.com/wp-content/themes/SurvationFromCDN/css/bootstrap.css?ver=4.9.15"),
  # App title ----
    titlePanel("Survation/Royal Holloway estimates of MP positions"),
    tabsetPanel(type = "tabs",
              tabPanel("Compare two MPs", {
                  sidebarLayout(
                      sidebarPanel(
                          selectizeInput(inputId = "mp_a",
                                         label = "First-named MP:",
                                         choices = unique(dat$DisplayName),
                                         options = list(placeholder = 'type an MP name')),
                          selectizeInput(inputId = "mp_b",
                                         label = "Second-named MP:",
                                         choices = unique(dat$DisplayName),
                                         options = list(placeholder = 'type an MP name')),
                          actionButton("compare", "Compare!")
                      ),
                      mainPanel(
                          htmlOutput("text_descr", inline = TRUE)        
                      )
                      )
              }),
              tabPanel("Compare an MP with their party", {
                  sidebarLayout(
                      sidebarPanel(
                          selectizeInput(inputId = "mp_party",
                                         label = "MP:",
                                         choices = unique(dat$DisplayName),
                                         options = list(placeholder = 'type an MP name')),
                          actionButton("compare_within_party", "Compare!")
                      ),
                      mainPanel(
                          htmlOutput("party_comparison", inline = TRUE)        
                      )
                  )
              }),
              tabPanel("Compare an MP with the whole Commons", {
                  sidebarLayout(
                      sidebarPanel(
                          selectizeInput(inputId = "mp_sole",
                                         label = "MP:",
                                         choices = unique(dat$DisplayName),
                                         options = list(placeholder = 'type an MP name')),
                          actionButton("compare_sole", "Compare!")
                      ),
                      mainPanel(
                          htmlOutput("sole_comparison", inline = TRUE)        
                      )
                  )
              }),
              tabPanel("FAQs", {
                  sidebarLayout(
                      sidebarPanel(
                      ),
                      mainPanel(
                          h3("Frequently asked questions"),
                          h4("What are these estimates based on?"),
                          p('These estimates are based on over six thousand comparisons between pairs of MPs, made by local councillors in England, Wales and Scotland. Local councillors were presented with up to six comparisons between MPs in their local area, and two "anchor" MPs (Rishi Sunak and Keir Starmer), and asked to choose which MP from the pair was "more left-wing on economic issues". Councillors were allowed to say that they did not know the MPs, or that the MPs were "about the same. '),
                          h4("How many councillors took part in the survey?"),
                          p("1486 councillors took part in the survey, which was fielded between the 7th August and 3rd September 2023."),
                          h4("How many councillors said they didn't know the MPs, or that they were about the same?"),
                          p("Roughly one-third of responses were don't know responses; of the remaining responses, around a fifth indicated that the two MPs were about the same. The proportions of don't know and about-the-same responses reflect the fact that most of the time councillors were comparing MPs from their region, who might all be from the same party, and who might for that reason be harder to compare. "),
                          h4("What methodology did you use to turn the paired comparisons into scores?"),
                          p("You can find a full description of the methodology we used in this research note. If you've ever heard of a Bradley-Terry model, or if you know about Elo scores in chess, you should be able to understand the key idea behind pairwise comparisons, even if the implementation is a bit more complicated than either of those models. "),
                          h4("Can I use this data in my work?"),
                          p("You can use this data on the condition that you provide appropriate credit. Here, 'appropriate credit' means mentioning both Survation and Royal Holloway, University of London by name. We appreciate links, but understand that this may not always be possible. If you're using this data for academic work, you should cite the following working paper"),
                          h4("Can I download the data?"),
                          p("Yes: you can either download "),
                          tags$ul(
                                   tags$li(tags$a("this Excel spreadsheet", href = "https://github.com/chrishanretty/pairwise_mps/raw/a11e77efc875abe6d0c62a91d6da04e85d9557fd/outputs/mpsleftright_excel.xlsx"),
                                           ", which gives MP names, each MPs' party, their average score, their average rank, and 'low' and 'high' scores and ranks (explained in the spreadsheet). The scores have been rescaled so that the lowest (most left-wing) score is 0, and the highest (most right-wing) score is 100. "),
                                   tags$li(tags$a("this comma separated values file", href = "https://github.com/chrishanretty/pairwise_mps/raw/main/outputs/mpsleftright_full.csv.gz"),
                                           ", which includes the full output of the measurement model, with ONS constituency identifiers and TheyWorkForYou person codes")
                               ),
                          p("If you don't know which file to use, use the Excel spreadsheet. ")
                          
                          

                      )
                  )
              })
              ),

    div(style = "position: fixed; right: 2%; bottom: 2%;",
        HTML("&nbsp;"),
        img(src='https://intranet.royalholloway.ac.uk/staff/assets/img/brand-toolkit/logo-small-london-cmyk.jpg', align = "right", width = "150"),
        HTML("&nbsp;"),
        span(style = "width: 2em; "),
        img(src='https://cdn.survation.com/wp-content/theme/images/logo.png', align = "right", width = "150", style = "margin-top: 26px"),
        HTML("&nbsp;")
        )
    
)


### ################################################################
### Server
### ################################################################

server <- function(input, output) {
    require(stringr)

    ### Declare some reactive variables
    theta_a <- eventReactive(input$compare, {
        dat |> filter(as.character(DisplayName) == input$mp_a)
    })
    theta_b <- eventReactive(input$compare, {
        dat |> filter(as.character(DisplayName) == input$mp_b)
    })

    theta_mp_party <- eventReactive(input$compare_within_party, {
        pt_a <- dat |> filter(as.character(DisplayName) == input$mp_party) |>
            mutate(selected = 1)
        pt_b <- dat |> filter(Party == pt_a$Party[1]) |>
            mutate(selected = 0)
        rbind(pt_a, pt_b)
    })

    theta_mp_sole <- eventReactive(input$compare_sole, {
        pt_a <- dat |> filter(DisplayName == input$mp_sole) |>
            mutate(selected = 1)
        pt_b <- dat |> filter(DisplayName != input$mp_sole) |>
            mutate(selected = 0)
        rbind(pt_a, pt_b)
    })
    ### Output elements
    output$text_descr <- renderUI({
        bar_a <- mean(theta_a()$value.sc)
        bar_b <- mean(theta_b()$value.sc)

        if (bar_a > bar_b) {
            direction <- "right"
            prob <- mean(theta_a()$value > theta_b()$value)
        } else {
            direction <- "left"
            prob <- mean(theta_b()$value > theta_a()$value)
        }


        MP_A <- theta_a()$DisplayName |> unique()
        MP_B <- theta_b()$DisplayName |> unique()
        MP_A_full <- MP_A
        MP_B_full <- MP_B
        MP_A <- sub(" \\(.*", "", MP_A)
        MP_B <- sub(" \\(.*", "", MP_B)
        wep <- get_wep(prob)
        bar_a <- round(bar_a)
        bar_b <- round(bar_b)
        prob <- round(prob * 100)

        delta <- theta_a()$value - theta_b()$value
        cumprobs <- plogis(mean(delta) - cutoffs)
        cum2prob <- function(cp) {
            pr <- rep(NA, length(cp) + 1)
            pr[1] <- 1 - cp[1]
            for (i in 2:(length(pr) - 1)) {
                pr[i] <- cp[i-1] - cp[i]
            }
            pr[length(pr)] <- cp[length(cp)]
            pr
        }
        
### prs[1] is the probability
        pr <- round(100 * cum2prob(cumprobs))
        
        str_for_glueing <- "
<ul>
<li> <em>{MP_A_full}</em> is <strong>{wep}</strong> {direction} of <em>{MP_B_full}</em> on economic issues. </li>

<li> Our score for {MP_A}, on a scale from 0-100, is <texttt>{bar_a}</texttt>; the same score for {MP_B} is <texttt>{bar_b}</texttt> </li>

<li> The probability that {MP_A} is to the {direction} of {MP_B} is {prob}%. </li>

<li> If we asked 100 councillors to compare these two MPs (and all of them answered), we'd expect the following responses: 

<ul>
 <li> {pr[1]} councillors would say {MP_A} is much more left-wing than {MP_B} </li>
 <li> {pr[2]} councillors would say {MP_A} is somewhat more left-wing than {MP_B} </li>
 <li> {pr[3]} councillors would say the two MPs are about the same  </li>
 <li> {pr[4]} councillors would say {MP_B} is somewhat more left-wing than {MP_A} </li>
 <li> {pr[5]} councillors would say {MP_B} is much more left-wing than {MP_A} </li>
</ul>
</li>
</ul>

"
        
        print(HTML(stringr::str_glue(str_for_glueing)))
          
  })    

    output$party_comparison <- renderUI({
###
        party <- theta_mp_party() |> pull(Party) |> unique()
        bar_a <- mean(theta_mp_party() |> filter(selected == 1) |> pull(value.sc))
        bar_party <- median(theta_mp_party() |> filter(selected == 0) |> pull(value.sc))

        if (bar_a > bar_party) {
            direction <- "right"
            prob <- mean(theta_mp_party() |>
                         filter(selected == 1) |>
                         pull(value) >
                         theta_mp_party() |>
                         filter(selected == 0) |>
                         pull(value) |>
                         median())
        } else {
            direction <- "left"
            prob <- mean(theta_mp_party() |>
                         filter(selected == 1) |>
                         pull(value) <
                         theta_mp_party() |>
                         filter(selected == 0) |>
                         pull(value) |>
                         median())
        }

        MP_A <- theta_mp_party() |> filter(selected == 1) |> pull(DisplayName) |> unique()
        MP_A_full <- MP_A
        MP_A <- sub(" \\(.*", "", MP_A)
        
        wep <- get_wep(prob)
        bar_a <- round(bar_a)
        bar_party <- round(bar_party)
        prob <- round(prob * 100)
        
        str_for_glueing <- "
<ul>
<li> <em>{MP_A_full}</em> is <strong>{wep}</strong> {direction} of the average (median) MP in the {party} party. </li>

<li> Our score for {MP_A}, on a scale from 0-100, is <texttt>{bar_a}</texttt>; the same score for the median MP from the same party is <texttt>{bar_party}</texttt> </li>

<li> The probability that {MP_A} is to the {direction} of the median {party} MP is {prob}%. </li>
</ul>

"
        n_comparisons <- nrow(theta_mp_party())
        if (n_comparisons == 1) {
            print(HTML("This comparison doesn't make sense for Caroline Lucas, the sole Green MP"))
        } else {
            print(HTML(stringr::str_glue(str_for_glueing)))
        }

    })
    
    output$sole_comparison <- renderUI({
###
        bar_a <- mean(theta_mp_sole() |> filter(selected == 1) |> pull(value.sc))
        bar_party <- median(theta_mp_sole() |> filter(selected == 0) |> pull(value.sc))

        if (bar_a > bar_party) {
            direction <- "right"
            prob <- mean(theta_mp_sole() |>
                         filter(selected == 1) |>
                         pull(value) >
                         theta_mp_sole() |>
                         filter(selected == 0) |>
                         pull(value) |>
                         median())
        } else {
            direction <- "left"
            prob <- mean(theta_mp_sole() |>
                         filter(selected == 1) |>
                         pull(value) <
                         theta_mp_sole() |>
                         filter(selected == 0) |>
                         pull(value) |>
                         median())
        }

        MP_A <- theta_mp_sole() |> filter(selected == 1) |> pull(DisplayName) |> unique()
        MP_A_full <- MP_A
        MP_A <- sub(" \\(.*", "", MP_A)
        
        wep <- get_wep(prob)
        bar_a <- round(bar_a)
        bar_party <- round(bar_party)
        prob <- round(prob * 100)
        
        str_for_glueing <- "
<ul>
<li> <em>{MP_A_full}</em> is <strong>{wep}</strong> {direction} of the average (median) MP in the 2019-2014 House of Commons. </li>

<li> Our score for {MP_A}, on a scale from 0-100, is <texttt>{bar_a}</texttt>; the same score for the median MP is <texttt>{bar_party}</texttt> </li>

<li> The probability that {MP_A} is to the {direction} of the median MP is {prob}%. </li>
</ul>

"
        print(HTML(stringr::str_glue(str_for_glueing)))

    })
}

shinyApp(ui = ui, server = server)
