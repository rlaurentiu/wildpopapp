library(shiny)
library(DT)
library(unmarked)
library(ggplot2)
library(dplyr)
library(tidyr)
library(bslib)
library(MuMIn)
library(shinybusy)
library(shiny.info)

# global variable version
VERSION <- "version 1.0.1. Questions to rlaurentiu@gmail.com"


# Define UI
ui <- fluidPage(
  tags$head(tags$style(
    HTML(
      "
      #footer {
        position: fixed;
        left: 0;
        bottom: 0;
        width: 100%;
        background-color: #f5f5f5;
        color: black;
        text-align: center;
        padding: 10px;
      }
      .navbar-header {
        float: left !important;
        padding-top: 0;
        padding-bottom: 0;
      }
      .navbar-brand img {
        height: 100px;
        display: inline-block;
        vertical-align: middle;
      }
      .navbar-brand h2 {
        display: inline-block;
        margin: 0;
        vertical-align: middle;
      }
      .nav-tabs a {
        font-weight: bold;

      }
    "
    )
  )),
  tagList(
    shiny.info::version(position = "bottom left")),
  navbarPage(
    div(
      style = "height: 100px; width: auto; display: inline-block;",
      img(
        src = "wildpop.png",
        height = "100",
        style = "float: left;"
      ),
      h2("WildPop", style = "float: left; margin-left: 10px; margin-top: 30px;")
    ),
    theme = bslib::bs_theme(bootswatch = "minty", base_font = "Noto"),
    tabPanel("home",  uiOutput("homeContent")),
    tabPanel("simN-mixture",
             fluidPage(
               h3("Simulare model N-mixture fără covariate"),
               sidebarLayout(
                 sidebarPanel(
                   numericInput("seed", "Random seed reproductibilitate", value = 24),
                   sliderInput(
                     "num_sites",
                     "Număr zone (M):",
                     min = 1,
                     max = 200,
                     value = 100
                   ),
                   sliderInput(
                     "num_measurements",
                     "Număr vizite (J):",
                     min = 1,
                     max = 50,
                     value = 3
                   ),
                   sliderInput(
                     "lambda",
                     "Abundență așteptată (lambda):",
                     min = 1,
                     max = 100,
                     value = 4
                   ),
                   sliderInput(
                     "detection_prob",
                     "Probabilitate de detecție (p):",
                     min = 0,
                     max = 1,
                     value = 0.6
                   ),
                   add_busy_bar(color = "#FF0000"),
                   actionButton("simulate_btn", "Rulează modelul")
                 ),
                 mainPanel(
                   uiOutput("text_rnsim"),
                   plotOutput("abundance_plot"),
                   dataTableOutput("table_summary"),
                   uiOutput("descript_rnsim_table"),
                   uiOutput("correlation_output"),
                   verbatimTextOutput("model_summary_text"),
                   verbatimTextOutput("back_transformed_text"),
                   uiOutput("descript_rnsim_summ"),
                   uiOutput("text_referinta")
                 )
               )
             )),
    tabPanel("simN-mixturecov",
             fluidPage(
               h3("Simulare model N-mixture cu covariate"),
               sidebarLayout(
                 sidebarPanel(
                   HTML("Random seed reproductibilitate"),
                   numericInput("seed2", "", value = 24),
                   HTML("Zone și vizite"),
                   selectInput(
                     "M2",
                     "Număr zone (M):",
                     choices = c(3, 9, 30, 90, 150, 210, 300),
                     selected = 150
                   ),
                   numericInput("J2", "Număr vizite (J):", value = 3),
                   HTML("Parametrii pentru modelare abundență"),
                   numericInput("beta0", "Interceptarea VegHt (beta0):", value = 0),
                   numericInput("beta1", "Panta pentru vegHt (beta1):", value = 2),
                   HTML("Parametrii pentru modelare detecție"),
                   numericInput("alpha0", "Interceptarea vânt (alpha0):", value = -2),
                   numericInput("alpha1", "Panta pentru vânt (alpha1):", value = -3),
                   add_busy_bar(color = "#FF0000"),
                   actionButton("run_model", "Rulează modelul")
                 ),
                 mainPanel(tabsetPanel(
                   tabPanel(
                     "Grafice Covariate",
                     add_busy_bar(color = "#FF0000"),
                     uiOutput("text_rnsimcov"),
                    plotOutput("abundance_plot2"),
                    plotOutput("wind_plot2"),
                    uiOutput("rnsim_cov_fig"),
                    uiOutput("text_referinta2")
                   ),
                   tabPanel(
                     "Sumarul modelelor rulate",
                     add_busy_bar(color = "#FF0000"),
                     verbatimTextOutput("model_summary1"),
                     verbatimTextOutput("model_summary2"),
                     verbatimTextOutput("model_summary3"),
                     DTOutput("modelComparisonOutput"),
                     uiOutput("rnsim_cov_summary"),
                     uiOutput("text_referinta3")
                   ),
                   tabPanel("Comparație cu model GLM",
                            add_busy_bar(color = "#FF0000"),
                            plotOutput("comparison_plot"),
                            uiOutput("rnsim_cov_plotsum"),
                            uiOutput("text_referinta4")
                 )
               )
             ))
             )
             ),
    tabPanel("despre wildPop", uiOutput("aboutContent"))
  ),
  # Footer
  tags$footer(style = "text-align: center; padding-top: 10px; padding-bottom: 10px;",
              HTML(
                paste(
                  "&copy; CCMESI",
                  format(Sys.Date(), "%Y"),
                  "WildPop. PN-III-P2-2.1-PED-2021-1965 Interactive tool for estimating abundance of wildlife populations"
                )
              ))
)

# Define server logic for app
server <- function(input, output, session) {
  simulated_data_sinnmix <- reactiveVal(NULL)
  model_summary_snmix <- reactiveVal(NULL)
  
  observeEvent(input$simulate_btn, {
    if (!is.null(input$seed)) {
      set.seed(input$seed)
    }
    
    M <- input$num_sites
    J <- input$num_measurements
    lambda <- input$lambda
    p <- input$detection_prob
    
    C <- matrix(NA, nrow = M, ncol = J)
    N <- rpois(n = M, lambda = lambda)
    for (j in 1:J) {
      C[, j] <- rbinom(n = M,
                       size = N,
                       prob = p)
    }
    
    simulated_data_sinnmix(C)
    
    total_pop <- sum(apply(C, 1, max))
    num_occupied_sites <- sum(apply(C, 1, max) > 0)
    mean_abundance <- mean(apply(C, 1, max))
    
    output$table_summary <- renderDataTable({
      data <- data.frame(
        Metric = c(
          "Mărimea reală a populației",
          "Numărul real de zone ocupate",
          "Abundența medie (reală)",
          "Mărimea observată a populației",
          "Numărul observat de zone ocupate",
          "Abundența medie (observată)"
        ),
        Value = c(
          sum(N),
          sum(N > 0),
          mean(N),
          total_pop,
          num_occupied_sites,
          mean_abundance
        )
      )
      datatable(data,
                options = list(dom = 'lpt', searching = FALSE))
    })
    
    output$abundance_plot <- renderPlot({
      data <- data.frame(True_Abundance = N,
                         Observed_Abundance = apply(C, 1, max))
      
      data %>%
        pivot_longer(
          cols = c(True_Abundance, Observed_Abundance),
          names_to = "Variable",
          values_to = "Value"
        ) %>%
        ggplot(aes(x = Value, color = Variable)) +
        geom_density() +
        labs(title = "Abundența reală vs. Abundența observată",
             x = "Value",
             y = "Density") +
        scale_color_manual(values = c(
          "True_Abundance" = "blue",
          "Observed_Abundance" = "red"
        )) +
        theme_bw()
    }, width = function() {
      if (is.null(session$clientData$output_abundance_plot_width)) {
        NULL
      } else {
        0.8 * session$clientData$output_abundance_plot_width
      }
    }, height = 400)
    
    correlation_value <- cor(C)[1, 2]
    output$correlation_output <- renderUI({
      correlation_text <-
        paste("Corelatia dintre detecția reală și detecția observată (p):",
              correlation_value)
      
      HTML(
        paste(
          "<div style='font-size: 16px; font-weight: bold;'>",
          "<br>",
          "Corelație modelare vs. observații:</div>",
          "<br>",
          "<div style='font-size: 14px;'>",
          correlation_text,
          "</div>"
        ),
        "<br>"
      )
    })
    
    umf_snmix <- unmarkedFramePCount(y = simulated_data_sinnmix())
    fm1 <- pcount( ~ 1 ~ 1, data = umf_snmix)
    model_summary_snmix(summary(fm1))
    
    output$model_summary_text <- renderPrint({
      capture.output({
        summary(fm1)
      })
    })
    
    back_transformed_state <- backTransform(fm1, "state")
    back_transformed_det <- backTransform(fm1, "det")
    
    output$back_transformed_text <- renderPrint({
      cat("Back-Transformed State Estimates (Abundance):\n")
      print(back_transformed_state)
      cat("\nBack-Transformed Detection Estimates (p):\n")
      print(back_transformed_det)
    })
  })
  
  observeEvent(input$run_model, {
    seed <- input$seed2
    set.seed(seed)
    
    M <- as.numeric(input$M2)
    J <- input$J2
    beta0 <- input$beta0
    beta1 <- input$beta1
    alpha0 <- input$alpha0
    alpha1 <- input$alpha1
    
    vegHt <- sort(runif(M, -1, 1))
    
    lambda <- exp(beta0 + beta1 * vegHt)
    N <- rpois(M, lambda)
    
    wind <- array(runif(M * J, -1, 1), dim = c(M, J))
    p <- plogis(alpha0 + alpha1 * wind)
    
    C <- matrix(NA, nrow = M, ncol = J)
    for (j in 1:J) {
      C[, j] <- rbinom(M, N, p[, j])
    }
    
    time <-
      matrix(rep(as.character(1:J), M), ncol = J, byrow = TRUE)
    Mhab <- M / 3
    hab <- c(rep("A", Mhab), rep("B", Mhab), rep("C", Mhab))
    
    umf_snmix <- unmarkedFramePCount(
      y = C,
      siteCovs = data.frame(vegHt = vegHt, hab = hab),
      obsCovs = list(time = time, wind = wind)
    )
    
    fm.Nmix1 <- pcount( ~ wind ~ vegHt, data = umf_snmix, mixture = "P")
    fm.Nmix2 <- pcount( ~ wind ~ vegHt, data = umf_snmix, mixture = "NB")
    fm.Nmix3 <- pcount( ~ wind ~ vegHt, data = umf_snmix, mixture = "ZIP")
    
    models <- list(fm.Nmix1, fm.Nmix2, fm.Nmix3)
    modelComparison <- model.sel(models)
    aicValues <- as.data.frame(modelComparison)
    
    lambda.hat <- predict(fm.Nmix1, type = "state")
    p.hat <- predict(fm.Nmix1, type = "det")
    
    fm.glm <-
      glm(c(umf_snmix@y) ~ rep(umf_snmix@siteCovs$vegHt, J), family = poisson)
    
    df <- data.frame(
      vegHt = umf_snmix@siteCovs$vegHt,
      C = as.vector(umf_snmix@y),
      glm_with_p = predict(fm.Nmix1, type = "state") [, 1],
      glm_without_p = exp(coef(fm.glm)[1] + coef(fm.glm)[2] * vegHt)
    )
    
    comparison_plot <- ggplot(data = df, aes(x = vegHt, y = C)) +
      geom_point(aes(color = "Counts"), size = 3) +
      geom_line(
        data = df,
        aes(x = vegHt, y = glm_with_p, color = "GLM Poisson cu p"),
        linewidth = 1
      ) +
      geom_line(
        data = df,
        aes(x = vegHt, y = glm_without_p, color = "GLM Poisson fără p"),
        linewidth = 1
      ) +
      geom_line(data = df,
                aes(
                  x = vegHt,
                  y = exp(beta0 + beta1 * vegHt),
                  color = "Truth"
                ),
                linewidth = 1) +
      labs(x = "Înălțimea vegetației (VegHt)", y = "Număr apariții") +
      theme_minimal() +
      scale_color_manual(
        values = c(
          "Număr apariții" = "gray",
          "GLM Poisson cu p" = "blue",
          "GLM Poisson fără p" = "black",
          "Truth" = "red"
        )
      )
    
    #  data(list(M = M, J = J, vegHt = vegHt, N = N, wind = wind, C = C))
    
    output$abundance_plot2 <- renderPlot({
      df <- data.frame(vegHt = vegHt,
                       lambda = lambda,
                       N = N)
      ggplot(data = df) +
        geom_line(aes(x = vegHt, y = lambda),
                  lwd = 3,
                  color = "blue") +
        geom_point(aes(x = vegHt, y = N)) +
        labs(x = "Înălțimea vegetației (VegHt)", y = "Abundență")
    }, width = function() {
      if (is.null(session$clientData$output_abundance_plot2_width)) {
        NULL
      } else {
        0.8 * session$clientData$output_abundance_plot2_width
      }
    }, height = 400)
    
    output$wind_plot2 <- renderPlot({
      wind_data <-
        data.frame(wind = as.vector(wind),
                   p = as.vector(p),
                   C = as.vector(C))
      
      ggplot(data = wind_data) +
        geom_line(aes(x = wind, y = p), lwd = 3, color = "red") +
        geom_point(aes(x = wind, y = C / max(C))) +
        labs(x = "Vânt", y = "Număr apariții (scalat): C/max(C)")
    }, width = function() {
      if (is.null(session$clientData$output_wind_plot2_width)) {
        NULL
      } else {
        0.8 * session$clientData$output_wind_plot2_width
      }
    }, height = 400)
    output$model_summary1 <- renderPrint({
      model_summary_snmix <- summary(fm.Nmix1)
    })
    
    output$model_summary2 <- renderPrint({
      model_summary_snmix <- summary(fm.Nmix2)
    })
    
    output$model_summary3 <- renderPrint({
      model_summary_snmix <- summary(fm.Nmix3)
    })
    
    output$comparison_plot <- renderPlot({
      comparison_plot
    }, width = function() {
      if (is.null(session$clientData$output_comparison_plot_width)) {
        NULL
      } else {
        0.8 * session$clientData$output_comparison_plot_width
      }
    }, height = 400)
    
    output$modelComparisonOutput <- renderDT({
      datatable(aicValues, options = list(
        dom = 't',
        initComplete = JS(
          "function(settings, json) {",
          "$('.dataTables_filter').parent().remove();",
          "$('.dataTables_length').parent().remove();",
          "}"
        )
      )) %>% formatRound(
        columns = c(
          "p(Int)",
          "lam(Int)",
          "p(wind)",
          "lam(vegHt)",
          "logLik",
          "delta",
          "AICc",
          "weight"
        ),
        digits = 4
      )
    })
  })
  
  # home page text
  
  output$homeContent <- renderUI({
    fluidPage(
      h3(
        "Aplicație interactivă estimare abundență animale sălbatice cu date simulate"
      ),
      p(
        "Estimarea abundenței speciilor constituie una din cele mai dificile și importante puncte în managementul speciilor sălbatice. Datele pentru aceste estimări se obțin în principal prin: studii de capturare-recaptuare (fiecare individ poate fi identificat) și studii de tip ocupanță (occupancy), în care indivizii nu sunt identificați."
      ),
      p(
        "Datele pentru studii de tip ocupanță pot fi obținute prin înregistrarea directă sau indirectă a prezenței (observații, camere foto, înregistrare sunete, urme). Studiile implicând înregistrarea de urme sau imaginilor cu camere foto sunt relativ ușor de implementat pe teren, dar pentru a obține date robuste sunt necesare protocoale de prelevare corecte și o analiză statistică a datelor riguroasă."
      ),
      p(
        "Analiza statistică poate fi realizată folosind metode complexe (de exemplu în R, prin pachetul unmarked), dar sunt necesare cunoștințe solide de programare precum și o înțelegere foarte bună a teoriei analizei ierahizatoare a datelor populaționale."
      ),
      p(
        "Modelele ierarhice se bazează pe principiul că datele ecologice sunt rezultatul a două procese interdependente. Primul, procesul ecologic, determină starea adevărată a mediului, cum ar fi ocuparea reală sau abundența unei specii. Acesta este un factor critic, deoarece reprezintă scenariul real studiat. Al doilea, procesul de observare, este cel care influențează datele culese în timpul sondajelor. Acest proces depinde în mod inerent de procesul ecologic, deoarece ceea ce este observat și înregistrat este dependent de starea reală a mediului."
      ),
      p(
        "Primul tip este reprezentat de modelele de ocupanță, modele orientate spre evaluarea apariției speciilor. Ele oferă un cadru pentru estimarea probabilității ca o specie să fie prezentă într-o anumită zonă, încorporând totodată probabilitățile de detectare în calculele lor.
        "
      ),
      p(
        "Al doilea tip este reprezentat de modelele N-mixture, care sunt deosebit de utile pentru estimarea abundenței speciilor. Aceste modele sunt eficiente în abordarea variațiilor naturale legate de numărul speciilor și sunt dezvoltate pentru a integra posibilele erori de detectare. Acest lucru le face potrivite pentru studii în care estimările precise ale numărului de specii sunt importante.
"
      ),
      p(
        "Un aspect cheie modelelor ierarhice este dependența lor de datele din monitorizări replicate temporal efectuate pe mai multe site-uri. Înregistrările repetate îmbunătățește rezultatele și fiabilitatea modelelor.
        "
      ),
      p(
        "În pagina simoccu (simulare modelare ocupanță) vom explora rezultatele unui model simplu de ocupanță iar în pagina simoccucov îl vom extinde cu covariate care explică ocupanța și probabilitatea de detecție. Modelele de ocupanță au câteva constrângeri de care trebuie să ținem seamă când proiectăm un studiu:
      "
      ),
      tags$p(
        "În pagina simoccu (simulare modelare ocupanță) vom explora rezultatele unui model simplu de ocupanță iar în pagina simoccucov îl vom extinde cu covariate care explică ocupanța și probabilitatea de detecție. Modelele de ocupanță au câteva constrângeri de care trebuie să ținem seamă când proiectăm un studiu:"
      ),
      tags$ul(
        tags$li(
          "populația este închisă, adică perioada de studiu este suficient de scurtă pentru a afirma că nu există schimbări numerice în populații
              de la începutul pănă la sfârșitul studiului. Aceasta ne permită să prelevăm date din aceeași populație."
        ),
        tags$li(
          "nu sunt observații fals-pozitive. Dacă nu suntem siguri de specie, mai degrabă renunțăm la aceea înregistrare."
        ),
        tags$li(
          "observațiile din zonele de prelevare sunt independente unele de altele. Adică, de exemplu, nu există șanse mari ca un urs de la situl A să fie înregistrat și la situl B."
        ),
        tags$li(
          "detecția este omogenă între situri. Dacă nu, va trebui să includem un model cu covariată."
        )
      )
    )
  })
  
  # about page text
  
  output$aboutContent <- renderUI({
    fluidPage(
      h3("Despre Wildpop"),
      tags$p(
        "Proiectul Aplicație interactivă pentru estimarea abundenței populațiilor de animale sălbatice (WildPop) PN-III-P2-2.1-PED-2021-1965 Proiect experimental demonstrativ (PED) este finanțat de Unitatea Executivă pentru Finanțarea Învățământului Superior, a Cercetării, Dezvoltării și Inovării (UEFISCDI)."
      ),
      
      tags$p(
        "Estimarea abundenței speciilor constituie una din cele mai dificile și importante puncte în managementul speciilor sălbatice. Datele pentru aceste estimări se obțin în principal prin: studii de capturare-recaptuare (fiecare individ poate fi identificat) și studii de tip ocupanță, în care indivizii nu sunt identificați. Datele pentru studii de tip occupancy pot fi obținute prin înregistrarea urmelor sau camere foto. Studiile implicând urme sau camere sunt relativ ușor de implementat pe teren, dar pentru a obține date robuste sunt necesare protocoale de prelevare corecte și o analiză statistică a datelor riguroasă. Analiza statistică poate fi realizată folosind metode complexe (de exemplu în R, prin pachetul unmarked), dar sunt necesare cunoștințe solide de programare precum și o înțelegere foarte bună a teoriei analizei ierahizatoare a datelor populaționale. Scopul proiectului WildPop este accesibilizeze astfel de modele de analiză și să îmbunătățească estimările demografice ale populațiilor speciilor de animale sălbatice din România, prin dezvoltarea, testarea și punerea la dispoziție a unei aplicații interactive de evaluarea a abundenței și distribuției animalelor sălbatice care să poată utiliza date obținute de cercetători și administratorii fondurilor de vânătoare. Pentru atingerea acestui scop, se vor evalua rezultatele proiectelor de monitorizare a speciilor, identifica punctele tari și punctele slabe ale acestor proiecte din perspectiva robusteței estimărilor demografice în urma aplicării de modelare de tip occupancy, elabora in cadru eficient de estimare a populațiilor utilizând camere foto și transecte utilizând modele interactive R Shiny. Practicienii administrării fondurilor de vânătoare și biologii vor avea la dispoziție un cadru eficient de planificare și implementare a unor studii de monitorizare, care să reducă costurile, crească robustețea datelor și producă date pentru justificarea de măsuri eficiente de conservare a diversității biologice."
      ),
      
      
      tags$p("Obiectivele proiectului sunt:"),
      tags$ul(
        tags$li(
          "O1: Evaluarea inițiativelor din România de estimare a abundenței sau ocupanței animalelor sălbatice, urmărind în principal inițiativele dedicate speciilor de carnivore mari de interes comunitar (urs, lup și râs);"
        ),
        tags$li(
          "O2: Realizarea în parteneriat cu actori locali a unui ghid de monitorizare eficientă și robustă a evoluției abundenței sau ocupanței animalelor sălbatice folosind date de prezență a indivizilor nemarcați;
O3: Dezvoltarea unei aplicații interactive pentru estimarea abundenței sau ocupanței animalelor sălbatice utilizând pachetul R Shiny;"
        ),
        tags$li(
          "O4: Facilitarea dezvoltării pieței de servicii de consultanță privind estimarea abundenței sau ocupanței animalelor sălbatice prin training-ul părților interesate (e.g., autorități publice, manageri de fonduri de vânătoare, administratori de arie protejate)."
        ),
        tags$a(
          href = "https://wildpop.ccmesi.ro/",
          "Mai multe detalii pe pagina web de prezentare a aplicației!"
        )
        ## <a href="https://wildpop.ccmesi.ro/">Mai multe detalii pe pagina web de prezentare a aplicației!</a>
      )
    )
  })
  
  
  # text_rnsim
  output$text_rnsim <- renderText({
    HTML(
      "
      <p>În partea stângă vom introduce parametrii modelării, adică numărul de zone, numărul de vizite, abundența așteptată la un sit, probabilitatea de detecție. Puteți realiza modele pentru diferite scenarii (număr mai mic sau mai mare de vizite sau situri, abundență sau detectabilitate mai mare sau mai mică). Astfel, vom putea estima care trebuie să fie intensitatea studiului pentru a obține rezultate robuste. Dacă probabilitatea de detecție asumată este mică (șanse mici să înregistrăm specia la camere) încercați să măriți numărul de vizite.</p>

      <ul>
        <li>random seed pentru reproductibilitate. Utilizarea unui număr (de exemplu, 24) de toți utilizatorii asigură obținerea de rezultate asemănătoare dacă valoare parametrilor nu se schimbă. În caz contrar se vor obține rezultate ușor diferite.</li>
        <li>numărul de zone investigate M. Vom introduce numărul de zone în care dorim să punem camere. De exemplu, 100 zone. Explorați modele cu număr mai mare sau mai mic de zone investigate, dar țineți cont de echipamentele de care dispuneți.</li>
        <li>numărul de vizite (J). Este un parametru important, pentru că dacă avem detectabilitate mică trebuie să mergem mai des pe teren. Nu exagerați cu numărul de vizite, și nu lungiți mult studiul. Găsiți un echilibru.</li>
        <li>abundența așteptată (lambda). Abundența așteptată (expert opinion, literatură) la o zonă investigată.</li>
        <li>probabilitatea de detecție per individ. Care este șansa să detectez un individ (fotografiat de cameră) dacă suntem siguri că se găsește în zonă? Este un parametru care poate varia în realitate și de la o zonă la alta, dar noi vom considera o valoare comună pentru toate siturile.</li>
      </ul>

      <p>După ce ajustați parametrii rulați modelul din nou. Notați rezultatele și găsiți valorile optime pentru voi.
</p>
    "
    )
  })
  
  
  data_generated_rn <- reactiveVal(FALSE) # reactive test rn sim
  
  observeEvent(input$simulate_btn, {
    # reactive test sim rn
    data_generated_rn(TRUE)
  })
  
  
  
  output$descript_rnsim_table <- renderText({
    if (data_generated_rn()) {
      HTML(
        "
        <p>Rezultatele simulării (figura si tabel) indică mărimea reală (modelată) a populației și mărimea detectată (înregistrată de camere). De asemenea, putem analiza numărul de zone la care s-au înregistrat fotografii cu speciile studiate versus numărul de zone estimate ca fiind ocupate. Ar trebui să notăm rezulatele (sau salvăm graficul) și rulăm modelul cu parametrii diferiți (număr mai mare sau mai mic de vizite/zone). </p>  
 <p> De asemenea, de interes este și corelația modelare vs. observații (jos), care ar trebui să fie cât mai apropiată de probabilitate de detecție (p) introdusă de noi în stânga. </p>
      "
      )
    }
  })
  

output$descript_rnsim_summ <- renderText({
  if (data_generated_rn()) {
    HTML(
      "
        <p>Cele două casete cu informații reprezintă modelul nul (fără covariate pentru abundență și detecție) N-mixture un singur sezon, o singură specie (pcount în unmarked). </p>  
 <p> După rularea modelului vom obține sumarul așa cum rezultă din unmarked. Modelul este pcount(formula = ~1 ~ 1, data = umf), adică se modelează doar abundența și detecția, fără covariante. </p>
 <p> După ajutare, vom obține o abundență (valoarea de sub Estimate Backtransformed linear combination(s) of Abundance estimate(s) și o probabilitate de detecție (Backtransformed linear combination(s) of Detection estimate(s)))</p>
 <p> După ce ajustați parametrii re-rulați modelul. Notați rezultatele și găsiți valorile optime (vizite și zone) pentru voi.</p>
 <p> Comparați rezultatul de la detecție cu Număr observat de zone ocupate și pe cel de la abundență cu valoarea introdusă de noi la Abundență așteptată (lambda).</p>
      "
      
    )
  }
  
})

output$text_referinta <- renderText({
  if (data_generated_rn()) {
    HTML(
      "
        <p>Simularea a fost adaptată din Kéry, M., & Royle, J. A. (2016). Chapter 6 - Modeling Abundance with Counts of Unmarked Individuals in Closed Populations: Binomial N-Mixture Models. In M. Kéry & J. A. Royle (Eds.), Applied Hierarchical Modeling in Ecology (pp. 220-312). Boston: Academic Press.
        Pentru script s-a folosit programul R cu pachetele shiny, bslib, DT, ggplot2 și unmarked.</p>,
  
      "
    )
  }
})

# text_rnsim
output$text_rnsimcov <- renderText({
  HTML(
    "
      <p>În partea stângă vom introduce parametrii modelării, adică numărul de zone, numărul de vizite, abundența așteptată la un sit, probabilitatea de detecție, parametrii covariatelor (interceptarea și panta). Puteți realiza modele pentru diferite scenarii (număr mai mic sau mai mare de vizite sau situri, abundență sau detectabilitate mai mare sau mai mică). Astfel, vom putea estima care trebuie să fie intensitatea studiului pentru a obține rezultate robuste. Dacă probabilitatea de detecție asumată este mică (șanse mici să înregistrăm specia la camere) încercați să măriți numărul de vizite.</p>
<ul>
        <li>random seed pentru reproductibilitate. Utilizarea unui număr (de exemplu, 24) de toți utilizatorii asigură obținerea de rezultate asemănătoare dacă valoare parametrilor nu se schimbă. În caz contrar se vor obține rezultate ușor diferite.</li>
        <li>numărul de zone investigate M. Vom introduce numărul de zone în care dorim să punem camere. De exemplu, 100 zone. Explorați modele cu număr mai mare sau mai mic de zone investigate, dar țineți cont de echipamentele de care dispuneți.</li>
        <li>numărul de vizite (J). Este un parametru important, pentru că dacă avem detectabilitate mică trebuie să mergem mai des pe teren. Nu exagerați cu numărul de vizite, și nu lungiți mult studiul. Găsiți un echilibru.</li>
        <li>Parametrii pentru modelare abundență în scară logaritmică: Interceptarea pentru covariata înălțime vegetație (beta0) și Panta pentru înălțime vegetație (beta1) Interceptarea se stabilește de regulă la 0.</li>
        <li>Parametrii pentru modelare detecției. Interceptarea pentru covariata intensitate vânt (alpha0) și Panta pentru ntensitate vânt (alpha1) </li>
      </ul>

      <p>După ce ajustați parametrii rulați modelul. Calculul este intensiv, astfel că trebuie să așteptați 1-2 minute pentru un nou rezultat. Notați rezultatele și găsiți valorile optime pentru voi. În acest model, abundența așteptată lambda este funcție exponențială a beta0 și beta1 [exp(beta0 + beta1 * vegHt)] iar abundența realizată (reală) N este stabilită random dintr-o distribuție Poisson [rpois(M, lambda)].
</p>


    "
  )
})

data_generated_rncov <- reactiveVal(FALSE) # reactive test rn sim cov

observeEvent(input$run_model, {
  # reactive test sim rn cov
  data_generated_rncov(TRUE)
})


output$rnsim_cov_fig <- renderText({
  if (data_generated_rncov()) {
    HTML(
      "
        <p>Figura de sus indică relația dintre abundența așteptată și covariata înălțime vegetație, materializată în abundență reală (N).Cu cât erorile de măsurare sunt mai mari, cu atât valorile individuale sunt dispuse mai aleator și nu se poate observa o tendință clară. Acest lucru poate apărea și pentru că covariata nu influențează abundența. </p>  
 <p> Figura de jos, modelează detecția funcție de intensitatea vântului (scară logaritmică). Observațiile (număr apariții) sunt scalate funcție de valoarea maximă pentru a le face comparabile cu probabilitatea de detecție. </p>
      "
    )
  }
})


output$text_referinta2 <- renderText({
  if (data_generated_rncov()) {
    HTML(
      "
        <p>Simularea a fost adaptată din Kéry, M., & Royle, J. A. (2016). Chapter 6 - Modeling Abundance with Counts of Unmarked Individuals in Closed Populations: Binomial N-Mixture Models. In M. Kéry & J. A. Royle (Eds.), Applied Hierarchical Modeling in Ecology (pp. 220-312). Boston: Academic Press.
        Pentru script s-a folosit programul R cu pachetele shiny, bslib, DT, ggplot2 și unmarked.</p>,
  
      "
    )
  }
})

output$text_referinta3 <- renderText({
  if (data_generated_rncov()) {
    HTML(
      "
        <p>Simularea a fost adaptată din Kéry, M., & Royle, J. A. (2016). Chapter 6 - Modeling Abundance with Counts of Unmarked Individuals in Closed Populations: Binomial N-Mixture Models. In M. Kéry & J. A. Royle (Eds.), Applied Hierarchical Modeling in Ecology (pp. 220-312). Boston: Academic Press.
        Pentru script s-a folosit programul R cu pachetele shiny, bslib, DT, ggplot2 și unmarked.</p>,
  
      "
    )
  }
})

output$text_referinta4 <- renderText({
  if (data_generated_rncov()) {
    HTML(
      "
        <p>Simularea a fost adaptată din Kéry, M., & Royle, J. A. (2016). Chapter 6 - Modeling Abundance with Counts of Unmarked Individuals in Closed Populations: Binomial N-Mixture Models. In M. Kéry & J. A. Royle (Eds.), Applied Hierarchical Modeling in Ecology (pp. 220-312). Boston: Academic Press.
        Pentru script s-a folosit programul R cu pachetele shiny, bslib, DT, ggplot2 și unmarked.</p>,
  
      "
    )
  }
})

output$rnsim_cov_summary <- renderText({
  if (data_generated_rncov()) {
    HTML(
      "
        <p>Cele trei casete indică sumarul modelării N-mixture un sezon, 
        o specie cu covariată pentru abundență (VegHt) și detecție (vânt), 
        cu mixture Poisson (P), Negative bionomial (NB) și Zero-Inflated: 
        pcount(formula = ~wind ~ vegHt, data = umf, mixture = P), 
        pcount(formula = ~wind ~ vegHt, data = umf, mixture = NB) 
        și pcount(formula = ~wind ~ vegHt, data = umf, mixture = ZIP). 
        Cele trei modele sunt comparate după  Akaike information criterion - AIC (predictor al erorilor), cel cu AIC cel mai mic fiind modelul cel mai robust. Observărm că diferența dintre ele nu este foarte mare, cel mai robust model fiind cel Poisson (cu parametrii stabiliți default).</p>
  
      "
    )
  }
})


output$rnsim_cov_plotsum <- renderText({
  if (data_generated_rncov()) {
    HTML(
      "
        <p>Figura prezintă numărul de indivizi obținuți conform modelării N-mixture (linie albastră) funcție de înălțimea vegetației în scară logaritmică și comparația cu rezultatul obținut dintr-un model GLM fără a lua în considerare probabilitatea de detecție (linie neagră). Este reprezentată pentru comparație și numărul real de indivizi (observați). Se constată că GLM fără a lua în considerare probabilitatea de detecție subestimează mult numărul de indivizi la un sit. </p> 
      "
    )
  }
})



}
# Run the Shiny app
shinyApp(ui = ui, server = server)
