library(shiny)
library(bslib)
library(unmarked)
library(shinybusy)
library(shiny.info)

# global variable version
VERSION <- "version 1.0.1. Questions to rlaurentiu@gmail.com"

# UI
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
    tabPanel("home", uiOutput("homeContent")),
    tabPanel("simoccu",
             fluidPage(
               h3("Simulare ocupanță fără covariate în unmarked"),
               sidebarLayout(
                 sidebarPanel(
                   numericInput("seed_input", "Random Seed:", value = 24),
                   numericInput("M_input", "Număr zone (M):", value = 100),
                   numericInput("J_input", "Număr vizite (J):", value = 2),
                   sliderInput(
                     "psi_slider",
                     "Probabilitatea reală de ocupare (psi):",
                     min = 0,
                     max = 1,
                     value = 0.8,
                     step = 0.01
                   ),
                   sliderInput(
                     "p_slider",
                     "Probabilitatea de detecţie (p):",
                     min = 0,
                     max = 1,
                     value = 0.5,
                     step = 0.01
                   ),
                   add_busy_bar(color = "#FF0000"),
                   actionButton("generate_data_btn", "Rulează modelul")
                 ),
                 mainPanel(
                   uiOutput("text_occusim"),
                   textOutput("sum_z_text1"),
                   textOutput("sum_z_text2"),
                   verbatimTextOutput("model_summary"),
                   uiOutput("text_modelsum_sim"),
                   verbatimTextOutput("back_transformed_text"),
                   uiOutput("text_back_transf_sim"),
                   uiOutput("text_referinta1")
                 )
               )
             )),
    tabPanel("simoccucov", fluidPage(
      h3("Simulare ocupanță cu covariate în unmarked"),
      sidebarLayout(
        sidebarPanel(
          numericInput("seed", "Set Seed:", value = 1),
          numericInput("M", "Număr zone (M):", value = 100),
          numericInput("J", "Număr vizite (J):", value = 3),
          numericInput("beta0", "Beta0 (Interceptarea pentru VegHt):", value = 0),
          numericInput("beta1", "Beta1 (Panta pentru covariata vegHt):", value = 3),
          numericInput("alpha0", "Alpha0 (Interceptarea pentru vânt):", value = -2),
          numericInput("alpha1", "Alpha1 (Panta pentru covariata vânt):", value = -3),
          add_busy_bar(color = "#FF0000"),
          actionButton("generate_plots", "Rulează modelul")
        ),
        mainPanel(
          add_busy_bar(color = "#FF0000"),
          uiOutput("text_occusimcov"),
          plotOutput("true_presence_plot"),
          plotOutput("observed_data_plot"),
          uiOutput("text_truobsplots"),
          verbatimTextOutput("null_model_summary"),
          verbatimTextOutput("unmarked_summary"),
          uiOutput("text_summaryoccusimcov"),
          plotOutput("new_graph"),
          uiOutput("graph_truevsLR"),
          uiOutput("text_referinta")
        )
      )
    )),
    tabPanel("despre wildPop", uiOutput("aboutContent"),
             textOutput("deploymentDate")),
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
  data <- reactive({
    req(input$generate_data_btn)
    set.seed(input$seed_input)
    M <- input$M_input
    J <- input$J_input
    y <- matrix(NA, nrow = M, ncol = J)
    
    z <- rbinom(n = M,
                size = 1,
                prob = input$psi_slider)
    
    for (j in 1:J) {
      y[, j] <- rbinom(n = M,
                       size = 1,
                       prob = z * input$p_slider)
    }
    
    umf <- unmarkedFrameOccu(y = y)
    
    list(y = y, z = z, umf = umf)
  })
  
  output$model_summary <- renderText({
    data_df <- data()
    y <- data_df$y
    umf <- data_df$umf
    fm1 <- occu( ~ 1 ~ 1, data = umf)
    summary_text <- capture.output(summary(fm1))
    paste(summary_text, collapse = "\n")
  })
  
  output$back_transformed_text <- renderPrint({
    data_df <- data()
    umf <- data_df$umf
    fm1 <- occu( ~ 1 ~ 1, data = umf)
    back_transformed_state <- backTransform(fm1, "state")
    back_transformed_det <- backTransform(fm1, "det")
    
    cat("Back-Transformed State Estimates (Abundance):\n")
    print(back_transformed_state)
    cat("\nBack-Transformed Detection Estimates (p):\n")
    print(back_transformed_det)
  })
  
  observeEvent(input$generate_plots, {
    set.seed(input$seed)
    M <- input$M
    J <- input$J
    beta0 <- input$beta0
    beta1 <- input$beta1
    alpha0 <- input$alpha0
    alpha1 <- input$alpha1
    
    vegHt <- sort(runif(M, -1, 1))
    psi <- plogis(beta0 + beta1 * vegHt)
    z <- rbinom(M, 1, psi)
    
    wind <- array(runif(M * J, -1, 1), dim = c(M, J))
    p <- plogis(alpha0 + alpha1 * wind)
    
    y <- matrix(NA, nrow = M, ncol = J)
    for (j in 1:J) {
      y[, j] <- rbinom(M, z, p[, j])
    }
    
    time <-
      matrix(rep(as.character(1:J), M), ncol = J, byrow = TRUE)
    A_count <- round(M / J)
    B_count <- round(M / J)
    C_count <- M - A_count - B_count
    
    hab <-
      c(rep("A", A_count), rep("B", B_count), rep("C", C_count))
    
    true_presence_data <- data.frame(vegHt = vegHt, z = z)
    
    observed_data <-
      data.frame(wind = wind, y1 = plogis(alpha0 + alpha1 * wind))
    
    output$true_presence_plot <- renderPlot({
      plot(
        vegHt,
        z,
        xlab = "Vegetation height",
        ylab = "True presence/absence (z)",
        frame = F,
        cex = 1.5
      )
      plot(
        function(x)
          plogis(beta0 + beta1 * x),
        -1,
        1,
        add = T,
        lwd = 3,
        col = "red"
      )
    }, width = 0.8 * session$clientData$output_new_graph_width, height = 400)
    
    output$observed_data_plot <- renderPlot({
      plot(
        wind,
        y,
        xlab = "Wind",
        ylab = "Observed det./nondetection data (y)",
        frame = F,
        cex = 1.5
      )
      plot(
        function(x)
          plogis(alpha0 + alpha1 * x),
        -1,
        1,
        add = T,
        lwd = 3,
        col = "red"
      )
    }, width = 0.8 * session$clientData$output_new_graph_width, height = 400)
    
    umf <- unmarkedFrameOccu(
      y = y,
      siteCovs = data.frame(vegHt = vegHt, hab = hab),
      obsCovs = list(wind = wind, time = time)
    )
    
    umf.null <- unmarkedFramePCount(y = y)
    summary(umf.null)
    
    fm.null <- pcount( ~ 1 ~ 1, data = umf.null)
    
    null_summary <- summary(fm.null)
    
    fm1.occ <- occu( ~ wind ~ vegHt, data = umf)
    
    unmarked_summary <- summary(fm1.occ)
    output$unmarked_summary <- renderPrint({
      unmarked_summary
    })
    
    output$null_model_summary <- renderPrint({
      null_summary
    })
    
    output$new_graph <- renderPlot({
      # Add truth from data simulation
      newdat <- data.frame(vegHt = seq(-1, 1, 0.01))
      pred.occ <- predict(fm1.occ, type = "state", newdata = newdat)
      newdat <- data.frame(wind = seq(-1, 1, 0.1))
      pred.det <- predict(fm1.occ, type = "det", newdata = newdat)
      
      # ... for values of wind of -1 to 1
      newdat <- data.frame(wind = seq(-1, 1, , 5))
      pred.det_append <-
        predict(fm1.occ,
                type = "det",
                newdata = newdat,
                append = TRUE)
      
      # Fit detection-naive GLM to observed occurrence and plot comparison
      summary(fm.glm <-
                glm(apply(y, 1, max) ~ vegHt, family = binomial))
      
      # Create the plot
      par(mfrow = c(1, 1))  # Set the plot layout to 1 row, 1 column
      plot(
        vegHt,
        apply(y, 1, max),
        xlab = "Vegetation height",
        ylab = "Observed occurrence ('ever observed ?')",
        frame = F,
        cex = 1.5
      )
      plot(
        function(x)
          plogis(beta0 + beta1 * x),
        -1,
        1,
        add = T,
        lwd = 3,
        col = "red"
      )
      lines(vegHt,
            predict(fm.glm, , "response"),
            type = "l",
            lwd = 3)
      lines(vegHt,
            predict(fm1.occ, type = "state")[, 1],
            col = "blue",
            lwd = 3)
      legend(
        -1,
        0.9,
        c("Truth", "LR with p", "LR without p"),
        col = c("red", "blue", "black"),
        lty = 1,
        lwd = 3,
        cex = 1.2
      )
    }, width = 0.8 * session$clientData$output_new_graph_width, height = 400)
  })
  
  output$text_occusim <- renderText({
    HTML(
      "
      <p>În partea stângă vom introduce parametrii modelării, adică numărul de zone, numărul de vizite, probabilitatea estimată de ocupanță, probabilitatea de detecție. Puteți realiza modele pentru diferite scenarii (număr mai mic sau mai mare de vizite sau situri, ocupanță sau detectabilitate mai mare sau mai mică). Astfel, vom putea estima care trebuie să fie intensitatea studiului pentru a obține rezultate robuste. Dacă probabilitatea de detecție asumată este mică încercați să măriți numărul de vizite.</p>

      <ul>
        <li>random seed: O valoare pentru inițializarea generării de numere pseudorandom. Dacă vom schimba valoarea, vom obține rezultate ușor diferite la fiecare rulare de model (se vor genera numere random diferite).</li>
        <li>numărul de zone investigate M: Introducem numărul de zone în care montăm camere. De exemplu, 100 pentru 100 de zone. Explorați modele cu număr mai mare sau mai mic de zone investigate, de exemplu pentru a vedea dacă echipamentele de care dispuneți sunt suficiente pentru a obține rezultate robuste.</li>
        <li>numărul de vizite a siturilor: Acesta este un parametru important, deoarece, dacă detectabilitate speciei de interes este mică trebuie să mergem mai des pe teren. Nu exagerați cu numărul de vizite, și nu lungiți mult studiul pentru că populația trebuie să fie închisă.</li>
        <li>probabilitatea estimată de ocupare a siturilor (din literatură sau experiența voastră): Încercați diferite probabilități, adică în cât la % din zone vom găsi specia țintă (0 = 0%, 0.98 = 98%, 1 = 100%).</li>
        <li>probabilitatea de detecție a specie: Din 100 de animale care se găsesc în zonele de studiu, câte vor fi detectate de noi în condiții normale (0 = 0%, 0.98 = 98%, 1 = 100%). Este un parametru care poate varia în realitate de la o zonă la alta, dar vom considera o valoare comună pentru toate siturile.</li>
      </ul>

      <p>După ce ajustați parametrii rulați modelul de mai multe ori. Notați rezultatele și găsiți valorile optime pentru studiul pe care îl proiectați.</p>
    "
    )
  })
  
  # sum z / true occupies sites observed occupied
  
  sum_z1 <- reactive({
    data_df <- data()
    sum(data_df$z)
  })
  
  sum_z2 <- reactive({
    data_df <- data()
    sum(apply(data_df$y, 1, max))
  })
  
  
  output$sum_z_text1 <- renderText({
    sum_z_val1 <- sum_z1()
    paste("Număr real de zone ocupate:", sum_z_val1)
  })
  
  output$sum_z_text2 <- renderText({
    sum_z_val2 <- sum_z2()
    paste("Număr observat de zone ocupate:", sum_z_val2)
  })
  
  
  output$text_modelsum_sim <- renderText({
    if (data_generated()) {
      HTML(
        "
        <p>După rularea modelului vom obține sumarul așa cum rezultă din unmarked (funcția occu). Modelul este occu(formula = ~1 ~ 1, data = umf), adică se modelează doar date de detecție/non detecție, fără covariante. </p>
        <p>Ocupanța și probabilitatea de detecție sunt obținute ca log, astfel că le vom transforma în probabilități cu ajutorul funcției back-transform.</p>
      "
      )
    }
  })
  output$text_back_transf_sim <- renderText({
    if (data_generated()) {
      HTML(
        "
        <p>După ajutare, vom obține o probabilitate de ocupanță (valoarea de sub Estimate Backtransformed linear combination(s) of Occupancy estimate(s) și o probabilitate de detecție (Backtransformed linear combination(s) of Detection estimate(s))). </p>
        <p>După ce ajustați parametrii rulați modelul. Notați rezultatele și găsiți valoarea optimă pentru studiul proiectat.</p>
        <p>Comparați rezultatul de la detecție cu numărul observat de zone ocupate și pe cel de la ocupanță cu valoarea introdusă de noi la probabilitatea reală de ocupare (psi).</p>
      "
      )
    }
  })
  
  data_generated <- reactiveVal(FALSE) # reactive test sim occu
  
  observeEvent(input$generate_data_btn, {
    # reactive test sim occu
    data_generated(TRUE)
  })
  
  output$text_occusimcov <- renderText({
    HTML(
      "
      <p>În general, nu modelăm ocupanța fără a investiga influența unor covariate asupra detecției sau ocupanței. Modelul null testat în pagina simoccu îl folosim
      doar pentru comparație. Vom modela cei doi parametri funcție de o covariată pentru zonă (variază de la zonă la zonă, nu variază între vizite),
      și una pentru vizite (variază de la vizită la vizită). Pentru acest model vom introduce parametri.</p>

      <ul>
        <li>set seed: O valoare pentru inițializarea generării de numere pseudorandom. Dacă vom schimba valoarea, vom obține rezultate ușor diferite la fiecare rulare de model (se vor genera numere random diferite).</li>
        <li>numărul de zone investigate M: Introducem numărul de zone în care montăm camere. De exemplu, 100 pentru 100 de zone. Explorați modele cu număr mai mare sau mai mic de zone investigate, de exemplu pentru a vedea dacă echipamentele de care dispuneți sunt suficiente pentru a obține rezultate robuste.</li>
        <li>numărul de vizite a siturilor: Acesta este un parametru important, deoarece, dacă detectabilitate speciei de interes este mică trebuie să mergem mai des pe teren. Nu exagerați cu numărul de vizite, și nu lungiți mult studiul pentru că populația trebuie să fie închisă.</li>
        <li>beta0 (Interceptarea pentru covariata VegHt). Pentru ca este o regresie putem vom forța interceptarea să plece de la 0. Putem înceca și alte valori de interceptare și urmărim dacă cum se modifică liniile Truth și LR with P (trebuie să fie cât mai apropiate într-un studiu ideal. Modificarea interceptarii si pantei o observam in primul grafic.</li>
        <li>beta1 (Panta pentru covariata vegHt). Valoarea predefinită în acest model este 3, care corespunde numărului de vizite. Modificarea valorii pantei o putem studia in primul grafic.</li>
        <li>alpha0 (Interceptarea) pentru covariata vânt. Valoarea predefinită în acest model este -2. Modificarea interceptării o putem studia in al doilea grafic. Prin el interpretăm influența covariate asupra estimatorului analizat.</li>
        <li>alpha1 (Panta pentru covariata pentru vânt). Valoarea predefinită în acest model este -3. Modificarea valorii pantei o putem studia in al doilea grafic. Prin el interpretăm influența covariate asupra estimatorului analizat.</li>
      </ul>

      <p>După ce ajustăm parametrii rulăm modelul de mai multe ori. Notăm rezultatele și găsim valorile optime pentru studiul pe care îl proiectăm.</p>
    "
    )
  })
  
  # text grafice simoccucov
  
  data_gen_simcov <- reactiveVal(FALSE) # reactive test sim occu cov
  
  observeEvent(input$generate_plots, {
    # reactive tetext sim occu
    data_gen_simcov(TRUE)
  })
  
  
  output$text_truobsplots <- renderText({
    if (data_gen_simcov()) {
      HTML(
        "
        <p>Prima figura indică relația dintre prezența/absența indiviziilor din specia țintă și covariata înălțimea vegetației (linia roșie, scală la 0). Cercurile indică detecția/non detecția reală (înregistrată de camere). Dacă modificăm interceptarea (beta0) și panta (beta1) putem evalua comportamentul modelului. </p>
        <p>Figura a doua indică relația dintre probabilitatea de detecție și intensitatea vântului (linia roșie). Cercurile indică detecția/non detecția observată (înregistrată de camere). Dacă modificăm interceptarea (alpha0) și panta (alpha1) covariatei putem evalua comportamentul modelului.</p>
      "
      )
    }
  })
  
  output$text_summaryoccusimcov <- renderText({
    if (data_gen_simcov()) {
      HTML(
        "
        <p>Prima casetă (de sus) indică sumarul modelului nul (fără covariate pentru ocupanță - state și detecție - det), cea de a doua casetă (de jos) indică modelul cu o covariată pentru ocupanță (înălțimea vegetației) și detecție (intensitate vânt). Comparați rezultatele după rularea cu diferite valori pentru interceptare și pantă. Semnul + la estimate indică o influență pozitivă (de exemplu, ocupanța crește cu înălțimea vegetației). </p>
  
      "
      )
    }
  })
  
  output$graph_truevsLR <- renderText({
    if (data_gen_simcov()) {
      HTML(
        "
        <p>Ultimul grafic compară relația dintre ocupanța observată (înregistrată de camere) și covariata înălțime vegetație (linia roșie - truth) și estimarea rezultată din modelului single-season occupancy cu covariantă (linia albastră, regresie logistică care i-a în calcul detecția imperfectă p, LR with p). S-a mai reprezentat și o modelare tip regresie logistică fără a lua în calcul detecția imperfectă fără p LR without p). Ignorarea detecției imperfecte duce la subestimarea prezenței speciei la siturile analizate (vezi LR without p vs. LR with p) și bias către 0 coeficienților de regresie ai VegHt.
        ) </p>
  
      "
      )
    }
  })
  
  observeEvent(input$text_referinta, {
    # reactive 
    data_gen_simcov(TRUE)
  })
  
  observeEvent(input$text_referinta1, {
    # reactive u
    data_generated(TRUE)
  })
  
  output$text_referinta1 <- renderText({
    if (data_generated()) {
      HTML(
        "
        <p>Simularea a fost adaptată din cartea Kéry, M., & Royle, J. A. (2016). Chapter 10 - Modeling Static Occurrence and Species Distributions Using Site-occupancy Models. In M. Kéry & J. A. Royle (Eds.), Applied Hierarchical Modeling in Ecology (pp. 551-629). Boston: Academic Press.</p>
        <p>Pentru script s-a folosit programul R cu pachetele shiny, bslib și unmarked.</p>
  
      "
      )
    }
  })
  
  output$text_referinta <- renderText({
    if (data_gen_simcov()) {
      HTML(
        "
        <p>Simularea a fost adaptată din Kéry, M., & Royle, J. A. (2016). Chapter 10 - Modeling Static Occurrence and Species Distributions Using Site-occupancy Models. In M. Kéry & J. A. Royle (Eds.), Applied Hierarchical Modeling in Ecology (pp. 551-629). Boston: Academic Press.</p>
        <p>Pentru script s-a folosit programul R cu pachetele shiny, bslib și unmarked.</p>
  
      "
      )
    }
  })
  
  # home page text
  
  output$homeContent <- renderUI({
    fluidPage(
      h3(
        "Aplicație interactivă estimare ocupanță animale sălbatice cu date simulate"
      ),
      p(
        "Estimarea abundenței speciilor constituie una din cele mai dificile și importante aspecte ale managementului speciilor sălbatice. Datele pentru aceste estimări se obțin în principal prin studii de capturare-recaptuare (fiecare individ poate fi identificat) și studii de tip ocupanță (occupancy) sau N-mixture, în care indivizii nu sunt identificați."
      ),
      p(
        "Datele pentru studii de tip ocupanță pot fi obținute prin înregistrarea directă sau indirectă a prezenței (de exemplu, observații directe, camere foto, înregistrare sunete, urme). Studiile care implica identificarea de urme sau inregistrarea imaginilor cu camere foto sunt relativ ușor de pus în practică pe teren, dar pentru a obține date robuste sunt necesare protocoale de prelevare corecte și o analiză statistică a datelor riguroasă."
      ),
      p(HTML(
        "Analiza statistică poate fi realizată folosind metode complexe (de exemplu în <i>programul R</i>, prin pachetul <i>unmarked</i>), dar sunt necesare cunoștințe solide de programare precum și o înțelegere foarte bună a teoriei analizei ierarhice a datelor populaționale."
      )),
      p(
        "Modelele ierarhice se bazează pe principiul că datele ecologice se pot analiza ca două procese interdependente. Primul, procesul ecologic, determină starea adevărată a sistemului analizat, cum ar fi ocupanța sau abundența reală unei specii. Acesta este un factor critic, deoarece reprezintă scenariul real studiat. Al doilea, procesul de observare, este cel care influențează datele culese în timpul sondajelor. Acest proces depinde în mod inerent de procesul ecologic, deoarece ceea ce este observat și înregistrat este dependent de starea reală a mediului."
      ),
      p(
        HTML("Considerând aceste două procese putem realiza două tipuri simple de analiză: <b>ocupanță</b> și <b>N-mixture</b>.")),
      
      p(HTML("Primul tip este reprezentat de <b>modelele de ocupanță</b>, orientate spre evaluarea prezenței speciilor, nu a numărului de indivizi. Ele oferă un cadru pentru estimarea probabilității ca o specie să fie prezentă, încorporând probabilitățile de detectare a speciei în calculele lor.
        "
      )),
      p(HTML(
        "Al doilea tip este reprezentat de <b>modelele N-mixture</b>, care sunt utile pentru estimarea abundenței speciilor. Aceste modele sunt eficiente în abordarea variațiilor naturale legate de numărul speciilor și sunt dezvoltate pentru a integra posibilele erori de detectare. Acest lucru le face potrivite pentru studii în care estimările precise ale numărului de specii sunt importante, fără a se putea identifica indivizii unici.
"
      )),
      p(
        "Un aspect cheie a modelelor ierarhice este dependența lor de datele din monitorizări replicate temporal efectuate pe mai multe site-uri. Înregistrările repetate îmbunătățesc robustețea modelelor și gradul de încredere în rezultate.
        "
      ),
      p(
        HTML("Pentru a întelege comportamentul acestor modele, în <b>pagina simoccu</b> (simulare modelare ocupanță) vom explora rezultatele unui model simplu de ocupanță iar în <b>pagina simoccucov</b> îl vom extinde cu covariate care explică ocupanța și probabilitatea de detecție. Modelele de ocupanță au câteva constrângeri de care trebuie să ținem seamă când proiectăm un studiu:
      "
        )),
      tags$ul(
        tags$li(HTML(
          "<i>populația analizată este închisă</i>, adică perioada de studiu este suficient de scurtă pentru a considera că nu există schimbări numerice în populație
              de la începutul pănă la sfârșitul studiului. Această condiție ne asigură că prelevăm date din aceeași populație de studiu."
        )),
        tags$li(HTML(
          "<i>datele nu conțin observații fals-pozitive</i>. Dacă nu suntem siguri de specia înregistrată, mai degrabă renunțăm la aceea înregistrare."
        )),
        tags$li(HTML(
          "<i>observațiile din zonele de prelevare sunt independente unele de altele</i>. Adică, de exemplu, nu există șanse mari ca un urs de la situl A să fie înregistrat și la situl B pe parcursul studiului."
        )),
        tags$li(HTML(
          "<i>detecția este omogenă între situri</i>, adică nu există variabilitate de mediu sau alte elemente care schimbă condițiile de observare. Dacă nu, va trebui să includem un model cu covariată (cel mai adesea variabilitatea este mare, deci covariatele pot explica detecția sau ocupanța)."
        ))
      ),
      tags$a(
        href = "https://wildpop.ccmesi.ro/", "https://wildpop.ccmesi.ro"
      ),
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
        "Estimarea abundenței speciilor constituie unul din cele mai dificile și importante aspecte ale managementului speciilor sălbatice. 
        Datele pentru aceste estimări se obțin în principal prin studii de capturare-recaptuare (fiecare individ poate fi identificat) și studii de tip 
        ocupanță și N-mixture, în care indivizii nu sunt identificați. Datele pentru studii de tip occupancy pot fi obținute prin înregistrarea urmelor 
        sau camere foto. Studiile care implica identificarea de urme sau inregistrarea imaginilor cu camere foto sunt relativ ușor de implementat, dar 
        pentru a obține date robuste sunt necesare protocoale de prelevare corecte și o analiză statistică a datelor riguroasă. Analiza statistică 
        poate fi realizată folosind metode complexe (de exemplu în R, prin pachetul unmarked), dar sunt necesare cunoștințe solide de programare 
        precum și o înțelegere foarte bună a teoriei analizei ierahizatoare a datelor populaționale. Scopul proiectului WildPop este să accesibilizeze 
        implementarea unor astfel de modele de analiză și să îmbunătățească estimările demografice ale populațiilor speciilor de animale sălbatice 
        din România, prin dezvoltarea, testarea și punerea la dispoziție aplicației interactive de evaluarea a abundenței și distribuției 
        animalelor sălbatice care să poată utiliza date obținute de cercetători și administratorii fondurilor de vânătoare. Pentru atingerea acestui scop, 
        s-au evaluat studii de monitorizare a speciilor, fiind identificate punctele tari și punctele slabe din perspectiva 
        fiabilității estimărilor demografice rezultate prin aplicarea de modele de tip occupancy. De asemenea am realizat un ghid de implementare 
        a acestor modele. 
        Astfel, practicienii administrării fondurilor de vânătoare, ecologii și biologii 
        au la dispoziție un cadru eficient de planificare și implementare a unor studii de monitorizare, care să reducă costurile, crească 
        robustețea datelor și producă date pentru fundamentarea de măsuri eficiente pentru conservarea diversității biologice."
      ),
      
      
      tags$p("Obiectivele proiectului sunt:"),
      tags$ul(
        tags$li(
          "O1: Evaluarea inițiativelor din România de estimare a abundenței sau ocupanței animalelor sălbatice, urmărind în principal inițiativele dedicate speciilor de carnivore mari de interes comunitar (urs, lup și râs);"
        ),
        tags$li(
          "O2: Realizarea în parteneriat cu actori locali a unui ghid de monitorizare eficientă și robustă a evoluției abundenței sau ocupanței animalelor sălbatice folosind date de prezență a indivizilor nemarcați;
"        ),
        tags$li(
          "O3: Dezvoltarea unei aplicații interactive pentru estimarea abundenței sau ocupanței animalelor sălbatice utilizând pachetul R Shiny;"
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

}

# Run the Shiny app
shinyApp(ui = ui, server = server)