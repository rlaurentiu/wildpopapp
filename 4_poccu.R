library(shiny)
library(bslib)
library(MuMIn)
library(DT)
library(AICcmodavg)
library(ggplot2)
library(tidyr)
library(unmarked)
library(shinybusy)
library(shiny.info)



# global variable version
VERSION <- "version 1.0.1. Questions to rlaurentiu@gmail.com"


# Define the path to the predefined data file on the server
predefinedFile_nmixt <- "/srv/connect/apps/nmixture/plagionotus_detritus.csv"

ui <- fluidPage(
  tags$head(
    tags$style(HTML("
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
    "))
  ),
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
    tabPanel("Nmixture", fluidPage(h3("Modelare N-mixture (abundență) o specie un sezon cu sau fără covariate")),
             sidebarLayout(
               sidebarPanel(
                 verbatimTextOutput("selectedFile_nmixt"),
                 radioButtons("dataSource_nmixt", "Date pentru analiză",
                              choices = list("Incarcă fişier propriu" = "upload_nmixt", "Foloseşte datele noastre" = "predefined_nmixt"),
                              inline = TRUE),
                 # File input to upload data
                 fileInput("dataFile_nmix", "Încarcă un fişier csv dacă nu foloseşti datele noastre. Verifică în manual structura necesară"),
                 
                 # Dynamic UI for selecting detection history columns
                 uiOutput("yColumnsSelector_nmix"),
                 
                 # Dynamic UI for selecting site covariates columns
                 uiOutput("siteCovsColumnsSelector_nmix"),
                 
                 # Dynamic UI for selecting observation covariates columns
                 uiOutput("obsCovsColumnsSelector_nmix")
               ),
               
               mainPanel(tabsetPanel(
                 tabPanel("Verifică datele încărcate",
                          DTOutput("inputDataTable_nmixt")),
                 tabPanel(
                   "Ierarhizează modelele",
                   uiOutput("expl_selectnmixture"),
                   add_busy_bar(color = "#FF0000"),
                   actionButton("runModelComparison_nmixt", "Compara modelele nul după AIC"),
                   DTOutput("modelComparisonOutput_nmixt"),
                   selectInput("selectedMixture_nmixt", "Selectează distribuție", choices = c("P", "NB", "ZIP")),
                   uiOutput("text_sel_met"),
                   selectInput(
                     "modelType_nmix",
                     "Selectează tipul de modelare:",
                     choices = c(
                       "Model fără covariate (model nul)",
                       "Modele cu covariate pentru situri de prelevare",
                       "Modele cu covariate pentru vizite",
                       "Modele cu covariate pentru situri şi vizite"
                     )
                   ),
                   add_busy_bar(color = "#FF0000"),
                   actionButton("runModelsButton_nmix", "Rulează modelarea"),
                   verbatimTextOutput("umfSummary_nmix"),
                   verbatimTextOutput("modelSummaries_nmix"),
                   uiOutput("text_umf_summaries")
                   
                 ),
                 tabPanel(
                   "Rezultate modelare",
                   verbatimTextOutput("modelSummariesAfterDredge_nmixt"),
                   selectInput("selectedModel_nmixt", "Selectează model:", choices = NULL),
                   uiOutput("expl_which_model"),
                   verbatimTextOutput("selectedModelOutput_nmix"),
                   uiOutput("expl_model_output"),
                 ),
                 # tab for Probability Plots
                 tabPanel(
                   "Grafice probabilităţi abundență",
                   plotOutput("numericObsCovsPlots_nmix"), # Plot for numeric obsCovs
                   plotOutput("numericSiteCovsPlots_nmix"),
                   uiOutput("expl_grafice_probab")
                 ),
                 tabPanel(
                   "Predicţii calculate",
                   plotOutput("detHistOutput_nmix"),
                   plotOutput("lambdaHistOutput"),
                   verbatimTextOutput("overallEstimates_nmix"),
                   uiOutput("expl_predictii")
                 )
               )
               ))
    ),
    tabPanel("despre wildPop", uiOutput("aboutContent"))
  ),
  
  # Footer
  tags$footer(
    style = "text-align: center; padding-top: 10px; padding-bottom: 10px;",
    HTML(paste("&copy; CCMESI", format(Sys.Date(), "%Y"), "WildPop. PN-III-P2-2.1-PED-2021-1965 Interactive tool for estimating abundance of wildlife populations"))
  )
)

server <- function(input, output, session) {
  # Create a reactiveVal for holding data
  data_reactive_nmixt <- reactiveVal()
  models_reactive_nmixt <- reactiveVal()
  obsCovs_columns_nmixt <- NULL # Declare obsCovs_columns_nmixt outside of observeEvent
  detpred_plots_nmixt <- reactiveVal() # Create reactiveVal for detpred_plots_nmixt
  lambdapred_plots <- reactiveVal()
  selectedFileName_nmixt <- reactiveVal("")  # ReactiveVal to hold the selected file name
  
  # Observe changes in the uploaded data file
  observe({
    req(input$dataSource_nmixt)
    if (input$dataSource_nmixt == "upload_nmixt") {
      req(input$dataFile_nmix)
      data_reactive_nmixt(read.csv(input$dataFile_nmix$datapath, header = TRUE))
      # Update the selected file name if uploaded
      selectedFileName_nmixt(input$dataFile_nmix$name)
    } else if (input$dataSource_nmixt == "predefined_nmixt") {
      # Load predefined file from the server
      data_reactive_nmixt(read.csv(predefinedFile_nmixt, header = TRUE))
      # Get the name of the predefined file
      fileName_nmixt <- basename(predefinedFile_nmixt)
      # Update the selected file name if predefined
      selectedFileName_nmixt(fileName_nmixt)
    }
  })
  
  # Render the selected file name
  output$selectedFile_nmixt <- renderText({
    paste("Nume fişier selectat", selectedFileName_nmixt())
  })
  # Define UI for selecting detection history columns
  output$yColumnsSelector_nmix <- renderUI({
    req(data_reactive_nmixt())
    
    label_with_tooltip_nmix <- HTML(
      '<div class="tooltip-container">
       <span class="tooltip-label"><b>Selectaţi coloanele cu abundența per sit:</b></span>
       <div class="tooltip-text"> <span style="font-size: 80%;">
         Selectaţi coloanele cu vizite, unde 0 este animal nedetectat şi >0 reprezintă numărul de detecții (abundența naivă). Se pot include celule fără date, NA</span>
       </div>
     </div>'
    )
    
    div(
      label_with_tooltip_nmix,
      checkboxGroupInput(
        "yColumns_nmixt",
        label = NULL,
        choices = names(data_reactive_nmixt()),
        inline = TRUE
      )
    )
  })
  
  # Define UI for selecting site covariates columns
  output$siteCovsColumnsSelector_nmix <- renderUI({
    req(data_reactive_nmixt())
    available_site_covs <- setdiff(names(data_reactive_nmixt()), input$yColumns_nmixt)
    
    label_with_tooltip_nmix <- HTML(
      '<div class="tooltip-container">
     <span class="tooltip-label"><b>Selectaţi covariate pentru situri:</b></span>
     <div class="tooltip-text"> <span style="font-size: 80%;">
       Selectaţi coloanele cu covariatele pentru situri. Nu trebuie să aveţi celule fără date sau cu NA. Recomandăm max 3 covariate. 
       Dacă nu doriţi sa lucraţi cu astfel de covariate nu selectaţi coloane şi alegeţi ulterior modelare fără covariate pentru situri</span>
     </div>
   </div>'
    )
    
    div(
      label_with_tooltip_nmix,
      checkboxGroupInput(
        "siteCovsColumns_nmix",
        label = NULL,
        choices = available_site_covs,
        selected = character(0),
        inline = TRUE
      )
    )
  })
  
  # Define UI for selecting observation covariates columns
  output$obsCovsColumnsSelector_nmix <- renderUI({
    req(data_reactive_nmixt())
    available_obs_covs <- setdiff(names(data_reactive_nmixt()), union(input$yColumns_nmixt, input$siteCovsColumns_nmix))
    
    label_with_tooltip_nmix <- HTML(
      '<div class="tooltip-container">
     <span class="tooltip-label"><b>Selectaţi covariate pentru observaţii:</b></span>
     <div class="tooltip-text"> <span style="font-size: 80%;">
       Selectaţi coloanele cu covariatele pentru vizite. Puteţi încărca o singură covariată, care este compusă din un număr de coloane egal cu numărul de vizite. 
       Nu trebuie să aveţi celule fără date sau cu NA.
       Dacă nu doriţi sa lucraţi cu astfel de covariate nu selectaţi coloane şi alegeţi ulterior modelare fără covariate pentru vizite</span>
     </div>
   </div>'
    )
    
    div(
      label_with_tooltip_nmix,
      checkboxGroupInput(
        "obsCovsColumns_nmix",
        label = NULL,
        choices = available_obs_covs,
        selected = character(0),
        inline = TRUE
      )
    )
  })
  
  observeEvent(input$runModelsButton_nmix, {
    req(input$yColumns_nmixt)
    data <- data_reactive_nmixt()
    
    if (!all(is.na(data[, input$yColumns_nmixt]) | (data[, input$yColumns_nmixt] %% 1 == 0 & data[, input$yColumns_nmixt] >= 0))) {
      return(showNotification("Detecţii non-numerice (nu aveți doar NA sau un număr întreg >= 0). Corectaţi datele de intrare.", type = "error"))
    }
    
    y_nmix <- as.matrix(data[, input$yColumns_nmixt])
    
    siteCovs_nmix <- NULL
    if (length(input$siteCovsColumns_nmix) > 0) {
      selected_site_covs_nmix <- data[, input$siteCovsColumns_nmix, drop = FALSE]
      siteCovs_numeric_nmix <- scale(selected_site_covs_nmix[sapply(selected_site_covs_nmix, is.numeric)])
      colnames(siteCovs_numeric_nmix) <- paste0("scale.", colnames( siteCovs_numeric_nmix))
      siteCovs_nmix <- cbind(selected_site_covs_nmix[sapply(selected_site_covs_nmix, Negate(is.numeric))],  siteCovs_numeric_nmix)
      colnames(siteCovs_nmix)<- paste0("SC.", colnames(siteCovs_nmix))
    }
    
    obsCovs_list_nmix <- NULL
    if (length(input$obsCovsColumns_nmix) > 0) {
      selected_obs_covs <- data[, input$obsCovsColumns_nmix, drop = FALSE]
      obsCovs_numeric <- selected_obs_covs[sapply(selected_obs_covs, is.numeric)]
      
      if (ncol(obsCovs_numeric) > 0) {
        colnames(obsCovs_numeric) <- paste0("_scale", colnames(obsCovs_numeric))
        obsCovs_list_nmix <- list("obsCovs1_scaled" = scale(obsCovs_numeric))
      } else {
        obsCovs_list_nmix <- list("obsCovs1" = selected_obs_covs)
      }
    }
  
     global_model_formula_nmixt <- NULL
    if (input$modelType_nmix == "Model fără covariate (model nul)") {
      umf_nmixt <- unmarkedFramePCount(y = y_nmix)
      global_model_formula_nmixt <- as.formula("~ 1 ~ 1")
    } else if (input$modelType_nmix == "Modele cu covariate pentru situri de prelevare" && !is.null(siteCovs_nmix)) {
      umf_nmixt <- unmarkedFramePCount(y = y_nmix, siteCovs = siteCovs_nmix)
      global_model_formula_nmixt <- as.formula(paste("~ 1 ~ ", paste(names(siteCovs_nmix), collapse = " + ")))
    } else if (input$modelType_nmix == "Modele cu covariate pentru vizite" && !is.null(obsCovs_list_nmix)) {
      obs_covariate_name_nmix <- names(obsCovs_list_nmix)[1]
      umf_nmixt <- unmarkedFramePCount(y = y_nmix, obsCovs = obsCovs_list_nmix)
      global_model_formula_nmixt <- as.formula(paste("~", obs_covariate_name_nmix, "~ 1"))
    } else if (input$modelType_nmix == "Modele cu covariate pentru situri şi vizite" && !is.null(siteCovs_nmix) && !is.null(obsCovs_list_nmix)) {
      obs_covariate_name_nmix <- names(obsCovs_list_nmix)[1]
      umf_nmixt <- unmarkedFramePCount(y = y_nmix, siteCovs = siteCovs_nmix, obsCovs = obsCovs_list_nmix)
      global_model_formula_nmixt <- as.formula(paste("~", obs_covariate_name_nmix, "~ ", paste(names(siteCovs_nmix), collapse = " + ")))
    } else {
      return(showNotification("Nu aţi selectat covariate potrivite pentru modelul selectat. Verificaţi selecţia covariatelor şi reluaţi analiza. Verificați manual pentru alte posibile erori.", type = "error"))
    }
    
    umf_summary_nmixt <- capture.output({ summary(umf_nmixt) })
    output$umfSummary_nmix <- renderPrint({ cat(umf_summary_nmixt, sep = "\n") })
    
    global_model_nmxit <- pcount(global_model_formula_nmixt, umf_nmixt, mixture = input$selectedMixture_nmixt)
    
     model_summary_nmixt <- capture.output({ summary(global_model_nmxit) })
    output$modelSummaries_nmix <- renderPrint({ cat( model_summary_nmixt, sep = "\n") })
    
    m1_nmix <- MuMIn::dredge(global_model_nmxit, rank = "AICc", trace = TRUE)
    models_reactive_nmixt(m1_nmix)
    
    modelIDs_nmixt <- paste0("model clasat pe locul #", 1:nrow(m1_nmix))
    updateSelectInput(session, "selectedModel_nmixt", choices = modelIDs_nmixt)
    
    output$modelSummariesAfterDredge_nmixt <- renderPrint({
      cat("Ierarhizarea modelelor după AICc (cu MuMIn::dredge):\n")
      print(m1_nmix)
    })
  })
  
  observeEvent(input$selectedModel_nmixt, {
    req(models_reactive_nmixt(), input$selectedModel_nmixt)
    selected_model_index <- as.numeric(gsub("\\model clasat pe locul #", "", input$selectedModel_nmixt))
    selected_model_nmix <- MuMIn::get.models(models_reactive_nmixt(), subset = selected_model_index)
    
    output$selectedModelOutput_nmix <- renderPrint({
      summary_text <- capture.output({ summary(selected_model_nmix[[1]]) })
      cat("Sumarul modelului selectat:\n")
      cat(summary_text, sep = "\n")
    })
    
    # Use selected_model_nmix[[1]] for predictions
    detpred_plots_data1_nmixt <- predict(selected_model_nmix[[1]], type = "det", appendData = TRUE)
    detpred_plots_data_nmixt <- data.frame(detpred_plots_data1_nmixt) # Convert to data frame
    detpred_plots_nmixt(detpred_plots_data_nmixt)
    
    lambdapred_plots_data1_nmixt <- predict(selected_model_nmix[[1]], type = "state", appendData = TRUE)
    lambdapred_plots_data_nmixt <- data.frame(lambdapred_plots_data1_nmixt) # Convert to data frame
    lambdapred_plots(lambdapred_plots_data_nmixt)
    
    truabund <- sum(bup(ranef(selected_model_nmix[[1]], stat = "mode")))
    
    output$lambdaHistOutput <- renderPlot({ hist(lambdapred_plots_data_nmixt$Predicted) })
    
    output$detHistOutput_nmix <- renderPlot({ hist(detpred_plots_data_nmixt$Predicted) })
    
    output$overallEstimates_nmix <- renderPrint({
      cat("Estimare abundenţă medie: ", mean(lambdapred_plots_data1_nmixt$Predict), "\n")
      #cat("Estimare probabilitate detecţie pentru întreg studiul: ", mean(detpred_plots_data1_nmixt$Predict), "\n")
      cat("Abundenţă situri: ", truabund, "\n")
    })
    
    # Search for columns starting with "obsCovs1" in detpred_plots_nmixt
    req(detpred_plots_nmixt())
    obsCovs_columns_nmixt <- grep("^obsCovs1", colnames(detpred_plots_nmixt()), value = TRUE)
    if (length(obsCovs_columns_nmixt) > 0) {
      numeric_obsCovs_nmix <- sapply(detpred_plots_nmixt()[, obsCovs_columns_nmixt], is.numeric)
      numeric_obsCovs_columns <- obsCovs_columns_nmixt[numeric_obsCovs_nmix]
      if (length(numeric_obsCovs_columns) > 0) {
        
        output$numericObsCovsPlots_nmix <- renderPlot({
          plot_data_nmixt <- detpred_plots_nmixt()
          plot_data_nmixt$Predicted <- as.numeric(plot_data_nmixt$Predicted)
          
          if (length(obsCovs_columns_nmixt) > 0) {
            obsCovs_data_nmixt <- plot_data_nmixt[, obsCovs_columns_nmixt, drop = FALSE]
            obsCovs_data_long_nmix <- tidyr::gather(obsCovs_data_nmixt, key = "Variable", value = "Value")
            
            # Repeat 'Predicted' column to match the number of rows
            obsCovs_data_long_nmix$Predicted <- rep(plot_data_nmixt$Predicted, each = nrow(obsCovs_data_long_nmix) / nrow(plot_data_nmixt))
            
            # Check if all columns are numeric
            if (all(sapply(obsCovs_data_long_nmix$Value, is.numeric))) {
              # If all columns are numeric, create scatter plots
              p_nmix <- ggplot(obsCovs_data_long_nmix, aes(x = Value, y = Predicted)) +
                geom_point(alpha = 0.7) +
                geom_smooth(method = "glm", se = TRUE) +
                labs(
                  x = "Covariată observaţii (numeric, valori scalate)",
                  y = "Probabilitate detecţie"
                ) + theme_minimal()
              
              print(p_nmix)
            } else {
              # If there are factor columns, create boxplots
              p_nmix <- ggplot(obsCovs_data_long_nmix, aes(x = Variable, y = Predicted)) +
                geom_boxplot() +
                labs(
                  x = "Covariată observaţii (factor)",
                  y = "Probabilitate detecţie"
                ) + theme_minimal()
              
              print(p_nmix)
            }
          }
        })
        
      }
    }
  })
  
  output$numericSiteCovsPlots_nmix <- renderPlot({
    req(lambdapred_plots())
    plot_dataSC <- lambdapred_plots()
    # Identify columns starting with "SC"
    sc_columns <- grep("^SC.", names(plot_dataSC), value = TRUE)
    
    # Create an empty list to store plots
    plots_nmix <- list()
    
    # Loop through SC columns and create plots_nmix
    if (length(sc_columns) > 0) {
      for (sc_col in sc_columns) {
        # Determine if the column is numeric or factor
        if (is.numeric(plot_dataSC[[sc_col]])) {
          # Create scatter plot for numeric data
          p_nmix <- ggplot(plot_dataSC, aes_string(x = sc_col, y = "Predicted")) +  # Updated 'predicted' here
            geom_point(alpha = 0.7) +
            geom_smooth(method = "glm", se = TRUE) +
            labs(x =sc_col, y = "Abundenţă") +  # Updated 'predicted' here
            theme_minimal() +
            facet_wrap(~ .)
        } else if (is.factor(plot_dataSC[[sc_col]])) {
          # Create boxplot for factor data
          p_nmix <- ggplot(plot_dataSC, aes_string(x = sc_col, y = "Predicted")) +  # Updated 'predicted' here
            geom_boxplot() +
            labs(x = sc_col, y = "Abundenţă") +  # Updated 'predicted' here
            theme_minimal() +
            facet_wrap(~ .)
        }
        
        # Add plot to the list
        plots_nmix[[length(plots_nmix) + 1]] <- p_nmix
      }
      
      # Combine plots_nmix into a single grid
      gridExtra::grid.arrange(grobs = plots_nmix, ncol = 2)
    }
  })
  
  
  # Render the Input Data table
  output$inputDataTable_nmixt <- renderDT({
    req(data_reactive_nmixt(), input$yColumns_nmixt)
    
    selected_columns_nmix <- c(input$yColumns_nmixt)
    
    if (!is.null(input$siteCovsColumns_nmix)) {
      selected_columns_nmix <- c(selected_columns_nmix, input$siteCovsColumns_nmix)
    }
    
    if (!is.null(input$obsCovsColumns_nmix)) {
      selected_columns_nmix <- c(selected_columns_nmix, input$obsCovsColumns_nmix)
    }
    
    datatable(data_reactive_nmixt()[1:10, selected_columns_nmix], options = list(paging = TRUE), rownames = FALSE)
  })
  
  
  # home page text
  
  output$homeContent <- renderUI({
    fluidPage(
      h3(
        "Aplicație interactivă abundență/ocupanță animale sălbatice"
      ),
      p(
        "Estimarea abundenței speciilor constituie una din cele mai dificile și importante aspecte ale managementului speciilor sălbatice.
        Datele pentru aceste estimări se obțin în principal prin studii de capturare-recaptuare (fiecare individ poate fi identificat) și studii de
        tip ocupanță, în care indivizii nu trebuie identificați."
      ),
      p(
        "Datele pentru studii de tip ocupanță pot fi obținute prin înregistrarea directă sau indirectă a prezenței
        (de exemplu, observații directe, camere foto, înregistrare sunete, urme). Studiile care implica identificarea de urme sau înregistrarea imaginilor
        cu camere foto sunt relativ ușor de pus în practică pe teren, dar pentru a obține date robuste sunt necesare protocoale de prelevare corecte și o
        analiză statistică a datelor riguroasă."
      ),
      p(
        HTML(
          "Analiza statistică poate fi realizată folosind metode complexe (de exemplu în <i>programul R</i>, prin pachetul <i>unmarked</i>), dar sunt necesare
        cunoștințe solide de programare precum și o înțelegere foarte bună a teoriei analizei ierarhice a datelor populaționale."
        )
      ),
      p(
        "Modelele ierarhice se bazează pe principiul că datele ecologice se pot analiza ca două procese interdependente.
        Primul, procesul ecologic, determină starea adevărată a sistemului analizat, cum ar fi ocupanța sau abundența reală unei specii.
        Acesta este un factor critic, deoarece reprezintă scenariul real studiat. Al doilea, procesul de observare, este cel care influențează datele culese
        în timpul sondajelor. Acest proces depinde în mod inerent de procesul ecologic, deoarece ceea ce este observat și înregistrat este dependent de
        starea reală a mediului."
      ),
      p(
        HTML(
          "Considerând aceste două procese putem realiza două tipuri simple de analiză: <b>ocupanță</b> și <b>N-mixture</b>."
        )
      ),
      
      p(
        HTML(
          "Primul tip este reprezentat de <b>modelele de ocupanță</b>, orientate spre evaluarea prezenței speciilor, 
          nu a numărului de indivizi. Ele oferă un cadru pentru estimarea probabilității ca o specie să fie prezentă, încorporând probabilitățile de detectare a 
          speciei în calculele lor.
        "
        )
      ),
      p(
        HTML(
          "Al doilea tip, pe care îl rulăm din această aplicație este reprezentat de <b>modelele N-mixture</b>, care sunt utile pentru estimarea abundenței speciilor. Aceste modele sunt eficiente în 
          abordarea variațiilor naturale legate de numărul speciilor și sunt dezvoltate pentru a integra posibilele erori de detectare. Acest lucru le face potrivite 
          pentru studii în care estimările precise ale numărului de specii sunt importante, fără a se putea identifica indivizii unici.
"
        )
      ),
      p(
        "Un aspect cheie a modelelor ierarhice este dependența lor de datele din monitorizări replicate temporal efectuate la mai multe site-uri. 
        Înregistrările repetate îmbunătățesc robustețea modelelor și gradul de încredere în rezultate.
        "
      ),
      p(
        HTML(
          "Din această aplicație vom putea rula modele N-mixture de tip un sezon, o singură specie cu sau fără covariate pentru vizite și situri. Pentru această accesați <b>pagina NMixture</b>. 
          Acest model, dezvoltat de MacKenzie et al. (2002), este rulat prin intermediul pachetului R <i>unmarked</i> și are câteva constrângeri de care trebuie să ținem seama 
          când proiectăm un studiu:
      "
        )
      ),
      tags$ul(
        tags$li(
          HTML(
            "<i>populația analizată este închisă</i>, adică perioada de studiu este suficient de scurtă pentru a considera că nu există schimbări numerice în populație
              de la începutul pănă la sfârșitul studiului. Această condiție ne asigură că prelevăm date din aceeași populație de studiu."
          )
        ),
        tags$li(
          HTML(
            "<i>datele nu conțin observații fals-pozitive</i>. Dacă nu suntem siguri de specia înregistrată, mai degrabă renunțăm la aceea înregistrare."
          )
        ),
        tags$li(
          HTML(
            "<i>observațiile din zonele de prelevare sunt independente unele de altele</i>. Adică, de exemplu, nu există șanse mari ca un urs de la situl A să fie înregistrat și la situl B pe parcursul studiului."
          )
        ),
        tags$li(
          HTML(
            "<i>detecția este omogenă între situri</i>, adică nu există variabilitate de mediu sau alte elemente care schimbă condițiile de observare. 
            Dacă nu, va trebui să includem un model cu covariată (cel mai adesea variabilitatea este mare, deci covariatele pot explica detecția sau ocupanța)."
          )
        )
      ),
      p(
        HTML(
          "Aplicaţia este programată în R 4.3.2 cu pachetele shiny, unmarked, MuMIn, AICcmodavg, DT, ggplot2, tidyr, dplyr, bslib şi shinybusy 
          (Kellner et al. 2023, Bartoń 2024, Chang et al. 2024, Mazerolle 2024, Meyer and Perrier 2024, R Core Team 2024, Sievert et al. 2024, Xie et al. 2024).
      "
        )),
      tags$a(href = "https://wildpop.ccmesi.ro/", "https://wildpop.ccmesi.ro"),
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
  
  # Model comparison using MuMIn
  observeEvent(input$runModelComparison_nmixt, {
    req(input$yColumns_nmixt)
    data1_mixt <- data_reactive_nmixt()
    
    if (!all(is.na(data1_mixt[, input$yColumns_nmixt]) | (data1_mixt[, input$yColumns_nmixt] %% 1 == 0 & data1_mixt[, input$yColumns_nmixt] >= 0))) {
      return(showNotification("Detecţii non-numerice (nu este doar NA sau un număr întreg >= 0). Corectaţi datele de intrare.", type = "error"))
    }
    
    y <- as.matrix(data1_mixt[, input$yColumns_nmixt])
    
    countData <- data1_mixt[, input$yColumns_nmixt, drop = FALSE]
    umf_mixt <- unmarkedFramePCount(y = countData)
    
    models_m <- list()
    for (mixture_nmix in c("P", "NB", "ZIP")) {
      models_m[[mixture_nmix]] <- pcount(~ 1 ~ 1, data = umf_mixt, mixture = mixture_nmix)
    }
    
    modelComparison_nmixt <- model.sel(models_m)
    aicValues_m <- as.data.frame(modelComparison_nmixt)
    output$modelComparisonOutput_nmixt <- renderDT({ aicValues_m })
  })
  
  # texte reactive
  
  modelling_yes_poccu <- reactiveVal(FALSE) # reactive text
  modelling_yes_poccu2 <- reactiveVal(FALSE) # reactive text
  
  observeEvent(input$runModelComparison_nmixt, {
    # reactive text description
    modelling_yes_poccu(TRUE)
  })
  
  observeEvent(input$runModelsButton_nmix, {
    # reactive text description
    modelling_yes_poccu2(TRUE)
  })
  
  
  output$text_umf_summaries <- renderText({
    if (modelling_yes_poccu2()) {
      HTML("
        <p> Casetele de mai sus prezintă sumarul datelor introduse în modelul N-mixture o singură specie, un singur sezon, așa cum sunt ele
         integrate în formatul pachetului R unmarked. Sumarul oferă informații privind numărul de zone analizate, numărul de înregistrări ale animalelor, 
         statistică descriptivă a covariatelor la nivel de sit (covariate pentru ocupanță) și la sit (covariate pentru detecție).
         Acestea vor apărea dacă le selectăm în pasul anterior. Dacă apare abrevierea scaled, atunci covariate au fost scalate pentru comparație.
         Aplicația scalează automat variabilele numerice. Variabilele factor sunt introduse fără prelucrare.
         <br> 
          Sumarul unmarked se verifică pentru a depista eventualele erori din datele introduse.
          <br> 
          Următoarea casetă indică rezultatul modelului global (toate covariatele), pentru abundență (abundance) și
        detecție (detection). Estimatorii pentru aceste ocupanță și abundență includ interceptarea (variația abundență sau detecției fără 
        influența covariatei) și valorile pentru covariatele selectate. Ele vor apărea doar dacă se selectează covariate. Estimatorii nu se
        pot interpreta direct deoarece sunt în scara logaritmică sau logit. Din tab-ul următor vom rula mai multe modele, îl vom selecta pe cel mai adecvat pentru
        întrebările de cercetare pe care le explorăm și transforma estimatorii în scară naturală.
        <br>
        Din această casetă putem deduce dacă covariatele răspund sau nu. Astfel, dacă este p mult mai mare de 0.05 pentru unele covariate,
        putem relua modelarea fără covariata respectivă. Dacă selectăm prea multe covariate, cel mai probabil nu vom putea realiza un model robust, mai
        ales dacă avem puține situri și/sau vizite</p>
      ")
    }
  })
  
  
  output$expl_which_model <- renderText({
    if (modelling_yes_poccu2()) {
      HTML(
        "
        <p>Funcția dredge din pachetul R MuMIn realizează o selectare automată a modelelor după AICc, după ce rulează toate combinațiile cu termenii fixi. Pentru 
        interpretare, selectează modelul preferat. ID-ul selectat corespunde poziţiei modelului din tabelul Ierarhizarea modelelor după AICc.
        Primul model (modelul clasat pe locul #1 in tabel) este cel mai robust statistic, dar selecţia ar trebui să se facă şi funcţie de valoarea explicativă 
        a acestuia (relevanţa pentru întrebarea de cercetare pusă). Dacă avem multe situri/zone și covariate, calculele vor lua mult timp, astfel că mai potrivit ar fi
        să rulați aplicația din computerul propriu. </p>"
      )
    }
  })
  
  
  output$expl_model_output <- renderText({
    if (modelling_yes_poccu2()) {
      HTML("
        <p> Mai sus avem caseta cu rezultatul modelului selectat de noi, cu estimările pentru abundență (abundance) și
        detecție (detection). Estimatorii pentru abundență și detecție includ interceptarea (variația abundenței sau detecției fără 
        influența covariatei) și valorile pentru covariatele păstrate în model. Estimatorii nu se
        pot interpreta direct deoarece sunt în scara logit sau log (în tab-urile următoare le vom putea interpreta în scară naturală).
        <br>
        Formula modelului selectat este redată după call, poccu(formula = ~obs_pentru_detecție + 1 ~ obs1_pentru_vizite + ... + obsn_pentru_vizite 
        +  1, data = umf_poccu).
        Din această casetă putem deduce dacă covariatele răspund sau nu. Astfel, dacă p este mult mai mare decât 0.05 pentru unele covariate,
        mai degrabă selectăm alt model, dar uneori chiar și aceea covariată ar putea influența modelul, în sensul că ar putea controla comportamentul altor covariate. </p>   ")
    }
  })
  
  
  output$expl_grafice_probab <- renderText({
    if (modelling_yes_poccu2()) {
      HTML("
        <p> În această pagină vom avea un grafic cu probabilitatea de detecție la fiecare sit și vizită (primul), urmat de eventuale grafice pentru probabilitatea de
           abundență Dacă nu selectăm variabile graficele nu vor fi afișate. Banda gri reprezintă intervalul de confidență 95% pentru eroarea standard din jurul netezirii curbei (smoothing GLM). Din tendința curbei
           putem intrepreta cum influențează covariata respectivă detecția sau ocupanța. Dacă covariata este factor, vom fi afișate box-plot-uri. Dacă valorile sunt constante (ltoate punctele 
           pe aceeași linie) atunci modelul respectiv nu include covariatele respective, deși au fost selectate în modelul global. </p>   ")
    }
  })
  
  
  output$expl_predictii <- renderText({
    if (modelling_yes_poccu2()) {
      HTML("
        <p> Graficele de mai sus ilustrează distribuția probabilităților de detecție și abudență în fiecare sit, așa cum a rezultat din modelul selectat. Dacă valoarea este
           constantă, atunci în model este doar interceptarea (aceeași valoare pentru toate siturile). </p>   ")
    }
  })
  
  # text explicativ selecţie model
  
  output$text_sel_met <- renderText({
    if (modelling_yes_poccu()) {
      HTML( "
        <p>După introducerea datelor (număr animale la un sit, covariate pentru siturile de prelevare, covariate pentru vizite), selectaţi tipul de modelare.
        Astfel, selectaţi: model fără covariate (model nul) dacă aţi încărcat doar coloanele care indică numărul de animale la un sit,
        modele cu covariate pentru situri de prelevare dacă aţi încărcat coloanele care indică numărul de animale la un sit şi covariate pentru situri,
        modele cu covariate pentru vizite dacă aţi încărcat coloanele care indică numărul de animale la un sit şi covariate pentru vizite sau
        modele cu covariate pentru situri şi vizită dacă aţi încărcat coloanele care indică numărul de animale la un sit, covariate pentru situri şi covariate pentru vizite. 
        <br>
        Dacă selectaţi un model care nu se potriveşte cu datele veţi primi mesaj de eroare. Selectaţi modelul potrivit şi reluaţi analiza.</p>
      ")
    } 
  })
  
  # text explicativ selecţie model
  
  
  
  output$expl_selectnmixture <- renderText({
    HTML( "
        <p>Ce distributie trebuie să utilizăm în modelare? Pentru a afla cea mai adecvată distribuție sau mixture (Poisson, Negative Binomial, Zero Inflated Poisson), clasificați modelele nul după AICc.
        V-a apărea un tabel cu clasamentul, din care vom alege din meniul dropdown distribuția P, NB sau ZIP. </p>
      ")
  })
  
  
}

shinyApp(ui = ui, server = server)
