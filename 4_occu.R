library(shiny)
library(bslib)
library(MuMIn)
library(DT)
library(ggplot2)
library(tidyr)
library(unmarked)
library(shinybusy)
library(shiny.info)


# global variable version
VERSION <- "version 1.0.1. Questions to rlaurentiu@gmail.com"

# path to the predefined data file on the server
predefinedFile_occu1 <- "/srv/connect/apps/singlespecies/minkfrogs_occ.csv"

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
    "
    )
  )),
  tagList(shiny.info::version(position = "bottom left")),
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
    tabPanel(
      "occusinglesp",
      fluidPage(h3(
        "Modelare ocupanţă o specie un sezon cu sau fără covariate"
      )),
      sidebarLayout(
        sidebarPanel(
          verbatimTextOutput("selectedFile_occ"),
          radioButtons(
            "dataSource_occ",
            "Date pentru analiză",
            choices = list(
              "Incarcă fişier propriu" = "upload_occ",
              "Foloseşte datele noastre" = "predefined_occ"
            ),
            inline = TRUE
          ),
          # File input to upload data
          fileInput(
            "dataFile_occ",
            "Încarcă un fişier csv dacă nu foloseşti datele noastre. Verifică în manual structura necesară"
          ),
          
          # Dynamic UI for selecting detection history columns
          uiOutput("yColumnsSelector_occ"),
          
          # Dynamic UI for selecting site covariates columns
          uiOutput("siteCovsColumnsSelector_occ"),
          
          # Dynamic UI for selecting observation covariates columns
          uiOutput("obsCovsColumnsSelector_occ")
        ),
        
        mainPanel(
          tabsetPanel(
            tabPanel("Verifică datele încărcate", DTOutput("inputDataTable_occ")),
            tabPanel(
              "Ierarhizează modelele",
              uiOutput("text_sel_met"),
              selectInput(
                "modelType_occ",
                "Selectează tipul de modelare:",
                choices = c(
                  "Model fără covariate (model nul)",
                  "Modele cu covariate pentru situri de prelevare",
                  "Modele cu covariate pentru vizite",
                  "Modele cu covariate pentru situri şi vizite"
                )
              ),
              uiOutput("descript_tip_modelare"),
              add_busy_bar(color = "#FF0000"),
              actionButton("runModelsButton_occ", "Rulează modelarea"),
              verbatimTextOutput("umfSummary_occ"),
              uiOutput("text_umf_occu"),
              verbatimTextOutput("modelSummaries_occ"),
              uiOutput("text_umf_summaries")
              
            ),
            tabPanel(
              "Rezultate modelare",
              add_busy_bar(color = "#FF0000"),
              verbatimTextOutput("modelSummariesAfterDredge_occ"),
              selectInput("selectedModel_occ", "Selectează model:", choices = NULL),
              uiOutput("expl_which_model"),
              verbatimTextOutput("selectedModelOutput_occ"),
              uiOutput("expl_model_output")
            ),
            # tab for Probability Plots
            tabPanel(
              "Grafice probabilităţi ocupanţă",
              plotOutput("numericObsCovsPlots_occ"),
              # Plot for numeric obsCovs
              plotOutput("numericSiteCovsPlots_occ"),
              uiOutput("expl_grafice_probab")
            ),
            tabPanel(
              "Predicţii calculate",
              plotOutput("detHistOutput_occ"),
              plotOutput("psiHistOutputt_occ"),
              verbatimTextOutput("overallEstimates_occ"),
              uiOutput("expl_predictii")
            )
          )
        )
      )
    ),
    tabPanel("despre wildPop", uiOutput("aboutContent"))
  ),
  
  # Footer
  tags$footer(style = "text-align: center; padding-top: 10px; padding-bottom: 10px;", HTML(
    paste(
      "&copy; CCMESI",
      format(Sys.Date(), "%Y"),
      "WildPop. PN-III-P2-2.1-PED-2021-1965 Interactive tool for estimating abundance of wildlife populations"
    )
  ))
)

server <- function(input, output, session) {
  # Create a reactiveVal for holding data
  data_reactive_occu <- reactiveVal()
  models_reactive_occu <- reactiveVal()
  obsCovs_columns_occu <- NULL # Declare obsCovs_columns_occu outside of observeEvent
  detpred_plots_occu <- reactiveVal() # Create reactiveVal for detpred_plots_occu
  psipred_plots_occu <- reactiveVal()
  selectedFileName_occ <- reactiveVal("")  # ReactiveVal to hold the selected file name
  
  # Observe changes in the uploaded data file
  observe({
    req(input$dataSource_occ)
    if (input$dataSource_occ == "upload_occ") {
      req(input$dataFile_occ)
      data_reactive_occu(read.csv(input$dataFile_occ$datapath, header = TRUE))
      # Update the selected file name if uploaded
      selectedFileName_occ(input$dataFile_occ$name)
    } else if (input$dataSource_occ == "predefined_occ") {
      # Load predefined file from the server
      data_reactive_occu(read.csv(predefinedFile_occu1, header = TRUE))
      # Get the name of the predefined file
      fileName_occ <- basename(predefinedFile_occu1)
      # Update the selected file name if predefined
      selectedFileName_occ(fileName_occ)
    }
  })
  
  # Render the selected file name
  output$selectedFile_occ <- renderText({
    paste("Nume fişier selectat", selectedFileName_occ())
  })
  # Define UI for selecting detection history columns
  output$yColumnsSelector_occ <- renderUI({
    req(data_reactive_occu())
    
    label_with_tooltip <- HTML(
      '<div class="tooltip-container">
       <span class="tooltip-label"><b>Selectaţi coloanele cu detecţiile:</b></span>
       <div class="tooltip-text"> <span style="font-size: 80%;">
         Selectaţi coloanele cu vizite, unde 0 este animal nedetectat şi 1 este animal detectat. Se pot include celule fără date, NA</span>
       </div>
     </div>'
    )
    
    div(
      label_with_tooltip,
      checkboxGroupInput(
        "yColumns_occ",
        label = NULL,
        choices = names(data_reactive_occu()),
        inline = TRUE
      )
    )
  })
  
  # Define UI for selecting site covariates columns
  output$siteCovsColumnsSelector_occ <- renderUI({
    req(data_reactive_occu())
    available_site_covs <- setdiff(names(data_reactive_occu()), input$yColumns_occ)
    
    label_with_tooltip <- HTML(
      '<div class="tooltip-container">
     <span class="tooltip-label"><b>Selectaţi covariate pentru situri:</b></span>
     <div class="tooltip-text"> <span style="font-size: 80%;">
       Selectaţi coloanele cu covariatele pentru situri. Nu trebuie să aveţi celule fără datesau cu NA. Recomandăm max 3 covariate.
       Dacă nu doriţi sa lucraţi cu astfel de covariate nu selectaţi coloane şi alegeţi ulterior modelare fără covariate pentru situri</span>
     </div>
   </div>'
    )
    
    div(
      label_with_tooltip,
      checkboxGroupInput(
        "siteCovsColumns_occ",
        label = NULL,
        choices = available_site_covs,
        selected = character(0),
        inline = TRUE
      )
    )
  })
  
  # Define UI for selecting observation covariates columns
  output$obsCovsColumnsSelector_occ <- renderUI({
    req(data_reactive_occu())
    available_obs_covs <- setdiff(names(data_reactive_occu()), input$yColumns_occ)
    
    label_with_tooltip <- HTML(
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
      label_with_tooltip,
      checkboxGroupInput(
        "obsCovsColumns",
        label = NULL,
        choices = available_obs_covs,
        selected = character(0),
        inline = TRUE
      )
    )
  })
  
  observeEvent(input$runModelsButton_occ, {
    req(input$yColumns_occ)
    data_occu <- data_reactive_occu()
    
    if (!all(is.na(data_occu[, input$yColumns_occ]) |
             data_occu[, input$yColumns_occ] == 0 |
             data_occu[, input$yColumns_occ] == 1)) {
      return(
        showNotification(
          "Detecţii non-numerice (nu este doar 0, 1 sau NA). Corectaţi datele de intrare.",
          type = "error"
        )
      )
    }
    
    y <- as.matrix(data_occu[, input$yColumns_occ])
    y[y > 1] <- 1
    
    siteCovs_occ <- NULL
    if (length(input$siteCovsColumns_occ) > 0) {
      selected_site_covs_occ <- data_occu[, input$siteCovsColumns_occ, drop = FALSE]
      siteCovs_numeric_occ <- scale(selected_site_covs_occ[sapply(selected_site_covs_occ, is.numeric)])
      colnames(siteCovs_numeric_occ) <- paste0("scale.", colnames(siteCovs_numeric_occ))
      siteCovs_occ <- cbind(selected_site_covs_occ[sapply(selected_site_covs_occ, Negate(is.numeric))], siteCovs_numeric_occ)
      colnames(siteCovs_occ) <- paste0("SC.", colnames(siteCovs_occ))
    }
    
    obsCovs_list_occ <- NULL
    if (length(input$obsCovsColumns) > 0) {
      selected_obs_covs <- data_occu[, input$obsCovsColumns, drop = FALSE]
      obsCovs_numeric <- selected_obs_covs[sapply(selected_obs_covs, is.numeric)]
      
      if (ncol(obsCovs_numeric) > 0) {
        colnames(obsCovs_numeric) <- paste0("_scale", colnames(obsCovs_numeric))
        obsCovs_list_occ <- list("obsCovs1_scaled" = scale(obsCovs_numeric))
      } else {
        obsCovs_list_occ <- list("obsCovs1" = selected_obs_covs)
      }
    }
    
    global_model_formula_occ <- NULL
    if (input$modelType_occ == "Model fără covariate (model nul)") {
      umf_occu <- unmarkedFrameOccu(y = y)
      global_model_formula_occ <- as.formula("~ 1 ~ 1")
    } else if (input$modelType_occ == "Modele cu covariate pentru situri de prelevare" &&
               !is.null(siteCovs_occ)) {
      umf_occu <- unmarkedFrameOccu(y = y, siteCovs = siteCovs_occ)
      global_model_formula_occ <- as.formula(paste("~ 1 ~ ", paste(names(siteCovs_occ), collapse = " + ")))
    } else if (input$modelType_occ == "Modele cu covariate pentru vizite" &&
               !is.null(obsCovs_list_occ)) {
      obs_covariate_name <- names(obsCovs_list_occ)[1]
      umf_occu <- unmarkedFrameOccu(y = y, obsCovs = obsCovs_list_occ)
      global_model_formula_occ <- as.formula(paste("~", obs_covariate_name, "~ 1"))
    } else if (input$modelType_occ == "Modele cu covariate pentru situri şi vizite" &&
               !is.null(siteCovs_occ) && !is.null(obsCovs_list_occ)) {
      obs_covariate_name <- names(obsCovs_list_occ)[1]
      umf_occu <- unmarkedFrameOccu(y = y,
                                    siteCovs = siteCovs_occ,
                                    obsCovs = obsCovs_list_occ)
      global_model_formula_occ <- as.formula(paste(
        "~",
        obs_covariate_name,
        "~ ",
        paste(names(siteCovs_occ), collapse = " + ")
      ))
    } else {
      return(
        showNotification(
          "Nu aţi selectat covariate potrivite pentru modelul selectat. Verificaţi selecţia covariatelor şi reluaţi analiza.",
          type = "error"
        )
      )
    }
    
    umf_summary_occ <- capture.output({
      summary(umf_occu)
    })
    output$umfSummary_occ <- renderPrint({
      cat(umf_summary_occ, sep = "\n")
    })
    
    global_model_occ <- occu(global_model_formula_occ, umf_occu)
    
    model_summary_occ <- capture.output({
      summary(global_model_occ)
    })
    output$modelSummaries_occ <- renderPrint({
      cat(model_summary_occ, sep = "\n")
    })
    
    m1_occ <- MuMIn::dredge(global_model_occ, rank = "AICc", trace = TRUE)
    models_reactive_occu(m1_occ)
    
    modelIDs_occ <- paste0("model clasat pe locul #", 1:nrow(m1_occ))
    updateSelectInput(session, "selectedModel_occ", choices = modelIDs_occ)
    
    output$modelSummariesAfterDredge_occ <- renderPrint({
      cat("Ierarhizarea modelelor după AIC (cu MuMIn::dredge):\n")
      print(m1_occ)
    })
  })
  
  observeEvent(input$selectedModel_occ, {
    req(models_reactive_occu(), input$selectedModel_occ)
    selected_model_index <- as.numeric(gsub("\\model clasat pe locul #", "", input$selectedModel_occ))
    selected_model_occ <- MuMIn::get.models(models_reactive_occu(), subset = selected_model_index)
    
    output$selectedModelOutput_occ <- renderPrint({
      summary_text <- capture.output({
        summary(selected_model_occ[[1]])
      })
      cat("Sumarul modelului selectat:\n")
      cat(summary_text, sep = "\n")
    })
    
    # Modified section: Use selected_model_occ[[1]] for predictions
    detpred_plots_data_occ <- predict(selected_model_occ[[1]],
                                      type = "det",
                                      appendData = TRUE)
    detpred_plots_data_occ <- data.frame(detpred_plots_data_occ) # Convert to data frame
    detpred_plots_occu(detpred_plots_data_occ)
    
    psipred_plots_data_occ <- predict(selected_model_occ[[1]],
                                      type = "state",
                                      appendData = TRUE)
    psipred_plots_data_occ <- data.frame(psipred_plots_data_occ) # Convert to data frame
    psipred_plots_occu(psipred_plots_data_occ)
    
    truocc <- sum(bup(ranef(selected_model_occ[[1]], stat = "mode")))
    
    output$psiHistOutputt_occ <- renderPlot({
      ggplot(psipred_plots_data_occ,
             aes(x = psipred_plots_data_occ$Predicted)) +
        geom_histogram(
          binwidth = 0.05,
          fill = "grey",
          color = "black",
          alpha = 0.7
        ) +
        theme_minimal() +
        labs(title = "Probabilitatea de ocupanţă", x = "Probabilitatea de ocupanţă", y = "Număr cazuri")
    })
    
    output$detHistOutput_occ <- renderPlot({
      ggplot(detpred_plots_data_occ,
             aes(x = detpred_plots_data_occ$Predicted)) +
        geom_histogram(
          binwidth = 0.05,
          fill = "grey",
          color = "black",
          alpha = 0.7
        ) +
        theme_minimal() +
        labs(title = "Probabilitatea de detecţie", x = "Probabilitatea de detecţie", y = "Număr cazuri")
    })
    
    
    output$overallEstimates_occ <- renderPrint({
      cat(
        "Estimare probabilitate ocupanţă pentru întreg studiul: ",
        mean(psipred_plots_data_occ$Predicted),
        "\n"
      )
      cat(
        "Estimare probabilitate detecţie pentru întreg studiul: ",
        mean(detpred_plots_data_occ$Predicted),
        "\n"
      )
      cat("Procent de situri ocupate extras din model cu funcţia ranef: ",
          truocc,
          "\n"
      )
      
    })
    
    # Search for columns starting with "obsCovs1" in detpred_plots_occu
    req(detpred_plots_occu())
    obsCovs_columns_occu <- grep("^obsCovs1", colnames(detpred_plots_occu()), value = TRUE)
    if (length(obsCovs_columns_occu) > 0) {
      numeric_obsCovs <- sapply(detpred_plots_occu()[, obsCovs_columns_occu], is.numeric)
      numeric_obsCovs_columns <- obsCovs_columns_occu[numeric_obsCovs]
      if (length(numeric_obsCovs_columns) > 0) {
        output$numericObsCovsPlots_occ <- renderPlot({
          plot_data_occ <- detpred_plots_occu()
          plot_data_occ$Predicted <- as.numeric(plot_data_occ$Predicted)
          
          if (length(obsCovs_columns_occu) > 0) {
            obsCovs_data_occ <- plot_data_occ[, obsCovs_columns_occu, drop = FALSE]
            obsCovs_data_long_occ <- tidyr::gather(obsCovs_data_occ,
                                                   key = "Variable",
                                                   value = "Value")
            
            # Repeat 'Predicted' column to match the number of rows
            obsCovs_data_long_occ$Predicted <- rep(
              plot_data_occ$Predicted,
              each = nrow(obsCovs_data_long_occ) / nrow(plot_data_occ)
            )
            
            # Check if all columns are numeric
            if (all(sapply(obsCovs_data_long_occ$Value, is.numeric))) {
              # If all columns are numeric, create scatter plots
              p <- ggplot(obsCovs_data_long_occ,
                          aes(x = Value, y = Predicted)) +
                geom_point(alpha = 0.7) +
                geom_smooth(method = "glm", se = TRUE) +
                labs(x = "Covariată vizite (numeric, valori scalate)", y = "Probabilitate detecţie") + theme_minimal()
              
              print(p)
            } else {
              # If there are factor columns, create boxplots
              p <- ggplot(obsCovs_data_long_occ,
                          aes(x = Variable, y = Predicted)) +
                geom_boxplot() +
                labs(x = "Covariată vizite (factor)", y = "Probabilitate detecţie") + theme_minimal()
              
              print(p_occ)
            }
          }
        })
        
      }
    }
  })
  
  output$numericSiteCovsPlots_occ <- renderPlot({
    req(psipred_plots_occu())
    plot_dataSC_occ <- psipred_plots_occu()
    # Identify columns starting with "SC"
    sc_columns_occ <- grep("^SC.", names(plot_dataSC_occ), value = TRUE)
    
    # Create an empty list to store plots
    plots <- list()
    
    # Loop through SC columns and create plots
    if (length(sc_columns_occ) > 0) {
      for (sc_col in sc_columns_occ) {
        # Determine if the column is numeric or factor
        if (is.numeric(plot_dataSC_occ[[sc_col]])) {
          # Create scatter plot for numeric data
          p_occ <- ggplot(plot_dataSC_occ,
                          aes_string(x = sc_col, y = "Predicted")) +  # Updated 'predicted' here
            geom_point(alpha = 0.7) +
            geom_smooth(method = "glm", se = TRUE) +
            labs(x = sc_col, y = "Probabilitate ocupanţă") +  # Updated 'predicted' here
            theme_minimal() +
            facet_wrap( ~ .)
        } else if (is.factor(plot_dataSC_occ[[sc_col]])) {
          # Create boxplot for factor data
          p_occ <- ggplot(plot_dataSC_occ,
                          aes_string(x = sc_col, y = "Predicted")) +  # Updated 'predicted' here
            geom_boxplot() +
            labs(x = sc_col, y = "Probabilitate ocupanţă") +  # Updated 'predicted' here
            theme_minimal() +
            facet_wrap( ~ .)
        }
        
        # Add plot to the list
        plots[[length(plots) + 1]] <- p_occ
      }
      
      # Combine plots into a single grid
      gridExtra::grid.arrange(grobs = plots, ncol = 2)
    }
  })
  
  
  # Render the Input Data table
  output$inputDataTable_occ <- renderDT({
    req(data_reactive_occu(), input$yColumns_occ)
    
    selected_columns_occ <- c(input$yColumns_occ)
    
    if (!is.null(input$siteCovsColumns_occ)) {
      selected_columns_occ <- c(selected_columns_occ, input$siteCovsColumns_occ)
    }
    
    if (!is.null(input$obsCovsColumns)) {
      selected_columns_occ <- c(selected_columns_occ, input$obsCovsColumns)
    }
    
    datatable(data_reactive_occu()[1:10, selected_columns_occ],
              options = list(paging = TRUE),
              rownames = FALSE)
  })
  
  
  # home page text
  
  output$homeContent <- renderUI({
    fluidPage(
      h3("Aplicație interactivă estimare ocupanță animale sălbatice"),
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
          "Primul tip, cel pe care îl rulăm din această aplicație,  este reprezentat de <b>modelele de ocupanță</b>, orientate spre evaluarea prezenței speciilor, 
          nu a numărului de indivizi. Ele oferă un cadru pentru estimarea probabilității ca o specie să fie prezentă, încorporând probabilitățile de detectare a 
          speciei în calculele lor.
        "
        )
      ),
      p(
        HTML(
          "Al doilea tip este reprezentat de <b>modelele N-mixture</b>, care sunt utile pentru estimarea abundenței speciilor. Aceste modele sunt eficiente în 
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
          "Din această aplicație vom putea rula modele de tip un sezon, o singură specie cu sau fără covariate pentru vizite și situri. Pentru această accesați <b>pagina occusinglesp</b>. 
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
  
  
  # text explicativ selecţie model
  
  output$text_sel_met <- renderText({
    "
        <p>După selectarea datelor (detecţie/nondetecţie, covariate pentru siturile de prelevare, covariate pentru vizite), selectaţi tipul de modelare.
        Astfel, selectaţi: model fără covariate (model nul) dacă aţi încărcat doar coloanele care indică detecţie/nondetecţie,
        modele cu covariate pentru situri de prelevare dacă aţi încărcat coloanele care indică detecţie/nondetecţie şi covariate pentru situri,
        modele cu covariate pentru vizite dacă aţi încărcat coloanele care indică detecţie/nondetecţie şi covariate pentru vizite sau
        modele cu covariate pentru situri şi vizită dacă aţi încărcat coloanele care indică detecţie/nondetecţie, covariate pentru situri şi covariate pentru vizite. 
        <br>
        Dacă selectaţi un model care nu se potriveşte cu datele veţi primi mesaj de eroare. Selectaţi modelul potrivit şi reluaţi analiza.</p>
      "
  })
  
  
  # texte reactive
  
  modelling_yes_occ <- reactiveVal(FALSE) # reactive test sim occu
  
  observeEvent(input$runModelsButton_occ, {
    # reactive text umf_occu occu description
    modelling_yes_occ(TRUE)
  })
  
  
  
  output$text_umf_occu <- renderText({
    if (modelling_yes_occ()) {
      HTML("
        <p>Caseta de mai sus prezintă sumarul datelor introduse în modelul o singură specie, un singur sezon, așa cum sunt ele
         integrate în formatul pachetului R unmarked. Sumarul oferă informații privind numărul de zone analizate, numărul de detecții (1) și
         de non-detecții (0), statistică descriptivă a covariatelor la nivel de sit (covariate pentru ocupanță) și la sit (covariate pentru detecție).
         Acestea vor apărea dacă le selectăm în pasul anterior. Dacă apare abrevierea scaled, atunci covariate au fost scalate pentru comparație.
         Aplicația scalează automat variabilele numerice. Variabilele factor sunt introduse fără prelucrare.
         <br> 
          Sumarul unmarked se verifică pentru a depista eventualele erori din datele introduse.</p>
      ")
    }
  })
  
  output$text_umf_summaries <- renderText({
    if (modelling_yes_occ()) {
      HTML("
        <p> Mai sus avem caseta cu rezultatul modelului global (toate covariatele), pentru ocupanță (occupancy) și
        detecție (detection). Estimatorii pentru ocupanță și detecție includ interceptarea (variația ocupanței sau detecției fără 
        influența covariatei) și valorile pentru covariatele selectate. Ele vor apărea doar dacă se selectează covariate. Estimatorii nu se
        pot interpreta direct deoarece sunt în scara logit. Din tab-ul următor vom rula mai multe modele, îl vom selecta pe cel mai adecvat pentru
        întrebările de cercetare pe care le explorăm și transforma estimatorii în scară naturală.
        <br>
        Din această casetă putem deduce dacă covariatele răspund sau nu. Astfel, dacă este p mult mai mare de 0.05 pentru unele covariate,
        putem relua modelarea fără covariata respectivă. Dacă selectăm prea multe covariate, cel mai probabil nu vom putea realiza un model robust, mai
        ales dacă avem puține situri și/sau vizite. </p>
      ")
    }
  })
  
  output$text_umf_summaries <- renderText({
    if (modelling_yes_occ()) {
      HTML("
        <p>Mai sus avem caseta cu rezultatul modelului selectat de noi, cu estimările pentru ocupanță (occupancy) și
        detecție (detection). Estimatorii pentru aceste ocupanță și detecție includ interceptarea (variația ocupanței sau detecției fără 
        influența covariatei) și valorile pentru covariatele păstrate în model. Estimatorii nu se
        pot interpreta direct deoarece sunt în scara logit. Din tab-ul următor vom rula mai multe modele, îl vom selecta pe cel mai adecvat pentru
        întrebările de cercetare pe care le explorăm și transforma estimatorii în scară naturală.
        <br>
        Formula modelului selectat este redată după call, occu(formula = ~obs_pentru_detecție + 1 ~ obs1_pentru_vizite + ... + obsn_pentru_vizite 
        +  1, data = umf_occu).
        Din această casetă putem deduce dacă covariatele răspund sau nu. Astfel, dacă p este mult mai mare decât 0.05 pentru unele covariate,
        putem relua modelarea fără covariata respectivă. Dacă selectăm prea multe covariate, cel mai probabil nu vom putea realiza un model robust, mai
        ales dacă avem puține situri și/sau vizite. </p>
      ")
    }
  })
  
  output$expl_which_model <- renderText({
    if (modelling_yes_occ()) {
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
    if (modelling_yes_occ()) {
      HTML("
        <p> Mai sus avem caseta cu rezultatul modelului selectat de noi, cu estimările pentru ocupanță (occupancy) și
        detecție (detection). Estimatorii pentru aceste ocupanță și detecție includ interceptarea (variația ocupanței sau detecției fără 
        influența covariatei) și valorile pentru covariatele păstrate în model. Estimatorii nu se
        pot interpreta direct deoarece sunt în scara logit (în tab-urile următoare le vom putea interpreta în scară naturală).
        <br>
        Formula modelului selectat este redată după call, occu(formula = ~obs_pentru_detecție + 1 ~ obs1_pentru_vizite + ... + obsn_pentru_vizite 
        +  1, data = umf_occu).
        Din această casetă putem deduce dacă covariatele răspund sau nu. Astfel, dacă p este mult mai mare decât 0.05 pentru unele covariate,
        mai degrabă selectăm alt model, dar uneori chiar și aceea covariată ar putea influența modelul, în sensul că ar putea controla comportamentul altor covariate. </p>   ")
    }
  })
  
  
  output$expl_grafice_probab <- renderText({
    if (modelling_yes_occ()) {
      HTML("
        <p> În această pagină vom avea un grafic cu probabilitatea de detecție la fiecare sit și vizită (primul), urmat de eventuale grafice pentru probabilitatea de
           ocupanță. Dacă nu selectăm variabile graficele nu vor fi afișate. Banda gri reprezintă intervalul de confidență 95% pentru eroarea standard din jurul netezirii curbei (smoothing GLM). Din tendința curbei
           putem intrepreta cum influențează covariata respectivă detecția sau ocupanța. Dacă covariata este factor, vom fi afișate box-plot-uri. Dacă valorile sunt constante (ltoate punctele 
           pe aceeași linie) atunci modelul respectiv nu include covariatele respective, deși au fost selectate în modelul global. </p>   ")
    }
  })
  
  
  output$expl_predictii <- renderText({
    if (modelling_yes_occ()) {
      HTML("
        <p> Graficele de mai sus ilustrează distribuția probabilităților de detecție și ocupanță în fiecare sit, așa cum a rezultat din modelul selectat. Dacă valoarea este
           constantă, atunci în model este doar interceptarea (aceeași valoare pentru toate siturile). </p>   ")
    }
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
  
}

shinyApp(ui = ui, server = server)
