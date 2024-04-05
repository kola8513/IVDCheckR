#libraries
pacman::p_load(shiny,shinydashboard,fresh,rhandsontable,shinyjs,rmarkdown,pagedown,pdftools,mcr,tinytex,shinycssloaders,knitr,digest)

# theme
IVDCheckR_theme <- create_theme(adminlte_color(light_blue = "#003B73"),
                                adminlte_sidebar(width = "250px",dark_bg = "#40668d",
                                                 dark_hover_bg = "#40668d",dark_color = "#FFF"),
                                adminlte_global(content_bg = "#FFFFFF",box_bg = "#FFFFFF",info_box_bg = "#FFFFFF"))


buttoncolors <- "color: #000; background-color: #CCCCCC; border-color: #000"

setwd(tempdir())

# Define a function to sanitize inputs
sanitize_input <- function(input) {
  # Specify allowed patterns using regular expressions
  allowed_pattern <- "^[a-zA-Z0-9.\\s-]*$"
  # Check if input matches allowed pattern
  if (!grepl(allowed_pattern, input)) {
    # If input does not match, replace with empty string or handle appropriately
    input <- ""
  }
  return(input)
}


writeLines(
  con = 'ivdcheckr.rmd',
  text = '---
title: "IVDCheckR"
output: rmdformats::material

params:
  projectInput: "Title"
  DateRange: "Date"
  ParameterInfo: "Measurand description"
  ParameterUse: "Intended use of the measurand"
  Indication: "Indication"
  ScientificValidity: "Scientific validity"
  MeasurementScale: "Measurement scale"
  SampleMaterial: "Sample material"
  others_SampleMaterial: "Other sample material"
  RiskClass: "Risk class"
  RiskClassDetails: "Details"
  ModificationProduct: "ModificationProduct"
  SOPInput: "SOP"
  JustificationInput: "Justification for use of the LDT"
  DeclarationInput: "Declaration of Conformity"
  RiskInput: "Risk management"
  MonitoringInput: "Monitoring and review activities"
  AnalyticalInput: "Analytical performance"
  ClinicalInput: "Clinical performance"
  extraText: "Further comments"


  analyte: "Analyte Name"
  description: "Description of Analyte"  # Define default values or leave it empty
  withbetw: "Within/Between Run"  # Define default values or leave it empty
  df: ""
  QCresults: ""
  mcdf: ""
  table_data: ""


---

## Project title
 `r params$projectInput`

## Date
 `r params$DateRange`

## Analyte Description
`r params$ParameterInfo`

## Intended use of the analyte
`r params$ParameterUse`

## Indication
`r params$Indication`

## Scientific validity
`r params$ScientificValidity`

## Measurement scale
`r params$MeasurementScale`

## Sample material
`r params$SampleMaterial`

## Other sample material
`r params$others_SampleMaterial`

## Risk class
`r params$RiskClass`

## Modification of a existing product (standard) OR new product/development
`r params$ModificationProduct`

## SOP
`r params$SOPInput`

## Justification for use of the LDT
`r params$JustificationInput`

## Declaration of Conformity
`r params$DeclarationInput`

## Risk management
`r params$RiskInput`

## Monitoring and review activities
`r params$ MonitoringInput`

## Analytical performance
`r params$AnalyticalInput`

## Clinical performance
`r params$ClinicalInput`


## Further comments
`r params$extraText`

\n\r

```{r mc, include=FALSE}
knitr::opts_chunk$set(echo = TRUE, warning = FALSE, message = FALSE, fig.height = 6)
```

## Method Comparison

```{r, echo=FALSE, comment=NA}


# Define a reactive expression to handle MC tool data
mcdf_data <- reactive({
  if (!is.null(input$mctable)) {
    mcdf <- hot_to_r(input$mctable)
    mcdf <- mcdf[!is.na(as.numeric(mcdf$x)), ]
    mcdf <- mcdf[!is.na(as.numeric(mcdf$y)), ]
    return(mcdf)
  } else {
    return(data.frame()) # Return an empty data frame if no input
  }
})


# Check if there is MC tool data available
if (is.null(mcdf_data()) || nrow(mcdf_data()) < 2) {
  plot.new()

} else {

  plot1(mcdf_data())
  reg <- mcreg(mcdf_data()$x, mcdf_data()$y, method.reg = "PaBa")

  cat(paste0("Passing Bablok regression function: y = ",
             format(getCoefficients(reg)[1], digits = 2),
             " + ",
             format(getCoefficients(reg)[2], digits = 2),
             " * x"
  ))

  cat(paste0("Spearman coefficient of correlation: ", format(cor(mcdf_data()$x, mcdf_data()$y, method = "spearman"), digits = 2)))

  plot2(mcdf_data())

  cat("Raw data entered for method comparison\n")

  # Raw data 
  print(mcdf_data())
} 

```
\r

```{r qc, include=FALSE}
knitr::opts_chunk$set(echo = TRUE, warning = FALSE, message = FALSE, fig.height = 6)
```

## Quality control 

```{r, echo=FALSE, comment=NA}

# Define a reactive expression to handle QC tool data
df_data <- reactive({
  if (!is.null(input$QCtable)) {
    df <- hot_to_r(input$QCtable)
    results <- calcs(df)
    return(results)
  } else {
    return(NULL)
  }
})

# Check if there is QC tool data available
if (!is.null(input$QCtable)) {

## Analyte
print(analyte)

## Description
print(description)

## QC type
print(withbetw)

## Result
print(QCresults)

\n

## Raw data entered for quality control

print(df)

}

```

```{r ChemSoft, include=FALSE}
knitr::opts_chunk$set(echo = TRUE, warning = FALSE, message = FALSE, fig.height = 6)
```
## List of used chemicals and software

```{r, echo=FALSE, comment=NA}

print(table_data)

'
)

# for resizing the column widths of tables
jsCode <- c(
  "function(el, x) {",
  "  Handsontable.hooks.add('afterColumnResize', function(index, size){",
  "    Shiny.setInputValue('newsize', {index: index+1, size: size});",
  "  });",
  "}"
)

#-----------------------------------------------------------------------------------

# text box fields
# Enter the title of the project:
# 1. Analyte Description
# 2. Intended use of the analyte
# 3. Indication
# 4. Scientific validity
# 5. Measurement Scale:
# 6. Sample material :
# 7. Risk classification : Details
# 8. Modification of a existing product (standard) OR new product/development


# List of files to upload
files_to_upload <- c(
  "9. Standard Operating Procedures (SOPs)",
  "10. Justification for use of the LDT",
  "11. Declaration of Conformity",
  "12. Risk management",
  "13. Monitoring and review activities",
  "14. Analytical performance",
  "15. Clinical performance")


ui <- dashboardPage(
  dashboardHeader(title = "IVDCheckR"),
  dashboardSidebar(    
    sidebarMenu(
      img(src='umo.svg', height='110px', style='padding:10px;text-align:center;width:100%;'),
      br(),
      id = "tabs",
      menuItem("Information", tabName = "Info", icon = icon("info")),
      menuItem("LDT project description", tabName = "Project_Details", startExpanded = TRUE, icon = icon("paperclip")),
      menuItem("Method comparison", tabName = "MCTool", startExpanded = TRUE, icon = icon("vials")),
      menuItem("Quality control", tabName = "QCTool", startExpanded = TRUE, icon = icon("check-to-slot")),
      menuItem("Chemicals and software", tabName = "ChemSoft", startExpanded = TRUE, icon = icon("flask-vial")),
      menuItem("Disclaimer",icon = icon("paperclip"), href = "https://kc.uol.de/disclaimer/")
    )
  ),
  
  dashboardBody(
    
    tags$head(
      tags$style(HTML("
      .custom-input textarea,
      .custom-input input {
        font-size: 22px;
        font-family: 'Nunito Sans', sans-serif;
        text-align: justify;
      }
    "))
    ),
    
    tags$head(
      tags$style(
        HTML(
          "#current_project_name {font-size: 22px; font-family: Nunito Sans; text-align: center; color: #fff; background-color: #003B73}"
        )
      )
    ),
    
    tags$script("
  $(document).ready(function() {
    $('#startButton').click(function() {
      $(this).css('color', 'white'); // Change font color to white
    });
  });
"),
    
    
    tags$head(tags$style(HTML('* {font-size: 15px; font-family: "Nunito Sans"};'))),
    
    
    useShinyjs(),
    
    tabItems(
      
      tabItem(tabName = "Info",
              br(),
              fluidRow(
                box(
                  status = "primary",
                  solidHeader = TRUE,
                  width = 12,
                  style = "border: 1px solid #40668d; border-color:#40668d;display: flex; justify-content: left;",
                  column(
                    width = 12,
                    br(),
                    HTML("<p style='font-size: 22px; font-family: Nunito Sans; text-align: justify;'>
                            Welcome to IVDCheckR, your comprehensive integration platform designed to ensure electronic IVDR compliance for your laboratory developed tests (LDTs) according to Article 5 (5) of the in Vitro Diagnostic Medical Device Regulation (EU) 2017/746 (EU-IVDR). Begin by pressing the start button below, you'll be directed to the LDT project details landing page, where you can enter specific details of your project.

                            </p>"),
                    
                    br(),
                    
                    HTML("<p style='font-size: 22px; font-family: Nunito Sans; text-align: justify;'>Click <a href='https://eur-lex.europa.eu/legal-content/EN/TXT/?uri=CELEX%3A02017R0746-20230320' target='_blank' style='font-size: 22px; font-family: Nunito Sans;'>here</a> for the official website of the EU-IVDR.</p>"),
                    
                    br(),
                    br(),
                    
                    HTML("<p style='font-size: 22px; font-family: Nunito Sans; text-align: justify;'>
                           How to use this tool: </p>"),
                    br(),
                    HTML("<p style='font-size: 22px; font-family: Nunito Sans; text-align: justify;'>
                           1. Enter the title of your project. </p>"),
                    HTML("<p style='font-size: 22px; font-family: Nunito Sans; text-align: justify;'>
                           2. Add specific details about your project in the project details tab , e.g., upload all relevant SOPs. </p>"),
                    HTML("<p style='font-size: 22px; font-family: Nunito Sans; text-align: justify;'>
                           3. Perform method comparison using the Method Comparison tab. </p>"),
                    HTML("<p style='font-size: 22px; font-family: Nunito Sans; text-align: justify;'>
                           4. Perform quality controls using the Quality Control tab. </p>"),
                    HTML("<p style='font-size: 22px; font-family: Nunito Sans; text-align: justify;'>
                           5. Press Download combined pdf to get a complete report of your project. </p>"),
                    br(),
                    
                    div(style = "text-align: center;",
                        actionButton(inputId = "startButton", "Start", class = "btn-primary",
                                     style = "background-color: #003B73; border: none; font-size: 20px; color: lightgray",
                                     icon = icon("power-off", lib = "font-awesome", class = "fa-lg"))
                    )
                    
                  )
                )
              )
      ),
      
      tabItem(tabName = "Project_Details",
              downloadButton("downloadBtn", "Download Combined PDF",block = TRUE,icon = icon("download"),
                             style="color: #fff; background-color: #003B73; border-color: black"),
              
              #downloadButton("download_word_report", "Download Word Report",block = TRUE,icon = icon("download"),
              #style="color: #fff; background-color: #003B73; border-color: black"),
              
              tags$br(),
              tags$br(),
              
              fluidRow(
                box(
                  status = "primary",
                  solidHeader = TRUE,
                  width = 6,
                  style = "border: 1px solid #40668d; border-color:#40668d;display: flex; justify-content: left;",
                  column(width = 5,
                         #offset = 1,  # To center the first column
                         style = "white-space: nowrap;",
                         tags$head(tags$style(type="text/css", "#someid {width: 600px}")),
                         
                         # Date part
                         dateRangeInput("dates", "Date", start = as.character(Sys.Date()), width = "500px",separator = " - "),
                         
                         textInput("projectInput", 
                                   label = HTML("<p font-family: Nunito Sans; text-align: justify;'>Enter the title of the project:</p>"), value = ""),
                         
                         
                         div(
                           textAreaInput("ParameterInfo", label = "1. Measurand description", 
                                         placeholder = "Add the measurand name, give a general description and information regarding the measurand, e.g., sodium",
                                         height = "100px", 
                                         width = "300px"),
                           HTML("Click <a href='https://eur-lex.europa.eu/legal-content/EN/TXT/HTML/?uri=CELEX:02017R0746-20230320#tocId1139' target='_blank'>here</a> for the official excerpt of the IVDR")
                         ),
                         
                         br(),
                         
                         div(textAreaInput("ParameterUse", label = "2. Intended use of the measurand",
                                           
                                           placeholder = "Specify the intended purpose of the measurand, e.g., detection of hypernatremia or hyponatremia in the general population independent of age, gender and ethnicity.",height="100px", width="300px"),
                             
                             HTML("Click <a href='https://eur-lex.europa.eu/legal-content/EN/TXT/HTML/?uri=CELEX:02017R0746-20230320#tocId1138' target='_blank'>here</a> for the official excerpt of the IVDR")
                         ),
                         
                         br(),
                         
                         div(textAreaInput("Indication",label = "3. Indication",
                                           placeholder = "Add the diagnostic indication e.g., hypernatremia or hyponatremia.",height="100px", width="300px"),
                             
                         ),
                         
                         HTML("<a href='https://eur-lex.europa.eu/legal-content/EN/TXT/HTML/?uri=CELEX:02017R0746-20230320#tocId1131' target='_blank' style='font-size: 14px;'>Link 1</a> and <a href='https://eur-lex.europa.eu/legal-content/EN/TXT/HTML/?uri=CELEX:02017R0746-20230320#tocId1139' target='_blank' style='font-size: 14px;'>Link 2</a> for the official excerpt of the IVDR"),
                         
                         br(),
                         br(),
                         
                         div(textAreaInput("ScientificValidity",label = "4. Scientific validity",
                                           
                                           placeholder = "Enter the scientific association of the analyte with a specific clinical or physiological condition.",
                                           height="100px", width="300px")),
                         
                         HTML("Click <a href='https://eur-lex.europa.eu/legal-content/EN/TXT/HTML/?uri=CELEX:02017R0746-20230320#tocId1191' target='_blank' style='font-size: 14px;'> here</a> for the official excerpt of the IVDR"),
                         
                         br(),
                         br(),
                         #radio button options
                         radioButtons("MeasurementScale",label = "5. Measurement Scale:",
                                      
                                      choices = c("Quantitative", "Qualitative", "Semi-quantitative"),
                                      selected = "Quantitative"),
                         
                         
                         HTML("<br><a href='https://eur-lex.europa.eu/legal-content/EN/TXT/HTML/?uri=CELEX:02017R0746-20230320#tocId1139' target='_blank' style='font-size: 14px;'>Click here</a> for the official excerpt of the IVDR"), 
                         br(),
                         br(),
                         
                         # multiple checkboxes selection
                         div(
                           checkboxGroupInput("SampleMaterial", label = "6. Sample material :",
                                              choices = c("Serum", "Heparin plasma", "Citrate plasma", "EDTA",
                                                          "Whole blood", "Urine", "CSF", "Other")),
                           HTML("<br><a href='https://eur-lex.europa.eu/legal-content/EN/TXT/HTML/?uri=CELEX:02017R0746-20230320#tocId1131' target='_blank'>Link 1</a> and <a href='https://eur-lex.europa.eu/legal-content/EN/TXT/HTML/?uri=CELEX:02017R0746-20230320#tocId1139' target='_blank'>Link 2</a> for the official excerpt of the IVDR"),
                           conditionalPanel(
                             condition = "input.SampleMaterial.includes('Other')",
                             textInput("others_SampleMaterial", "Please specify:")
                           )
                         ),
                         br(),
                         
                         div(
                           selectInput("RiskClass",label = "7. Risk classification",
                                       
                                       choices = c("A", "B", "C", "D"),
                                       selected = "B"),
                           textAreaInput("RiskClassDetails", label = "Details",
                                         placeholder = "The risk classification of an IVD is divided into: A-D classes (low to high risk) which is classified to indicate the risk arising from the device/assay regarding users, patients and third parties",
                                         height = "100px", width = "300px"),
                           HTML("Click <a href='Table 1_ RiskClassification.png' target='_blank'>here</a> for detailed risk classification"),
                           br(),
                           HTML("Click <a href='https://eur-lex.europa.eu/legal-content/EN/TXT/HTML/?uri=CELEX:02017R0746-20230320#tocId1169' target='_blank' style='font-size: 14px;'> here</a> for the official excerpt of the IVDR")
                         ),
                         
                         br(),
                         radioButtons("ModificationProduct", label = "8. Modification of a existing product (standard) OR new product/development",
                                      choices = c("Yes", "No"),
                                      selected = "Yes")
                  )
                ),
                fluidRow(
                  box(
                    status = "primary",
                    solidHeader = TRUE,
                    width = 6,
                    style = "border: 1px solid #40668d; border-color:#40668d;display: flex; justify-content: left;",
                    column(6,
                           div(
                             id = "fileUploadOptions",
                             lapply(1:7, function(i) {
                               div(
                                 radioButtons(paste0("fileOption", i), label = files_to_upload[i],
                                              choices = c("Not Applicable", "Upload PDF and/or Add Text"), 
                                              selected = "Not Applicable"),
                                 conditionalPanel(
                                   
                                   condition = paste0("input.fileOption", i, " != 'Not Applicable'"),
                                   
                                   fileInput(paste0("file", i), label = "Upload PDF", accept = ".pdf"),
                                   
                                   if (i == 1) {
                                     div(
                                       textAreaInput(paste0("text", i), label = paste("Enter text"),
                                                     placeholder = "e.g. document name ‘QM-SOP-12-Analyte’ or https://lab/qm/sops/12.pdf", rows = 5),
                                       HTML("Click <a href='SOP.pdf' target='_blank'>here</a> for SOP template"),
                                       
                                       HTML("<br><a href='https://eur-lex.europa.eu/legal-content/EN/TXT/HTML/?uri=CELEX:02017R0746-20230320#tocId1131' target='_blank'>Link 1</a> and <a href='https://eur-lex.europa.eu/legal-content/EN/TXT/HTML/?uri=CELEX:02017R0746-20230320#tocId1139' target='_blank'>Link 2</a> for the official excerpt of the IVDR"),
                                     )
                                     
                                   } 
                                   
                                   
                                   else if (i ==2)
                                   {
                                     div(
                                       textAreaInput(
                                         
                                         paste0("text", i), 
                                         label = paste("Enter text"),
                                         placeholder = "Compare the LDT with all its characteristics with CE-marked devices available on the EU market", rows = 5),
                                       HTML("Click <a href='https://eur-lex.europa.eu/legal-content/EN/TXT/HTML/?uri=CELEX:02017R0746-20230320#tocId115' target='_blank'>here</a> for the official excerpt of the IVDR"),
                                       
                                       
                                     )
                                   }
                                   
                                   else if (i ==3)
                                   {
                                     div(
                                       textAreaInput(paste0("text", i), label = paste("Enter text"),
                                                     placeholder = "Add document(s) declaring that a manufactured or modified CE labelled assay complies with relevant EU law, including analytical and clinical performance.", rows = 5),
                                       HTML("Click <a href='Declaration_of_conformity.pdf' target='_blank'>here</a> for declaration of conformity template"),
                                       
                                       br(),
                                       HTML("Click <a href='https://eur-lex.europa.eu/legal-content/EN/TXT/HTML/?uri=CELEX:02017R0746-20230320#tocId36
' target='_blank'>here</a> for the official excerpt of the IVDR")
                                       
                                       
                                     )
                                   }
                                   
                                   else if (i ==4)
                                   {
                                     div(
                                       textAreaInput(paste0("text", i), label = paste("Enter text"),
                                                     placeholder = "Add information about safety requirements and safety procedure to minimize the risk for the user and environment. A risk management plan based on the risk assessment over all individual components of an assay has to be created. For modified CE-labelled assays, the manufacturer's safety data sheets for the assay and the additional components added (e.g. diluent) can be used.", rows = 5),
                                       br(),
                                       HTML("Click <a href='https://eur-lex.europa.eu/legal-content/EN/TXT/HTML/?uri=CELEX:02017R0746-20230320#tocId1131
' target='_blank'>here</a> for the official excerpt of the IVDR")
                                       
                                     )
                                   }
                                   
                                   
                                   else if (i ==5)
                                   {
                                     div(
                                       textAreaInput(paste0("text", i), label = paste("Enter text"),
                                                     placeholder = "Implementation of a system to detect and evaluate effects that can pose an unnecessary risk or damage by implementing appropriate measures", rows = 5),
                                       br(),
                                       HTML("Click <a href='https://eur-lex.europa.eu/legal-content/EN/TXT/HTML/?uri=CELEX:02017R0746-20230320#tocId115
' target='_blank'>here</a> for the official excerpt of the IVDR")
                                       
                                       
                                     )
                                   }
                                   
                                   else if (i ==6)
                                   {
                                     div(
                                       textAreaInput(paste0("text", i), label = paste("Enter text"),
                                                     placeholder = "e.g., Consider using QC tool", rows = 5),
                                     )
                                   }
                                   
                                   else if (i ==7)
                                   {
                                     div(
                                       textAreaInput(paste0("text", i), label = paste("Enter text"),
                                                     placeholder = "e.g., Consider using MC tool", rows = 5),
                                     )
                                   }
                                   
                                   else {
                                     textAreaInput(paste0("text", i), label = paste("Enter text"),
                                                   placeholder = "Enter text here...", rows = 5)
                                   },
                                   
                                   actionButton(paste0("resetBtn", i), "", class = "reset-btn",
                                                style = "background-color: transparent; border: none; font-size: 20px",
                                                icon = icon("trash", lib = "font-awesome", class = "fa-lg"))
                                 )
                               )
                             }),
                             
                             # additional textAreaInput area for extra comments
                             textAreaInput("extraText", label = "Further comments", placeholder = "Enter text", rows =5)
                             
                           )
                    )
                  )
                  
                )
              )
      ),
      
      tabItem(tabName = "MCTool",
              
              actionButton('action',
                           label = "Calculate",
                           color = "success",
                           icon = icon("calculator"),
                           block = TRUE,
                           style="color: #fff; background-color: #003B73; border-color: black"),
              tags$br(),
              tags$br(),
              
              fluidRow(
                box(width=4,
                    status = "primary",
                    title = "MC Tool",
                    h4("Enter details of method x and y"),
                    solidHeader = TRUE,
                    textInput("descx", NULL, "X"),
                    h5("Reference range (from/to)"),
                    fluidRow(
                      column(4, numericInput("refllx", NULL, "0")),
                      column(4, numericInput("refulx", NULL, "0"))
                    ),
                    textInput("descy", NULL, "Y"),
                    h5("Reference range (from/to)"),
                    fluidRow(
                      column(4, numericInput("reflly", NULL, "0")),
                      column(4, numericInput("refuly", NULL, "0"))
                    ),
                    h5("Enter or copy/paste raw values for method x and y"),
                    rHandsontableOutput("mctable"),
                    uiOutput("validation_message")  # Display validation message
                    
                ),
                box(
                  status = "primary",
                  title = "Plots",
                  solidHeader = TRUE,
                  plotOutput("pbblot"),
                  plotOutput("diffplot")
                )
              )
      ),
      
      tabItem(tabName = "QCTool",
              
              actionButton('action2',
                           label = "Calculate",
                           color = "success",
                           icon = icon("calculator"),
                           block = TRUE,
                           style="color: #fff; background-color: #003B73; border-color: black"),
              tags$br(),
              tags$br(),
              
              fluidRow(
                box(
                  status = "primary",
                  title = "QC Tool",
                  solidHeader = TRUE,
                  h4("Analyte"),textInput('analyte', label = NULL, value = "Analyte"),
                  h4("Description"),textInput('description', label = NULL, value = NULL),
                  h4("QC type"),radioButtons("withbetw",NULL,
                                             c("within-run" = "withinrun", "between-run" = "betweenrun")
                  ),
                  h4("Target values"),
                  
                  fluidRow(
                    column(4, numericInput('qc1target', label = 'QC1', value = 0)),
                    column(4, numericInput('qc2target', label = 'QC2', value = 0)),
                    column(4, numericInput('qc3target', label = 'QC3', value = 0))
                  ),
                  rHandsontableOutput("QCtable")
                ),
                box(
                  status = "primary",
                  title = "Results",
                  solidHeader = TRUE,
                  tableOutput("restab")
                )
              ),
              
              mainPanel()
      ),
      
      
      tabItem(
        tabName = "ChemSoft",
        fluidRow(
          box(
            status = "primary",
            title = "List of used chemicals and software",
            solidHeader = TRUE,
            width = 12, 
            rHandsontableOutput("exctable",width = 800, height = 800)
            
          )
        )
      )
      
    ),
    use_theme(IVDCheckR_theme)
    
  )
)

server <- function(input, output, session) {
  
  
  observeEvent(input$projectInput, {
    # Remove special characters from projectInput
    updateTextInput(session, "projectInput", value = gsub("[^[:alnum:] ]", "", input$projectInput))
  })
  
  # for the start button which redirects to the project details tab
  observeEvent(input$startButton, {
    updateTabsetPanel(session = session, inputId = "tabs", selected = "Project_Details")
  })
  
  #ChemSoft
  # Create the initial data frame
  initial_data <- data.frame(
    Product = "",
    Manufacturer = "",
    Product_number = "",
    Use_case = "",
    Refered_document = "",
    stringsAsFactors = FALSE
  )
  
  # Define the columns for the table
  columns <- data.frame(
    title = c('Product/Instrument/Software', 'Manufacturer', 'Product Number', 'Use Case','Referred Document'),
    type = c("text","text","text","text","text"),
    stringsAsFactors = FALSE
  )
  
  
  # Rename the columns
  renamed_data <- dplyr::rename(initial_data,
                         "Product/Instrument/Software" = Product,
                         "Manufacturer"= Manufacturer,
                         "Product Number"= Product_number,
                         "Use Case" = Use_case,
                         "Referred Document" = Refered_document)
  
  # Define the output
  output$exctable <- renderRHandsontable({
    rhandsontable(renamed_data,manualColumnResize = TRUE) |>
      hot_context_menu(allowRowEdit = T, allowColEdit = T,useTypes = FALSE) |>
      onRender(jsCode)  
    
  })
  
  
  
  # output$exctable <- renderRHandsontable({
  #   rhandsontable(initial_data, col_headers = c('Product/Instrument/Software', 'Manufacturer', 'Product Number', 'Use Case', 'Referred Document'), manualColumnResize = TRUE, manualRowResize = TRUE) |>
  #     onRender(jsCode) |> 
  #     hot_col(c("Product", "Manufacturer", "Product_number", "Use_case", "Refered_document"), type = "text")
  # })
  
  # changing column names
  # output$exctable <- renderRHandsontable(rhandsontable(rename(initial_data,
  #                                                           "Product/Instrument/Software" = Product,
  #                                                          "Manufacturer"= Manufacturer,
  #                                                          "Product Number"= Product_number,
  #                                                          "Use Case" = Use_case,
  #                                                          "Referred Document" = Refered_document)))
  
  
  
  # Individual reset buttons for each file input
  for (i in 1:7) {
    observeEvent(input[[paste0("file", i)]], {
      shinyjs::enable(paste0("resetBtn", i))
    })
  }
  
  observe({
    lapply(1:7, function(i) {
      observeEvent(input[[paste0("resetBtn", i)]], {
        shinyjs::reset(paste0("file", i))
      })
    })
  })
  
  #mc part
  mcdf <- data.frame(x = rep(NA, 50),
                     y = rep(NA, 50))
  mcdf$x = as.numeric(mcdf$x)
  mcdf$y = as.numeric(mcdf$y)
  
  output$mctable <- 
    renderRHandsontable(rhandsontable(mcdf, width = 500, height = 800))
  
  
  plot1 <- function(mcdf) {
    par(pty = "s")
    reg <- mcreg(mcdf$x, mcdf$y, method.reg = "PaBa")
    MCResult.plot(
      reg,
      main = input$method,
      sub = " ",
      equal.axis = TRUE,
      x.lab = input$descx,
      y.lab = input$descy,
      points.cex = 1,
      reg = TRUE,
      reg.col = "#003f6b",
      identity.col = "#00abda",
      add.grid = FALSE,
      add.legend = FALSE,
      ci.area = TRUE,
      add.cor = TRUE,
      cor.method = "spearman"
    )
    list(reg)
    includeLegend(
      colors = c("#003f6b"),
      model.names = c("PaBa"),
      models = list(reg),
      bg = NULL,
      cex = 1.0,
      design = "2",
      digits = 2
    )
    refrangecol = adjustcolor("black", alpha.f = 0.05)
    if (input$refllx >= 0 &&
        input$reflly >= 0 &&
        input$refulx > input$refllx &&
        input$refuly > input$reflly) {
      rect(
        input$refllx,input$reflly,input$refulx,input$refuly,
        col = refrangecol,
        border = FALSE
      )
      abline(v = input$refllx, col = refrangecol)
      abline(v = input$refulx, col = refrangecol)
      abline(h = input$reflly, col = refrangecol)
      abline(h = input$refuly, col = refrangecol)
    }
  }
  
  plot2 <- function(mcdf) {
    par(pty = "s")
    mean <- (mcdf$x + mcdf$y) / 2
    diff <- (mcdf$y - mcdf$x) * 100 / mean
    plot(
      mean,
      diff,
      axis = TRUE,
      main = "difference plot",
      xlab = "(x + y) / 2",
      ylab = "(y - x) / ((x + y) / 2) [%]",
      xlim = c(min(mcdf), max(mcdf)),
      ylim = c(min(c(diff,-100)), max(c(diff, 100)))
    )
    abline(h = 0,col = "#00abda",lty = 2)
    abline(h = mean(diff), col = "#003f6b")
    abline(h = mean(diff) - sd(diff),col = "gray",lty = 2)
    abline(h = mean(diff) + sd(diff),col = "gray",lty = 2)
  }
  
  
  observeEvent(input$action, {
    mcdf <- hot_to_r(input$mctable)
    mcdf <- mcdf[!is.na(as.numeric(mcdf$x)),]
    mcdf <- mcdf[!is.na(as.numeric(mcdf$y)),]
    output$pbblot <- renderPlot(plot1(mcdf))
    output$diffplot <- renderPlot(plot2(mcdf))
  })
  
  # QC part
  df <-
    data.frame(qc1 = rep(NA, 21),
               qc2 = rep(NA, 21),
               qc3 = rep(NA, 21))
  df$qc1 = as.numeric(df$qc1)
  df$qc2 = as.numeric(df$qc2)
  df$qc3 = as.numeric(df$qc3)
  
  calcs <- function(df) {
    cv_qc1 <- sd(df$qc1, na.rm = TRUE) / mean(df$qc1, na.rm = TRUE) * 100
    cv_qc2 <- sd(df$qc2, na.rm = TRUE) / mean(df$qc2, na.rm = TRUE) * 100
    cv_qc3 <- sd(df$qc3, na.rm = TRUE) / mean(df$qc3, na.rm = TRUE) * 100
    bias_qc1 <- (mean(df$qc1, na.rm = TRUE) - input$qc1target) / input$qc1target * 100
    bias_qc2 <- (mean(df$qc2, na.rm = TRUE) - input$qc2target) / input$qc2target * 100
    bias_qc3 <- (mean(df$qc3, na.rm = TRUE) - input$qc3target) / input$qc3target * 100
    rmsd_qc1 <- sqrt(mean((df$qc1 - input$qc1target) ^ 2, na.rm = TRUE)) / input$qc1target * 100
    rmsd_qc2 <- sqrt(mean((df$qc2 - input$qc2target) ^ 2, na.rm = TRUE)) / input$qc2target * 100
    rmsd_qc3 <- sqrt(mean((df$qc3 - input$qc3target) ^ 2, na.rm = TRUE)) / input$qc3target * 100
    
    QCresults <-
      data.frame(
        c(mean(df$qc1, na.rm = TRUE),input$qc1target,cv_qc1,bias_qc1,rmsd_qc1),
        c(mean(df$qc2, na.rm = TRUE),input$qc2target,cv_qc2,bias_qc2,rmsd_qc2),
        c(mean(df$qc3, na.rm = TRUE),input$qc3target,cv_qc3,bias_qc3,rmsd_qc3)
      )
    colnames(QCresults) <- c("QC1", "QC2", "QC3")
    rownames(QCresults) <- c("Mean", "Target value", "CV [%]", "Bias [%]", "RMSD [%]")
    QCresults
  }
  
  # Define rendering of QC table
  output$QCtable <- renderRHandsontable({
    rhandsontable(df, width = 500, height = 800)
  })
  
  # Define rendering of QC results
  output$restab <- renderTable({
    if (!is.null(input$QCtable) && nrow(df) >= 2) {
      df <- hot_to_r(input$QCtable)
      calcs(df)
    }
  }, rownames = TRUE)
  
  # Define the server logic for processing QC data upon button click
  observeEvent(input$action2, {
    if (!is.null(input$QCtable) && nrow(df) >= 2) {
      df <- hot_to_r(input$QCtable)
      output$restab <- renderTable(calcs(df), rownames = TRUE)
    }
  })
  
  
  output$downloadBtn <- downloadHandler(
    filename = function() {
      title <- input$projectInput
      filename <- paste0(gsub(" ", "_", title), ".pdf")
      return(filename)
    },
    
    setwd(tempdir()),
    
    content = function(file) {
      # Retrieve user inputs
      projectInput <- input$projectInput
      DateRange = paste(input$dates[1], input$dates[2], sep = " - ")
      ParameterInfo <- input$ParameterInfo
      ParameterUse <- input$ParameterUse
      Indication <- input$Indication
      ScientificValidity <- input$ScientificValidity
      MeasurementScale <- input$MeasurementScale
      SampleMaterial <- input$SampleMaterial
      others_SampleMaterial <- input$others_SampleMaterial
      RiskClass <- input$RiskClass
      RiskClassDetails <- input$RiskClassDetails
      ModificationProduct <- input$ModificationProduct
      TextInputs <- paste(input$text1, input$text2, sep = "\n")  # Combine the text inputs
      SOPInput = input$text1
      JustificationInput = input$text2
      DeclarationInput = input$text3
      RiskInput = input$text4
      MonitoringInput = input$text5
      AnalyticalInput = input$text6
      ClinicalInput = input$text7
      extraText = input$extraText
      
      # MC part
      mcdf <- hot_to_r(input$mctable)
      mcdf <- mcdf[!is.na(as.numeric(mcdf$x)),]
      mcdf <- df[!is.na(as.numeric(mcdf$y)),]
      
      # QC part
      df = kable(hot_to_r(input$QCtable))
      QCresults = kable(calcs(hot_to_r(input$QCtable)), digits = 2)
      #QCresults <- calcs(df)
      analyte = input$analyte
      description = input$description
      withbetw = input$withbetw
      
      
      # Extract data from the Excel-like table
      table_data <- kable(hot_to_r(input$exctable))
      
      # Render the R Markdown file
      out <- rmarkdown::render(
        'ivdcheckr.rmd',
        output_file = "ivdcheckr.pdf",
        params = list(
          projectInput = projectInput,
          DateRange = paste(input$dates[1], input$dates[2], sep = " - "),
          ParameterInfo = ParameterInfo,
          ParameterUse = ParameterUse,
          Indication = Indication,
          ScientificValidity = ScientificValidity,
          MeasurementScale = MeasurementScale,
          SampleMaterial = SampleMaterial,
          others_SampleMaterial = others_SampleMaterial,
          RiskClass = RiskClass,
          RiskClassDetails = RiskClassDetails,
          ModificationProduct = ModificationProduct,
          SOPInput = SOPInput,
          JustificationInput = JustificationInput,
          DeclarationInput = DeclarationInput,
          RiskInput = RiskInput,
          MonitoringInput = MonitoringInput,
          AnalyticalInput = AnalyticalInput,
          ClinicalInput = ClinicalInput,
          extraText = extraText,
          
          df = df,
          QCresults = QCresults,
          analyte = analyte,
          description = description,
          withbetw = withbetw,
          
          table_data = table_data
        ),
        output_format = pdf_document()
      )
      
      # List to store paths of uploaded PDF files
      uploaded_pdf_paths <- c()
      
      # Check if PDF files were uploaded
      for (i in 1:7) {
        file_id <- paste0("file", i)
        if (!is.null(input[[file_id]]) && input[[paste0("fileOption", i)]] == 'Upload PDF and/or Add Text') {
          uploaded_pdf_paths <- c(uploaded_pdf_paths, input[[file_id]]$datapath)
        }
      }
      
      # List to store text inputs for SOPs and Declaration of Conformity
      text_inputs <- c()
      
      # Retrieve text inputs
      for (i in 1:7) {
        text_id <- paste0("text", i)
        if (!is.null(input[[text_id]])) {
          text_inputs <- c(text_inputs, input[[text_id]])
        }
      }
      
      # Combine the generated PDF with the uploaded PDF files and text inputs
      combined_pdf <- out
      if (length(uploaded_pdf_paths) > 0 || length(text_inputs) > 0) {
        combined_pdf <- file.path(tempdir(), "combined_ivdcheckr.pdf")
        pdf_files <- c(out, uploaded_pdf_paths)
        
        if (length(text_inputs) > 0) {
          # Convert text inputs to PDF and add to the list of files to combine
          
          for (text_input in text_inputs) {
            # Convert text to PDF
            text_pdf <- file.path(tempdir(), paste0("text_", digest(text_input), ".pdf"))
            pdf(text_pdf, width = 8.5, height = 11)
            cat(text_input)
            dev.off()
            pdf_files <- c(pdf_files, text_pdf)
          }
        }
        
        
        # Combine all PDF files
        pdf_combine(pdf_files, combined_pdf)
      }
      
      # Move the combined PDF to the download location
      file.rename(combined_pdf, file)
    }
  )
  
  ##################################### report as a word file ################################################
  
  generate_report_text <- function(projectInput,dates,ParameterInfo,ParameterUse,
                                   Indication,ScientificValidity,MeasurementScale,
                                   SampleMaterial,
                                   others_SampleMaterial,
                                   RiskClass,
                                   RiskClassDetails,
                                   ModificationProduct,
                                   TextInputs,  # Combine the text inputs
                                   SOPInput,
                                   DeclarationInput, 
                                   AnalyticalInput,
                                   RiskInput,
                                   ClinicalInput,
                                   PostMarketInput,
                                   extraText,
                                   
                                   # MC part
                                   mcdf,
                                   
                                   # QC part
                                   df,
                                   QCresults,
                                   analyte,
                                   description,
                                   withbetw
  )
  
  {
    
    # Generate the Markdown text
    report_text <-
      
      paste0(
        
        "## **Project title =**", projectInput, "\n",
        
        "Date =", dates, "\n",
        
        "### Analyte Description = " , ParameterInfo, "\n",
        
        "## Intended use of the analyte = ", ParameterUse, "\n",
        
        "## Indication = ", Indication, "\n",
        
        "## Scientific validity = ", ScientificValidity, "\n",
        
        "## Measurement scale = " , MeasurementScale, "\n",
        
        "## Sample material = ", SampleMaterial, "\n",
        
        "## Other sample material = ", others_SampleMaterial, "\n",
        
        "## Risk class = ", RiskClass, "\n",
        
        "## Modification of a existing product (standard) OR new product/development = ", ModificationProduct)
    
    report_text
  }
  
  # Create a temporary RMarkdown file
  temp_report_rmd <- tempfile(fileext = ".Rmd")
  
  # Define the output PDF file
  output_pdf <- tempfile(fileext = ".pdf")
  
  observe({
    # Generate RMarkdown report text
    report_text <- generate_report_text(input$projectInput,input$dates,input$ParameterInfo,input$ParameterUse,
                                        input$Indication,input$ScientificValidity,input$MeasurementScale,
                                        input$SampleMaterial,input$others_SampleMaterial,input$RiskClass,
                                        input$RiskClassDetails,input$ModificationProduct,
                                        paste(input$text1, input$text2, sep = "\n"),  # Combine the text inputs
                                        input$text1,input$text2,input$text3,input$text4,
                                        input$text5,input$text6,input$extraText,
                                        
                                        # MC part
                                        mcdf <- hot_to_r(input$mctable),
                                        mcdf <- mcdf[!is.na(as.numeric(mcdf$x)),],
                                        mcdf <- df[!is.na(as.numeric(mcdf$y)),],
                                        
                                        # QC part
                                        df = kable(hot_to_r(input$QCtable)),
                                        QCresults = kable(calcs(hot_to_r(input$QCtable)), digits = 2),
                                        #QCresults <- calcs(df)
                                        input$analyte)
    
    # Create a temporary directory to store intermediate files
    temp_dir <- tempdir()
    
    # Write the RMarkdown content to a temporary file
    temp_report_rmd <- file.path(temp_dir, "report.Rmd")
    writeLines(report_text, temp_report_rmd)
    
    
  })
  
}



shinyApp(ui, server)