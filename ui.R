# Cargo las librerías necesarias
library(shiny)
library(plotly)
library(ggthemr)
library(shinymaterial)
ggthemr('fresh')

# Interfaz
material_page(
  title = "Recuperación Información Variantes",
  nav_bar_color = "deep-purple darken-4",
  
  # Codigo del navbar
  material_side_nav(
    fixed = TRUE, 
    material_side_nav_tabs(
      side_nav_tabs = c(
        "Información variantes" = "variant_info",
        "Acerca del trabajo" = "about_cr"
      ),
      icons = c("explore", "code")
    ),
    tags$br(),
    tags$br(),
    tags$hr(),
    material_row(
      material_column(
        width = 12,
        tags$br(),
        tags$br(),
        tags$b("Ejemplo de consulta"),
        tags$p("cromosoma: 7, posición: 140453136"),
        tags$p("referencia: A, alternativa: T"),
        actionButton("buscar_ejemplo", "Ejecutar ejemplo"),
        tags$br(),
        tags$hr(),
        tags$br(),
        tags$b("Variante"),
        material_text_box(
          input_id = "inChr",
          label = "Cromosoma"
        ),
        material_text_box(
          input_id = "inPos",
          label = "Posición"
        ),
        material_text_box(
          input_id = "inRef",
          label = "Alelo Referencia"
        ),
        material_text_box(
          input_id = "inAlt",
          label = "Alelo Alternativo"
        ),
        actionButton("buscar_variante", "Recuperar información")
      )
    )
  ), # Fin navbar
  material_side_nav_tab_content(
    side_nav_tab_id = "variant_info",
    material_tabs(
      tabs = c(
        "Reporte" = "tab_1"
      )
    ),
    material_tab_content(
      tab_id = "tab_1",
      material_row(
        material_column(
          width = 6,
          material_card(
            title = "Información variante",
            depth = 4,
            #textOutput("geneVar"),
            #textOutput("vartypeVar")
            
            material_row(
              material_column(width = 2, tags$b("Gen")),
              material_column(width = 2, tags$b("Cromosoma")),
              material_column(width = 2, tags$b("Posición")),
              material_column(width = 2, tags$b("Referencia")),
              material_column(width = 2, tags$b("Alternativa")),
              material_column(width = 2, tags$b("dbSNP IDB"))
            ),
            material_row(
              material_column(width = 2, textOutput("geneVar")),
              material_column(width = 2, textOutput("chrVar")),
              material_column(width = 2, textOutput("posVar")),
              material_column(width = 2, textOutput("refVar")),
              material_column(width = 2, textOutput("altVar")),
              material_column(width = 2, textOutput("idVar"))
            ),
            material_row(
              material_column(width = 2, tags$b("ID transcripto")),
              material_column(width = 2, tags$b("Cambio nucleótido")),
              material_column(width = 2, tags$b("Cambio proteína")),
              material_column(width = 2, tags$b("Tipo variante")),
              material_column(width = 2, tags$b("Efecto variante")),
              material_column(width = 2, tags$b("Impacto variante"))
            ),
            material_row(
              material_column(width = 2, textOutput("tidVar")),
              material_column(width = 2, textOutput("ncVar")),
              material_column(width = 2, textOutput("pcVar")),
              material_column(width = 2, textOutput("tipovarVar")),
              material_column(width = 2, textOutput("efvarVar")),
              material_column(width = 2, textOutput("imvarVar"))
            )
            
            
          )
        ),
        material_column(
          width = 6,
          material_card(
            title = "Información Civic",
            depth = 4,
            textOutput("civic_info")
            #,
            #dataTableOutput("variant_cosmic")
          )
        )
          
        )
      ),
      material_row(
        material_column(
          width = 6,
          #offset = 2,
          material_card(
            title = "Clinvar",
            depth = 4,
            dataTableOutput("out_clinvar")
          )
        ),
        material_column(
          width = 6,
          material_card(
            title = "Información Cosmic",
            depth = 4,
            #textOutput("civic_info")
            #,
            dataTableOutput("variant_cosmic")
          )
      )
    )
    
    
  ),
  material_side_nav_tab_content(
    side_nav_tab_id = "about_cr",
    material_row(
      material_column(
        width = 6,
        offset = 3,
        material_card(
          title = "Acerca de",
          tags$p("Alumno: Cristian Rohr"),
          tags$br(),
          tags$p("Implementación para el curso Sistemas de Recuperación de Información y Recomendación")
        )
      )
    )
  )
  
)