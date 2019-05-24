#options(repos = BiocInstaller::biocinstallRepos())
#getOption("repos")
library(shiny)
library(ggplot2)
library(dplyr)
require(scales)
library(plotly)
library(shinymaterial)
library(DT)
library(rsconnect)
library(shinyWidgets)

options(scipen=999)


# Lógica del servidor
function(input, output, session) {
    
  observeEvent(input$buscar_ejemplo, {
    chr <- "7"
    pos <- "140453136"
    ref <- "A"
    alt <- "T"
    hgvs_g <- format_g(chr, pos, ref, alt)
    d <- getVariant(hgvs_g)[[1]]
    
    print(hgvs_g)
    
    output$out_clinvar <- renderDataTable({
      
      df <- as.data.frame(d$clinvar$rcv[[1]])
      df
      
    }, options = list(scrollX = TRUE))
    
    
    output$geneVar <- renderText({ 
      var <- ""
      if(length(d$dbsnp$gene) == 2) {
        var <- as.character(d$dbsnp$gene$symbol)
      }
      var
    })

    output$chrVar <- renderText({
      var <- ""
      var <- as.character(d$dbsnp$chrom)
      var
    })
    
    output$posVar <- renderText({
      var <- ""
      var <- pos
      var
    })
    
    output$refVar <- renderText({
      var <- ""
      var <- as.character(d$dbsnp$ref)
      var
    })
    
    output$altVar <- renderText({
      var <- ""
      var <- as.character(d$dbsnp$alt)
      var
    })
    
    output$idVar <- renderText({
      var <- ""
      var <- as.character(d$dbsnp$rsid)
      var
    })
    
    output$tipovarVar <- renderText({
      var <- ""
      var <- as.character(d$dbsnp$vartype)
      var
    })
    
    output$imvarVar <- renderText({
      var <- ""
      var <- as.character(d$snpeff$ann$putative_impact)
      var
    })
    
    output$efvarVar <- renderText({
      var <- ""
      var <- as.character(d$snpeff$ann$effect)
      var
    })
    
    output$pcVar <- renderText({
      var <- ""
      var <- as.character(d$snpeff$ann$hgvs_p)
      var
    })
    
    output$ncVar <- renderText({
      var <- ""
      var <- as.character(d$snpeff$ann$hgvs_c)
      var
    })
    
    output$tidVar <- renderText({
      var <- ""
      var <- as.character(d$snpeff$ann$feature_id)
      var
    })
    
    
    output$civic_info <- renderText({
      
      # Civic
      civic.data <- "No tiene informacion de civic"
      tryCatch(
        civic.data <- d$civic$description,
        error=function(e) print("No tiene informacion de civic")
      )
      
      civic.data
      
    })
    

    output$variant_cosmic <- renderDataTable({
      

      cosmic.data <- data.frame()
      
      tryCatch(
        cosmic.data <- as.data.frame(d$cosmic),
        error=function(e) print("No tiene informacion de cosmic")
      )
      
      cosmic.data
      
    }, options = list(scrollX = TRUE))
    
    
    
  })
  
  observeEvent(input$buscar_variante, {
    
    chr <- input$inChr
    pos <- input$inPos
    ref <- input$inRef
    alt <- input$inAlt
    
    if(chr == "" | pos == "" | ref == "" | alt == "") {
      sendSweetAlert(session, title = "Error", text = "Faltan datos de consulta", type = "error", btn_labels = "Ok")  
    } else {
      hgvs_g <- format_g(chr, pos, ref, alt)
      d <- getVariant(hgvs_g)[[1]]
      
      print(hgvs_g)
      
      print(hgvs_g)
      
      output$clinvar <- renderDataTable({
        
        df <- as.data.frame(d$clinvar$rcv[[1]])
        df
        
      }, options = list(scrollX = TRUE))
      
      
      output$geneVar <- renderText({ 
        var <- ""
        if(length(d$dbsnp$gene) == 2) {
          var <- as.character(d$dbsnp$gene$symbol)
        }
        var
      })
      
      output$chrVar <- renderText({
        var <- ""
        var <- as.character(d$dbsnp$chrom)
        var
      })
      
      output$posVar <- renderText({
        var <- ""
        var <- pos
        var
      })
      
      output$refVar <- renderText({
        var <- ""
        var <- as.character(d$dbsnp$ref)
        var
      })
      
      output$altVar <- renderText({
        var <- ""
        var <- as.character(d$dbsnp$alt)
        var
      })
      
      output$idVar <- renderText({
        var <- ""
        var <- as.character(d$dbsnp$rsid)
        var
      })
      
      output$tipovarVar <- renderText({
        var <- ""
        var <- as.character(d$dbsnp$vartype)
        var
      })
      
      output$imvarVar <- renderText({
        var <- ""
        var <- as.character(d$snpeff$ann$putative_impact)
        var
      })
      
      output$efvarVar <- renderText({
        var <- ""
        var <- as.character(d$snpeff$ann$effect)
        var
      })
      
      output$pcVar <- renderText({
        var <- ""
        var <- as.character(d$snpeff$ann$hgvs_p)
        var
      })
      
      output$ncVar <- renderText({
        var <- ""
        var <- as.character(d$snpeff$ann$hgvs_c)
        var
      })
      
      output$tidVar <- renderText({
        var <- ""
        var <- as.character(d$snpeff$ann$feature_id)
        var
      })
      
      
      output$civic_info <- renderText({
        
        # Civic
        civic.data <- "No tiene informacion de civic"
        tryCatch(
          civic.data <- d$civic$description,
          error=function(e) print("No tiene informacion de civic")
        )
        
        civic.data
        
      })
      
      
      output$variant_cosmic <- renderDataTable({
        
        
        cosmic.data <- data.frame()
        
        tryCatch(
          cosmic.data <- as.data.frame(d$cosmic),
          error=function(e) print("No tiene informacion de cosmic")
        )
        
        cosmic.data
        
      }, options = list(scrollX = TRUE))
    }
    
  })
  
}
