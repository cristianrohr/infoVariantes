library(httr)
library(jsonlite)
library(xml2)
library(urltools)
library(myvariant)
library(magrittr)
library(plyr)
library(dplyr)

# --------------------
# Funciones auxiliares
# --------------------

format_g <- function(chr, pos, ref, alt){
  paste0("chr", chr, ":", pos, ref, ">", alt)
}

format_g_row <- function(chr, pos, ref, alt){
  
  if(ref != "-"){
    if(alt == "-"){
      #delecion
      variant <- paste0("del")
    }
    else{
      #snp
      variant <- paste0(ref, ">", alt)
    }
  }
  else{
    #insercion
    variant <- paste0("ins", alt)
  }
  
  gsub(" ", "", paste0(chr, ":g.", pos, variant))
}


# --------------------
# Funciones principales
# --------------------


# Function: get_variant_data
# Descripción: Función para obtener la información de la variante
# Parámetros
# chr: cromosoma
# pos: posición
# ref: alelo de referencia
# alt: alelo alternativo
get_variant_data <- function(chr, pos, ref, alt){
  
  hgvs_g <- format_g(chr, pos, ref, alt)
  d <- getVariant(hgvs_g)[[1]]
  
  gnomad_af_exome  <- d[['gnomad_exome']][['af']]
  if(!is.null(gnomad_af_exome)) {
    gnomad_af_exome  <- ldply(gnomad_af_exome, data.frame)
    names(gnomad_af_exome) <- c("id", "value")
    gnomad_af_exome$summary <- paste0(gnomad_af_exome$id, ": ", gnomad_af_exome$value)
    gnomad_af_exome <- paste0(gnomad_af_exome$summary, collapse = " | ")
  }
  
  
  gnomad_af_genome <- d[['gnomad_genome']][['af']]
  if(!is.null(gnomad_af_genome)) {
    gnomad_af_genome  <- ldply(gnomad_af_genome, data.frame)
    names(gnomad_af_genome) <- c("id", "value")
    gnomad_af_genome$summary <- paste0(gnomad_af_genome$id, ": ", gnomad_af_genome$value)
    gnomad_af_genome <- paste0(gnomad_af_genome$summary, collapse = " | ")
  }
  
  
  gene <- d[['clinvar']][['gene']][['symbol']]
  hgvs_c <- d[['clinvar']][['hgvs']][['coding']]
  clinvar_entries <- d[['clinvar']][['rcv']]
  clinvar_entries <- ldply (clinvar_entries, data.frame)[,c("clinical_significance", "name", "last_evaluated", 
                                                            "origin", "conditions.name")]
  clinvar_entries <- clinvar_entries[clinvar_entries$origin == "germline",]
  clinvar_entries$disease <- apply(clinvar_entries, 1, function(row){
    if (is.na(row[['name']])){
      row[['conditions.name']]
    }else if (is.na(row[['conditions.name']])){
      row[['name']]
    }else{
      #ambas 
      paste(row[['name']], row[['conditions.name']], sep = " | ")
    }
  })
  clinvar_entries$name <- NULL
  clinvar_entries$conditions.name <- NULL
  clinvar_entries$origin <- NULL
  clinvar_entries_2 <- clinvar_entries %>% group_by(clinical_significance)  %>% summarise(le = paste(unique(last_evaluated[!is.na(last_evaluated)]), collapse = "|" ),
                                                                                          dis = paste(unique(disease[!is.na(disease)]), collapse = "|"),
                                                                                          n = n())
  
  clinvar_entries_2$summary <- paste0(clinvar_entries_2$clinical_significance, " (", clinvar_entries_2$n, ") [", 
                                      clinvar_entries_2$le, "] [", clinvar_entries_2$dis, "]")
  clinvar_entries_2$n <- NULL
  clinvar_entries_2$le <- NULL
  clinvar_entries_2$dis <- NULL
  clinvar_entries_2$clinical_significance <- NULL
  
  clinvar_entries <- paste0(clinvar_entries_2$summary, collapse = " | ")
  data.frame(gene = gene, hgvsc = hgvs_c, gnomad_exome = gnomad_af_exome, gnomad_genome = gnomad_af_genome, clinvar = clinvar_entries)
}

get_variants_data <- function (variantList){
  f <- data.frame(getVariants(variantList))
  g <- f[,names(f) %in% variant_fields]
  return(g)
}
