#'---
#'  title:  "Source script with functions to format & export LMER Chi2 and PostHoc test tables to word"
#'  author: "Milou Sep"
#'  date: "8/29/2018"
#'  output: html_document
#'---

# Load required packages --------------------------------------------------
library(flextable) # To make publication ready tables for word | info: https://davidgohel.github.io/flextable/articles/overview.html
library(officer) # https://davidgohel.github.io/flextable/articles/layout.html   # To export chi tables to word

# Function for Chi2 Tables ------------------------------------------------
Export.Chi2.Table <- function(Chi2Table, TableName, file.export){
  # Create Flextable
  Chi2_Table <- flextable(Chi2Table)
  # Add Layout settings to flextable.
  # Make significant value's Bold [Info: https://cran.r-project.org/web/packages/flextable/vignettes/format.html#bold]
  Chi2_Table <-  bold(Chi2_Table, i= ~pvalue < 0.05 , j="pvalue")  # NB is j= is included, only value in not complete row bold
  #italic(sAA.PostHoc.Table, i= ~pvalue < 0.05 )#, j="pvalue")
  # set digits  # https://davidgohel.github.io/flextable/articles/format.html#set_formatter-function
  Chi2_Table <- display(Chi2_Table, col_key="df", pattern = "{{df}}", formatters = list(df~sprintf("%.00f",df)))  # no decimals for df  [for 1 decimal: %.01f etc]
  Chi2_Table <- display(Chi2_Table, col_key="deltadf", pattern = "{{deltadf}}", formatters = list(deltadf~sprintf("%.00f",deltadf)))
  Chi2_Table <- theme_vanilla(Chi2_Table)  # remove thick lines
  Chi2_Table <- set_header_labels(Chi2_Table, LogLikelihood="Log Likelihood" ,deltadf= "delta df", pvalue="p-value")  # change names
  Chi2_Table <- autofit(Chi2_Table) # change table dimensions to content
  
  if (file.export == T){
    doc <- read_docx()
    doc <- body_add_flextable(doc, value = Chi2_Table, align="center")
    print(doc, target = paste0(TableName,".docx"))
  }
  
  return(Chi2_Table)
}

# Function for PostHoc Pairwise Comparisons Table -------------------------
Export.PostHoc.Table <- function(PostHocTable, TableName, file.export){
  # Create Flextable
  PostHoc_Table <- flextable(PostHocTable)
  # Add Layout settings to flextable.
  PostHoc_Table<- bold(PostHoc_Table, i= ~p.value < 0.05 )
  PostHoc_Table<- theme_vanilla(PostHoc_Table)  # dikke lijnen weg
  PostHoc_Table<- autofit(PostHoc_Table) # afmetingen tabel aanpassen aan content
  
  if (file.export == T){
    doc <- read_docx()
    doc <- body_add_flextable(doc, value = PostHoc_Table, align="center")
    print(doc, target = paste0(TableName,".docx"))
  }
  
  return(PostHoc_Table)
}
