## run first
#detach("package:SHELF", unload = TRUE)
#library("SHELF")
# this code was run to generate the R-file used in PBA_combination.Rmd
# knitr::purl("PBA_health_outcome_category.Rmd") #then modified


hocs = c('immunotox','metabolic','neurotox','repro','carcino')
## FIT DISTRIBUTIONS AND SAVE INTO WORD FILE - FOR THE APPENDIX
{
  
  for(hid in c(5,4,3,2,1)){
    hoc = hocs[hid]
    rmarkdown::render("PBA_health_outcome_category_word.Rmd",
                      output_file = paste0("PBA_",hoc),
                      output_dir = "figs01_word",
                      params = list(hocs = hocs,
                                    probmassoutside = 0.001,
                                    origALI = TRUE,
                                    hoc = hoc,
                                    fit_distributions = TRUE,
                                    Q1_ALI = c(0,0),
                                    figdir = paste0("fig_fit_distributions_",hoc,"/")))
  }
}

## FIT DISTRIBUTIONS AND SAVE INTO HTML FILE
{
  
for(hid in c(5,4,3,2,1)){
  
  hoc = hocs[hid]
  rmarkdown::render("PBA_health_outcome_category.Rmd",
                    output_format = c("html_document"),
                    output_file = paste0("PBA_",hoc),
                    output_dir = "figs01",
                    params = list(hocs = hocs,
                                  probmassoutside = 0.001,
                                  origALI = TRUE,
                                  hoc = hoc,
                                  fit_distributions = TRUE,
                                  Q1_ALI = c(0,0),
                                  figdir = paste0("fig_fit_distributions_",hoc,"/")))
}
  
}

if(FALSE){#DO NOT RUN
metadata <- do.call('rbind',lapply(1:5,function(hid){
load(file=paste('mdf_',hocs[hid],'.Rdata'))
data.frame(group=hocs[hid],do.call('rbind',mdf_group))}))
write.csv(metadata,file='metadata.csv')
}
## RUN COMBINATION with sensitivity analysis
## original judgments envelope 
{
rmarkdown::render("PBA_combination.Rmd",output_format = c("html_document"),
                  output_file = "PBA_combination_SA",
                  output_dir = "figs01",
                  params = list(hocs = hocs,
                                probmassoutside = 0.001,
                  origALI = TRUE,
                  fit_distributions = FALSE,
                  Q1_ALI = c(0,0),
                  figdir = paste0("fig_combination","/")))
}


## RUN COMBINATION with linear pooling
#knitr::purl("PBA_combination_linearpool.Rmd") 
#modify to extract percentiles and save into PBA_combination_linearpool_percentiles.R
# and PBA_combination_linearpool_SA.R

## run sensitivity on linear pool

{
      rmarkdown::render("PBA_combination_linearpool.Rmd",output_format = c("html_document"),
                      output_file = "PBA_combination_linearpool",
                      output_dir = "figs01",
                      params = list(hocs = hocs,
                                    probmassoutside = 0.001,
                                    origALI = TRUE,
                                    fit_distributions = FALSE,
                                    Q1_ALI = c(0,0),
                                    SADEP = TRUE,
                                    usesavedoutputs = FALSE,
                                    figdir = paste0("fig_linearpool","/")))
    

}


## extract and save a calculated percentiles into one excelfile 
{
OUT <- createWorkbook()
addWorksheet(OUT,"env")
addWorksheet(OUT,"linpool")

  Q1_ALI = c(0,0)
  # Write the data to the sheets
  load(file=paste0("env_",Q1_ALI[1],"_",Q1_ALI[2]))
  load(file=paste0("linpool_",Q1_ALI[1],"_",Q1_ALI[2]))
  #if(qqq == 1){
  writeData(OUT, sheet = "env", x = df_calc_perc, startCol = 1,startRow = 2)
    df_cm <- data.frame(perc = df_cm$perc,
                        expert = df_calc_multi$expert,
                        lower = df_cm$lower, 
                        upper = df_cm$upper)
      writeData(OUT, sheet = "linpool", x = df_cm, startCol = 1,startRow = 2)

saveWorkbook(OUT, "percentiles_newP.xlsx", overwrite = TRUE)
}


