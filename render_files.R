########################################################
### Code for probabilistic uncertainty analysis 
### to support BPA Opinion 2021
### Prepared by Ullrika Sahlin, Lund University
### based on instructions from Andy Hart 
########################################################
### run first to get a modified version of the SHELF package
#detach("package:SHELF", unload = TRUE)
#devtools::install_github("Ullrika/SHELF")
#library(SHELF)

hocs = c('immunotox','metabolic','neurotox','repro','carcino')
## FIT DISTRIBUTIONS
{
  
for(hid in c(1,2,3,4,5)){
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
                                  figdir = paste0("fig_fit_distributions",hoc,"/")))
}
  Q1_ALI = c(50,90)
  hoc <- hocs[1]
  rmarkdown::render("PBA_health_outcome_category.Rmd",
                    output_format = c("html_document"),
                     output_file = paste0("PBA_",hoc,"_revisedjudgements_Q1_ALI",Q1_ALI[1],"_",Q1_ALI[2]),
                       output_dir = "figs01",
                    params = list(hocs = hocs,
                                  probmassoutside = 0.001,
                                  origALI = FALSE,
                                  hoc = hoc,
                                  fit_distributions = TRUE,
                                  Q1_ALI = Q1_ALI,
                                  figdir = paste0("fig_fit_revised_",hoc,"/")))
}


## RUN COMBINATION with sensitivity analysis
## original and revised judgments on allergic lung inflammation  
{
for(origALI in c(TRUE,FALSE)){
  Q1_ALI = c(50,90)

rmarkdown::render("PBA_combination.Rmd",output_format = c("html_document"),
                  output_file = paste0("PBA_combination_SA_",origALI,Q1_ALI[1],Q1_ALI[2]),
                  output_dir = "figs01",
                  params = list(hocs = hocs,
                                probmassoutside = 0.001,
                  origALI = origALI,
                  fit_distributions = FALSE,
                  Q1_ALI = Q1_ALI,
                  figdir = paste0("fig_combination",origALI,"/")))

}
}


## RUN COMBINATION with linear pooling
{
  q1_low = c(66, 33, 50)
  q1_up = c(66, 90, 90)
    
    qqq = 1
    Q1_ALI = c(q1_low[qqq],q1_up[qqq])
      rmarkdown::render("PBA_combination_linearpool.Rmd",output_format = c("html_document"),
                      output_file = paste0("PBA_combination_linearpool_",Q1_ALI[1],"_",Q1_ALI[2]),
                      output_dir = "figs01",
                      params = list(hocs = hocs,
                                    probmassoutside = 0.001,
                                    origALI = FALSE,
                                    fit_distributions = FALSE,
                                    Q1_ALI = Q1_ALI,
                                    SADEP = TRUE,
                                    usesavedoutputs = TRUE,
                                    figdir = paste0("fig_linearpool",Q1_ALI[1],"_",Q1_ALI[2],"/")))
    
    for(qqq in 2:3){
      Q1_ALI = c(q1_low[qqq],q1_up[qqq])
      rmarkdown::render("PBA_combination_linearpool.Rmd",output_format = c("html_document"),
                        output_file = paste0("PBA_combination_linearpool_",Q1_ALI[1],"_",Q1_ALI[2]),
                        output_dir = "figs01",
                        params = list(hocs = hocs,
                                      probmassoutside = 0.001,
                                      origALI = FALSE,
                                      fit_distributions = FALSE,
                                      Q1_ALI = Q1_ALI,
                                      SADEP = FALSE,
                                      usesavedoutputs = FALSE,
                                      figdir = paste0("fig_linearpool",Q1_ALI[1],"_",Q1_ALI[2],"/")))
      
    }
}


## Extract and save calculated percentiles into one excelfile 
{
OUT <- createWorkbook()
addWorksheet(OUT,"env")
addWorksheet(OUT,"linpool")

for(qqq in 1:length(q1_low)){ 
  Q1_ALI = c(q1_low[qqq],q1_up[qqq])
  # Write the data to the sheets
  load(file=paste0("env_",Q1_ALI[1],"_",Q1_ALI[2]))
  load(file=paste0("linpool_",Q1_ALI[1],"_",Q1_ALI[2]))
  if(qqq == 1){
  writeData(OUT, sheet = "env", x = df_calc_perc, startCol = 1,startRow = 2)
    df_cm <- data.frame(perc = df_cm$perc,
                        expert = df_calc_multi$expert,
                        lower = df_cm$lower, 
                        upper = df_cm$upper)
      writeData(OUT, sheet = "linpool", x = df_cm, startCol = 1,startRow = 2)
   } else{
      writeData(OUT, sheet = "env", x = df_calc_perc[,c("lower" ,"upper")], startCol =  (2*qqq-1)+1,startRow = 2)
      writeData(OUT, sheet = "linpool", x = df_cm[,c("lower" ,"upper")], startCol =  (2*qqq-1)+2,startRow = 2)
    }
  writeData(OUT, sheet = "env", x = paste0(Q1_ALI[1],"_",Q1_ALI[2]),
            startCol = (2*qqq-1)+1,startRow = 1)
  writeData(OUT, sheet = "env", x = paste0(Q1_ALI[1],"_",Q1_ALI[2]),
            startCol = (2*qqq-1)+2,startRow = 1)
  writeData(OUT, sheet = "linpool", x = paste0(Q1_ALI[1],"_",Q1_ALI[2]),
            startCol = (2*qqq-1)+2,startRow = 1)
  writeData(OUT, sheet = "linpool", x = paste0(Q1_ALI[1],"_",Q1_ALI[2]),
            startCol = (2*qqq-1)+3,startRow = 1)
}
saveWorkbook(OUT, "percentiles_differentQ1ALI.xlsx", overwrite = TRUE)
}

