# Generate final AP4 figures from the completed 1000-run output in a separate folder.
# generate Figures 5, 6, S5 and S6 from one completed 1000-run AP4 cost output.
# No dispatch or cost recalculation.
ROOT_R1 <- Sys.getenv("PHASED_R1_ROOT", unset=getwd())
HOME_R1 <- file.path(ROOT_R1,'3 Total Costs/Cost production audit R1')
run_figures_R1 <- function() {
 suppressPackageStartupMessages(library(data.table))
 last <- file.path(HOME_R1,'Last_output_R1.txt')
 output <- Sys.getenv('PHASED_COST_OUTPUT_R1',unset='')
 if(!nzchar(output)) {
  if(!file.exists(last))stop('Run the final cost pipeline first')
  output <- readLines(last,warn=FALSE)[1]
 }
 if(!file.exists(file.path(output,'COMPLETE.txt')))stop('Cost run has no completion marker')
 input <- file.path(output,'Figure inputs R1')
 x <- fread(file.path(input,'All_Costs_per_Simulation.csv'),select=c('Simulation','Pathway'))
 if(!setequal(x$Simulation,1:1000)||nrow(x)!=9000L)stop('Expected complete final 1000-simulation cost tables')
 cfg <- fread(file.path(output,'Run_settings_R1.csv'))
 if(cfg$Air_model!='AP4_hybrid')stop('Select the primary AP4_hybrid output for final figures')
 old <- Sys.getenv(c('PHASED_FIGURE_INPUT_R1','PHASED_FIGURE_OUTPUT_R1','MPLCONFIGDIR'),unset=NA_character_)
 on.exit({for(k in names(old))if(is.na(old[[k]]))Sys.unsetenv(k) else do.call(Sys.setenv,setNames(list(old[[k]]),k))},add=TRUE)
 dest <- file.path(output,'Figures R1');dir.create(dest,showWarnings=FALSE)
 Sys.setenv(PHASED_FIGURE_INPUT_R1=input,PHASED_FIGURE_OUTPUT_R1=dest,MPLCONFIGDIR=file.path(tempdir(),'matplotlib_R1'))
 python <- Sys.getenv('PHASED_PYTHON_R1',unset=Sys.which("python3"))
 if(!file.exists(python))stop('Set PHASED_PYTHON_R1 to Python with pandas, numpy and matplotlib')
 script <- file.path(ROOT_R1,'6 Figures/Publication revision R1/1 Generate figures_R1.py')
 status <- system2(python,c('-s',shQuote(script)))
 if(status!=0)stop('Figure generation failed; review console')
 # refresh the familiar publication folder because its earlier files otherwise continue to show the 50-run preview.
 current <- list.files(dest,pattern='^Figure.*[.](png|svg|pdf|csv|json)$',full.names=TRUE)
 file.copy(current,file.path(ROOT_R1,'6 Figures/Publication revision R1'),overwrite=TRUE)
 cat('R1 cost figures saved to:',dest,'\n')
}
run_figures_R1()
