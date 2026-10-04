# regenerate the publication exhibits from one completed cost run so the figures and tables share inputs.
# regenerate the publication exhibits from one completed cost run so the figures and tables share inputs.
root_R1 <- Sys.getenv("PHASED_R1_ROOT", unset=getwd())
if (!file.exists(file.path(root_R1, "README.md"))) stop("Set PHASED_R1_ROOT to the repository root")
home_R1 <- file.path(root_R1,'7 Reproduction Information Document/Final results review R1')
cost_home_R1 <- file.path(root_R1,'7 Reproduction Information Document/Cost production audit R1')
output_R1 <- readLines(file.path(cost_home_R1,'Last_output_R1.txt'),warn=FALSE)[1]
stopifnot(file.exists(file.path(output_R1,'COMPLETE.txt')))
python_R1 <- Sys.getenv('PHASED_PYTHON_R1',unset=Sys.which("python3"))
Sys.setenv(PHASED_COST_OUTPUT_R1=output_R1,
 PHASED_REVIEW_OUTPUT_R1=file.path(home_R1,'Results R1'),
 MPLCONFIGDIR=file.path(tempdir(),'phased_review_matplotlib'))
source(file.path(cost_home_R1,'4 Generate final cost figures_R1.R'))
# Generate the publication diagrams and Figures 6 and 7 after the summary outputs.
for(script_R1 in c('Review_full_ensemble_R1.py',
 'Compare_AP4_full_ensemble_R1.py','Review_cost_coverage_R1.py',
 'Audit_facility_inventory_R1.py','Variable_distributions_R1.py','Author_figures_R1.py')) {
 status_R1 <- system2(python_R1,c('-s',shQuote(file.path(home_R1,script_R1))))
 if(status_R1!=0)stop('Review output failed: ',script_R1)
}
cat('Figures and tables: ',file.path(home_R1,'Results R1'),'\n',sep='')
