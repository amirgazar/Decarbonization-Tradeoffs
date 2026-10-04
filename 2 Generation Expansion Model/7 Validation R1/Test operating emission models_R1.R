# Purpose: Fit or verify unit operating-rate relationships and calculate emissions from final load/ramp rather than independently sampled masses.
# mathematical, fallback and validation-selection checks.
suppressPackageStartupMessages(library(data.table))
a<-commandArgs(TRUE);source(a[1]);out<-a[2]
check<-function(name,condition){if(!isTRUE(condition))stop('FAILED: ',name);cat('PASS:',name,'\n')}
set.seed(9102026)
n<-8000L;x<-runif(n,.05,1.05);delta<-runif(n,-.3,.3);g<-100*x
z<-data.table(Date=as.Date('2013-01-01')+seq_len(n)%/%24,Load_fraction=x,Ramp_fraction=delta,
 Load=pmin(5L,pmax(1L,ceiling(4*x))),Ramp=ifelse(delta>.1,3L,ifelse(delta< -.1,1L,2L)),Gross_Load_MW=g,
 mass=g*(.4+.2*x+.3*x*x+.1*delta+.1*x*delta))
f<-r1v2_fit(z)
check('Known continuous load/ramp coefficients recovered',max(abs(f$curve_load_ramp$coefficients-c(.4,.2,.3,.1,.1)))<1e-10)
v<-copy(z);v[,Date:=Date+400];sel<-r1v2_select(f,v)
check('Validation chooses the supported load/ramp curve',sel$model=='curve_load_ramp')
check('Sparse history cannot fit a continuous curve',is.null(r1v2_fit(z[1:100])$curve_load))
q<-copy(z);q[,mass:=Gross_Load_MW*.5];cfit<-r1v2_fit(q);cs<-r1v2_select(cfit,q)
check('Constant true rate keeps the fixed model',cs$model=='fixed')
q[,mass:=0];check('Zero observed mass retains a valid zero fixed rate',r1v2_select(r1v2_fit(q),q)$model=='fixed')
p<-list(fit=f,model='curve_load_ramp',r1_model='load_ramp',load_curve_fallback=TRUE)
e<-list(capacity=100,pollutants=setNames(rep(list(p),4),c('CO2','NOx','SO2','HI')))
gen<-c(0,51,51.1,200,50);prev<-c(50,50,50,180,50);con<-c(TRUE,TRUE,TRUE,TRUE,FALSE)
r<-r1v2_predict_unit(gen,prev,con,e)
expected<-function(x,d).4+.2*x+.3*x*x+.1*d+.1*x*d
check('Exact load and ramp affect rates within the same old bin',abs(r$CO2[2]-expected(.51,.01))<1e-10&&abs(r$CO2[3]-expected(.511,.011))<1e-10&&r$CO2[2]!=r$CO2[3])
check('Zero generation produces zero emissions',all(vapply(r,function(x)x[1]*gen[1]==0,logical(1))))
check('Outside observed load support uses retained R1 fallback',r$CO2[4]==f$load_ramp[r1v2_bins(2,3)])
loadval<-r1v2_rate(f,'curve_load',.5,NA_real_,4,'load_ramp')
check('Unknown ramp uses validated load-only curve',r$CO2[5]==loadval)
p$load_curve_fallback<-FALSE;e$pollutants<-setNames(rep(list(p),4),names(e$pollutants));r2<-r1v2_predict_unit(gen,prev,con,e)
check('Unknown ramp without a validated load curve uses R1 fallback',r2$CO2[5]==f$load_ramp[r1v2_bins(.5,4)])
check('Negative modeled generation is rejected',inherits(try(r1v2_predict_unit(-1,0,TRUE,e),silent=TRUE),'try-error'))
p$fit$curve_load_ramp$coefficients<-c(-1,0,0,0,0);e$pollutants<-setNames(rep(list(p),4),names(e$pollutants))
check('Negative predicted rates are floored at zero',r1v2_predict_unit(51,50,TRUE,e)$CO2==0)
if(length(a)>=3){artifact<-readRDS(a[3]);fleet<-data.table(Facility_Unit.ID=names(artifact$units),Estimated_NameplateCapacity_MW=vapply(artifact$units,function(x)x$capacity,numeric(1)))
 check('Full real artifact validates',isTRUE(r1v2_validate_artifact(artifact,fleet,artifact$fleet_md5)))
 check('Mismatched facility input is rejected',inherits(try(r1v2_validate_artifact(artifact,fleet,'wrong'),silent=TRUE),'try-error'))
 bad<-artifact;bad$units[[1]]$pollutants[[1]]$model<-'typo'
 check('Unknown selected model is rejected',inherits(try(r1v2_validate_artifact(bad,fleet,bad$fleet_md5),silent=TRUE),'try-error'))
}
writeLines('All operating emission model checks passed.',out)
