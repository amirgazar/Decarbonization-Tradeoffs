# isolate AP4 repricing on unchanged emissions and source donors for all 1,000 simulations.
"""R1: isolate AP4 repricing on unchanged emissions and source donors for all 1,000 simulations."""
from pathlib import Path
import os,json
import numpy as np,pandas as pd
ROOT=Path(os.environ.get('PHASED_R1_ROOT', os.getcwd()))
RUN=Path(os.environ.get('PHASED_COST_OUTPUT_R1',(ROOT/'3 Total Costs/Cost production audit R1/Last_output_R1.txt').read_text().strip()))
OUT=Path(os.environ.get('PHASED_REVIEW_OUTPUT_R1',str(Path(__file__).resolve().parent/'Results R1')))
DEST=OUT/'AP4 comparison R1';DEST.mkdir(parents=True,exist_ok=True)
comp=RUN/'discount_R1_0.015/Components R1'
pol=['NOx','SO2','PM2.5','VOC','PM10','CO'];mass=['total_NOx_tons','total_SO2_tons','PM2.5_tons','VOC_tons','PM10_tons','CO_tons']
cost=['total_'+p+'_USD' for p in pol];keys=['Simulation','Pathway','Year']
ref=pd.read_csv(comp/'AP3_reference_coefficients_R1.csv').set_index('Facility_Unit.ID')
assert not ref.index.duplicated().any()
parts=[];checks=[]
for fleet in ['Existing','New']:
    file=comp/f'Facility_Level_Results_{fleet}.csv';count=0;err=0
    for x in pd.read_csv(file,usecols=keys+['Facility_Unit.ID']+mass+cost+pol,chunksize=100000):
        assert x.notna().all().all() and np.isfinite(x[mass+cost+pol]).all().all()
        ap3_reference=ref.reindex(x['Facility_Unit.ID']);assert ap3_reference.notna().all().all()
        z=x[keys].copy()
        for p,m,c in zip(pol,mass,cost):
            err=max(err,float(np.abs(x[m]*x[p]-x[c]).max()))
            z['AP3_'+p]=x[m].to_numpy()*ap3_reference[p].to_numpy()
            z['AP4_'+p]=x[c].to_numpy()
        parts.append(z.groupby(keys).sum());count+=len(x)
    checks.append({'Fleet':fleet,'Rows':count,'Max_mass_times_rate_error_USD':err})
    assert err<.001
annual=pd.concat(parts).groupby(level=keys).sum().reset_index()
assert len(annual)==208000 and set(annual.Simulation)==set(range(1,1001))
annual.to_csv(DEST/'Annual_paired_air_costs_R1.csv',index=False)
summaries=[];individual=[]
for rate in [.015,.02,.025]:
    z=annual.copy();names=['AP3_'+p for p in pol]+['AP4_'+p for p in pol]
    z[names]=z[names].div((1+rate)**(z.Year-2024),axis=0)
    z=z.groupby(['Simulation','Pathway'])[names].sum().reset_index();z['Rate']=rate
    z['AP3_air_bUSD']=z[['AP3_'+p for p in pol]].sum(axis=1)/1e9
    z['AP4_air_bUSD']=z[['AP4_'+p for p in pol]].sum(axis=1)/1e9
    z['AP4_added_bUSD']=z.AP4_air_bUSD-z.AP3_air_bUSD
    individual.append(z)
    for path,q in z.groupby('Pathway'):
        summaries.append([rate,path,len(q),q.AP3_air_bUSD.mean(),q.AP4_air_bUSD.mean(),q.AP4_added_bUSD.mean(),q.AP4_added_bUSD.quantile(.05),q.AP4_added_bUSD.quantile(.95)])
pd.concat(individual).to_csv(DEST/'Paired_AP3_AP4_R1.csv',index=False)
summary=pd.DataFrame(summaries,columns=['Rate','Pathway','N','AP3_mean_bUSD','AP4_mean_bUSD','Added_mean_bUSD','Added_P05_bUSD','Added_P95_bUSD']);summary.to_csv(DEST/'AP4_added_cost_summary_R1.csv',index=False)
(DEST/'Independent_checks_R1.json').write_text(json.dumps({'checks':checks,'annual_rows':len(annual),'scope':'Same emissions, donors, CPI convention and AP3 PM10 and the published CO cost range. AP3 reference means the retained pipeline coefficients, not a newly harmonized AP3 model run.'},indent=2))
import matplotlib
matplotlib.use('Agg')
import matplotlib.pyplot as plt
plt.rcParams.update({'svg.fonttype':'none','pdf.fonttype':42,'font.size':11,'axes.spines.top':False,'axes.spines.right':False})
q=summary[summary.Rate==.02].set_index('Pathway').loc[['A','B1','B2','B3','C1','C2','C3','D']]
fig,axs=plt.subplots(1,2,figsize=(12,5));x=np.arange(8)
axs[0].bar(x-.18,q.AP3_mean_bUSD,width=.36,label='Retained AP3 coefficients',color='#aab8c5');axs[0].bar(x+.18,q.AP4_mean_bUSD,width=.36,label='AP4 + AP3 PM10 and the published CO cost range',color='#3f7397');axs[0].set_ylabel('Air-pollution damage NPV (billion 2024 USD)');axs[0].legend(frameon=False,fontsize=9)
axs[1].bar(x,q.Added_mean_bUSD,color='#3f7397');axs[1].vlines(x,q.Added_P05_bUSD,q.Added_P95_bUSD,color='black');axs[1].set_ylabel('Increase in total cost from AP4 (billion 2024 USD)')
for a in axs:a.set_xticks(x,q.index)
fig.text(.5,-.005,'2% discount rate; 1,000 paired simulations. Only the air-damage coefficients change.',ha='center')
fig.tight_layout()
for ext in ['png','svg','pdf']:fig.savefig(OUT/'Figures R1'/f'AP4_cost_increase_1000_R1.{ext}',dpi=300,bbox_inches='tight')
print(summary[summary.Rate==.02].to_string(index=False))
