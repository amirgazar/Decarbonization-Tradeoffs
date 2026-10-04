# Figure S4 shows annual means; remove the invisible uncertainty-band reference from its plotting code and footer. Dispatch code is unchanged.
"""R1: regenerate annual and ecological exhibits from the downloaded ensemble and retained inputs."""
from pathlib import Path
import os,json,hashlib
import numpy as np
import pandas as pd
import matplotlib
matplotlib.use('Agg')
import matplotlib.pyplot as plt
from matplotlib.patches import Patch
from scipy import stats

ROOT=Path(os.environ.get('PHASED_R1_ROOT', os.getcwd()))
FINAL=Path(os.environ.get('PHASED_FINAL_R1',str(ROOT/'2 Generation Expansion Model/5 Dispatch Curve/2 Advanced Research Computing/1 ARC Codes/Downloads R1/R1_ensemble101_1000_20260920_01/Final')))
OUT=Path(os.environ.get('PHASED_REVIEW_OUTPUT_R1',str(Path(__file__).resolve().parent/'Results R1')))
FIG=OUT/'Figures R1';TAB=OUT/'Tables R1'
for p in (FIG,TAB):p.mkdir(parents=True,exist_ok=True)
PATHS=['A','B1','B2','B3','C1','C2','C3','D']
plt.rcParams.update({'font.size':10,'axes.spines.top':False,'axes.spines.right':False,'svg.fonttype':'none','pdf.fonttype':42})
def save(fig,name):
    for ext in ['png','svg','pdf']:fig.savefig(FIG/f'{name}.{ext}',dpi=300,bbox_inches='tight',facecolor='white')
    plt.close(fig)
def summarize(d,groups,cols):
    x=d.melt(id_vars=groups+['Simulation'],value_vars=cols,var_name='Measure',value_name='Value')
    return x.groupby(groups+['Measure']).Value.agg(N='size',Mean='mean',SD='std',P05=lambda v:v.quantile(.05),P95=lambda v:v.quantile(.95)).reset_index()
y=pd.read_csv(FINAL/'Yearly_Results.csv')
keys=['Simulation','Pathway','Year'];idx=pd.MultiIndex.from_product([range(1,1001),PATHS,range(2025,2051)],names=keys)
assert len(y)==len(idx) and not y.duplicated(keys).any() and set(pd.MultiIndex.from_frame(y[keys]))==set(idx)
assert np.isfinite(y.select_dtypes('number')).all().all()
c=pd.read_csv(FINAL/'Coverage_and_accounting.csv');s=pd.read_csv(FINAL/'Yearly_Results_Shortages.csv')
assert len(c)==8000 and not c.duplicated(keys[:2]).any() and (c.Hours==227904).all()
short=y.set_index(keys).Calibrated_Shortage_MWh-s.set_index(keys).Unmet_Demand_total_MWh
assert short.abs().max()<1e-6
balance=y.Clean_MWh+y.Old_Fossil_Fuels_adj_MWh+y.New_Fossil_Fuel_MWh+y.Calibrated_Total_import_net_MWh+y.Calibrated_Battery_discharge_grid+y.Calibrated_Shortage_MWh-y.Demand-y.Calibrated_Battery_charge_grid-y.Calibrated_Curtailments_MWh
assert (balance.abs()<=y.Hours_present*.005001).all()
checks={'Simulations':1000,'Annual_rows':len(y),'Coverage_rows':len(c),'Hours_per_simulation_pathway':227904,'Annual_balance_max_MWh':float(balance.abs().max()),'Reported_hourly_balance_max_MWh':float(c.Peak_balance_error.max()),'Shortage_file_max_difference_MWh':float(short.abs().max()),'Hourly_ramp_storage_import_constraints':'Not tested: hourly partitions absent from downloaded Final folder','Heat_input':'Facility aggregation applies the retained hourly cap; regional aggregation is uncapped','Provenance':(FINAL/'SUMMARY_COMPLETE.txt').read_text()}
(OUT/'Checks_R1.json').write_text(json.dumps(checks,indent=2))
metrics=['Demand','Calibrated_Shortage_MWh','Calibrated_Curtailments_MWh','Old_Fossil_Fuels_adj_MWh','Old_Fossil_Fuels_net_MWh','New_Fossil_Fuel_MWh','Biomass_MWh','Clean_MWh','Calibrated_Battery_charge_grid','Calibrated_Battery_discharge_grid','Calibrated_Total_import_net_MWh','CO2_tons','NOx_lbs','SO2_lbs','SMR_MWh']
p=y.groupby(['Simulation','Pathway'])[metrics].sum().reset_index()
p['Unserved_percent_demand']=100*p.Calibrated_Shortage_MWh/p.Demand
p['Surplus_percent_demand']=100*p.Calibrated_Curtailments_MWh/p.Demand
p['Fossil_adjustment_TWh']=(p.Old_Fossil_Fuels_adj_MWh-p.Old_Fossil_Fuels_net_MWh)/1e6
p.to_csv(TAB/'Physical_totals_per_simulation_R1.csv',index=False)
ps=summarize(p,['Pathway'],metrics+['Unserved_percent_demand','Surplus_percent_demand','Fossil_adjustment_TWh'])
ps.to_csv(TAB/'Physical_summary_R1.csv',index=False)
annual=summarize(y,['Pathway','Year'],metrics);annual.to_csv(TAB/'Annual_physical_summary_R1.csv',index=False)
# annual unserved energy supports energy-adequacy reporting, but not LOLE or hourly constraint certification.
fig,axs=plt.subplots(1,3,figsize=(13,4.2))
for ax,metric,label,scale in zip(axs,['Calibrated_Shortage_MWh','Surplus_percent_demand','Fossil_adjustment_TWh'],['Unserved energy, 2025–2050 (TWh)','Surplus / cumulative demand (%)','Existing-fleet adjustment (TWh)'],[1e6,1,1]):
    q=ps[ps.Measure==metric].set_index('Pathway').loc[PATHS];x=np.arange(8)
    ax.bar(x,q.Mean/scale,color='#9EB1D4');ax.vlines(x,q.P05/scale,q.P95/scale,color='black');ax.set(xticks=x,xticklabels=PATHS,ylabel=label)
fig.text(.5,-.015,'Means and empirical P05–P95 across 1,000 simulations. Surplus includes restored thermal output.',ha='center')
fig.tight_layout();save(fig,'Annual_adequacy_R1')
stability=[]
for path,g in p.groupby('Pathway'):
    g=g.sort_values('Simulation')
    for n in [50,100,250,500,750,1000]:
        for metric in ['Calibrated_Shortage_MWh','Calibrated_Curtailments_MWh','CO2_tons']:
            z=g.iloc[:n][metric];stability.append([path,n,metric,z.mean(),z.std(),z.quantile(.05),z.quantile(.95)])
pd.DataFrame(stability,columns=['Pathway','N','Measure','Mean','SD','P05','P95']).to_csv(TAB/'Ensemble_stability_R1.csv',index=False)

# keep generation and battery discharge together as Figure S3; S4 is reserved for the B1 benchmark.
y['HQ_TWh']=y.Calibrated_Long_Term_Imports_HQ_TWh+y.Calibrated_Spot_Market_Imports_HQ_TWh
cols=['Hydro_TWh','Biomass_TWh','Nuclear_TWh','HQ_TWh','Calibrated_Import_NYISO_TWh','Calibrated_Import_NBSO_TWh','Old_Fossil_Fuels_adj_TWh','SMR_TWh','New_Fossil_Fuel_TWh','Onshore_TWh','Offshore_TWh','Solar_TWh']
labels=['Hydropower','Biomass','Large nuclear','Québec imports','NYISO imports','NBSO imports','Existing fossil','SMRs','New gas','Onshore wind','Offshore wind','Solar']
colors=['#9EB1D4','#A2D9B1','#C27BA0','#D1B1D6','#F4B6C2','#F2A2B0','#D9BCA9','#F0C78A','#E9967A','#B4D6E3','#A9CAD6','#FBE7A1']
means=y.groupby(['Pathway','Year'])[cols+['Demand','Calibrated_Curtailments_TWh','Calibrated_Battery_discharge_grid']].mean()
means.to_csv(TAB/'Figure_S3_source_R1.csv')
plt.rcParams.update({'font.size':19})
fig,axs=plt.subplots(2,8,figsize=(19,9),sharex='col',sharey='row',gridspec_kw={'height_ratios':[3,1]})
for i,path in enumerate(PATHS):
    q=means.loc[path];x=q.index.to_numpy();bot=np.zeros(len(q))
    for col,color in zip(cols,colors):axs[0,i].fill_between(x,bot,bot+q[col],color=color);bot+=q[col].to_numpy()
    axs[0,i].plot(x,q.Demand/1e6,color='black',ls='--',lw=1)
    axs[0,i].fill_between(x,bot-q.Calibrated_Curtailments_TWh,bot,facecolor='none',hatch='///',edgecolor='#666',linewidth=0)
    axs[0,i].set_title(path);axs[1,i].bar(x,q.Calibrated_Battery_discharge_grid/1e6,color='#B4D6E3')
    for a in axs[:,i]:a.set_xlim(2025,2050);a.set_xticks([2025,2050])
    axs[1,i].tick_params(axis='x',rotation=90)
axs[0,0].set_ylabel('Annual supply (TWh)');axs[1,0].set_ylabel('Battery discharge\n(TWh/year)')
handles=[Patch(facecolor=c,label=l) for c,l in zip(colors,labels)]+[Patch(facecolor='white',hatch='///',label='Total surplus'),plt.Line2D([],[],ls='--',color='black',label='Demand')]
fig.legend(handles=handles,ncol=4,loc='lower center',frameon=False,fontsize=17)
fig.subplots_adjust(bottom=.30,hspace=.12,wspace=.15);save(fig,'FigureS3_R1')
plt.rcParams.update({'font.size':12})

road=ROOT/'4 External Data/Massachusetts 2050 Decarbonization Roadmap Study/Massachusetts Workbook of Energy Modeling Results 2024_modified.xlsx'
raw=pd.read_excel(road,sheet_name='10.1 Total Gen',header=None)
years=raw.iloc[1,1:8].astype(int).to_numpy();ref=raw.iloc[2:12,1:8].apply(pd.to_numeric).set_axis(raw.iloc[2:12,0],axis=0)
# add a total using the same six clean-source categories as the Roadmap Grand Total.
matched={'solar':'Solar_TWh','offshore wind':'Offshore_TWh','onshore wind':'Onshore_TWh','hydro':'Hydro_TWh','nuclear':'Nuclear_TWh','transmission (Quebec imports)':'HQ_TWh'}
y['Matched_clean_total_TWh']=y[list(matched.values())].sum(axis=1)
assert np.allclose(ref.loc[list(matched)].sum(axis=0),ref.loc['Grand Total'],atol=.15)
mapping={'solar':'Solar_TWh','onshore wind':'Onshore_TWh','offshore wind':'Offshore_TWh','Grand Total':'Matched_clean_total_TWh'}
# show annual mean lines only because the uncertainty bands are not visible at the published scale.
titles=['Solar','Onshore wind','Offshore wind','Total clean supply']
fig,axs=plt.subplots(2,2,figsize=(11,7.2));bench=[]
for ax,(name,col),title in zip(axs.flat,mapping.items(),titles):
    b=y[y.Pathway=='B1'].groupby('Year')[col].agg(Mean='mean',P05=lambda v:v.quantile(.05),P95=lambda v:v.quantile(.95));vals=ref.loc[name].to_numpy(float)
    ax.plot(b.index,b.Mean,color='#315d82',label='PHASED B1 mean')
    ax.plot(years[1:],vals[1:],'o--',color='#b3674c',label='Roadmap High Electrification');ax.set(title=title,ylabel='TWh/year',xticks=[2025,2035,2050])
    for yr,v in zip(years[1:],vals[1:]):bench.append([name,int(yr),v,b.loc[yr,'Mean'],b.loc[yr,'P05'],b.loc[yr,'P95']])
axs[0,0].legend(frameon=False,fontsize=9)
fig.text(.5,.012,'Total includes solar, onshore wind, offshore wind, hydro, existing nuclear and Québec imports.',ha='center',fontsize=10)
fig.tight_layout(rect=(0,.05,1,1));save(fig,'FigureS4_R1')
pd.DataFrame(bench,columns=['Resource','Year','Roadmap_TWh','PHASED_mean_TWh','PHASED_P05_TWh','PHASED_P95_TWh']).to_csv(TAB/'Figure_S4_matched_resources_R1.csv',index=False)

schedule=pd.concat([d.assign(Pathway=k) for k,d in pd.read_excel(ROOT/'1 Decarbonization Pathways/Decarbonization_Pathways.xlsx',sheet_name=None).items()],ignore_index=True)
schedule=schedule[pd.to_numeric(schedule.Year,errors='coerce').between(2024,2050)].copy();schedule['Year']=schedule.Year.astype(int)
assert not schedule.duplicated(['Pathway','Year']).any()
schedule.to_csv(TAB/'Table_S2_capacity_schedule_R1.csv',index=False)
capacity=schedule.query('Year==2050').set_index('Pathway').loc[PATHS];capacity.to_csv(TAB/'Table_S1_2050_capacity_R1.csv')
meta=pd.read_csv(ROOT/'2 Generation Expansion Model/2 Generation/2 Fossil Generation/1 Existing Fossil Fuels/1 Fossil Fuels Facilities Data/Fossil_Fuel_Facilities_Data.csv')
meta.to_csv(TAB/'Table_S4_source_metadata_R1.csv',index=False)
gens=['Nuclear','Hydropower','Biomass','Solar','Onshore Wind','Offshore Wind','SMR','New NG']
# preserve Figure 2's capacity and remaining-fleet panels, and keep storage energy in a separate exhibit.
ret=pd.to_numeric(meta.Retirement_year,errors='coerce')
mw=pd.to_numeric(meta.Estimated_NameplateCapacity_MW,errors='coerce')
assert mw.notna().all()
baseline=meta[ret.isna()|(ret>=2025)]
remaining=meta[ret.isna()|(ret>=2050)]
capacity['Existing fossil']=[baseline.Estimated_NameplateCapacity_MW.sum() if p in ['A','D'] else remaining.Estimated_NameplateCapacity_MW.sum() for p in PATHS]
techs=['Existing fossil','Nuclear','Hydropower','Biomass','Imports QC','Imports NYISO','Imports NBSO','Onshore Wind','Offshore Wind','Solar','New NG','SMR']
pal=['#D9BCA9','#C27BA0','#9EB1D4','#A2D9B1','#D1B1D6','#F4B6C2','#F2A2B0','#B4D6E3','#A9CAD6','#FBE7A1','#E9967A','#EFD9B4']
order=['A','D','B1','B2','B3','C1','C2','C3']
fig,axs=plt.subplots(1,2,figsize=(12,6),gridspec_kw={'width_ratios':[1.8,1]})
bot=np.zeros(8)
for tech,color in zip(techs,pal):
    v=capacity.loc[order,tech].to_numpy()/1000;axs[0].bar(order,v,bottom=bot,color=color,label=tech);bot+=v
axs[0].set_ylabel('Generation and intertie capacity in 2050 (GW)');axs[0].set_title('(a) Prescribed capacities',loc='left')
axs[0].axvline(1.5,color='gray',ls='--');axs[0].set_ylim(0,bot.max()*1.08)
bot=np.zeros(8)
fuels=sorted(meta.Primary_Fuel_Type.dropna().unique())
fleet=[]
for i,fuel in enumerate(fuels):
    v=np.array([(baseline if p in ['A','D'] else remaining).query('Primary_Fuel_Type == @fuel').Estimated_NameplateCapacity_MW.sum()/1000 for p in order])
    axs[1].bar(order,v,bottom=bot,label=fuel,color=plt.get_cmap('tab20c')(i/len(fuels)));bot+=v
    fleet.extend([[p,fuel,x*1000] for p,x in zip(order,v)])
axs[1].bar(order,capacity.loc[order,'New NG']/1000,bottom=bot,color='#E9967A',label='New gas')
axs[1].set_ylabel('Thermal fleet capacity in 2050 (GW)');axs[1].set_title('(b) Existing fleet and new gas',loc='left')
axs[1].tick_params(axis='x',rotation=45)
axs[0].legend(ncol=3,frameon=False,loc='upper left',bbox_to_anchor=(-.03,-.12),fontsize=11)
axs[1].legend(ncol=2,frameon=False,loc='upper left',bbox_to_anchor=(-.03,-.12),fontsize=10)
fig.subplots_adjust(bottom=.32,wspace=.35);save(fig,'Figure2_capacity_schedule_R1')
capacity.to_csv(TAB/'Figure_2_capacity_source_R1.csv')
pd.DataFrame(fleet,columns=['Pathway','Fuel','Capacity_MW']).to_csv(TAB/'Figure_2_fleet_source_R1.csv',index=False)
fig,ax=plt.subplots(figsize=(7,4));ax.bar(PATHS,capacity.Storage/1000,color='#adadad');ax.set_ylabel('Storage energy used by dispatch (GWh)');fig.tight_layout();save(fig,'Storage_capacity_check_R1')

# retain published coefficient families, remove the forced C1 zero and share each draw across pathways.
base=schedule.query('Year==2024').set_index('Pathway');delta=capacity[gens]-base.loc[PATHS,gens]
draw=np.random.default_rng(20260920);n=1000;paths=PATHS[1:];records=[]
landrates={'Solar':draw.gamma(4.25,.82,n),'Onshore Wind':draw.gamma(3.695,9.382,n),'SMR':np.full(n,.017),'New NG':np.full(n,.032),'Canadian Hydro':draw.uniform(43.1,146.7,n)}
birdrates={'Onshore birds':draw.gamma(.20,2.54,n),'Onshore bats':draw.gamma(1.538,4.160,n),'Solar birds':draw.lognormal(np.log(1.214),1.409,n)}
gaswater=draw.uniform(15,50,n)
land=np.zeros((n,len(paths)));birds=np.zeros_like(land);water=np.zeros_like(land)
for j,path in enumerate(paths):
    for tech,rate in landrates.items():
        # preserve the original 3,692.308 MW land-allocation assumption and flag it for Canadian-boundary review.
        cap=(3692.308 if path=='B3' else 0) if tech=='Canadian Hydro' else delta.loc[path,tech]
        v=cap*rate/1e6;land[:,j]+=v
        records.extend(zip(range(1,n+1),[path]*n,['Land occupation']*n,[tech]*n,v,['million hectares']*n))
    for tech,rate in birdrates.items():
        cap=delta.loc[path,'Solar' if tech=='Solar birds' else 'Onshore Wind'];v=cap*rate/1e6;birds[:,j]+=v
        records.extend(zip(range(1,n+1),[path]*n,['Bird and bat mortality']*n,[tech]*n,v,['million deaths/year in 2050 from added capacity']*n))
    g=p[p.Pathway==path].sort_values('Simulation');assert np.array_equal(g.Simulation,np.arange(1,1001))
    # sample the Table 1 gas-withdrawal range instead of silently using only its midpoint.
    for tech,v in [('SMRs',g.SMR_MWh.to_numpy()*740/1e12),('New gas',g.New_Fossil_Fuel_MWh.to_numpy()*gaswater/1e12)]:
        water[:,j]+=v;records.extend(zip(range(1,n+1),[path]*n,['Water withdrawals']*n,[tech]*n,v,['trillion US gallons over 2025–2050']*n))
eco=pd.DataFrame(records,columns=['Simulation','Pathway','Endpoint','Technology','Value','Unit'])
eco.to_csv(TAB/'Ecological_draws_R1.csv',index=False)
summ=eco.groupby(['Endpoint','Technology','Unit','Pathway']).Value.agg(Mean='mean',SD='std',P05=lambda v:v.quantile(.05),P95=lambda v:v.quantile(.95)).reset_index()
tot=eco.groupby(['Simulation','Pathway','Endpoint','Unit']).Value.sum().reset_index().assign(Technology='Total')
ts=tot.groupby(['Endpoint','Technology','Unit','Pathway']).Value.agg(Mean='mean',SD='std',P05=lambda v:v.quantile(.05),P95=lambda v:v.quantile(.95)).reset_index()
pd.concat([summ,ts]).to_csv(TAB/'Table_S17_recalculated_R1.csv',index=False)
fig,axs=plt.subplots(1,3,figsize=(14,5))
for ax,z,title,label in zip(axs,[land,birds,water],['Land occupation','Bird and bat mortality','Cooling-water withdrawals'],['Added capacity in 2050 (million ha)','Added capacity in 2050 (million/year)','New SMRs and gas, 2025–2050\n(trillion US gallons)']):
    x=np.arange(7);ax.bar(x,z.mean(axis=0),color='#B4D6E3');ax.vlines(x,np.quantile(z,.05,axis=0),np.quantile(z,.95,axis=0),color='black');ax.set(title=title,ylabel=label,xticks=x,xticklabels=paths)
fig.tight_layout();save(fig,'Figure7_recalculated_panels_R1')
coefs=[]
for name,dist,unit in [('Onshore birds',stats.gamma(a=.20,scale=2.54),'deaths/MW/year'),('Onshore bats',stats.gamma(a=1.538,scale=4.160),'deaths/MW/year'),('Solar birds',stats.lognorm(s=1.409,scale=1.214),'deaths/MW/year'),('Canadian hydro land',stats.uniform(loc=43.1,scale=103.6),'ha/MW'),('Onshore wind land',stats.gamma(a=3.695,scale=9.382),'ha/MW'),('Solar land',stats.gamma(a=4.25,scale=.82),'ha/MW'),('New gas water',stats.uniform(loc=15,scale=35),'US gal/MWh')]:coefs.append([name,dist.mean(),dist.ppf(.05),dist.ppf(.95),unit])
pd.DataFrame(coefs,columns=['Coefficient','Mean','P05','P95','Unit']).to_csv(TAB/'Table_1_coefficient_check_R1.csv',index=False)

# keep the original S2 chart unchanged, as requested by the author.
import shutil
for item in (Path(__file__).resolve().parent/'Original SI figures R1').glob('FigureS2*'):
    shutil.copy2(item,FIG/item.name)
(OUT/'Readme_R1.md').write_text('Annual figures use all 1,000 simulations. Figure S3 combines supply and storage; Figure S4 compares matching B1 resource quantities with the retained Roadmap workbook.\n\nEcological coefficients retain the source families. C1 land is calculated from its SMR additions rather than forced to zero. Bird and bat mortality is an annual 2050 endpoint, not cumulative deaths. Gas withdrawal coefficients follow Table 1 Uniform(15,50), with common draws across pathways. Land and mortality ranges use P05/P95 in both tables and figures.\n\nViewshed cannot be recalculated because the CONUS LandScan raster is absent. The old ten-placement viewshed results are not relabeled as a 1,000-run result. Figure 4 and full hourly reliability checks require hourly records that are absent from the downloaded folder. Canadian land uses the retained 3,692.308 MW allocation; reconcile it with the cost boundary before publication. The source units for the storage schedule still require confirmation.\n')
print('Saved full-ensemble annual checks, figures and ecological tables to',OUT)
