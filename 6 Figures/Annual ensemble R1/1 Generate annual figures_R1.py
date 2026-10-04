# Purpose: Generate final-ensemble annual supply, grid battery discharge, water, existing-fleet emissions and available-cost plots with matched source tables.
"""Draw figures supported by the completed regional annual ensemble."""
from pathlib import Path
import os,json,hashlib
import numpy as np
import pandas as pd
import matplotlib
matplotlib.use('Agg')
import matplotlib.pyplot as plt
from matplotlib.patches import Patch

ROOT=Path(os.environ.get('PHASED_R1_ROOT', os.getcwd()))
FINAL=Path(os.environ.get('PHASED_FINAL_R1',str(ROOT/'2 Generation Expansion Model/5 Dispatch Curve/2 Advanced Research Computing/1 ARC Codes/Downloads R1/R1_ensemble101_1000_20260920_01/Final')))
OUTPUT=Path(os.environ.get('PHASED_ANNUAL_OUTPUT_R1',''))
if not os.environ.get('PHASED_ANNUAL_OUTPUT_R1'):
    OUTPUT=Path((ROOT/'7 Reproduction Information Document/Cost production audit R1/Last_annual_output_R1.txt').read_text().strip())
DEST=OUTPUT/'Figures R1';DEST.mkdir(parents=True,exist_ok=True)
PATHS=['A','B1','B2','B3','C1','C2','C3','D']
plt.rcParams.update({'font.size':11,'axes.spines.top':False,'axes.spines.right':False,'svg.fonttype':'none','pdf.fonttype':42})

def save(fig,name):
    for ext in ('png','svg','pdf'):fig.savefig(DEST/f'{name}.{ext}',dpi=300,bbox_inches='tight',facecolor='white')
    plt.close(fig)

def summaries(data,keys,columns):
    long=data.melt(id_vars=keys+['Simulation'],value_vars=columns,var_name='Component',value_name='Value')
    return long.groupby(keys+['Component']).Value.agg(N='size',Mean='mean',SD='std',P05=lambda v:v.quantile(.05),P95=lambda v:v.quantile(.95)).reset_index()

def intervals(ax,positions,lo,hi):
    ax.vlines(positions,lo,hi,color='black',lw=1)
    ax.hlines(lo,positions-.12,positions+.12,color='black',lw=1)
    ax.hlines(hi,positions-.12,positions+.12,color='black',lw=1)

source=FINAL/'Yearly_Results.csv';source_hash=hashlib.sha256(source.read_bytes()).hexdigest()
y=pd.read_csv(source)
assert len(y)==208000 and set(y.Simulation)==set(range(1,1001)) and set(y.Pathway)==set(PATHS)
assert not y.duplicated(['Simulation','Pathway','Year']).any() and not y.isna().any().any()
assert y.groupby(['Simulation','Pathway']).Year.apply(lambda v:set(v)==set(range(2025,2051))).all()
# use final delivered imports rather than pre-calibration availability so the stack matches the saved balance.
y['HQ_TWh']=y.Calibrated_Long_Term_Imports_HQ_TWh+y.Calibrated_Spot_Market_Imports_HQ_TWh
columns=['Hydro_TWh','Biomass_TWh','Nuclear_TWh','HQ_TWh','Calibrated_Import_NYISO_TWh','Calibrated_Import_NBSO_TWh','Old_Fossil_Fuels_adj_TWh','SMR_TWh','New_Fossil_Fuel_TWh','Onshore_TWh','Offshore_TWh','Solar_TWh']
labels=['Hydropower','Biomass','Large nuclear','Hydro-Québec imports','NYISO imports','NBSO imports','Existing fossil','SMRs','New natural gas','Onshore wind','Offshore wind','Solar']
colors=['#9EB1D4','#A2D9B1','#C27BA0','#D1B1D6','#F4B6C2','#F2A2B0','#D9BCA9','#F0C78A','#E9967A','#B4D6E3','#A9CAD6','#FBE7A1']
y['Demand_TWh']=y.Demand/1e6
y['Battery_discharge_TWh']=y.Calibrated_Battery_discharge_grid/1e6
y['Battery_charge_TWh']=y.Calibrated_Battery_charge_grid/1e6
supply=y[columns].sum(axis=1)
expected=y.Clean_TWh+y.Old_Fossil_Fuels_adj_TWh+y.New_Fossil_Fuel_TWh+y.Calibrated_Total_import_net_TWh
assert np.allclose(supply,expected,rtol=0,atol=1e-8)
residual=supply+y.Battery_discharge_TWh+y.Calibrated_Shortage_TWh-y.Demand_TWh-y.Battery_charge_TWh-y.Calibrated_Curtailments_TWh
assert np.abs(residual).max()<.00005
annual=summaries(y,['Pathway','Year'],columns+['Demand_TWh','Battery_discharge_TWh','Battery_charge_TWh','Calibrated_Shortage_TWh','Calibrated_Curtailments_TWh'])
annual.to_csv(DEST/'Annual_figure_source_R1.csv',index=False)
means=y.groupby(['Pathway','Year'])[columns+['Demand_TWh','Calibrated_Curtailments_TWh']].mean()
fig,axes=plt.subplots(1,8,figsize=(20,6),sharey=True)
for ax,path in zip(axes,PATHS):
    data=means.loc[path];years=data.index.to_numpy();bottom=np.zeros(len(data))
    for col,color in zip(columns,colors):
        values=data[col].to_numpy();ax.fill_between(years,bottom,bottom+values,color=color);bottom+=values
    # mark the saved total surplus without attributing it to a particular renewable technology.
    ax.fill_between(years,bottom-data.Calibrated_Curtailments_TWh,bottom,facecolor='none',edgecolor='#666666',hatch='///',linewidth=0)
    ax.plot(years,data.Demand_TWh,color='black',ls='--',lw=1.5)
    ax.set(title=path,xlim=(2025,2050),xticks=[2025,2050]);ax.tick_params(axis='x',rotation=90)
axes[0].set_ylabel('Annual supply before surplus allocation (TWh)')
handles=[Patch(facecolor=c,label=l) for c,l in zip(colors,labels)]+[Patch(facecolor='white',edgecolor='#666666',hatch='///',label='Unallocated surplus'),plt.Line2D([],[],color='black',ls='--',label='Demand')]
fig.legend(handles=handles,loc='lower center',ncol=7,frameon=False,bbox_to_anchor=(.5,.035),fontsize=10)
fig.suptitle('Annual supply by pathway — means across 1,000 simulations',y=.98)
fig.text(.5,.012,'Storage charging/discharging and unserved demand are recorded separately in the source tables.',ha='center',fontsize=10)
fig.subplots_adjust(bottom=.25,wspace=.16,top=.88)
save(fig,'FigureS3_generation_1000_R1')

fig,axes=plt.subplots(1,8,figsize=(20,4.5),sharey=True)
battery=annual[annual.Component=='Battery_discharge_TWh']
for ax,path in zip(axes,PATHS):
    d=battery[battery.Pathway==path].sort_values('Year');ax.bar(d.Year,d.Mean,color='#B4D6E3',width=.75)
    ax.vlines(d.Year,d.P05,d.P95,color='#36516e',lw=.8)
    ax.set(title=path,xticks=[2025,2050]);ax.tick_params(axis='x',rotation=90)
axes[0].set_ylabel('Grid-side battery discharge (TWh/year)')
fig.suptitle('Battery discharge — 1,000 simulations',y=.98)
fig.text(.5,.015,'Bars: means. Lines: empirical P05–P95. These annual summaries do not establish hourly reliability.',ha='center',fontsize=10)
fig.subplots_adjust(bottom=.18,wspace=.16,top=.85);save(fig,'FigureS4_battery_1000_R1')

physical_cols=columns+['Demand_TWh','Battery_discharge_TWh','Battery_charge_TWh','Calibrated_Shortage_TWh','Calibrated_Curtailments_TWh','CO2_tons','NOx_lbs','SO2_lbs']
physical=y.groupby(['Simulation','Pathway'])[physical_cols].sum().reset_index()
physical.to_csv(DEST/'Physical_totals_per_simulation_1000_R1.csv',index=False)
summaries(physical,['Pathway'],physical_cols).to_csv(DEST/'Physical_totals_summary_1000_R1.csv',index=False)
# retain the original water coefficients; convert TWh × gal/MWh to trillion gal with 10^6 / 10^12.
water=physical[['Simulation','Pathway']].copy()
water['SMRs']=physical.SMR_TWh*740/1e6
water['New natural gas']=physical.New_Fossil_Fuel_TWh*32.5/1e6
water['Total']=water['SMRs']+water['New natural gas']
water.to_csv(DEST/'Water_per_simulation_1000_R1.csv',index=False)
ws=summaries(water,['Pathway'],['SMRs','New natural gas','Total']);ws.to_csv(DEST/'Table_S17_water_source_1000_R1.csv',index=False)
paths=PATHS[1:];x=np.arange(len(paths));m=water.groupby('Pathway')[['SMRs','New natural gas']].mean().reindex(paths)
fig,ax=plt.subplots(figsize=(8,5));bottom=np.zeros(len(paths))
for col,color in zip(m,['#EFD9B4','#E9967A']):ax.bar(x,m[col],bottom=bottom,color=color,label=col);bottom+=m[col]
d=ws[ws.Component=='Total'].set_index('Pathway').loc[paths]
# use the same P05/P95 definitions in figure and table; the old table used the median as its lower bound.
intervals(ax,x,d.P05.to_numpy(),d.P95.to_numpy())
ax.set(xticks=x,xticklabels=paths,ylabel='2025–2050 withdrawals (trillion US gallons)',title='Water withdrawals from new SMR and gas generation')
ax.legend(frameon=False);fig.text(.5,.018,'1,000 simulations; P05–P95. Fixed original water coefficients; other generation is outside this panel.',ha='center',fontsize=9)
fig.tight_layout(rect=(0,.05,1,1));save(fig,'Figure7_water_1000_R1')

# label these as existing-fleet emissions because regional pollutant fields omit the separate new-gas calculation.
em=physical[['Simulation','Pathway']].copy()
em['CO2_Mt']=physical.CO2_tons*.90718474/1e6
em['NOx_kt']=physical.NOx_lbs*.45359237/1e6
em['SO2_kt']=physical.SO2_lbs*.45359237/1e6
es=summaries(em,['Pathway'],['CO2_Mt','NOx_kt','SO2_kt']);es.to_csv(DEST/'Existing_fleet_emissions_source_1000_R1.csv',index=False)
fig,axes=plt.subplots(1,3,figsize=(13,4.5));x=np.arange(8)
for ax,metric,label in zip(axes,['CO2_Mt','NOx_kt','SO2_kt'],['CO₂ (million metric tonnes)','NOₓ (thousand metric tonnes)','SO₂ (thousand metric tonnes)']):
    d=es[es.Component==metric].set_index('Pathway').loc[PATHS];ax.bar(x,d.Mean,color='#D9BCA9');intervals(ax,x,d.P05.to_numpy(),d.P95.to_numpy());ax.set(xticks=x,xticklabels=PATHS,ylabel=label)
fig.suptitle('Existing-fleet emissions, 2025–2050 — new gas excluded')
fig.text(.5,.015,'1,000 simulations; P05–P95. Physical emissions only; no AP4 damage valuation is included.',ha='center',fontsize=10)
fig.tight_layout(rect=(0,.05,1,.94));save(fig,'Existing_fleet_emissions_1000_R1')

cost_file=OUTPUT/'Available_cost_summary_R1.csv'
if cost_file.exists():
    cost=pd.read_csv(cost_file);cost=cost[cost.Rate==.02];paths=['A','B1','B2','B3(1)','B3(2)','C1','C2','C3','D'];x=np.arange(9)
    groups=[('CAPEX','Generation + import CAPEX'),('FOM','Generation + import fixed O&M'),('Nonfossil_VOM','Non-fossil variable O&M'),('Nonfossil_Fuel','Non-fossil fuel'),('Imports','Import purchases'),('Unmet_demand','Unmet-demand penalty'),('CAN_CAPEX','Canadian hydro CAPEX'),('CAN_FOM','Canadian hydro fixed O&M'),('CAN_VOM','Canadian hydro variable O&M'),('CAN_CH4','Canadian reservoir methane')]
    fig,axes=plt.subplots(3,4,figsize=(16,10))
    for ax,(component,title) in zip(axes.flat,groups):
        d=cost[cost.Component==component].set_index('Pathway').loc[paths];ax.bar(x,d.Mean,color='#3f7f9f');intervals(ax,x,d.P05.to_numpy(),d.P95.to_numpy());ax.set(xticks=x,xticklabels=paths,title=title);ax.tick_params(axis='x',rotation=40);ax.set_ylabel('Billion 2024 USD',fontsize=9)
    for ax in list(axes.flat)[len(groups):]:ax.set_visible(False)
    fig.suptitle('Provisional available cost components — 1,000 simulations, 2% discount rate')
    fig.text(.5,.012,'Fossil VOM/fuel, regional GHG and AP4 are pending. Nuclear VOM/fuel retain 2030–2050 coverage. P05–P95 shown.',ha='center',fontsize=10)
    fig.tight_layout(rect=(0,.04,1,.96));save(fig,'Available_cost_components_1000_R1')
assert hashlib.sha256(source.read_bytes()).hexdigest()==source_hash
(DEST/'Annual_figure_manifest_R1.json').write_text(json.dumps({'simulations':1000,'annual_input':str(source),'annual_sha256':source_hash,'cost_scope':'Annual/capacity components only; AP4 not yet calculated','water_coefficients_gal_per_MWh':{'SMR':740,'New_gas':32.5},'interval':'Empirical P05-P95','generation_imports':'Final calibrated delivered imports','pending':'Facility data, hourly validation and ecological coefficient review'},indent=2))
print('Generated annual ensemble figures and source tables:',DEST)
