# Purpose: Separate exogenous inputs and endogenous outputs, with representative distribution plots.
"""R1: distinguish prescribed inputs, sampled inputs and calculated outputs because their plotted distributions have different meanings."""
from pathlib import Path
import os,json,html
import numpy as np
import pandas as pd
import matplotlib
matplotlib.use('Agg')
import matplotlib.pyplot as plt
from scipy import stats

ROOT=Path(os.environ.get('PHASED_R1_ROOT', os.getcwd()))
RUN=Path(os.environ['PHASED_COST_OUTPUT_R1'])
OUT=Path(os.environ.get('PHASED_REVIEW_OUTPUT_R1',str(Path(__file__).resolve().parent/'Results R1')))
DEST=OUT/'Variable distributions R1';DEST.mkdir(parents=True,exist_ok=True)
plt.rcParams.update({'font.size':9,'axes.spines.top':False,'axes.spines.right':False,'svg.fonttype':'none','pdf.fonttype':42})
rows=[]
def add(symbol,name,role,distribution,sharing,source,kind='fixed',values=None,dist=None,unit='',note=''):
    number=len(rows)+1;file=f'Variable_{number:02d}_R1'
    fig,ax=plt.subplots(figsize=(3.1,1.25))
    if kind=='fixed':
        ax.axvline(.5,ymax=.82,color='#315d82',lw=2);ax.set(xlim=(0,1),ylim=(0,1),xticks=[],yticks=[])
        ax.set_xlabel('Fixed for the specified year / case',fontsize=8,labelpad=5)
    elif kind=='empirical':
        vals=np.asarray(values,dtype=float);assert np.isfinite(vals).all()
        if np.ptp(vals)<1e-12:
            ax.axvline(vals[0],color='#315d82',lw=2);ax.set_yticks([]);ax.set_xlabel(unit+' (constant in these runs)',fontsize=8)
        else:
            ax.hist(vals,bins=24,density=True,color='#9eb1d4',edgecolor='white',lw=.3);ax.set_xlabel(unit,fontsize=8);ax.set_ylabel('Density',fontsize=8)
    elif kind=='index':
        vals=np.asarray(values,dtype=int);counts=np.bincount(vals,minlength=100)[1:100];assert counts.sum()==vals.size
        ax.bar(np.arange(1,100),counts/counts.sum(),color='#9eb1d4',width=1);ax.set(xlim=(1,99),xticks=[1,50,99]);ax.set_xlabel('Retained percentile index',fontsize=8);ax.set_ylabel('Frequency',fontsize=8)
    else:
        # show the bird-mortality CDF because its gamma density is unbounded at zero and obscures the rest of the curve.
        if dist.dist.name=='gamma' and dist.kwds.get('a',1)<1:
            x=np.linspace(0,dist.ppf(.995),500);ax.plot(x,dist.cdf(x),color='#315d82');ax.set(ylim=(0,1));ax.set_ylabel('Cumulative probability',fontsize=7)
            note='Assumed coefficient CDF shown because the density is unbounded at zero; horizontal range ends at P99.5.'
        else:
            a,b=dist.ppf([.001,.995]);x=np.linspace(a,b,500);ax.plot(x,dist.pdf(x),color='#315d82');ax.fill_between(x,dist.pdf(x),alpha=.2,color='#9eb1d4');ax.set_ylabel('Density',fontsize=8)
        ax.set_xlabel(unit,fontsize=8)
    ax.tick_params(labelsize=7);fig.tight_layout(pad=.5)
    for ext in ([] if kind=='fixed' else ['png','svg','pdf']):fig.savefig(DEST/f'{file}.{ext}',dpi=160,bbox_inches='tight')
    plt.close(fig)
    rows.append({'Symbol / quantity':symbol,'Variable':name,'Role':role,'Distribution / treatment':distribution,'Sharing and dependence':sharing,'Plot meaning':note or ('Assumed coefficient distribution' if kind=='theory' else 'Fixed input' if kind=='fixed' else 'Empirical calculated output'),'Source':source,'Plot':'' if kind=='fixed' else file+'.png'})

fixed='Exogenous : fixed'
sampled='Exogenous : sampled'
output='Endogenous : calculated'
fixed_rows=[
 ('D(t)','Electricity demand','Prescribed hourly demand trajectory; demand forecast error is not sampled.','Same demand trajectory across simulations.','SI demand methods; Yearly_Results.csv'),
 ('K(k,p,y)','Generation and intertie capacities','Prescribed pathway/year schedule. SMRs and existing nuclear are separate technologies.','Fixed within each pathway and year.','Decarbonization_Pathways.xlsx; Table S2'),
 ('R(u,y)','Unit retirements and eligibility','Prescribed retirement dates and eligible-unit set. Exact ARC provenance remains open.','Fixed within a pathway/year.','Facility metadata; ARC manifest still needed'),
 ('K(storage), η, Pmax','Storage size, efficiency and operating limits','Prescribed capacity and efficiency; source MW/MWh interpretation still requires confirmation.','Fixed operating assumptions.','SI storage methods; dispatch_curve_base_v2.R'),
 ('ramp(u), floor(u)','Thermal ramp and minimum-output constraints','Unit-specific operating rules; no forced-outage or startup-emission draw.','Fixed by unit; hourly verification pending.','SI thermal methods; dispatch code'),
 ('CF(contract)','Contracted Québec import capacity factor','Fixed at 0.95 before final balancing.','Same bound across simulations.','SI imports methods'),
 ('P(fossil,y)','Fossil-fuel prices','Prescribed annual price schedule; fuel-price shocks omitted.','Fixed within year and valuation case.','Fossil fuel module; ATB_2021_Fuel_Costs_Fossil.xlsx'),
 ('EF(q,u)','Operating emission-rate functions and other pollutant factors','Selected operating lookup and technology/fuel factors; no emission-rate draw during dispatch.','Fixed lookup conditional on unit and operating state; ARC artifact confirmation pending.','SI operating-emission methods; Table S7'),
 ('MD(AP4,q,u)','AP4 marginal damage coefficients','NOx, SO2, PM2.5 and VOC coefficients; EGU match or documented county/donor proxy.','No damage-coefficient uncertainty sampled.','AP4_source_mapping_R1.csv; Table S8'),
 ('MD(AP3,PM10,u)','PM10 damage coefficient','AP3 is used because PM10 coefficients are unavailable in the AP4 tables used here.','Fixed by source assignment.','AP3 input coefficient files; Table S8'),
 ('MD(CO,u)','CO damage coefficient','Separate published 2019-USD range, converted to 2024 USD and assigned using the retained SO2-based scaling.','No independent CO coefficient draw. This is not an AP3 coefficient.','10_Air_emissions.R; SI air-damage methods'),
 ('SC(q,y,r)','GHG social-cost schedules','Year- and discount-case-specific EPA CO2, CH4 and N2O schedules.','Fixed within each valuation case.','GHG cost module; EPA schedule'),
 ('VOLL','Unserved-energy penalty','Fixed $3,500/MWh. The curve in original Figure S2 is contextual.','Same coefficient across pathways and simulations.','11_Unmet_Demand.R; Figure S2'),
 ('r','Discount-and-valuation case','1.5%, 2% or 2.5%; separate cases, not a random distribution.','Both discounting and the selected GHG schedule vary by case.','Run_settings_R1.csv'),
 ('L(SMR), L(gas), W(SMR)','Fixed ecological coefficients','SMR land: 0.017 ha/MW; new gas land: 0.032 ha/MW; SMR water: 740 US gal/MWh.','Common coefficients across pathways.','Table 1; Review_full_ensemble_R1.py')]
for symbol,name,treatment,sharing,source in fixed_rows:add(symbol,name,fixed,treatment,sharing,source)

# show saved percentile selections rather than inventing a fitted hourly-output density.
random=ROOT/'2 Generation Expansion Model/4 Randomization/1 Randomized Data/Random_Sequence.csv'
indices=[]
with random.open() as f:
    f.readline()
    for line in f:
        vals=[int(v.strip('"\n\r')) for v in line.split(',',1000)[:1000]]
        assert len(vals)==1000 and all(1<=v<=99 for v in vals);indices.append(vals)
indices=np.asarray(indices)
for offset,name in enumerate(['Solar profile','Onshore-wind profile','Offshore-wind profile','Existing Québec import profile','NYISO import profile','NBSO import profile']):
    chosen=indices[(np.arange(len(indices))*6+offset)%len(indices)]
    add(f'p({offset+1},s,t)',name,sampled,'Historical empirical percentile lookup (indices 1–99), conditional on the retained time/profile tables.','Same indexed inputs across pathways. Separate profile selections do not establish joint weather-demand-import dependence.','Random_Sequence.csv, simulations 1–1000; dispatch_curve_base_v2.R',kind='index',values=chosen.ravel(),note='Retained input-index frequencies; exact correspondence to the downloaded ARC run requires its manifest.')
add('p(u,s,t)','Existing thermal output profile',sampled,'Historical unit-generation percentile lookup; donor/eligibility assignments constrain its use.','Shared simulation indices; chronology, floors and ramps subsequently modify realized output.','Random_Sequence.csv; dispatch unit-allocation code',kind='index',values=indices.ravel(),note='Representative retained index pool, not a distribution of dispatched generation or fitted emissions.')

for symbol,name,source in [
 ('U(CAPEX)','Generation capital coefficients','ATBe_2024.csv; ATB_CAPEX_FOM_Common_Draws.csv'),
 ('U(FOM)','Generation fixed-O&M coefficients','ATBe_2024.csv; ATB_CAPEX_FOM_Common_Draws.csv'),
 ('U(VOM)','Fossil and non-fossil variable-O&M coefficients','ATBe_2024.csv; Additional_cost_common_draws_R1.csv'),
 ('U(fuel)','Non-fossil fuel coefficients','ATBe_2024.csv; Additional_cost_common_draws_R1.csv'),
 ('U(import)','Electricity purchase and intertie-cost coefficients','Imports and CAPEX_FOM_Imports outputs; Additional_cost_common_draws_R1.csv'),
 ('U(Canada)','Canadian hydro capital, FOM, VOM and reservoir-CH4 coefficients','Canadian coefficient endpoints; Additional_cost_common_draws_R1.csv')]:
    add(symbol,name,sampled,'Uniform interpolation between the retained endpoint cases. Plot shows the normalized draw U, not dollar-valued coefficients.','One draw per simulation/technology/component key, shared across pathways and years; distinct keys sampled independently. Single-valued bounds remain fixed.',source,kind='theory',dist=stats.uniform(),unit='Normalized draw U (0–1)')
for symbol,name,dist,desc,unit in [
 ('L(solar)','Solar land intensity',stats.gamma(a=4.25,scale=.82),'Gamma(shape=4.25, scale=0.82)','ha/MW'),
 ('L(wind)','Onshore-wind land intensity',stats.gamma(a=3.695,scale=9.382),'Gamma(shape=3.695, scale=9.382)','ha/MW'),
 ('L(hydro)','Canadian hydro land intensity',stats.uniform(loc=43.1,scale=103.6),'Uniform(43.1, 146.7)','ha/MW'),
 ('B(wind)','Onshore-wind bird mortality',stats.gamma(a=.20,scale=2.54),'Gamma(shape=0.20, scale=2.54)','deaths/MW/year'),
 ('Bat(wind)','Onshore-wind bat mortality',stats.gamma(a=1.538,scale=4.160),'Gamma(shape=1.538, scale=4.160)','deaths/MW/year'),
 ('B(solar)','Solar bird mortality',stats.lognorm(s=1.409,scale=1.214),'Lognormal(median=1.214, log-SD=1.409)','deaths/MW/year'),
 ('W(gas)','New-gas cooling-water withdrawals',stats.uniform(loc=15,scale=35),'Uniform(15, 50)','US gal/MWh')]:
    add(symbol,name,sampled,desc,'One coefficient draw per simulation and endpoint, shared across pathways.','Table 1; Review_full_ensemble_R1.py',kind='theory',dist=dist,unit=unit,note='Assumed coefficient PDF; plotted between its 0.1st and 99.5th percentiles.')
physical=pd.read_csv(OUT/'Tables R1/Physical_totals_per_simulation_R1.csv').query("Pathway=='B1'")
for symbol,name,col,scale,unit in [('G(fossil)','Realized existing-fossil generation','Old_Fossil_Fuels_adj_MWh',1e6,'2025–2050 TWh'),('I(final)','Realized imports','Calibrated_Total_import_net_MWh',1e6,'2025–2050 TWh'),('E(storage)','Battery discharge','Calibrated_Battery_discharge_grid',1e6,'2025–2050 TWh'),('E(unserved)','Unserved energy','Calibrated_Shortage_MWh',1e6,'2025–2050 TWh'),('E(surplus)','Surplus / curtailment','Calibrated_Curtailments_MWh',1e6,'2025–2050 TWh'),('M(CO2)','CO2 emission mass','CO2_tons',1e6,'2025–2050 million short tons')]:
    add(symbol,name,output,'Calculated by dispatch, balancing and emission accounting; no separate output distribution is assumed.','Depends jointly on the sampled profiles and fixed model rules.','Yearly_Results.csv; Physical_totals_per_simulation_R1.csv',kind='empirical',values=physical[col]/scale,unit=unit,note='Empirical B1 output distribution across all 1,000 simulations.')
cost=pd.read_csv(RUN/'discount_R1_0.02/Totals R1/All_Costs_per_Simulation.csv')
add('C(p,s)','Total monetized cost',output,'Sum of calculated financial costs, damages and the unserved-energy penalty.','Combines dispatch variability and sampled cost coefficients.','All_Costs_per_Simulation.csv',kind='empirical',values=cost.query("Pathway=='B1'").Total_Costs_mean_bUSD,unit='Billion 2024 USD at 2%',note='Empirical B1 output distribution across all 1,000 simulations; nuclear cost-year question remains open.')
wide=cost.pivot(index='Simulation',columns='Pathway',values='Total_Costs_mean_bUSD')
add('ΔC(p,B1,s)','Paired cost difference',output,'Pathway cost minus B1 within the same simulation.','Preserves covariance from shared inputs; not an independent draw.','All_Costs_per_Simulation.csv; Figure 5',kind='empirical',values=wide.C3-wide.B1,unit='C3 minus B1, billion 2024 USD',note='Representative empirical paired output across 1,000 simulations; other pathways have different distributions.')
df=pd.DataFrame(rows);df.to_csv(DEST/'Variables_R1.csv',index=False)
head='''<!doctype html><html><head><meta charset="utf-8"><title>Variables and distributions</title><style>body{font:15px Arial,sans-serif;color:#24333c;margin:30px;max-width:1500px}h1{font-size:26px}table{border-collapse:collapse;width:100%;table-layout:fixed}th,td{border:1px solid #d5dde1;padding:10px;text-align:left;vertical-align:top}th{background:#24485a;color:white}tr:nth-child(even){background:#f1f5f7}img{width:100%;max-width:300px}small{display:block;margin-top:7px;color:#50616b;font-size:12px}tr{break-inside:avoid}@media print{body{margin:0;font-size:13px}small{font-size:11px}th,td{padding:5px}@page{size:A3 landscape;margin:12mm}}</style></head><body><h1>Variables and distributions</h1><p>Exogenous variables are prescribed or sampled inputs. Endogenous variables are calculated by the model. A fixed input can vary by pathway, year or source without being randomly sampled.</p><p>Plots distinguish assumed coefficient distributions, retained percentile-index frequencies and empirical calculated outputs. The index plots document the retained source file; confirming the exact ARC input still requires the run manifest. Distribution tails are included in the calculations even where a theoretical PDF is cropped for display.</p><table><colgroup><col style="width:16%"><col style="width:12%"><col style="width:24%"><col style="width:23%"><col style="width:25%"></colgroup><thead><tr><th>Variable</th><th>Role</th><th>Distribution or treatment</th><th>Sharing and source</th><th>Representative plot</th></tr></thead><tbody>'''
esc=lambda x:html.escape(str(x))
# fixed inputs have no probability distribution and need no chart.
for r in rows:
    plot=('<img src="'+esc(r['Plot'])+'">') if r['Plot'] else 'Fixed input; no distribution'
    head+=f'<tr><td><b>{esc(r["Variable"])}</b><small>{esc(r["Symbol / quantity"])}</small></td><td>{esc(r["Role"])}</td><td>{esc(r["Distribution / treatment"])}</td><td>{esc(r["Sharing and dependence"])}<small>Source: {esc(r["Source"])}</small></td><td>{plot}<small>{esc(r["Plot meaning"])}</small></td></tr>'
(DEST/'Variables_R1.html').write_text(head+'</tbody></table></body></html>')
print('Saved',len(rows),'variable definitions and representative plots.')
