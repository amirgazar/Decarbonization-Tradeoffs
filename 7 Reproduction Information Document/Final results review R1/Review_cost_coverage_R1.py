# quantify uncovered nuclear years separately because filling them changes an input assumption.
"""R1: quantify uncovered nuclear years separately because filling them changes an input assumption."""
from pathlib import Path
import os,json
import numpy as np
import pandas as pd
ROOT=Path(os.environ.get('PHASED_R1_ROOT', os.getcwd()))
RUN=Path(os.environ['PHASED_COST_OUTPUT_R1'])
OUT=Path(os.environ.get('PHASED_REVIEW_OUTPUT_R1',str(Path(__file__).resolve().parent/'Results R1')))/'Tables R1'
OUT.mkdir(parents=True,exist_ok=True)
cfg=pd.read_csv(RUN/'Run_settings_R1.csv') if (RUN/'Run_settings_R1.csv').exists() else None
final=ROOT/'2 Generation Expansion Model/5 Dispatch Curve/2 Advanced Research Computing/1 ARC Codes/Downloads R1/R1_ensemble101_1000_20260920_01/Final'
y=pd.read_csv(final/'Yearly_Results.csv',usecols=['Simulation','Pathway','Year','Nuclear_TWh'])
y=y[y.Year.between(2025,2029)].copy()
atb=pd.read_csv(ROOT/'7 Reproduction Information Document/Cost production audit R1/Inputs R1/ATB_2024_numeric_R1.csv',low_memory=False)
# retain the coefficient coverage check because missing or duplicate ATB keys can silently alter costs.
coverage_keys = [['UtilityPV', 'Class5', 'CAPEX'], ['UtilityPV', 'Class5', 'Fixed O&M'], ['LandbasedWind', 'Class4', 'CAPEX'], ['LandbasedWind', 'Class4', 'Fixed O&M'], ['OffShoreWind', 'Class4', 'CAPEX'], ['OffShoreWind', 'Class4', 'Fixed O&M'], ['Commercial Battery Storage', '8Hr Battery Storage', 'CAPEX'], ['Commercial Battery Storage', '8Hr Battery Storage', 'Fixed O&M'], ['Nuclear', 'Nuclear - Large', 'CAPEX'], ['Nuclear', 'Nuclear - Large', 'Fixed O&M'], ['Nuclear', 'Nuclear - Large', 'Variable O&M'], ['Nuclear', 'Nuclear - Large', 'Fuel'], ['Nuclear', 'Nuclear - Small', 'CAPEX'], ['Nuclear', 'Nuclear - Small', 'Fixed O&M'], ['Nuclear', 'Nuclear - Small', 'Variable O&M'], ['Nuclear', 'Nuclear - Small', 'Fuel'], ['Hydropower', 'NSD1', 'CAPEX'], ['Hydropower', 'NSD1', 'Fixed O&M'], ['Biopower', 'Dedicated', 'CAPEX'], ['Biopower', 'Dedicated', 'Fixed O&M'], ['Biopower', 'Dedicated', 'Variable O&M'], ['Biopower', 'Dedicated', 'Fuel']]
expected = pd.DataFrame([(t,d,p,y,c) for t,d,p in coverage_keys for y in range(2025,2051) for c in ['Advanced','Moderate','Conservative']], columns=['Technology','Detail','Parameter','Year','Case'])
lookup = atb[(atb.core_metric_case=='Market') & (atb.crpyears==30)].rename(columns={'technology':'Technology','techdetail':'Detail','core_metric_parameter':'Parameter','core_metric_variable':'Year','scenario':'Case'})
counts = lookup.groupby(['Technology','Detail','Parameter','Year','Case']).size().rename('Records').reset_index()
coverage = expected.merge(counts, on=['Technology','Detail','Parameter','Year','Case'], how='left', validate='one_to_one')
coverage['Records'] = coverage.Records.fillna(0).astype(int)
coverage.to_csv(OUT/'Nonfossil_coefficient_coverage_R1.csv',index=False)
assert not (coverage.Records>1).any(), 'Duplicate non-fossil coefficient keys require review.'
missing = coverage[coverage.Records==0]
assert ((missing.Technology=='Nuclear') & (missing.Year<2030)).all(), 'Unexpected coefficient-year gaps require review.'
z=atb[(atb.technology=='Nuclear')&(atb.techdetail=='Nuclear - Large')&(atb.core_metric_case=='Market')&(atb.crpyears==30)&(atb.core_metric_variable==2030)]
results=[]
for rate in [.015,.02,.025]:
    draws=pd.read_csv(RUN/'discount_R1_0.015/Totals R1/Additional_cost_common_draws_R1.csv')
    base=y.assign(PV_MWh=y.Nuclear_TWh*1e6/(1+rate)**(y.Year-2024)).groupby(['Simulation','Pathway']).PV_MWh.sum().reset_index()
    for comp,param in [('FOM','Fixed O&M'),('VOM','Variable O&M'),('Fuel','Fuel')]:
        b=z[z.core_metric_parameter==param].set_index('scenario').value
        assert {'Advanced','Conservative'}.issubset(b.index)
        # include fixed O&M because its large-nuclear coefficient series also starts in 2030.
        active_draws=pd.read_csv(RUN/'discount_R1_0.015/Totals R1/ATB_CAPEX_FOM_Common_Draws.csv') if comp=='FOM' else draws
        u=active_draws[(active_draws.Technology=='Nonfossil:Nuclear')&(active_draws.Component==comp)][['Simulation','U']]
        if comp=='FOM':
            schedule=pd.concat([v.assign(Pathway=k) for k,v in pd.read_excel(ROOT/'1 Decarbonization Pathways/Decarbonization_Pathways.xlsx',sheet_name=None).items()])
            cap=schedule[pd.to_numeric(schedule.Year,errors='coerce').between(2025,2029)][['Pathway','Year','Nuclear']]
            active=y[['Simulation','Pathway','Year']].merge(cap,on=['Pathway','Year'],validate='many_to_one')
            active['PV_Activity']=active.Nuclear*1000/(1+rate)**(active.Year-2024)
            activity=active.groupby(['Simulation','Pathway']).PV_Activity.sum().reset_index()
        else:activity=base.rename(columns={'PV_MWh':'PV_Activity'})
        d=activity.merge(u,on='Simulation',validate='many_to_one')
        d['Added_bUSD']=d.PV_Activity*(b['Advanced']+d.U*(b['Conservative']-b['Advanced']))/1e9
        d['Rate']=rate;d['Component']=comp
        results.append(d)
all=pd.concat(results)
all.to_csv(OUT/'Nuclear_2025_2029_proxy_sensitivity_R1.csv',index=False)
tot=all.groupby(['Rate','Simulation','Pathway']).Added_bUSD.sum().reset_index()
assert tot.groupby(['Rate','Simulation']).Added_bUSD.nunique().max()==1
summary=tot.groupby(['Rate','Pathway']).Added_bUSD.agg(Mean='mean',P05=lambda v:v.quantile(.05),P95=lambda v:v.quantile(.95)).reset_index()
summary.to_csv(OUT/'Nuclear_coverage_sensitivity_summary_R1.csv',index=False)
(OUT/'Nuclear_coverage_note_R1.txt').write_text('The main cost run retains the source coefficient-year coverage. This separate sensitivity applies each 2030 large-nuclear fixed-O&M, variable-O&M and fuel coefficient to 2025–2029, using the same simulation-indexed uniform draw as the corresponding component. The added cost is identical across pathways within every simulation, so paired differences are unchanged. Author decision required before adopting this extrapolation in the primary results. This is not a new dispatch run.\n')
print(summary.to_string(index=False))
