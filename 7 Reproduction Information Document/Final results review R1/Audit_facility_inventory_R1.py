# distinguish the downloaded fleet from source metadata before replacing Table S4.
"""R1: distinguish the downloaded fleet from source metadata before replacing Table S4."""
from pathlib import Path
import os,json
import pandas as pd
ROOT=Path(os.environ.get('PHASED_R1_ROOT', os.getcwd()))
FINAL=ROOT/'2 Generation Expansion Model/5 Dispatch Curve/2 Advanced Research Computing/1 ARC Codes/Downloads R1/R1_ensemble101_1000_20260920_01/Final'
OUT=Path(os.environ.get('PHASED_REVIEW_OUTPUT_R1',str(Path(__file__).resolve().parent/'Results R1')))/'Tables R1'
meta=pd.read_csv(ROOT/'2 Generation Expansion Model/2 Generation/2 Fossil Generation/1 Existing Fossil Fuels/1 Fossil Fuels Facilities Data/Fossil_Fuel_Facilities_Data.csv')
parts=[]
for d in pd.read_csv(FINAL/'Yearly_Facility_Level_Results.csv',usecols=['Facility_Unit.ID','Simulation','Year','Pathway'],chunksize=500000):
    parts.append(d.groupby('Facility_Unit.ID').agg(First_year=('Year','min'),Last_year=('Year','max'),Rows=('Year','size')))
summary=pd.concat(parts).groupby(level=0).agg({'First_year':'min','Last_year':'max','Rows':'sum'}).reset_index()
assert not meta['Facility_Unit.ID'].duplicated().any()
assert set(summary['Facility_Unit.ID']).issubset(set(meta['Facility_Unit.ID']))
meta=meta.merge(summary,on='Facility_Unit.ID',how='left',validate='one_to_one')
meta['Represented_in_final']=meta.Rows.notna()
meta.to_csv(OUT/'Table_S4_inventory_audit_R1.csv',index=False)
cols=['Facility_Unit.ID','Facility_Name','State','County','Unit_Type','Primary_Fuel_Type','Estimated_NameplateCapacity_MW','Retirement_year','mean_CO2_tons_MW_estimate','mean_NOx_lbs_MW_estimate','mean_SO2_lbs_MW_estimate','First_year','Last_year','Represented_in_final']
meta[cols].to_csv(OUT/'Table_S4_review_R1.csv',index=False)
(OUT/'Facility_inventory_note_R1.txt').write_text(f'{len(summary)} existing units are represented in the downloaded annual facility file; {len(meta)} rows are present in the source metadata. The table retains both sets with an explicit represented flag. Estimated emission-rate metadata are not substituted for operating-model rates. The national donor denominator and exact ARC eligibility rules still require the run manifest.\n')
print((OUT/'Facility_inventory_note_R1.txt').read_text())
