import os
"""Audit native MATLAB outputs and serialized identifier tables; preserve original outputs.
Uses scipy's private MAT-v5 splitting reader only to recover MCOS table field arrays.
Fails on unexpected serialization layout rather than guessing row identities.
"""
from pathlib import Path
from io import BytesIO
import csv,json,hashlib
import numpy as np
from scipy.io import loadmat
from scipy.io.matlab._mio5 import varmats_from_mat
from openpyxl import load_workbook
ROOT=Path('/Users/amirgazar/Downloads/AP4 Model')
ARC=Path('/Users/amirgazar/Documents/GitHub/Decarbonization-Tradeoffs R1.v.2/3 Total Costs/5 Air Pollutant Emissions Costs/AP4_ARC')
RUN=ARC/'outputs/AP4_native_20260920_01'
OUT=RUN/'validated_tables';OUT.mkdir(exist_ok=True)
p=RUN/'native_results.mat'
raw=loadmat(p,variable_names=['MD_Ground','MD_Non_EGU_Point','MD_EGU_Point','wtp','usd_year','wtp_year','__function_workspace__'])
header=p.read_bytes()[:128]
embedded=raw['__function_workspace__'].tobytes()
assert embedded[:4]==b'\x00\x01IM'
parts=varmats_from_mat(BytesIO(header+embedded[8:]))
obj=loadmat(parts[0][1],struct_as_record=False,squeeze_me=True)['__function_workspace__'][0,0]
assert obj._fieldnames==['MCOS']
fields=obj.MCOS['arr'][0].ravel()
expected=[['row','fips','county','fipsst','state'],['row','fips','eis','orispl','name','lon','lat','nei_2017']]
assert list(fields[7])==expected[0] and list(fields[14])==expected[1]
assert int(fields[4])==3108 and int(fields[11])==1814
native_tables=[];checks=[]
def numeric(x):
 try:return float(x)
 except (ValueError,TypeError):return float('nan') if x in ['NA','',None] else None
for which,(di,ni) in enumerate([(2,7),(9,14)]):
 columns=list(fields[ni]);arrays=list(fields[di]);n=[3108,1814][which]
 assert len(arrays)==len(columns) and all(len(a)==n for a in arrays)
 table=[dict(zip(columns,vals)) for vals in zip(*arrays)]
 book=load_workbook(ROOT/'AP4_Inputs'/['AP4_County_List.xlsx','AP4_EGU_List.xlsx'][which],read_only=True,data_only=True)
 vals=list(book.active.values);external=[dict(zip(vals[0],v)) for v in vals[1:]]
 if which:external=[r for r in external if r['nei_2017']=='Yes']
 assert len(external)==n
 for col in columns:
  mismatch=0;maxdiff=0.
  for a,b in zip(table,external):
   x,y=a[col],b[col]
   if col in ['row','fips','fipsst','eis','orispl','lon','lat']:
    x,y=numeric(x),numeric(y)
    if np.isnan(x) and np.isnan(y):continue
    diff=abs(x-y);maxdiff=max(maxdiff,diff)
    if not np.isclose(x,y,rtol=0,atol=1e-10 if col in ['lon','lat'] else 0,equal_nan=True):mismatch+=1
   elif str(x)!=str(y):mismatch+=1
  checks.append(dict(table=['county','egu'][which],field=col,rows=n,mismatches=mismatch,max_numeric_difference=maxdiff))
  assert mismatch==0,(which,col,mismatch)
 native_tables.append(table)
def write(name,rows,folder=OUT):
 with (folder/name).open('w',newline='') as f:
  w=csv.DictWriter(f,fieldnames=list(rows[0]));w.writeheader();w.writerows(rows)
def rows(path):
 with path.open() as f:return list(csv.DictReader(f))
summary=[]
for source,key,which in [('ground','MD_Ground',0),('non_egu_point','MD_Non_EGU_Point',0),('egu_point','MD_EGU_Point',1)]:
 a=raw[key];assert a.shape==(len(native_tables[which]),5)
 native_csv=rows(RUN/f'{source}_native_USD2020_per_metric_tonne.csv')
 csv_values=np.array([[float(r[k]) for k in ['NH3','NOx','PM25','SO2','VOC']] for r in native_csv])
 assert np.allclose(a,csv_values,rtol=5e-14,atol=1e-8)
 py=rows(ROOT/'AP4_Investigation_2026-09-20'/f'{source}_as_downloaded_USD2020_per_metric_tonne.csv')
 b=np.array([[float(r[k]) for k in ['NH3','NOx','PM2.5','SO2','VOC']] for r in py])
 assert np.isfinite(a).all() and (a>=0).all()
 for j,pol in enumerate(['NH3','NOx','PM2.5','SO2','VOC']):
  error=abs(a[:,j]-b[:,j]);relative=error/np.maximum(abs(a[:,j]),1e-300)
  assert error.max()<.05
  summary.append(dict(source_type=source,pollutant=pol,source_count=len(a),max_absolute_difference_USD_per_tonne=float(error.max()),max_relative_difference=float(relative.max()),min_native=float(a[:,j].min()),max_native=float(a[:,j].max())))
 outrows=[]
 for i,ident in enumerate(native_tables[which]):
  r={}
  for k,v in ident.items():
   if k=='fips':r[k]=str(int(v)).zfill(5)
   elif k in ['row','fipsst','eis','orispl']:r[k]='' if np.isnan(v) else str(int(v))
   else:r[k]=v
  assert r['fips']==str(int(float(native_csv[i]['fips']))).zfill(5)
  assert r['row']==str(int(float(native_csv[i]['row'])))
  r.update(source_type=source,model_variant='as_downloaded',usd_year=2020,baseline_year=2017,emission_unit='metric_tonne',validation='native_MATLAB_and_identifier_order_verified')
  r.update(dict(zip(['NH3','NOx','PM2.5','SO2','VOC'],a[i])))
  outrows.append(r)
 write(f'{source}_USD2020_per_metric_tonne.csv',outrows)
 write(f'{source}_NEW_ENGLAND_USD2020_per_metric_tonne.csv',[r for r in outrows if r['fips'][:2] in ['09','23','25','33','44','50']])
write('identifier_checks.csv',checks,RUN);write('coefficient_comparison.csv',summary,RUN)
meta=dict(job_id='7639197',original_workspace_coefficients=40150,county_count=3108,egu_count=1814,all_identifier_fields_match=True,identifier_fields_checked=len(checks),max_absolute_coefficient_difference=max(r['max_absolute_difference_USD_per_tonne'] for r in summary),max_relative_difference=max(r['max_relative_difference'] for r in summary),native_mat_sha256=hashlib.sha256(p.read_bytes()).hexdigest(),wtp=float(raw['wtp'][0,0]),usd_year=int(raw['usd_year'][0,0]),wtp_income_year=int(raw['wtp_year'][0,0]),native_validation_scope='Original downloaded scripts and 1814-source workspace only; excludes proposed index fix and extra45 extension.')
(RUN/'native_validation_audit.json').write_text(json.dumps(meta,indent=2))
print(json.dumps(meta,indent=2))
