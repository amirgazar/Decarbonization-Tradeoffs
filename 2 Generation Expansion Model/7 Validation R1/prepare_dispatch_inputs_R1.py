# Purpose: Prepare indexed inputs, run bounded partitions and check final accounting; supports reproducibility of the operational method.
#: bounded local extraction and selected random columns from authoritative inputs.
# No generated draws, donor records, or missing-hour imputation.
import argparse, csv, hashlib, json
from pathlib import Path
p=argparse.ArgumentParser();p.add_argument('--data-root',type=Path,required=True);p.add_argument('--output',type=Path,required=True);p.add_argument('--simulations',default='1,2');p.add_argument('--days',type=int,default=7);p.add_argument('--random-only',action='store_true')
p.add_argument('--flat-data',action='store_true')
a=p.parse_args();ids=sorted(set(int(x) for x in a.simulations.split(',')))
if not ids or min(ids)<1 or not 1<=a.days<=366: p.error('Positive simulation IDs and 1–366 days required')
a.output.mkdir(parents=True,exist_ok=False)
r=a.data_root/'2 Generation Expansion Model/4 Randomization/1 Randomized Data/Random_Sequence.csv'
f=a.data_root/'2 Generation Expansion Model/2 Generation/2 Fossil Generation/1 Existing Fossil Fuels/2 Fossil Fuels Generation and Emissions/Fossil_Fuel_Generation_Emissions.csv'
# explicitly selected legacy ARC flat input layout.
if a.flat_data:r=a.data_root/r.name;f=a.data_root/f.name
sources={str(x):dict(bytes=x.stat().st_size,mtime_ns=x.stat().st_mtime_ns) for x in (f,r)}
print('Extracting requested original random columns',flush=True)
with r.open('rb') as src,(a.output/'Random_Sequence.csv').open('w') as dst:
 h=src.readline();ncols=h.count(b',')+1
 if max(ids)>ncols:raise ValueError('Simulation exceeds available saved columns')
 dst.write(','.join('V'+str(i) for i in ids)+'\n');n=0
 for line in src:
  fields=line.split(b',',max(ids));v=[int(fields[i-1].strip(b'"\r\n')) for i in ids]
  if any(not 1<=x<=99 for x in v):raise ValueError('Invalid saved percentile')
  dst.write(','.join(map(str,v))+'\n');n+=1
rows=0
if not a.random_only:
 print('Extracting fossil records; scanning source once',flush=True)
 rows=0
 with f.open('rb') as src,(a.output/f.name).open('wb') as dst:
  header=src.readline()
  if header.split(b',')[:3]!=[b'DayLabel',b'Hour',b'Facility_Unit.ID']:raise ValueError('Unexpected fossil key columns')
  dst.write(header)
  for line in src:
   if 1<=int(line.split(b',',1)[0])<=a.days:dst.write(line);rows+=1
outputs={x.name:dict(bytes=x.stat().st_size,sha256=hashlib.sha256(x.read_bytes()).hexdigest()) for x in a.output.glob('*.csv')}
(a.output/'manifest.json').write_text(json.dumps(dict(sources=sources,outputs=outputs,days=a.days,simulations=ids,random_rows=n,random_source_columns=ncols,fossil_rows=rows),indent=2))
print(f'Prepared {rows} fossil rows; {n} saved percentile rows for simulations {ids}',flush=True)
