# retain the original plotting layouts and colours while inserting current county and ecological results.
"""R1: retain the original plotting layouts and colours while inserting current county and ecological results."""
from pathlib import Path
import json, hashlib, re, os, shutil
import numpy as np
import pandas as pd
import matplotlib
matplotlib.use('Agg')
import matplotlib.pyplot as plt
from matplotlib.patches import Polygon
from matplotlib.colors import TwoSlopeNorm
from matplotlib.cm import ScalarMappable
R=Path(os.environ.get('PHASED_R1_ROOT', os.getcwd()))
P=R/'7 Reproduction Information Document/Final results review R1'
O=Path(os.environ.get('PHASED_AUTHOR_FIGURE_OUTPUT_R1',str(P/'Results R1/Figures R1')));O.mkdir(parents=True,exist_ok=True)
plt.rcParams.update({'font.size':16,'svg.fonttype':'none','pdf.fonttype':42,'axes.spines.top':False,'axes.spines.right':False})
def save(fig,name):
 for ext in ['png','pdf','svg']:fig.savefig(O/(name+'.'+ext),dpi=300,bbox_inches='tight',facecolor='white')
 plt.close(fig)

# adapt the original Emissions.ipynb county means and A-relative changes to B1/D maps and bars; retain every simulation.
src=R/'6 Figures/Publication revision R1/Figure6_source_R1.csv'
d=pd.read_csv(src)
computed=(d.Mean_pathway_mUSD-d.Mean_A_mUSD).divide(d.Mean_A_mUSD.replace(0,np.nan)).multiply(100)
assert np.allclose(computed,d.Percent_change_in_means,equal_nan=True)
topo=json.loads((R/'4 External Data/U.S. Census Geo Data/New_England_county_boundaries.json').read_text())
arcs=[np.cumsum(np.array(a),axis=0)*np.array(topo['transform']['scale'])+np.array(topo['transform']['translate']) for a in topo['arcs']]
states=dict(zip(['23','33','50','25','44','09'],['ME','NH','VT','MA','RI','CT']));shapes=[]
for g in topo['objects']['counties']['geometries']:
 place=g['properties']['name']+', '+states[g['properties']['state']]
 for poly in (g['arcs'] if g['type']=='MultiPolygon' else [g['arcs']]):
  shapes.append((place,np.concatenate([arcs[a] if a>=0 else arcs[~a][::-1] for a in poly[0]])))
vmax=max(1,np.nanmax(np.abs(d.loc[d.Pathway.isin(['B1','D']),'Percent_change_in_means'])));norm=TwoSlopeNorm(vmin=-vmax,vcenter=0,vmax=vmax)
fig,axs=plt.subplots(2,2,figsize=(13,14),gridspec_kw={'width_ratios':[1,1.25]})
lim=max(abs(d.loc[d.Pathway.isin(['B1','D']),'Mean_change_mUSD']))*1.1
for row,path in enumerate(['B1','D']):
 z=d[d.Pathway==path].set_index('Source_county');ma,ba=axs[row]
 for place,ring in shapes:
  v=z.at[place,'Percent_change_in_means'] if place in z.index else np.nan
  ma.add_patch(Polygon(ring,closed=True,facecolor=plt.get_cmap('RdBu_r')(norm(v)) if np.isfinite(v) else '#eeeeee',edgecolor='white',lw=.35))
 ma.set(xlim=(-73.8,-66.8),ylim=(40.8,47.6));ma.set_aspect(1/np.cos(np.deg2rad(44)));ma.axis('off')
 ma.set_title(f'({"ac"[row]}) {path}: percentage change from A',loc='left',fontsize=16)
 v=z.Mean_change_mUSD.sort_values();ba.barh(v.index,v,color=['#2166ac' if n<0 else '#b2182b' for n in v],height=.7)
 ba.axvline(0,color='#444',lw=.8);ba.set_xlim(-lim,lim);ba.tick_params(axis='y',labelsize=14);ba.grid(axis='x',alpha=.15);ba.set_axisbelow(True)
 ba.set_title(f'({"bd"[row]}) {path}: mean change from A',loc='left',fontsize=16);ba.set_xlabel('Million 2024 USD')
fig.subplots_adjust(left=.025,right=.98,top=.965,bottom=.085,hspace=.20,wspace=.54)
cax=fig.add_axes([.075,.032,.30,.014]);fig.colorbar(ScalarMappable(norm=norm,cmap='RdBu_r'),cax=cax,orientation='horizontal',label='Change in mean damages (%)')
save(fig,'Figure6_R1')
d[d.Pathway.isin(['B1','D'])].to_csv(O/'Figure6_source_R1.csv',index=False)

# adapt mean_df.plot(stacked=True) from the four original ecological notebooks, retaining their technology colours.
eco=pd.read_csv(P/'Results R1/Tables R1/Ecological_draws_R1.csv');paths=['B1','B2','B3','C1','C2','C3','D']
specs=[('Land occupation',['Canadian Hydro','New NG','SMR','Onshore Wind','Solar'],['#9EB1D4','#E9967A','#EFD9B4','#B4D6E3','#FBE7A1'],['Canadian hydro','Natural gas','SMRs','Onshore wind','Solar'],'Land occupation (million ha)'),('Bird and bat mortality',['Onshore birds','Onshore bats','Solar birds'],['#B4D6E3','#89B9CF','#FBE7A1'],['Onshore wind birds','Onshore wind bats','Solar birds'],'Annual deaths in 2050 (million)'),('Water withdrawals',['SMRs','New gas'],['#EFD9B4','#E9967A'],['SMRs','Natural gas'],'2025–2050 withdrawals (trillion US gal)')]
fig,axs=plt.subplots(2,2,figsize=(13,12));positions=[axs[0,0],axs[0,1],axs[1,1]]
for k,(endpoint,techs,colors,labels,ylabel) in enumerate(specs):
 ax=positions[k];z=eco[eco.Endpoint==endpoint];mean_df=z.groupby(['Pathway','Technology']).Value.mean().unstack().reindex(index=paths,columns=techs)
 mean_df.plot(kind='bar',stacked=True,color=colors,width=.7,ax=ax,legend=False)
 total=z.groupby(['Simulation','Pathway']).Value.sum().unstack().reindex(columns=paths);mu=total.mean();lo=total.quantile(.05);hi=total.quantile(.95)
 ax.errorbar(np.arange(7),mu,yerr=np.maximum(0,np.vstack([mu-lo,hi-mu])),fmt='none',ecolor='black',capsize=4)
 ax.set_xticklabels(paths,rotation=0);ax.set_xlabel('');ax.set_ylabel(ylabel);ax.set_title(f'({"abd"[k]}) {endpoint}',loc='left')
 ax.legend(labels,loc='upper left',bbox_to_anchor=(0,-.13),ncol=2,frameon=False,fontsize=14)
 ax.set_ylim(0,hi.max()*1.12)

# retain the original ten-placement visibility means; their summed marginal ranges are not a total simulation interval.
v=pd.read_excel(R/'5 Ecological impacts/viewshed_summary.xlsx',header=1).set_index('Technology')
labels=list(v.index[:-1]);means=pd.DataFrame({p:[float(re.match(r'([\d.]+)',str(v.at[t,p]))[1]) for t in labels] for p in paths},index=labels).T
ax=axs[1,0];means.plot(kind='bar',stacked=True,color=['#D1B1D6','#F4B6C2','#A9CAD6','#FBE7A1'],width=.7,ax=ax,legend=False)
ax.set_xticklabels(paths,rotation=0);ax.set_xlabel('');ax.set_ylabel('Summed population exposures (million)');ax.set_title('(c) Visibility: retained ten-placement means',loc='left')
ax.legend(['Québec transmission','NYISO transmission','Offshore wind','Solar'],loc='upper left',bbox_to_anchor=(0,-.13),ncol=2,frameon=False,fontsize=14)
fig.subplots_adjust(left=.075,right=.99,top=.96,bottom=.10,wspace=.32,hspace=.52)
save(fig,'Figure7_R1')
means.to_csv(O/'Figure7_visibility_source_R1.csv')
sources=[R/'5 Ecological impacts'/n for n in ['Landuse.ipynb','Avian_mortality.ipynb','Water_withdrawals.ipynb','visual_impacts.ipynb']]+[R/'6 Figures/Emissions/Emissions.ipynb',R/'6 Figures/Publication revision R1/Figure6_county_R1.py',src,P/'Results R1/Tables R1/Ecological_draws_R1.csv']
(O/'Figure_sources_R1.json').write_text(json.dumps([{'source':str(p),'sha256':hashlib.sha256(p.read_bytes()).hexdigest()} for p in sources],indent=2))
print('Saved Figure 6 B1/D maps and bars; Figure 7 original stacked-bar style with current ecological draws.')

# redraw the retained index and uniform plots at table size so their labels remain readable.
indices=[]
with (R/'2 Generation Expansion Model/4 Randomization/1 Randomized Data/Random_Sequence.csv').open() as f:
 f.readline()
 for line in f:indices.append([int(v.strip('"\n\r')) for v in line.split(',',1000)[:1000]])
indices=np.asarray(indices)
for num,offset in [(16,0),(19,3),(22,None),(23,-1),(25,-1),(26,-1),(27,-1),(28,-1)]:
 fig,ax=plt.subplots(figsize=(1.5,.85))
 if offset==-1:
  ax.plot([0,0,1,1],[0,1,1,0],color='#315d82',lw=1);ax.fill_between([0,1],1,color='#9eb1d4',alpha=.4);ax.set(xlim=(-.03,1.03),ylim=(0,1.2),xticks=[0,.5,1],yticks=[],xlabel='Uniform draw U')
 else:
  chosen=indices if offset is None else indices[(np.arange(len(indices))*6+offset)%len(indices)]
  counts=np.bincount(chosen.ravel(),minlength=100)[1:100];ax.bar(np.arange(1,100),counts/counts.sum(),width=1,color='#9eb1d4');ax.set(xlim=(1,99),xticks=[1,50,99],yticks=[],xlabel='Percentile index')
 ax.tick_params(labelsize=8,pad=1);ax.xaxis.label.set_size(8);fig.tight_layout(pad=.3)
 fig.savefig(O/f'Table_distribution_{num:02d}_R1.png',dpi=300);plt.close(fig)

# restore the author's original diagrams after the general figure runner has produced its draft alternatives.
for p in (P/'Original figures R1').glob('*.png'):shutil.copy2(p,O/p.name)
