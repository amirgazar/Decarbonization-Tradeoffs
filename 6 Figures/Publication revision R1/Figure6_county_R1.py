# Purpose: Show county means, percentage changes and requested map-and-bar alternatives.
"""R1: show mean county damages and percentage changes, with maps and signed bars requested by the author."""
import json
import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
from matplotlib.colors import TwoSlopeNorm
from matplotlib.patches import Polygon
from matplotlib.cm import ScalarMappable

def county_figures(county, root, output, save):
    paths=['A','B1','B2','B3','C1','C2','C3','D']
    means=county.groupby(['Place','Pathway']).npv_total_air_emission_USD.mean().unstack().reindex(columns=paths).fillna(0)/1e6
    delta=means[paths[1:]].subtract(means.A,axis=0)
    # use the percentage change in means; a zero reference has no defined percentage change.
    percent=delta.div(means.A.replace(0,np.nan),axis=0)*100
    rows=[]
    for place in means.index:
        for path in paths[1:]:
            rows.append([place,path,means.loc[place,'A'],means.loc[place,path],delta.loc[place,path],percent.loc[place,path]])
    pd.DataFrame(rows,columns=['Source_county','Pathway','Mean_A_mUSD','Mean_pathway_mUSD','Mean_change_mUSD','Percent_change_in_means']).to_csv(output/'Figure6_source_R1.csv',index=False)
    assert np.allclose(delta,means[paths[1:]].to_numpy()-means.A.to_numpy()[:,None])
    vmax=max(np.nanmax(np.abs(percent.to_numpy())),1)
    norm=TwoSlopeNorm(vmin=-vmax,vcenter=0,vmax=vmax)
    fig,axes=plt.subplots(1,2,figsize=(14.5,max(8,len(means)*.29)),gridspec_kw={'width_ratios':[8,7]})
    for ax,frame,title,cmap,n in [(axes[0],means,'(a) Mean damages','Blues',None),(axes[1],percent,'(b) Change from A (%)','RdBu_r',norm)]:
        im=ax.imshow(np.ma.masked_invalid(frame),aspect='auto',cmap=cmap,norm=n)
        ax.set_xticks(range(len(frame.columns)),frame.columns);ax.xaxis.tick_top()
        ax.set_yticks(range(len(frame)),frame.index if ax is axes[0] else ['']*len(frame))
        ax.tick_params(labelsize=9);ax.set_title(title,pad=30,loc='left',fontweight='bold')
        for i in range(len(frame)):
            for j in range(len(frame.columns)):
                v=frame.iloc[i,j]
                label='N/A' if not np.isfinite(v) else (f'{v:+.0f}%' if n else f'{v:.1f}')
                white=np.isfinite(v) and (abs(v)>vmax*.58 if n else v>np.nanmax(frame)*.58)
                ax.text(j,i,label,ha='center',va='center',fontsize=7.7,color='white' if white else '#202020')
        fig.colorbar(im,ax=ax,orientation='horizontal',pad=.045,fraction=.035,label='Change in mean damages (%)' if n else 'Mean damages (million 2024 USD)')
    fig.suptitle('Air-pollution damages attributed to source counties',y=.985,fontweight='bold')
    fig.text(.5,.012,'1,000 simulations; 2% discount rate. Percentage = 100 × (pathway mean − A mean) / A mean. N/A: zero A mean.',ha='center',fontsize=9)
    fig.subplots_adjust(left=.17,right=.99,top=.85,bottom=.1,wspace=.09)
    save(fig,'Figure6_R1')

    # use the retained county boundaries so the map requires no new geographic inputs.
    topo=json.loads((root/'4 External Data/U.S. Census Geo Data/New_England_county_boundaries.json').read_text())
    scale=np.array(topo['transform']['scale']);translate=np.array(topo['transform']['translate'])
    arcs=[np.cumsum(np.array(a),axis=0)*scale+translate for a in topo['arcs']]
    states={'09':'CT','23':'ME','25':'MA','33':'NH','44':'RI','50':'VT'}
    shapes=[]
    for g in topo['objects']['counties']['geometries']:
        place=g['properties']['name']+', '+states[g['properties']['state']]
        polys=g['arcs'] if g['type']=='MultiPolygon' else [g['arcs']]
        for poly in polys:
            ring=np.concatenate([arcs[a] if a>=0 else arcs[~a][::-1] for a in poly[0]])
            shapes.append((place,ring))
    assert set(means.index).issubset({p for p,_ in shapes}), 'Source county missing from retained map.'
    for path in paths[1:]:
        fig,(mapax,barax)=plt.subplots(1,2,figsize=(13,9),gridspec_kw={'width_ratios':[1,1.05]})
        for place,ring in shapes:
            v=percent.loc[place,path] if place in percent.index else np.nan
            color=plt.get_cmap('RdBu_r')(norm(v)) if np.isfinite(v) else '#eeeeee'
            mapax.add_patch(Polygon(ring,closed=True,facecolor=color,edgecolor='white',lw=.35))
        mapax.set(xlim=(-73.8,-66.8),ylim=(40.8,47.6));mapax.set_aspect(1/np.cos(np.deg2rad(44)));mapax.axis('off')
        mapax.set_title('(a) Change in mean damages (%)',loc='left',fontsize=12)
        fig.colorbar(ScalarMappable(norm=norm,cmap='RdBu_r'),ax=mapax,orientation='horizontal',fraction=.035,pad=.02,label='Change from A (%)')
        vals=delta[path].sort_values();colors=['#2166ac' if v<0 else '#b2182b' for v in vals]
        barax.barh(vals.index,vals,color=colors,height=.7);barax.axvline(0,color='#444444',lw=.8)
        barax.tick_params(axis='y',labelsize=9);barax.grid(axis='x',alpha=.15);barax.set_axisbelow(True)
        barax.set_xlabel('Mean change from A (million 2024 USD)');barax.set_title('(b) Mean increase or decrease',loc='left',fontsize=12)
        fig.suptitle(f'{path} relative to A — damages attributed to source counties',fontweight='bold',y=.98)
        fig.text(.5,.015,'1,000 simulations; 2% discount rate. Blue: decrease. Red: increase. Grey counties: no represented source or undefined percentage.',ha='center',fontsize=9)
        fig.subplots_adjust(left=.03,right=.98,top=.92,bottom=.09,wspace=.48)
        save(fig,f'Figure6_map_{path}_R1')
