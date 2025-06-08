<img alt="GitHub Release Date" src="https://img.shields.io/github/release-date/amirgazar/Decarbonization-Pathways?color=black"> 
<img alt="GitHub last commit" src="https://img.shields.io/github/last-commit/amirgazar/Decarbonization-Pathways?color=gold"> 
<img alt="GitHub repo size" src="https://img.shields.io/github/repo-size/amirgazar/Decarbonization-Pathways?color=cyan"> 
[<img alt="License: CC-BY-4.0" src="https://img.shields.io/badge/license-CC--BY--4.0-lightgrey">](https://creativecommons.org/licenses/by/4.0/) 
[<img alt="Manuscript DOI" src="https://img.shields.io/badge/manuscript_doi-10.XXXX/XXXXX-blue">](https://doi.org/10.XXXX/XXXXX)

<div class="topper-featured-image__inner">
  <figure class="topper-featured-image__figure">
    <!-- Replace with your own featured image -->
    <img src="misc/featured_decarb.jpg" alt="Decarbonization Pathways Overview">
    <figcaption class="topper-featured-image__caption">
      Visualization of stochastic decarbonization scenarios in New England
    </figcaption>
  </figure>
</div>

# Cost uncertainties and ecological impacts drive tradeoffs between electrical system decarbonization pathways in New England, U.S.A.

<p>
  Amir M. Gazar<sup>1,2</sup>, Chloe Jackson<sup>3</sup>, Georgia Mavrommati<sup>3</sup>, Rich B. Howarth<sup>4</sup>, Ryan S.D. Calder<sup>1,2,5,6,7,*</sup>
</p>
<p>
  <sup>1</sup>Dept. of Population Health Sciences, Virginia Tech, Blacksburg, VA, 24061, USA<br/>
  <sup>2</sup>Global Change Center, Virginia Tech, Blacksburg, VA, 24061, USA<br/>
  <sup>3</sup>School for the Environment, University of Massachusetts Boston, Boston, MA, 02125, USA<br/>
  <sup>4</sup>Environmental Program, Dartmouth College, Hanover, NH, 03755, USA<br/>
  <sup>5</sup>Dept. of Civil & Environmental Engineering, Duke University, Durham, NC, 27708, USA<br/>
  <sup>6</sup>Faculty of Health Sciences, Virginia Tech, Roanoke, VA, 24016, USA<br/>
  <sup>7</sup>Dept. of Civil & Environmental Engineering, Virginia Tech, Blacksburg, VA, 24061, USA<br/>
  <strong>* Contact:</strong> rsdc@vt.edu
</p>

<h1 id="abstract" tabindex="-1">Abstract</h1>

Advocates, researchers and policymakers seek characterizations of tradeoffs from diverse decarbonization pathways beyond outputs of optimization models, and robust quantification of uncertainties. We develop and apply to New England, U.S.A. an hourly-scale probabilistic model accepting portfolio decisions and demand as inputs and simulating costs and impacts through 2050. In New England, new natural gas minimizes direct monetary costs, but monetized impacts vastly outweigh these savings compared to decarbonized pathways. Monetized mean expected costs are similar across diverse pathways ($331 billion to $350 billion at 2 % discount rate) but have very different uncertainties (e.g. 90 % CI ≈ 23 % of mean costs for new natural gas vs. 69 % for small modular nuclear). Likewise, land use varies from negligible to 9 890 km². Tracking uncertainties correlated across pathways improves decision support (e.g. > 90 % confidence that constraining transmission with Canada increases costs by ≥ $1.9 billion despite overlapping 90 % CIs for absolute costs).

<p><strong>Keywords:</strong> Decarbonization · Stochastic modeling · Ecological impact · Uncertainty quantification · Energy policy</p>


## Reproduction Information Document
A comprehensive step-by-step guide to reproduce every analysis in this repository (26 pp.):

- **Section 1 (p 3):** Configuration & setup—hardware (macOS 14.5 Sonoma, ARC cluster), R 4.4.2 & RStudio 2024.09.1, package versions, install & run times.  
- **Section 2 (p 4):** Conceptual overview of the modeling framework and code availability.  
- **Section 3 (pp 4–5):** Decarbonization pathways data processing (Excel → R, metadata tagging, year‐range extraction).  
- **Section 4 (pp 5–11):** Generation expansion model scripts—hourly wind/solar CFs, SMR specs, fossil facility & emissions processing, new fossil additions, imports, demand processing, randomization, dispatch‐curve generation.  
- **Section 5 (p 12):** Dispatch‐curve results processing.  
- **Section 6 (pp 12–23):** Total cost modules—CAPEX, FOM, VOM for fossil, non-fossil & imports; fuel & import cost adjustments; GHG & air pollutant emissions cost interpolation & NPV; unmet demand penalties; hydropower cost assumptions & capacity modeling; consolidation of all costs.  
- **Section 6.9 (pp 23–24):** CPI retrieval (BLS API) and supplemental cost calculations (GDP, population).  
- **Section 7 (pp 25–26):** Ecological impact metrics—land use, water withdrawals, avian mortality, viewshed.  
- **Section 8 (p 26):** Figures & diagrams illustrating workflows.

View the full PDF here:  
https://github.com/amirgazar/Decarbonization-Tradeoffs/blob/main/7%20Reproduction%20Information%20Document/Reproduction%20Information%20Document.pdf :contentReference[oaicite:0]{index=0}

## Data Browser

Explore an interactive portal for probabilistic hourly generation and emissions data for New England fossil-fuel power plants (as of March 20, 2025). The portal offers:

- **Power Plant Report & Data Download**  
- **Bulk Data Download**  
- **Templates for Future/New Power Plants**  
- **User Guide**  
- **Citation Information**  
- **Contact Us**  

🔗 Check it out here: https://amirgazar.github.io/powerplants/


<h1 tabindex="-1" id="Copyrights" dir="auto">Copyrights<svg class="octicon octicon-link" viewBox="0 0 16 16" version="1.1" width="16" height="16" aria-hidden="true"><path d="m7.775 3.275 1.25-1.25a3.5 3.5 0 1 1 4.95 4.95l-2.5 2.5a3.5 3.5 0 0 1-4.95 0 .751.751 0 0 1 .018-1.042.751.751 0 0 1 1.042-.018 1.998 1.998 0 0 0 2.83 0l2.5-2.5a2.002 2.002 0 0 0-2.83-2.83l-1.25 1.25a.751.751 0 0 1-1.042-.018.751.751 0 0 1-.018-1.042Zm-4.69 9.64a1.998 1.998 0 0 0 2.83 0l1.25-1.25a.751.751 0 0 1 1.042.018.751.751 0 0 1 .018 1.042l-1.25 1.25a3.5 3.5 0 1 1-4.95-4.95l2.5-2.5a3.5 3.5 0 0 1 4.95 0 .751.751 0 0 1-.018 1.042.751.751 0 0 1-1.042.018 1.998 1.998 0 0 0-2.83 0l-2.5 2.5a1.998 1.998 0 0 0 0 2.83Z"></path></svg></a></h1>

</article>
          </div>

© 2025 Amir M. Gazar et al. All rights reserved.  
This work is licensed under a Creative Commons Attribution 4.0 International License</a>. </br>[<img alt="Static Badge" src="https://img.shields.io/badge/license-CC--BY--4.0-tst">](https://creativecommons.org/licenses/by/4.0/) 
