<img alt="GitHub Release Date" src="https://img.shields.io/github/release-date/amirgazar/Decarbonization-Tradeoffs?color=black"> <img alt="GitHub last commit" src="https://img.shields.io/github/last-commit/amirgazar/Decarbonization-Tradeoffs?color=gold"> <img alt="GitHub repo size" src="https://img.shields.io/github/repo-size/amirgazar/Decarbonization-Tradeoffs?color=cyan"> [<img alt="License: CC-BY-4.0" src="https://img.shields.io/badge/license-CC--BY--4.0-lightgrey">](https://creativecommons.org/licenses/by/4.0/) [![preprint-doi](https://img.shields.io/badge/preprint_doi-10.31224/4684--4505/ad8fce-blue)](https://doi.org/10.31224/4684)[![Launch the Portal](https://img.shields.io/badge/Launch%20Portal-Visit-blue?style=for-the-badge)](https://amirgazar.github.io/powerplants/)


# Cost uncertainties and ecological impacts drive tradeoffs between electrical system decarbonization pathways in New England, U.S.A.

<p>
  Amir M. Gazar<sup>1,2</sup>, Chloe Jackson<sup>3</sup>, Georgia Mavrommati<sup>3</sup>, Rich B. Howarth<sup>4</sup>, Ryan S.D. Calder<sup>1,2,5,*</sup>
</p>
<p>
  <sup>1</sup>Dept. of Population Health Sciences, Virginia Tech, Blacksburg, VA, 24061, USA<br/>
  <sup>2</sup>Global Change Center, Virginia Tech, Blacksburg, VA, 24061, USA<br/>
  <sup>3</sup>School for the Environment, University of Massachusetts Boston, Boston, MA, 02125, USA<br/>
  <sup>4</sup>Environmental Program, Dartmouth College, Hanover, NH, 03755, USA<br/>
  <sup>5</sup>Department of Civil and Environmental Engineering, Duke University, Durham, NC, 27708, USA<br/>
  <strong>* Contact:</strong> rsdc@vt.edu
</p>

<h1 id="abstract" tabindex="-1">Abstract</h1>

Advocates, researchers and policymakers seek characterizations of tradeoffs from diverse decarbonization pathways beyond outputs of optimization models, and robust quantification of uncertainties. We develop and apply to New England, U.S.A. an hourly-scale probabilistic model accepting portfolio decisions and demand as inputs and simulating costs and impacts through 2050. In New England pathway incorporating small modular reactors lowers total social costs but increases cost uncertainties compared to the target “all options” pathway currently guiding policymakers ($470 ± 97 billion vs. $477 ± 87.5 billion by 2050). New natural gas minimizes direct monetary costs, but monetized impacts vastly outweigh these savings compared to decarbonized pathways (e.g., $262 billion above “all options”). Likewise, land use varies from negligible to 9,890 km<sup>2</sup> across pathways. Tracking uncertainties correlated across pathways improves decision support (e.g., >90% confidence that constraining transmission with Canada increases costs by ≥ $19.4 billion despite overlapping 90% CIs for absolute costs).


<p><strong>Keywords:</strong> integrated assessment model; discount rate; capacity expansion model; decarbonization; renewable energy; energy policy; cost-benefit analysis</p>

## Computer Code and Data Download
<a href="https://drive.google.com/file/d/1_4CJ6x3oWdZjMocYRkqfHUSGeeRW7L50/view?usp=sharing" target="_blank" rel="noopener noreferrer">
  Download the computer code and all required data
</a>
<p>Note: 30 GB of space is required.</p>

## Reproduction Information Document
A comprehensive step-by-step guide to reproduce every analysis in this repository:

- **Section 1:** Configuration & setup, hardware (macOS 14.5 Sonoma, ARC cluster), R 4.4.2 & RStudio 2024.09.1, package versions, install & run times.  
- **Section 2:** Conceptual overview of the modeling framework and code availability.  
- **Section 3:** Decarbonization pathways data processing (Excel → R, metadata tagging, year‐range extraction).  
- **Section 4:** Generation expansion model scripts, hourly wind/solar CFs, SMR specs, fossil facility & emissions processing, new fossil additions, imports, demand processing, randomization, dispatch‐curve generation.  
- **Section 5:** Dispatch‐curve results processing.  
- **Section 6:** Total cost modules; CAPEX, FOM, VOM for fossil, non-fossil & imports; fuel & import cost adjustments; GHG & air pollutant emissions cost interpolation & NPV; unmet demand penalties; hydropower cost assumptions & capacity modeling; consolidation of all costs.  
- **Section 7:** Ecological impact metrics; land use, water withdrawals, avian mortality, viewshed.  
- **Section 8:** Figures & diagrams illustrating workflows.

<a href="https://github.com/amirgazar/Decarbonization-Tradeoffs/blob/main/7%20Reproduction%20Information%20Document/Reproduction%20Information%20Document.pdf" target="_blank" rel="noopener noreferrer">
  View the full PDF here
</a>

## Fossil‐Fuel Power Plants Data Portal
An interactive web portal offering probabilistic hourly generation and emissions data for all New England fossil‐fuel power plants. It provides:
- **State Power Plants Data Download**  
  Download probabilistic hourly generation and emissions data for all fossil‐fuel plants in a selected state.
- **Individual Power Plant Report and Data Download**  
  Access pre-generated PDF reports and CSV files showing hourly outputs and emissions for a single facility.
- **Templates for Future/New Power Plants**  
  Get probablistic templates (from similar facilities) to plug in new plant specifications and run them through the probabilistic models.
- **Historical Generation and Emissions (U.S. EPA CAMPD)**  
  Retrieve historical U.S. EPA CAMPD data stored in a Harvard dataverse.
- **Citation**  
  Instructions on how to cite this portal and its underlying datasets in your publications.
- **Contact Us**  
  Support contact details for questions, feedback, or technical issues.

🔗 **Launch the portal:** https://amirgazar.github.io/powerplants/  

<h1 tabindex="-1" id="Copyrights and Citation" dir="auto">Copyrights and Citation<svg class="octicon octicon-link" viewBox="0 0 16 16" version="1.1" width="16" height="16" aria-hidden="true"><path d="m7.775 3.275 1.25-1.25a3.5 3.5 0 1 1 4.95 4.95l-2.5 2.5a3.5 3.5 0 0 1-4.95 0 .751.751 0 0 1 .018-1.042.751.751 0 0 1 1.042-.018 1.998 1.998 0 0 0 2.83 0l2.5-2.5a2.002 2.002 0 0 0-2.83-2.83l-1.25 1.25a.751.751 0 0 1-1.042-.018.751.751 0 0 1-.018-1.042Zm-4.69 9.64a1.998 1.998 0 0 0 2.83 0l1.25-1.25a.751.751 0 0 1 1.042.018.751.751 0 0 1 .018 1.042l-1.25 1.25a3.5 3.5 0 1 1-4.95-4.95l2.5-2.5a3.5 3.5 0 0 1 4.95 0 .751.751 0 0 1-.018 1.042.751.751 0 0 1-1.042.018 1.998 1.998 0 0 0-2.83 0l-2.5 2.5a1.998 1.998 0 0 0 0 2.83Z"></path></svg></a></h1>


</article>
</div>

<div class="citation">
  <p>
    <strong>Gazar, A. M.</strong>, Jackson, C., Mavrommati, G., Howarth, R. B., &amp; Calder, R. (2025). 
    <em>Cost uncertainties and ecological impacts drive tradeoffs between electrical system decarbonization pathways in New England, U.S.A., Engineering Archive. </em> 
    <a href="https://doi.org/10.31224/4684" target="_blank" rel="noopener">https://doi.org/10.31224/4684</a>
  </p>

  <p>
    © 2025 Amir M. Gazar et al. All rights reserved.  
    This work is licensed under a 
    <a href="https://creativecommons.org/licenses/by/4.0/" target="_blank" rel="license noopener">
      Creative Commons Attribution 4.0 International License
    </a>.
  </p>

  <p>
    <a href="https://creativecommons.org/licenses/by/4.0/" target="_blank" rel="license noopener">
      <img alt="CC BY 4.0 License" src="https://img.shields.io/badge/license-CC--BY--4.0-lightgrey">
    </a>
  </p>
</div>
