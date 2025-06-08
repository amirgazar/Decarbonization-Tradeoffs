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

<h1 id="contents" tabindex="-1">Contents</h1>
<ul>
  <li><a href="#abstract">Abstract</a></li>
  <li><a href="#introduction">Introduction</a></li>
  <li><a href="#repository-structure">Repository Structure</a></li>
  <li><a href="#installation">Installation</a></li>
  <li><a href="#usage">Usage</a></li>
  <li><a href="#results">Results</a></li>
  <li><a href="#reproduction-information-document">Reproduction Information Document</a></li>
  <li><a href="#authors">Authors</a></li>
  <li><a href="#citation">Citation</a></li>
  <li><a href="#license">License</a></li>
</ul>

<h1 id="introduction" tabindex="-1">Introduction</h1>
<p>
  *Add a few sentences about the background, motivation and objectives of this study.*
</p>

<h1 id="repository-structure" tabindex="-1">Repository Structure</h1>
<pre><code>
.ipynb_checkpoints/
0 Stochastic Power Plant Model/
1 Decarbonization Pathways/
2 Generation Expansion Model/
3 Total Costs/
4 External Data/
5 Ecological impacts/
6 Figures/
7 Reproduction Information Document/
README.md
</code></pre>

<h1 id="installation" tabindex="-1">Installation</h1>
<ol>
  <li>Clone the repository.<br/>
      <code>git clone https://github.com/amirgazar/Decarbonization-Pathways.git</code>
  </li>
  <li>Create & activate environment:<br/>
      <code>conda env create -f environment.yml</code><br/>
      <code>conda activate decarb-env</code>
  </li>
  <li>Install Python deps:<br/>
      <code>pip install -r requirements.txt</code>
  </li>
</ol>

<h1 id="usage" tabindex="-1">Usage</h1>
<p>
  *Run individual notebooks or scripts:*<br/>
  <code>jupyter notebook "1 Decarbonization Pathways/analysis.ipynb"</code>
</p>

<h1 id="results" tabindex="-1">Results</h1>
<p>
  *Key outputs—cost distributions, ecological‐impact maps, sensitivity analyses—are in <code>6 Figures/</code>.*
</p>

<h1 id="reproduction-information-document" tabindex="-1">Reproduction Information Document</h1>
<p>
  Detailed reproduction steps, data sources, and parameter settings are in<br/>
  <code>7 Reproduction Information Document/</code>
</p>

<h1 id="authors" tabindex="-1">Authors</h1>
<p>
  Amir M. Gazar<sup>1,2</sup>, Chloe Jackson<sup>3</sup>, Georgia Mavrommati<sup>3</sup>, Rich B. Howarth<sup>4</sup>, Ryan S.D. Calder<sup>1,2,5,6,7,*</sup>
</p>

<h1 id="citation" tabindex="-1">Citation</h1>
<p>
  If you use or build on this work, please cite:<br/>
  Gazar, A. M., Jackson, C., Mavrommati, G., Howarth, R. B., & Calder, R. S. D. (2025).<br/>
  <em>Cost uncertainties and ecological impacts drive tradeoffs between electrical system decarbonization pathways in New England, U.S.A.</em> [Journal/Preprint].  
</p>

<h1 id="license" tabindex="-1">License</h1>
<p>
  © 2025 Amir M. Gazar et al. All rights reserved.  
  Licensed under a <a href="https://creativecommons.org/licenses/by/4.0/">Creative Commons Attribution 4.0 International License</a>.
</p>
