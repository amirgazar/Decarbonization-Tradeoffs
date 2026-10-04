# Cost uncertainties and ecological impacts drive tradeoffs between electrical system decarbonization pathways in New England, U.S.A.

<p>
  Amir M. Gazar<sup>1,2</sup>, Chloe Jackson<sup>3</sup>, Georgia Mavrommati<sup>3</sup>, Rich B. Howarth<sup>4</sup>, Ryan S.D. Calder<sup>1,2,5,*</sup>
</p>
<p>
  <sup>1</sup>Dept. of Population Health Sciences, Virginia Tech, Blacksburg, VA, 24061, USA<br/>
  <sup>2</sup>Global Change Center, Virginia Tech, Blacksburg, VA, 24061, USA<br/>
  <sup>3</sup>School for the Environment, University of Massachusetts Boston, Boston, MA, 02125, USA<br/>
  <sup>4</sup>Environmental Program, Dartmouth College, Hanover, NH, 03755, USA<br/>
  <sup>5</sup>Dept. of Civil & Environmental Engineering, Virginia Tech, Blacksburg, VA, 24061, USA<br/>
  <strong>* Contact:</strong> rsdc@vt.edu
</p>

## Run the analysis

This repository contains the preparation scripts, dispatch model, cost calculations and figure code for the study. Large generated datasets are supplied or generated separately.

Start with [REPRODUCING.md](REPRODUCING.md). It lists the run order, required inputs, settings and checks. The numbered folders follow the analysis stages:

1. `0 Stochastic Power Plant Model`: historical power-plant data and simulation inputs.
2. `1 Decarbonization Pathways`: hourly installed-capacity schedules.
3. `2 Generation Expansion Model`: demand, generation, imports, shared random draws and dispatch.
4. `3 Total Costs`: the 13 cost components and shared coefficient sampling.
5. `4 External Data`: external input tables and source material.
6. `5 Ecological impacts`: ecological calculations.
7. `6 Figures`: figure scripts and notebooks.
8. `7 Reproduction Information Document`: the cost runner, validation and publication output runner.

The dispatch model and cost components use their original filenames. Git history records the changes between submissions. Names containing `R1` in supporting scripts, environment settings and output formats are retained for compatibility.

See [LICENSE.txt](LICENSE.txt), [AUTHORS.txt](AUTHORS.txt) and [citation.bib](citation.bib). The original reproduction PDF describes the first submission; use the run guide here for the current entry points.
