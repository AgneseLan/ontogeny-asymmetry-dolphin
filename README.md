# The ontogeny of asymmetry in echolocating whales
Repository fo R code and data for the project "The ontogeny of asymmetry in echolocating whales" :dolphin: 🔊

Authors: [Agnese Lanzetti](mailto:agnese.lanzetti@gmail.com?subject=[GitHub]%20Ontogeny%20Asymmetry%20Paper%20Code), - Anjali Goswami

To cite the paper: 


Available at: https://github.com/AgneseLan/ontogeny-asymmetry-dolphin

If using any of this code or data please cite the paper above and this repo

To cite this repo:

DOI ZENODO

## Data :floppy_disk: 

The data are provided in the Data folder. Mesh files (PLY) needed to test postioning when importing landmarks are available at Phenome10k (https://www.phenome10k.org/). 
Zipped data need to be extracted before use.

- __Landmark data__: *pts folder* <br />
Text files with landmark coordinates for each specimen in PTS format. 

- __Surface data__: *ply folder* <br />
Empty folder where mesh files from Phenome10k need to be saved to reproduce the code.

- __Specimens' classifiers, landmark/curves lists__: *absent_curves.csv, absent_LMs.csv, curves.csv, landpairs_LM.csv, LMs.csv, specimens.csv* <br />
Spreadsheets with additional inforation for analyses: list of absent landmarks and curves, list of curves, list of landmark pairs, list of landmakrs, classifiers for specimens (ID, genera, family, growth stage, Age, length, bizygomatic width).

- __Phylogentic tree__: *tree_odontonly.txt* <br />
Tree with branch lenghts for the taxa used in the analyses in Nexus format. Branch lenghts and topology from McGowen et al., 2020, Syst. Biol. 69, 479-501.

- __Reference meshes for plotting__: *ply_refmesh.zip* <br />
Zipped folder with reduced meshes in PLY format used as reference for plotting landmarks and asyemmtry variation.

- __Silhouettes of taxa for plots__: *delphinapterus.png, globicephala.png, lagenorhynchus.png, phocoena.png, stenella.png*

## Analysis :computer:
In this repository you will find raw data (.csv and data files) and code for analyses (code supplied as .R files)

📁 Data

As described above. Meshes used to collect and test landmarks available on Phenome10k (https://www.phenome10k.org/). 

⌨ Code for analyses - .R files

*1-Import-resample-slide.R, 1-Slider3d_2.R, 2-Absent_bones.R, 3-GPA_PCA.R, 4-Allometry.R, 5-Trajectory.R, 6-Asymmetry.R, 7-Ancestral_allometry.R*

Code files are numbered providing the order the analyses need to be performed in.
Before running analyses, save Data folder in the same directory as the R project. This will allow to import the data as detailed in the code provided.

## License 📃
This project is licensed under the MIT License - see the LICENSE.md file for details

## Session Info 📋
For reproducibility purposes, here is the output of devtools::session_info() used to perform the analyses in the publication.

```
R	version	4.1.2	(2021-11-01)					
Platform:	x86_64-w64-mingw32/x64	(64-bit)						
Running	under:	Windows	10	x64	(build	19044)		
								
Matrix	products:	default						
								
locale:								
[1]	LC_COLLATE=English_United States.1252	LC_CTYPE=English_United	States.1252		
[3]	LC_MONETARY=English_United States.1252	LC_NUMERIC=C	
[5]	LC_TIME=English_United States.1252					
								
attached	base	packages:						
[1]	grid	stats	graphics	grDevices	utils	datasets	methods	base
								
other	attached	packages:						
[1]	landvR_0.5.2	car_3.0-12	carData_3.0-5	emmeans_1.7.2	mcp_0.3.1			
[6]	reshape2_1.4.4	evomap_0.0.0.9000	phytools_1.0-1	maps_3.4.0	gridExtra_2.3			
[11]	ggplotify_0.1.0	ggpubr_0.4.0	ggthemes_4.2.4	borealis_2021.02.04	RColorBrewer_1.1-2			
[16]	gginnards_0.1.0-1	ggrepel_0.9.1	magick_2.7.3	SURGE_0.1.0	devtools_2.4.3			
[21]	geiger_2.0.7	ape_5.6-1	qgraph_1.9	EMMLi_0.0.3	paleomorph_0.1.4			
[26]	Matrix_1.4-0	rgl_0.108.3	RRPP_1.1.2	Morpho_2.9	forcats_0.5.1			
[31]	purrr_0.3.4	readr_2.1.1	tidyr_1.1.4	tibble_3.1.6	ggplot2_3.3.5			
[36]	rray_0.1.0.9000	scales_1.1.1	png_0.1-7	rphylopic_0.3.0	ggfortify_0.4.14			
[41]	ggphylomorpho_0.1.0	usethis_2.1.5	abind_1.4-5	Rvcg_0.20.2	geomorph_4.0.1			
[46]	stringr_1.4.0	dplyr_1.0.7	tidyverse_1.3.1					
								
loaded	via	a	namespace	(and	not	attached):		
[1]	clipr_0.7.1	utf8_1.2.2	tidyselect_1.1.1	htmlwidgets_1.5.4				
[5]	codetools_0.2-18	withr_2.4.3	colorspace_2.0-2	knitr_1.37				
[9]	stats4_4.1.2	ggsignif_0.6.3	mnormt_2.0.2	rprojroot_2.0.2				
[13]	generics_0.1.1	clusterGeneration_1.3.7	xfun_0.29	R6_2.5.1				
[17]	gridGraphics_0.5-1	assertthat_0.2.1	nnet_7.3-17	gtable_0.3.0				
[21]	rlang_0.4.12	scatterplot3d_0.3-41	splines_4.1.2	rstatix_0.7.0				
[25]	modelr_0.1.8	backports_1.4.1	Hmisc_4.6-0	tools_4.1.2				
[29]	gridBase_0.4-7	ellipsis_0.3.2	phyclust_0.1-30	sessioninfo_1.2.2				
[33]	base64enc_0.1-3	ps_1.6.0	prettyunits_1.1.1	rpart_4.1-15				
[37]	deSolve_1.30	haven_2.4.3	cluster_2.1.2	colorRamps_2.3				
[41]	magrittr_2.0.1	RSpectra_0.16-0	data.table_1.14.2	strap_1.4				
[45]	mvtnorm_1.1-3	castor_1.7.2	matrixStats_0.61.0	pkgload_1.2.4				
[49]	patchwork_1.1.1	xtable_1.8-4	jpeg_0.1-9	readxl_1.3.1				
[53]	crayon_1.4.2	htmltools_0.5.2	mgcv_1.8-38	corpcor_1.6.10				
[57]	expm_0.999-6	lubridate_1.8.0	DBI_1.1.2	magic_1.5-9				
[61]	MASS_7.3-55	ade4_1.7-18	permute_0.9-5	brio_1.1.3				
[65]	quadprog_1.5-8	parallel_4.1.2	igraph_1.2.11	pkgconfig_2.0.3				
[69]	xml2_1.3.3	foreach_1.5.1	pbivnorm_0.6.0	Claddis_0.6.3				
[73]	yulab.utils_0.0.4	bezier_1.1.2	callr_3.7.0	digest_0.6.29				
[77]	cellranger_1.1.0	fastmatch_1.1-3	htmlTable_2.4.0	curl_4.3.2				
[81]	nlme_3.1-155	glasso_1.11	jsonlite_1.7.3	desc_1.4.0				
[85]	geoscale_2.0	lattice_0.20-45	loo_2.4.1	fastmap_1.1.0				
[89]	pkgbuild_1.3.1	survival_3.2-13	glue_1.6.1	remotes_2.4.2				
[93]	stringi_1.7.6	latticeExtra_0.6-29	memoise_2.0.1	iterators_1.0.13				
[97]	combinat_0.0-8	munsell_0.5.0	geometry_0.4.5	rstudioapi_0.13				
[101]	tzdb_0.2.0	Formula_1.2-4	coda_0.19-4	vctrs_0.3.8				
[105]	dbplyr_2.1.1	subplex_1.7	doParallel_1.0.16	cachem_1.0.6				
[109]	cli_3.1.1	naturalsort_0.1.3	processx_3.5.2	phangorn_2.8.1				
[113]	numDeriv_2016.8-1.1	foreign_0.8-82	broom_0.7.11	checkmate_2.0.0				
[117]	estimability_1.3	rvest_1.0.2	psych_2.1.9	lavaan_0.6-9				
[121]	vegan_2.5-7	httpcode_0.3.0	Rcpp_1.0.8	plyr_1.8.6				
[125]	gtools_3.9.2	lifecycle_1.0.1	pbapply_1.5-0	zoo_1.8-9				
[129]	fansi_1.0.2	pillar_1.6.4	fs_1.5.2	crul_1.2.0				
[133]	httr_1.4.2	plotrix_3.8-2	reprex_2.0.1	tmvnsim_1.0-2				
[137]	fdrtool_1.2.17	testthat_3.1.2	dispRity_1.6.0	hms_1.1.1				
[141]	compiler_4.1.2							
```
