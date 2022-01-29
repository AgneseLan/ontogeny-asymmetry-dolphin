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
Tree with branch lenghts for the taxa used in the analyses in Nexus format. Branch lenghts and topology from McGowen et al. 2020

- __Surface data__: *ply folder* <br />
Empty folder where mesh files from Phenome10k need to be saved to reproduce the code.

- __Silhouettes of taxa and groups for plots__: *b.bona.png, b.physalus.png, phocoena.png, stenella.png*

## Analysis :computer:
In this repository you will find raw data (.csv and data files) and code for analyses (code supplied as .R files)

📁 Data

As described above. Raw shape data files and GPSA results available on Dryad (https://datadryad.org/stash/share/P6YMy3QspoUdNIIyy5A9DV5ojdryerS1Bb1GlRO4uwE - https://doi.org/10.6086/D1R69C).

📁 Code for analyses

*ossification_size_allometry_analyses.R, tympanic_bulla_shape_analyses.R, periotic_shape_analyses.R*

Before running analyses, save Data folder in the same directory as the R project. This will allow to import the data as detailed in the code provided.

## License 📃
This project is licensed under the MIT License - see the LICENSE.md file for details

## Session Info 📋
For reproducibility purposes, here is the output of devtools::session_info() used to perform the analyses in the publication.
