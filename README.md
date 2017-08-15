# Attmap Data Analysis
Data analysis code from a portion of my dissertation. **Attmap** is short for *attention-mapping*.

The data were from three psychophysics experiments that measured listeners' intensity deviance detection thresholds for various time points in musical sequences. The goal was to psychophysically "map" listeners' temporal attention using a Bayesian threshold algorithm called [*Zippy Estimation by Sequential Testing (ZEST)*](https://bkhurley.github.io/assets/King-Smith_etal_1994_VisionRes.pdf) and compare this map to predictions of a computational model of time-varying attention. [The model](https://bkhurley.github.io/assets/Tomic&Janata_JASA_2008.pdf) uses banks of damped linear oscillators (reson filters) to profile an acoustic signal's rhythmic structure. I generated metrics from this model to predict moment-to-moment attention fluctuations while participants' listened to auditory rhythms, and I tested those predictions with the deviance detection expriments described above.

As a whole, the code in this repo cleans, statistically models (primarily via mixed-effects models), and plots data from the three experiments.

## Requirements
Required packages for R code:
- dplyr
- ggplot2
- grid
- gridExtra
- lattice
- nlme

Required packages for Python code:
- matplotlib
- NumPy
- pandas
- SciPy

## Usage
Data analysis code are organized into folders for 3 experiments. The following wrapper scripts are entry points for the analyses of each experiment's data. Parameters in these scripts control which data sets are analyzed, which functions are called, and the behavior of those functions.
- `experiment_1/attmap_v1p2_analyses.R`
- `experiment_2/attmap_intensitydec_analyses.R`
- `experiment_3/attmap_altstim_analyses.R` 

The repo also contains python code for plotting waveforms of the experiments' audio stimuli, and python code in the `threshold_convergnce_analysis` folder analyzes individual subjects' threshold convergence trajectories.
