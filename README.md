Data analysis code from a portion of my dissertation. 

The data were from psychophysics experiments that measured listeners' intensity deviance detection thresholds for various time points in musical sequences. The goal was to psychophysically "map" listeners' temporal attention and compare this map to predictions of a computational model of time-varying attention.

attmap_altstim_analyses.R is a wrapper script and is the entry point to the code. Parameters in this script control which data set is analyzed, which functions are called, and the behavior of those functions. 

As a whole, the code cleans, statistically models (primarily via mixed-effects models), and plots the data.
