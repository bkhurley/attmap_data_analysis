# wrapper script for plotting attmap_altstim stimulus waveforms
#
# 19 Nov 2016 - BKH

from plot_wvfm import waveplot

# define waveplot params
params = {
	'xlim': [0, 140],
	'savefig': 1,
	'fig_fpath': '/data/attmap/attmap_altstim/figs/local_figs/',
	'sound_fpath': '/data/attmap/attmap_altstim/sample_wav/',
	'remove_axis': 1
}

stim_names = ['vdc_conga1_1.wav', 'vdc_shaker_1.wav', 'vdc_snap_1.wav']

# loop through stims, plotting each one
for i in range(len(stim_names)):
	waveplot(stim_names[i], params)
