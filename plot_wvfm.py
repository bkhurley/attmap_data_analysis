# plots time domain waveform of aoudio file
#
# REQUIRES:
# 	* sound_fname - file name (wihout path) of sound file to plot
# 	* params - dict populated with the following keys:
#		* 'save_fig' - 0 or 1
#		* 'fig_fpath' - character string of file path to save
#		* 'sound_fpath' - file path where to find sound file
#		*'remove_axis' - 0 or 1
#
# 18 Nov 2016 - BH

from scipy.io.wavfile import read
import numpy as np
import matplotlib.pyplot as plt

def waveplot(sound_fname, params):
	sound_fname_path = '%s%s' % (params['sound_fpath'], sound_fname)
	wvf_in = read(sound_fname_path)
	fs = wvf_in[0]
	fs_ms_ratio = fs/1000

	wvf = np.array(wvf_in[1], dtype=float)
	
	# convert samples to ms
	wvf_samp_vect = range(0,len(wvf),1)
	wvf_ms_vect = np.divide(wvf_samp_vect,fs_ms_ratio)
	samp_ms = np.size(wvf)/fs_ms_ratio
	
	plt.plot(wvf_ms_vect, wvf, color='k')
	
	plt.xlim(params['xlim'][0], params['xlim'][1])

	# remove axes so we only waveform line
	if params['remove_axis']:
		plt.axis('off')	

	# save figure to file
	if params['savefig']:
		plt.savefig('%s%s.eps' % (params['fig_fpath'], sound_fname))

	plt.show()
	plt.close()