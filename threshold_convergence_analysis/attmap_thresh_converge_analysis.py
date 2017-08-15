"""
Use subject data from attmap_v1p2 experiment to calculate standard deviation of threshold probability density functions (p.d.f.) for each trial within each subject. 
Function output will is a table containing threshold and SD values for each trial within each subject.
Assessing the p.d.f spread associated with threshold convergence will allow us to determine an ideal p.d.f standard deviation in which we can consider a threshold to be converged (i.e. a dynamic termination criterion rather than an arbitrary number-of-trials criterion) 
"""

import pandas as pd
import numpy as np
from zest import initialize_zest, converge_zest


# name of experiment (must match file naming)
exp_name = 'attmap_intensitydec'
data_in_fpath = '/data/attmap/%s/tables/%s_trial_data.csv' % (
	exp_name, exp_name)
data_out_fpath = '/data/attmap/attmap_convergence_analysis/tables/%s_data_sd.csv' % (exp_name)
# import subject data
trial_data = pd.read_csv(data_in_fpath)

# initialize pdf_sd array that will be appended to data table
n_data_rows = len(trial_data)
pdf_sd = np.empty(n_data_rows) * np.nan

subjects = pd.unique(trial_data['subject_id'])
nsubs = len(subjects)
stims = pd.unique(trial_data['stim_name'])
nstims = len(stims)

# zest parameters
zest_params = {
	# params for initial p.d.f.
	'A': 1.0,
	'B': 2.5,
	'C': 2.5,
	'minrange': np.log10(0.01), # 0.01 dB
	'maxrange': np.log10(20.0), # 20 dB
	'diffLvl': 10.0, # dB units

	# params for psychometric (weibull) response function
	'fa': 0.1, 		# false alarm rate; gamma in Marvit et al
	'miss': 0.02, 	# miss rate; delta in Marvit et al
	'beta': 10.0, 		# slope of response function
	'eta': 0.0, 		#'sweat factor' or response criterion
}

# create a discrete vector of the extent/range of possible difference limens
T = np.linspace(zest_params['minrange'],zest_params['maxrange'],2000)
# add to zest_params
zest_params['T'] = T

# loop through subjects
for isub in range(0,nsubs):
	
	# index trials for this subject
	sub_idx = trial_data.subject_id==subjects[isub]	
	
	# loop through each stim
	for istim in range(0,nstims):
		# index trials with this subject & stim
		stim_idx = trial_data.stim_name==stims[istim]
		sub_stim_idx = np.logical_and(sub_idx,stim_idx)

		# identify probes for this stim
		probes = np.unique(trial_data.probe_time[sub_stim_idx])
		nprobes = len(np.unique(probes))
		
		# loop through probes
		for iprobe in range(0,nprobes):
			
			# index trials with this subj, stim, & probe
			probe_idx = trial_data.probe_time == probes[iprobe]
			stim_probe_idx = np.logical_and(stim_idx, probe_idx)
			sub_stim_probe_idx = np.logical_and(sub_stim_idx, probe_idx)

			# get data for plotting
			# probe X obs table of pdf_sd means, indexed by stim


			# initialize parameters and establish starting threshold p.d.f.
			this_sub_init_zest = initialize_zest(zest_params)
			
			# loop through each observation
			n_obs = len(trial_data.obs_num[sub_stim_probe_idx])
			observations = trial_data.obs_num[sub_stim_probe_idx]
			for iobs in range(1,n_obs+1):
				if iobs > 20:
					continue
				obs_idx = trial_data.obs_num == iobs
				stim_probe_obs_idx = np.logical_and(stim_probe_idx,obs_idx)
				sub_stim_probe_obs_idx = np.logical_and(sub_stim_probe_idx,obs_idx)
				response = trial_data.sub_resp.loc[sub_stim_probe_obs_idx]
				# at least one subject had duplicate observation number.
				# in such cases, take the first of the duplicates.
				if len(response) > 1:
					response = response.iloc[0]
				response = int(response)

				# if first observation, use pdf from initialization
				if iobs==1:
					pdf = this_sub_init_zest['pdf']
					meanpdf = this_sub_init_zest['meanpdf']
				else:
					pdf = this_sub_trial_zest['pdf']
					meanpdf = this_sub_trial_zest['meanpdf']
				
				this_sub_trial_zest = converge_zest(zest_params,pdf,meanpdf,response)

				# assign std dev to pdf_sd array
				pdf_sd[np.where(sub_stim_probe_obs_idx)] = this_sub_trial_zest['stand_dev_dB']


# append std dev data to main data frame
trial_data['pdf_sd'] = pdf_sd

# sd data grouped by stim, probe, and observation
sd_data = trial_data['pdf_sd'].groupby([trial_data['stim_name'], trial_data['probe_time'], trial_data['obs_num']])


# print to file
trial_data.to_csv('/data/attmap/attmap_convergence_analysis/tables/attmap_v1p2_data_sd.csv')