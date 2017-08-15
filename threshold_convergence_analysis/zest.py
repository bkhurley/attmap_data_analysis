 """
 zest.py
 
 perform calculations for the ZEST algorithm based on (a) specified params and (b) incoming responses (0 = miss, 1 = hit)

params is a dict and should contain the following keys:
	A
	B
	C
	maxrange
 	minrange
	fa
	miss
	beta
	eta
	diffLvl

Parameter names correspond to Equations 2, 3, and 4 in the following paper:

Marvit P., Florentine, M., & Buus Soren. (2003) A comparison of psychophysical procedures for level-discrimination thresholds. Journal of the Acoustical Society of America. 113 (6), 3348-3361.
"""

import numpy as np

def initialize_zest(params):
	"""
	populate parameters and initialize probability density function to start with our "best-guess" of threshold. 
	"""

	# starting thresh estimate
	init_t = np.log10(params['diffLvl'])

	# calculate initial p.d.f. and midpoint
	q = params['A']/(params['B']*np.exp(-params['C']*(params['T']-init_t)) + params['C']*np.exp(params['B']*(params['T']-init_t)))
	meanpdf = init_t

	# calculate standard deviation
	v = np.sum(np.multiply(np.power((params['T']-meanpdf),2),q))/np.sum(q)
	s = np.sqrt(v)
	
	# convert critical variables back to dB
	meanpdf_dB = np.power(10,meanpdf)
	s_dB = np.power(10,s)

	# output data
	init_out = {
		'meanpdf': meanpdf,
		'meanpdf_dB': meanpdf_dB,
		'stand_dev': s, 
		'stand_dev_dB': s_dB, 
		'pdf': q
		}

	return init_out


def converge_zest(params,prev_q,prev_meanpdf,response):

	""" 
	after initialization, calculate the next thresh estimate. 
	meanpdf is threshold estimate for previous trial.
	psychometric function (Weibull distribution) is prob of response given log_lev_diff if true log(difference limen) is T.
	"""

	# calculate weibull distribution of response probability
	p = 1-params['miss']-((1-params['fa']-params['miss'])*
		np.exp(-np.power(10,(params['beta']*(prev_meanpdf-params['T']+params['eta'])))))

	if response==0:
		p=1-p

	# compute the next p.d.f.
	next_q = np.multiply(p,prev_q)
	# calculate new midpoint
	new_meanpdf = np.sum(np.multiply(params['T'],next_q))/np.sum(next_q)

	# calculate variance and std. dev. of new p.d.f.
	v = np.sum(np.multiply(np.power((params['T']-new_meanpdf),2),next_q))/np.sum(next_q)
	s = np.sqrt(v)

	# convert critical variables back to dB
	meanpdf_dB = np.power(10,new_meanpdf)
	s_dB = np.power(10,s)

	out_data = {
		'meanpdf': new_meanpdf,
		'meanpdf_dB': meanpdf_dB,
		'stand_dev': s, 
		'stand_dev_dB': s_dB, 
		'pdf': next_q
		}

	return out_data
