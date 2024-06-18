import numpy as np
import numba
import math

@numba.njit
def norm_pdf( x ):
    return np.exp( -x**2/2 ) / np.sqrt(2*np.pi)

@numba.njit
def number_of_grids(x, y):
    z = (x / y)-1 
    # check if integer of z is even
    if int(z) % 2 == 0:
      # Round down to nearest even number
        result = math.floor(z)
    else:
      # Round up to nearest even number
        result = math.ceil(z) 
    return int(result/2)

@numba.njit
def gaussian_smoothing(x, t, v, xmin, xmax, delta_x, tmin, tmax, delta_t, hx, ht):
    
    # create numpy arrays of zeros, with dimension repesenting the number of grids
    numerator = np.zeros( ( int((xmax-xmin)//delta_x), int((tmax-tmin)//delta_t) ) )
    denominator = np.zeros( ( int((xmax-xmin)//delta_x), int((tmax-tmin)//delta_t )) )
    m = np.zeros( ( int((xmax-xmin)//delta_x), int((tmax-tmin)//delta_t )) )
    
    # define grid limits
    last_grid_x = int(np.floor((xmax-xmin) / delta_x))  # used floor because first grid is 0 # but what if its a natural number?
    last_grid_t = int(np.floor((tmax-tmin) / delta_t))  
    
    # define number of grids in window
    num_g_x = number_of_grids((10*hx), delta_x)  
    num_g_t = number_of_grids((10*ht), delta_t)
    
    # for each datapoint i m, containing the smoothed values over grids, is updated 
    for i in range(0,len(x)):
      # value starts with zero
        weight = 0.0
        
        # calculate grid index for each data point
        xg = int(np.floor( ( x[i] - xmin ) / delta_x ))
        tg = int(np.floor( ( t[i] - tmin ) / delta_t ))
        
        # define the min and max grid within the window of the current data point
        left_grid_x = max(0, xg-num_g_x)  # max() ensures no negative grids indices
        right_grid_x = min(last_grid_x, xg+ num_g_x) # min() ensures no grids indeces exceed grid limits
        down_grid_t = max(0, tg-num_g_t)
        up_grid_t = min(last_grid_t, tg+num_g_t)

        # for each grid within the window, the datapoint is weighted and the weighted values are added to numerator and denominator¯
        for ix in range( left_grid_x , right_grid_x ):   
            for it in range( down_grid_t , up_grid_t ):
                
                # calculate weight for the datapoint using gaussian normalization
                weight = norm_pdf((xmin + ix*delta_x - x[i]) / hx) * norm_pdf((tmin + it*delta_t - t[i]) / ht)
                
                # update numerator and denominator for specific grid of 
                numerator[ix, it] += v[i] * weight
                denominator[ix,it] += weight

    m = numerator/ (denominator+ 1e-100)
    
    return m 


   
