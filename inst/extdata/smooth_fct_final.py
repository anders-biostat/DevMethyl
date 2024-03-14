import numpy as np
import numba
import math

@numba.njit
def norm_pdf( x ):
    return np.exp( -x**2/2 ) / np.sqrt(2*np.pi)

@numba.njit
def number_of_grids(x, y):
    z = (x / y)-1 
    # Check if integer part of z is even
    if int(z) % 2 == 0:
        # Round down to nearest even number
        result = math.floor(z)
    else:
        # Round up to nearest even number
        result = math.ceil(z) 
    return int(result/2)

@numba.njit
def gaussian_smoothing(x, t, v, xmin, xmax, delta_x, tmin, tmax, delta_t, hx, ht):
    numerator = np.zeros( ( int((xmax-xmin)//delta_x), int((tmax-tmin)//delta_t) ) )
    denominator = np.zeros( ( int((xmax-xmin)//delta_x), int((tmax-tmin)//delta_t )) )
    m = np.zeros( ( int((xmax-xmin)//delta_x), int((tmax-tmin)//delta_t )) )
    
    last_grid_x = int(np.floor((xmax-xmin) / delta_x))  # here 2786
    last_grid_t = int(np.floor((tmax-tmin) / delta_t))  # used floor beacuse first grid is 0
    
    num_g_x = number_of_grids((10*hx), delta_x)  # here: 49 same for t
    num_g_t = number_of_grids((10*ht), delta_t)
    
    for i in range(0,len(x)):
        weight = 0.0
        xg = int(np.floor( ( x[i] - xmin ) / delta_x ))
        tg = int(np.floor( ( t[i] - tmin ) / delta_t ))

        left_grid_x = max(0, xg-num_g_x)  # so if xg = 0 there wont be negative grids
        right_grid_x = min(last_grid_x, xg+ num_g_x)
        left_grid_t = max(0, tg-num_g_t)
        right_grid_t = min(last_grid_t, tg+num_g_t)

        for ix in range( left_grid_x , right_grid_x ):   
            for it in range( left_grid_t , right_grid_t ):
                weight = norm_pdf((xmin + ix*delta_x - x[i]) / hx) * norm_pdf((tmin + it*delta_t - t[i]) / ht)
                numerator[ix, it] += v[i] * weight
                denominator[ix,it] += weight

    m = numerator/ (denominator+ 1e-100)
    
    return m 


def gaussian_smoothing_outer(df, hx, ht):
    return gaussian_smoothing(df["i"].values, df["j_ptimes"].values, df["data"].values, min(df["i"].values), max(df["i"].values) , 20, min(df["j_ptimes"].values), max(df["j_ptimes"].values), 0.04, hx,ht)

   
