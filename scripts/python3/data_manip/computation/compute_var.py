"""
Fonction to computation Physical values from a TelemacFile
"""

import numpy as np

def calc_water_depth(res, record=-1):
    """
    Compute water depth from free surface and bottom

    @param res (TelemacFile)
    @param record (int) Record to compute for
    """

    varname = "FREE SURFACE" if "FREE SURFACE" in res.varnames \
                                else "SURFACE LIBRE"
    free_surface = res.get_data_value(varname, record=record)

    varname = "BOTTOM" if "BOTTOM" in res.varnames else "FOND"
    bottom = res.get_data_value(varname, record=record)

    water_depth = free_surface - bottom

    return water_depth

def calc_kinetic_energy(res, record=-1):
    """
    Compute kinetic energy from velocity

    @param res (TelemacFile)
    @param record (int) Record to compute for
    """

    names = ["U", "V", "W"]

    varname = "VELOCITY " if "VELOCITY U" in res.varnames else "VITESSE "

    vel = None

    # Sim of velocities
    for comp in names[0:res.ndim]:
        val = res.get_data_value(varname+comp, record=record)
        if vel is None:
            vel = val
        else:
            vel += val

    result = np.power(vel, 3.0/2.0)

    return result
