#!/usr/bin/env python3
"""@author TELEMAC-MASCARET Consortium

   @brief Check if discretization parameters are able to represent the chosen spectrum.
          It uses a JONSWAP spectrum with a chosen peak frequency.
          This spectrum is typically the imposed spectrum at the boundary of TOMAWAC.
"""
import argparse
import os
import numpy as np
from execution.telemac_cas import TelemacCas

def create_frequencies_discretization(min_frequency, ratio, num_frequencies):
    """
    Create a set of frequencies as a geometric progression of ratio with minimal
    frequency min_frequency.
    @param min_frequency Minimal frequency.
    @param ratio Ratio of the geometric progression.
    @param num_frequencies Number of frequencies.
    """
    frequencies = np.array([min_frequency * ratio**i for i in range(num_frequencies)])
    return frequencies

def jonswap(frequencies, frequency_peak, gamma, alpha=0.0081):
    """
    Calculate the JONSWAP spectrum for a set of frequencies.
    @param frequencies The table of frequencies.
    @param frequency_peak The peak frequency.
    @param gamma A parameter of the JONSWAP function.
    @param alpha The Phillips constant.
    """
    gravity = 9.81
    deupi = 2*np.pi
    coefficient = alpha*gravity**2/(deupi**4)
    spectrum = np.zeros(len(frequencies))
    arg1 = np.zeros(len(frequencies))
    mask = frequencies < frequency_peak
    # arg1 arg2 and arg3 are temporary results.
    arg1[mask] = gamma*np.exp(-0.5*((frequencies[mask]-frequency_peak)/(0.07*frequency_peak))**2)
    mask = frequencies >= frequency_peak
    arg1[mask] = gamma*np.exp(-0.5*((frequencies[mask]-frequency_peak)/(0.09*frequency_peak))**2)
    arg2 = np.exp(-1.25*(frequency_peak/frequencies)**4)
    arg3 = coefficient/(frequencies**5)
    spectrum = arg1*arg2*arg3
    return spectrum

def plot(frequencies, spectrum, file_output=None):
    """
    Plot the spectrum and export the output to a PNG file.
    @param frequencies Abscissae.
    @param spectrum The spectrum to plot.
    @param file_output The name of the exported PNG file.
    """
    import matplotlib.pyplot as plt
    __, axe = plt.subplots(figsize=(12, 10))
    axe.plot(frequencies, spectrum)
    axe.set_xlabel("frequencies")
    axe.set_ylabel("spectrum")
    if file_output is not None:
        plt.savefig(file_output)
        plt.close(1)
        print('Exporting the output to ', file_output)
    else:
        plt.show()
    del plt

def main():
    """
    Plot the spectrum defined in a TOMAWAC cas file.
    """
    parser = argparse.ArgumentParser(description='Check spectral discretization')

    parser.add_argument("cas_file",
                        default=None,
                        help="The TOMAWAC cas file")

    parser.add_argument("--png",
                        dest='png_file',
                        default=None,
                        help="Export the plot output to a PNG file.")

    args = parser.parse_args()
    png_file = args.png_file
    cas_file = args.cas_file
    hometel = os.getenv("HOMETEL")
    dico = hometel+'/sources/tomawac/tomawac.dico'
    cas = TelemacCas(cas_file, dico)
    min_frequency = cas.get("MINIMAL FREQUENCY")
    ratio = cas.get("FREQUENTIAL RATIO")
    num_frequencies = cas.get("NUMBER OF FREQUENCIES")
    frequencies = create_frequencies_discretization(min_frequency, ratio, num_frequencies)

    peak_frequency = cas.get("BOUNDARY PEAK FREQUENCY")
    peak_factor = cas.get("BOUNDARY PEAK FACTOR")
    alpha = cas.get("BOUNDARY PHILLIPS CONSTANT")
    spectrum = jonswap(frequencies, peak_frequency, peak_factor, alpha=alpha)

    plot(frequencies, spectrum, file_output=png_file)

if __name__ == "__main__":
    main()
