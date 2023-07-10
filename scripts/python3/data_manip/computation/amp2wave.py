"""
Fonction to computation Wave file from Amplitude and phase file
"""
from os import path, remove
import numpy as np
from data_manip.extraction.telemac_file import TelemacFile
from utils.progressbar import ProgressBar

def amp2wave(amp_file, wave_file,
             start_time=2006.07, time_step=0.34, end_time=2108.75,
             force=False):
    """
    Compute a file containe WAVE SURFACE computed from the amplitude and phase
    file (artemis animation)

    @param amp_file (string) Amplitude ans pahse file
    @param wave_file (string) Wave file
    @param start_time (float) Time from which to compute WAVE SURFACE
    @param time_step (float) Time step for which to compute WAVE SURFACE
    @param end_time (float) End Time for which to compute WAVE SURFACE
    @param force (bool) If True will overwrite wave file if it already exists
    """
    # ~~> Dependencies
    #     needs all the components to recreate the free surface signal

    amp = TelemacFile(amp_file)

    if force:
        if path.exists(wave_file):
            remove(wave_file)

    wave = TelemacFile(wave_file, access="w")

    # Copying mesh data + adding one variable
    wave.read_mesh(amp)

    wave.add_variable("WAVE SURFACE", "M")

    wave.write()

    wave.close()

    # Reopening file in rw to write results directly (memory efficient)
    wave = TelemacFile(wave_file, access="rw")

    ndir = amp.nvar // 2

    sqrt2 = np.sqrt(2.0)

    times = np.arange(start_time, end_time, time_step)

    wave_surface = np.zeros((amp.npoin3), dtype=np.float64)

    wave._times = times
    wave._ntimestep = len(times)
    # Reading wave height and phase
    # prereading all information to optimize
    wave_height = np.zeros((ndir, amp.ntimestep, amp.npoin3), dtype=np.float64)
    wave_phase = np.zeros((ndir, amp.ntimestep, amp.npoin3), dtype=np.float64)
    for idir in range(ndir):
        for itime2, time2 in enumerate(amp.times):
            wave_height_name = "WAVE HEIGHT D{0:02d}".format(idir+1)
            wave_phase_name = "WAVE PHASE D{0:02d}".format(idir+1)
            wave_height[idir, itime2] =\
                amp.get_data_value(wave_height_name, itime2)
            wave_phase[idir, itime2] =\
                amp.get_data_value(wave_phase_name, itime2)

    print('\n      > Input signal based on:')
    print('      - {} direction(s)'.format(ndir))
    print('      - for the following periods: {}'.format(amp.times))

    print('\n      > Going through time ({} time steps) :'\
          .format(wave._ntimestep))
    pbar = ProgressBar(maxval=wave._ntimestep).start()

    # Loop on result time
    for itime, time in enumerate(times):
        wave_surface = 0.0

        # Computing surface wave
        for idir in range(ndir):
            for itime2, time2 in enumerate(amp.times):
                dfr = np.float64(0.04/time2)*\
                        np.float64(idir+1-(ndir+1)/2)

                coeff = -2.0*np.pi*np.float64(1.0/time2+dfr)*np.float64(time)
                wave_surface += wave_height[idir, itime2]*\
                         np.cos(coeff+wave_phase[idir, itime2])/(2.0*sqrt2)

        wave.set_data_value("WAVE SURFACE", itime, wave_surface)
        pbar.update(itime)

    pbar.finish()

    amp.close()
    wave.close()
