
"""
Validation script for flume_frazil
"""
from vvytel.vnv_study import AbstractVnvStudy
from execution.telemac_cas import TelemacCas, get_dico
from data_manip.extraction.telemac_file import TelemacFile
from data_manip.computation.datetimes import compute_datetimes
from matplotlib import rc

class VnvStudy(AbstractVnvStudy):
    """
    Class for validation
    """

    def _init(self):
        """
        Defines the general parameter
        """
        self.rank = 5
        self.tags = ['telemac2d', 'khione']

    def _pre(self):
        """
        Defining the studies
        """
        # Carstens 1 monoclass - parallel
        cas = TelemacCas('t2d_racetrack_carstens.cas', get_dico('telemac2d'))
        cas.set('PARALLEL PROCESSORS', 4)
        cas.set('DURATION', 600.)
        cas.set('INITIAL VALUES OF TRACERS', [0., 0.])
        cas.set('KHIONE STEERING FILE', 'ice_monoclass_carstens1_sr.cas')
        self.add_study('vnv_1','telemac2d', 't2d_racetrack_carstens.cas', cas=cas)
        del cas

        # Carstens 2 monoclass - parallel
        cas = TelemacCas('t2d_racetrack_carstens.cas', get_dico('telemac2d'))
        cas.set('PARALLEL PROCESSORS', 4)
        cas.set('DURATION', 800.)
        cas.set('INITIAL VALUES OF TRACERS', [0., 0.])
        cas.set('KHIONE STEERING FILE', 'ice_monoclass_carstens2_sr.cas')
        self.add_study('vnv_2','telemac2d', 't2d_racetrack_carstens.cas', cas=cas)
        del cas

        # Carstens 1 multiclass - parallel
        cas = TelemacCas('t2d_racetrack_carstens.cas', get_dico('telemac2d'))
        cas.set('PARALLEL PROCESSORS', 4)
        cas.set('DURATION', 600.)
        cas.set('KHIONE STEERING FILE', 'ice_multiclass_carstens1_sr.cas')
        self.add_study('vnv_3','telemac2d', 't2d_racetrack_carstens.cas', cas=cas)
        del cas

        # Carstens 2 multiclass - parallel
        cas = TelemacCas('t2d_racetrack_carstens.cas', get_dico('telemac2d'))
        cas.set('PARALLEL PROCESSORS', 4)
        cas.set('DURATION', 800.)
        cas.set('KHIONE STEERING FILE', 'ice_multiclass_carstens2_sr.cas')
        self.add_study('vnv_4','telemac2d', 't2d_racetrack_carstens.cas', cas=cas)
        del cas

    def _check_results(self):
        """
        Post-treatment processes
        """
        # Carstens 1 monoclass
        self.check_epsilons('vnv_1:T2DRES', 'f2d_racetrack_carstens_1_mono_sr.slf', eps=[1.E-2])
        self.check_epsilons('vnv_1:ICERES', 'fce_racetrack_carstens_1_mono_sr.slf', eps=[1.E-5])
        # Carstens 2 monoclass
        self.check_epsilons('vnv_2:T2DRES', 'f2d_racetrack_carstens_2_mono_sr.slf', eps=[1.E-2])
        self.check_epsilons('vnv_2:ICERES', 'fce_racetrack_carstens_2_mono_sr.slf', eps=[1.E-5])
        # Carstens 1 multiclass
        self.check_epsilons('vnv_3:T2DRES', 'f2d_racetrack_carstens_1_multi_sr.slf', eps=[3.E-2])
        self.check_epsilons('vnv_3:ICERES', 'fce_racetrack_carstens_1_multi_sr.slf', eps=[1.E-1])
        # Carstens 2 multiclass
        self.check_epsilons('vnv_4:T2DRES', 'f2d_racetrack_carstens_2_multi_sr.slf', eps=[4.E-2])
        self.check_epsilons('vnv_4:ICERES', 'fce_racetrack_carstens_2_multi_sr.slf', eps=[1.E-1])

    def _post(self):
        """
        Post-treatment processes
        """
        import numpy as np
        import matplotlib.pyplot as plt
        import matplotlib.dates as mdates
        import matplotlib.patches as patches
        from postel.plot_vnv import vnv_plot2d,\
            vnv_plot1d_polylines, vnv_khione_plot1d_TC
        from postel.plot2d import plot2d_triangle_mesh, \
            plot2d_scalar_filled_contour, plot2d_vectors
        import datetime

        geo, _ = self.get_study_res('vnv_1:T2DGEO', load_bnd=True)
        res = TelemacFile(self.get_study_file('vnv_1:T2DRES'))

        #======================================================================
        # FIRST OBSERVATION:
        #
        fig_size = (6.5, 4.5)
        extract = [2., -0.6]
        ite_to_plot = [0, -1]

        #=====================================================================
        # Carstens 1 monoclass:
        res = TelemacFile(self.get_study_file('vnv_1:T2DRES'))
        # reference data
        datarep = "data/data_carstens_1966/"
        T1 = np.loadtxt(datarep+"case1.csv", delimiter=',', comments='#', skiprows=0)
        T1[:,0] = 60.*T1[:,0]
        # get data values
        T, Ct, _ = khione_get_timeseries_on_points(res, [extract], ncfra=1)
        # Plot timeseries on points:
        vnv_khione_plot1d_TC(
            res.times, T, Ct, C=None,
            ref_T_data=T1, 
            ref_T_label="$T - Obs.$", 
            ref_T_error=0.01,
            normalize_time=True,
            normalize_reftime=True,
            normalize_Ct=True,
            fig_size=fig_size,
            fig_name='img/carstens1_monoclass_sr')

        #=====================================================================
        # Carstens 2 monoclass:
        res = TelemacFile(self.get_study_file('vnv_2:T2DRES'))
        # reference data
        datarep = "data/data_carstens_1966/"
        T2 = np.loadtxt(datarep+"case2.csv", delimiter=',', comments='#', skiprows=0)
        T2[:,0] = 60.*T2[:,0]
        # get data values
        T, Ct, _ = khione_get_timeseries_on_points(res, [extract], ncfra=1)
        # Plot timeseries on points:
        vnv_khione_plot1d_TC(
            res.times, T, Ct, C=None,
            ref_T_data=T2, 
            ref_T_label="$T - Obs.$", 
            ref_T_error=0.01,
            normalize_time=True,
            normalize_reftime=True,
            normalize_Ct=True,
            fig_size=fig_size,
            fig_name='img/carstens2_monoclass_sr')

        #=====================================================================
        # Carstens 1 multiclass:
        res = TelemacFile(self.get_study_file('vnv_3:T2DRES'))
        # reference data
        datarep = "data/data_carstens_1966/"
        T1 = np.loadtxt(datarep+"case1.csv", delimiter=',', comments='#', skiprows=0)
        T1[:,0] = 60.*T1[:,0]
        # get data values
        T, Ct, C = khione_get_timeseries_on_points(res, [extract], ncfra=10)
        # Plot timeseries on points:
        vnv_khione_plot1d_TC(
            res.times, T, Ct, C=C,
            ref_T_data=T1, 
            ref_T_label="$T - Obs.$", 
            ref_T_error=0.01,
            normalize_time=True,
            normalize_reftime=True,
            fig_size=(10,5),
            fig_name='img/carstens1_multiclass_sr_ck')
        vnv_khione_plot1d_TC(
            res.times, T, Ct, C=None,
            ref_T_data=T1, 
            ref_T_label="$T - Obs.$", 
            ref_T_error=0.01,
            normalize_time=True,
            normalize_reftime=True,
            normalize_Ct=True,
            fig_size=fig_size,
            fig_name='img/carstens1_multiclass_sr')

        #=====================================================================
        # Carstens 2 multiclass:
        res = TelemacFile(self.get_study_file('vnv_4:T2DRES'))
        # reference data
        datarep = "data/data_carstens_1966/"
        T2 = np.loadtxt(datarep+"case2.csv", delimiter=',', comments='#', skiprows=0)
        T2[:,0] = 60.*T2[:,0]
        # get data values
        T, Ct, C = khione_get_timeseries_on_points(res, [extract], ncfra=10)
        # Plot timeseries on points:
        vnv_khione_plot1d_TC(
            res.times, T, Ct, C=C,
            ref_T_data=T2, 
            ref_T_label="$T - Obs.$", 
            ref_T_error=0.01,
            normalize_time=True,
            normalize_reftime=True,
            fig_size=(10,5),
            fig_name='img/carstens2_multiclass_sr_ck')
        vnv_khione_plot1d_TC(
            res.times, T, Ct, C=None,
            ref_T_data=T2, 
            ref_T_label="$T - Obs.$", 
            ref_T_error=0.01,
            normalize_time=True,
            normalize_reftime=True,
            normalize_Ct=True,
            fig_size=fig_size,
            fig_name='img/carstens2_multiclass_sr')

        #=====================================================================
        # Closing files
        geo.close()
        res.close()

def khione_get_timeseries_on_points(res, points, ncfra=1):
    import numpy as np
    T = res.get_timeseries_on_points('TEMPERATURE', points)
    T = T[0, :]
    C = np.zeros((ncfra, len(T)))
    Ct = np.zeros((len(T)))
    if ncfra>1:
        for i in range(1, ncfra+1):
            Ci = res.get_timeseries_on_points('FRAZIL {}'.format(i), points)
            C[i-1, :] = Ci[0, :]
        for i in range(len(T)):
            Ct[i] = np.sum(C[:, i])
    else:
        Ct = res.get_timeseries_on_points('FRAZIL', points)
        Ct = Ct[0, :]
    return T, Ct, C
