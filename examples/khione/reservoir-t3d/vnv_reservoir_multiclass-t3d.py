
"""
Validation script for multi-class frazil ice model
"""
from vvytel.vnv_study import AbstractVnvStudy
from execution.telemac_cas import TelemacCas, get_dico
from data_manip.extraction.telemac_file import TelemacFile
from data_manip.computation.datetimes import compute_datetimes

class VnvStudy(AbstractVnvStudy):
    """
    Class for validation
    """
    def _init(self):
        """
        Defines the general parameter
        """
        self.rank = 4
        self.tags = ['telemac3d', 'khione']

    def _pre(self):
        """
        Defining the studies
        """
        # thermal budget under icy conditions (serial)
        self.add_study('vnv_1', 'telemac3d', 't3d_reservoir_multiclass.cas')

        # thermal budget under icy conditions (parallel)
        cas = TelemacCas('t3d_reservoir_multiclass.cas', get_dico('telemac3d'))
        cas.set('PARALLEL PROCESSORS', 4)
        self.add_study('vnv_2', 'telemac3d', 't3d_reservoir_multiclass_par.cas', cas=cas)
        del cas

        # thermal budget melting (serial)
        self.add_study('vnv_3', 'telemac3d', 't3d_reservoir_multiclass_melting.cas')

        # thermal budget melting (parallel)
        cas = TelemacCas('t3d_reservoir_multiclass_melting.cas', get_dico('telemac3d'))
        cas.set('PARALLEL PROCESSORS', 4)
        self.add_study('vnv_4', 'telemac3d', 't3d_reservoir_multiclass_melting_par.cas', cas=cas)
        del cas

        # thermal budget under icy conditions (seeding model)
        cas = TelemacCas('t3d_reservoir_multiclass.cas', get_dico('telemac3d'))
        ice_cas = TelemacCas('ice_reservoir_multiclass.cas', get_dico('khione'))
        ice_cas.set('MODEL FOR FRAZIL SEEDING', 0)
        ice_cas.write('ice_tmp.cas')
        cas.set('KHIONE STEERING FILE','ice_tmp.cas')
        self.add_study('vnv_seed0', 'telemac3d', 't3d_reservoir_multiclass_seed0.cas', cas=cas)
        ice_cas.set('MODEL FOR FRAZIL SEEDING', 2)
        ice_cas.write('ice_tmp.cas')
        cas.set('KHIONE STEERING FILE','ice_tmp.cas')
        self.add_study('vnv_seed2', 'telemac3d', 't3d_reservoir_multiclass_seed2.cas', cas=cas)
        ice_cas.set('MODEL FOR FRAZIL SEEDING', 3)
        ice_cas.write('ice_tmp.cas')
        cas.set('KHIONE STEERING FILE','ice_tmp.cas')
        self.add_study('vnv_seed3', 'telemac3d', 't3d_reservoir_multiclass_seed3.cas', cas=cas)
        del cas

        # thermal budget under icy conditions (seeding rate)
        cas = TelemacCas('t3d_reservoir_multiclass.cas', get_dico('telemac3d'))
        ice_cas = TelemacCas('ice_reservoir_multiclass.cas', get_dico('khione'))
        ice_cas.set('MODEL FOR FRAZIL SEEDING', 2)
        ice_cas.set('FRAZIL SEEDING RATE', [4.E3, 0., 0., 0., 0., 0., 0., 0., 0., 0.])
        ice_cas.set('MINIMUM NUMBER OF FRAZIL CRYSTALS', 0.)
        ice_cas.write('ice_tmp.cas')
        cas.set('KHIONE STEERING FILE','ice_tmp.cas')
        self.add_study('vnv_sr1', 'telemac3d', 't3d_reservoir_multiclass_sr1.cas', cas=cas)
        ice_cas.set('FRAZIL SEEDING RATE', [6.E3, 0., 0., 0., 0., 0., 0., 0., 0., 0.])
        ice_cas.write('ice_tmp.cas')
        cas.set('KHIONE STEERING FILE','ice_tmp.cas')
        self.add_study('vnv_sr2', 'telemac3d', 't3d_reservoir_multiclass_sr2.cas', cas=cas)
        ice_cas.set('FRAZIL SEEDING RATE', [8.E3, 0., 0., 0., 0., 0., 0., 0., 0., 0.])
        ice_cas.write('ice_tmp.cas')
        cas.set('KHIONE STEERING FILE','ice_tmp.cas')
        self.add_study('vnv_sr3', 'telemac3d', 't3d_reservoir_multiclass_sr3.cas', cas=cas)
        ice_cas.set('FRAZIL SEEDING RATE', [10.E3, 0., 0., 0., 0., 0., 0., 0., 0., 0.])
        ice_cas.write('ice_tmp.cas')
        cas.set('KHIONE STEERING FILE','ice_tmp.cas')
        self.add_study('vnv_sr4', 'telemac3d', 't3d_reservoir_multiclass_sr4.cas', cas=cas)
        del cas

        # thermal budget under icy conditions (min number of particle)
        cas = TelemacCas('t3d_reservoir_multiclass.cas', get_dico('telemac3d'))
        ice_cas = TelemacCas('ice_reservoir_multiclass.cas', get_dico('khione'))
        ice_cas.set('MODEL FOR FRAZIL SEEDING', 1)
        ice_cas.set('FRAZIL SEEDING RATE', [0., 0., 0., 0., 0., 0., 0., 0., 0., 0.])
        ice_cas.set('MINIMUM NUMBER OF FRAZIL CRYSTALS', 1000.)
        ice_cas.write('ice_tmp.cas')
        cas.set('KHIONE STEERING FILE','ice_tmp.cas')
        self.add_study('vnv_min1', 'telemac3d', 't3d_reservoir_multiclass_min1.cas', cas=cas)
        ice_cas.set('MINIMUM NUMBER OF FRAZIL CRYSTALS', 2000.)
        ice_cas.write('ice_tmp.cas')
        cas.set('KHIONE STEERING FILE','ice_tmp.cas')
        self.add_study('vnv_min2', 'telemac3d', 't3d_reservoir_multiclass_min2.cas', cas=cas)
        ice_cas.set('MINIMUM NUMBER OF FRAZIL CRYSTALS', 4000.)
        ice_cas.write('ice_tmp.cas')
        cas.set('KHIONE STEERING FILE','ice_tmp.cas')
        self.add_study('vnv_min3', 'telemac3d', 't3d_reservoir_multiclass_min3.cas', cas=cas)
        ice_cas.set('MINIMUM NUMBER OF FRAZIL CRYSTALS', 5000.)
        ice_cas.write('ice_tmp.cas')
        cas.set('KHIONE STEERING FILE','ice_tmp.cas')
        self.add_study('vnv_min4', 'telemac3d', 't3d_reservoir_multiclass_min4.cas', cas=cas)
        del cas

        # thermal budget under icy conditions (model for floculation)
        cas = TelemacCas('t3d_reservoir_multiclass.cas', get_dico('telemac3d'))
        ice_cas = TelemacCas('ice_reservoir_multiclass.cas', get_dico('khione'))
        ice_cas.set('MODEL FOR THE FLOCCULATION AND BREAKUP', 0)
        ice_cas.write('ice_tmp.cas')
        cas.set('KHIONE STEERING FILE','ice_tmp.cas')
        self.add_study('vnv_nofloc', 'telemac3d', 't3d_reservoir_multiclass_nofloc.cas', cas=cas)
        del cas

        # thermal budget under icy conditions (afloc parameter)
        cas = TelemacCas('t3d_reservoir_multiclass.cas', get_dico('telemac3d'))
        ice_cas = TelemacCas('ice_reservoir_multiclass.cas', get_dico('khione'))
        ice_cas.set('FLOCCULATION AFLOC PARAMETER', 1e-6)
        ice_cas.write('ice_tmp.cas')
        cas.set('KHIONE STEERING FILE','ice_tmp.cas')
        self.add_study('vnv_af6', 'telemac3d', 't3d_reservoir_multiclass_af6.cas', cas=cas)
        ice_cas.set('FLOCCULATION AFLOC PARAMETER', 1e-5)
        ice_cas.write('ice_tmp.cas')
        cas.set('KHIONE STEERING FILE','ice_tmp.cas')
        self.add_study('vnv_af5', 'telemac3d', 't3d_reservoir_multiclass_af5.cas', cas=cas)
        ice_cas.set('FLOCCULATION AFLOC PARAMETER', 1e-3)
        ice_cas.write('ice_tmp.cas')
        cas.set('KHIONE STEERING FILE','ice_tmp.cas')
        self.add_study('vnv_af3', 'telemac3d', 't3d_reservoir_multiclass_af3.cas', cas=cas)
        ice_cas.set('FLOCCULATION AFLOC PARAMETER', 1e-2)
        ice_cas.write('ice_tmp.cas')
        cas.set('KHIONE STEERING FILE','ice_tmp.cas')
        self.add_study('vnv_af2', 'telemac3d', 't3d_reservoir_multiclass_af2.cas', cas=cas)
        del cas

        # thermal budget under icy conditions (nmax parameter)
        cas = TelemacCas('t3d_reservoir_multiclass.cas', get_dico('telemac3d'))
        ice_cas = TelemacCas('ice_reservoir_multiclass.cas', get_dico('khione'))
        ice_cas.set('SECONDARY NUCLEATION NMAX PARAMETER', 1.e5)
        ice_cas.write('ice_tmp.cas')
        cas.set('KHIONE STEERING FILE','ice_tmp.cas')
        self.add_study('vnv_sn4', 'telemac3d', 't3d_reservoir_multiclass_sn4.cas', cas=cas)
        ice_cas.set('SECONDARY NUCLEATION NMAX PARAMETER', 5.e5)
        ice_cas.write('ice_tmp.cas')
        cas.set('KHIONE STEERING FILE','ice_tmp.cas')
        self.add_study('vnv_sn5', 'telemac3d', 't3d_reservoir_multiclass_sn5.cas', cas=cas)
        ice_cas.set('SECONDARY NUCLEATION NMAX PARAMETER', 5.e6)
        ice_cas.write('ice_tmp.cas')
        cas.set('KHIONE STEERING FILE','ice_tmp.cas')
        self.add_study('vnv_sn6', 'telemac3d', 't3d_reservoir_multiclass_sn6.cas', cas=cas)
        ice_cas.set('SECONDARY NUCLEATION NMAX PARAMETER', 1.e7)
        ice_cas.write('ice_tmp.cas')
        cas.set('KHIONE STEERING FILE','ice_tmp.cas')
        self.add_study('vnv_sn7', 'telemac3d', 't3d_reservoir_multiclass_sn7.cas', cas=cas)
        del cas

        # thermal budget under icy conditions (model for secondary nucleation)
        cas = TelemacCas('t3d_reservoir_multiclass.cas', get_dico('telemac3d'))
        ice_cas = TelemacCas('ice_reservoir_multiclass.cas', get_dico('khione'))
        ice_cas.set('MODEL FOR THE SECONDARY NUCLEATION', 0)
        ice_cas.write('ice_tmp.cas')
        cas.set('KHIONE STEERING FILE','ice_tmp.cas')
        self.add_study('vnv_nonuc', 'telemac3d', 't3d_reservoir_multiclass_nonuc.cas', cas=cas)
        del cas

        # thermal budget under icy conditions (buoyancy velocity)
        cas = TelemacCas('t3d_reservoir_multiclass.cas', get_dico('telemac3d'))
        ice_cas = TelemacCas('ice_reservoir_multiclass.cas', get_dico('khione'))
        ice_cas.set('MODEL FOR THE BUOYANCY VELOCITY', 1)
        ice_cas.write('ice_tmp.cas')
        cas.set('KHIONE STEERING FILE','ice_tmp.cas')
        self.add_study('vnv_buo1', 'telemac3d', 't3d_reservoir_multiclass_buo1.cas', cas=cas)
        ice_cas.set('MODEL FOR THE BUOYANCY VELOCITY', 2)
        ice_cas.write('ice_tmp.cas')
        cas.set('KHIONE STEERING FILE','ice_tmp.cas')
        self.add_study('vnv_buo2', 'telemac3d', 't3d_reservoir_multiclass_buo2.cas', cas=cas)
        ice_cas.set('MODEL FOR THE BUOYANCY VELOCITY', 3)
        ice_cas.write('ice_tmp.cas')
        cas.set('KHIONE STEERING FILE','ice_tmp.cas')
        self.add_study('vnv_buo3', 'telemac3d', 't3d_reservoir_multiclass_buo3.cas', cas=cas)
        del cas

        # thermal budget under icy conditions (salinity)
        cas = TelemacCas('t3d_reservoir_multiclass.cas', get_dico('telemac3d'))
        ice_cas = TelemacCas('ice_reservoir_multiclass.cas', get_dico('khione'))
        ice_cas.set('SALINITY', False)
        ice_cas.write('ice_tmp.cas')
        cas.set('KHIONE STEERING FILE','ice_tmp.cas')
        self.add_study('vnv_nosal', 'telemac3d', 't3d_reservoir_multiclass_nosal.cas', cas=cas)
        del cas


    def _check_results(self):
        """
        Post-treatment processes
        """
        self.check_epsilons('vnv_1:T3DRES', 'f3d_reservoir_multiclass.slf', eps=[1.E-6])
        self.check_epsilons('vnv_2:T3DRES', 'f3d_reservoir_multiclass.slf', eps=[1.E-6])
        self.check_epsilons('vnv_1:T3DRES', 'vnv_2:T3DRES', eps=[1.E-6])
        self.check_epsilons('vnv_1:ICERES', 'fce_reservoir_multiclass.slf', eps=[1.E-6])
        self.check_epsilons('vnv_2:ICERES', 'fce_reservoir_multiclass.slf', eps=[1.E-6])
        self.check_epsilons('vnv_1:ICERES', 'vnv_2:ICERES', eps=[1.E-6])


    def _post(self):
        """
        Post-treatment processes
        """
        from postel.plot_vnv import vnv_plot2d,\
            vnv_plot1d_polylines,vnv_plot1d_history
        import datetime
        import matplotlib.pyplot as plt

        geo, _ = self.get_study_res('vnv_1:T3DGEO')
        res = TelemacFile(self.get_study_file('vnv_1:ICERES'))
        res_melt = TelemacFile(self.get_study_file('vnv_3:ICERES'))
        seed0 = TelemacFile(self.get_study_file('vnv_seed0:ICERES'))
        seed2 = TelemacFile(self.get_study_file('vnv_seed2:ICERES'))
        seed3 = TelemacFile(self.get_study_file('vnv_seed3:ICERES'))
        sr1 = TelemacFile(self.get_study_file('vnv_sr1:ICERES'))
        sr2 = TelemacFile(self.get_study_file('vnv_sr2:ICERES'))
        sr3 = TelemacFile(self.get_study_file('vnv_sr3:ICERES'))
        sr4 = TelemacFile(self.get_study_file('vnv_sr4:ICERES'))
        min1 = TelemacFile(self.get_study_file('vnv_min1:ICERES'))
        min2 = TelemacFile(self.get_study_file('vnv_min2:ICERES'))
        min3 = TelemacFile(self.get_study_file('vnv_min3:ICERES'))
        min4 = TelemacFile(self.get_study_file('vnv_min4:ICERES'))
        nofloc = TelemacFile(self.get_study_file('vnv_nofloc:ICERES'))
        af6 = TelemacFile(self.get_study_file('vnv_af6:ICERES'))
        af5 = TelemacFile(self.get_study_file('vnv_af5:ICERES'))
        af3 = TelemacFile(self.get_study_file('vnv_af3:ICERES'))
        af2 = TelemacFile(self.get_study_file('vnv_af2:ICERES'))
        sn4 = TelemacFile(self.get_study_file('vnv_sn4:ICERES'))
        sn5 = TelemacFile(self.get_study_file('vnv_sn5:ICERES'))
        sn6 = TelemacFile(self.get_study_file('vnv_sn6:ICERES'))
        sn7 = TelemacFile(self.get_study_file('vnv_sn7:ICERES'))
        nonuc = TelemacFile(self.get_study_file('vnv_nonuc:ICERES'))
        buo1 = TelemacFile(self.get_study_file('vnv_buo1:ICERES'))
        buo2 = TelemacFile(self.get_study_file('vnv_buo2:ICERES'))
        buo3 = TelemacFile(self.get_study_file('vnv_buo3:ICERES'))
        nosal = TelemacFile(self.get_study_file('vnv_nosal:ICERES'))

        fig_size = (6., 4.)

        #Plotting mesh
        vnv_plot2d(\
            '',
            geo,
            plot_mesh=True,
            fig_size=(7, 7),
            fig_name='img/mesh')

        # Plot timeseries on points:
        plot1d_history_TCf(\
            res=res,
            points=[[0., 0.]],
            fig_name='img/multiclass_TCf')

        # Plot timeseries on points:
        plot1d_history_TCf(\
            res=res_melt,
            points=[[0., 0.]],
            fig_name='img/multiclass_TCf_melt')

        #Plot seeding model
        vnv_plot1d_history(\
            'TOTAL CONCENTRAT',
            [seed0,res,seed2],
            points_labels=['', '', ''],
            legend_labels=['No seeding',
                           'Minimum conc. threshold',
                           'Constant surfacic seeding rate'],
            points=[[0., 0.]],
            fig_size=fig_size,
            fig_name='img/seed_fra')

        vnv_plot1d_history(\
            'TEMPERATURE',
            [seed0,res,seed2],
            points_labels=['', '', ''],
            legend_labels=['No seeding',
                           'Minimum conc. threshold',
                           'Constant surfacic seeding rate'],
            points=[[0., 0.]],
            fig_size=fig_size,
            fig_name='img/seed_temp')

        #plot frazil seeding rate sensitivity
        vnv_plot1d_history(\
            'TOTAL CONCENTRAT',
            [sr1,sr2,sr3,sr4],
            points_labels=['', '', '', '', ''],
            legend_labels=['$\\tau_{s}=8.10^{3}$',
                           '$\\tau_{s}=6.10^{3}$',
                           '$\\tau_{s}=8.10^{3}$',
                           '$\\tau_{s}=10.10^{3}$'],
            points=[[0., 0.]],
            fig_size=fig_size,
            fig_name='img/seeding_rate_fra')

        vnv_plot1d_history(\
            'TEMPERATURE',
            [sr1,sr2,sr3,sr4],
            points_labels=['', '', '', '', ''],
            legend_labels=['$\\tau_{s}=8.10^{3}$',
                           '$\\tau_{s}=6.10^{3}$',
                           '$\\tau_{s}=8.10^{3}$',
                           '$\\tau_{s}=10.10^{3}$'],
            points=[[0., 0.]],
            fig_size=fig_size,
            fig_name='img/seeding_rate_temp')

        #plot minimal number of frazil sensitivity
        vnv_plot1d_history(\
            'TOTAL CONCENTRAT',
            [min1,min2,res,min3,min4],
            points_labels=['', '', '', '', ''],
            legend_labels=['Min. $n_k=100$',
                           'Min. $n_k=200$',
                           'Min. $n_k=320$',
                           'Min. $n_k=400$',
                           'Min. $n_k=500$'],
            points=[[0., 0.]],
            fig_size=fig_size,
            fig_name='img/min_part_fra')

        vnv_plot1d_history(\
            'TEMPERATURE',
            [min1,min2,res,min3,min4],
            points_labels=['', '', '', '', ''],
            legend_labels=['Min. $n_k=100$',
                           'Min. $n_k=200$',
                           'Min. $n_k=320$',
                           'Min. $n_k=400$',
                           'Min. $n_k=500$'],
            points=[[0., 0.]],
            fig_size=fig_size,
            fig_name='img/min_part_temp')

        #plot floculation model
        vnv_plot1d_history(\
            'TOTAL CONCENTRAT',
            [nofloc,res],
            points_labels=['', ''],
            legend_labels=['No floculation','Floculation'],
            points=[[0., 0.]],
            fig_size=fig_size,
            fig_name='img/floc_model_fra')

        vnv_plot1d_history(\
            'TEMPERATURE',
            [nofloc,res],
            points_labels=['', ''],
            legend_labels=['No floculation','Floculation'],
            points=[[0., 0.]],
            fig_size=fig_size,
            fig_name='img/floc_model_temp')

        # Plot timeseries on points:
        plot1d_history_TCf(\
            res=nofloc,
            points=[[0., 0.]],
            fig_name='img/multiclass_TCf_nofloc')

        #plot afloc sensitivity
        vnv_plot1d_history(\
            'TOTAL CONCENTRAT',
            [af6,af5,res,af3,af2],
            points_labels=['', '', '', '', ''],
            legend_labels=['$a_{floc}=10^{-6}$',
                           '$a_{floc}=10^{-5}$',
                           '$a_{floc}=10^{-4}$',
                           '$a_{floc}=10^{-3}$',
                           '$a_{floc}=10^{-2}$'],
            points=[[0., 0.]],
            fig_size=fig_size,
            fig_name='img/floc_alpha_fra')

        vnv_plot1d_history(\
            'TEMPERATURE',
            [af6,af5,res,af3,af2],
            points_labels=['', '', '', '', ''],
            legend_labels=['$a_{floc}=10^{-6}$',
                           '$a_{floc}=10^{-5}$',
                           '$a_{floc}=10^{-4}$',
                           '$a_{floc}=10^{-3}$',
                           '$a_{floc}=10^{-2}$'],
            points=[[0., 0.]],
            fig_size=fig_size,
            fig_name='img/floc_alpha_temp')

        #plot nmax sensitivity
        vnv_plot1d_history(\
            'TOTAL CONCENTRAT',
            [sn4,sn5,res,sn6,sn7],
            points_labels=['', '', '', '', ''],
            legend_labels=['$n_{max}=10^{5}$',
                           '$n_{max}=5.10^{5}$',
                           '$n_{max}=10^{6}$',
                           '$n_{max}=5.10^{6}$',
                           '$n_{max}=10^{7}$'],
            points=[[0., 0.]],
            fig_size=fig_size,
            fig_name='img/nmax_fra')

        vnv_plot1d_history(\
            'TEMPERATURE',
            [sn4,sn5,res,sn6,sn7],
            points_labels=['', '', '', '', ''],
            legend_labels=['$n_{max}=10^{5}$',
                           '$n_{max}=5.10^{5}$',
                           '$n_{max}=10^{6}$',
                           '$n_{max}=5.10^{6}$',
                           '$n_{max}=10^{7}$'],
            points=[[0., 0.]],
            fig_size=fig_size,
            fig_name='img/nmax_temp')

        #plot floculation model
        vnv_plot1d_history(\
            'TOTAL CONCENTRAT',
            [nonuc,res],
            points_labels=['', ''],
            legend_labels=['No secondary nucleation','Secondary nucleation'],
            points=[[0., 0.]],
            fig_size=fig_size,
            fig_name='img/nuc_model_fra')

        vnv_plot1d_history(\
            'TEMPERATURE',
            [nonuc,res],
            points_labels=['', ''],
            legend_labels=['No secondary nucleation','Secondary nucleation'],
            points=[[0., 0.]],
            fig_size=fig_size,
            fig_name='img/nuc_model_temp')

        # Plot timeseries on points:
        plot1d_history_TCf(\
            res=nonuc,
            points=[[0., 0.]],
            fig_name='img/multiclass_TCf_nonuc')

        #plot buoyancy velocity model
        vnv_plot1d_history(\
            'TOTAL CONCENTRAT',
            [buo1,buo2,buo3,res],
            points_labels=['', '', '', ''],
            legend_labels=['Daly (1984)',
                           'Daly intermediate (1984)',
                           'Matousek (1992)',
                           'Gosik and Osterkamp (1983)'],
            points=[[0., 0.]],
            fig_size=fig_size,
            fig_name='img/buo_fra')

        vnv_plot1d_history(\
            'TEMPERATURE',
            [buo1,buo2,buo3,res],
            points_labels=['', '', '', ''],
            legend_labels=['Daly (1984)',
                           'Daly intermediate (1984)',
                           'Matousek (1992)',
                           'Gosik and Osterkamp (1983)'],
            points=[[0., 0.]],
            fig_size=fig_size,
            fig_name='img/buo_temp')

        #plot floculation model
        vnv_plot1d_history(\
            'TOTAL CONCENTRAT',
            [nosal,res],
            points_labels=['', ''],
            legend_labels=['No salinity','Salinity'],
            points=[[0., 0.]],
            fig_size=fig_size,
            fig_name='img/sal_fra')

        vnv_plot1d_history(\
            'TEMPERATURE',
            [nosal,res],
            points_labels=['', ''],
            legend_labels=['No salinity','Salinity'],
            points=[[0., 0.]],
            fig_size=fig_size,
            fig_name='img/sal_temp')

        # Plot timeseries on points:
        plot1d_history_TCf(\
            res=nosal,
            points=[[0., 0.]],
            fig_name='img/multiclass_TCf_nosal')

        # Closing files
        geo.close()
        res.close()
        res_melt.close()


def plot1d_history_TCf(res, points, ncfra=10, xlim=None, fig_name=''):
    """
    Plot 1d timeseries of temperature and frazil
    """
    import numpy as np
    import matplotlib.dates as mdates
    import matplotlib.pyplot as plt
    from postel.deco_vnv import decoVNV, decoVNV_1d

    # plot initialization
    plt.style.use('default')
    plt.rcParams.update(decoVNV)
    plt.rcParams.update(decoVNV_1d)
    fig, ax = plt.subplots(1, 1, figsize=(6.5, 4.5))
    axb = ax.twinx()

    # get data values
    T = res.get_timeseries_on_points('TEMPERATURE', points)
    Ci = []
    for i in range(1, ncfra+1):
        C = res.get_timeseries_on_points('FRAZIL CLASS {}'.format(i), points)
        Ci.append(C[0, :])

    C = res.get_timeseries_on_points('TOTAL CONCENTRAT', points)

    # get datetimes
    #datetimes = compute_datetimes(res.times, initial_date=res.datetime)

    # plot both T and Cf
    ax.plot(res.times, T[0, :], label='$T$', color='#003d74')
    axb.grid()
    for i in range(ncfra):
        axb.plot(res.times, Ci[i], label='$C_{}$'.format(i))
    axb.plot(res.times, C[0, :], label='$C_t$')

    # set labels
    #ax.xaxis.set_major_formatter(mdates.DateFormatter("%M"))
    if xlim is not None:
        ax.set_xlim(xlim[0], xlim[1])
    ax.set_xlabel('t (s)')
    ax.set_ylabel(r'$T$ $(\degree C)$')
    axb.set_ylabel('$C$ (-)')
    ax.legend(loc=3)
    axb.legend(loc=4)

    # save figure:
    print(" "*8+"~> Plotting {}".format(fig_name))
    fig.savefig(fig_name)

    # Close figure:
    plt.close('all')
