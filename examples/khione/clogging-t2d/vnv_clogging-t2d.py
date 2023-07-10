
"""
Validation script for clogging
"""
from vvytel.vnv_study import AbstractVnvStudy
from execution.telemac_cas import TelemacCas, get_dico
from data_manip.extraction.telemac_file import TelemacFile

class VnvStudy(AbstractVnvStudy):
    """
    Class for validation
    """

    def _init(self):
        """
        Defines the general parameter
        """
        self.rank = 3
        self.tags = ['telemac2d', 'khione']

    def _pre(self):
        """
        Defining the studies
        """

        # clogging
        self.add_study('vnv_1',
                       'telemac2d',
                       't2d_clogging.cas')


        # clogging (parallel)
        cas = TelemacCas('t2d_clogging.cas', get_dico('telemac2d'))
        cas.set('PARALLEL PROCESSORS', 4)

        self.add_study('vnv_2',
                       'telemac2d',
                       't2d_clogging_par.cas',
                       cas=cas)

        # clogging section
        cas = TelemacCas('t2d_clogging.cas', get_dico('telemac2d'))
        cas.set('KHIONE STEERING FILE','ice_clogging_section.cas')
        cas.set('PARALLEL PROCESSORS',1)

        self.add_study('vnv_3',
                       'telemac2d',
                       't2d_clogging_section.cas',
                       cas=cas)

        # clogging section (parallel)
        cas = TelemacCas('t2d_clogging.cas', get_dico('telemac2d'))
        cas.set('KHIONE STEERING FILE','ice_clogging_section.cas')
        cas.set('PARALLEL PROCESSORS',4)

        self.add_study('vnv_4',
                       'telemac2d',
                       't2d_clogging_section_par.cas',
                       cas=cas)
        del cas

    def _check_results(self):
        """
        Post-treatment processes
        """
        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_1:T2DRES',
                            'f2d_clogging.slf',
                            eps=[1.E-6])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_2:T2DRES',
                            'f2d_clogging.slf',
                            eps=[1.E-4])

        # Comparison between serial and parallel run.
        self.check_epsilons('vnv_1:T2DRES',
                            'vnv_2:T2DRES',
                            eps=[1.E-4])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_1:ICERES',
                            'fce_clogging.slf',
                            eps=[1.E-6])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_2:ICERES',
                            'fce_clogging.slf',
                            eps=[1.E-4])

        # Comparison between serial and parallel run.
        self.check_epsilons('vnv_1:ICERES',
                            'vnv_2:ICERES',
                            eps=[1.E-4])

    def _post(self):
        """
        Post-treatment processes
        """
        import numpy as np
        import matplotlib.pyplot as plt
        from postel.plot_vnv import vnv_plot2d,vnv_plot1d_polylines
        from postel.plot1d import plot1d
        from postel.plot2d import plot2d_scalar_map

        # GET TELEMAC RESULT FILES:
        res, _ = self.get_study_res('vnv_1:T2DRES',load_bnd=True)
        res_ice, _ = self.get_study_res('vnv_1:ICERES',load_bnd=True)
        clog_file = self.get_study_file('vnv_1:CLGRFO')
        clog_file_sec = self.get_study_file('vnv_3:CLGRFO')
        ice_cas = TelemacCas('ice_clogging_section.cas', get_dico('khione'))

        #Plotting mesh
        vnv_plot2d(\
            '',
            res,
            plot_mesh=True,
            annotate_bnd=True,
            fig_size=(14, 2.5),
            fig_name='img/mesh')

        #Plotting bottom
        vnv_plot2d(\
            'BOTTOM',
            res,
            record=0,
            filled_contours=True,
            fig_size=(14, 2.5),
            fig_name='img/bottom',
            x_label='$x$ (m)',
            y_label='$y$ (m)',
            cbar_label='Bottom (m)')

        #Profile elevation
        vnv_plot1d_polylines(\
            'FREE SURFACE',
            res,
            legend_labels='free surface',
            record=0,
            poly=[[0., 75.], [10000., 75.]],
            poly_number=[50],
            fig_size=(10, 3),
            y_label='$z$ (m)',
            x_label='$x$ (m)',
            ylim=[4,7],
            fig_name='img/l-section-hydro',
            plot_bottom=True)

        #Temperature and frazil
        poly_points = [[0.,75.],[10000.,75.]]
        poly_number =[50]
        _, abs_curv, T = res.get_timeseries_on_polyline('TEMPERATURE', poly_points, poly_number)
        _, abs_curv, F = res_ice.get_timeseries_on_polyline('FRAZIL', poly_points, poly_number)
        fig, ax = plt.subplots(figsize=(10,3))
        ax.set_xlabel('x (m)')
        color = 'tab:red'
        ax.plot(abs_curv,T[:,-1],color=color)
        ax.set_ylabel('Temperature (°)',color=color)
        ax.tick_params(axis='y',color=color)
        ax2 = ax.twinx()
        color = 'tab:blue'
        ax2.plot(abs_curv,F[:,-1],color=color)
        ax2.set_ylabel('Frazil concentration (-)',color=color)
        ax2.tick_params(axis='y',color=color)
        fig_name='img/l-section-trac'
        print(" "*8+"~> Plotting {}".format(fig_name))
        plt.savefig(fig_name)
        plt.close('all')

        #Clogging data file: avaliable area for the flow
        clog_data = np.loadtxt(clog_file, delimiter=" ", comments='#',skiprows=5)
        fig, ax = plt.subplots(figsize=(10,3))
        ax.plot(clog_data[:,0],clog_data[:,4])
        ax.set_xlabel('Time (s)')
        ax.set_ylabel('Avaliable area (m$^2$)')
        fig_name='img/clog_area'
        print(" "*8+"~> Plotting {}".format(fig_name))
        plt.savefig(fig_name)
        plt.close('all')

        #Clogging data file: frazil volume on the grid
        fig, ax = plt.subplots(figsize=(10,3))
        ax.plot(clog_data[:,0],clog_data[:,7])
        ax.set_xlabel('Time (s)')
        ax.set_ylabel('Frazil volume (m$^3$)')
        fig_name='img/clog_volume'
        print(" "*8+"~> Plotting {}".format(fig_name))
        plt.savefig(fig_name)
        plt.close('all')

        #Location of the section
        x=[]
        y=[]
        section = ice_cas.get('CLOGGED SECTIONS')
        bottom = res.get_data_value('BOTTOM', 0)
        fig, ax = plt.subplots(1, 1, figsize=(10, 3))
        x.append(res.meshx[section[0]-1])
        x.append(res.meshx[section[1]-1])
        y.append(res.meshy[section[0]-1])
        y.append(res.meshy[section[1]-1])
        plot2d_scalar_map(fig, ax, res.tri, bottom, data_name='bottom elevation (m)')
        ax.plot(x,y,'r--')
        fig_name='img/sec'
        print(" "*8+"~> Plotting {}".format(fig_name))
        plt.savefig(fig_name)
        plt.close('all')

        #Clogging data file: avaliable area for the flow over a section
        clog_data = np.loadtxt(clog_file_sec, delimiter=" ", comments='#',skiprows=5)
        fig, ax = plt.subplots(figsize=(10,3))
        ax.plot(clog_data[:,0],clog_data[:,12])
        ax.set_xlabel('Time (s)')
        ax.set_ylabel('Avaliable area (m$^2$)')
        fig_name='img/clog_area_sec'
        print(" "*8+"~> Plotting {}".format(fig_name))
        plt.savefig(fig_name)
        plt.close('all')

        #Clogging data file: frazil volume on the grid over a section
        fig, ax = plt.subplots(figsize=(10,3))
        ax.plot(clog_data[:,0],clog_data[:,15])
        ax.set_xlabel('Time (s)')
        ax.set_ylabel('Frazil volume (m$^3$)')
        fig_name='img/clog_volume_sec'
        print(" "*8+"~> Plotting {}".format(fig_name))
        plt.savefig(fig_name)
        plt.close('all')
