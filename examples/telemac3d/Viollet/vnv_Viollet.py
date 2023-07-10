
"""
Validation script for Viollet
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
        self.rank = 2
        self.tags = ['telemac3d']

    def _pre(self):
        """
        Defining the studies
        """

        # Viollet scalar mode
        self.add_study('vnv_1',
                       'telemac3d',
                       't3d_viollet.cas')


        # Viollet parallel mode
        cas = TelemacCas('t3d_viollet.cas', get_dico('telemac3d'))
        cas.set('PARALLEL PROCESSORS', 4)

        self.add_study('vnv_2',
                       'telemac3d',
                       't3d_viollet_par.cas',
                       cas=cas)

        del cas



    def _check_results(self):
        """
        Post-treatment processes
        """

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_1:T3DRES',
                            'f3d_viollet.slf',
                            eps=[1.E-7, 1.E-8, 1.E-9, 1.E-8, 1.E-11, 1.E-11, 1.E-11, 2.E-4, 1.E-5, 1.E-11])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_2:T3DRES',
                            'f3d_viollet.slf',
                            eps=[1.E-7, 1.E-8, 1.E-9, 1.E-8, 1.E-11, 1.E-11, 1.E-11, 3.E-4, 1.E-5, 1.E-11])

        # Comparison between sequential and parallel run.
        self.check_epsilons('vnv_1:T3DRES',
                            'vnv_2:T3DRES',
                            eps=[1.E-7, 1.E-8, 1.E-9, 1.E-8, 1.E-11, 1.E-11, 1.E-11, 3.E-4, 1.E-5, 1.E-11])


    def _post(self):
        """
        Post-treatment processes
        """

        import matplotlib.pyplot as plt
        import numpy as np
        from postel.plot_actions import plot1d
        from postel.plot_vnv import vnv_plot2d

        # Getting files
        vnv_1_t3dres = self.get_study_file('vnv_1:T3DRES')
        res_vnv_1_t3dres = TelemacFile(vnv_1_t3dres)
        vnv_1_t3dhyd = self.get_study_file('vnv_1:T3DHYD')
        res_vnv_1_t3dhyd = TelemacFile(vnv_1_t3dhyd)
        res_vnv_1_t3dgeo, _ = self.get_study_res('vnv_1:T3DGEO', load_bnd=True, module="T3D")

        #Plotting mesh
        vnv_plot2d('BOTTOM',
                   res_vnv_1_t3dgeo,
                   plot_mesh=True,
                   fig_size=(18, 4),
                   fig_name='img/Mesh',
                   annotate_bnd=True)

        #Plotting vertical mesh
        vnv_plot2d('ELEVATION Z',
                   res_vnv_1_t3dres,
                   poly=[[0., 0.5], [10., 0.5]],
                   record=0,
                   y_label='z (m)',
                   plot_mesh=True,
                   fig_size=(12, 4),
                   fig_name='img/MeshV')

        # Plotting mesh with bottom at record 0
        vnv_plot2d('BOTTOM',
                   res_vnv_1_t3dhyd,
                   record=0,
                   filled_contours=True,
                   cbar_label='Bottom elevation (m)',
                   plot_mesh=True,
                   fig_size=(18, 4),
                   fig_name='img/Bottom')

        U1 = 1./30.
        h = 0.1
        T1 = 20.
        T2 = 25.35

        point1 = [1., 0.5]
        point2 = [3., 0.5]
        point3 = [10., 0.5]

        # Slice x = 1.

        timeseries_z = res_vnv_1_t3dres.get_timeseries_on_vertical_segment('ELEVATION Z', point1)
        timeseries_vel = res_vnv_1_t3dres.get_timeseries_on_vertical_segment('VELOCITY U', point1)
        timeseries_temp = res_vnv_1_t3dres.get_timeseries_on_vertical_segment('TEMPERATURE', point1)

        # Plotting vertical section of velocity along x
        fig, ax = plt.subplots(figsize=(6, 7))

        plot1d(ax, timeseries_vel[:,-1]/U1, timeseries_z[:,-1]/h,
               x_label='$U/U_1$', y_label='$z/h$',
               plot_label='TELEMAC-3D')

        # Loading profile 10
        data_10_vx = np.genfromtxt("data/Fr09_10_VX.dat",
                                   names=["z", "brut", "corr1", "corr2"])
        mes_vx = data_10_vx["corr1"]
        mes_z  = data_10_vx["z"]
        
        plt.plot(mes_vx, mes_z,'dk', label='Viollet Exp.')
        plt.xlabel('$U/U_1$')
        plt.ylabel('$z/h$')
        plt.ylim(0., 2.)
        plt.grid()
        plt.title('Velocity profile $x/h$ = 10')
        ax.legend()

        fig_name = 'img/velocity_profile_10'
        print(" "*8+'~> Plotting '+fig_name)
        plt.savefig(fig_name)
        plt.close('all')

        # Plotting vertical section of temperature
        fig, ax = plt.subplots(figsize=(6, 7))

        plot1d(ax, (timeseries_temp[:,-1]-T1)/(T2-T1), timeseries_z[:,-1]/h,
               x_label='$T-T_1 / T_2-T_1$', y_label='$z/h$',
               plot_label='TELEMAC-3D')

        # Loading profile 10
        data_10_temp = np.genfromtxt("data/Fr09_10_TC.dat",
                                     names=["z", "T"])
        mes_T = data_10_temp["T"]
        mes_z = data_10_temp["z"]
        
        plt.plot(mes_T, mes_z,'dk', label='Viollet Exp.')
        plt.xlabel('$T-T_1 / T_2-T_1$')
        plt.ylabel('$z/h$')
        plt.ylim(0., 2.)
        plt.grid()
        plt.title('Temperature profile $x/h$ = 10')
        ax.legend()

        fig_name = 'img/temp_profile_10'
        print(" "*8+'~> Plotting '+fig_name)
        plt.savefig(fig_name)
        plt.close('all')

        # Slice x = 3.

        # Plotting vertical section of velocity along x
        fig, ax = plt.subplots(figsize=(6, 7))

        timeseries_z = res_vnv_1_t3dres.get_timeseries_on_vertical_segment('ELEVATION Z', point2)
        timeseries_vel = res_vnv_1_t3dres.get_timeseries_on_vertical_segment('VELOCITY U', point2)
        timeseries_temp = res_vnv_1_t3dres.get_timeseries_on_vertical_segment('TEMPERATURE', point2)

        plot1d(ax, timeseries_vel[:,-1]/U1, timeseries_z[:,-1]/h,
               x_label='$U/U_1$', y_label='$z/h$',
               plot_label='TELEMAC-3D')

        # Loading profile 30
        data_30_vx = np.genfromtxt("data/Fr09_30_VX.dat",
                                   names=["z", "brut", "corr1", "corr2"])
        mes_vx = data_30_vx["corr1"]
        mes_z  = data_30_vx["z"]
        
        plt.plot(mes_vx, mes_z,'dk', label='Viollet Exp.')
        plt.xlabel('$U/U_1$')
        plt.ylabel('$z/h$')
        plt.ylim(0., 2.)
        plt.grid()
        plt.title('Velocity profile $x/h$ = 30')
        ax.legend()

        fig_name = 'img/velocity_profile_30'
        print(" "*8+'~> Plotting '+fig_name)
        plt.savefig(fig_name)
        plt.close('all')

        # Plotting vertical section of temperature
        fig, ax = plt.subplots(figsize=(6, 7))

        plot1d(ax, (timeseries_temp[:,-1]-T1)/(T2-T1), timeseries_z[:,-1]/h,
               x_label='$T-T_1 / T_2-T_1$', y_label='$z/h$',
               plot_label='TELEMAC-3D')

        # Loading profile 30
        data_30_temp = np.genfromtxt("data/Fr09_30_TC.dat",
                                     names=["z", "T"])
        mes_T = data_30_temp["T"]
        mes_z = data_30_temp["z"]
        
        plt.plot(mes_T, mes_z,'dk', label='Viollet Exp.')
        plt.xlabel('$T-T_1 / T_2-T_1$')
        plt.ylabel('$z/h$')
        plt.ylim(0., 2.)
        plt.grid()
        plt.title('Temperature profile $x/h$ = 30')
        ax.legend()

        fig_name = 'img/temp_profile_30'
        print(" "*8+'~> Plotting '+fig_name)
        plt.savefig(fig_name)
        plt.close('all')

        # Slice x = 10.

        # Plotting vertical section of velocity along x
        fig, ax = plt.subplots(figsize=(6, 7))

        timeseries_z = res_vnv_1_t3dres.get_timeseries_on_vertical_segment('ELEVATION Z', point3)
        timeseries_vel = res_vnv_1_t3dres.get_timeseries_on_vertical_segment('VELOCITY U', point3)
        timeseries_temp = res_vnv_1_t3dres.get_timeseries_on_vertical_segment('TEMPERATURE', point3)

        plot1d(ax, timeseries_vel[:,-1]/U1, timeseries_z[:,-1]/h,
               x_label='$U/U_1$', y_label='$z/h$',
               plot_label='TELEMAC-3D')

        # Loading profile 100
        data_100_vx = np.genfromtxt("data/Fr09_100_VX.dat",
                                    names=["z", "brut", "corr1", "corr2"])
        mes_vx = data_100_vx["corr1"]
        mes_z  = data_100_vx["z"]
        
        plt.plot(mes_vx, mes_z,'dk', label='Viollet Exp.')
        plt.xlabel('$U/U_1$')
        plt.ylabel('$z/h$')
        plt.ylim(0., 2.)
        plt.grid()
        plt.title('Velocity profile $x/h$ = 100')
        ax.legend()

        fig_name = 'img/velocity_profile_100'
        print(" "*8+'~> Plotting '+fig_name)
        plt.savefig(fig_name)
        plt.close('all')

        # Plotting vertical section of temperature
        fig, ax = plt.subplots(figsize=(6, 7))

        plot1d(ax, (timeseries_temp[:,-1]-T1)/(T2-T1), timeseries_z[:,-1]/h,
               x_label='$T-T_1 / T_2-T_1$', y_label='$z/h$',
               plot_label='TELEMAC-3D')

        # Loading profile 100
        data_100_temp = np.genfromtxt("data/Fr09_100_TC.dat",
                                      names=["z", "T"])
        mes_T = data_100_temp["T"]
        mes_z = data_100_temp["z"]
        
        plt.plot(mes_T, mes_z,'dk', label='Viollet Exp.')
        plt.xlabel('$T-T_1 / T_2-T_1$')
        plt.ylabel('$z/h$')
        plt.ylim(0., 2.)
        plt.grid()
        plt.title('Temperature profile $x/h$ = 100')
        ax.legend()

        fig_name = 'img/temp_profile_100'
        print(" "*8+'~> Plotting '+fig_name)
        plt.savefig(fig_name)
        plt.close('all')

        res_vnv_1_t3dres.close()
        res_vnv_1_t3dhyd.close()
        res_vnv_1_t3dgeo.close()
