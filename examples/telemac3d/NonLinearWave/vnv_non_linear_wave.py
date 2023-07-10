
"""
Validation script for NonLinearWave
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
        self.tags = ['telemac3d', 'postel3d']
        # For vnv_1
        self.walltime = "12:00:00"

    def _pre(self):
        """
        Defining the studies
        """

        # NonLinearWave scalar mode
        self.add_study('vnv_1',
                       'telemac3d',
                       't3d_nonlinearwave.cas')


        # NonLinearWave parallel mode
        cas = TelemacCas('t3d_nonlinearwave.cas', get_dico('telemac3d'))
        cas.set('PARALLEL PROCESSORS', 4)

        self.add_study('vnv_2',
                       'telemac3d',
                       't3d_nonlinearwave_par.cas',
                       cas=cas)

        del cas

        # Test of postel3d post-treatment
        self.add_study('p3d',
                       'postel3d',
                       'p3d_nonlinearwave.cas')


    def _check_results(self):
        """
        Post-treatment processes
        """

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_1:T3DRES',
                            'f3d_nonlinearwave.slf',
                            eps=[1.E-9])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_2:T3DRES',
                            'f3d_nonlinearwave.slf',
                            eps=[1.E-9])

        # Comparison between sequential and parallel run.
        self.check_epsilons('vnv_1:T3DRES',
                            'vnv_2:T3DRES',
                            eps=[1.E-9])


    def _post(self):
        """
        Post-treatment processes
        """
        from postel.plot_vnv import vnv_plot2d, vnv_plot1d_polylines
        from postel.plot1d import plot1d
        import numpy as np
        import matplotlib.pyplot as plt
                # Getting files
        vnv_1_t3dres = self.get_study_file('vnv_1:T3DRES')
        res_vnv_1_t3dres = TelemacFile(vnv_1_t3dres)
        vnv_1_t2dres = self.get_study_file('vnv_1:T3DHYD')
        res_vnv_1_t2dres = TelemacFile(vnv_1_t2dres)

        #Plotting mesh
        vnv_plot2d('ELEVATION Z',
                   res_vnv_1_t3dres,
                   plot_mesh=True,
                   fig_size=(12, 7),
                   fig_name='img/mesh')

        #Plotting vertical mesh at initial time step along x
        vnv_plot2d('ELEVATION Z',
                   res_vnv_1_t3dres,
                   poly=[[0, 0.15], [32, 0.15]],
#                   x_label='y (m)',
                   y_label='z (m)',
                   record=0,
                   plot_mesh=True,
                   fig_size=(12, 3.5),
                   fig_name='img/MeshV')

        # Plotting FREE SURFACE in vertical slice
        vnv_plot1d_polylines('FREE SURFACE',
                             res_vnv_1_t2dres,
                             'Free Surface',
                             fig_size=(12, 5),
                             poly=[[0., 0.15], [32., 0.15]],
                             record=-1,
                             fig_name='img/FreeSurface_bottom',
                             plot_bottom=True)

        # Plotting FREE SURFACE in vertical slice
        vnv_plot1d_polylines('FREE SURFACE',
                             res_vnv_1_t2dres,
                             'Free Surface',
                             fig_size=(12, 3),
                             poly=[[0., 0.15], [32., 0.15]],
                             record=-1,
                             fig_name='img/FreeSurface_long_profile')
        measures_file = 'measures.txt'
        dingeman = np.array(np.loadtxt(measures_file))
        indictime = np.where((dingeman[:,0]>=31) & (dingeman[:,0]<=33))[0]
        indicpoint=[3,5,6,8,10,12]

        times=res_vnv_1_t2dres.times
        indictel=np.where((times>=31) & (times<=33))[0]
        points=[[5.2,0.15],[10.5,0.15],[12.5,0.15],[14.5,0.15],[17.3,0.15],[21,0.15]]
        data = res_vnv_1_t2dres.get_timeseries_on_points('FREE SURFACE', points)
        
        for i, point in enumerate(points):
            fig, axe = plt.subplots(figsize=(6, 3))
            axe.plot( times[indictel], data[i,indictel], label='TELEMAC-3D')
            axe.plot(dingeman[indictime,0], dingeman[indictime,indicpoint[i]], 'rx', label='Dingemans')
            titre = 'Free surface at x = '+str(point[0])+' m'
            axe.set_xlabel('t (s)')
            axe.set_ylabel('z (m)')
            axe.legend(loc='upper left')
            axe.set_title(titre)
            file_fig='img/pt'+str(i)+'.png'
            print('saving',file_fig)
            plt.savefig(file_fig)
            plt.close(1)

        res_vnv_1_t3dres.close()
        res_vnv_1_t2dres.close()
