
"""
Validation script for lock-exchange
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
        self.rank = 1
        self.tags = ['telemac3d', 'med']

    def _pre(self):
        """
        Defining the studies
        """

        # lock-exchange scalar mode
        self.add_study('vnv_1',
                       'telemac3d',
                       't3d_lock-exchange.cas')


        # lock-exchange parallel mode
        cas = TelemacCas('t3d_lock-exchange.cas', get_dico('telemac3d'))
        cas.set('PARALLEL PROCESSORS', 4)

        self.add_study('vnv_2',
                       'telemac3d',
                       't3d_lock-exchange_par.cas',
                       cas=cas)

        del cas



    def _check_results(self):
        """
        Post-treatment processes
        """

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_1:T3DRES',
                            'f3d_lock-exchange.slf',
                            eps=[1.E-7])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_2:T3DRES',
                            'f3d_lock-exchange.slf',
                            eps=[1.E-7])

        # Comparison between sequential and parallel run.
        self.check_epsilons('vnv_1:T3DRES',
                            'vnv_2:T3DRES',
                            eps=[1.E-7])


    def _post(self):
        """
        Post-treatment processes
        """
        from postel.plot_vnv import vnv_plot2d
                # Getting files
        vnv_1_t3dres = self.get_study_file('vnv_1:T3DRES')
        res = TelemacFile(vnv_1_t3dres)

        # Plotting vertical mesh
        vnv_plot2d('ELEVATION Z',
                   res,
                   poly=[[0, 1], [30, 1]],
                   y_label='z (m)',
                   record=-1,
                   plot_mesh=True,
                   fig_size=(20, 10),
                   xlim=(0, 30),
                   ylim=(-4, 0),
                   fig_name="img/lock_mesh_vertical")

        # Plotting mesh
        vnv_plot2d('ELEVATION Z',
                   res,
                   plane=res.nplan-1,
                   plot_mesh=True,
                   fig_size=(20, 10),
                   xlim=(0, 30),
                   ylim=(0, 1.2),
                   fig_name="img/lock_mesh_horizontal")

        # Plotting vertical split
        vnv_plot2d('SALINITY',
                   res,
                   poly=[[0, 1], [30, 1]],
                   fig_size=(20, 10),
                   record=0,
                   cbar_label='Salinity (g/L)',
                   filled_contours=True,
                   fig_name='img/lock_initial_salinity')

        # Plotting vertical split
        vnv_plot2d('SALINITY',
                   res,
                   poly=[[0, 1], [30, 1]],
                   fig_size=(20, 10),
                   y_label='z (m)',
                   record=-1,
                   cbar_label='Salinity (g/L)',
                   filled_contours=True,
                   fig_name='img/lock-exchange_dpwaveq_PSI2_5corr')

        res.close()
