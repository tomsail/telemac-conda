
"""
Validation script for wave
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
        self.rank = 0
        self.tags = ['telemac2d']

    def _pre(self):
        """
        Defining the studies
        """

        # wave scalar mode
        self.add_study('vnv_1',
                       'telemac2d',
                       't2d_wave.cas')


        # wave parallel mode
        cas = TelemacCas('t2d_wave.cas', get_dico('telemac2d'))
        cas.set('PARALLEL PROCESSORS', 4)

        self.add_study('vnv_2',
                       'telemac2d',
                       't2d_wave_par.cas',
                       cas=cas)

        del cas



    def _check_results(self):
        """
        Post-treatment processes
        """

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_1:T2DRES',
                            'f2d_wave.slf',
                            eps=[1e-4])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_2:T2DRES',
                            'f2d_wave.slf',
                            eps=[1.5e-4])

        # Comparison between sequential and parallel run.
        self.check_epsilons('vnv_1:T2DRES',
                            'vnv_2:T2DRES',
                            eps=[1.5e-4])


    def _post(self):
        """
        Post-treatment processes
        """
        from postel.plot_vnv import vnv_plot1d_polylines, vnv_plot2d

        # Getting files
        res_vnv_1_t2dgeo, _ = self.get_study_res('vnv_1:T2DGEO', load_bnd=True)
        res_vnv_1_t2dres, _ = self.get_study_res('vnv_1:T2DRES')

        #Plotting mesh
        vnv_plot2d('',
                   res_vnv_1_t2dgeo,
                   plot_mesh=True,
                   fig_size=(10, 2),
                   fig_name='img/Mesh',
                   annotate_bnd=True)

        # Plotting FREE SURFACE at 1
        vnv_plot2d('FREE SURFACE',
                   res_vnv_1_t2dres,
                   record=1,
                   filled_contours=True,
                   fig_size=(12, 3),
                   fig_name='img/FreeSurface_t1')

        # Plotting FREE SURFACE at 2
        vnv_plot2d('FREE SURFACE',
                   res_vnv_1_t2dres,
                   record=2,
                   filled_contours=True,
                   fig_size=(12, 3),
                   fig_name='img/FreeSurface_t2')

        # Plotting FREE SURFACE at 3
        vnv_plot2d('FREE SURFACE',
                   res_vnv_1_t2dres,
                   record=3,
                   filled_contours=True,
                   fig_size=(12, 3),
                   fig_name='img/FreeSurface_t3')

        # Plotting FREE SURFACE at 4
        vnv_plot2d('FREE SURFACE',
                   res_vnv_1_t2dres,
                   record=4,
                   filled_contours=True,
                   fig_size=(12, 3),
                   fig_name='img/FreeSurface_t4')

        # Plot
        vnv_plot1d_polylines(\
            'FREE SURFACE',
            res_vnv_1_t2dres,
            'free surface',
            poly=[[0, 0.15], [16, 0.15]],
            record=-1,
            fig_size=(10, 4),
            ref_name='SOLANAL',
            fig_name='img/FreeSurface_Y0_5')

        # Closing files
        res_vnv_1_t2dgeo.close()
        res_vnv_1_t2dres.close()
