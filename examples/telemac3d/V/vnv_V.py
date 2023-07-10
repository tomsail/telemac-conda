
"""
Validation script for V
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
        self.tags = ['telemac3d', 'postel3d']

    def _pre(self):
        """
        Defining the studies
        """

        # V scalar mode
        self.add_study('vnv_1',
                       'telemac3d',
                       't3d_V_tetra.cas')


        # V parallel mode
        cas = TelemacCas('t3d_V_tetra.cas', get_dico('telemac3d'))
        cas.set('PARALLEL PROCESSORS', 4)

        self.add_study('vnv_2',
                       'telemac3d',
                       't3d_V_tetra_par.cas',
                       cas=cas)

        del cas


        # V scalar mode
        self.add_study('vnv_3',
                       'telemac3d',
                       't3d_V_prism.cas')


        # V parallel mode
        cas = TelemacCas('t3d_V_prism.cas', get_dico('telemac3d'))
        cas.set('PARALLEL PROCESSORS', 4)

        self.add_study('vnv_4',
                       'telemac3d',
                       't3d_V_prism_par.cas',
                       cas=cas)

        del cas

        # post-treatment
        self.add_study('p3d',
                       'postel3d',
                       'p3d_V.cas')



    def _check_results(self):
        """
        Post-treatment processes
        """

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_1:T3DRES',
                            'f3d_V.slf',
                            eps=[1.E-15])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_2:T3DRES',
                            'f3d_V.slf',
                            eps=[1.E-15])

        # Comparison between sequential and parallel run.
        self.check_epsilons('vnv_1:T3DRES',
                            'vnv_2:T3DRES',
                            eps=[1.E-15])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_3:T3DRES',
                            'f3d_V_prism.slf',
                            eps=[1.E-15])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_4:T3DRES',
                            'f3d_V_prism.slf',
                            eps=[1.E-15])

        # Comparison between sequential and parallel run.
        self.check_epsilons('vnv_3:T3DRES',
                            'vnv_4:T3DRES',
                            eps=[1.E-15])


    def _post(self):
        """
        Post-treatment processes
        """
        from postel.plot_vnv import vnv_plot2d
        # Getting files
        vnv_3_t3dres = self.get_study_file('vnv_3:T3DRES')
        res_vnv_3_t3dres = TelemacFile(vnv_3_t3dres)
        vnv_1_t3dres = self.get_study_file('vnv_1:T3DRES')
        res_vnv_1_t3dres = TelemacFile(vnv_1_t3dres)
        vnv_1_t3dhyd = self.get_study_file('vnv_1:T3DHYD')
        res_vnv_1_t3dhyd = TelemacFile(vnv_1_t3dhyd)

        # Plotting mesh with bottom at record 0
        vnv_plot2d('BOTTOM',
                   res_vnv_1_t3dhyd,
                   record=0,
                   filled_contours=True,
                   cbar_label='Bottom elevation (m)',
                   plot_mesh=True,
                   fig_size=(12, 3),
                   fig_name='img/Mesh')

        # Plotting vertical split
        vnv_plot2d(\
                   'ELEVATION Z',
                   res_vnv_1_t3dres,
                   poly=[[250, 0], [250, 100]],
                   record=0,
                   y_label='z (m)',
                   plot_mesh=True,
                   fig_name='img/MeshV')


        # Plotting vertical split
        vnv_plot2d(\
                   'TEMPERATURE',
                   res_vnv_1_t3dres,
                   poly=[[250, 0], [250, 100]],
                   record=0,
                   y_label='z (m)',
                   filled_contours=True,
                   cbar_label='T (°C)',
                   fig_size=(10, 10),
                   fig_name='img/init_T_tetra')

        # Plotting vertical split
        vnv_plot2d(\
                   'TEMPERATURE',
                   res_vnv_1_t3dres,
                   poly=[[250, 0], [250, 100]],
                   record=-1,
                   y_label='z (m)',
                   filled_contours=True,
                   cbar_label='T (°C)',
                   fig_size=(10, 10),
                   fig_name='img/end_T_tetra')

        # Plotting vertical split
        vnv_plot2d(\
                   'TEMPERATURE',
                   res_vnv_3_t3dres,
                   poly=[[250, 0], [250, 100]],
                   record=0,
                   y_label='z (m)',
                   filled_contours=True,
                   cbar_label='T (°C)',
                   fig_size=(10, 10),
                   fig_name='img/init_T_prism')

        # Plotting vertical split
        vnv_plot2d(\
                   'TEMPERATURE',
                   res_vnv_3_t3dres,
                   poly=[[250, 0], [250, 100]],
                   record=-1,
                   y_label='z (m)',
                   filled_contours=True,
                   cbar_label='T (°C)',
                   fig_size=(10, 10),
                   fig_name='img/end_T_prism')

        # Plotting vertical split
        vnv_plot2d(\
                   'VELOCITY W',
                   res_vnv_1_t3dres,
                   poly=[[250, 0], [250, 100]],
                   record=0,
                   y_label='z (m)',
                   filled_contours=True,
                   cbar_label='W (m/s)',
                   fig_size=(10, 10),
                   fig_name='img/init_V_tetra')

        # Plotting vertical split
        vnv_plot2d(\
                   'VELOCITY W',
                   res_vnv_1_t3dres,
                   poly=[[250, 0], [250, 100]],
                   record=-1,
                   y_label='z (m)',
                   filled_contours=True,
                   cbar_label='W (m/s)',
                   fig_size=(10, 10),
                   fig_name='img/end_V_tetra')

        # Plotting vertical split
        vnv_plot2d(\
                   'VELOCITY W',
                   res_vnv_3_t3dres,
                   poly=[[250, 0], [250, 100]],
                   record=0,
                   y_label='z (m)',
                   filled_contours=True,
                   cbar_label='W (m/s)',
                   fig_size=(10, 10),
                   fig_name='img/init_V_prism')

        # Plotting vertical split
        vnv_plot2d(\
                   'VELOCITY W',
                   res_vnv_3_t3dres,
                   poly=[[250, 0], [250, 100]],
                   record=-1,
                   y_label='z (m)',
                   filled_contours=True,
                   cbar_label='W (m/s)',
                   fig_size=(10, 10),
                   fig_name='img/end_V_prism')
        # Closing files
        res_vnv_3_t3dres.close()
        res_vnv_1_t3dres.close()
        res_vnv_1_t3dhyd.close()
