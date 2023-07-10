
"""
Validation script for friction_2
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
        self.tags = ['artemis']

    def _pre(self):
        """
        Defining the studies
        """

        # friction_2 scalar mode
        self.add_study('vnv_1',
                       'artemis',
                       'art_friction_2.cas')


        # friction_2 parallel mode
        cas = TelemacCas('art_friction_2.cas', get_dico('artemis'))
        cas.set('PARALLEL PROCESSORS', 4)
        cas.set('SOLVER', 9)


        self.add_study('vnv_2',
                       'artemis',
                       'art_friction_2_par.cas',
                       cas=cas)

        del cas



    def _check_results(self):
        """
        Post-treatment processes
        """

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_1:ARTRES',
                            'f2d_friction_2.slf',
                            eps=[1.e-8, 1.e-8, 1.e-8, 1.e-8, 1.e-8, 1.e-8, 1.e-8, 1.e-8, 1.e-7, 1.e-8, 1.e-8, 1.e-8])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_2:ARTRES',
                            'f2d_friction_2.slf',
                            eps=[1.e-8, 1.e-8, 1.e-8, 1.e-8, 1.e-8, 1.e-8, 0.001, 0.001, 0.4, 1.e-5, 1.e-5, 1.e-5])

        # Comparison between sequential and parallel run.
        self.check_epsilons('vnv_1:ARTRES',
                            'vnv_2:ARTRES',
                            eps=[1.e-8, 1.e-8, 1.e-8, 1.e-8, 1.e-8, 1.e-8, 0.001, 0.001, 0.4, 1.e-5, 1.e-5, 1.e-5])


    def _post(self):
        """
        Post-treatment processes
        """
        from postel.plot_vnv import vnv_plot2d, vnv_plot1d_polylines
        # Getting files
        vnv_1_artres = self.get_study_file('vnv_1:ARTRES')
        res_vnv_1_artres = TelemacFile(vnv_1_artres)
        vnv_1_artgeo = self.get_study_file('vnv_1:ARTGEO')
        res_vnv_1_artgeo = TelemacFile(vnv_1_artgeo)

        # Plotting WAVE HEIGHT over polyline over records 0
        vnv_plot1d_polylines(\
                'WAVE HEIGHT',
                res_vnv_1_artres,
                legend_labels="wave hright at t=0.0s",
                poly=[[0, 0.6], [19.9, 0.6]],
                fig_size=(10, 7),
                fig_name='img/Section_2')

        #Plotting mesh
        vnv_plot2d('',
                   res_vnv_1_artgeo,
                   plot_mesh=True,
                   fig_size=(12, 4),
                   fig_name='img/Mesh_2')


        # Plotting WAVE HEIGHT at 0
        vnv_plot2d('WAVE HEIGHT',
                   res_vnv_1_artres,
                   record=0,
                   filled_contours=True,
                   fig_size=(12, 4),
                   fig_name='img/Wave_height_2')

        # Closing files
        res_vnv_1_artres.close()
        res_vnv_1_artgeo.close()
