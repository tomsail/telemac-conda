
"""
Validation script for phase_auto
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
        self.tags = ['artemis']

    def _pre(self):
        """
        Defining the studies
        """

        # AUTOMATIC PHASE FOR OFFSHORE -45deg INCIDENT WAVES
        self.add_study('vnv_1',
                       'artemis',
                       'art_phase.cas')


        # AUTOMATIC PHASE FOR OFFSHORE -45deg INCIDENT WAVES
        cas = TelemacCas('art_phase.cas', get_dico('artemis'))
        cas.set('PARALLEL PROCESSORS', 4)

        self.add_study('vnv_2',
                       'artemis',
                       'art_phase_par.cas',
                       cas=cas)

        del cas



    def _check_results(self):
        """
        Post-treatment processes
        """

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_1:ARTRES',
                            'f2d_phase.slf',
                            eps=[1.e-5, 1.e-5, 1.e-8, 1.e-8, 1.e-4, 1.e-4, 0.5, 1.e-8])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_2:ARTRES',
                            'f2d_phase.slf',
                            eps=[1.e-5, 1.e-5, 1.e-8, 1.e-8, 1.e-4, 1.e-4, 0.5, 1.e-8])

        # Comparison between sequential and parallel run.
        self.check_epsilons('vnv_1:ARTRES',
                            'vnv_2:ARTRES',
                            eps=[1.e-5, 1.e-5, 1.e-8, 1.e-8, 1.e-4, 1.e-4, 0.5, 1.e-8])


    def _post(self):
        """
        Post-treatment processes
        """
        from postel.plot_vnv import vnv_plot2d, vnv_plot1d_polylines
        # Getting files
        vnv_1_artgeo = self.get_study_file('vnv_1:ARTGEO')
        res_vnv_1_artgeo = TelemacFile(vnv_1_artgeo)
        vnv_1_artres = self.get_study_file('vnv_1:ARTRES')
        res_vnv_1_artres = TelemacFile(vnv_1_artres)

        polys = [[[300, 0], [900, 0]],
                 [[300, 100], [900, 100]],
                 [[300, 200], [900, 200]],
                 [[300, 300], [900, 300]]]

        for i, poly in enumerate(polys):
            # Plotting WAVE HEIGHT over polyline over records 0
            vnv_plot1d_polylines(\
                    'WAVE HEIGHT',
                    res_vnv_1_artres,
                    legend_labels="wave height at t=0.0s",
                    poly=poly,
                    fig_size=(10, 7),
                    fig_name='img/Section{}'.format(i+1))

        #Plotting mesh
        #TODO: add polylines on plot
        vnv_plot2d('',
                   res_vnv_1_artgeo,
                   plot_mesh=True,
                   fig_size=(12, 8),
                   fig_name='img/Mesh')


        # Plotting WAVE HEIGHT at 0
        vnv_plot2d('WAVE HEIGHT',
                   res_vnv_1_artres,
                   record=0,
                   filled_contours=True,
                   cmap_name='viridis',
                   fig_size=(12, 8),
                   fig_name='img/Wave_Height')


        # Plotting PHASE VELOCITY at 0
        vnv_plot2d('PHASE VELOCITY',
                   res_vnv_1_artres,
                   record=0,
                   filled_contours=True,
                   cmap_name='viridis',
                   fig_size=(12, 8),
                   fig_name='img/Phase_Velocity')

        # Closing files
        res_vnv_1_artgeo.close()
        res_vnv_1_artres.close()
