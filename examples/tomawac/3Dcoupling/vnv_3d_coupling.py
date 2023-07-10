
"""
Validation script for 3Dcoupling
"""
from vvytel.vnv_study import AbstractVnvStudy
from execution.telemac_cas import TelemacCas, get_dico
from data_manip.extraction.telemac_file import TelemacFile
from postel.plot_vnv import vnv_plot2d

class VnvStudy(AbstractVnvStudy):
    """
    Class for validation
    """

    def _init(self):
        """
        Defines the general parameter
        """
        self.rank = 1
        self.tags = ['telemac3d', 'tomawac']

    def _pre(self):
        """
        Defining the studies
        """

        # littoral3D T3D+TOM scalar mode 2D coupling
        self.add_study('vnv_1',
                       'telemac3d',
                       't3d_littoral.cas')


        # littoral3D T3D+TOM parallel mode 2D coupling
        cas = TelemacCas('t3d_littoral.cas', get_dico('telemac3d'))
        cas.set('PARALLEL PROCESSORS', 4)

        self.add_study('vnv_2',
                       'telemac3d',
                       't3d_littoral_par.cas',
                       cas=cas)
        del cas


        # littoral3D T3D+TOM scalar mode 3D coupling
        self.add_study('vnv_3',
                       'telemac3d',
                       't3d_3Dcoupling.cas')


        # littoral3D T3D+TOM parallel mode 3D coupling
        cas = TelemacCas('t3d_3Dcoupling.cas', get_dico('telemac3d'))
        cas.set('PARALLEL PROCESSORS', 4)

        self.add_study('vnv_4',
                       'telemac3d',
                       't3d_3Dcoupling_par.cas',
                       cas=cas)
        del cas

        # littoral3D T3D+TOM scalar TEL2TOM + mode 2D coupling
        self.add_study('vnv_5',
                       'telemac3d',
                       't3d_littoral_diff.cas')


        # littoral3D T3D+TOM parallel TEL2TOM  + mode 2D coupling
        cas = TelemacCas('t3d_littoral_diff.cas', get_dico('telemac3d'))
        cas.set('PARALLEL PROCESSORS', 4)

        self.add_study('vnv_6',
                       'telemac3d',
                       't3d_littoral_diff_par.cas',
                       cas=cas)
        del cas

        # littoral3D T3D+TOM scalar TEL2TOM + 3D coupling
        self.add_study('vnv_7',
                       'telemac3d',
                       't3d_3Dcoupling_diff.cas')

        # littoral3D T3D+TOM parallel TEL2TOM  + 3D coupling
        cas = TelemacCas('t3d_3Dcoupling_diff.cas', get_dico('telemac3d'))
        cas.set('PARALLEL PROCESSORS', 4)

        self.add_study('vnv_8',
                       'telemac3d',
                       't3d_3Dcoupling_diff_par.cas',
                       cas=cas)
        del cas

        # littoral3D T3D+TOM scalar TEL2TOM + 3D coupling
        self.add_study('vnv_9',
                       'telemac3d',
                       't3d_littoral_same.cas')

        # littoral3D T3D+TOM parallel TEL2TOM  + 3D coupling
        cas = TelemacCas('t3d_littoral_same.cas', get_dico('telemac3d'))
        cas.set('PARALLEL PROCESSORS', 4)

        self.add_study('vnv_10',
                       'telemac3d',
                       't3d_littoral_same_par.cas',
                       cas=cas)
        del cas

        # littoral3D T3D+TOM scalar TEL2TOM + 3D coupling
        self.add_study('vnv_11',
                       'telemac3d',
                       't3d_3Dcoupling_same.cas')


        # littoral3D T3D+TOM parallel TEL2TOM  + 3D coupling
        cas = TelemacCas('t3d_3Dcoupling_same.cas', get_dico('telemac3d'))
        cas.set('PARALLEL PROCESSORS', 4)

        self.add_study('vnv_12',
                       'telemac3d',
                       't3d_3Dcoupling_same_par.cas',
                       cas=cas)
        del cas


    def _check_results(self):
        """
        Post-treatment processes
        """

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_1:T3DRES',
                            'f3d_littoral.slf',
                            eps=[1e-8, 1e-8, 1e-8, 1e-8])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_2:T3DRES',
                            'f3d_littoral.slf',
                            eps=[1e-8, 1e-8, 1e-8, 1e-8])

        # Comparison between sequential and parallel run.
        self.check_epsilons('vnv_1:T3DRES',
                            'vnv_2:T3DRES',
                            eps=[1e-8, 1e-8, 1e-8, 1e-8])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_1:WACRES',
                            'fom_littoral.slf',
                            eps=[1e-8, 1e-8, 1e-8, 1e-8, 1e-8, 1e-8, 1e-8, 1e-8, 1e-8])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_2:WACRES',
                            'fom_littoral.slf',
                            eps=[1e-8, 1e-8, 1e-8, 1e-8, 1e-8, 1e-8, 1e-8, 1e-8, 1e-8])

        # Comparison between sequential and parallel run.
        self.check_epsilons('vnv_1:WACRES',
                            'vnv_2:WACRES',
                            eps=[1e-8, 1e-8, 1e-8, 1e-8, 1e-8, 1e-8, 1e-8, 1e-8, 1e-8])

        # 3D COUPLING Comparison with a reference for T3D.
        self.check_epsilons('vnv_3:T3DRES',
                            'f3d_littoralcoup.slf',
                            eps=[1e-4, 6e-3, 7e-3, 4e-4, 2e-5, 5e-5, 6e-6])

        # 3D COUPLING Comparison with a reference for WAC
        self.check_epsilons('vnv_3:WACRES',
                            'fom_couplittoral.slf',
                            eps=[5e-3, 1e-1, 1e-8, 2e-4, 7e-3, 3e-3, 6e-6, 2e-5, 2e-2])

        # 3D COUPLING Comparison between sequential and parallel for T3D.
        self.check_epsilons('vnv_3:T3DRES',
                            'vnv_4:T3DRES',
                            eps=[1e-8])

        # 3D COUPLING Comparison between sequential and parallel for WAC.
        self.check_epsilons('vnv_3:WACRES',
                            'vnv_4:WACRES',
                            eps=[1e-8, 3e-8, 1e-8, 1e-8, 1e-8, 1e-8, 1e-8, 1e-8, 1e-8])

        # TEL2TOM COUPLING Comparison with a reference for T3D.
        self.check_epsilons('vnv_5:T3DRES',
                            'f3d_littoral_diff.slf',
                            eps=[1e-8])

        # TEL2TOM COUPLING Comparison with a reference for WAC
        self.check_epsilons('vnv_5:WACRES',
                            'fom_littoral_diff.slf',
                            eps=[1e-8])

        # TEL2TOM COUPLING Comparison between sequential and parallel for T3D.
        self.check_epsilons('vnv_5:T3DRES',
                            'vnv_6:T3DRES',
                            eps=[1e-8])

        # TEL2TOM COUPLING Comparison between sequential and parallel for WAC.
        self.check_epsilons('vnv_5:WACRES',
                            'vnv_6:WACRES',
                            eps=[1e-8])

        # TEL2TOM 3D COUPLING Comparison with a reference for T3D.
        self.check_epsilons('vnv_7:T3DRES',
                            'f3d_couplittoral_diff.slf',
                            eps=[1e-8, 6e-8, 3e-8, 1e-8, 6e-8, 1e-8, 1e-8])

        # TEL2TOM 3D COUPLING Comparison with a reference for WAC
        self.check_epsilons('vnv_7:WACRES',
                            'fom_couplittoral_diff.slf',
                            eps=[1e-8])

        # TEL2TOM 3D COUPLING Comparison between sequential and parallel for T3D.
        self.check_epsilons('vnv_7:T3DRES',
                            'vnv_8:T3DRES',
                            eps=[1e-8])

        # TEL2TOM 3D COUPLING Comparison between sequential and parallel for WAC.
        self.check_epsilons('vnv_7:WACRES',
                            'vnv_8:WACRES',
                            eps=[1e-8])

        # TEL2TOM COUPLING Comparison with a reference for T3D.
        self.check_epsilons('vnv_9:T3DRES',
                            'f3d_littoral.slf',
                            eps=[1e-8])

        # TEL2TOM COUPLING Comparison with a reference for WAC
        self.check_epsilons('vnv_9:WACRES',
                            'fom_littoral.slf',
                            eps=[1e-8])

        # TEL2TOM COUPLING Comparison between sequential and parallel for T3D.
        self.check_epsilons('vnv_9:T3DRES',
                            'vnv_10:T3DRES',
                            eps=[1e-8])

        # TEL2TOM COUPLING Comparison between sequential and parallel for WAC.
        self.check_epsilons('vnv_9:WACRES',
                            'vnv_10:WACRES',
                            eps=[1e-8])

        # TEL2TOM COUPLING Comparison with a reference for T3D.
        self.check_epsilons('vnv_11:T3DRES',
                            'f3d_littoralcoup.slf',
                            eps=[1e-4, 6e-3, 7e-3, 4e-4, 2e-5, 5e-5, 6e-6])

        # TEL2TOM COUPLING Comparison with a reference for WAC
        self.check_epsilons('vnv_11:WACRES',
                            'fom_couplittoral.slf',
                            eps=[5e-3, 1e-1, 1e-8, 2e-4, 7e-3, 3e-3, 6e-6, 2e-5, 2e-2])

        # TEL2TOM COUPLING Comparison between sequential and parallel for T3D.
        self.check_epsilons('vnv_11:T3DRES',
                            'vnv_12:T3DRES',
                            eps=[1e-8])

        # TEL2TOM COUPLING Comparison between sequential and parallel for WAC.
        self.check_epsilons('vnv_11:WACRES',
                            'vnv_12:WACRES',
                            eps=[1e-8])

    def _post(self):
        """
        Post-treatment processes
        """
        # Getting files
        vnv_1_wacres = self.get_study_file('vnv_1:WACRES')
        bnd1_wacgeo = self.get_study_file('vnv_1:WACCLI')
        res_vnv_1_wacres = TelemacFile(vnv_1_wacres, bnd_file=bnd1_wacgeo)
        vnv_1_t3dres = self.get_study_file('vnv_1:T3DRES')
        bnd1_t3dgeo = self.get_study_file('vnv_1:T3DCLI')
        res_vnv_1_t3dres = TelemacFile(vnv_1_t3dres, bnd_file=bnd1_t3dgeo)
        vnv_1_t3dgeo = self.get_study_file('vnv_1:T3DGEO')
        res_vnv_1_t3dgeo = TelemacFile(vnv_1_t3dgeo, bnd_file=bnd1_t3dgeo)
        vnv_3_t3dres = self.get_study_file('vnv_3:T3DRES')
        bnd3_t3dgeo = self.get_study_file('vnv_3:T3DCLI')
        res_vnv_3_t3dres = TelemacFile(vnv_3_t3dres, bnd_file=bnd3_t3dgeo)
        vnv_3_wacres = self.get_study_file('vnv_3:WACRES')
        bnd3_wacgeo = self.get_study_file('vnv_3:WACCLI')
        res_vnv_3_wacres = TelemacFile(vnv_3_wacres, bnd_file=bnd3_wacgeo)

        #Plotting mesh
        vnv_plot2d('',
                   res_vnv_1_t3dgeo,
                   xlim=[-200, 1200],
                   plot_mesh=True,
                   annotate_bnd=True,
                   fig_size=(9, 4),
                   fig_name='img/fond')


        # Plotting vertical split
        vnv_plot2d('VELOCITY U',
                   res_vnv_1_t3dres,
                   poly=[[500, 0], [500, 200]],
                   record=-1,
                   cbar_label='Velocity U (m/s)',
                   filled_contours=True,
                   fig_size=(12, 7),
                   fig_name='img/resultscoupVert')

        # Plotting vertical split
        vnv_plot2d('VELOCITY U',
                   res_vnv_1_t3dres,
                   poly=[[0, 100], [1000, 100]],
                   record=-1,
                   cbar_label='Velocity U (m/s)',
                   filled_contours=True,
                   fig_size=(12, 7),
                   fig_name='img/resultscoupVert2')

        # Plotting horizontal split
        vnv_plot2d('VELOCITY U',
                   res_vnv_1_t3dres,
                   plane=0,
                   record=-1,
                   cbar_label='Velocity U (m/s)',
                   filled_contours=True,
                   fig_size=(12, 7),
                   fig_name='img/resultshori')

        # Plotting WAVE HEIGHT HM0 at -1
        vnv_plot2d('WAVE HEIGHT HM0',
                   res_vnv_1_wacres,
                   record=-1,
                   xlim=[-200, 1200],
                   vmin=0., vmax=1.4,
                   cbar_label='Wave height (m)',
                   filled_contours=True,
                   fig_size=(12, 7),
                   fig_name='img/HM01')


        # Plotting vertical split
        vnv_plot2d('VELOCITY U',
                   res_vnv_3_t3dres,
                   poly=[[500, 0], [500, 200]],
                   record=-1,
                   cbar_label='Velocity U (m/s)',
                   filled_contours=True,
                   fig_size=(12, 7),
                   fig_name='img/resultscoupVert3')

        # Plotting vertical split
        vnv_plot2d('VELOCITY U',
                   res_vnv_3_t3dres,
                   poly=[[0, 100], [1000, 100]],
                   record=-1,
                   cbar_label='Velocity U (m/s)',
                   filled_contours=True,
                   fig_size=(12, 7),
                   fig_name='img/resultscoupVert23')

        # Plotting horizontal split
        vnv_plot2d('VELOCITY U',
                   res_vnv_3_t3dres,
                   plane=0,
                   record=-1,
                   cbar_label='Velocity U (m/s)',
                   filled_contours=True,
                   fig_size=(12, 7),
                   fig_name='img/resultshori3')

        # Plotting WAVE HEIGHT HM0 at -1
        vnv_plot2d('WAVE HEIGHT HM0',
                   res_vnv_3_wacres,
                   record=-1,
                   xlim=[-200, 1200],
                   vmin=0., vmax=1.4,
                   cbar_label='Wave height (m)',
                   filled_contours=True,
                   fig_size=(12, 7),
                   fig_name='img/HM03')
        res_vnv_1_wacres.close()
        res_vnv_1_t3dres.close()
        res_vnv_1_t3dgeo.close()
        res_vnv_3_t3dres.close()
        res_vnv_3_wacres.close()

        # TEL2TOM
        vnv5_t3dgeo = self.get_study_file('vnv_5:T3DGEO')
        bnd5_t3dgeo = self.get_study_file('vnv_5:T3DCLI')
        mesh5_t3dgeo = TelemacFile(vnv5_t3dgeo, bnd_file=bnd5_t3dgeo)
        vnv5_wacgeo = self.get_study_file('vnv_5:WACGEO')
        bnd5_wacgeo = self.get_study_file('vnv_5:WACCLI')
        mesh5_wacgeo = TelemacFile(vnv5_wacgeo, bnd_file=bnd5_wacgeo)

        vnv_5_wacres = self.get_study_file('vnv_5:WACRES')
        res_vnv_5_wacres = TelemacFile(vnv_5_wacres)
#        vnv_5_t3dres = self.get_study_file('vnv_5:T3DRES')
#        res_vnv_5_t3dres = TelemacFile(vnv_5_t3dres)
#        vnv_7_t3dres = self.get_study_file('vnv_7:T3DRES')
#        res_vnv_7_t3dres = TelemacFile(vnv_7_t3dres)
        vnv_7_wacres = self.get_study_file('vnv_7:WACRES')
        res_vnv_7_wacres = TelemacFile(vnv_7_wacres)

        #Plotting mesh
        vnv_plot2d('',
                   mesh5_t3dgeo,
                   annotate_bnd=True,
                   plot_mesh=True,
                   xlim=[-200, 1200],
                   fig_size=(9, 4),
                   fig_name='img/mesh5T3D')

        vnv_plot2d('',
                   mesh5_wacgeo,
                   annotate_bnd=True,
                   plot_mesh=True,
                   fig_size=(9, 4),
                   fig_name='img/mesh5WAC')

        vnv_plot2d('WAVE HEIGHT HM0',
                   res_vnv_5_wacres,
                   record=-1,
                   xlim=[-200, 1200],
                   vmin=0., vmax=1.4,
                   cbar_label='Wave height (m)',
                   filled_contours=True,
                   fig_size=(12, 7),
                   fig_name='img/HM05')

        vnv_plot2d('WAVE HEIGHT HM0',
                   res_vnv_7_wacres,
                   record=-1,
                   xlim=[-200, 1200],
                   vmin=0., vmax=1.4,
                   cbar_label='Wave height (m)',
                   filled_contours=True,
                   fig_size=(12, 7),
                   fig_name='img/HM07')
