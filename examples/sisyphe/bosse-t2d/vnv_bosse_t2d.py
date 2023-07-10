
"""
Validation script for bosse-t2d
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
        self.tags = ['telemac2d', 'sisyphe']

    def _pre(self):
        """
        Defining the studies
        """

        # bosse-t2d scalar mode T2D+SIS
        self.add_study('vnv_1',
                       'telemac2d',
                       't2d_bosse-t2d.cas')


        # bosse-t2d parallel mode T2D+SIS
        cas = TelemacCas('t2d_bosse-t2d.cas', get_dico('telemac2d'))
        cas.set('PARALLEL PROCESSORS', 4)

        self.add_study('vnv_2',
                       'telemac2d',
                       't2d_bosse-t2d_par.cas',
                       cas=cas)

        del cas



    def _check_results(self):
        """
        Post-treatment processes
        """

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_1:SISRES',
                            'fis_bosse-t2d.slf',
                            eps=[4.E-5])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_2:SISRES',
                            'fis_bosse-t2d.slf',
                            eps=[4.E-5])

        # Comparison between sequential and parallel run.
        self.check_epsilons('vnv_1:SISRES',
                            'vnv_2:SISRES',
                            eps=[4.E-5])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_1:T2DRES',
                            'f2d_bosse-t2d.slf',
                            eps=[2.E-5])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_2:T2DRES',
                            'f2d_bosse-t2d.slf',
                            eps=[2.E-5])

        # Comparison between sequential and parallel run.
        self.check_epsilons('vnv_1:T2DRES',
                            'vnv_2:T2DRES',
                            eps=[2.E-5])


    def _post(self):
        """
        Post-treatment processes
        """
        from postel.plot_vnv import vnv_plot1d_polylines, vnv_plot2d
                # Getting files
        vnv_1_sisres = self.get_study_file('vnv_1:SISRES')
        res_vnv_1_sisres = TelemacFile(vnv_1_sisres)

        # Plotting analytic bottom and bottom on ()
        vnv_plot1d_polylines('BOTTOM',
                             res_vnv_1_sisres,
                             'BOTTOM',
                             poly=[[0, 0], [16, 0]],
                             ref_name='ANALYTIC BOTTOM',
                             ref_label='ANALYTIC BOTTOM',
                             record=-1,
                             fig_name="img/Elevation",
                             fig_size=(12, 5))

        # Plotting ANALYTIC BOTTOM at -1
        vnv_plot2d('ANALYTIC BOTTOM',
                   res_vnv_1_sisres,
                   record=-1,
                   filled_contours=True,
                   fig_size=(12, 5),
                   fig_name='img/bed')

        # Closing files
        res_vnv_1_sisres.close()
