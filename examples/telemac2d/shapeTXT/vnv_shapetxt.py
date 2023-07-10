"""
Validation script for thacker (planar surface in a paraboloid)
"""
from vvytel.vnv_study import AbstractVnvStudy
from execution.telemac_cas import TelemacCas, get_dico

class VnvStudy(AbstractVnvStudy):
    """
    Class for verification of thacker (planar surface in a paraboloid)

    """
    def _init(self):
        """
        Defining general parameters
        """
        self.rank = 2
        self.tags = ['telemac2d']

    def _pre(self):
        """
        Defining the studies
        """

        #======================================================================
        # eria run
        self.add_study('in_poly', 'telemac2d', 't2d_thacker.cas')
        # eria run in parallel
        cas = TelemacCas('t2d_thacker.cas', get_dico('telemac2d'))
        cas.set('PARALLEL PROCESSORS', 4)
        self.add_study('in_poly_par', 'telemac2d', 't2d_thacker_par.cas', cas=cas)
        del cas


    def _check_results(self):
        """
        Post-treatment processes
        """
        # Comparison with the last time frame of the reference file.
        self.check_epsilons('in_poly:T2DRES', 'f2d_thacker.slf', eps=[1e-10])       
        self.check_epsilons('in_poly_par:T2DRES', 'f2d_thacker.slf', eps=[1e-10])       
        self.check_epsilons('in_poly:T2DRES', 'in_poly_par:T2DRES', eps=[1e-10])
        
    def _post(self):
        from postel.plot_vnv import vnv_plot2d
        """
        Post-treatment processes
        """
        #======================================================================
        # GET TELEMAC RESULT FILES:
        #
        in_poly_res, _ = self.get_study_res('in_poly:T2DRES')
        
        # Plot 2d bathymetry:
        vnv_plot2d(\
            'BOTTOM',
            in_poly_res,
            record=0,
            filled_contours=True,
            fig_size=(8, 6),
            fig_name="img/t2d_thacker_bathy")

        # Delete results
        in_poly_res.close()
