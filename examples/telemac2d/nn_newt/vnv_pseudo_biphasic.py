
"""
Validation script for pseudo-biphasic
"""
from vvytel.vnv_study import AbstractVnvStudy
from execution.telemac_cas import TelemacCas, get_dico

class VnvStudy(AbstractVnvStudy):
    """
    Class for validation
    """

    def _init(self):
        """
        Defines the general parameter
        """
        self.rank = 2
        self.tags = ['fv','telemac2d']
        self.listing = True

    def _pre(self):
        """
        Defining the studies
        """

        # Pseudo biphasique
        self.add_study('vnv_1',
                       'telemac2d',
                       't2d_pseudo_biphasic.cas')


        # Pseudo biphasique parallel
        cas = TelemacCas('t2d_pseudo_biphasic.cas', get_dico('telemac2d'))
        cas.set('PARALLEL PROCESSORS', 4)

        self.add_study('vnv_2',
                       'telemac2d',
                       't2d_pseudo_biphasic_par.cas',
                       cas=cas)

        del cas

    def _check_results(self):
        """
        Post-treatment processes
        """

        # Comparison with reference file
        self.check_epsilons('vnv_1:T2DRES',
                            'f2d_pseudo_biphasique.slf',
                            eps=[1.E-3])

        # Comparison seq/par.
        self.check_epsilons('vnv_1:T2DRES',
                            'vnv_2:T2DRES',
                            eps=[1.E-3])

    def _post(self):
        """
        Post-treatment processes
        """
        from postel.plot_vnv import vnv_plot2d

        #Get T2D results file
        res, _ = self.get_study_res('vnv_1:T2DRES')

        #Plot the mesh
        vnv_plot2d(\
                '',res,
                plot_mesh=True,
                fig_name='img/mesh-pb',
                fig_size=(5,10),
                x_label='x (m)',y_label='y (m)')

        #Plot the final state
        vnv_plot2d(\
                'WATER DEPTH',
                res,
                record=-1,
                fig_name='img/depth',
                fig_size=(5,10),
                cbar_label='Flow depth (m)',
                filled_contours=True,
                x_label='x (m)',y_label='y (m)')

        vnv_plot2d(\
                'NON-NEWTON MIX',
                res,
                record=-1,
                fig_name='img/concentration',
                fig_size=(5,10),
                cbar_label='Volumic concentration (-)',
                filled_contours=True,
                x_label='x (m)',y_label='y (m)')

        vnv_plot2d(\
                'FLUID DENSITY',
                res,
                record=-1,
                fig_name='img/density',
                fig_size=(5,10),
                cbar_label='Fluid density (kg/m\u00B3)',
                filled_contours=True,
                x_label='x (m)',y_label='y (m)')

        vnv_plot2d(\
                'YIELD STRESS',
                res,
                record=-1,
                fig_name='img/yield-stress',
                fig_size=(5,10),
                cbar_label='Yield stress (Pa)',
                filled_contours=True,
                x_label='x (m)',y_label='y (m)')

        vnv_plot2d(\
                'BINGHM VISCOSITY',
                res,
                record=-1,
                fig_name='img/viscosity',
                fig_size=(5,10),
                cbar_label='Dynamic viscosity (Pa.s)',
                filled_contours=True,
                x_label='x (m)',y_label='y (m)')

