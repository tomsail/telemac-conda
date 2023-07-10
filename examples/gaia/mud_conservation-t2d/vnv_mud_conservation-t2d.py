
"""
Validation script for mud_conservation-t2d
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
        self.tags = ['telemac2d', 'gaia']
        # Forcing listing
        self.listing = True

    def _pre(self):
        """
        Defining the studies
        """

        # Mud conservation T2D+GAI scalar mode
        self.add_study('vnv_mud_cons_scalar',
                       'telemac2d',
                       't2d_mud_cons.cas')


        # Mud conservation T2D+GAI scalar mode
        cas = TelemacCas('t2d_mud_cons.cas', get_dico('telemac2d'))
        cas.set('PARALLEL PROCESSORS', 4)

        self.add_study('vnv_mud_cons_para',
                       'telemac2d',
                       't2d_mud_cons_par.cas',
                       cas=cas)

        del cas



    def _check_results(self):
        """
        Post-treatment processes
        """

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_mud_cons_scalar:GAIRES',
                            'gai_ref_mud_cons-t2d.slf',
                            eps=[1e-7])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_mud_cons_para:GAIRES',
                            'gai_ref_mud_cons-t2d.slf',
                            eps=[1e-7])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_mud_cons_para:GAIRES',
                            'vnv_mud_cons_scalar:GAIRES',
                            eps=[1e-7])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_mud_cons_scalar:T2DRES',
                            'f2d_mud_cons-t2d.slf',
                            eps=[1e-7])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_mud_cons_para:T2DRES',
                            'f2d_mud_cons-t2d.slf',
                            eps=[1e-7])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_mud_cons_para:T2DRES',
                            'vnv_mud_cons_scalar:T2DRES',
                            eps=[1e-7])


    def _post(self):
        """
        Post-treatment processes
        """
        from postel.parser_output import get_latest_output_files,OutputFileData
        from postel.plot_vnv import vnv_plot1d
        from postel.plot_vnv import vnv_plot1d_polylines, vnv_plot2d, vnv_plot3d
        from postel.plot1d import plot1d
        import matplotlib.pyplot as plt
        import numpy as np
        # Getting the name of the result file of the sequential run
        res_file = self.get_study_file('vnv_mud_cons_scalar:T2DRES')
        bnd_file = self.get_study_file('vnv_mud_cons_scalar:T2DCLI')
        res_seq = TelemacFile(res_file, bnd_file=bnd_file)

        # Plotting water depth over polyline for time step 0
        vnv_plot1d_polylines(\
            'WATER DEPTH',
            res_seq,
            legend_labels='initial elevation',
            fig_size=(8, 2),
            record=0,
            poly=[[0., 10.], [20., 10.]],
            poly_number=[50],
            fig_name='img/InitialElevation')

        # Plotting water depth over polyline for time step 0
        vnv_plot1d_polylines(\
            'COH SEDIMENT1',
            res_seq,
            legend_labels='initial concentration',
            fig_size=(8, 2),
            record=0,
            poly=[[0., 10.], [20., 10.]],
            poly_number=[50],
            fig_name='img/InitialConcentration')
        # plotting mesh
        vnv_plot2d(\
            '',
            res_seq,
            record=0,
            fig_size=(5, 5),
            fig_name="img/Mesh",
            plot_mesh=True)

        res_seq.close()
        cas_file = self.studies['vnv_mud_cons_scalar'].steering_file
        print("cas_file: ", cas_file)

        file_name = get_latest_output_files(cas_file)

        file_name = file_name[0]

        out_file = OutputFileData(file_name)

        mass = out_file.get_sediment_mass_profile()

        tmp_iterations, tmp_times = out_file.get_time_profile()
        _, iterations = tmp_iterations
        _, times = tmp_times

        for sed_class in mass:
            for ttype in ['Total', 'Lost','Re_ial']:

                title = "{}_mass_{:02d}".format(ttype.lower(), sed_class)

                vnv_plot1d(times[2::2], mass[sed_class][ttype.lower()],
                           [title],
                           x_label="time (s)", y_label="{} mass (kg)".format(ttype),
                           fig_name="img/"+title+".png")


