
"""
Validation script for bosse-t2d
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
        self.rank = 4
        self.tags = ['telemac2d', 'gaia','fv']
        self.listing = True
        self.walltime = '9:00:00'

    def _pre(self):
        """
        Defining the studies
        """

        # Flume_bc scalar mode T2D+GAI
        self.add_study('vnv_1',
                       'telemac2d',
                       't2d_flume_bc.cas')


        # Flume_bc parallel mode T2D+GAI
        cas = TelemacCas('t2d_flume_bc.cas', get_dico('telemac2d'))
        cas.set('PARALLEL PROCESSORS', 4)

        self.add_study('vnv_2',
                       'telemac2d',
                       't2d_flume_bc_par.cas',
                       cas=cas)

        # Multi class scalar
        cas = TelemacCas('t2d_flume_bc.cas', get_dico('telemac2d'))
        sed_cas = TelemacCas('gai_flume_bc.cas', get_dico('gaia'))
        sed_cas.set('CLASSES SEDIMENT DIAMETERS',[2e-4,4e-4])
        sed_cas.set('CLASSES TYPE OF SEDIMENT',['NCO','NCO'])
        sed_cas.set('CLASSES SEDIMENT DENSITY',[2650.,2650.])
        sed_cas.set('CLASSES INITIAL FRACTION',[0.4,0.6])
        sed_cas.write('gai_tmp.cas')
        cas.set('GAIA STEERING FILE','gai_tmp.cas')

        self.add_study('vnv_3',
                       'telemac2d',
                       't2d_flume_bc_multi.cas',
                       cas=cas)

        # Multi class parallel
        cas.set('PARALLEL PROCESSORS', 4)

        self.add_study('vnv_4',
                       'telemac2d',
                       't2d_flume_bc_multi_par.cas',
                       cas=cas)

        # Bedload boundary file scalar
        cas = TelemacCas('t2d_flume_bc.cas', get_dico('telemac2d'))
        sed_cas = TelemacCas('gai_flume_bc.cas', get_dico('gaia'))
        sed_cas.set('BEDLOAD BOUNDARIES FILE','flume_bc.lqd')
        sed_cas.write('gai_tmp.cas')
        cas.set('GAIA STEERING FILE','gai_tmp.cas')

        self.add_study('vnv_5',
                       'telemac2d',
                       't2d_flume_bc_bbf.cas',
                       cas=cas)

        # Bedload boundary file parallel
        cas.set('PARALLEL PROCESSORS', 4)

        self.add_study('vnv_6',
                       'telemac2d',
                       't2d_flume_bc_bbf_par.cas',
                       cas=cas)

        # Bedload boundary file + multiclass scalar
        cas = TelemacCas('t2d_flume_bc.cas', get_dico('telemac2d'))
        sed_cas = TelemacCas('gai_flume_bc.cas', get_dico('gaia'))
        sed_cas.set('BEDLOAD BOUNDARIES FILE','flume_bc.lqd')
        sed_cas.set('CLASSES SEDIMENT DIAMETERS',[2e-4,4e-4])
        sed_cas.set('CLASSES TYPE OF SEDIMENT',['NCO','NCO'])
        sed_cas.set('CLASSES SEDIMENT DENSITY',[2650.,2650.])
        sed_cas.set('CLASSES INITIAL FRACTION',[0.4,0.6])
        sed_cas.write('gai_tmp.cas')
        cas.set('GAIA STEERING FILE','gai_tmp.cas')

        self.add_study('vnv_7',
                       'telemac2d',
                       't2d_flume_bc_bbfmulti.cas',
                       cas=cas)

        # Bedload boundary file + multiclass parallel
        cas.set('PARALLEL PROCESSORS', 4)

        self.add_study('vnv_8',
                       'telemac2d',
                       't2d_flume_bc_bbfmulti_par.cas',
                       cas=cas)

        # Bedload boundary file + multiclass + co and nco scalar
        cas = TelemacCas('t2d_flume_bc.cas', get_dico('telemac2d'))
        sed_cas = TelemacCas('gai_flume_bc.cas', get_dico('gaia'))
        sed_cas.set('CLASSES SEDIMENT DIAMETERS',[6e-5,2e-4,4e-4])
        sed_cas.set('CLASSES TYPE OF SEDIMENT',['CO','NCO','NCO'])
        sed_cas.set('CLASSES SEDIMENT DENSITY',[2650.,2650.,2650.])
        sed_cas.set('CLASSES INITIAL FRACTION',[0.2,0.3,0.5])
        sed_cas.set('PRESCRIBED SUSPENDED SEDIMENTS CONCENTRATION VALUES',[0.,0.])
        sed_cas.set('LAYERS CRITICAL EROSION SHEAR STRESS OF THE MUD',0.)
        sed_cas.set('CLASSES SHIELDS PARAMETERS',[100.,100.,100.])
        sed_cas.write('gai_tmp.cas')
        cas.set('GAIA STEERING FILE','gai_tmp.cas')

        self.add_study('vnv_9',
                       'telemac2d',
                       't2d_flume_bc_coh.cas',
                       cas=cas)

        # Bedload boundary file + multiclass parallel
        cas.set('PARALLEL PROCESSORS', 4)

        self.add_study('vnv_10',
                       'telemac2d',
                       't2d_flume_bc_coh_par.cas',
                       cas=cas)

        # Multi class scalar with set distribution
        cas = TelemacCas('t2d_flume_bc.cas', get_dico('telemac2d'))
        sed_cas = TelemacCas('gai_flume_bc.cas', get_dico('gaia'))
        sed_cas.set('CLASSES SEDIMENT DIAMETERS',[2e-4,4e-4])
        sed_cas.set('CLASSES TYPE OF SEDIMENT',['NCO','NCO'])
        sed_cas.set('CLASSES SEDIMENT DENSITY',[2650.,2650.])
        sed_cas.set('CLASSES INITIAL FRACTION',[0.4,0.6])
        sed_cas.set('CLASSES IMPOSED SOLID DISCHARGES DISTRIBUTION',[0.6,0.4])
        sed_cas.write('gai_tmp.cas')
        cas.set('GAIA STEERING FILE','gai_tmp.cas')

        self.add_study('vnv_11',
                       'telemac2d',
                       't2d_flume_bc_multi_dist.cas',
                       cas=cas)

        # Multi class with set distribution parallel
        cas.set('PARALLEL PROCESSORS', 4)

        self.add_study('vnv_12',
                       'telemac2d',
                       't2d_flume_bc_multi_dist_par.cas',
                       cas=cas)
        del cas



    def _check_results(self):
        """
        Post-treatment processes
        """
        # Comparison with reference file
        self.check_epsilons('vnv_1:GAIRES',
                            'gai_ref_flume_bc.slf',
                            eps=[1.E-6])

        # Comparison seq/par.
        self.check_epsilons('vnv_1:GAIRES',
                            'vnv_2:GAIRES',
                            eps=[1.E-6])

        # Comparison with reference file
        self.check_epsilons('vnv_3:GAIRES',
                            'gai_ref_flume_bc_multi.slf',
                            eps=[1.E-6])

        # Comparison seq/par.
        self.check_epsilons('vnv_3:GAIRES',
                            'vnv_4:GAIRES',
                            eps=[1.E-6])

        # Comparison with reference file
        self.check_epsilons('vnv_5:GAIRES',
                            'gai_ref_flume_bc_bbf.slf',
                            eps=[1.E-6])

        # Comparison seq/par.
        self.check_epsilons('vnv_5:GAIRES',
                            'vnv_6:GAIRES',
                            eps=[1.E-6])

        # Comparison with reference file
        self.check_epsilons('vnv_7:GAIRES',
                            'gai_ref_bc_bbfmulti.slf',
                            eps=[1.E-6])

        # Comparison seq/par.
        self.check_epsilons('vnv_7:GAIRES',
                            'vnv_8:GAIRES',
                            eps=[1.E-6])

        # Comparison with reference file
        self.check_epsilons('vnv_9:GAIRES',
                            'gai_ref_flume_bc_coh.slf',
                            eps=[1.E-6])

        # Comparison seq/par.
        self.check_epsilons('vnv_9:GAIRES',
                            'vnv_10:GAIRES',
                            eps=[1.E-6])

        # Comparison with reference file
        self.check_epsilons('vnv_11:GAIRES',
                            'gai_ref_flume_bc_multi_dist.slf',
                            eps=[1.E-6])

        # Comparison seq/par.
        self.check_epsilons('vnv_11:GAIRES',
                            'vnv_12:GAIRES',
                            eps=[1.E-6])

    def _post(self):
        """
        Post-treatment processes
        """

        from postel.plot_vnv import vnv_plot1d_polylines
        from postel.plot1d import plot1d
        from data_manip.extraction.telemac_file import TelemacFile
        import numpy as np
        from postel.plot_vnv import vnv_plot2d
        import matplotlib.pyplot as plt
        from postel.parser_output import get_latest_output_files
        from postel.parser_output import OutputFileData
        import re


        liste = ['1','5']
        liste2 = ['3','7','11']
        liste3 = ['9']
        flux = []
        mass = []
        times = []

        for i in liste:
            #Extract mass fluxes from the listing
            t_form = re.compile(r'\s*BEDLOAD FLUX BOUNDARY    2', re.I) # Search the time

            cas_file = self.studies['vnv_'+i].steering_file
            file_name = get_latest_output_files(cas_file)
            file_name = file_name[0]
            out_file = OutputFileData(file_name)

            my_grep_t = out_file.get_user_defined_output(t_form)
            my_grep = out_file.get_time_profile()
            mass_flux = []
            mass_flux.append(0.)
            mass_flux.append(0.)
            for j in my_grep_t:
                mass_flux.append(float(j.split()[5]))
                mass_flux.append(float(j.split()[5]))
            mass_flux.append(float(j.split()[5]))
            mass.append(mass_flux)
            times.append(my_grep[1][1])

        for i in liste2:
            #Extract mass fluxes from the listing
            t_form = re.compile(r'\s*BEDLOAD FLUX BOUNDARY    2', re.I) # Search the time

            cas_file = self.studies['vnv_'+i].steering_file
            file_name = get_latest_output_files(cas_file)
            file_name = file_name[0]
            out_file = OutputFileData(file_name)

            my_grep_t = out_file.get_user_defined_output(t_form)
            my_grep = out_file.get_time_profile()
            mass_flux = [[],[],[]]
            mass_flux[0].append(0.)
            mass_flux[0].append(0.)
            mass_flux[1].append(0.)
            mass_flux[1].append(0.)
            mass_flux[2].append(0.)
            mass_flux[2].append(0.)
            cpt = 0
            for j in my_grep_t:
                mass_flux[cpt].append(float(j.split()[5]))
                mass_flux[cpt].append(float(j.split()[5]))
                cpt = cpt + 1
                if cpt == 3:
                    cpt = 0
            mass.append(mass_flux)
            times.append(my_grep[1][1])
            mass_flux[0].append(mass_flux[0][-1])
            mass_flux[1].append(mass_flux[1][-1])
            mass_flux[2].append(mass_flux[2][-1])

        for i in liste3:
            #Extract mass fluxes from the listing
            t_form = re.compile(r'\s*BEDLOAD FLUX BOUNDARY    2', re.I) # Search the time

            cas_file = self.studies['vnv_'+i].steering_file
            file_name = get_latest_output_files(cas_file)
            file_name = file_name[0]
            out_file = OutputFileData(file_name)

            my_grep_t = out_file.get_user_defined_output(t_form)
            my_grep = out_file.get_time_profile()
            mass_flux = [[],[],[],[]]
            mass_flux[0].append(0.)
            mass_flux[0].append(0.)
            mass_flux[1].append(0.)
            mass_flux[1].append(0.)
            mass_flux[2].append(0.)
            mass_flux[2].append(0.)
            mass_flux[3].append(0.)
            mass_flux[3].append(0.)
            cpt = 0
            for j in my_grep_t:
                mass_flux[cpt].append(float(j.split()[5]))
                mass_flux[cpt].append(float(j.split()[5]))
                cpt = cpt + 1
                if cpt == 4:
                    cpt = 0
            mass.append(mass_flux)
            times.append(my_grep[1][1])
            mass_flux[0].append(mass_flux[0][-1])
            mass_flux[1].append(mass_flux[1][-1])
            mass_flux[2].append(mass_flux[2][-1])
            mass_flux[3].append(mass_flux[3][-1])

       #
        vnv1 = self.get_study_file('vnv_1:T2DRES')
        res = TelemacFile(vnv1)

        #Plot the mesh
        vnv_plot2d(\
            '',res,
            plot_mesh=True,
            fig_name='img/mesh',
            fig_size=(10,3),
            x_label='x (m)',y_label='y (m)')

	#Plot the initial elevation
        vnv_plot2d(\
            'BOTTOM',
            res,
            fig_name='img/bottom',
            fig_size=(10,4),
            cbar_label='Bottom elevation (m)',
            filled_contours=True,
            x_label='x (m)',y_label='y (m)')

	#Plot the longitunal profile
        vnv_plot1d_polylines(\
                'BOTTOM',
                res,
                '',
                fig_size=(8, 2),
                record=-1,
                fig_name='img/profile',
                plot_bottom=True)

        #Plot for vnv_1
        fig, ax = plt.subplots(figsize=(10,5))
        plot1d(ax,times[0],mass[0],plot_label='Imposed solid discharge',\
                x_label='Time (s)',y_label='Solid discharges kg/s')
        ax.legend()
        fig_name = 'img/discharge1'
        print(" "*8+'~> Plotting '+fig_name)
        plt.savefig(fig_name)
        plt.close('all')

        #Plot for vnv_5
        fig, ax = plt.subplots(figsize=(10,5))
        plot1d(ax,times[1],mass[1],plot_label='Imposed solid discharge',\
                x_label='Time (s)',y_label='Solid discharges kg/s')
        ax.legend()
        fig_name = 'img/discharge3'
        print(" "*8+'~> Plotting '+fig_name)
        plt.savefig(fig_name)
        plt.close('all')

        #Plot for vnv_3
        fig, ax = plt.subplots(figsize=(10,5))
        plot1d(ax,times[2],mass[2][0],plot_label='Imposed solid discharge Class 1')
        plot1d(ax,times[2],mass[2][1],plot_label='Imposed solid discharge Class 2')
        plot1d(ax,times[2],mass[2][2],plot_label='Imposed solid discharge total',\
                x_label='Time (s)',y_label='Solid discharges kg/s')
        ax.legend()
        fig_name = 'img/discharge2'
        print(" "*8+'~> Plotting '+fig_name)
        plt.savefig(fig_name)
        plt.close('all')

        #Plot for vnv_7
        fig, ax = plt.subplots(figsize=(10,5))
        plot1d(ax,times[3],mass[3][0],plot_label='Imposed solid discharge Class 1')
        plot1d(ax,times[3],mass[3][1],plot_label='Imposed solid discharge Class 2')
        plot1d(ax,times[3],mass[3][2],plot_label='Imposed solid discharge total',\
                x_label='Time (s)',y_label='Solid discharges kg/s')
        ax.legend()
        fig_name = 'img/discharge4'
        print(" "*8+'~> Plotting '+fig_name)
        plt.savefig(fig_name)
        plt.close('all')

        #Plot for vnv_11
        fig, ax = plt.subplots(figsize=(10,5))
        plot1d(ax,times[4],mass[4][0],plot_label='Imposed solid discharge Class 1')
        plot1d(ax,times[4],mass[4][1],plot_label='Imposed solid discharge Class 2')
        plot1d(ax,times[4],mass[4][2],plot_label='Imposed solid discharge total',\
                x_label='Time (s)',y_label='Solid discharges kg/s')
        ax.legend()
        fig_name = 'img/discharge6'
        print(" "*8+'~> Plotting '+fig_name)
        plt.savefig(fig_name)
        plt.close('all')

        #Plot for vnv_9
        fig, ax = plt.subplots(figsize=(10,5))
        plot1d(ax,times[5],mass[5][1],plot_label='Imposed solid discharge Class 1')
        plot1d(ax,times[5],mass[5][2],plot_label='Imposed solid discharge Class 2')
        plot1d(ax,times[5],mass[5][3],plot_label='Imposed solid discharge total',\
                x_label='Time (s)',y_label='Solid discharges kg/s')
        ax.legend()
        fig_name = 'img/discharge5'
        print(" "*8+'~> Plotting '+fig_name)
        plt.savefig(fig_name)
        plt.close('all')
