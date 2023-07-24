#!/usr/bin/python3
"""
Contains plot functions wrappers for vnv
"""
import numpy as np
import matplotlib.pyplot as plt

from data_manip.extraction.telemac_file import linspace_poly
from data_manip.computation.polyline_integrals import compute_segments_tangents
from data_manip.computation.triangulation import triangulation_from_data
from postel.plot2d import \
        plot2d_triangle_mesh, plot2d_annotate_bnd, plot2d_annotate_liq_bnd, \
        plot2d_scalar_map, \
        plot2d_scalar_filled_contour, plot2d_scalar_contour, \
        plot2d_streamlines, plot2d_vectors, mask_triangles, set_extrema
from postel.plot3d import plot3d_scalar_map
from postel.deco_vnv import decoVNV, decoVNV_bar, decoVNV_1d, decoVNV_markers
from utils.exceptions import TelemacException

# _____             ________________________________________________
# ____/ BAR PLOTS  /_______________________________________________/
#


def get_data(res, var_name, record,
             zslice=None, poly=None, poly_number=None, plane=0):
    """
    Macro function to extract 2d info from a telemac file

    Select the right function depending on zslice, poly and plane number.

    @param res (TelemacFile) Structure of telemac file from which to extract
    @param var_name (str) Name of the variable to extract
    @param record (int) Record to extract
    @param zslice (float) value for horizontal slice at z=zslice
    @param poly (list) Polyline for vertical slice
    @param poly_number (list) Polygon discretization
    @param plane (int) Extraction on a specific plane

    @returns (matplotlib.triangulation, np.array) The data mesh, the data
    """
    ndim = res.get_mesh_dimension()

    if zslice is not None and poly is None:
        # Get data on horizontal slice plane
        scalar = res.get_data_on_horizontal_slice(var_name, record, zslice)
        return res.tri, scalar

    if poly is not None and zslice is None:
        # Get data on vertical slice plane (from polyline)
        namez = res.varnames[0]
        if poly_number is None:
            poly_number = res.discretize_polyline(poly)
        _, abs_curv, values_z = \
            res.get_data_on_vertical_plane(
                namez, record, poly, poly_number)
        _, _, scalar = \
            res.get_data_on_vertical_plane(
                var_name, record, poly, poly_number)
        mesh = triangulation_from_data(abs_curv, values_z)
        return mesh, scalar.flatten()

    if poly is None and zslice is None:
        if ndim == 3:
            # Get data on plane (3D)
            scalar = res.get_data_on_horizontal_plane(var_name, record, plane)

        elif ndim == 2:
            # Get data on mesh (2D)
            scalar = res.get_data_value(var_name, record)
        return res.tri, scalar

    raise TelemacException("Cannot extract from both horizontal and " +
                           "vertical slice planes")


def vnv_plotbar(
        data, fig_size=None,
        fig_name='', fig_title=None,
        x_labels='', y_label='', ylim=None,
        legend_labels=None, split_bars=True,
        bar_width=.75,
        y_scale='linear',
        annotate=False,
        annotate_format='e',
        annotate_threshold=None,
        **kwargs):
    """
    Plot data with bars

    @param data (list) list of data to plot
    @param fig_size (list) figure size
    @param fig_name (str) file name of the figure saved
    @param fig_title (str) figure title
    @param x_labels (list) list of ticks labels on x axis
    @param y_label (str) label of y axis
    @param ylim (list) y limits
    @param legend_labels (list) list of legends labels
    @param bar_width (float) bar width
    @param split_bars (bool) if False, bars are overlapped
    @param y_scale (str) y scaling method (default: 'linear')
    @param annotate (bool) if True annotate values on top of bars
    @param annotate_format (str) format of annotation (default: 'e')
    @param annotate_threshold (float) threshold for annotation (default: None)
    @param kwargs (dict) Arguments passed to plt.bar
    """
    # plot initialization
    plt.style.use('default')
    plt.rcParams.update(decoVNV)
    plt.rcParams.update(decoVNV_bar)
    fig, ax = plt.subplots(1, 1, figsize=fig_size)

    # number of pos and bars per pos
    if isinstance(data[0], list):
        npos = len(data[0])
        nbar = len(data)
    else:
        npos = len(data)
        nbar = 1

    # plot
    pos = np.arange(npos, dtype='float64')
    barps = []
    for n in range(nbar):
        # split bars on position
        if split_bars:
            d_x = bar_width/nbar
            if nbar % 2 == 0:
                posn = pos + float(n - nbar//2)*d_x + d_x/2
            else:
                posn = pos + float(n - nbar//2)*d_x
        # plot all bars at same position
        else:
            posn = pos
            d_x = bar_width

        barp = plt.bar(posn, data[n], d_x, **kwargs)
        barps.append(barp[0])

        # annotate bars with corresponding values
        if annotate:
            for x, y in zip(posn, data[n]):
                if annotate_format == 'e':
                    label = "{:.2e}".format(y)
                if annotate_format == 'f':
                    label = "{:.2f}".format(y)
                if annotate_format == 'f1':
                    label = "{:.1f}".format(y)
                if annotate_format == 'f0':
                    label = "{:.0f}".format(y)
                if annotate_threshold is not None:
                    if y <= annotate_threshold:
                        plt.annotate(
                            label, (x, y), textcoords="offset points",
                            fontsize=10, xytext=(0, 5), ha='center')
                else:
                    plt.annotate(
                        label, (x, y), textcoords="offset points",
                        fontsize=10, xytext=(0, 5), ha='center')

    # Setting y scale
    plt.yscale(y_scale)
    if y_scale == 'log':
        plt.grid(b=True, which='major', color='grey', linestyle='--')
        plt.grid(b=True, which='minor', color='grey', linestyle=':')

    # plot options
    if ylim is not None:
        ax.set_ylim(ylim[0], ylim[1])

    # labels
    if y_label != '':
        plt.ylabel(y_label)
    if x_labels != '':
        plt.xticks(pos, x_labels)
    if legend_labels is not None:
        plt.legend((barps), legend_labels)

    if fig_title is not None:
        ax.set_title(fig_title)

    # save figure:
    if fig_name != '':
        print(" "*8+"~> Plotting {}".format(fig_name))
        fig.savefig(fig_name)
    else:
        plt.show()

    # Close figure:
    plt.close('all')


def vnv_plotbar_cpu_times(
        action_time, fig_size=None,
        fig_name='',
        **kwargs):
    """
    Plot CPU times of cases run in vnv_study

    @param action_time (dict) vnv_study cases action times
    @param fig_size (list) figure size
    @param fig_name (str) file name of the figure saved
    @param kwargs (dict) Argument passed to plt.bar
    """
    # plot initialization
    plt.style.use('default')
    plt.rcParams.update(decoVNV)
    plt.rcParams.update(decoVNV_bar)
    fig, _ = plt.subplots(1, 1, figsize=fig_size)
    fig_legend = ('Par', 'Seq')
    bar_width = 0.35

    # retreive CPU times from vnv_study.action_time
    seq_cases = []
    seq_times = []
    for key, item in action_time.items():
        splitted_key = key.split('_')
        if len(splitted_key) > 1:
            if splitted_key[1] == 'seq':
                seq_cases.append(key.split('_')[0].upper())
                seq_times.append(item[1])

    par_cases = seq_cases
    par_times = [0.0 for i in range(len(par_cases))]
    for key, item in action_time.items():
        splitted_key = key.split('_')
        if len(splitted_key) > 1:
            if splitted_key[1] == 'par':
                name = key.split('_')[0].upper()
                idx = par_cases.index(name)
                par_times[idx] = item[1]

    if len(seq_cases) == 0 or len(par_cases) == 0:
        print("Doing nothing did not found any times")
        return

    # plot
    pos = np.arange(len(seq_cases))
    p_2 = plt.bar(pos-bar_width/2., seq_times, bar_width, **kwargs)
    p_1 = plt.bar(pos+bar_width/2., par_times, bar_width, **kwargs)

    # labels
    plt.ylabel('CPU times (s)')
    plt.xticks(pos, seq_cases)
    plt.legend((p_1[0], p_2[0]), fig_legend)

    # save figure:
    if fig_name != '':
        print(" "*8+"~> Plotting {}".format(fig_name))
        fig.savefig(fig_name)
    else:
        plt.show()

    # Close figure:
    plt.close('all')

# _____            _________________________________________________
# ____/ 1D PLOTS  /________________________________________________/
#


def vnv_plot1d(
        times, data, legend_labels, fig_size=(10, 4),
        fig_name='', fig_title=None,
        ref=None, ref_label=None,
        xlim=None, ylim=None,
        y_factor=1.0, x_factor=1.0,
        x_label=' ', y_label=' ',
        relative=False,
        markers=False,
        **kwargs):
    """
    Generic plot 1D (for timeseries or any data)

    @param times (np.array or list) times abscissa
    @param data (np.array or list) data to plot
    @param legend_labels (str or list) label of each result
    @param fig_size (list) figure size
    @param fig_name (str) file name of the figure saved
    @param fig_title (str) Title of the figure
    @param ref (str) reference variables to plot
    @param ref_label (str) label of the reference
    @param xlim (list) x axis limits
    @param ylim (list) y axis limits
    @param y_factor (float) variable adim factor (default: 1.0)
    @param x_factor (float) abscissa adim factor (default: 1.0)
    @param x_label (str) x axis label
    @param y_label (str) y axis label
    @param relative (bool) plot data(t)/data(t=0)
    @param markers (bool) plot markers
    @param kwargs (dict) Argument passed to plot
    """
    # plot initialization
    plt.style.use('default')
    plt.rcParams.update(decoVNV)
    plt.rcParams.update(decoVNV_1d)
    if markers:
        plt.rcParams.update(decoVNV_markers)
    fig, ax = plt.subplots(1, 1, figsize=fig_size)

    # plot reference
    if ref is not None:
        if isinstance(ref, list):
            assert len(times) == len(ref)
            for idx, item in enumerate(ref):
                item *= y_factor
                if relative:
                    item /= item[0]
                ax.plot(times[idx]*x_factor, item, label=legend_labels[idx],
                        color='r', ls='--', marker=',')
        else:
            ref *= y_factor
            if relative:
                ref /= ref[0]
            ax.plot(times*x_factor, ref, label=legend_labels,
                    color='r', ls='--', marker=',')

    # plot res
    if isinstance(data, list):
        assert len(times) == len(data)
        for idx, item in enumerate(data):
            item *= y_factor
            if relative:
                item /= item[0]
            ax.plot(times[idx]*x_factor, item, label=legend_labels[idx],
                    **kwargs)
    else:
        data *= y_factor
        if relative:
            data /= data[0]
        ax.plot(times*x_factor, data, label=legend_labels, **kwargs)

    # plot options
    if xlim is not None:
        ax.set_xlim(xlim[0], xlim[1])
    if ylim is not None:
        ax.set_ylim(ylim[0], ylim[1])

    # labels
    ax.legend()
    plt.xlabel(x_label)
    plt.ylabel(y_label)

    if fig_title is not None:
        ax.set_title(fig_title)

    # save figure:
    if fig_name != '':
        print(" "*8+"~> Plotting {}".format(fig_name))
        fig.savefig(fig_name)
    else:
        plt.show()

    # Close figure:
    plt.close('all')


def vnv_plot1d_history(
        var_name, res,
        legend_labels='', fig_size=None, fig_title=None,
        points=None, nodes=None,
        points_labels=None, nodes_labels=None,
        ref_name=None, ref_file=None, ref_label='analytic',
        fig_name='',
        xlim=None, ylim=None,
        y_factor=1.0, x_factor=1.0,
        x_label=' ', y_label=' ',
        markers=False,
        **kwargs):
    """
    Plot 1D history from TelemacFile results on any points or nodes list

    @param var_name (str) name of the variable to plot
    @param res (TelemacFile of list of TelemacFile) telemac result
    @param legend_labels (str or list) label of each result
    @param fig_size (list) figure size
    @param fig_title (string) Title of the figure
    @param points (list) list of numpy.array containing points of extraction
    @param points_labels (list) legend labels of points
    @param nodes (list) list of nodes to extract
    @param nodes_labels (list) legend labels of nodes
    @param ref_name (str) name of the reference variable (in first res)
    @param ref_file (str) name of the file containing reference values
    @param ref_label (str) label of the reference
    @param fig_name (str) file name of the figure saved
    @param xlim (list) x axis limits
    @param ylim (list) y axis limits
    @param y_factor (float) variable adim factor (default: 1.0)
    @param x_factor (float) abscissa adim factor (default: 1.0)
    @param x_label (str) x axis label
    @param y_label (str) y axis label
    @param markers (bool) plot markers
    @param kwargs (dict) Arguments passed to plot
    """
    if nodes is None and points is None:
        raise TelemacException("Either points or nodes must be given")

    # plot initialization
    plt.style.use('default')
    plt.rcParams.update(decoVNV)
    plt.rcParams.update(decoVNV_1d)
    if markers:
        plt.rcParams.update(decoVNV_markers)
    fig, ax = plt.subplots(1, 1, figsize=fig_size)

    # default parameters
    if isinstance(res, list):
        res0 = res[0]
    else:
        res0 = res

    # plot reference from variable in res
    if ref_name is not None:
        assert ref_file is None

        if points is not None:
            ref_data = res0.get_timeseries_on_points(ref_name, points)
            for i, _ in enumerate(points):
                ax.plot(res0.times*x_factor, ref_data[i, :]*y_factor,
                        label=ref_label, color='r', ls='--', marker=',')

        if nodes is not None:
            ref_data = res0.get_timeseries_on_nodes(ref_name, nodes)
            for i, _ in enumerate(nodes):
                ax.plot(res0.times*x_factor, ref_data[i, :]*y_factor,
                        label=ref_label, color='r', ls='--', marker=',')

    # plot reference from file
    if ref_file is not None:
        assert ref_name is None
        ref_data = np.loadtxt(ref_file)
        ax.plot(ref_data[:, 0]*x_factor, ref_data[:, 1]*y_factor,
                label=ref_label, color='r', ls='--', marker=',')

    # plot res
    if isinstance(res, list):
        for idx, res_item in enumerate(res):
            if points is not None:
                if points_labels is None:
                    points_labels = \
                            ["Point {}".format(point) for point in points]
                data = res_item.get_timeseries_on_points(var_name, points)
                for i, _ in enumerate(points):
                    ax.plot(res_item.times*x_factor, data[i, :]*y_factor,
                            label=legend_labels[idx] + ' ' + points_labels[i],
                            **kwargs)
            if nodes is not None:
                if nodes_labels is None:
                    nodes_labels = ["Node {}".format(node) for node in nodes]
                data = res_item.get_timeseries_on_nodes(var_name, nodes)
                for i, _ in enumerate(nodes):
                    ax.plot(res_item.times*x_factor, data[i, :]*y_factor,
                            label=legend_labels[idx] + ' ' + nodes_labels[i],
                            **kwargs)
    else:
        if points is not None:
            if points_labels is None:
                points_labels = ["Point {}".format(point) for point in points]
            data = res.get_timeseries_on_points(var_name, points)
            for i, _ in enumerate(points):
                ax.plot(res.times*x_factor, data[i, :]*y_factor,
                        label=legend_labels + ' ' + points_labels[i],
                        **kwargs)
        if nodes is not None:
            if nodes_labels is None:
                nodes_labels = ["Node {}".format(node) for node in nodes]
            data = res.get_timeseries_on_nodes(var_name, nodes)
            for i, _ in enumerate(nodes):
                ax.plot(res.times*x_factor, data[i, :]*y_factor,
                        label=legend_labels + ' ' + nodes_labels[i],
                        **kwargs)

    # plot options
    if xlim is not None:
        ax.set_xlim(xlim[0], xlim[1])
    if ylim is not None:
        ax.set_ylim(ylim[0], ylim[1])

    # labels
    ax.legend()
    plt.xlabel(x_label)
    plt.ylabel(y_label)

    if fig_title is not None:
        ax.set_title(fig_title)

    # save figure:
    if fig_name != '':
        print(" "*8+"~> Plotting {}".format(fig_name))
        fig.savefig(fig_name)
    else:
        plt.show()

    # Close figure:
    plt.close('all')


def vnv_plot1d_polylines(
        var_name, res,
        legend_labels='', fig_size=None, fig_title=None,
        ref_name=None, ref_file=None, ref_data=None,
        ref_label='analytic',
        poly=None, poly_number=None, record=0, time=None,
        fig_name='',
        xlim=None, ylim=None,
        y_factor=1.0, x_factor=1.0,
        x_label='x (m)', y_label='z (m)',
        plot_bottom=False, bottom_label='bottom',
        markers=False,
        **kwargs):
    """
    Plot along a polyline at different times
    or for multiple TelemacFile results

    @param var_name (str) name of the variable to plot
    @param res (TelemacFile of list of TelemacFile) telemac result
    @param legend_labels (str or list) label of each result *
    @param fig_size (list) figure size
    @param fig_title (list) figure title
    @param ref_name (str) name of the reference variable (in first res)
    @param ref_data (np.array) numpy array containing reference values
    @param ref_file (str) name of the file containing reference values
    @param ref_label (str) label of the reference
    @param poly (list) list of points defining the polyline
    @param poly_number (list) list of number of discretized points
    @param time (str) If >= 0.0 will get nearest record to that time (This
    overwrites record)
    @param record (int) record to plot
    @param time (float) time to plot
    @param fig_name (str) file name of the figure saved
    @param xlim (list) x axis limits
    @param ylim (list) y axis limits
    @param x_factor (float) abscissa adim factor (default: 1.0)
    @param y_factor (float) variable adim factor (default: 1.0)
    @param x_label (str) x axis label
    @param y_label (str) y axis label
    @param plot_bottom (bool) plot filled bathymetrie
    @param bottom_label (str) legend name of the bottom
    @param markers (bool) plot markers
    @param kwargs (dict) Arguments passed to plot
    """
    # plot initialization
    plt.style.use('default')
    plt.rcParams.update(decoVNV)
    plt.rcParams.update(decoVNV_1d)
    if markers:
        plt.rcParams.update(decoVNV_markers)

    fig, ax = plt.subplots(1, 1, figsize=fig_size)

    # default parameters
    if isinstance(res, list):
        res0 = res[0]
    else:
        res0 = res

    # default poly
    if poly is None:
        x, y = res0.tri.x, res0.tri.y
        y0 = 0.5*(np.max(y)+np.min(y))
        poly = [[np.min(x), y0], [np.max(x), y0]]
        poly_number = [500]
    else:
        if poly_number is None:
            poly_number = res0.discretize_polyline(poly)

    # Set record/time
    if time is not None:
        if isinstance(time, list):
            record = [res0.get_closest_record(time[i])
                      for i in range(len(time))]
        else:
            record = res0.get_closest_record(time)
    else:
        if isinstance(record, list):
            time = [res0.times[record[i]] for i in range(len(record))]
        else:
            time = res0.times[record]

    # check record
    if isinstance(record, list):
        if isinstance(res, list):
            raise TelemacException("select only one result file"
                                   + "to plot multiple records")

    # plot initialization
    plt.style.use('default')
    plt.rcParams.update(decoVNV)
    plt.rcParams.update(decoVNV_1d)
    if markers:
        plt.rcParams.update(decoVNV_markers)

    fig, ax = plt.subplots(1, 1, figsize=fig_size)

    # plot bathymetry
    if plot_bottom:
        if 'BOTTOM' in res0.varnames:
            _, abs_curv, bot_polylines = res0.get_timeseries_on_polyline(
                'BOTTOM', poly, poly_number)
        elif 'FOND' in res0.varnames:
            _, abs_curv, bot_polylines = res0.get_timeseries_on_polyline(
                'FOND', poly, poly_number)
        else:
            raise TelemacException("Need BOTTOM variable to plot bottom")
        ax.plot(abs_curv*x_factor, bot_polylines[:, 0]*y_factor,
                label=bottom_label, color='k', ls='-', lw=0.3, marker='')
        y_c = ax.get_ylim()
        ax.fill_between(abs_curv, y_c[0], bot_polylines[:, 0]*y_factor,
                        facecolor='k', alpha=0.15)

    # plot reference from variable in res
    if ref_name is not None:
        assert ref_data is None
        assert ref_file is None
        _, abs_curv, ana_polylines = res0.get_timeseries_on_polyline(
            ref_name, poly, poly_number)
        ax.plot(abs_curv*x_factor,
                ana_polylines[:, record]*y_factor,
                label=ref_label, color='r', ls='--', marker=',')

    # plot reference from array
    if ref_data is not None:
        assert ref_name is None
        assert ref_file is None
        ax.plot(ref_data[:, 0]*x_factor, ref_data[:, 1]*y_factor,
                label=ref_label, color='r', ls='--', marker=',')

    # plot reference from file
    if ref_file is not None:
        assert ref_data is None
        assert ref_name is None
        ref_data = np.loadtxt(ref_file)
        ax.plot(ref_data[:, 0]*x_factor, ref_data[:, 1]*y_factor,
                label=ref_label, color='r', ls='--', marker=',')

    # plot res
    if isinstance(res, list):
        for idx, res_item in enumerate(res):
            if isinstance(var_name, list):
                _, abs_curv, char_polylines = \
                        res_item.get_timeseries_on_polyline(var_name[idx],
                                                            poly, poly_number)
                ax.plot(abs_curv*x_factor, char_polylines[:, record]*y_factor,
                        label=legend_labels[idx], **kwargs)
            else:
                _, abs_curv, char_polylines = \
                        res_item.get_timeseries_on_polyline(var_name, poly,
                                                            poly_number)
                ax.plot(abs_curv*x_factor, char_polylines[:, record]*y_factor,
                        label=legend_labels[idx], **kwargs)

    elif isinstance(record, (list, range)):
        for idx, rec in enumerate(record):
            _, abs_curv, char_polylines = res.get_timeseries_on_polyline(
                var_name, poly, poly_number)
            legend_label = 't={} s'.format(res.get_data_time(rec))
            ax.plot(abs_curv*x_factor, char_polylines[:, rec]*y_factor,
                    label=legend_label, **kwargs)
    else:
        _, abs_curv, char_polylines = \
                res.get_timeseries_on_polyline(var_name, poly, poly_number)
        ax.plot(abs_curv*x_factor, char_polylines[:, record]*y_factor,
                label=legend_labels, **kwargs)

    # plot options
    if xlim is not None:
        ax.set_xlim(xlim[0], xlim[1])
    if ylim is not None:
        ax.set_ylim(ylim[0], ylim[1])

    # labels
    ax.legend()
    plt.xlabel(x_label)
    plt.ylabel(y_label)

    if fig_title is not None:
        ax.set_title(fig_title)

    # save figure:
    if fig_name != '':
        print(" "*8+"~> Plotting {}".format(fig_name))
        fig.savefig(fig_name)
    else:
        plt.show()

    # Close figure:
    plt.close('all')


def vnv_plot1d_convergence(
        abscissa, data, fig_size=None,
        fig_name='', fig_title=None, legend_labels=None,
        x_label='', y_label='',
        x_scale='log', y_scale='log',
        y_relative=True, x_relative=True,
        markers=True,
        fancy_grid=True,
        plot_firstorder_slope=True,
        plot_secondorder_slope=True,
        reference_data=None,
        reference_labels=None,
        reference_colors=None):
    """
    Plot 1d of errors vs mesh refinment

    @param abscissa (list) list of array abscissa to plot (mesh sizes)
    @param data (list) list of array data to plot (errors)
    @param fig_size (list) figure size
    @param fig_name (str) file name of the figure saved
    @param fig_title (str) Title of the figure
    @param legend_labels (list) list of legends labels
    @param x_label (str) label of x axis
    @param y_label (str) label of y axis
    @param x_scale (str) x scaling method (default: 'log')
    @param y_scale (str) y scaling method (default: 'log')
    @param x_relative (bool) divide x by x[0] (default: True)
    @param y_relative (bool) divide y by y[0] (default: True)
    @param plot_firstorder_slope (bool) plot 1st order slope (default: True)
    @param plot_secondorder_slope (bool) plot 2nd order slope (default: False)
    @param markers (bool) Display markers
    @param fancy_grid (bool) Display fancy grid
    @param reference_data (list/np.array) data of reference solution
    @param reference_labels (list) Labels for reference data
    @param reference_colors (list) Colors for reference data
    """
    # plot initialization
    plt.style.use('default')
    plt.rcParams.update(decoVNV)
    plt.rcParams.update(decoVNV_1d)
    if markers:
        plt.rcParams.update(decoVNV_markers)
    fig, ax = plt.subplots(1, 1, figsize=fig_size)

    # number of points on plot
    nvs = len(abscissa)
    if x_relative:
        abscissa = [abscissa[i]/abscissa[0] for i in range(nvs)]

    # Plot reference slope:
    if plot_firstorder_slope:
        ref_val_1 = [1./abscissa[i] for i in range(nvs)]
        ax.plot(abscissa, ref_val_1,
                label='order 1', color='r', ls='--', lw=0.5, marker='')

    if plot_secondorder_slope:
        ref_val_2 = [1./abscissa[i]**2 for i in range(nvs)]
        ax.plot(abscissa, ref_val_2,
                label='order 2', color='b', ls='--', lw=0.5, marker='')

    # Plot reference data:
    if reference_data is not None:
        if isinstance(reference_data, list):
            for idx, values in enumerate(reference_data):
                if y_relative:
                    values = [values[i]/values[0]
                              for i in range(nvs)]
                ax.plot(abscissa, values,
                        label=reference_labels[idx],
                        color=reference_colors[idx],
                        ls='--', lw=0.5, marker='')
        else:
            if y_relative:
                reference_data = [reference_data[i]/reference_data[0]
                                  for i in range(nvs)]
            ax.plot(abscissa, reference_data,
                    label=reference_labels,
                    color=reference_colors,
                    ls='--', lw=0.5, marker='')

    # Plot data:
    if isinstance(data, list):
        for idx, values in enumerate(data):
            if y_relative:
                values = [values[i]/values[0] for i in range(nvs)]

            ax.plot(abscissa, values, label=legend_labels[idx])
    else:
        if y_relative:
            data = [data[i]/data[0] for i in range(nvs)]

        ax.plot(abscissa, data, label=legend_labels)

    # Fancy grid
    if fancy_grid:
        plt.grid(b=True, which='major', color='grey', linestyle='--')
        plt.grid(b=True, which='minor', color='grey', linestyle=':')

    # Setting x,y scale
    plt.xscale(x_scale)
    plt.yscale(y_scale)

    # labels
    if y_label != '':
        plt.ylabel(y_label)
    if x_label != '':
        plt.xlabel(x_label)
    if legend_labels is not None:
        plt.legend()
    if fig_title is not None:
        ax.set_title(fig_title)

    # save figure:
    if fig_name != '':
        print(" "*8+"~> Plotting {}".format(fig_name))
        fig.savefig(fig_name)
    else:
        plt.show()

    # Close figure:
    plt.close('all')

# _____            _________________________________________________
# ____/ 2D PLOTS  /________________________________________________/
#


def vnv_plot2d(
        var_name, res, record=0, time=None, adim_factor=1.0,
        fig_name='', fig_size=None, fig_title=None,
        poly=None, poly_number=None, zslice=None, plane=0,
        var_type='scalar', var_factor=1.0,
        vect_name='VELOCITY', vect_factor=1.0,
        x_factor=1.0, y_factor=1.0,
        xlim=None, ylim=None, aspect_ratio='auto',
        x_label='x (m)', y_label='y (m)',
        vmin=None, vmax=None, nv=None,
        cmap_name='jet', cbar=True,
        cbar_ticks=None, cbar_properties=None,
        cbar_ax=None, cbar_cax=None,
        cbar_label='', cbar_priority='scalar',
        cbar_autoextend=False, cbar_extend='neither',
        plot_mesh=False, plot_only_dry_mesh=False,
        annotate_bnd=False,
        annotate_liq_bnd=False, annotate_time=False,
        mask_tidal_flats=False, tidal_flats_threshold=0.005,
        scalar_map=False, filled_contours=False,
        contours=False, colored_contours=False,
        streamlines=False, colored_streamlines=False, streamlines_density=4,
        vectors=False, colored_vectors=False,
        vectors_scale=20, vectors_normalize=False,
        grid_resolution=[20, 20],
        bathy_contours=False,
        **kwargs):
    """
    Generic multi-layer plot 2d

    @param var_name (str) name of the variable to plot
    @param res (TelemacFile) telemac result
    @param time (float) If given will get nearest record to that time (This
    overwrites record)
    @param record (int) record to plot
    @param adim_factor (float) Adimensional factor by which data is multiplied
    @param fig_name (str) file name of the figure saved
    @param fig_size (list) figure size
    @param fig_title (list) title of the figure
    @param poly (list) list of points defining the polyline
    @param poly_number (list) list of number of discretized points
    @param zslice (float) Horzontal slice value
    @param plane (int) index of the plane to plot for 3D results
    @param var_type (str) type of the variable: scalar, vector or vector_3d
    (default: 'scalar') if var_type is 'vector' or 'vector_2d' plots the
    2d norm (x,y). If var_type is 'vector_3d' plots the 3d norm (x,y,z)
    @param var_factor (float) variable adim factor (default: 1.0)
    @param vect_name (float) name of the vector for vectors and streamlines
    plots (default: 'VELOCITY')
    @param vect_factor (float) vector adim factor (default: 1.0)
    @param x_factor (float) x axis adim factor (default: 1.0)
    @param y_factor (float) y axis adim factor (default: 1.0)
    @param xlim (list) x axis limits
    @param ylim (list) y axis limits
    @param aspect_ratio (string) matplotlib aspect_ratio
    @param x_label (str) x axis label
    @param y_label (str) y axis label
    @param vmin (float) Minimal value of data to plot
    @param vmax (float) Maximal value of data to plot
    @param nv (integer) Number of sample for colorbar range
    @param cmap_name (str) name of the scalar map
    @param cbar (bool) trigger for colorbar plot
    @param cbar_ticks (list) list of values where to show color bar ticks
    @param cbar_ax (Axes) Parent axes from which space for a new colorbar
    axes will be stolen. If a list of axes is given they will all be resized
    to make room for the colorbar axes.
    @param cbar_cax (Axes) Axes into which the colorbar will be drawn.
    @param cbar_properties (dict) list additional properties of the colorbar
    @param cbar_label (str) name to show on scalar colorbar
    @param cbar_priority (str) defines which cbar to plot ('scalar', 'vector'
    or 'contours')
    @param cbar_autoextend (bool) Extend bar in auto
    @param cbar_extend (string) matplotlib cbar extend method
    @param plot_mesh (bool) plot mesh
    @param plot_only_dry_mesh (bool) plot mesh only on dry zones
    @param mask_tidal_flats (bool) mask scalar on dry zones
    @param tidal_flats_threshold (float) (default: 0.005)
    @param annotate_bnd (bool) annotate boundary conditions
    @param annotate_liq_bnd (bool) annotate liquid boundaries
    @param annotate_time (bool) annotate time
    @param scalar_map (bool) plot scalar map (mutualy exclusive with filled
    contours)
    @param filled_contours (bool) plot filled contours (mutualy exclusive with
    scalar map)
    @param contours (bool) plot contours (mutualy exclusive with colored
    contours)
    @param colored_contours (bool) plot colored contours (mutualy exclusive
    with contours)
    @param streamlines (bool) plot velocity streamlines
    (mutualy exclusive with colored streamlines)
    @param colored_streamlines (bool) plot colored velocity streamlines
    (mutualy exclusive with streamlines)
    @param streamlines_density (int) density of streamlines
    @param vectors (bool) plot velocity vectors (mutualy exclusive with colored
    vectors)
    @param colored_vectors (bool) plot colored velocity vectors
    (mutualy exclusive with vectors)
    @param vectors_scale (int) scale of the vectors
    @param vectors_normalize (bool) normalize the vectors
    @param grid_resolution (list) grid resolution for vectors and streamlines
    plots
    @param bathy_contours (bool) plot bathymetry contours
    @param kwargs (dict) Argument passed to the plotting function
    """
    # TODO: add grid compatibility

    # Set default var type if var_name is VELOCITY:
    if (var_name in ['VELOCITY', 'VITESSE']) \
       and var_type == 'scalar':
        var_type = 'vector'

    # default variables
    mesh = res.tri
    ndim = res.get_mesh_dimension()

    # Get scalar data for 2d maps and contours
    if var_name != '':
        # If time is positive searched for record
        if time is not None:
            record = res.get_closest_record(time)
        else:
            time = res.times[record]

        if var_type == 'scalar':
            mesh, scalar = get_data(res, var_name, record, zslice,
                                    poly, poly_number, plane)

        elif var_type in ['vector', 'vector_2d']:
            if var_name+' U' in res.varnames and var_name+' V' in res.varnames:
                vectx_name = var_name+' U'
                vecty_name = var_name+' V'
            elif var_name+' X' in res.varnames and \
                    var_name+' Y' in res.varnames:
                vectx_name = var_name+' X'
                vecty_name = var_name+' Y'
            else:
                raise TelemacException(
                    "Vector components not found in result file")

            mesh, vectx = get_data(res, vectx_name, record, zslice,
                                   poly, poly_number, plane)
            mesh, vecty = get_data(res, vecty_name, record, zslice,
                                   poly, poly_number, plane)

            scalar = np.sqrt(vectx**2 + vecty**2)

        elif var_type == 'vector_3d':
            assert ndim == 3
            if var_name+' U' in res.varnames and var_name+' V' in res.varnames\
                    and var_name+' W' in res.varnames:
                vectx_name = var_name+' U'
                vecty_name = var_name+' V'
                vectz_name = var_name+' W'
            elif var_name+' X' in res.varnames and \
                    var_name+' Y' in res.varnames and \
                    var_name+' Z' in res.varnames:
                vectx_name = var_name+' X'
                vecty_name = var_name+' Y'
                vectz_name = var_name+' Z'
            else:
                raise TelemacException(
                    "Vector components not found in result file")

            mesh, vectx = get_data(res, vectx_name, record, zslice,
                                   poly, poly_number, plane)
            mesh, vecty = get_data(res, vecty_name, record, zslice,
                                   poly, poly_number, plane)
            mesh, vectz = get_data(res, vectz_name, record, zslice,
                                   poly, poly_number, plane)
            scalar = np.sqrt(vectx**2 + vecty**2 + vectz**2)
        else:
            raise TelemacException("Unknown varriable type")

        scalar *= var_factor

    # Get velocity for vectors and streamlines plots
    if streamlines or colored_streamlines or vectors or colored_vectors:
        if vect_name+' U' in res.varnames:
            velx_name = vect_name+' U'
            vely_name = vect_name+' V'
            if poly is not None:
                velz_name = vect_name+' W'
        elif vect_name+' X' in res.varnames:
            velx_name = vect_name+' X'
            vely_name = vect_name+' Y'
            if poly is not None:
                velz_name = vect_name+' Z'
        else:
            raise TelemacException("Need VELOCITY to plot streamlines/vectors")

        _, velx = get_data(res, velx_name, record, zslice,
                           poly, poly_number, plane)
        _, vely = get_data(res, vely_name, record, zslice,
                           poly, poly_number, plane)
        if poly is not None:
            # If extraction along polyline, the x component depends on the
            # polyline tangents and the y component is the vertical velocity.
            if poly_number is None:
                poly_number = res.discretize_polyline(poly)
            polyd = linspace_poly(poly, poly_number)
            # Compute tangents of polyline segments:
            tangents = compute_segments_tangents(polyd)

            # Project horizontal velocity on segments tangents:
            for i, _ in enumerate(velx):
                # j: index of the polyline number
                j = i//res.nplan
                if j == 0:
                    # Skip first tangents (null value)
                    velx[i] = np.dot(np.asarray([velx[i], vely[i]]),
                                     tangents[j+1])
                else:
                    velx[i] = np.dot(np.asarray([velx[i], vely[i]]),
                                     tangents[j])

            _, velz = get_data(res, velz_name, record, zslice,
                               poly, poly_number, plane)
            vely = velz

        velx *= vect_factor
        vely *= vect_factor

    # Apply scaling facors
    mesh.x *= x_factor
    mesh.y *= y_factor

    # initialize masks
    if mask_tidal_flats or bathy_contours:
        mesh.set_mask(None)
        if "WATER DEPTH" in res.varnames:
            h = res.get_data_value("WATER DEPTH", record)
        elif "HAUTEUR D'EAU" in res.varnames:
            h = res.get_data_value("HAUTEUR D'EAU", record)
        else:
            raise TelemacException("Need WATER DEPTH to mask tidal flats")
        mask_dry = mask_triangles(
            mesh, h, relation='leq', threshold=tidal_flats_threshold)
        mask_wet = mask_triangles(
            mesh, h, relation='geq', threshold=tidal_flats_threshold)

    # initialize plot
    plt.style.use('default')
    plt.rcParams.update(decoVNV)
    fig, ax = plt.subplots(1, 1, figsize=fig_size)

    # aspect ratio:
    ax.set_aspect(aspect_ratio)

    # mesh
    if plot_mesh:
        if plot_only_dry_mesh:
            mesh.set_mask(mask_wet)
            plot2d_triangle_mesh(ax, mesh, color='k', linewidth=0.2, alpha=1.)
        else:
            plot2d_triangle_mesh(ax, mesh, color='k', linewidth=0.2, alpha=1.)

    # annotate boundaries
    if annotate_bnd:
        bnd_info = res.get_bnd_info()
        plot2d_annotate_bnd(ax, mesh, bnd_info, markersize=1.5, marker='o')

    # annotate boundaries
    if annotate_liq_bnd:
        liq_bnd_info = res.get_liq_bnd_info()
        plot2d_annotate_liq_bnd(ax, mesh, liq_bnd_info, markersize=1.5,
                                marker='o')

    # colorbar settings
    if cbar_priority == 'scalar':
        scalar_colorbar = True
        vector_colorbar = False
        contours_colorbar = False
    elif cbar_priority == 'vector':
        scalar_colorbar = False
        vector_colorbar = True
        contours_colorbar = False
    elif cbar_priority == 'contours':
        scalar_colorbar = False
        vector_colorbar = False
        contours_colorbar = True
    else:
        raise ValueError("Unknown cbar_priority")

    if cbar is False:
        scalar_colorbar = False
        vector_colorbar = False
        contours_colorbar = False

    if cbar_autoextend:
        if vmin is None or vmax is None:
            vmin, vmax = set_extrema(scalar, vmin, vmax)
        if vmax <= np.max(scalar) and vmin < np.min(scalar):
            cbar_extend = 'min'
        elif vmax > np.max(scalar) and vmin >= np.min(scalar):
            cbar_extend = 'max'
        elif vmax > np.max(scalar) and vmin < np.min(scalar):
            cbar_extend = 'both'
        else:
            cbar_extend = 'neither'

    #  mask tidal flats:
    if mask_tidal_flats:
        mesh.set_mask(mask_dry)

    # SCALAR LAYERS:

    # Scalar map layer
    if scalar_map:
        assert filled_contours is False
        plot2d_scalar_map(
            fig, ax, mesh, scalar, data_name=cbar_label,
            vmin=vmin, vmax=vmax, nv=nv, cmap_name=cmap_name,
            cbar_ticks=cbar_ticks, extend=cbar_extend,
            cbar_properties=cbar_properties,
            cbar_ax=cbar_ax, cbar_cax=cbar_cax,
            colorbar=scalar_colorbar, **kwargs)
    # filled contours layer
    if filled_contours:
        if nv is None:
            nv = 11
        assert scalar_map is False
        plot2d_scalar_filled_contour(
            fig, ax, mesh, scalar, data_name=cbar_label,
            vmin=vmin, vmax=vmax, nv=nv,
            cbar_ticks=cbar_ticks, extend=cbar_extend,
            cbar_properties=cbar_properties,
            cbar_ax=cbar_ax, cbar_cax=cbar_cax,
            cmap_name=cmap_name, colorbar=scalar_colorbar, **kwargs)
    # contours layer
    if contours:
        if nv is None:
            nv = 11
        assert colored_contours is False
        plot2d_scalar_contour(
            fig, ax, mesh, scalar, vmin=vmin, vmax=vmax, nv=nv,
            cbar_ticks=cbar_ticks,
            cbar_properties=cbar_properties,
            cbar_ax=cbar_ax, cbar_cax=cbar_cax,
            linewidths=0.4, colors='k', colorbar=contours_colorbar)
    # colored contours layer
    if colored_contours:
        if nv is None:
            nv = 11
        assert contours is False
        plot2d_scalar_contour(
            fig, ax, mesh, scalar, vmin=vmin, vmax=vmax, nv=nv,
            cbar_ticks=cbar_ticks,
            cbar_properties=cbar_properties,
            cbar_ax=cbar_ax, cbar_cax=cbar_cax,
            linewidths=0.4,
            cmap_name=cmap_name, colorbar=contours_colorbar)

    # VECTOR LAYERS:

    # streamlines layer
    if streamlines:
        assert colored_streamlines is False
        plot2d_streamlines(
            fig, ax, mesh, velx, vely,
            grid_resolution=grid_resolution, grid_xlim=xlim, grid_ylim=ylim,
            color='k', colorbar=vector_colorbar, data_name=cbar_label,
            cbar_ticks=cbar_ticks,
            cbar_properties=cbar_properties,
            cbar_ax=cbar_ax, cbar_cax=cbar_cax,
            density=streamlines_density)
    # colored streamlines layer
    if colored_streamlines:
        assert streamlines is False
        plot2d_streamlines(
            fig, ax, mesh, velx, vely,
            grid_resolution=grid_resolution, grid_xlim=xlim, grid_ylim=ylim,
            cmap_name='jet', colorbar=vector_colorbar, data_name=cbar_label,
            cbar_ticks=cbar_ticks,
            cbar_properties=cbar_properties,
            cbar_ax=cbar_ax, cbar_cax=cbar_cax,
            density=streamlines_density)
    # vectors layer
    if vectors:
        assert colored_vectors is False
        plot2d_vectors(
            fig, ax, mesh, velx, vely, normalize=vectors_normalize,
            scale=vectors_scale, headwidth=3, headlength=5,
            grid_resolution=grid_resolution, grid_xlim=xlim, grid_ylim=ylim,
            color='k', colorbar=vector_colorbar, data_name=cbar_label,
            cbar_ticks=cbar_ticks,
            cbar_properties=cbar_properties,
            cbar_ax=cbar_ax, cbar_cax=cbar_cax,
            alpha=0.75)
    # colored vectors layer
    if colored_vectors:
        assert vectors is False
        plot2d_vectors(
            fig, ax, mesh, velx, vely, normalize=vectors_normalize,
            scale=vectors_scale, headwidth=3, headlength=5,
            grid_resolution=grid_resolution, grid_xlim=xlim, grid_ylim=ylim,
            cmap_name='jet', colorbar=vector_colorbar, data_name=cbar_label,
            cbar_ticks=cbar_ticks,
            cbar_properties=cbar_properties,
            cbar_ax=cbar_ax, cbar_cax=cbar_cax,
            alpha=0.75)

    # bathymetry contours layer
    if bathy_contours:
        if 'BOTTOM' in res.varnames:
            bottom = res.get_data_value('BOTTOM', record)
        elif 'FOND' in res.varnames:
            bottom = res.get_data_value('FOND', record)
        else:
            raise TelemacException("Need BOTTOM to plot bottom contours")
        mesh.set_mask(mask_wet)
        plot2d_scalar_contour(
            fig, ax, mesh, bottom,
            data_name='bottom (m)', colors='k', linewidths=0.25,
            colorbar=False)

    # plot options
    if xlim is not None:
        ax.set_xlim(xlim[0], xlim[1])
    if ylim is not None:
        ax.set_ylim(ylim[0], ylim[1])

    ax.set_xlabel(x_label)
    ax.set_ylabel(y_label)

    # title
    if annotate_time:
        assert fig_title is None
        ax.set_title("t = {:.1f} s".format(time))
    if fig_title is not None:
        ax.set_title(fig_title)

    # save figure:
    if fig_name != '':
        print(" "*8+"~> Plotting {}".format(fig_name))
        fig.savefig(fig_name)
    else:
        plt.show()

    # Apply scaling facors
    mesh.x *= 1./x_factor
    mesh.y *= 1./y_factor
    # reset mesh properties:
    if mask_tidal_flats or bathy_contours:
        mesh.set_mask(None)

    # Close figure:
    plt.close('all')

# _____            _________________________________________________
# ____/ 3D PLOTS  /________________________________________________/
#


def vnv_plot3d(varname, res, record=-1, time=None,
               fig_size=None, fig_name='', fig_title=None,
               vmin=None, vmax=None, nv=10,
               xlim=None, ylim=None, zlim=None,
               x_label='x (m)', y_label='y (m)', z_label='z (m)',
               cmap_name='jet', cbar=True, annotate_time=False,
               cbar_ticks=None,
               cbar_label=None, **kwargs):
    """
    Plot a scalar map using values as z coordinates

    @param res (TelemacFile) Struct to file from which data will be read
    @param varname (str) Name of the variable to plot
    @param record (str) Record to plot
    @param time (float) If given will get nearest record to that time (This
    overwrites record)
    @param fig_size (2-uple) Size of figure
    @param fig_name (str) If not empty save the plot in that file instead of
    showing it
    @param fig_title (str) Figure title
    @param vmin (float) Minimal value of data to plot
    @param vmax (float) Maximal value of data to plot
    @param nv (integer) Number of sample for colorbar range
    @param xlim (list) x axis limits
    @param ylim (list) y axis limits
    @param zlim (list) y axis limits
    @param x_label (str) x axis label
    @param y_label (str) y axis label
    @param z_label (str) z axis label
    @param cmap_name (str) name of the scalar map
    @param cbar (bool) trigger for colorbar plot
    @param annotate_time (bool) Set time as title
    @param cbar_ticks (list) list of values where to show color bar ticks
    @param cbar_label (str) name to show on scalar colorbar
    @param kwargs (dict) Argument given to the scalar_map

    """
    # If time is positive searched for record
    if time is not None:
        record = res.get_closest_record(time)
    else:
        time = res.times[record]

    # get data
    data = res.get_data_value(varname, record)

    # initialize figure
    plt.style.use('default')
    plt.rcParams.update(decoVNV)
    fig = plt.figure(figsize=fig_size)
    ax = fig.add_subplot(111, projection='3d')

    # Plotting mesh
    plot3d_scalar_map(fig, ax, res.tri, data,
                      x_label=x_label, y_label=y_label,
                      vmin=vmin, vmax=vmax, nv=nv,
                      data_name=cbar_label,
                      cbar_ticks=cbar_ticks,
                      cmap_name=cmap_name,
                      colorbar=cbar, **kwargs)

    # plot options
    if xlim is not None:
        ax.set_xlim(xlim[0], xlim[1])
    if ylim is not None:
        ax.set_ylim(ylim[0], ylim[1])
    if zlim is not None:
        ax.set_zlim(zlim[0], zlim[1])

    ax.set_xlabel(x_label)
    ax.set_ylabel(y_label)
    ax.set_zlabel(z_label)

    # title
    if annotate_time:
        assert fig_title is None
        ax.set_title("t = {:.1f} s".format(time))
    if fig_title is not None:
        ax.set_title(fig_title)

    # save figure:
    if fig_name != '':
        print(" "*8+"~> Plotting {}".format(fig_name))
        fig.savefig(fig_name)
    else:
        plt.show()

    # Close figure:
    plt.close('all')

# _____                _____________________________________________
# ____/ KHIONE PLOTS  /____________________________________________/
#

def vnv_khione_plot1d_TC(
        times, T, Ct, C=None,
        Tb=None, Ctb=None, unit=None,
        T_label='$T$', Ct_label='$C$',
        Tb_label='$T_{bis}$', Ctb_label='$C_{bis}$',
        ref_Tf=0.,
        ref_T_data=None, ref_T_label="", ref_T_error=None,
        ref_C_data=None, ref_C_label="", ref_C_error=None,
        normalize_time=False,
        normalize_reftime=False,
        normalize_Ct=False,
        xlim=None, ylim=None, yblim=None, nby=None,
        legend_loc='best', legend_locb='best',
        verbose=False,
        fig_size=None,
        fig_name=None):
    """
    Plot temperature and frazil as functions of time

    @param times (np.array) result time array
    @param T (np.array) temperature array
    @param Ct (np.array) total frazil volume fraction array
    @param C (np.array) class frazil volume fraction array (default: None)
    @param Tb (np.array) secondary temperature array for comparison plots
    (default: None)
    @param Ctb (np.array) secondary total frazil volume fraction array
    for comparison plots (default: None)
    @param unit (str) if "perthousand" plot frazil olume fraction in
    perthousand unit else plots in m3/m3 (default: None)
    @param T_label (str) legend label of temperature array
    @param Ct_label (str) legend label of total frazil array
    @param Tb_label (str) legend label of secondary temperature array
    @param Ctb_label (str) legend label of secondary total frazil array
    @param ref_Tf (np.array) reference fusion temperature
    @param ref_T_data (np.array) reference array for temperature
    @param ref_T_label (str) reference label for temperature
    @param ref_T_error (float) reference error for temperature
    @param ref_C_data (np.array) reference array for frazil volume fraction
    @param ref_C_label (str) reference label for frazil volume fraction
    @param ref_C_error (float) reference error for frazil volume fraction
    @param normalize_time (bool) if True, normalize time axis with
    the computed critical time tc (default: False)
    @param normalize_reftime (bool) if True, normalize ref time
    axis with the computed critical time tc (default: False)
    @param normalize_Ct (bool) if True, normalize total frazil volume
    fraction with the computed critical total volume fraction
    (default: False)
    @param xlim (list) x axis limits
    @param ylim (list) y axis limits
    @param yblim (list) yb axis limits
    @param legend_loc (str) legend location (default: 'best')
    @param legend_locb (str) legend location of twin axis (default: 'best')
    @param nby (float) sets major locator
    @param verbose (bool) show recovery index, time and
    value when normalization is active
    @param fig_size (2-uple) Size of figure
    @param fig_name (str) If not empty save the plot in that file instead of
    showing it
    """
    # initialize plot
    plt.style.use('default')
    plt.rcParams.update(decoVNV)
    plt.rcParams.update(decoVNV_1d)
    fig, ax = plt.subplots(1, 1, figsize=fig_size)
    axb = ax.twinx()

    # normalization
    if normalize_time:
        # searching max supercooling index
        im = np.argmin(T)
        Tmin = T[im]
        # searching 90% recovery index
        Taux = np.zeros(len(T))
        for i in range(len(T)):
            Taux[i] = T[i]
            if (Taux[i]-ref_Tf) > 0.1*(Tmin-ref_Tf):
                Taux[i] = Tmin
        ic = im + np.argmax(Taux[im:])
        Tc = T[ic]
        # adim of time
        coeft = 1./times[ic]
        # adime of concentration
        if normalize_Ct:
            unit = None
            Ct = Ct/Ct[ic]
            if Ctb is not None:
                Ctb = Ctb/Ctb[ic]
        # verbose
        if verbose:
            print("Recovery index:")
            print("ic = ", ic)
            print("Tc = ", Tc)
            print("tc = ", times[ic])
    else:
        coeft = 1.

    # plot reference temperature
    if ref_T_data is not None:
        # reference with error bars:
        if ref_T_error is not None:
            if normalize_reftime:
                yerr = [ref_T_error for i in range(len(ref_T_data[:, 1]))]
                ax.errorbar(
                    coeft*ref_T_data[:, 0], ref_T_data[:, 1],
                    yerr=yerr, fmt='s', lw=0.5, capsize=2, capthick=1,
                    markersize=4, color='k', label=ref_T_label)
            else:
                yerr = [ref_T_error for i in range(len(ref_T_data[:, 1]))]
                ax.errorbar(
                    ref_T_data[:, 0], ref_T_data[:, 1],
                    yerr=yerr, fmt='s', lw=0.5, capsize=2, capthick=1,
                    markersize=4, color='k', label=ref_T_label)
        # simple reference points:
        else:
            if normalize_reftime:
                ax.plot(
                    coeft*ref_T_data[:, 0], ref_T_data[:, 1],
                    'k', marker='s', lw=0., label=ref_T_label)
            else:
                ax.plot(
                    ref_T_data[:, 0], ref_T_data[:, 1],
                    'k', marker='s', lw=0., label=ref_T_label)

    # plot reference frazil concentration
    if ref_C_data is not None:
        # reference with error bars:
        if ref_C_error is not None:
            if normalize_reftime:
                yerr = [ref_C_error for i in range(len(ref_C_data[:, 1]))]
                axb.errorbar(
                    coeft*ref_C_data[:, 0], ref_C_data[:, 1],
                    yerr=yerr, fmt='s', lw=0.5, capsize=2, capthick=1,
                    markersize=4, color='k', label=ref_C_label)
            else:
                yerr = [ref_C_error for i in range(len(ref_C_data[:, 1]))]
                axb.errorbar(
                    ref_C_data[:, 0], ref_C_data[:, 1],
                    yerr=yerr, fmt='s', lw=0.5, capsize=2, capthick=1,
                    markersize=4, color='k', label=ref_C_label)
        # simple reference points:
        else:
            if normalize_reftime:
                axb.plot(
                    coeft*ref_C_data[:, 0], ref_C_data[:, 1],
                    'k', marker='s', lw=0., label=ref_C_label)
            else:
                axb.plot(
                    ref_C_data[:, 0], ref_C_data[:, 1],
                    'k', marker='s', lw=0., label=ref_C_label)

    # plot temperature
    ax.plot(coeft*times, T, label=T_label, color='b')

    # plot temperature (bis)
    if Tb is not None:
        ax.plot(coeft*times, Tb, label=Tb_label, color='purple', ls='-.')

    # plot total frazil concentration
    if unit == "perthousand":
        coef = 1.e3
        unit_label = "$($\\textperthousand$)$"
    else:
        coef = 1.
        unit_label = "$(-)$"

    plt.grid()
    axb.plot(coeft*times, coef*Ct, label=Ct_label, ls='--', color='r')

    # plot total frazil concentration (bis)
    if Ctb is not None:
        axb.plot(coeft*times, coef*Ctb, label=Ctb_label, ls=':', color='orange')

    # plot frazil class concentration
    if C is not None:
        for i in range(C.shape[0]):
            axb.plot(coeft*times, coef*C[i, :], label='$C_{}$'.format(i))

    # ax labels
    if normalize_time:
        ax.set_xlabel("$t^*$ $(-)$")
    else:
        ax.set_xlabel("time $(s)$")

    if normalize_Ct:
        axb.set_ylabel("$C^*$ $(-)$")
    else:
        axb.set_ylabel("$C$ " + unit_label)

    ax.set_ylabel("$T$ $(^\circ {C})$")

    ax.legend(loc=legend_loc)
    axb.legend(loc=legend_locb)

    # ax limits and ticks
    if xlim is not None:
        ax.set_xlim(xlim[0], xlim[1])
        axb.set_xlim(xlim[0], xlim[1])
    if ylim is not None:
        ax.set_ylim(ylim[0], ylim[1])
    if yblim is not None:
        axb.set_ylim(yblim[0], yblim[1])
    if nby is not None:
        ax.yaxis.set_major_locator(ticker.MultipleLocator((ylim[1]-ylim[0])/nby))
        axb.yaxis.set_major_locator(ticker.MultipleLocator((yblim[1]-yblim[0])/nby))

    # save figure:
    if fig_name != '':
        print(" "*8+"~> Plotting {}".format(fig_name))
        fig.savefig(fig_name)
    else:
        plt.show()

    # Close figure:
    plt.close('all')
