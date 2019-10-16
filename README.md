# IAU\_Chrom 5.17+ Readme

*Content*
=========

[1 Overview and workflow 1]

[2 Features 2]

[2.1 Technical Notes 2]

[2.2 Main widget: "File" tab 3]

[2.3 Main widget: "DataAnalysis" tab 4]

[2.4 Main widget: "Viewer" tab 5]

[2.5 Main widget: "Advanced" tab (scripting) 6]

[2.6 Define your targets: "*ms.info*" config file 6]

[2.7 Define molecule fragments and ratios: "*fragments.txt"* config file 7]

[3 Data analysis: Tools to get an impression 8]

[3.1 Viewer: Multiple Chroms (MCV) 8]

[3.2 Viewer: Multiple Masses (MMV) 9]

[4 Data Analysis: Integrate! 10]

[4.1 Signal integration widget 11]

[4.1.1 Integration widget: "Config" tab 11]

[4.1.2 Integration widget: "BatchProcess" tab 12]

[4.1.3 Integration widget: "Report" tab 13]

[4.1.4 Integration widget: "Plot" tab 13]

[4.2 Signal integration: Settings 14]

[4.2.1 Flagging & Delay 14]

[4.2.2 Retention time and peak integration 14]

[4.2.3 Integration: general tips 15]

[4.2.4 Integration: methods 17]

[4.2.5 Noise calculation 18]

[5 Data Analysis: Fragment Ratios 19]

Overview and workflow
=====================

Analysis of chromatographic data in IAU\_Chrom is pretty straight-forward:

1.  ***Load chromatographic data*** from various sources like e.g. the Agilent Quadrupole‑MS or the Tofwerk TOFMS (sect. 2).

2.  ***Have a look*** at your data with the viewing tools (viewers) on the plot windows of IAU\_Chrom (sect. 3).

3.  ***Define the species*** you want to analyse by loading the ms.info configuration file (sect. 2.5).

4.  ***Integrate the signals*** of the defined substances and calculate the noise level on the respective baseline sections of the respective signals (sect. 4).

5.  ***Save results*** in the form of an IDL binary file. That allows you to continue working on the experiment later on. Results can also be saved as text-files (sects. 2.2 and 4.1.3).

6.  Specifically for mass spectrometric data, IAU\_Chrom offers the possibility to analyse molecule ***fragment ratios*** if multiple ions are recorded per species. Use the fragment ratio calculation tool to do so (sect. 5).

Features
========

Technical Notes 
----------------

-   Compatibility. IAU\_Chrom is written in the IDL programming language on and for Windows machines (Win 7). Win 10 is known to cause problems of the plot windows (lagging etc. but no crash). A version for Unix-based systems does not exist.

-   Version 5.02 and greater: Error handling is implemented which avoids total crashes for most errors. To avoid data loss in any case, save your experiment regularly.

-   To use IAU\_Chrom on the system partition on your machine (C:\\), it might be necessary to run IDL or the IDL virtual machine as in administrator mode.

-   To use the code version (not pre-compiled), an IDL installation (v8.4 or greater) is required.


**Start IAU\_Chrom:**

-   Runtime version, virtual machine only: run IAU\_Chrom\_vxxx.exe;

-   Code version, IDL installed: open and run the IAU\_Chrom\_v5...pro from the root directory of the IAU\_Chrom project.

The IAU\_Chrom main widget is shown in Fig. 1.

![1](https://raw.githubusercontent.com/MrFuppes/IDL_IAU_Chrom/master/doc/img/01_WidMain.png)

Fig. 1 - Main widget.

All further widgets (plots, tools) appear as soon as called/required.

Main widget: "File" tab
-----------------------

![2](https://raw.githubusercontent.com/MrFuppes/IDL_IAU_Chrom/master/doc/img/02_WidMain_File.png)

Fig. 2 - File menu on main widget.

'File' '**Import...':** Import chromatographic (raw) data in different formats like netcdf (.nc) or hdf5 (.h5) from different instruments. Choice is based on the instrument that produced the data.

-   To process measurement series, files should be named with an increasing index, like 01\_name, 02\_name or name\_01 and so on. \"name" in this case is a constant string. A useful tool to rename and/or enumerate files is Ant Renamer, http://www.antp.be/software/renamer.

-   v5.13 and higher: IAU\_Chrom also checks for a measurement timestamp in the data file; if loaded files are not sorted by ascending timestamp, it will ask you if you wish to sort by that quantity.

'File' '**Save Experiment**': Saves the current set of loaded data, settings used for analysis and results together in one binary file \*.sav. This file is only readable by IAU\_Chrom (IDL proprietary format).

'File' '**Restore Experiment**': Restores a \*.sav / IDL binary file.

'File' '**Set Filepath...**': Set the default directory where to look for save files etc.

'File' '**Exit IAU\_Chrom**': Closes the program.

(main widget) **Time Axis Format**: Change units of the x-axis to seconds or minutes. Chose the desired unit before loading data, otherwise results will be deleted. Restoring an experiment will automatically select the unit used in this experiment.

(main widget) **Loaded Files / Selected m/Q**: Dropdown-lists; appear as soon as data has been loaded or and experiment has been restored. Selecting one of the droplists will cause the plot window to appear and display the selection (Plot0).

Main widget: "DataAnalysis" tab
-------------------------------

![3](https://raw.githubusercontent.com/MrFuppes/IDL_IAU_Chrom/master/doc/img/03_WidMain_DataAnalysis.png)

Fig. 3 - Main widget, data analysis tab.

'DataAnalysis' '**Peak Integration / Noise**': Opens the integration widget. Requires: Chromatograms/experiment loaded, ms.info (see sect. 2.5).

'DataAnalysis' '**Fragment Ratios**': Opens the fragment ratio calculator widget. See Ch. 5. Requires: Chromatograms/experiment loaded, ms.info (see sect. 2.5), fragments.info (see 2.7).

Main widget: "Viewer" tab
-------------------------

![4](https://raw.githubusercontent.com/MrFuppes/IDL_IAU_Chrom/master/doc/img/04_WidMain_Viewer.png)

Fig. 4 - Main widget, viewer tab.

'Viewer' '**Multiple Chroms**': Opens a viewer widget that can display an overlay of up to seven chromatograms (one specific m/q), see sect. 3. Uses plot window 0 (upper). Requires: Chromatograms/experiment loaded.

'Viewer ' '**Multiple m/Q**': Opens a viewer widget that can display an overlay of upt to seven m/q signals (one specific chromatogram), see sect. 3. Uses plot window 0 (upper). Requires: Chromatograms/experiment loaded.

Viewer ' '**TPS HSK**': Opens a viewer widget for the housekeeping data recorded by the TPS module of the Frankfurt GC-TOFMS. Uses plot window 0 (upper). Requires: Chromatograms/experiment fromt the Tofwerk TOFMS loaded.

'Viewer ' '**Show Controls**': Opens a widget that allows you to control the plots' x- and y-ranges without using the „shiftkey-click-drag" method on the plot widgets. (Fig. 5).

![5](https://raw.githubusercontent.com/MrFuppes/IDL_IAU_Chrom/master/doc/img/05_WidPlotctrl.png)

Fig. 5 - Plot controls widget.

Main widget: "Advanced" tab (scripting)
---------------------------------------

![6](https://raw.githubusercontent.com/MrFuppes/IDL_IAU_Chrom/master/doc/img/06_WidMain_Adv.png)

Fig. 6 -- Main widget, advanced tab.

'Advanced' '**Run Database Script**': Allows to run a script that defines experiments including e.g.

-   directory where data is found

-   path of an ms.info file

-   a definition which data should be imported (TOFMS, Quad-MS etc.)

The database script is a .csv table which contains a description of its use in the header, it is therefore not described in detail here.

Define your targets: "*ms.info*" config file
--------------------------------------------

To analyse data, a configuration file is necessary that specifies target species' retention times and other parameters. It is named \*ms.info by default. Before the ms.info file can be loaded, chromatograms have to be imported, see sect. 2.2.

A prompt to select an ms.info config file appears as soon as you call the integration widget from the DataAnalysis menu. It can also be re-loaded from the integration widget, see sect. 4. Be aware that if you integrate signals and then choose to re-load the ms.info config file, results will be reset (deleted) to ensure coherency of calculation results and configuration (integration parameters etc.).

The ms.info file can be opened and edited e.g. to add a new substance with a text editor like e.g. Notepad++ or also with Excel. Make sure that the formatting stays intact after editing, i.e. leave no superfluous tabs, spaces, linefeeds etc.

Define molecule fragments and ratios: "*fragments.txt"* config file
-------------------------------------------------------------------

Necessary to use the fragment ratios calculation tool, see sect. 5. Contains information on how which species fragments in 70 eV electron ionisation. E.g. CFC-12 (CF2Cl2) forms main ions of m/q 85 and 87, i.e. CF~2~^35^Cl^+^ and CF~2~^37^Cl^+^ with calculated m/q 84.96511 and 86.96216.

Data analysis: Tools to get an impression
=========================================

Viewer: Multiple Chroms (MCV)
-----------------------------

![7](https://raw.githubusercontent.com/MrFuppes/IDL_IAU_Chrom/master/doc/img/07_Window_MCV_Plot.png)

Fig. 7 - Multi Chrom Viewer widget with plot window.

The "multiple chroms viewer" (MCV) visualises a specific mass trace („Selected m/Q") in up to seven different chromatograms (Chrom\_0 to Chrom\_6). The checkbox "Difference Chrom\_1-Chrom\_0" calculates and displays the difference between the first and the second chromatogram (Chrom\_1 is subtracted from Chrom\_0).

MCV, file tab **Exit**. Closes the widget (plot stays opened).

MCV, plot tab **Reset**. Clears Plot0 (nothing displayed) and resets all selected values on the widget to default (nothing selected).

MCV, plot tab **Recreate Textfields**. Recreates the textfields on Plot0 at their correct positions.

Viewer: Multiple Masses (MMV)
-----------------------------

![8](https://raw.githubusercontent.com/MrFuppes/IDL_IAU_Chrom/master/doc/img/08_Window_MMV_Plot.png)

Fig. 8 - Mulit Mass Viewer widget with plot window.

The "multiple mass viewer" (MMV) can display up to seven different mass traces taken from one chromatogram (m/Q\_0 to m/Q\_6). Selecting a predefined set of mass traces belonging to one species in the ms.info config is possible after the ms.info has been loaded ("Substance Preset" droplist).

MMV, file tab **Exit**. Closes the widget (plot stays opened).

MMV, config tab **(Re-)Load MSINFO**. Allows the user to load an ms.info if none has been loaded or overwrite an already loaded ms.info.

MMV, plot tab **Reset**. Clears Plot0 (nothing displayed) and resets all selected values on the widget to default (nothing selected).

MMV, plot tab **Recreate Textfields**. Recreates the textfields on Plot0 at their correct positions.

MMV, plot tab **Generate Legend**. Displays a plot legend naming the selected mass traces.

Data Analysis: Integrate!
=========================

![9](https://raw.githubusercontent.com/MrFuppes/IDL_IAU_Chrom/master/doc/img/09_Window_PI_Plot.png)

Fig. 9 - Peak integration widget with plot window. Integrated chromatographic signal shown.

-   data loaded

-   ms.info loaded

-   main widget data analysis peak integration/noise

The lower plot window (plot1, Fig. 9) shows the integrated chromatographic signal.

The upper plot window (plot0) shows the full mass trace of selected chromatogram and species. It can be refreshed by clicking the "Refresh Plot0 (Overview)" button on the integration widget.

Signal integration widget
-------------------------

![10](https://raw.githubusercontent.com/MrFuppes/IDL_IAU_Chrom/master/doc/img/10_WidInt.png)

Fig. 10 - Integration widget.

### Integration widget: "Config" tab

![11](https://raw.githubusercontent.com/MrFuppes/IDL_IAU_Chrom/master/doc/img/11_WidInt_Config.png)

Fig. 11 - Integration widget, config tab.

'Config' '**Reload Settings**': Reloads the ms.info file. Existing integration results will be deleted to avoid ambiguities.

'Config ' '**Reload Settings NOM**': same as above exept that m/Q given as floating point numbers in the ms.info will be rounded to integers ('nominal m/Q').

'Config ' '**Export Settings**': exports an ms.info file to a user-defined folder with the settings used in the selected chromatogram.

### Integration widget: "BatchProcess" tab

![12](https://raw.githubusercontent.com/MrFuppes/IDL_IAU_Chrom/master/doc/img/12_WidInt_BatchProcess.png)

Fig. 12 - Integration widget, batch processing tab.

'BatchProcess' '**Integrate**': All species defined in the ms.info will be integrated in all loaded chromatograms. During calculations, no results will be plotted.

'BatchProcess' '**Integrate+Calc. Noise**': For all species defined in the ms.info, first the noise level will be calculated and then signals will be integrated (if present). Also here, no results will be plotted during calculations.

### Integration widget: "Report" tab

![13](https://raw.githubusercontent.com/MrFuppes/IDL_IAU_Chrom/master/doc/img/13_WidInt_Report.png)

Fig. 13 - Integration widget, report tab.

'Report' '**Quicklook (.ps)**': Generates a postscript file that contains diagnostic plots for all species that where integrated. The x-axis of each plot shows the measurement number, the y-axis show different parameters which are always normed to their mean value (e.g. peak area of a substance in a specific chromatogram divided by the mean peak area of this substance in all chromatograms).

'Report' '**Report: sel. Subst.**': Generates a report-file (.txt) with integration results for the selected species in all chromatograms.

'Report' '**Report: all Subst.**': Generates a report-files (.txt) with integration results for all species in all chromatograms.

### Integration widget: "Plot" tab

‚Plot' ‚**Recreate Textfields Plot1**': Stellt die Textfelder auf Plot 1 an den korrekten Positionen wieder her.

'Report' '**Plot sel. Substance**': Öffnet einen Diagnostik-Plot für die aktuell gewählte Substanz.

Signal integration: Settings
----------------------------

### Flagging & Delay

The dropdown-menu "Flag" contains five different flags for selected chromatogram and substance. The following table lists the meanings of the flag values:

  **-2**   **Bad Peak** -- To be set manually by the user. Denotes unusable / faulty data. If the integration is applied (to all chromatograms), this chromatogram will be skipped.
  -------- ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  **-1**   **No Peak Found** -- Automatically set. Indicates that the integration routine has found and integrated a signal in the selected data (chromatogram and m/Q).
  **0**    **Not Integrated** -- Initial state. Indicates that the integration was not (yet) called for this set of data. Initial state of all data when the integration widget is first started.
  **1**    **Integrated** -- Automatically set. Chromatographic signal found and integrated.
  **2**    **Integrated (man.)** -- Automatically set. Denotes integration settings that have been changed in comparison to other chromatograms (selected species).

**Droplist "Delay"**: Lets you select a delay (pause) between integrations, i.e. if applying an integration setting to all chromatograms.

**Checkbox "Overwrite manual Settings"**: Defines if manually edited integrations of individual chromatograms of the experiment are overwritten (checkbox set) or kept (box unchecked) if the function "apply settings to all" is called.

### Retention time and peak integration

Note: See section 4.2.3 for further hints which settings to select under which circumstances.

**t\_R min. and t\_R max.:** selects the x-axis section / retention time section, within which a signal is expected (and should be integrated).

**Peak Integration: Method.** Selectes the integration method. Choices are *Baseline*, *Gaussfit* and different variants of a *Gumble-Fit*.

**Peak Integration: Baseline Fitfunction.** Different baseline functions are available, chose between *constant* (no inclination), *linear* (with inclination) und *quadratic* (2^nd^ order polynomial).

**Peak Integration: Sigma left / Sigma right.** These parameters determine, which data left and right of the signal apex are used to calculate the fit. 1 sigma is equivalent to the sigma (half width) of a Gaussian fit resembling the signal. The value you enter is a multiplier for this sigma value.

### Integration: general tips

General goal here are precise and accurate results. A relative calibration scheme is assumed (multiple calibration measurements within the measurement series). The choice how to configure the signal fit relies on two basic, opposing conditions (within the measurement series you analyse):

1)  Chromatography is very stable, the peak shape does not change a lot.

2)  Peak shape changes.

In case 1), the Gaussian fit is a good choice in most cases. In this case, it is most important to "feed" as much data into the fit as possible. This will give you a very good (precise!) representation of the actual signal. For integration settings this means: use as high multiplier values (sigma left / sigma right) as possible. It does not matter if the shape of the signal is exactly matched by the fit, key here is *representation*, not resemblance. Accuracy is very good as long as the peak shape doesn't change significantly.

In case 2), it is better to chose integration settings so that the fit resembles the signal so that the changing signal shape is also present in the calculated fit. Otherwise, results might be inaccurate (although precision can still be good...).

Fig. 14 shows an example of an integrated signal. From the two signals found in the retention time section shown in the plot, the first signal is the target -- the first thing to do to configure the integration would be to set the retention time window so that only the fist signal lies within.

Note that the fit is composed of a solid and a dashed line, red for the peak and blue for the baseline. The **solid line** shows you, which data (x-coordinate) has been used for the fit. The **dashed line** shows the complete fit. The fit integration is always equivalent to ± 15 sigma.

![14](https://raw.githubusercontent.com/MrFuppes/IDL_IAU_Chrom/master/doc/img/14_Window_PI_gauss.png)

Fig. 14 - Integration plot. Black, solid line: m/Q intensity recorded over time (dI/dt). Red, solid line: data in this x-section are used to calculate the fit function. Red, dashed line: resulting fit curve. Blue: baseline.

To configure the sigma left / sigma right values in the example Fig. 14, a large value can be used for sigma left (e.g. 20) as there is no other signal to the left of the targeted peak. However to the right, there is another signal -- to get an accurate fit, no data points of the non-targeted signal may be used to calculate the fit. Consequently, a sigma right value of e.g. 2 has to be used.

### Integration: methods

**\'Baseline\_dynamicRT**\': (ms.info tag: \'bl\')

-   IAU\_Chrom tries to detect a signal in the specified retention time window.

-   The peak detection algorithm returns peak parameters like estimated retention time or full width at half maximum (2 sigma).

-   Based on the sigma value from peak detection and the sigma multipliers on the integration widget, a retention time section "*int\_win*" is determined where baseline fitting and peak integration will be performed.

-   At beginning and end of the *int\_win* section, 6 data points on each side are used to calculate a baseline fit (order based on the selection on the integration widget).

-   All data points within *int\_win* are then integrated after baseline subtraction.

-   although no actual peak fitting is performed, this method compensates for shifts in retention time as peak detection is used.

**\'GaussFit\'**: (ms.info tag: \'gau\')

-   Peak detection and determination of *int\_win* according to '\'Baseline\_dynamicRT'.

-   Selection of data points to calculate the fit based on sigma multipliers (explanations see also sect. 4.2.3).

-   Calculation of the peak fit as a Gaussian distribution.

-   Integration of the fitted signal within ±15 sigma (you can't change this value).

**\'GumbleFit\'**: (ms.info tag: \'gbl\')

-   Peak detection and determination of *int\_win* according to '\'Baseline\_dynamicRT'.

-   Selection of data points according to 'GaussFit'.

-   Calculation of the peak fit as a Gumble distribution.

-   Integration of the fitted signal within the sigma values you entered on the integration widget.

**\'Dbl\_Gumble\_1stPeak**\': (ms.info tag: \'dblgbl\_1st\')

-   See 'GumbleFit'.

-   Assumes a double peak with the second (smaller) peak sitting on the right shoulder of the first (larger) peak.

-   '\_1stPeak' the peak parameters of the first peak are returned by this function.

**\'Dbl\_Gumble\_2ndPeak\'**: (ms.info tag: \'dblgbl\_2nd\')

-   See 'GumbleFit' / 'Dbl\_Gumble\_1stPeak'

-   '\_2ndPeak' the peak parameters of the second peak are returned by this function.

**\'Dbl\_Gumble\_PeakSum\'**: (ms.info tag: \'dblgbl\_sum\')

-   See 'GumbleFit' / 'Dbl\_Gumble\_1stPeak' / 'Dbl\_Gumble\_2ndPeak'

-   'PeakSum' the peak parameters of both peaks are returned (e.g. sum of areas peak 1 + peak 2) by this function.

**\'Fix\_2point\_BL\'**: (ms.info tag: \'bl\_fix\')

-   the first and last data point in the specified retention time window are used to calculate a line between these points (=baseline).

-   Integration of all data points in the retention time window after baseline subtraction.

-   If a noise level was calculated, results are only returned if the detected peak height is greater than the 1.5-fold noise level (otherwise, the result is 'no peak found').

-   If the integrated intensity (peak area) is below zero, a 'no peak found' is returned as well.

-   since this method does not use peak detection, it also doesn't compensate a shift in retention time during a measurement series. On the one hand side, this might reduce precision -- but on the other hand side, signals that are difficult to integrate with other methods might be feasible with this method due to its simplicity.

### Noise calculation

To calculate a noise level on a specific m/Q, a retention time window (Left Boundary & Right Boundary, see Fig. 10) has to be defined. In this section of the x-axis, a 2^nd^ order polynomial fit is calculated. The noise level is then calculated as the 3-fold standard deviation of the differences (residuals) between fit and data points.

Data Analysis: Fragment Ratios
==============================

![15](https://raw.githubusercontent.com/MrFuppes/IDL_IAU_Chrom/master/doc/img/15_Window_FragR_Plots.png)

Fig. 15 -- Fragment ratio calculator widget with plots.

This tool can analyse ratios of different ions fragmented from a molecule. Prerequisite: chromatographic data and ms.info file loaded or experiment restored. Additionally requires: eifrags.txt - a config file that tells the software which signals to compare to analyse a specific substance. To match substance information from ms.info and eifrags.txt, substance names have to be spelled equally in both files (case-sensitive!).

![16](https://raw.githubusercontent.com/MrFuppes/IDL_IAU_Chrom/master/doc/img/16_WidFragRat.png)

Fig. 16 - FragRat Calc widget.

Configuration options on the widget

-   **Chromatogram**, **Substance** **Pre-set** and **Fragments**: set as desired.

-   **Use nominal masses**: if checkbox is activated, ion masses (m/q) specified as decimal numbers will be rounded to integer values ("nominal masses"). If the data contains nominal and accurate m/q vectors, nominal values will be preferred. If data contains only accurate values, make sure to uncheck the box.

-   **Try correct scanshift**: if activated, a simple correction is applied to the result that compensates the temporal shift of intensity values of different m/q signals. Note that this is only a "data correction", not a correction of possible spectral skewing from the MS. Has no effect on TOFMS data.

-   **t\_R min, t\_R max**: retention time window; where the signal of interest is found in the chromatogram(s). Set the window so that an unambiguous identification of the signal is possible.

-   **Peak int. sigma left/right**: Required setting if peak fits should be used to derive the ratio, see next point (Comparison). The software will try to detect a signal (peak) in the specified retention time window. Sigma left and right specify the data (in multiples of ½ FWHM of a fit estimate returned by the peak detection algorithm) that is used to calculate a Gaussian fit of the detected signal. Change these values to get a fit that resembles peak and baseline.

-   **Comparison**: either select "Baseline/P2P" to perform a point-to-point comparison of data points or "Fit2Fit" to compare fitted curves to retrieve the ratio.

-   **FragRat sigma left/right**: Determines which data points are used to calculate the ratio. Numbers entered are multiples of sigma, i.e. ½ FWHM of a Gaussian peak representing the signal (see peak integration).

Some hints to get useful results

-   "save experiment" option (main widget): fragment ratio results and config file will not be saved. Make sure to save results as .txt (option on fragrat calc widget)

-   fragrat sigma left/right can strongly influence the result. Make sure to use equal values here if comparing different measurements.

-   ***Special note*** on scanning-type mass spectrometers, switching and recording m/q sequentially: ion composition in the ion source might have changed during the time it takes the instrument to switch from one m/q to another ("spectral skew"). A point-2-point ratio consequently might be biased ("comparing apples with apple-like pears"). To minimise the inaccuracy introduced by spectral skew, e.g. only points should be compared that are not located at the steepest parts of the signal flanks (max. dI/dt). The configuration options expressed in multiples of sigma can help in that respect, try e.g. sigma \< 1 in case of doubt.

What the report-file (.txt) tells you

-   **File**: data file name

-   **m/q\_f1**: m/q of fragment 1

-   **m/q\_f2**: m/q of fragment 2

-   **ratio**: mean ratio of all ratios calculated (point-2-point comparisons)

-   **rsd**: relative standard deviation of all ratios calculated

-   **n\_dp**: number of data points used to calculate the mean ratio

-   **RTp1**: retention time at peak apex of fragment 1

-   **dRT**: temporal shift; retention time at peak apex of fragment 2 signal minus retention time at peak apex of fragment 1 signal

-   **peakint\_sl**: sigma multiplier used for peak integration

-   **peakint\_sr**: sigma multiplier used for peak integration

-   **fragrat\_sl**: sigma multiplier used for determination of the retention time window within which to calculate the ratio

-   **fragrat\_sr**: sigma multiplier used for determination of the retention time window within which to calculate the ratio

-   **a\_ratio**: ratio of (fitted) peak areas

-   **h\_ratio**: ratio of (fitted) peak heights

  [1 Overview and workflow 1]: #overview-and-workflow
  [2 Features 2]: #features
  [2.1 Technical Notes 2]: #technical-notes
  [2.2 Main widget: "File" tab 3]: #main-widget-file-tab
  [2.3 Main widget: "DataAnalysis" tab 4]: #main-widget-dataanalysis-tab
  [2.4 Main widget: "Viewer" tab 5]: #main-widget-viewer-tab
  [2.5 Main widget: "Advanced" tab (scripting) 6]: #_Toc501793635
  [2.6 Define your targets: "*ms.info*" config file 6]: #define-your-targets-ms.info-config-file
  [2.7 Define molecule fragments and ratios: "*fragments.txt"* config file 7]: #define-molecule-fragments-and-ratios-fragments.txt-config-file
  [3 Data analysis: Tools to get an impression 8]: #data-analysis-tools-to-get-an-impression
  [3.1 Viewer: Multiple Chroms (MCV) 8]: #viewer-multiple-chroms-mcv
  [3.2 Viewer: Multiple Masses (MMV) 9]: #viewer-multiple-masses-mmv
  [4 Data Analysis: Integrate! 10]: #data-analysis-integrate
  [4.1 Signal integration widget 11]: #signal-integration-widget
  [4.1.1 Integration widget: "Config" tab 11]: #integration-widget-config-tab
  [4.1.2 Integration widget: "BatchProcess" tab 12]: #integration-widget-batchprocess-tab
  [4.1.3 Integration widget: "Report" tab 13]: #integration-widget-report-tab
  [4.1.4 Integration widget: "Plot" tab 13]: #integration-widget-plot-tab
  [4.2 Signal integration: Settings 14]: #signal-integration-settings
  [4.2.1 Flagging & Delay 14]: #flagging-delay
  [4.2.2 Retention time and peak integration 14]: #retention-time-and-peak-integration
  [4.2.3 Integration: general tips 15]: #integration-general-tips
  [4.2.4 Integration: methods 17]: #integration-methods
  [4.2.5 Noise calculation 18]: #noise-calculation
  [5 Data Analysis: Fragment Ratios 19]: #data-analysis-fragment-ratios
  [1]: https://github.com/MrFuppes/IDL_IAU_Chrom/blob/master/doc/img/01_WidMain.png {width="2.625in" height="2.1745964566929135in"}
  [2]: media/image2.png {width="3.3958333333333335in" height="2.4772265966754157in"}
  [3]: media/image3.png {width="2.875in" height="2.323629702537183in"}
  [4]: media/image4.png {width="2.6875in" height="2.1831430446194227in"}
  [5]: media/image5.png {width="1.5625in" height="2.4358978565179354in"}
  [6]: media/image6.png {width="3.0520833333333335in" height="2.3566721347331585in"}
  [7]: media/image7.PNG {width="6.141936789151356in" height="2.7393208661417323in"}
  [8]: media/image8.PNG {width="5.946593394575678in" height="2.816389982502187in"}
  [9]: media/image9.PNG {width="6.458333333333333in" height="3.774288057742782in"}
  [10]: media/image10.png {width="2.1769695975503063in" height="4.943000874890639in"}
  [11]: media/image11.png {width="2.4864381014873143in" height="1.4083333333333334in"}
  [12]: media/image12.png {width="2.454341644794401in" height="1.4076367016622922in"}
  [13]: media/image13.png {width="2.274094488188976in" height="1.185952537182852in"}
  [14]: media/image14.PNG {width="6.3in" height="3.897222222222222in"}
  [15]: media/image15.PNG {width="6.3in" height="4.591666666666667in"}
  [16]: media/image16.PNG {width="3.2083333333333335in" height="4.757183945756781in"}
