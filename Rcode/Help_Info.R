#'==============================================================================
#'  Help_info.R
#'  This file contains objects used for popup info modules 
#'==============================================================================
# Data Error  -------------------------------------------------------------------
Info_data_input_Error_title <- "Input data contain non-numbers"
Info_data_input_Error <- HTML("

<p>The some data are read as <strong> character </strong> not as <strong> numbers </strong> (numberic, integer).</p>
<p>This is most likely casused by</p> 
<ol>
	<li>The data were formated with thousands comma <strong>","</strong> (e.g., <strong>2,150</strong>)</li>
	<li>The data have some <strong> footnote/superscript </strong> (e.g., <strong>2125<sup>a</sup></strong>)</li>
	<li>The data have additional non-number colums </li>
</ol>

Please correct errors 
<ol>
	<li>Reformat data to general</li>
	<li>Remove character footnote/superscripts</li>
	<li>Remove non-number columns </li>
</ol>
                              ") # End HTML

# Age Error  -------------------------------------------------------------------
Info_data_Age_Error_title <- "Incorrect Age colmun names"
Info_data_Age_Error <- HTML("
<p>The Run data require Age column named as: A+run age (e.g.,<strong>A1,A2,A3......</strong>) or a+European scale&nbsp; age (e.g., <strong>a0.1, a1.1, a1.4, ....</strong>).</p>
Please correct the age column name.
                              ") # End HTML

# Data Input -------------------------------------------------------------------
Info_data_input_title <- "Data Type Options: Help for further information"
Info_data_input <- HTML("
<h4><strong>Run Data</strong></h4>
<p>Run Data file columns must be (from the first colum to the last)</p>
<ol>
	<li>Year,</li>
	<li>Spwner&nbsp; (Escapement) size</li>
	<li>Run size</li>
	<li>Run by age or proportion: <strong>Age column name MUST be written as</strong>
	<ul>
		<li>A+run age (e.g., A1,A2,A3......)</li>
		<li>a+European scale&nbsp; age (e.g., a0.1, a1.1, a1.4, ....)</li>
	</ul>
	</li>
	<li>CV and age effective sample size for State-Space Modeling with <strong>specified column name</strong>
	<ul>
		<li><strong>cv_N: (Run cv)</strong></li>
		<li><strong>cv_E:&nbsp; (Spawner/Escapement cv)</strong></li>
		<li><strong>cv_H: (Harvest cv)</strong></li>
		<li><strong>efn:</strong> <strong>(Effective sample size of age composition) (MUST BE INTEGER)</strong></li>
	</ul>
	</li>
</ol>

<h4><strong>S-R Data</strong></h4>
<p>S-R Data file columns must be</p>
<ol>
	<li>Year</li>
	<li>Spawner (Escapement) size</li>
	<li>Recruitment size</strong></li>
</ol>

<h4><strong>Escapement only Data</strong></h4>
<p>Escapement Only Data file columns must be</p>
<ol>
	<li>Year</li>
	<li>Spawner (Escapement) size</li>
</ol>
") # End HTML

# Pool Age  -------------------------------------------------------------------
Info_pool_title <- "Age Pooling Options"
Info_pool <- HTML("
<p><h4>Pool Age Options</h4></p>
<p>Age pooling (reducing minor age classes) has two options:</p>

<ol>
	<li><strong>Pool Age: Sum proprtion of the minor ages with adjacent age proprion</strong></li>
	<li><strong>Drop Age (uncheck Pool Age): Delete proprtion of the minor ages and restandardize the proportion of remaing ages </strong></li>
</ol>

<p><strong><span>Pool Age vs. Drop Age option Example</span></strong></p>
<table border=1>
	<thead>
		<tr>
			<th>Option</th>
			<th>Age 3</th>
			<th>Age 4</th>
			<th>Age 5</th>
			<th>Age 6</th>
			<th>Age 7</th>
		</tr>
	</thead>
	<tbody>
		<tr>
			<td>&nbsp; Raw Age</td>
			<td>&nbsp; 0.05</td>
			<td>&nbsp; 0.15</td>
			<td>&nbsp; 0.35</td>
			<td>&nbsp; 0.40</td>
			<td>&nbsp; 0.05</td>
		</tr>
		<tr>
			<td>&nbsp; Pool Age</td>
			<td>&nbsp;</td>
			<td>&nbsp; <strong>0.20</strong> =0.05+0.15</td>
			<td>&nbsp; <strong>0.35</strong></td>
			<td>&nbsp; <strong>0.45</strong> = 0.05+0.40</td>
			<td>&nbsp;</td>
		</tr>
		<tr>
			<td>&nbsp; Drop Age</td>
			<td>&nbsp;</td>
			<td>&nbsp; <strong>0.17</strong> =0.15/0.9</td>
			<td>&nbsp; <strong>0.39</strong> = 0.35/0.9</td>
			<td>&nbsp; <strong>0.44</strong> = 0.40/0.9</td>
			<td>&nbsp;</td>
		</tr>
	</tbody>
</table>
         ") # End HTML

#  Outlier Removed -------------------------------------------------------------
Info_Outlier_title <- "Remove Outliers"
Info_Outlier <- HTML("
<h4> This option removes outliers data from the Escapement Goal Analyses  </h4>
<ol>
	<li>beta is negative (beta &lt; 0)</li>
	<li><strong>log-odd ratio: ln(beta/(1-beta)) </strong> is outlier</li>
</ol>
<p> Please see Outlier summary table for the number and percentage of outliers removed <p>

<p>Outlier is defined as posteriors that are above <strong><em>q</em><sub>0.75</sub>+1.5&sdot;IQR</strong> or below <strong><em>q</em><sub>0.25</sub>&minus;1.5&sdot;IQR</strong> </p>
<p>(where&nbsp;<strong><em>q</em><sub>0.25</sub></strong> and&nbsp;<strong><em>q</em><sub>0.25</sub></strong>&nbsp; correspond to first and third quartile respectively, and IQR is the difference between the third and first quartile)</p>
") # End HTML


# Median vs. Mean  -------------------------------------------------------------
Info_md_title <- "Median vs. Mean Recruit"
Info_md <- HTML("
        <b>Median Recruit</b><br/>
          Expected Median recruitement. Use lnalpha for SR model to #estimate biological reference points and profile.<br/>
          Expected Mean recruitement. Use lnalpha.c for SR model to #estimate biological reference points and profile.<br/>
         ") # End HTML

#  MSE Management Option -------------------------------------------------------
Info_MSE_manage_title <- "Management Strategies"
Info_MSE_manage <- HTML("
<h4>Fishery ManagementStrategies</h4>
<p>Management strategy is based on Projecte Run size (<strong>PR</strong>), Minimum Harvest (<strong>MinH</strong>), Maximum Harvest (<strong>MaxH</strong>), Target Harvest 
Rate (<strong>THR</strong>),Maximum Harvest Rate (<strong>MHR</strong>), and Target Escapement (<strong>TE</strong>)</p>

<p>Escapement Goal :&nbsp; Fishery opens when run is above escapement target</p>

<ol>
	<li>PR &lt; TE:&nbsp;&nbsp; No fishery</li>
	<li>PR &gt; TE:&nbsp; Fishery opens
	<ol>
		<li>Set Projected Yield (<strong>PY</strong>): PR- TE</li>
		<li>Set Projected Harvest Rate (<strong>PHR</strong>):&nbsp; (PY)/PR</li>
		<li>Harvest: Run*PHR,Run* MHR, or MaxH&nbsp; whichever the smallest&nbsp;</li>
	</ol>
	</li>
</ol>

<p>Harvest Rate:&nbsp; Fishery opens when run is above the Target Escapement and Harvest target is determined based on target harvest rate</p>
<ol>
	<li>PR &lt; TE:&nbsp;&nbsp; No fishery</li>
	<li>PR &gt; TE:&nbsp; Fishery opens
	<ol>
		<li>Set Projected Yield (<strong>PY</strong>): PR- TE</li>
		<li>Set Projected Harvest Rate (<strong>PHR</strong>):&nbsp; (PY)/PR</li>
		<li>Harvest: Run*PHR,Run* MHR, or MaxH&nbsp; whichever the smallest&nbsp;</li>
	</ol>
	</li>
</ol>

<p>Constatnt Harvest : Fishery is open regardless run size</p>

<ul>
	<li>Harvest:&nbsp; up to the <strong>Run*(MHR)</strong> or <strong>MaxH</strong> whichever smaller</li>
</ul>



<p>Hybrid:&nbsp;&nbsp; MinH occur regardless run size</p>

<ol>
	<li>PR &lt; TE&nbsp; &nbsp;
	<ol>
		<li>Harvest: MinH or Run*MHR whichever smaller</li>
	</ol>
	</li>
	<li>PR &gt; TE
	<ol>
		<li>PY &lt; MinH:&nbsp; MinH</li>
		<li>PY &gt; MinH:&nbsp; PHR*Run, MHR*Run, or MaxH&nbsp; whichever the smallest&nbsp;</li>
	</ol>
	</li>
  ")  # HTML

#  MSE Harvest Strategy --------------------------------------------------------
