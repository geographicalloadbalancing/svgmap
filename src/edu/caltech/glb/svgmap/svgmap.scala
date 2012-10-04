/*
Test program

To run, you must expand the stack size due to deep recursion in the XML library, e.g.:
exec env JAVA_OPTS='-Xss32M' scala -classpath 'anti-xml_2.9.1-0.3.jar' "$0" "$@"
!#
*/
package edu.caltech.glb.svgmap

import org.apache.commons.compress
import au.com.bytecode.opencsv
import java.io.InputStreamReader
import java.io
import scala.collection.JavaConverters._
import ucar.nc2

/** A simple test program that generates a map using some data. */
object Main {def main(args : Array[String]) = {
	// Read in the position for data centers and population centers
	// These values are in the latlng form. East and North semisphere are positive.
	// Read in the traces for solar, wind, and total power demand at each data center.
	// The values are normalized so that they are each in the same units.
	val (dc_loc_raw, state_loc_raw, solar, wind, total, cooling, brown) = {
		def read_file(fname : String) = new opencsv.CSVReader(new InputStreamReader(
			new compress.compressors.CompressorStreamFactory createCompressorInputStream
			  (ClassLoader getSystemResourceAsStream ("edu/caltech/glb/svgmap/indata/" + fname))
		)).readAll.asScala
		("dc_loc.csv.bz2", "state_loc.csv.bz2", "solar.csv.bz2", "wind.csv.bz2", "total.csv.bz2", "cooling.csv.bz2", "brown.csv.bz2") map read_file
	}

	val dcs : Seq[DataCenter] = {
		// Create the datacenter location list of WorldPt.
		val datacenter_location = dc_loc_raw map {case Array(lat, lng) ⇒ WorldPt(lat.toDouble, lng.toDouble)}
		
		datacenter_location.zipWithIndex map {case (dc_loc, dc) ⇒
			val solar_for_dc = solar map (_(dc))
			val wind_for_dc = wind map (_(dc))
			val cooling_for_dc = cooling map (_(dc))
			val total_for_dc = total map (_(dc))
			DataCenter(dc_loc, solar_for_dc zip wind_for_dc zip total_for_dc zip cooling_for_dc map {case (((sstr, wstr), tstr),cstr) ⇒
				val (s, w, t, c) = (sstr, wstr, tstr, cstr) map (_.toDouble) : (Double, Double, Double, Double)
				val max_demand : Double = 4.1335e+05	// scale input data
				// Clamp negative values to 0.
				DataCenterState(
					(	// demand
						/* energy usage for computing */ (t max 0),
						/* energy usage for cooling */ (c max 0)
					) map (_/max_demand),
					(	// supply
						/* solar */ (s max 0),
						/* wind */ (w max 0),
						/* brown */ (t + c - s - w) max 0
					) map (_/max_demand),
					/*The storage data. Now is @@@@*/
					(t max 0) / max_demand
				)
			})
			//throw(new Exception)
		}
	}

	// Create the population center location list of WorldPt
	val client_locs = state_loc_raw map {case Array(lat, lng) ⇒ WorldPt(lat.toDouble, lng.toDouble)}
	// Draw 1 line for each (client, dc) pair
	val lines = {
		def load_netcdf(var_name : String) : nc2.Variable = {
			val infile : io.InputStream = ClassLoader getSystemResourceAsStream ("edu/caltech/glb/svgmap/indata/routing.nc")
			nc2.NetcdfFile.openInMemory("routing.nc", com.google.common.io.ByteStreams toByteArray infile) findVariable var_name
		}
		val routing = load_netcdf("routingPlan")
		// Population center requests load over time. 48 by 1008
		val load_raw = load_netcdf("load")
		// The normalized version of load. For line width change purpose.
		val load_normalized = load_netcdf("load_normalized")
		
		dcs.zipWithIndex flatMap {case (DataCenter(dc_loc, _), dc) ⇒
			client_locs.zipWithIndex map {case (client_loc, client) ⇒ {
				// extract a vector section from a multidimensional netCDF array
				// section is the position of the vector to extract
				// inputMatrix is the variable name in the netCDF
				def extractData(section : String, inputMatrix : nc2.Variable) : Seq[Double] = {
					val buf = inputMatrix.read(section).getDataAsByteBuffer.asDoubleBuffer()
					buf.clear
					var arr = new Array[Double](buf.capacity)
					buf.get(arr, 0, arr.length)
					arr
				}
				val line_data = extractData(client + "," + dc + ", :", routing)
				val load_data = extractData(": ," + client, load_raw)
				val load_data_normalized = extractData(": ," + client, load_normalized)
				// load_data is longer; discard the rest
				// the line opacity is the fraction of the total load of a population center
				// i.e. lambda_{ij}/lambda{j}
				val line_opacity = (line_data, load_data).zipped map (_/_)
				val line_width = load_data_normalized map {_*7}
				Line(client_loc, dc_loc, (line_opacity, line_width).zipped map LineState)
			}}
		}
	}
	
	val anim_time_per_step : Double = 0.2 /*s*/
	val world_time_per_step : Double= 10 /*min*/ * 60 /*s / min*/
	val dccolors = DataCenterColors(("yellow", "blue"), ("#0A8", "#0F0", "brown"), "red")
	val dclegend = DataCenterLegendText(("power demand", "cooling"), ("solar availability", "wind availability", "grid usage"), "storage")
	
	val line_plot_stats = {
		val totals0 = dcs(0).stats map {_ ⇒ 0.0}
		// Calculates the sum (or other f) over all DCs, for each time t, returning a list of the totals over time.
		def foldDCs(stat_selector : DataCenterState ⇒ Double, f : (Double, Double) ⇒ Double = (_+_)) =
			(totals0 /: dcs){case (totals, dc) ⇒ (totals, dc.stats map stat_selector).zipped map f}
		// Scale by the max total supply
		val max_Σ_supply : Double = foldDCs(_.supplies.asSeq.sum).max
		// Line plot needs to be changed.
		List(
			LinePlotStat(/* Total energy demand of all DCs */"#FFFF00", foldDCs(_.demands match {case (t, c) ⇒ t}) map (_/max_Σ_supply)),
			LinePlotStat(/* Total brown energy usage of all DCs */"brown", foldDCs(_.supplies match {case (_, _, g) ⇒ g}) map (_/max_Σ_supply)),
			LinePlotStat(/* Total wind energy available */"#0F0", foldDCs(_.supplies match {case (s, w, _) ⇒ w}) map (_/max_Σ_supply)),
			LinePlotStat(/* Total solar energy available */"#0A8", foldDCs(_.supplies match {case (s, w, _) ⇒ s}) map (_/max_Σ_supply)),
		     LinePlotStat(/* Total energy used for cooling of all DCs */"blue", foldDCs(_.demands match {case (t, c) ⇒ c}) map (_/max_Σ_supply))
		)
	}
	
	System.out write generate_visualization(anim_time_per_step, world_time_per_step, dclegend, dccolors, dcs, lines, line_plot_stats)

}}