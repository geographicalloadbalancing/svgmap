/*
Test program

To run, you must expand the stack size due to deep recursion in the XML library, e.g.:
exec env JAVA_OPTS='-Xss32M' scala -classpath 'anti-xml_2.9.1-0.3.jar' "$0" "$@"
!#
*/
package edu.caltech.glb.svgmap

import scala.collection.GenSeq
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
	val (dc_loc_raw, state_loc_raw, solar, wind, total) = {
		def read_file(fname : String) = new opencsv.CSVReader(new InputStreamReader(
			new compress.compressors.CompressorStreamFactory createCompressorInputStream
			  (ClassLoader getSystemResourceAsStream ("edu/caltech/glb/svgmap/indata/" + fname))
		)).readAll.asScala
		("dc_loc.csv.bz2", "state_loc.csv.bz2", "solar.csv.bz2", "wind.csv.bz2", "total.csv.bz2") map read_file
	}

	val dcs : Seq[DataCenter] = {
		// Create the datacenter location list of WorldPt.
		val datacenter_location = dc_loc_raw map {case Array(lat, lng) ⇒ WorldPt(lat.toDouble, lng.toDouble)}


		datacenter_location.zipWithIndex map {case (dc_loc, dc) ⇒ {
			val solar_for_dc = solar map (_(dc))
			// There are 2 solar readings for each wind reading. For now, double each wind reading to match them:
			val wind_for_dc = {
				val raw = wind map (_(dc))
				// interleave with self
				raw zip raw flatMap {case (x, y) ⇒ List(x, y) }
			}
			val total_for_dc = total map (_(dc))
			DataCenter(dc_loc, solar_for_dc zip wind_for_dc zip total_for_dc map {case ((sstr, wstr), tstr) ⇒ {
				val (s, w, t) : (Double, Double, Double) = (sstr, wstr, tstr) map (_.toDouble)
				val max_demand : Double = 4.1335e+05	// scale input data
				// Clamp negative values to 0.
				DataCenterState(
					/* demand */ (t max 0) / max_demand, (
						/* solar */  (s max 0),
						/* wind */  (w max 0),
						/* brown */ (t - s - w) max 0
				) map (_/max_demand))
			}})
		}}
	}

	// Create the population center location list of WorldPt
	val client_locs = state_loc_raw map {case Array(lat, lng) ⇒ WorldPt(lat.toDouble, lng.toDouble)}
	// Draw 1 line for each (client, dc) pair
	val lines = {
		val routing : nc2.Variable = {
			val infile : io.InputStream = ClassLoader getSystemResourceAsStream ("edu/caltech/glb/svgmap/indata/routing.nc")
			nc2.NetcdfFile.openInMemory("routing.nc", com.google.common.io.ByteStreams toByteArray infile) findVariable "routingPlan"
		}
		// Population center requests load over time. 48 by 1008
		val load_raw : nc2.Variable = {
			val infile : io.InputStream = ClassLoader getSystemResourceAsStream("edu/caltech/glb/svgmap/indata/routing.nc")
			nc2.NetcdfFile.openInMemory("routing.nc", com.google.common.io.ByteStreams toByteArray infile) findVariable "load"
		}
		// The normalized version of load. For line width change purpose.
		val load_normalized : nc2.Variable = {
			val infile : io.InputStream = ClassLoader getSystemResourceAsStream("edu/caltech/glb/svgmap/indata/routing.nc")
			nc2.NetcdfFile.openInMemory("routing.nc", com.google.common.io.ByteStreams toByteArray infile) findVariable "load_normalized"
		}
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
				val line_opacity = line_data zip load_data map {case (line, load) ⇒ line / load}
				val line_width = load_data_normalized map {_*7}
				Line(client_loc, dc_loc, (line_opacity zip line_width) map {case (o, w) ⇒ LineState(o, w)})
			}}
		}
	}
	
	val anim_time_per_step : Double = 0.2 /*s*/
	val world_time_per_step : Double= 5 /*min*/ * 60 /*s / min*/
	
	val dccolors = DataCenterColors("yellow", ("#08F", "#0F0", "brown"))
	
	System.out write generate_visualization(anim_time_per_step, world_time_per_step, dccolors, dcs, lines)

}}