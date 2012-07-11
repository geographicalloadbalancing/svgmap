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


	val dcs : Seq[DataCenter] = {
		/** From geo_capacity.m */
		val datacenter_location : Seq[WorldPt] = List(
			WorldPt(37, -120), WorldPt(47, -120), WorldPt(44, -120), WorldPt(40, -90), WorldPt(31, -83),
			WorldPt(38, -78), WorldPt(31, -99), WorldPt(28, -81), WorldPt(35, -79), WorldPt(33, -81))
		
		// Raw traces
		val (solar, wind) = {
			def read_file(fname : String) = new opencsv.CSVReader(new InputStreamReader(
				new compress.compressors.CompressorStreamFactory createCompressorInputStream
					(ClassLoader getSystemResourceAsStream ("edu/caltech/glb/svgmap/indata/" + fname))
			)).readAll.asScala
			("solar_supply_week.csv.bz2", "wind_supply_week.csv.bz2") map read_file
		}
		
		datacenter_location.zipWithIndex map {case (dc_loc, dc) ⇒ {
			val solar_for_dc = solar map (_(dc))
			// There are 2 solar readings for each wind reading. For now, double each wind reading to match them:
			val wind_for_dc = {
				val raw = wind map (_(dc))
				// interleave with self
				raw zip raw flatMap {case (x, y) ⇒ List(x, y) }
			}
			DataCenter(dc_loc, solar_for_dc zip wind_for_dc map {case (s, w) ⇒
				// scale input data (ranges: solar ∈ [-17.13, 721.47], wind ∈ [0, 774.9])
				// sqrt so that the value is proportional to the area.
				// Clamp negative values to 0.
				DataCenterState(List(math.sqrt(0.0014 * (s.toDouble max 0)), math.sqrt(0.0014 * w.toDouble), 0))
			})
		}}
	}
	
	/** Also hard-coded for now. Values taken from <code>geo_capacity.m</code>. */
	val client_locs : Seq[WorldPt] = List(WorldPt(32, -87), WorldPt(34, -119), WorldPt(35, -92), WorldPt(37, -120), WorldPt(39, -105), WorldPt(41, -73),
		WorldPt(39, -76), WorldPt(28, -81), WorldPt(31, -83), WorldPt(44, -114), WorldPt(40, -89), WorldPt(40, -86),
		WorldPt(41, -93), WorldPt(38, -98), WorldPt(38, -85), WorldPt(31, -92), WorldPt(45, -69), WorldPt(39, -77),
		WorldPt(42, -72), WorldPt(43, -84), WorldPt(45, -93), WorldPt(33, -90), WorldPt(38, -92), WorldPt(47, -110),
		WorldPt(41, -99), WorldPt(39, -116), WorldPt(43, -71), WorldPt(40, -74), WorldPt(34, -106), WorldPt(43, -76),
		WorldPt(35, -79), WorldPt(47, -100), WorldPt(40, -83), WorldPt(35, -97), WorldPt(44, -120), WorldPt(41, -78),
		WorldPt(42, -71), WorldPt(32, -80), WorldPt(44, -100), WorldPt(35, -86), WorldPt(31, -99), WorldPt(40, -112),
		WorldPt(44, -73), WorldPt(38, -79), WorldPt(47, -121), WorldPt(39, -81), WorldPt(44, -89), WorldPt(43, -107))
	
	def randomLineStat = {
		def randLine = LineState(math.random)
		(GenSeq fill 20)(randLine).seq
	}

	// Draw 1 line for each (client, dc) pair
	val lines = {
		val routing : nc2.Variable = {
			val infile : io.InputStream = ClassLoader getSystemResourceAsStream ("edu/caltech/glb/svgmap/indata/routing.nc")
			nc2.NetcdfFile.openInMemory("routing.nc", com.google.common.io.ByteStreams toByteArray infile) findVariable "routingPlan"
		}
		// Population center requests load over time. 48 by 1008
		val load : nc2.Variable = {
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
				val load_data = extractData(": ," + client, load)
				val load_data_normalized = extractData(": ," + client, load_normalized)
				// load_data is longer; discard the rest
				// the line opacity is the fraction of the total load of a population center
				// i.e. lambda_{ij}/lambda{j}
				val line_opacity = line_data zip load_data map {case (line, load) ⇒ line / load} map LineState
				// the line width is 6 * sqrt(load of a population center)
				val line_width = load_data_normalized map math.sqrt map {_*6} map LineState
				Line(client_loc, dc_loc, line_opacity, line_width)
			}}
		}
	}
	System.out write generate_visualization(dcs, lines)

}}