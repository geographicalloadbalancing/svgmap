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
import scala.collection.JavaConverters._

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
				DataCenterState(List(0.0014 * s.toDouble, 0.0014 * w.toDouble, 0))
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
	val lines = dcs flatMap {case DataCenter(dc_loc, _) ⇒
		client_locs map (client_loc ⇒ Line(client_loc, dc_loc, randomLineStat))
	}
	System.out write generate_visualization(dcs, lines)
}}