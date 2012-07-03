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
		val datacenter_location : Seq[WorldPt] = List(WorldPt(37, -120), WorldPt(47, -120), WorldPt(44, -120), WorldPt(40, -90), WorldPt(31, -83), WorldPt(38, -78), WorldPt(31, -99), WorldPt(28, -81), WorldPt(35, -79), WorldPt(33, -81))
		
		// Raw traces
		val (solar, wind) = {
			def read_file(fname : String) = new opencsv.CSVReader(new InputStreamReader(new compress.compressors.CompressorStreamFactory createCompressorInputStream (ClassLoader getSystemResourceAsStream ("edu/caltech/glb/svgmap/indata/" + fname)))).readAll.asScala
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
				// scale input data
				DataCenterState(List(-0.1 * s.toDouble, 0.1 * w.toDouble, 0))
			})
		}}
	}
	
	def randomLineStat = {
		def randLine = LineState(math.random)
		(GenSeq fill 20)(randLine).seq
	}
	val lines = List(
		Line(WorldPt(40, -90), WorldPt(35, -100), randomLineStat)
	)
	System.out write generate_visualization(dcs, lines)
}}