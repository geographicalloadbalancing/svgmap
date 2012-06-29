/*
Test program

To run, you must expand the stack size due to deep recursion in the XML library, e.g.:
exec env JAVA_OPTS='-Xss32M' scala -classpath 'anti-xml_2.9.1-0.3.jar' "$0" "$@"
!#
*/
package edu.caltech.glb.svgmap

import scala.collection.GenSeq

/** A simple test program that generates a map using sample data. */
object Main extends App {
	def randomStats = {
		def randDC = DataCenterState(List(math.random, math.random, math.random))
		(GenSeq fill 20)(randDC).seq
	}
	def randomLineStat = {
		def randLine = LineState(math.random)
		(GenSeq fill 20)(randLine).seq
	}
	val dcs = List(
		DataCenter(WorldPt(41, -104), randomStats),
		DataCenter(WorldPt(34, -84), randomStats),
		DataCenter(WorldPt(42, -87), randomStats)
	)
	val lines = List(
		Line(WorldPt(40, -90), WorldPt(35, -100), randomLineStat)
	)
	System.out write generate_visualization(dcs, lines)
}