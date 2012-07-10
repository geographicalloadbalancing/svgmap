/*
Contains functions that generate an SVG visualization based on input data.
*/

package edu.caltech.glb
package object svgmap extends svgmap.svgmap_types {

import com.codecommit.antixml._

/** Convenience function to allow easy use of Anti-XML in XML literals.
<p>Usage: {@code
val antixml = <child attr="val">...</child>.convert
<xmlliteral>{U(antixml)}</xmlliteral>.convert
}</p>**/
private def U(antixml : Node) : scala.xml.NodeSeq = scala.xml.XML.loadString(antixml.toString)
private def U(antixml : Group[Node]) : scala.xml.NodeSeq = scala.xml.XML.loadString(antixml.toString)

/** The original blank map */
val BACKGROUND_MAP : Elem = {
	XML fromInputStream (ClassLoader getSystemResourceAsStream "edu/caltech/glb/svgmap/base_map.svg")
}

/** The total time the animation should run, if there are <var>n</var> time steps to display. */
private def animation_duration(n : Int) : String = "%.1fs" format (0.5 * n)

/** Method to use for animation. (For example, "linear" ⇒ smoothly interpolate between data points; "discrete" ⇒ jump) */
val CALC_MODE = "linear"

/** Draws an animated data center indicator at the specified coordinates, displaying the given stats over time.
@return an SVG fragment to be inserted into the SVG document. */
def draw_datacenter(dc : DataCenter) : Group[Node] = {
	val DataCenter(coords, stats) = dc
	val DevicePt(x, y) = coords.toDevicePt
	
	// The various parts of the data center indicator are specified relative to (0, 0); they are then translated to the appropriate spot.
	
	// Dot indicating the exact location of the data center
	val dot = <circle cx="0" cy="0" r="2" style="fill:rgb(0,0,0)" />.convert
	
	// Example animation based on http://www.w3.org/TR/2011/REC-SVG11-20110816/animate.html#AnimationElementsExample
	val label = <text x="15" y="0">
		I'm a data center!
	</text>.convert
	
	// Draw the sector chart, indicating some data center statistics.
	/*val sector_stats = stats map (_ match {
		case DataCenterState(a, b, c) ⇒ List(a, b, c)
	})*/
	val sector_stats = stats map (_.sector_stats)
	val sector_g = <g>{
		val NUM_SECTORS = sector_stats(0).length
		// Evenly distributed around the color wheel
		val COLORS = 0 until NUM_SECTORS map (360.0 / NUM_SECTORS * _) map ("hsla(%.4f, 100%%, 70%%, 0.7)" format _)
		0 until NUM_SECTORS map { s ⇒ {
			/** Radius of a full sector (for value = 1.0) */
			val r = 30
			
			// Calculate sector's endpoints
			val (start_angle, end_angle) = (s, s + 1) map (_ * 2 * math.Pi / NUM_SECTORS)
			val (start_x, end_x) = (start_angle, end_angle ) map (+r * math.sin(_))
			val (start_y, end_y) = (start_angle, end_angle ) map (-r * math.cos(_))
			
			// Defines outline of sector
			val path : String = (
				/* center */ "M 0,0" +
				/* draw line */ " L " + ("%.4f" format start_x) + "," + ("%.4f" format start_y) +
				/* draw arc */ " A " + r + "," + r + " 0 0 1 " + ("%.4f" format end_x) + "," + ("%.4f" format end_y) +
				/* close with line */ " Z"
			)
			// Animate. We use <animateTransform> to scale the sector according to the corresponding value.
			<path d={path} style={"fill: " + COLORS(s) + "; stroke: black; stroke-width: 1px; vector-effect: non-scaling-stroke;"}>
				<animateTransform
					attributeName="transform" attributeType="XML"
					type="scale" calcMode={CALC_MODE}
					values={sector_stats map {"%.2f" format _(s)} mkString ";"}
					dur={animation_duration(stats.length)} fill="freeze"
				/>
			</path>
		}}
	}</g>.convert
	
	// Translate everything to the desired data center location
	<g transform={"translate(" + ("%.1f" format x) + "," + ("%.1f" format y) + ")"}>
		{U(dot)}{U(label)}{U(sector_g)}
	</g>.convert
}

/** Draws an animated line, displaying the given stats over time.
@return an SVG fragment to be inserted into the SVG document. */
def draw_line(line : Line) : Group[Node] = {
	val Line(p1, p2, stat) = line
	val dp1 = p1.toDevicePt
	val dp2 = p2.toDevicePt
	<line x1={"%.1f" format dp1.x} x2={"%.1f" format dp2.x} y1={"%.1f" format dp1.y} y2={"%.1f" format dp2.y}
		 style="stroke: black; stroke-width: 1px;">
		<animate
			attributeName="opacity"
			calcMode={CALC_MODE}
			values={stat map {"%.2f" format _.line_stat} mkString ";"}
			dur={animation_duration(stat.length)} fill="freeze"
		/>
	</line>.convert
}


/** Generates an SVG map visualization according to the provided data. */
def generate_visualization(dcdata : Seq[DataCenter], lineData : Seq[Line]) : Array[Byte] = {
	val overlay = <g id="overlay">
		{lineData map (line ⇒ U(draw_line(line)))}
		{dcdata map (dc ⇒ U(draw_datacenter(dc)))}
	</g>.convert
	
	// Append to the svg document as child
	val doc = BACKGROUND_MAP.copy(children = BACKGROUND_MAP.children :+ overlay)
	
	val os = new java.io.ByteArrayOutputStream
	XMLSerializer(outputDeclaration = true).serializeDocument(doc, os)
	os.toByteArray
}

}
