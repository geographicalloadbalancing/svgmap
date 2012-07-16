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

/**
The total time the animation should run, if there are <var>n</var> time steps to display.
 @return the total time, as a string that can be inserted into the SVG
*/
private def animation_duration(anim_time_per_step : Double, n : Int) : String = "%.4fs" format (anim_time_per_step * n)

/** Method to use for animation. (For example, "linear" ⇒ smoothly interpolate between data points; "discrete" ⇒ jump) */
val CALC_MODE = "linear"

/**
Draws a small dot indicating the exact location of some object.
@param coords the coordinates of the dot to draw
*/
def draw_dot(coords : WorldPt) : Elem = {
	val DevicePt(x, y) = coords.toDevicePt
	<circle cx={"%.4f" format x} cy={"%.4f" format y} r="2" style="fill:rgb(0,0,0)" />.convert
}

/** Draws an animated data center indicator at the specified coordinates, displaying the given stats over time.
@return an SVG fragment to be inserted into the SVG document. */
def draw_datacenter(anim_time_per_step : Double, dc : DataCenter, colors : DataCenterColors) : Group[Node] = {
	val DataCenter(coords, stats) = dc
	val DevicePt(x, y) = coords.toDevicePt
	
	// The various parts of the data center indicator are specified relative to (0, 0); they are then translated to the appropriate spot.
	
	/*
	Draw the sector chart, indicating some data center statistics.
	
	The current design is a “supply‒demand” sector chart.
	The chart is split into demand (left) and supply (right) semicircles.
	The left half is a single large sector whose area represents the DC's gross electricity demand given its current load.
	The right half's overall area represents the amount of energy the DC is currently producing or buying from the grid.
	It is split into multiple sectors of varying angular widths, proportional the energy from various sources (wind power available, solar available, purchased from grid).
	Hence the area of each sector represents the amount of electricity consumed or produced from each source.
	Each data center has a ring drawn around it. The size of the ring matches the maximum of the power demand (according to server load) at this data center.
	Thus, when the supply side extends beyond the ring, it indicates that the DC is generating more power than the DC can use under full load (well technically, assuming that it reaches full utilization at some point during the animation).
	*/
	val supply_sector_stats = stats map (_.supplies)
	val sector_g = <g style="opacity: 0.7;">{
		val NUM_SUPPLY_SECTORS = supply_sector_stats(0).productArity
		val SUPPLY_COLORS = colors.supplies
		/** Radius of a full sector (for value = 1.0) */
		val r = 30
		
		// Draws a single static sector of the sector chart
		def draw_sector(start_angle : Double, end_angle : Double, color : String) : Elem = {
			// Calculate sector's endpoints
			val (start_x, end_x) = (start_angle, end_angle) map (+r * math.cos(_))
			val (start_y, end_y) = (start_angle, end_angle) map (-r * math.sin(_))
			
			// Defines outline of sector
			val path : String = (
				/* center */ "M 0,0" +
				/* draw line */ " L " + ("%.4f" format start_x) + "," + ("%.4f" format start_y) +
				// Set sweep-flag to 0 (to draw the arc in the standard direction)
				/* draw arc */ " A " + r + "," + r + " 0 0 0 " + ("%.4f" format end_x) + "," + ("%.4f" format end_y) +
				/* close with line */ " Z"
			)
			
			<path
				d={path}
				style={"fill: " + color + "; stroke: black; stroke-width: 1px; vector-effect: non-scaling-stroke;"}
			/>.convert
		}
		// Animates the shape in radius so that the sector will have area proportional to areas.
		// We use <animateTransform> to scale the sector according to the corresponding value.
		// sqrt so that the area is proportional to the value
		implicit def animate_radius_conv(shape : Elem) = new {
			def animate_radius(areas : Seq[Double]) : Elem =
				shape addChild <animateTransform
					attributeName="transform" attributeType="XML"
					type="scale" calcMode={CALC_MODE}
					values={areas map {"%.2f" format math.sqrt(_)} mkString ";"}
					dur={animation_duration(anim_time_per_step, stats.length)} fill="freeze"
				/>.convert
		}
		// Animates the shape, rotating by the specified angle in degrees.
		implicit def animate_rotate_conv(shape : Elem) = new {
			def animate_rotations(angles_deg : Seq[Double]) : Elem =
				shape addChild <animateTransform
					attributeName="transform" attributeType="XML"
					type="rotate" calcMode={CALC_MODE}
					values={angles_deg map {"%.2f" format _} mkString ";"}
					dur={animation_duration(anim_time_per_step, stats.length)} fill="freeze"
					/>.convert
		}
		
		val demand_side : Elem = draw_sector(0.5 * math.Pi, 1.5 * math.Pi, colors.demand) animate_radius
			(stats map (_.demand))
		
		val supply_side : Elem = {
			val supply_totals = supply_sector_stats map (_.asSeq.sum)
			/** Fraction of available energy each sector represents */
			val supply_fracs = supply_sector_stats zip supply_totals map {case (sstats, total) ⇒ sstats map (_ / total)}
			val supply_sectors = (0 until NUM_SUPPLY_SECTORS map { s ⇒ {
				/*
				Conceptually, what we want to do here is an angular scaling on the sector.
				But SVG only supports <em>affine</em> transformations.
				As a workaround, we draw each sector on the full right side of the pie,
				and <em>rotate</em> it the appropriate angle to expose the desired angle.
				Then use clipping so that only the real chart (right half) displays.
				*/
				// Each one starts as a semicircle on the right side
				val (start_angle, end_angle) : (Double, Double) = (-0.5, 0.5) map (_ * math.Pi )
				
				val unrotated_sector = draw_sector(start_angle, end_angle, SUPPLY_COLORS.asSeq(s))
				// Animate rotating sector to correct position
				// At each point in time, rotate by the sum of values for sectors (0 until s)
				val θs : Seq[Double] = supply_fracs map (_.asSeq.slice(0, s).sum)
				unrotated_sector animate_rotations (θs map (-_ * 180.0/* svg uses degrees */))
			}})
			// Clip so that only right side is visible.
			// Then draw a stroke around the entire semicircle (to create the boundary on the clipped sides).
			val supply_sector_g = <g>
				<g clip-path="url(#dcSectorChartSupplySideClip)">{supply_sectors map U}</g>
				{U(draw_sector(-0.5 * math.Pi, 0.5 * math.Pi, "transparent"))}
			</g>.convert
			// Scale the sectors as a group    
			supply_sector_g animate_radius supply_totals
		}
		
		/** Relative radius of the ring according to the maximum energy demand of this DC. sqrt for equal area.*/
		val ringSizeFactor = math.sqrt((stats map (_.demand)).max)
		val bounding_circle = <circle
			cx="0" cy="0" r={(r * ringSizeFactor).toString}
			style="fill: none; stroke: black; stroke-width: 1px; opacity: 0.3;"
		/>
		List(U(demand_side), U(supply_side), bounding_circle)
	}</g>.convert
	
	// Translate everything to the desired data center location
	<g transform={"translate(" + ("%.1f" format x) + "," + ("%.1f" format y) + ")"}>
		{U(sector_g)}
	</g>.convert
}

/**
Draws an animated line, displaying the given stats over time.
The <em>start</em> of the line will be indicated with a dot.
@return an SVG fragment to be inserted into the SVG document. */
def draw_line(anim_time_per_step : Double, line : Line) : Group[Node] = {
	val Line(p1, p2, stats) = line
	val dp1 = p1.toDevicePt
	val dp2 = p2.toDevicePt
	<g>
		{U(draw_dot(p1))}
		<line x1={"%.1f" format dp1.x} x2={"%.1f" format dp2.x} y1={"%.1f" format dp1.y} y2={"%.1f" format dp2.y}
			 style="stroke: hsl(0, 0%, 50%); stroke-width: 10px;">
			<animate
				attributeName="opacity"
				calcMode={CALC_MODE}
				values={stats map {"%.2f" format _.opacity} mkString ";"}
				dur={animation_duration(anim_time_per_step, stats.length)} fill="freeze"
			/>
			<animate
				attributeName="stroke-width"
				calcMode={CALC_MODE}
				values={stats map {"%.4f" format _.width} mkString ";"}
				dur={animation_duration(anim_time_per_step, stats.length)} fill="freeze"
			/>
		</line>
	</g>.convert
}

/**
Draws a legend identifying the colors used.
*/
def draw_legend(labels : DataCenterLegendText, colors : DataCenterColors) : Elem = {
	val stats : Seq[(String, String)] = (labels.demand +: labels.supplies.asSeq) zip (colors.demand +: colors.supplies.asSeq)
	
	<g id="legendWrap" clip-path="url(#legendClip)" transform="translate(700, 4)">
		<rect id="legendRect"
			x="0" y="0"
			width="200" height={(4 + (16 + 4) * stats.length).toString}
			style="fill: #e0fff0; stroke-width: 3px; stroke: #000050"
		/>
		<clipPath id="legendClip">
			<use xlink:href="#legendRect"/>
		</clipPath>
		{stats.zipWithIndex map {case ((label, color), i) ⇒
			val y : Int = 4 + (16 + 4) * i
			<g transform={"translate(0, %d)" format y}>
				<use xlink:href="#legendColorSquare" fill={color}/>
				<text x="24" y="12" font-size="16px">{label}</text>
			</g>
		}}
	</g>.convert
}


/**
Generates an SVG map visualization according to the provided data.
@param anim_time_per_step Animated time per step of the animation, in s
@param world_time_per_step Real-world time per step of the animation, in s
*/
def generate_visualization(anim_time_per_step : Double, world_time_per_step : Double, dclegend : DataCenterLegendText, dccolors : DataCenterColors, dcdata : Seq[DataCenter], lineData : Seq[Line]) : Array[Byte] = {
	// Add the speed and the time as metadata, for use by the XHTML wrapper
	val num_steps = {
		// Lengths of all the elements of the animation
		val lengths = (dcdata map  (_.stats.length)) ++ (lineData map (_.stats.length))
		/** @@@TO-DO: uncomment again when the script for building outside the IDE is implemented
		if(lengths.min != lengths.max)
			System.err println "Warning: different durations for different parts of the animation; using the shorter"
		*/
		lengths.min
	}
	val timelapse_factor = world_time_per_step / anim_time_per_step 
	val timing_metadata : Elem = <timing id="timing" xmlns="http://rsrg.cms.caltech.edu/xml/2012/timing"
		numSteps={num_steps.toString}
		timelapseFactor={"%f" format timelapse_factor}
		duration={"%.4f" format (num_steps * anim_time_per_step)}
	/>.convert
	
	var doc : Elem = {
		val metadata_elem_zipper : Zipper[Elem] = BACKGROUND_MAP \ "metadata"
		val map_with_updated_metadata_elem = metadata_elem_zipper.updated(0, metadata_elem_zipper.head addChild timing_metadata).unselect apply 0
		// Zipper.unselect has unnecessarily restrictive type. Work around by casting. (Cheat, but seems to work)
		map_with_updated_metadata_elem.asInstanceOf[Elem]
	}
	
	val overlay = <g id="overlay">
		{lineData map (line ⇒ U(draw_line(anim_time_per_step, line)))}
		{dcdata map (dc ⇒ U(draw_datacenter(anim_time_per_step, dc, dccolors)))}
	</g>.convert
	
	// Append to the svg document as child
	doc = doc addChild draw_legend(dclegend, dccolors) addChild overlay
	
	val os = new java.io.ByteArrayOutputStream
	XMLSerializer(outputDeclaration = true).serializeDocument(doc, os)
	os.toByteArray
}

}
