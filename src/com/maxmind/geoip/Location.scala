package com.maxmind.geoip
import java.lang.Math

case class Location(	
    val countryCode:String="",
	val countryName:String="",
	val region:String="",
	val city:String="",
	val postalCode:String="",
	val latitude:Float=0,
	val longitude:Float=0,
	val dmaCode:Int=0,
	val areaCode:Int=0,
	val metroCode:Int=0) {
  
  def distance(loc: Location) = {
		val lat1 = latitude * Location.RAD_CONVERT.floatValue;
		val lon1 = longitude;
		val lat2 = loc.latitude * Location.RAD_CONVERT.floatValue;
		val lon2 = loc.longitude;

		// find the deltas
		val delta_lat = lat2 - lat1
		val delta_lon = (lon2 - lon1) * Location.RAD_CONVERT

		// Find the great circle distance
		val temp =
		        Math.pow(Math.sin(delta_lat / 2), 2) + Math.cos(lat1) *
		                Math.cos(lat2) * Math.pow(Math.sin(delta_lon / 2), 2)
		Location.EARTH_DIAMETER *
		        Math.atan2(Math.sqrt(temp), Math.sqrt(1 - temp))
	}
}

object Location{ 
  val EARTH_DIAMETER = 2 * 6378.2
  val PI = Math.PI
  val RAD_CONVERT = PI / 180
}