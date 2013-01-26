package com.maxmind.geoip
import java.lang.Math

class Location(	var countryCode:String="",
		var countryName:String="",
		var region:String="",
		var city:String="",
		var postalCode:String="",
		var latitude:Float=0,
		var longitude:Float=0,
		var dma_code:Int=0,
		var area_code:Int=0,
		var metro_code:Int=0) {
  
  def distance(loc: Location) = {
		var lat1 = latitude;
		var lon1 = longitude;
		var lat2 = loc.latitude;
		var lon2 = loc.longitude;

		// convert degrees to radians
		lat1 *= Location.RAD_CONVERT.floatValue
		lat2 *= Location.RAD_CONVERT.floatValue

		// find the deltas
		val delta_lat = lat2 - lat1
		val delta_lon = (lon2 - lon1) * Location.RAD_CONVERT

		// Find the great circle distance
		var temp =
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