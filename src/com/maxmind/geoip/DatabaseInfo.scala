package com.maxmind.geoip
import java.text.SimpleDateFormat

/**
 * Encapsulates metadata about the GeoIP database. The database has a date, is a
 * premium or standard version, and is one of the following types:
 * 
 * <ul>
 * <li>Country edition -- this is the most common version of the database. It
 * includes the name of the country and it's ISO country code given an IP
 * address.
 * <li>Region edition -- includes the country information as well as what U.S.
 * state or Canadian province the IP address is from if the IP address is from
 * the U.S. or Canada.
 * <li>City edition -- includes country, region, city, postal code, latitude,
 * and longitude information.
 * <li>Org edition -- includes country and netblock owner.
 * <li>ISP edition -- includes country, region, city, postal code, latitude,
 * longitude, ISP, and organization information.
 * </ul>
 * 
 * @see com.maxmind.geoip.LookupService#getDatabaseInfo()
 * @author Matt Tucker
 */
case class DatabaseInfo(val info:String) {
	val getType:Int = {
		if (info == null || info.equals("")) {
			DatabaseInfo.COUNTRY_EDITION
		} else {
			// Get the type code from the database info string and then
			// subtract 105 from the value to preserve compatability with
			// databases from April 2003 and earlier.
			Integer.parseInt(info.substring(4, 7)) - 105
		}
	}

	/**
	 * Returns true if the database is the premium version.
	 * 
	 * @return true if the premium version of the database.
	 */
	val isPremium = info.indexOf("FREE") < 0

	/**
	 * Returns the date of the database.
	 * 
	 * @return the date of the database.
	 */
	val getDate = {
	    val dateString = info.replaceAll("""\s""", "")
		DatabaseInfo.formatter.parse(dateString)
	}

	override def toString:String = info.toString
	
}

object DatabaseInfo {
  	val COUNTRY_EDITION = 1;

	val REGION_EDITION_REV0 = 7;

	val REGION_EDITION_REV1 = 3;

	val CITY_EDITION_REV0 = 6;

	val CITY_EDITION_REV1 = 2;

	val ORG_EDITION = 5;

	val ISP_EDITION = 4;

	val PROXY_EDITION = 8;

	val ASNUM_EDITION = 9;

	val NETSPEED_EDITION = 10;
	
	val formatter = new SimpleDateFormat("yyyyMMdd");

}