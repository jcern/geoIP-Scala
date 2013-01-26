package com.maxmind.geoip

import java.util.HashMap
import java.io.File
import java.io.RandomAccessFile
import java.net.InetAddress
import java.net.UnknownHostException
import java.io.IOException
import javax.naming.directory.InitialDirContext
import java.util.Hashtable
import javax.naming.NamingException
import scala.collection.mutable.HashTable

/**
 * Provides a lookup service for information based on an IP address. The
 * location of a database file is supplied when creating a lookup service
 * instance. The edition of the database determines what information is
 * available about an IP address. See the DatabaseInfo class for further
 * details.
 * <p>
 * 
 * The following code snippet demonstrates looking up the country that an IP
 * address is from:
 * 
 * <pre>
 * // First, create a LookupService instance with the location of the database.
 * LookupService lookupService = new LookupService(&quot;c:\\geoip.dat&quot;);
 * // Assume we have a String ipAddress (in dot-decimal form).
 * Country country = lookupService.getCountry(ipAddress);
 * System.out.println(&quot;The country is: &quot; + country.getName());
 * System.out.println(&quot;The country code is: &quot; + country.getCode());
 * </pre>
 * 
 * In general, a single LookupService instance should be created and then reused
 * repeatedly.
 * <p>
 * 
 * <i>Tip:</i> Those deploying the GeoIP API as part of a web application may
 * find it difficult to pass in a File to create the lookup service, as the
 * location of the database may vary per deployment or may even be part of the
 * web-application. In this case, the database should be added to the classpath
 * of the web-app. For example, by putting it into the WEB-INF/classes directory
 * of the web application. The following code snippet demonstrates how to create
 * a LookupService using a database that can be found on the classpath:
 * 
 * <pre>
 * 
 * String fileName =
 *         getClass().getResource(&quot;/GeoIP.dat&quot;).toExternalForm().substring(6);
 * 
 * LookupService lookupService = new LookupService(fileName);
 * </pre>
 * 
 * @author Matt Tucker (matt@jivesoftware.com)
 */
class LookupService() {
  import LookupService._
	/**
	 * Database file.
	 */
	var file:Option[RandomAccessFile] = None
	var databaseFile:Option[File] = None

	/**
	 * Information about the database.
	 */
	var databaseInfo:Option[DatabaseInfo] = None
	/**
	 * The database type. Default is the country edition.
	 */
	var databaseType = DatabaseInfo.COUNTRY_EDITION
	var databaseSegments:Array[Int] = Array[Int]()
	var recordLength:Int = 0
	var licenseKey:String = ""
	var dnsService = 0
	var dboptions:Int = 0
	var dbbuffer:Array[Byte] = Array[Byte]()
	var indexCache:Array[Byte] = Array[Byte]()
    val hashmapcountryCodetoindex = new HashMap[String, Int]()
    val hashmapcountryNametoindex = new HashMap[String, Int]()
	var mtime:Long=0
	
	/**
	 * Create a new lookup service using the specified database file.
	 * 
	 * @param databaseFile
	 *            the database file.
	 * @throws java.io.IOException
	 *             if an error occured creating the lookup service from the
	 *             database file.
	 */
	def this(df:File) = {
    	this()
		file = Some(new RandomAccessFile(df, "r"))
		databaseFile = Some(df)
		init()
	}	
	
	/**
	 * Create a new distributed lookup service using the license key
	 * 
	 * @param databaseFile
	 *            String representation of the database file.
	 * @param licenseKey
	 *            license key provided by Maxmind to access distributed service
	 */
	def this(databaseFile:String, key:String) = {
		this(new File(databaseFile))
		licenseKey = key
		dnsService = 1
	}

	/**
	 * Create a new distributed lookup service using the license key
	 * 
	 * @param databaseFile
	 *            the database file.
	 * @param licenseKey
	 *            license key provided by Maxmind to access distributed service
	 */
	def this(databaseFile:File, key:String) = {
		this(databaseFile);
		licenseKey = key;
		dnsService = 1;
	}

	/**
	 * Create a new distributed lookup service using the license key
	 * 
	 * @param options
	 *            Resevered for future use
	 * @param licenseKey
	 *            license key provided by Maxmind to access distributed service
	 */
	def this(options:Int, key:String) = {
	    this()
		licenseKey = key;
		dnsService = 1;
		init();
	}

	/**
	 * Create a new lookup service using the specified database file.
	 * 
	 * @param databaseFile
	 *            String representation of the database file.
	 * @throws java.io.IOException
	 *             if an error occured creating the lookup service from the
	 *             database file.
	 */
	def this(databaseFile:String) = {
		this(new File(databaseFile));
	}

	/**
	 * Create a new lookup service using the specified database file.
	 * 
	 * @param databaseFile
	 *            the database file.
	 * @param options
	 *            database flags to use when opening the database GEOIP_STANDARD
	 *            read database from disk GEOIP_MEMORY_CACHE cache the database
	 *            in RAM and read it from RAM
	 * @throws java.io.IOException
	 *             if an error occured creating the lookup service from the
	 *             database file.
	 */
	def this(df:File, options:Int) = {
    	this()
		databaseFile = Some(df)
		file = Some(new RandomAccessFile(df, "r"))
		dboptions = options;
		init();
	}

	/**
	 * Create a new lookup service using the specified database file.
	 * 
	 * @param databaseFile
	 *            String representation of the database file.
	 * @param options
	 *            database flags to use when opening the database GEOIP_STANDARD
	 *            read database from disk GEOIP_MEMORY_CACHE cache the database
	 *            in RAM and read it from RAM
	 * @throws java.io.IOException
	 *             if an error occured creating the lookup service from the
	 *             database file.
	 */
	def this(databaseFile:String, options:Int) = {
		this(new File(databaseFile), options);
	}
	
	/**
	 * Reads meta-data from the database file.
	 * 
	 * @throws java.io.IOException
	 *             if an error occurs reading from the database file.
	 */
	def init() = {
		var delim = Array[Byte]()
		var buf = Array[Byte]() // { LookupService.SEGMENT_RECORD_LENGTH) }
		file match {
		  case Some(f) => 
		    if ((dboptions & LookupService.GEOIP_CHECK_CACHE) != 0) {
		    	mtime = databaseFile.map { _.lastModified() }.getOrElse(0l)
		    }
		    f.seek(f.length() - 3)
		    (0 to LookupService.STRUCTURE_INFO_MAX_SIZE) foreach { i =>
				f.read(delim)
				if (delim(0) == -1 && delim(1) == -1 && delim(2) == -1) {
					databaseType = f.readByte()
					if (databaseType >= 106) {
						// Backward compatibility with databases from April 2003 and earlier
						databaseType -= 105
					}
					databaseType match {
					   case DatabaseInfo.REGION_EDITION_REV0 => 
						 databaseSegments = Array[Int] { LookupService.STATE_BEGIN_REV0 }
						 recordLength = LookupService.STANDARD_RECORD_LENGTH
					   case DatabaseInfo.REGION_EDITION_REV1 =>
					     databaseSegments = Array[Int] { LookupService.STATE_BEGIN_REV1 }
						 recordLength = LookupService.STANDARD_RECORD_LENGTH
					   case DatabaseInfo.CITY_EDITION_REV0 |
							DatabaseInfo.CITY_EDITION_REV1 |
							DatabaseInfo.ORG_EDITION |
							DatabaseInfo.ISP_EDITION |
							DatabaseInfo.ASNUM_EDITION =>
						 databaseSegments = Array[Int] { 0 }
						 if (databaseType == DatabaseInfo.CITY_EDITION_REV0 ||
								databaseType == DatabaseInfo.CITY_EDITION_REV1 ||
								databaseType == DatabaseInfo.ASNUM_EDITION) {
							recordLength = LookupService.STANDARD_RECORD_LENGTH;
						 } else {
							recordLength = LookupService.ORG_RECORD_LENGTH;
						 }
						 f.read(buf);
						 (0 to LookupService.SEGMENT_RECORD_LENGTH) foreach { j =>
							databaseSegments(0) +=
						        (LookupService.unsignedByteToInt(buf(j)) << (j * 8));
						 }
					   case _ => f.seek(f.getFilePointer() - 4);
					}
				}
		    }
		    if ((databaseType == DatabaseInfo.COUNTRY_EDITION) ||
		        (databaseType == DatabaseInfo.PROXY_EDITION) ||
		        (databaseType == DatabaseInfo.NETSPEED_EDITION)) {
		    	databaseSegments = Array[Int] { LookupService.COUNTRY_BEGIN }
		    	recordLength = LookupService.STANDARD_RECORD_LENGTH
			}
		    if ((dboptions & LookupService.GEOIP_MEMORY_CACHE) == 1) {
		    	dbbuffer = Array[Byte]()
		    	file.foreach { f =>
		    		val l = f.length.intValue;
		    		f.seek(0)
		    		f.read(dbbuffer, 0, l);
		    		databaseInfo = Some(getDatabaseInfo())
		    		f.close();
		    	}
		    }
		    if ((dboptions & LookupService.GEOIP_INDEX_CACHE) != 0) {
		    	val l = databaseSegments(0) * recordLength * 2;
				indexCache = Array[Byte]()
				if (indexCache != null) {
					f.seek(0);
					f.read(indexCache, 0, l);
				}
		    } else {
		    	indexCache = null;
		    }
		
		
		  case _ =>
		    LookupService.countryCode.zipWithIndex.foreach { c =>
				hashmapcountryCodetoindex.put(c._1, c._2)
				hashmapcountryNametoindex.put(c._1, c._2)
			}
		}
	}

	/**
	 * Closes the lookup service.
	 */
	def close() = {
		try {
			file.foreach { f =>
				f.close()
			}
		} catch {
		  case e:Exception => 
		}
		file = None
	}

	def getID(ipAddress:String):Int = {
		var addr:InetAddress = null
		try {
			addr = InetAddress.getByName(ipAddress);
			getID(LookupService.bytesToLong(addr.getAddress()));
		} catch {
		  case e:UnknownHostException => 0
		}
	}

	def getID(ipAddress:InetAddress):Int = {
		return getID(LookupService.bytesToLong(ipAddress.getAddress()));
	}

	def getID(ipAddress:Long):Int = {
		if (file == null && (dboptions & GEOIP_MEMORY_CACHE) == 0) {
			throw new IllegalStateException("Database has been closed.");
		}
		val ret = seekCountry(ipAddress) - databaseSegments(0);
		return ret;
	}

	/**
	 * Returns information about the database.
	 * 
	 * @return database info.
	 */
	def getDatabaseInfo():DatabaseInfo = {
	    databaseInfo match {
	      case Some(db) => db
	      case _ => file.foreach { f =>
	        try {
			  // Synchronize since we're accessing the database file.
			  this.synchronized  {
				_check_mtime();
				var hasStructureInfo = false
				var delim = Array[Byte]()
				// Advance to part of file where database info is stored.
				f.seek(f.length() - 3);
				(0 to LookupService.STRUCTURE_INFO_MAX_SIZE) foreach { i =>
					f.read(delim);
					if (delim(0) == 255 && delim(1) == 255 && delim(2) == 255) {
						hasStructureInfo = true;
						//break;
					}
				}
				if (hasStructureInfo) {
					f.seek(f.getFilePointer() - 3);
				} else {
					// No structure info, must be pre Sep 2002 database, go back to end.
					f.seek(f.length() - 3);
				}
				// Find the database info string.
				(0 to LookupService.DATABASE_INFO_MAX_SIZE) foreach { i =>
					f.read(delim);
					if (delim(0) == 0 && delim(1) == 0 && delim(2) == 0) {
						var dbInfo = Array[Byte]()
						f.read(dbInfo);
						// Create the database info object using the string.
						databaseInfo = Some(
						        new DatabaseInfo(new String(dbInfo)))
						//return databaseInfo;
					}
					f.seek(f.getFilePointer() - 4);
				}
			  }
		    } catch {
		      case e:Exception => e.printStackTrace();
		    }
	      }
		  new DatabaseInfo("");	        
	    }
	}

	def _check_mtime() = {
	  this.synchronized {
		try {
			if ((dboptions & LookupService.GEOIP_CHECK_CACHE) != 0) {
				val t = databaseFile.map{ _.lastModified() != mtime }
				if (t getOrElse true) {
					/* GeoIP Database file updated */
					/* refresh filehandle */
					file.foreach { _.close() }
					file = databaseFile.map{ db => new RandomAccessFile(db, "r") }
					databaseInfo = null;
					init();
				}
			}
		} catch {
		  case e:IOException => System.out.println("file not found");
		}
	  }
	}

	def getLocation(ipnum:Long):Option[Location] = {
	  this.synchronized {
		var record_pointer:Int = 0
		var record_buf = Array[Byte]() //byte[FULL_RECORD_LENGTH];
		var record_buf_offset = 0;
		var record = new Location()
		var str_length = 0;
		var j:Int = 0
		var seek_country:Int =0;
		var latitude = 0d
		var longitude = 0d;

		try {
			seek_country = seekCountry(ipnum);
			if (seek_country == databaseSegments(0)) {
				return null;
			}
			record_pointer =
			        seek_country + (2 * recordLength - 1) * databaseSegments(0)

			if ((dboptions & LookupService.GEOIP_MEMORY_CACHE) == 1) {
				//read from memory
				System.arraycopy(dbbuffer, record_pointer, record_buf, 0, math
				        .min(dbbuffer.length - record_pointer,
				                LookupService.FULL_RECORD_LENGTH));
			} else {
				//read from disk
				file.foreach { f =>
				  f.seek(record_pointer);
				  f.read(record_buf);
				}
			}

			// get country
			record.countryCode = countryCode(LookupService.unsignedByteToInt(record_buf(0)))
			record.countryName = countryName(LookupService.unsignedByteToInt(record_buf(0)))
			record_buf_offset += 1;

			// get region
			while (record_buf(record_buf_offset + str_length) != '\0')
				str_length += 1;
			if (str_length > 0) {
				record.region =
				        new String(record_buf, record_buf_offset, str_length);
			}
			record_buf_offset += str_length + 1;
			str_length = 0;

			// get city
			while (record_buf(record_buf_offset + str_length) != '\0')
				str_length += 1;
			if (str_length > 0) {
				record.city =
				        new String(record_buf, record_buf_offset, str_length,
				                "ISO-8859-1");
			}
			record_buf_offset += str_length + 1;
			str_length = 0;

			// get postal code
			while (record_buf(record_buf_offset + str_length) != '\0')
				str_length += 1;
			if (str_length > 0) {
				record.postalCode =
				        new String(record_buf, record_buf_offset, str_length);
			}
			record_buf_offset += str_length + 1;

			// get latitude
			(0 to 3) foreach { j =>
				latitude +=
				        (LookupService.unsignedByteToInt(record_buf(record_buf_offset + j)) << (j * 8));
			}
			record.latitude = (latitude / 10000f).toFloat - 180f;
			record_buf_offset += 3;

			// get longitude
			(0 to 3).foreach { j => 
				longitude +=
				        (LookupService.unsignedByteToInt(record_buf(record_buf_offset + j)) << (j * 8));
			}
			record.longitude = (longitude / 10000f).toFloat - 180f;

			record.dma_code = 0
			record.metro_code = 0;
			record.area_code = 0;
			if (databaseType == DatabaseInfo.CITY_EDITION_REV1) {
				// get DMA code
				var metroarea_combo = 0;
				if (record.countryCode == "US") {
					record_buf_offset += 3;
					(0 to 3).foreach { j =>
						metroarea_combo +=
						        (LookupService.unsignedByteToInt(record_buf(record_buf_offset
						                + j)) << (j * 8));
					}
					record.metro_code = metroarea_combo / 1000
					record.dma_code = metroarea_combo / 1000;
					record.area_code = metroarea_combo % 1000;
				}
			}
		} catch {
		  case e:IOException => System.err.println("IO Exception while seting up segments");
		}
		Some(record)
	  }
	}
	
	// for GeoIP City only
	def getLocation( addr:InetAddress): Option[Location] = {
		getLocation(LookupService.bytesToLong(addr.getAddress()));
	}

	// for GeoIP City only
	def getLocation(str:String): Option[Location] = {
		if (dnsService == 0) {
			try {
				getLocation(InetAddress.getByName(str))
			} catch {
			  case e:UnknownHostException => None;
			}
		} else {
			getDnsAttributes(str) map {
				getLocationwithdnsservice _
			}
			// TODO if DNS is not available, go to local file as backup
		}
	}

	def getDnsAttributes(ip:String) = {
		try {
			val env = new Hashtable[String, String]();
			env.put("java.naming.factory.initial",
			        "com.sun.jndi.dns.DnsContextFactory");
			// TODO don't specify ws1, instead use ns servers for s.maxmind.com
			env.put("java.naming.provider.url", "dns://ws1.maxmind.com/");

			val ictx = new InitialDirContext(env);
			val attrs =
			        ictx.getAttributes(
			                licenseKey + "." + ip + ".s.maxmind.com",
			                Array( "txt" ));
			//System.out.println(attrs.get("txt").get());
			Some(attrs.get("txt").get().toString())
		} catch {
			// TODO fix this to handle exceptions
		  case e:NamingException => 
		    println("DNS error");
		    None
		}

	}

	def getLocationwithdnsservice(str:String):Location = {
	    import java.util.StringTokenizer
		val record = new Location()
		val st = new StringTokenizer(str, ";=\"");
		while (st.hasMoreTokens()) {
			val key = st.nextToken();
			val value = if (st.hasMoreTokens()) st.nextToken else ""
			  			// TODO, ISP and Organization
			//if (key.equals("or")) {
			//record.org = value;
			//}
			//if (key.equals("is")) {
			//record.isp = value;
			//}

			key match {
			  case "co" =>
				record.countryCode = value;
				record.countryName = countryName(hashmapcountryCodetoindex.get(value))
			  case "ci" =>
				record.city = value;
			  case "re" =>
				record.region = value;
			  case "zi" =>
				record.postalCode = value;
			  case "la" =>
			    val v = asFloat(value)
			    record.latitude = v
			  case "lo" =>
					record.longitude = asFloat(value)
			  case "dm" | "me" =>
				val v = asInt(value)
			    record.metro_code = v
			    record.dma_code = v
			  case "ac" =>
				record.area_code = asInt(value);
			}
		}
		return record;
	}

	private def asInt(str:String) = {
	  try {
	    str.toInt
	  } catch {
	    case e:NumberFormatException => 0
	  }
	}	
	
	private def asFloat(str:String) = {
	  try {
	    str.toFloat
	  } catch {
	    case e:NumberFormatException => 0
	  }
	}
	
	def getRegion( str:String):Region = {
	  this.synchronized {
		var addr:InetAddress = null
		try {
			addr = InetAddress.getByName(str);
		} catch {
		  case e:UnknownHostException => null;
		}

		getRegion(LookupService.bytesToLong(addr.getAddress()));
	  }
	}

	def  getRegion(ipnum:Long):Region = {
	  this.synchronized {
		var seekRegion = 0;
		databaseType match {
		  case DatabaseInfo.REGION_EDITION_REV0 => 
			seekRegion = seekCountry(ipnum) - LookupService.STATE_BEGIN_REV0;
			var ch = Array[Char]()
			if (seekRegion >= 1000) {
				ch(0) = (((seekRegion - 1000) / 26) + 65).asInstanceOf[Char]
				ch(1) = (((seekRegion - 1000) % 26) + 65).asInstanceOf[Char]
				new Region(countryCode = "US", countryName = "United States", region=new String(ch))
			} else {
				new Region(countryCode = countryCode(seekRegion), countryName = countryName(seekRegion), region="")
			}
		  case DatabaseInfo.REGION_EDITION_REV1 =>
			seekRegion = seekCountry(ipnum) - LookupService.STATE_BEGIN_REV1;
			var ch = Array[Char]()
			if (seekRegion < LookupService.US_OFFSET) {
			    new Region("","","")
			} else if (seekRegion < LookupService.CANADA_OFFSET) {
				ch(0) = (((seekRegion - LookupService.US_OFFSET) / 26) + 65).asInstanceOf[Char]
				ch(1) = (((seekRegion - LookupService.US_OFFSET) % 26) + 65).asInstanceOf[Char]
				new Region(countryCode = "US", countryName = "United States", region=new String(ch))
			} else if (seekRegion < LookupService.WORLD_OFFSET) {
				ch(0) = (((seekRegion - LookupService.CANADA_OFFSET) / 26) + 65).asInstanceOf[Char]
				ch(1) = (((seekRegion - LookupService.CANADA_OFFSET) % 26) + 65).asInstanceOf[Char]
				new Region(countryCode = "CA", countryName = "Canada", region=new String(ch))
			} else {
			  	new Region(countryCode = countryCode((seekRegion - LookupService.WORLD_OFFSET) / LookupService.FIPS_RANGE), 
			  	    countryName = countryCode((seekRegion - LookupService.WORLD_OFFSET) / LookupService.FIPS_RANGE),
			  	    region="")
			}
		}
	  }
	}


	// GeoIP Organization and ISP Edition methods
	def getOrg(ipnum:Long):Option[String] = synchronized {
		var str_length = 0
		var buf = Array[Byte]() //[MAX_ORG_RECORD_LENGTH];

		val dbSeg = databaseSegments(0)
		seekCountry(ipnum) match {
		  case t if(t == dbSeg) => None
		  case seekOrg => try {
			val recordPointer =
			        seekOrg + (2 * recordLength - 1) * databaseSegments(0)
			if ((dboptions & LookupService.GEOIP_MEMORY_CACHE) == 1) {
				//read from memory
				System.arraycopy(dbbuffer, recordPointer, buf, 0, math
				        .min(dbbuffer.length - recordPointer,
				                LookupService.MAX_ORG_RECORD_LENGTH));
			} else {
				//read from disk
				file.foreach { f =>
				  f.seek(recordPointer)
				  f.read(buf)
				}
			}
			while (buf(str_length) != '\0') {
				str_length += 1
			}
			Some(new String(buf, 0, str_length, "ISO-8859-1"))
			//return org_buf;
		  } catch  {
		    case e:IOException =>
			  println("IO Exception");
			  None
		  }
		}
	}	
	
	def getOrg(addr:InetAddress):Option[String] = {
		getOrg(LookupService.bytesToLong(addr.getAddress()))
	}

	def getOrg(str:String):Option[String] = {
		try {
			getOrg(LookupService.bytesToLong(InetAddress.getByName(str).getAddress))
		} catch {
		  case e:UnknownHostException => None
		}
	}



	/**
	 * Finds the country index value given an IP address.
	 * 
	 * @param ipAddress
	 *            the ip address to find in long format.
	 * @return the country index.
	 */
	private def seekCountry(ipAddress:Long):Int = {
		var buf = Array[Byte]() //[2 * MAX_RECORD_LENGTH];
		var x = Array[Int]()
		var offset = 0;
		_check_mtime();
		 (31 to 0).foreach { depth =>
			if ((dboptions & LookupService.GEOIP_MEMORY_CACHE) == 1) {
				//read from memory
				(0 to  2 * LookupService.MAX_RECORD_LENGTH) foreach { i=>
					buf(i) = dbbuffer((2 * recordLength * offset) + i)
				}
			} else if ((dboptions & LookupService.GEOIP_INDEX_CACHE) != 0) {
				//read from index cache
				(0 to  2 * LookupService.MAX_RECORD_LENGTH) foreach { i=>
					buf(i) = indexCache((2 * recordLength * offset) + i)
				}
			} else {
				//read from disk 
				try {
					file.foreach { f => 
					  f.seek(2 * recordLength * offset)
					  f.read(buf)
					}
				} catch {
				  case e:IOException => System.out.println("IO Exception");
				}
			}
			(0 to 2).foreach { i =>
				x(i) = 0;
				(0 to recordLength) foreach { j =>
					var y:Int = buf(i * recordLength + j)
					if (y < 0) {
						y += 256;
					}
					x(i) += (y << (j * 8));
				}
			}

			if ((ipAddress & (1 << depth)) > 0) {
				if (x(1) >= databaseSegments(0)) {
					return x(1);
				}
				offset = x(1);
			} else {
				if (x(0) >= databaseSegments(0)) {
					return x(0);
				}
				offset = x(0);
			}
		}

		// shouldn't reach here
		System.err.println("Error seeking country while seeking " + ipAddress);
		return 0;
	}
	

	/**
	 * Returns the country the IP address is in.
	 * 
	 * @param ipAddress
	 *            the IP address in long format.
	 * @return the country the IP address is from.
	 */
	def getCountry(ipAddress:Long):Country = {
	    file match {
	      case None if ((dboptions & GEOIP_MEMORY_CACHE) == 0) => 
			throw new IllegalStateException("Database has been closed.")
	      case _ => 
	        val ret = seekCountry(ipAddress) - COUNTRY_BEGIN;
	        if (ret == 0) {
	        	UNKNOWN_COUNTRY;
	        } else {
	        	new Country(countryCode(ret), countryName(ret))
	        }
	    }
	}	

	/**
	 * Returns the country the IP address is in.
	 * 
	 * @param ipAddress
	 *            the IP address.
	 * @return the country the IP address is from.
	 */
	def getCountry(ipAddress:InetAddress):Country = {
		getCountry(LookupService.bytesToLong(ipAddress.getAddress))
	}
	
	/**
	 * Returns the country the IP address is in.
	 * 
	 * @param ipAddress
	 *            String version of an IP address, i.e. "127.0.0.1"
	 * @return the country the IP address is from.
	 */
	def getCountry(ipAddress:String):Country = {
		try {
			getCountry(bytesToLong(InetAddress.getByName(ipAddress).getAddress))
		} catch {
		  case e:UnknownHostException => UNKNOWN_COUNTRY
		}
	}	

}

/**
 * LookupService.java
 *
 * Copyright (C) 2003 MaxMind LLC.  All Rights Reserved.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 */
object LookupService {
        val  US_OFFSET = 1;

        val  CANADA_OFFSET = 677;

        val  WORLD_OFFSET = 1353;

        val  FIPS_RANGE = 360;

        val  COUNTRY_BEGIN = 16776960;

        val  STATE_BEGIN_REV0 = 16700000;

        val  STATE_BEGIN_REV1 = 16000000;

        val  STRUCTURE_INFO_MAX_SIZE = 20;

        val  DATABASE_INFO_MAX_SIZE = 100;

        val  GEOIP_STANDARD = 0;

        val  GEOIP_MEMORY_CACHE = 1;

        val  GEOIP_CHECK_CACHE = 2;

        val  GEOIP_INDEX_CACHE = 4;

        val  GEOIP_UNKNOWN_SPEED = 0;

        val  GEOIP_DIALUP_SPEED = 1;

        val  GEOIP_CABLEDSL_SPEED = 2;

        val  GEOIP_CORPORATE_SPEED = 3;

        val  SEGMENT_RECORD_LENGTH = 3;

        val  STANDARD_RECORD_LENGTH = 3;

        val  ORG_RECORD_LENGTH = 4;

        val  MAX_RECORD_LENGTH = 4;

        val  MAX_ORG_RECORD_LENGTH = 300;

        val  FULL_RECORD_LENGTH = 60;

        val  UNKNOWN_COUNTRY = new Country("--", "N/A");
        
        val countryCode = Array("--",
	    "AP",
	    "EU",
	    "AD",
	    "AE",
	    "AF",
	    "AG",
	    "AI",
	    "AL",
	    "AM",
	    "AN",
	    "AO",
	    "AQ",
	    "AR",
	    "AS",
	    "AT",
	    "AU",
	    "AW",
	    "AZ",
	    "BA",
	    "BB",
	    "BD",
	    "BE",
	    "BF",
	    "BG",
	    "BH",
	    "BI",
	    "BJ",
	    "BM",
	    "BN",
	    "BO",
	    "BR",
	    "BS",
	    "BT",
	    "BV",
	    "BW",
	    "BY",
	    "BZ",
	    "CA",
	    "CC",
	    "CD",
	    "CF",
	    "CG",
	    "CH",
	    "CI",
	    "CK",
	    "CL",
	    "CM",
	    "CN",
	    "CO",
	    "CR",
	    "CU",
	    "CV",
	    "CX",
	    "CY",
	    "CZ",
	    "DE",
	    "DJ",
	    "DK",
	    "DM",
	    "DO",
	    "DZ",
	    "EC",
	    "EE",
	    "EG",
	    "EH",
	    "ER",
	    "ES",
	    "ET",
	    "FI",
	    "FJ",
	    "FK",
	    "FM",
	    "FO",
	    "FR",
	    "FX",
	    "GA",
	    "GB",
	    "GD",
	    "GE",
	    "GF",
	    "GH",
	    "GI",
	    "GL",
	    "GM",
	    "GN",
	    "GP",
	    "GQ",
	    "GR",
	    "GS",
	    "GT",
	    "GU",
	    "GW",
	    "GY",
	    "HK",
	    "HM",
	    "HN",
	    "HR",
	    "HT",
	    "HU",
	    "ID",
	    "IE",
	    "IL",
	    "IN",
	    "IO",
	    "IQ",
	    "IR",
	    "IS",
	    "IT",
	    "JM",
	    "JO",
	    "JP",
	    "KE",
	    "KG",
	    "KH",
	    "KI",
	    "KM",
	    "KN",
	    "KP",
	    "KR",
	    "KW",
	    "KY",
	    "KZ",
	    "LA",
	    "LB",
	    "LC",
	    "LI",
	    "LK",
	    "LR",
	    "LS",
	    "LT",
	    "LU",
	    "LV",
	    "LY",
	    "MA",
	    "MC",
	    "MD",
	    "MG",
	    "MH",
	    "MK",
	    "ML",
	    "MM",
	    "MN",
	    "MO",
	    "MP",
	    "MQ",
	    "MR",
	    "MS",
	    "MT",
	    "MU",
	    "MV",
	    "MW",
	    "MX",
	    "MY",
	    "MZ",
	    "NA",
	    "NC",
	    "NE",
	    "NF",
	    "NG",
	    "NI",
	    "NL",
	    "NO",
	    "NP",
	    "NR",
	    "NU",
	    "NZ",
	    "OM",
	    "PA",
	    "PE",
	    "PF",
	    "PG",
	    "PH",
	    "PK",
	    "PL",
	    "PM",
	    "PN",
	    "PR",
	    "PS",
	    "PT",
	    "PW",
	    "PY",
	    "QA",
	    "RE",
	    "RO",
	    "RU",
	    "RW",
	    "SA",
	    "SB",
	    "SC",
	    "SD",
	    "SE",
	    "SG",
	    "SH",
	    "SI",
	    "SJ",
	    "SK",
	    "SL",
	    "SM",
	    "SN",
	    "SO",
	    "SR",
	    "ST",
	    "SV",
	    "SY",
	    "SZ",
	    "TC",
	    "TD",
	    "TF",
	    "TG",
	    "TH",
	    "TJ",
	    "TK",
	    "TM",
	    "TN",
	    "TO",
	    "TL",
	    "TR",
	    "TT",
	    "TV",
	    "TW",
	    "TZ",
	    "UA",
	    "UG",
	    "UM",
	    "US",
	    "UY",
	    "UZ",
	    "VA",
	    "VC",
	    "VE",
	    "VG",
	    "VI",
	    "VN",
	    "VU",
	    "WF",
	    "WS",
	    "YE",
	    "YT",
	    "RS",
	    "ZA",
	    "ZM",
	    "ME",
	    "ZW",
	    "A1",
	    "A2",
	    "O1",
	    "AX",
	    "GG",
	    "IM",
	    "JE",
	    "BL",
	    "MF")
	    
	    val countryName = Array("N/A",
	    "Asia/Pacific Region",
	    "Europe",
	    "Andorra",
	    "United Arab Emirates",
	    "Afghanistan",
	    "Antigua and Barbuda",
	    "Anguilla",
	    "Albania",
	    "Armenia",
	    "Netherlands Antilles",
	    "Angola",
	    "Antarctica",
	    "Argentina",
	    "American Samoa",
	    "Austria",
	    "Australia",
	    "Aruba",
	    "Azerbaijan",
	    "Bosnia and Herzegovina",
	    "Barbados",
	    "Bangladesh",
	    "Belgium",
	    "Burkina Faso",
	    "Bulgaria",
	    "Bahrain",
	    "Burundi",
	    "Benin",
	    "Bermuda",
	    "Brunei Darussalam",
	    "Bolivia",
	    "Brazil",
	    "Bahamas",
	    "Bhutan",
	    "Bouvet Island",
	    "Botswana",
	    "Belarus",
	    "Belize",
	    "Canada",
	    "Cocos (Keeling) Islands",
	    "Congo, The Democratic Republic of the",
	    "Central African Republic",
	    "Congo",
	    "Switzerland",
	    "Cote D'Ivoire",
	    "Cook Islands",
	    "Chile",
	    "Cameroon",
	    "China",
	    "Colombia",
	    "Costa Rica",
	    "Cuba",
	    "Cape Verde",
	    "Christmas Island",
	    "Cyprus",
	    "Czech Republic",
	    "Germany",
	    "Djibouti",
	    "Denmark",
	    "Dominica",
	    "Dominican Republic",
	    "Algeria",
	    "Ecuador",
	    "Estonia",
	    "Egypt",
	    "Western Sahara",
	    "Eritrea",
	    "Spain",
	    "Ethiopia",
	    "Finland",
	    "Fiji",
	    "Falkland Islands (Malvinas)",
	    "Micronesia, Federated States of",
	    "Faroe Islands",
	    "France",
	    "France, Metropolitan",
	    "Gabon",
	    "United Kingdom",
	    "Grenada",
	    "Georgia",
	    "French Guiana",
	    "Ghana",
	    "Gibraltar",
	    "Greenland",
	    "Gambia",
	    "Guinea",
	    "Guadeloupe",
	    "Equatorial Guinea",
	    "Greece",
	    "South Georgia and the South Sandwich Islands",
	    "Guatemala",
	    "Guam",
	    "Guinea-Bissau",
	    "Guyana",
	    "Hong Kong",
	    "Heard Island and McDonald Islands",
	    "Honduras",
	    "Croatia",
	    "Haiti",
	    "Hungary",
	    "Indonesia",
	    "Ireland",
	    "Israel",
	    "India",
	    "British Indian Ocean Territory",
	    "Iraq",
	    "Iran, Islamic Republic of",
	    "Iceland",
	    "Italy",
	    "Jamaica",
	    "Jordan",
	    "Japan",
	    "Kenya",
	    "Kyrgyzstan",
	    "Cambodia",
	    "Kiribati",
	    "Comoros",
	    "Saint Kitts and Nevis",
	    "Korea, Democratic People's Republic of",
	    "Korea, Republic of",
	    "Kuwait",
	    "Cayman Islands",
	    "Kazakhstan",
	    "Lao People's Democratic Republic",
	    "Lebanon",
	    "Saint Lucia",
	    "Liechtenstein",
	    "Sri Lanka",
	    "Liberia",
	    "Lesotho",
	    "Lithuania",
	    "Luxembourg",
	    "Latvia",
	    "Libyan Arab Jamahiriya",
	    "Morocco",
	    "Monaco",
	    "Moldova, Republic of",
	    "Madagascar",
	    "Marshall Islands",
	    "Macedonia",
	    "Mali",
	    "Myanmar",
	    "Mongolia",
	    "Macau",
	    "Northern Mariana Islands",
	    "Martinique",
	    "Mauritania",
	    "Montserrat",
	    "Malta",
	    "Mauritius",
	    "Maldives",
	    "Malawi",
	    "Mexico",
	    "Malaysia",
	    "Mozambique",
	    "Namibia",
	    "New Caledonia",
	    "Niger",
	    "Norfolk Island",
	    "Nigeria",
	    "Nicaragua",
	    "Netherlands",
	    "Norway",
	    "Nepal",
	    "Nauru",
	    "Niue",
	    "New Zealand",
	    "Oman",
	    "Panama",
	    "Peru",
	    "French Polynesia",
	    "Papua New Guinea",
	    "Philippines",
	    "Pakistan",
	    "Poland",
	    "Saint Pierre and Miquelon",
	    "Pitcairn Islands",
	    "Puerto Rico",
	    "Palestinian Territory",
	    "Portugal",
	    "Palau",
	    "Paraguay",
	    "Qatar",
	    "Reunion",
	    "Romania",
	    "Russian Federation",
	    "Rwanda",
	    "Saudi Arabia",
	    "Solomon Islands",
	    "Seychelles",
	    "Sudan",
	    "Sweden",
	    "Singapore",
	    "Saint Helena",
	    "Slovenia",
	    "Svalbard and Jan Mayen",
	    "Slovakia",
	    "Sierra Leone",
	    "San Marino",
	    "Senegal",
	    "Somalia",
	    "Suriname",
	    "Sao Tome and Principe",
	    "El Salvador",
	    "Syrian Arab Republic",
	    "Swaziland",
	    "Turks and Caicos Islands",
	    "Chad",
	    "French Southern Territories",
	    "Togo",
	    "Thailand",
	    "Tajikistan",
	    "Tokelau",
	    "Turkmenistan",
	    "Tunisia",
	    "Tonga",
	    "Timor-Leste",
	    "Turkey",
	    "Trinidad and Tobago",
	    "Tuvalu",
	    "Taiwan",
	    "Tanzania, United Republic of",
	    "Ukraine",
	    "Uganda",
	    "United States Minor Outlying Islands",
	    "United States",
	    "Uruguay",
	    "Uzbekistan",
	    "Holy See (Vatican City State)",
	    "Saint Vincent and the Grenadines",
	    "Venezuela",
	    "Virgin Islands, British",
	    "Virgin Islands, U.S.",
	    "Vietnam",
	    "Vanuatu",
	    "Wallis and Futuna",
	    "Samoa",
	    "Yemen",
	    "Mayotte",
	    "Serbia",
	    "South Africa",
	    "Zambia",
	    "Montenegro",
	    "Zimbabwe",
	    "Anonymous Proxy",
	    "Satellite Provider",
	    "Other",
	    "Aland Islands",
	    "Guernsey",
	    "Isle of Man",
	    "Jersey",
	    "Saint Barthelemy",
	    "Saint Martin")



	/**
	 * Returns the long version of an IP address given an InetAddress object.
	 * 
	 * @param address
	 *            the InetAddress.
	 * @return the long form of the IP address.
	 */
	def bytesToLong(address:Array[Byte]):Long = {
		var ipnum = 0l;
		var i = 0;
		for (i <- 0 to 4) {
			var y:Long = address(i);
			if (y < 0) {
				y += 256;
			}
			ipnum += y << ((3 - i) * 8);
		}
		ipnum;
	}

	def unsignedByteToInt(b:Byte):Int = {
		(b & 0xFF).asInstanceOf[Int]
	}
}
