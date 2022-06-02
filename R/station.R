#' @title Sampling Station Numbering
#'
#' @description Functions to extract or determine trawl sampling station identification numbers.
#'              A sampling station is to be interpreted as a particular location, e.g. if two tows share
#'              a station number they can be considered as having been sampled in the same location.
#'              Station numbers can be assigned or determined based on spatial proximity.
#'
#' @param x Data object.
#' @param method Character string specifying the method to determine the station number. Options are \sQuote{observed} and \sQuote{calculated}.
#' @param longitude,latitude Numeric vector in decimal degrees.
#' @param time Numeric vector or time (\sQuote{POSIX}) objects.
#' @param distance.tolerance Distance (in kilometers) below which a pair of points is considered to belong to the same sampling station.
#' @param time.tolerance Numeric value specifying the time-difference below which a pair of points is considered to have occured at the same time.
#' @param ... Other arguments (not used).

# @describeIn station Generic 'station' function.
#' @export
station <- function(x, ...){
   UseMethod("station")
}

# @describeIn station 'station.number' is an alias for 'station'.
#' @export station.number
station.number <- station

#' @describeIn station Assign a spatial location identifier based on spatio-temporal proximity.
#' @export
station.default <- function(longitude, latitude, time, distance.tolerance = 1, time.tolerance, ...){
   # Check input arguments:
   if (missing(longitude) | missing(latitude)) stop("'longitude' and 'latitude' must be specified.")
   if (length(longitude) != length(latitude)) stop("'longitude' and 'latitude' vectors must be the same length.")
   if (!missing(time)) if (length(time) != length(longitude)) stop("'time' and coordinate vectors must have the same lengths.")

   # Tolerance values:
   distance.tolerance <- abs(distance.tolerance)
   if (!missing(time.tolerance)) time.tolerance <- abs(time.tolerance)

   # Check whether there are NA values in the coordinates:
   ix <- which(!is.na(longitude) & !is.na(latitude))

   # Non-NA data:
   lon <- longitude[ix]
   lat <- latitude[ix]

   # Cluster by proximate distance:
   tree <- stats::hclust(stats::dist(deg2km(lon, lat)))
   if (any(tree$height < distance.tolerance)){
      k <- min(which(rev(tree$height) < distance.tolerance))
      k <- max(k, 2)
      v <- stats::cutree(tree, k = k)
   }else{
      v <- 1:length(lon)
   }

   # Cluster by proximate time:
   if (!missing(time.tolerance)){
      tree <- stats::hclust(stats::dist(as.numeric(time) / 60))
      if (any(tree$height < time.tolerance)){
         k <- min(which(rev(tree$height) < time.tolerance))
         k <- max(k, 2)
         v <- paste(v, "-", cutree(tree, k = k))
         v <- match(v, unique(v))
      }else{
         v <- 1:length(lon)
      }
   }

   # Exapand to original data set:
   t <- rep(NA, length(longitude))
   t[ix] <- v

   return(t)
}

#' @describeIn station Return survey sampling station identifier.
#' @rawNamespace S3method(station,gulf.set)
station.gulf.set <- function(x, method = "observed", distance.tolerance = 2, time.tolerance = 10, ...){
   # Check 'method' argument:
   method <- match.arg(tolower(method), c("observed", "calculated"))

   # Define 'cruise' variable:
   cruise <- paste(x$vessel.code, x$cruise.number, sep = "")

   # Initialize result vector:
   v <- rep(NA, nrow(x))

   # Assign station numbers based on SAS program:
   if (method == "observed"){
      if ("station" %in% names(x)) v <- x$station
      v[is.na(v)] <- x$set.number[is.na(v)]

      # 1984: 4 repeat stations (checked visually on a map)
      ix <- (cruise == "P312")
      temp <- c(302, 202,
                402, 202,
                306, 206,
                309, 209,
                310, 210,
                410, 210)
      from <- temp[seq(1, length(temp), by = 2)]
      to <- temp[seq(2, length(temp), by = 2)]
      s <- match(x$set.number[ix], from)
      v[ix][!is.na(s)] <- to[s[!is.na(s)]]

      # Repeat sets for 1983, 1985, 1986 and 1987:
      ix <- cruise %in% c("P296", "P327", "H141", "H159", "H179")
      v[ix & (x$set.number %in% c(203))] <- 3
      v[ix & (x$set.number %in% c(229))] <- 29
      v[ix & (x$set.number %in% c(233, 333))] <- 33
      v[ix & (x$set.number %in% c(235, 335))] <- 35
      v[ix & (x$set.number %in% c(237))] <- 37
      v[ix & (x$set.number %in% c(238))] <- 38
      v[ix & (x$set.number %in% c(243))] <- 43
      v[ix & (x$set.number %in% c(250))] <- 50
      v[ix & (x$set.number %in% c(251))] <- 51
      v[ix & (x$set.number %in% c(-54, seq(254, 954, by = 100)))] <- 54
      v[ix & (x$set.number %in% c(255))] <- 55
      v[ix & (x$set.number %in% seq(256, 956, by = 100))] <- 56
      v[ix & (x$set.number %in% seq(257, 857, by = 100))] <- 57
      v[ix & (x$set.number %in% c(258))] <- 58
      v[ix & (x$set.number %in% c(259))] <- 59
      v[ix & (x$set.number %in% c(260))] <- 60
      v[ix & (x$set.number %in% c(261))] <- 61
      v[ix & (x$set.number %in% c(269))] <- 69
      v[ix & (x$set.number %in% c(275))] <- 75
      v[ix & (x$set.number %in% c(281, 381))] <- 81
      v[ix & (x$set.number %in% c(282, 382))] <- 82
      v[ix & (x$set.number %in% c(285, 385))] <- 85
      v[ix & (x$set.number %in% c(290))] <- 90
      v[ix & (x$set.number %in% c(291, 391))] <- 91
      v[ix & (x$set.number %in% c(294))] <- 94
      v[ix & (x$set.number %in% c(211))] <- 111
      v[ix & (x$set.number %in% c(212, 512, 612))] <- 112
      v[ix & (x$set.number %in% c(513, 613))] <- 113
      v[ix & (x$set.number %in% c(214))] <- 114
      v[ix & (x$set.number %in% c(520))] <- 120
      v[ix & (x$set.number %in% c(239))] <- 139
      v[ix & (x$set.number %in% c(541))] <- 141
      v[ix & (x$set.number %in% c(542))] <- 142

      # 1988: Repeat sets galore.
      ix <- (cruise == "H192")
      temp <- c( 30,  27,
                 5,   7,
                 150, 146,
                 147, 143,
                 148, 144,
                 149, 145,
                 100,  97,
                 109, 103,
                 115, 114,
                 118, 110,
                 116, 112,
                 117, 111,
                 107, 101,
                 129, 122,
                 128, 121,
                 108, 102,
                 119, 120,
                 141, 134,
                 132, 126,
                 131, 125,
                 130, 124,
                 127, 123,
                 138, 137,
                 140, 135,
                 139, 136,
                 86,  81,
                 98,  94,
                 90,  85,
                 91,  92,
                 87,  82,
                 88,  83,
                 37,  34,
                 89,  84,
                 151, 154,
                 158, 157,
                 159, 156,
                 160, 155,
                 47,  44,
                 161, 163,
                 152, 153,
                 39,  41,
                 40,  42,
                 28,  25,
                 21,  24,
                 19,  23,
                 29,  26,
                 14,  17,
                 18,  22,
                 77,  76,
                 78,  75,
                 79,  74,
                 73,   1,
                 72,   8,
                 2,   9,
                 3,  10,
                 12,  16,
                 13,  69,
                 49,  52,
                 50,  51,
                 57,  54,
                 46,  53,
                 61,  65,
                 62,  66,
                 59,  56,
                 58,  55,
                 60,  64,
                 48,  45)
      from <- temp[seq(1, length(temp), by = 2)]
      to <- temp[seq(2, length(temp), by = 2)]
      s <- match(x$set.number[ix], from)
      v[ix][!is.na(s)] <- to[s[!is.na(s)]]

      # Comparative day-night surveys for 1993:
      ix <- (cruise == "N192")
      temp <- c( 27,  30,
                 5,   7,
                 146, 150,
                 143, 147,
                 144, 148,
                 145, 149,
                 97, 100,
                 103, 109,
                 114, 115,
                 110, 118,
                 112, 116,
                 111, 117,
                 101, 107,
                 122, 129,
                 121, 128,
                 102, 108,
                 119, 120,
                 134, 141,
                 126, 132,
                 125, 131,
                 124, 130,
                 123, 127,
                 137, 138,
                 135, 140,
                 136, 139,
                 81,  86,
                 94,  98,
                 85,  90,
                 91,  92,
                 82,  87,
                 83,  88,
                 34,  37,
                 84,  89,
                 151, 154,
                 157, 158,
                 156, 159,
                 155, 160,
                 44,  47,
                 161, 163,
                 152, 153,
                 39,  41,
                 40,  42,
                 25,  28,
                 21,  24,
                 19,  23,
                 26,  29,
                 14,  17,
                 18,  22,
                 76,  77,
                 75,  78,
                 74,  79,
                 1,  73,
                 8,  72,
                 2,   9,
                 3,  10,
                 12,  16,
                 13,  69,
                 49,  52,
                 50,  51,
                 54,  57,
                 46,  53,
                 61,  65,
                 62,  66,
                 56,  59,
                 55,  58,
                 60,  64,
                 45,  48)
      to <- temp[seq(1, length(temp), by = 2)]
      from <- temp[seq(2, length(temp), by = 2)]
      s <- match(x$set.number[ix], from)
      v[ix][!is.na(s)] <- to[s[!is.na(s)]]

      # Comparative day-night surveys for 1998:
      ix <- (cruise == "N846")
      v[ix & (x$set.number == 16)]  <- 12
      v[ix & (x$set.number == 17)]  <- 13
      v[ix & (x$set.number == 18)]  <- 14
      v[ix & (x$set.number == 23)]  <- 19
      v[ix & (x$set.number == 25)]  <- 20
      v[ix & (x$set.number == 26)]  <- 21
      v[ix & (x$set.number == 46)]  <- 41
      v[ix & (x$set.number == 47)]  <- 42
      v[ix & (x$set.number == 48)]  <- 43
      v[ix & (x$set.number == 184)] <- 178
      v[ix & (x$set.number == 185)] <- 179
      v[ix & (x$set.number == 186)] <- 180
      v[ix & (x$set.number == 187)] <- 181
      v[ix & (x$set.number == 188)] <- 182
      v[ix & (x$set.number == 189)] <- 183
      v[ix & (x$set.number == 199)] <- 196
      v[ix & (x$set.number == 200)] <- 197
      v[ix & (x$set.number == 201)] <- 198
      v[ix & (x$set.number == 205)] <- 202
      v[ix & (x$set.number == 206)] <- 203
      v[ix & (x$set.number == 207)] <- 204
      v[ix & (x$set.number == 211)] <- 208
      v[ix & (x$set.number == 214)] <- 210
      v[ix & (x$set.number == 215)] <- 209
      v[ix & (x$set.number == 216)] <- 212
      v[ix & (x$set.number == 217)] <- 213

      # Comparative day-night surveys for 1999:
      ix <- (cruise == "N941")
      temp <- c(  2,   5,
                  1,   6,
                  3,   7,
                  4,   9,
                  20,  25,
                  21,  26,
                  23,  27,
                  24,  28,
                  37,  42,
                  35,  43,
                  174, 177,
                  175, 178,
                  176, 179,
                  184, 188,
                  185, 189,
                  186, 190,
                  193, 198,
                  196, 199,
                  197, 200,
                  194, 201,
                  195, 202)
      to <- temp[seq(1, length(temp), by = 2)]
      from <- temp[seq(2, length(temp), by = 2)]
      s <- match(x$set.number[ix], from)
      v[ix][!is.na(s)] <- to[s[!is.na(s)]]

      # Comparative day-night surveys for 2000:
      ix <- (cruise == "N045")
      temp <- c(199, 204,
                200, 205,
                201, 206,
                11,  14,
                10,  12,
                197, 202,
                4,   8,
                2,   6,
                195, 196,
                3,   7,
                5,   9,
                29,  34,
                33,  38,
                32,  37,
                30,  35,
                58,  59,
                49,  50,
                57,  60,
                56,  61,
                31,  36,
                198, 203)
      to <- temp[seq(1, length(temp), by = 2)]
      from <- temp[seq(2, length(temp), by = 2)]
      s <- match(x$set.number[ix], from)
      v[ix][!is.na(s)] <- to[s[!is.na(s)]]

      # Comparative survey in 2004:
      ix <- (cruise == "T434")
      temp <- c(7,   2,
                6,   3,
                16,  7,
                18,  9,
                139, 99,
                141, 98,
                136, 104,
                147, 96,
                137, 103,
                146, 97,
                134, 105,
                124, 106,
                126, 113,
                132, 108,
                127, 110,
                129, 111,
                117, 21,
                154, 95,
                113, 19,
                181, 15,
                182, 17,
                175, 20,
                179, 18,
                10,  4,
                5,   5,
                19,  10,
                20,  12,
                12,  6,
                22,  13,
                23,  14)
      from <- temp[seq(1, length(temp), by = 2)]
      to   <- temp[seq(2, length(temp), by = 2)]
      s <- match(x$set.number[ix], from)
      v[ix][!is.na(s)] <- to[s[!is.na(s)]]

      # Comparative survey in 2005:
      ix <- (cruise == "T507")
      temp <- c(121, 221,
                135, 235,
                136, 236,
                137, 237,
                138, 238,
                139, 239,
                140, 240,
                141, 241,
                142, 242,
                143, 243)
      from <- temp[seq(1, length(temp), by = 2)]
      to <- temp[seq(2, length(temp), by = 2)]
      s <- match(x$set.number[ix], from)
      v[ix][!is.na(s)] <- to[s[!is.na(s)]]

      # Comparative survey in 2021:
      ix <- (cruise == "C222")
      temp <- c(87, 110,
                95, 117,
               160, 183,
               161, 184,
               159, 182,
               149, 172,
               148, 170,
               147, 169,
                19, 11,
                17, 13,
                18, 14,
                26, 40,
                25, 39,
                27, 41,
                40, 56,
                38, 53,
                32, 48,
                61, 78,
                62, 79,
                63, 80,
                64, 81,
                65, 82,
                69, 86,
                56, 73,
                58, 75,
                60, 77,
                72, 89,
                74, 91,
                71, 88,
                68, 85,
                67, 84,
                15, 37,
                81, 103,
                82, 104,
                49, 65,
                78, 98,
                84, 107,
                85, 108,
                76, 95,
                75, 93,
                42, 58,
                43, 59,
                44, 60,
                51, 67,
                50, 66,
                53, 69,
                54, 70,
                55, 71,
                 2, 22,
                45, 61,
                47, 63,
                16, 38,
                 9, 30,
                79, 101,
                80, 102,
                92, 114,
                93, 115,
                90, 113,
                89, 112,
                94, 116,
                86, 109,
                88, 111,
               105, 127,
               106, 128,
                98, 120,
                99, 121,
                97, 119,
                96, 118,
               157, 180,
               156, 179,
               150, 173,
               146, 168,
               145, 167,
               144, 166,
               151, 174,
               153, 176,
               152, 175,
               154, 177,
               155, 178,
               111, 133,
               110, 132,
               104, 126,
               109, 131,
               107, 129,
               141, 163,
               139, 161,
               140, 162,
               114, 136,
               113, 135,
               102, 124,
               103, 125,
               122, 144,
               115, 137,
               132, 154,
               133, 155,
               112, 134,
               124, 146,
               129, 151,
               131, 153,
               130, 152,
               136, 158,
               138, 160,
               120, 142,
               123, 145,
               126, 148,
               119, 141,
               121, 143,
               125, 147,
               128, 150,
               127, 149,
               162, 185,
                41, 57,
                33, 49,
                70, 87,
               100, 122,
               143, 165,
               158, 181,
               137, 159,
               116, 138,
               134, 156,
               142, 164,
                34, 51,
                39, 54,
                66, 83,
                 5, 26,
                11, 32,
               163, 186,
               101, 123,
               135, 157,
               118, 140)
      from <- temp[seq(1, length(temp), by = 2)]
      to <- temp[seq(2, length(temp), by = 2)]
      s <- match(x$set.number[ix], from)
      v[ix][!is.na(s)] <- to[s[!is.na(s)]]

      # Additionnal station number reassignments:
      ix <- (x$year %in% 1984:1987)
      v[ix & (cruise %in% c("H159", "H179")) & (v == 117)] <- 1017
   }

   # Group sets using observed coordinates:
   if (method == "calculated"){
      years <- sort(unique(year(x)))
      v <- rep(NA, nrow(x))
      for (i in 1:length(years)){
         ix <- which((year(x) == years[i]) & (x$experiment != 3))
         time <- as.POSIXct(paste0(x[ix, "date"], " ", x[ix, "start.time"]))
         v[ix] <- station(gulf.spatial::longitude(x[ix, ]), gulf.spatial::latitude(x[ix, ]), time = time ,
                          distance.tolerance = distance.tolerance, time.tolerance = time.tolerance)
      }
   }

   return(v)
}
