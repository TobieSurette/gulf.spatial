#' Loran Coordinate Conversion
#'
#' @aliases loran
#'
#' @description Function to convert Loran coordinates format to decimal latitude-longitude degree format.
#'
#' @param x Numeric vector of Loran X coordinates.
#' @param y Numeric vector of Loran Y coordinates.
#'
#' @return Data frame with fields \code{long} and \code{lat} in decimal degree format with the same
#' dimensions as \code{x} and \code{y}.
#'
#' @examples
#' # Check whether character string is a candidate loran coordinate:
#' is.loran(c("", "25.000", "0025500", "14456.123", "014512345", "14'456", "0000"))
#'
#' # Loran coordinate clean-up function:
#' as.loran(c("", "25.000", "0025500", "14456.123", "014512345", "14'456", "0000"))
#'
#' # Convert a Loran XY coordinate to decimal degree format:
#' loran2deg(14500, 29500)
#'
#' # Convert a vector of Loran XY coordinates to decimal degree format:
#' loran2deg(seq(14000, 14500, len = 21), seq(29000, 29500, len = 21))
#'
#' loran_xz_lat(14508.27,	43316.88) # Return latitude
#' loran_xz_lon(14508.27,	43316.88) # Return longitude
#' loranxz2deg(14508.27,	43316.88)
#'
#' @export
loran2deg <- function(x, y, z){
   x <- as.loran(x)
   y <- as.loran(y)

   index <- !is.na(x) & !is.na(y)
   xsave <- x
   ysave <- y
   x <- x[index]
   y <- y[index]

   if (length(x) != length(y)) stop("'x' and 'y' must be the same length.")
   if (length(x) == 0) stop("One or both of coordinate vectors are empty.")

   if (length(x) > 1){
      long <- rep(NA, length(x))
      lat  <- rep(NA, length(x))
      for (i in 1:length(x)){
         r <- loran2deg(x[i], y[i])
         long[i] <- r$long
         lat[i] <- r$lat
      }
   }else{
   #========================== LATITUDE CONVERSION =============================
   Pi = 3.141592653
   p2 = Pi / 180;
   Ae = 6378206.4;
   Fl = 0.0033900753;
   rlax = 0.95;
   tImine1 = 'start';
   tImine2 = 'end';
   l = 0;

   m = 1;

   dor1 = 2;

   xd1 = 41;
   xm1 = 15;
   xs1 = 11.98;
   xd2 = 69;
   xm2 = 58;
   xs2 = 40.51;
   xd3 = 46;
   xm3 = 48;
   xs3 = 27.54;
   xd4 = 67;
   xm4 = 55;
   xs4 = 39.35;
   count1 = 4263.76;
   delta1 = 11000;
   yd1 = 46;
   ym1 = 46;
   ys1 = 32.62;
   yd2 = 53;
   ym2 = 10;
   ys2 = 32.41;
   yd3 = xd3;
   ym3 = xm3;
   ys3 = xs3;
   yd4 = xd4;
   ym4 = xm4;
   ys4 = xs4;
   count2 = 7509.86;
   delta2 = 25000;
   dd1 = 49;
   dm1 = 51;
   ds1 = 30.07;
   dd2 = 64;
   dm2 = 26;
   ds2 = 41.18;

   nstn = 1;
   dur = 1;
   xlIsect1 = 45;
   xlIsect2 = 30;
   xlIsect3 = 30;
   xlIsect4 = 65;
   xlIsect5 = 0;
   xlIsect6 = 0;

   pos11 = (xd1 + xm1 / 60 + xs1 / 3600) * p2;
   pos12 = (xd2 + xm2 / 60 + xs2 / 3600) * p2;
   pos21 = (xd3 + xm3 / 60 + xs3 / 3600) * p2;
   pos22 = (xd4 + xm4 / 60 + xs4 / 3600) * p2;

   Php = pos11;
   Flp = pos12;
   Phs = pos21;
   Fls = pos22;

   Dlon = Fls - Flp;
   Sl = sin(Dlon);
   Cl = cos(Dlon);
   Sp = sin(Php);
   Cp = cos(Php);
   SS = sin(Phs);
   Cs = cos(Phs);
   C = SS * Sp + Cs * Cp * Cl;
   S = sqrt(1 - C ^ 2);
   U = atan(S / C);
   Dv = (U + 3 * S) / (1 - C);
   Dv = Dv * (Sp - SS) ^ 2;
   Dw = (U - 3 * S) / (1 + C);
   Dw = Dw * (Sp + SS) ^ 2;
   Range = Ae * (U - 0.25 * Fl * (Dv + Dw));
   a_1 = (-1) * Cp * SS + Sp * Cs * Cl;
   a_1 = a_1 * Ae / S;
   a_2 = (-1) * Ae * Cp * Cs * Sl / S;

   base1 = Range;
   wIdegth1 = base1 / count1;

   pos31 = (yd1 + ym1 / 60 + ys1 / 3600) * p2;
   pos32 = (yd2 + ym2 / 60 + ys2 / 3600) * p2;
   pos41 = (yd3 + ym3 / 60 + ys3 / 3600) * p2;
   pos42 = (yd4 + ym4 / 60 + ys4 / 3600) * p2;

   Php = pos31;
   Flp = pos32;
   Phs = pos41;
   Fls = pos42;

   Dlon = Fls - Flp;
   Sl = sin(Dlon);
   Cl = cos(Dlon);
   Sp = sin(Php);
   Cp = cos(Php);
   SS = sin(Phs);
   Cs = cos(Phs);
   C = SS * Sp + Cs * Cp * Cl;
   S = sqrt(1 - C ^ 2);
   U = atan(S / C);
   Dv = (U + 3 * S) / (1 - C);
   Dv = Dv * (Sp - SS) ^ 2;
   Dw = (U - 3 * S) / (1 + C);
   Dw = Dw * (Sp + SS) ^ 2;
   Range = Ae * (U - 0.25 * Fl * (Dv + Dw));
   a_1 = (-1) * Cp * SS + Sp * Cs * Cl;
   a_1 = a_1 * Ae / S;
   a_2 = (-1) * Ae * Cp * Cs * Sl / S;

   base2 = Range;
   wIdegth2 = base2 / count2;

   pos51 = (xlIsect1 + xlIsect2 / 60 + xlIsect3 / 3600) * p2;
   pos52 = (xlIsect4 + xlIsect5 / 60 + xlIsect6 / 3600) * p2;

   pa1 = x - delta1;
   pa2 = y - delta2;
   test = 1000;
   iter = 0;

   while ((iter < 40) && (test > 2)){
      iter = iter + 1;

      Php = pos51;
      Flp = pos52;
      Phs = pos11;
      Fls = pos12;

      Dlon = Fls - Flp;
      Sl = sin(Dlon);
      Cl = cos(Dlon);
      Sp = sin(Php);
      Cp = cos(Php);
      SS = sin(Phs);
      Cs = cos(Phs);
      C = SS * Sp + Cs * Cp * Cl;
      S = sqrt(1 - C ^ 2);
      U = atan(S / C);
      Dv = (U + 3 * S) / (1 - C);
      Dv = Dv * (Sp - SS) ^ 2;
      Dw = (U - 3 * S) / (1 + C);
      Dw = Dw * (Sp + SS) ^ 2;
      Range = Ae * (U - 0.25 * Fl * (Dv + Dw));
      a_1 = (-1) * Cp * SS + Sp * Cs * Cl;
      a_1 = a_1 * Ae / S;
      a_2 = (-1) * Ae * Cp * Cs * Sl / S;

      r1 = Range;
      a1 = a_1;
      b1 = a_2;

      Php = pos51;
      Flp = pos52;
      Phs = pos21;
      Fls = pos22;

      Dlon = Fls - Flp;
      Sl = sin(Dlon);
      Cl = cos(Dlon);
      Sp = sin(Php);
      Cp = cos(Php);
      SS = sin(Phs);
      Cs = cos(Phs);
      C = SS * Sp + Cs * Cp * Cl;
      S = sqrt(1 - C ^ 2);
      U = atan(S / C);
      Dv = (U + 3 * S) / (1 - C);
      Dv = Dv * (Sp - SS) ^ 2;
      Dw = (U - 3 * S) / (1 + C);
      Dw = Dw * (Sp + SS) ^ 2;
      Range = Ae * (U - 0.25 * Fl * (Dv + Dw));
      a_1 = (-1) * Cp * SS + Sp * Cs * Cl;
      a_1 = a_1 * Ae / S;
      a_2 = (-1) * Ae * Cp * Cs * Sl / S;

      r2 = Range;
      a2 = a_1;
      b2 = a_2;

      Php = pos51;
      Flp = pos52;
      Phs = pos31;
      Fls = pos32;

      Dlon = Fls - Flp;
      Sl = sin(Dlon);
      Cl = cos(Dlon);
      Sp = sin(Php);
      Cp = cos(Php);
      SS = sin(Phs);
      Cs = cos(Phs);
      C = SS * Sp + Cs * Cp * Cl;
      S = sqrt(1 - C ^ 2);
      U = atan(S / C);
      Dv = (U + 3 * S) / (1 - C);
      Dv = Dv * (Sp - SS) ^ 2;
      Dw = (U - 3 * S) / (1 + C);
      Dw = Dw * (Sp + SS) ^ 2;
      Range = Ae * (U - 0.25 * Fl * (Dv + Dw));
      a_1 = (-1) * Cp * SS + Sp * Cs * Cl;
      a_1 = a_1 * Ae / S;
      a_2 = (-1) * Ae * Cp * Cs * Sl / S;

      r3 = Range;
      a3 = a_1;
      b3 = a_2;

      Php = pos51;
      Flp = pos52;
      Phs = pos41;
      Fls = pos42;

      Dlon = Fls - Flp;
      Sl = sin(Dlon);
      Cl = cos(Dlon);
      Sp = sin(Php);
      Cp = cos(Php);
      SS = sin(Phs);
      Cs = cos(Phs);
      C = SS * Sp + Cs * Cp * Cl;
      S = sqrt(1 - C ^ 2);
      U = atan(S / C);
      Dv = (U + 3 * S) / (1 - C);
      Dv = Dv * (Sp - SS) ^ 2;
      Dw = (U - 3 * S) / (1 + C);
      Dw = Dw * (Sp + SS) ^ 2;
      Range = Ae * (U - 0.25 * Fl * (Dv + Dw));
      a_1 = (-1) * Cp * SS + Sp * Cs * Cl;
      a_1 = a_1 * Ae / S;
      a_2 = (-1) * Ae * Cp * Cs * Sl / S;

      r4 = Range;
      a4 = a_1;
      b4 = a_2;

      r2 = r1 - r2;
      r3 = r3 - r4;
      a2 = a1 - a2;
      b2 = b1 - b2;
      a3 = a3 - a4;
      b3 = b3 - b4;

      cr1 = r2 + base1;
      a1 = a2;
      b1 = b2;
      hyp1 = pa1 * wIdegth1 * 2;
      dr1 = hyp1 - cr1;

      cr2 = r3 + base2;
      a2 = a3;
      b2 = b3;
      hyp2 = pa2 * wIdegth2 * 2;
      dr2 = hyp2 - cr2;

      test = abs(dr1) + abs(dr2);
      det = a1 * b2 - a2 * b1;

      a1 = a1 / det;
      b1 = b1 / det;

      a2 = a2 / det;
      b2 = b2 / det;

      tmp = a1;
      a1 = b2;
      b2 = tmp;
      a2 = a2 * (-1);
      b1 = b1 * (-1);

      pos51 = pos51 + a1 * rlax * dr1 + b1 * rlax * dr2;
      pos52 = pos52 + a2 * rlax * dr2 + b2 * rlax * dr2;

      if (all(det == 0)){
          test = 0;
          pos51 = 0;
          pos52 = 0;
      }
      if (iter == 40){
          pos51 = 0;
      }
   }

   Ang = pos51;
   lat = Ang * 180 / Pi;

   #========================== LONGITUDE CONVERSION ============================
   Pi = 3.14159265358979;
   p2 = Pi / 180;
   Ae = 6378206.4;
   Fl = 0.0033900753;
   rlax = 0.95;
   tImine1 = 'start';
   tImine2 = 'end';
   l = 0;

   m = 1;
   dor1 = 2;

   xd1 = 41;
   xm1 = 15;
   xs1 = 11.98;
   xd2 = 69;
   xm2 = 58;
   xs2 = 40.51;
   xd3 = 46;
   xm3 = 48;
   xs3 = 27.54;
   xd4 = 67;
   xm4 = 55;
   xs4 = 39.35;
   count1 = 4263.76;
   delta1 = 11000;
   yd1 = 46;
   ym1 = 46;
   ys1 = 32.62;
   yd2 = 53;
   ym2 = 10;
   ys2 = 32.41;
   yd3 = xd3;
   ym3 = xm3;
   ys3 = xs3;
   yd4 = xd4;
   ym4 = xm4;
   ys4 = xs4;
   count2 = 7509.86;
   delta2 = 25000;
   dd1 = 49;
   dm1 = 51;
   ds1 = 30.07;
   dd2 = 64;
   dm2 = 26;
   ds2 = 41.18;

   nstn = 1;
   dur = 1;
   xlIsect1 = 45;
   xlIsect2 = 30;
   xlIsect3 = 30;
   xlIsect4 = 65;
   xlIsect5 = 0;
   xlIsect6 = 0;

   pos11 = (xd1 + xm1 / 60 + xs1 / 3600) * p2;
   pos12 = (xd2 + xm2 / 60 + xs2 / 3600) * p2;
   pos21 = (xd3 + xm3 / 60 + xs3 / 3600) * p2;
   pos22 = (xd4 + xm4 / 60 + xs4 / 3600) * p2;

   Php = pos11;
   Flp = pos12;
   Phs = pos21;
   Fls = pos22;

   Dlon = Fls - Flp;
   Sl = sin(Dlon);
   Cl = cos(Dlon);
   Sp = sin(Php);
   Cp = cos(Php);
   SS = sin(Phs);
   Cs = cos(Phs);
   C = SS * Sp + Cs * Cp * Cl;
   S = sqrt(1 - C ^ 2);
   U = atan(S / C);
   Dv = (U + 3 * S) / (1 - C);
   Dv = Dv * (Sp - SS) ^ 2;
   Dw = (U - 3 * S) / (1 + C);
   Dw = Dw * (Sp + SS) ^ 2;
   Range = Ae * (U - 0.25 * Fl * (Dv + Dw));
   a_1 = (-1) * Cp * SS + Sp * Cs * Cl;
   a_1 = a_1 * Ae / S;
   a_2 = (-1) * Ae * Cp * Cs * Sl / S;

   base1 = Range;
   wIdegth1 = base1 / count1;

   pos31 = (yd1 + ym1 / 60 + ys1 / 3600) * p2;
   pos32 = (yd2 + ym2 / 60 + ys2 / 3600) * p2;
   pos41 = (yd3 + ym3 / 60 + ys3 / 3600) * p2;
   pos42 = (yd4 + ym4 / 60 + ys4 / 3600) * p2;

   Php = pos31;
   Flp = pos32;
   Phs = pos41;
   Fls = pos42;

   Dlon = Fls - Flp;
   Sl = sin(Dlon);
   Cl = cos(Dlon);
   Sp = sin(Php);
   Cp = cos(Php);
   SS = sin(Phs);
   Cs = cos(Phs);
   C = SS * Sp + Cs * Cp * Cl;
   S = sqrt(1 - C ^ 2);
   U = atan(S / C);
   Dv = (U + 3 * S) / (1 - C);
   Dv = Dv * (Sp - SS) ^ 2;
   Dw = (U - 3 * S) / (1 + C);
   Dw = Dw * (Sp + SS) ^ 2;
   Range = Ae * (U - 0.25 * Fl * (Dv + Dw));
   a_1 = (-1) * Cp * SS + Sp * Cs * Cl;
   a_1 = a_1 * Ae / S;
   a_2 = (-1) * Ae * Cp * Cs * Sl / S;

   base2 = Range;
   wIdegth2 = base2 / count2;

   pos51 = (xlIsect1 + xlIsect2 / 60 + xlIsect3 / 3600) * p2;
   pos52 = (xlIsect4 + xlIsect5 / 60 + xlIsect6 / 3600) * p2;

   pa1 = x - delta1;
   pa2 = y - delta2;
   test = 1000;
   iter = 0;

   while ((iter < 40) && (test > 2)){
      iter = iter + 1;

      Php = pos51;
      Flp = pos52;
      Phs = pos11;
      Fls = pos12;

      Dlon = Fls - Flp;
      Sl = sin(Dlon);
      Cl = cos(Dlon);
      Sp = sin(Php);
      Cp = cos(Php);
      SS = sin(Phs);
      Cs = cos(Phs);
      C = SS * Sp + Cs * Cp * Cl;
      S = sqrt(1 - C ^ 2);
      U = atan(S / C);
      Dv = (U + 3 * S) / (1 - C);
      Dv = Dv * (Sp - SS) ^ 2;
      Dw = (U - 3 * S) / (1 + C);
      Dw = Dw * (Sp + SS) ^ 2;
      Range = Ae * (U - 0.25 * Fl * (Dv + Dw));
      a_1 = (-1) * Cp * SS + Sp * Cs * Cl;
      a_1 = a_1 * Ae / S;
      a_2 = (-1) * Ae * Cp * Cs * Sl / S;

      r1 = Range;
      a1 = a_1;
      b1 = a_2;

      Php = pos51;
      Flp = pos52;
      Phs = pos21;
      Fls = pos22;

      Dlon = Fls - Flp;
      Sl = sin(Dlon);
      Cl = cos(Dlon);
      Sp = sin(Php);
      Cp = cos(Php);
      SS = sin(Phs);
      Cs = cos(Phs);
      C = SS * Sp + Cs * Cp * Cl;
      S = sqrt(1 - C ^ 2);
      U = atan(S / C);
      Dv = (U + 3 * S) / (1 - C);
      Dv = Dv * (Sp - SS) ^ 2;
      Dw = (U - 3 * S) / (1 + C);
      Dw = Dw * (Sp + SS) ^ 2;
      Range = Ae * (U - 0.25 * Fl * (Dv + Dw));
      a_1 = (-1) * Cp * SS + Sp * Cs * Cl;
      a_1 = a_1 * Ae / S;
      a_2 = (-1) * Ae * Cp * Cs * Sl / S;

      r2 = Range;
      a2 = a_1;
      b2 = a_2;

      Php = pos51;
      Flp = pos52;
      Phs = pos31;
      Fls = pos32;

      Dlon = Fls - Flp;
      Sl = sin(Dlon);
      Cl = cos(Dlon);
      Sp = sin(Php);
      Cp = cos(Php);
      SS = sin(Phs);
      Cs = cos(Phs);
      C = SS * Sp + Cs * Cp * Cl;
      S = sqrt(1 - C ^ 2);
      U = atan(S / C);
      Dv = (U + 3 * S) / (1 - C);
      Dv = Dv * (Sp - SS) ^ 2;
      Dw = (U - 3 * S) / (1 + C);
      Dw = Dw * (Sp + SS) ^ 2;
      Range = Ae * (U - 0.25 * Fl * (Dv + Dw));
      a_1 = (-1) * Cp * SS + Sp * Cs * Cl;
      a_1 = a_1 * Ae / S;
      a_2 = (-1) * Ae * Cp * Cs * Sl / S;

      r3 = Range;
      a3 = a_1;
      b3 = a_2;

      Php = pos51;
      Flp = pos52;
      Phs = pos41;
      Fls = pos42;

      Dlon = Fls - Flp;
      Sl = sin(Dlon);
      Cl = cos(Dlon);
      Sp = sin(Php);
      Cp = cos(Php);
      SS = sin(Phs);
      Cs = cos(Phs);
      C = SS * Sp + Cs * Cp * Cl;
      S = sqrt(1 - C ^ 2);
      U = atan(S / C);
      Dv = (U + 3 * S) / (1 - C);
      Dv = Dv * (Sp - SS) ^ 2;
      Dw = (U - 3 * S) / (1 + C);
      Dw = Dw * (Sp + SS) ^ 2;
      Range = Ae * (U - 0.25 * Fl * (Dv + Dw));
      a_1 = (-1) * Cp * SS + Sp * Cs * Cl;
      a_1 = a_1 * Ae / S;
      a_2 = (-1) * Ae * Cp * Cs * Sl / S;

      r4 = Range;
      a4 = a_1;
      b4 = a_2;

      r2 = r1 - r2;
      r3 = r3 - r4;
      a2 = a1 - a2;
      b2 = b1 - b2;
      a3 = a3 - a4;
      b3 = b3 - b4;

      cr1 = r2 + base1;
      a1 = a2;
      b1 = b2;
      hyp1 = pa1 * wIdegth1 * 2;
      dr1 = hyp1 - cr1;

      cr2 = r3 + base2;
      a2 = a3;
      b2 = b3;
      hyp2 = pa2 * wIdegth2 * 2;
      dr2 = hyp2 - cr2;

      test = abs(dr1) + abs(dr2);
      det = a1 * b2 - a2 * b1;

      a1 = a1 / det;
      b1 = b1 / det;

      a2 = a2 / det;
      b2 = b2 / det;

      tmp = a1;
      a1 = b2;
      b2 = tmp;
      a2 = a2 * (-1);
      b1 = b1 * (-1);

      pos51 = pos51 + a1 * rlax * dr1 + b1 * rlax * dr2;
      pos52 = pos52 + a2 * rlax * dr2 + b2 * rlax * dr2;

      if (all(det == 0)){
          test = 0;
          pos51 = 0;
          pos52 = 0;
      }

      if (iter == 40) pos52 = 0;
   }
   Ang = pos52;
   long = -(Ang * 180 / Pi);

   # Restore original vector.
   xsave <- xsave * NA
   ysave <- ysave * NA
   xsave[index] <- long
   ysave[index] <- lat
   long <- xsave
   lat <- ysave
   }

   return(data.frame(lat = lat, long = long))
}

#' @export
loranxz2deg <- function(x, z){

   ix <- which(!is.na(x) & !is.na(z) & (x > 0) & (z > 0))

   v <- rep(NA, length(x))
   if (length(ix) > 0){
      v[ix] <- data.frame(longitude = -loranxz2lon(x[ix], z[ix]), latitude = loranxz2lat(x[ix], z[ix]))
   }

   return(v)
}

loranxz2lat <- function(x, z){
   # Returns the latitude for a given loran X and Z coordinate:
   Pi = 3.14159265358979;   p2 = Pi / 180;
   Ae = 6378206.4; Fl = 0.0033900753; rlax = 0.75;
   tImine1 = "start"; tImine2 = "end";
   l = 0; m = 1; dor1 = 2;

   xd1 = 41; xm1 = 15; xs1 = 11.98; xd2 = 69; xm2 = 58; xs2 = 40.51; xd3 = 46;
   xm3 = 48; xs3 = 27.54; xd4 = 67; xm4 = 55; xs4 = 39.35; count1 = 4263.76;
   delta1 = 11000; yd1 = 52; ym1 = 22; ys1 = 35.11; yd2 = 55; ym2 = 42;
   ys2 = 31.35; yd3 = xd3; ym3 = xm3; ys3 = xs3; yd4 = xd4; ym4 = xm4; ys4 = xs4;
   count2 = 7189.12; delta2 = 38000; dd1 = 49; dm1 = 51; ds1 = 30.07; dd2 = 64;
   dm2 = 26; ds2 = 41.18; nstn = 1; dur = 1; xlIsect1 = 45; xlIsect2 = 30;
   xlIsect3 = 30; xlIsect4 = 65; xlIsect5 = 0; xlIsect6 = 0;

   pos11 = (xd1 + xm1 / 60 + xs1 / 3600) * p2;
   pos12 = (xd2 + xm2 / 60 + xs2 / 3600) * p2;
   pos21 = (xd3 + xm3 / 60 + xs3 / 3600) * p2;
   pos22 = (xd4 + xm4 / 60 + xs4 / 3600) * p2;

   Php = pos11; Flp = pos12; Phs = pos21; Fls = pos22; Dlon = Fls - Flp;
   Sl = sin(Dlon); Cl = cos(Dlon); Sp = sin(Php); Cp = cos(Php); Ss = sin(Phs); Cs = cos(Phs);
   C = Ss * Sp + Cs * Cp * Cl; S = sqrt(1 - C ^ 2); U = atan(S / C);
   Dv = (U + 3 * S) / (1 - C); Dv = Dv * (Sp - Ss) ^ 2; Dw = (U - 3 * S) / (1 + C);
   Dw = Dw * (Sp + Ss) ^ 2;
   Range = Ae * (U - 0.25 * Fl * (Dv + Dw));
   a_1 = (-1) * Cp * Ss + Sp * Cs * Cl; a_1 = a_1 * Ae / S;
   a_2 = (-1) * Ae * Cp * Cs * Sl / S;
   base1 = Range; wIdegth1 = base1 / count1;

   pos31 = (yd1 + ym1 / 60 + ys1 / 3600) * p2;
   pos32 = (yd2 + ym2 / 60 + ys2 / 3600) * p2;
   pos41 = (yd3 + ym3 / 60 + ys3 / 3600) * p2;
   pos42 = (yd4 + ym4 / 60 + ys4 / 3600) * p2;

   Php = pos31; Flp = pos32; Phs = pos41; Fls = pos42; Dlon = Fls - Flp; Sl = sin(Dlon);
   Cl = cos(Dlon); Sp = sin(Php); Cp = cos(Php); Ss = sin(Phs); Cs = cos(Phs);
   C = Ss * Sp + Cs * Cp * Cl; S = sqrt(1 - C ^ 2); U = atan(S / C);
   Dv = (U + 3 * S) / (1 - C); Dv = Dv * (Sp - Ss) ^ 2; Dw = (U - 3 * S) / (1 + C);
   Dw = Dw * (Sp + Ss) ^ 2; Range = Ae * (U - 0.25 * Fl * (Dv + Dw));
   a_1 = (-1) * Cp * Ss + Sp * Cs * Cl; a_1 = a_1 * Ae / S;
   a_2 = (-1) * Ae * Cp * Cs * Sl / S;
   base2 = Range; wIdegth2 = base2 / count2;

   pos51 = (xlIsect1 + xlIsect2 / 60 + xlIsect3 / 3600) * p2
   pos52 = (xlIsect4 + xlIsect5 / 60 + xlIsect6 / 3600) * p2

   pa1 = x - delta1; pa2 = z - delta2;
   test = 1000;
   iter = 0;

   while ((iter < 40) && (test > 2)){
      iter = iter + 1;

      Php = pos51; Flp = pos52; Phs = pos11; Fls = pos12;
      Dlon = Fls - Flp;
      Sl = sin(Dlon); Cl = cos(Dlon); Sp = sin(Php); Cp = cos(Php); Ss = sin(Phs); Cs = cos(Phs);
      C = Ss * Sp + Cs * Cp * Cl; S = sqrt(1 - C ^ 2); U = atan(S / C);
      Dv = (U + 3 * S) / (1 - C); Dv = Dv * (Sp - Ss) ^ 2; Dw = (U - 3 * S) / (1 + C);
      Dw = Dw * (Sp + Ss) ^ 2;
      Range = Ae * (U - 0.25 * Fl * (Dv + Dw));
      a_1 = (-1) * Cp * Ss + Sp * Cs * Cl; a_1 = a_1 * Ae / S; a_2 = (-1) * Ae * Cp * Cs * Sl / S;

      r1 = Range; a1 = a_1; b1 = a_2;
      Php = pos51; Flp = pos52; Phs = pos21; Fls = pos22;

      Dlon = Fls - Flp;
      Sl = sin(Dlon); Cl = cos(Dlon); Sp = sin(Php); Cp = cos(Php); Ss = sin(Phs); Cs = cos(Phs);
      C = Ss * Sp + Cs * Cp * Cl; S = sqrt(1 - C ^ 2); U = atan(S / C); Dv = (U + 3 * S) / (1 - C);
      Dv = Dv * (Sp - Ss) ^ 2; Dw = (U - 3 * S) / (1 + C); Dw = Dw * (Sp + Ss) ^ 2;
      Range = Ae * (U - 0.25 * Fl * (Dv + Dw));
      a_1 = (-1) * Cp * Ss + Sp * Cs * Cl; a_1 = a_1 * Ae / S; a_2 = (-1) * Ae * Cp * Cs * Sl / S;

      r2 = Range; a2 = a_1; b2 = a_2;
      Php = pos51; Flp = pos52; Phs = pos31; Fls = pos32;

      Dlon = Fls - Flp;
      Sl = sin(Dlon); Cl = cos(Dlon); Sp = sin(Php); Cp = cos(Php); Ss = sin(Phs); Cs = cos(Phs);
      C = Ss * Sp + Cs * Cp * Cl; S = sqrt(1 - C ^ 2); U = atan(S / C); Dv = (U + 3 * S) / (1 - C);
      Dv = Dv * (Sp - Ss) ^ 2; Dw = (U - 3 * S) / (1 + C); Dw = Dw * (Sp + Ss) ^ 2;
      Range = Ae * (U - 0.25 * Fl * (Dv + Dw));
      a_1 = (-1) * Cp * Ss + Sp * Cs * Cl; a_1 = a_1 * Ae / S; a_2 = (-1) * Ae * Cp * Cs * Sl / S;

      r3 = Range; a3 = a_1; b3 = a_2;

      Php = pos51; Flp = pos52; Phs = pos41; Fls = pos42;

      Dlon = Fls - Flp;
      Sl = sin(Dlon); Cl = cos(Dlon); Sp = sin(Php); Cp = cos(Php); Ss = sin(Phs); Cs = cos(Phs)
      C = Ss * Sp + Cs * Cp * Cl; S = sqrt(1 - C ^ 2); U = atan(S / C);
      Dv = (U + 3 * S) / (1 - C); Dv = Dv * (Sp - Ss) ^ 2; Dw = (U - 3 * S) / (1 + C); Dw = Dw * (Sp + Ss) ^ 2;
      Range = Ae * (U - 0.25 * Fl * (Dv + Dw));
      a_1 = (-1) * Cp * Ss + Sp * Cs * Cl; a_1 = a_1 * Ae / S; a_2 = (-1) * Ae * Cp * Cs * Sl / S;

      r4 = Range; a4 = a_1; b4 = a_2;
      r2 = r1 - r2; r3 = r3 - r4; a2 = a1 - a2; b2 = b1 - b2; a3 = a3 - a4; b3 = b3 - b4;
      cr1 = r2 + base1; a1 = a2; b1 = b2; hyp1 = pa1 * wIdegth1 * 2; dr1 = hyp1 - cr1;

      cr2 = r3 + base2; a2 = a3; b2 = b3; hyp2 = pa2 * wIdegth2 * 2; dr2 = hyp2 - cr2;

      test = abs(dr1) + abs(dr2)
      det = a1 * b2 - a2 * b1

      a1 = a1 / det; b1 = b1 / det; a2 = a2 / det; b2 = b2 / det;
      tmp = a1; a1 = b2; b2 = tmp; a2 = a2 * (-1); b1 = b1 * (-1);

      pos51 = pos51 + a1 * rlax * dr1 + b1 * rlax * dr2
      pos52 = pos52 + a2 * rlax * dr2 + b2 * rlax * dr2

      if (det == 0){
         test = 0
         pos51 = 0
         pos52 = 0
      }
      if (iter == 40) pos51 = 0
   }

   # Ang = pos51
   Loranc1 = pos51 * 180 / Pi
   return(Loranc1)
}

loranxz2lon <- function(x, z){
   # Returns the longitude for a given loran X and Z coordinate:
    Pi = 3.14159265358979; p2 = Pi / 180; Ae = 6378206.4; Fl = 0.0033900753; rlax = 0.75;
    tImine1 = "start"; tImine2 = "end"; l = 0; m = 1; dor1 = 2;
    xd1 = 41; xm1 = 15; xs1 = 11.98; xd2 = 69; xm2 = 58; xs2 = 40.51; xd3 = 46;
    xm3 = 48; xs3 = 27.54; xd4 = 67; xm4 = 55; xs4 = 39.35; count1 = 4263.76;
    delta1 = 11000; yd1 = 52; ym1 = 22; ys1 = 35.11; yd2 = 55; ym2 = 42; ys2 = 31.35;
    yd3 = xd3; ym3 = xm3; ys3 = xs3; yd4 = xd4; ym4 = xm4; ys4 = xs4;
    count2 = 7189.12; delta2 = 38000; dd1 = 49; dm1 = 51; ds1 = 30.07; dd2 = 64;
    dm2 = 26; ds2 = 41.18;

    nstn = 1; dur = 1; xlIsect1 = 45; xlIsect2 = 30; xlIsect3 = 30; xlIsect4 = 65; xlIsect5 = 0; xlIsect6 = 0;

    pos11 = (xd1 + xm1 / 60 + xs1 / 3600) * p2;
    pos12 = (xd2 + xm2 / 60 + xs2 / 3600) * p2;
    pos21 = (xd3 + xm3 / 60 + xs3 / 3600) * p2;
    pos22 = (xd4 + xm4 / 60 + xs4 / 3600) * p2;

    Php = pos11; Flp = pos12; Phs = pos21; Fls = pos22;

    Dlon = Fls - Flp;
    Sl = sin(Dlon);
    Cl = cos(Dlon);
    Sp = sin(Php);
    Cp = cos(Php);
    Ss = sin(Phs);
    Cs = cos(Phs);
    C = Ss * Sp + Cs * Cp * Cl;
    S = sqrt(1 - C ^ 2);
    U = atan(S / C);
    Dv = (U + 3 * S) / (1 - C);
    Dv = Dv * (Sp - Ss) ^ 2;
    Dw = (U - 3 * S) / (1 + C);
    Dw = Dw * (Sp + Ss) ^ 2;
    Range = Ae * (U - 0.25 * Fl * (Dv + Dw));
    a_1 = (-1) * Cp * Ss + Sp * Cs * Cl;
    a_1 = a_1 * Ae / S;
    a_2 = (-1) * Ae * Cp * Cs * Sl / S;

    base1 = Range;
    wIdegth1 = base1 / count1;

    pos31 = (yd1 + ym1 / 60 + ys1 / 3600) * p2;
    pos32 = (yd2 + ym2 / 60 + ys2 / 3600) * p2;
    pos41 = (yd3 + ym3 / 60 + ys3 / 3600) * p2;
    pos42 = (yd4 + ym4 / 60 + ys4 / 3600) * p2;

    Php = pos31;
    Flp = pos32;
    Phs = pos41;
    Fls = pos42;

    Dlon = Fls - Flp;
    Sl = sin(Dlon);
    Cl = cos(Dlon);
    Sp = sin(Php);
    Cp = cos(Php);
    Ss = sin(Phs);
    Cs = cos(Phs);
    C = Ss * Sp + Cs * Cp * Cl;
    S = sqrt(1 - C ^ 2);
    U = atan(S / C);
    Dv = (U + 3 * S) / (1 - C);
    Dv = Dv * (Sp - Ss) ^ 2;
    Dw = (U - 3 * S) / (1 + C);
    Dw = Dw * (Sp + Ss) ^ 2;
    Range = Ae * (U - 0.25 * Fl * (Dv + Dw));
    a_1 = (-1) * Cp * Ss + Sp * Cs * Cl;
    a_1 = a_1 * Ae / S;
    a_2 = (-1) * Ae * Cp * Cs * Sl / S;

    base2 = Range;
    wIdegth2 = base2 / count2;

    pos51 = (xlIsect1 + xlIsect2 / 60 + xlIsect3 / 3600) * p2;
    pos52 = (xlIsect4 + xlIsect5 / 60 + xlIsect6 / 3600) * p2;

    pa1 = x - delta1;
    pa2 = z - delta2;
    test = 1000;
    iter = 0;

   while ((iter < 40) && (test > 2)){
      iter = iter + 1;

       Php = pos51;
        Flp = pos52;
        Phs = pos11;
        Fls = pos12;

        Dlon = Fls - Flp;
        Sl = sin(Dlon);
        Cl = cos(Dlon);
        Sp = sin(Php);
        Cp = cos(Php);
        Ss = sin(Phs);
        Cs = cos(Phs);
        C = Ss * Sp + Cs * Cp * Cl;
        S = sqrt(1 - C ^ 2);
        U = atan(S / C);
        Dv = (U + 3 * S) / (1 - C);
        Dv = Dv * (Sp - Ss) ^ 2;
        Dw = (U - 3 * S) / (1 + C);
        Dw = Dw * (Sp + Ss) ^ 2;
        Range = Ae * (U - 0.25 * Fl * (Dv + Dw));
        a_1 = (-1) * Cp * Ss + Sp * Cs * Cl;
        a_1 = a_1 * Ae / S;
        a_2 = (-1) * Ae * Cp * Cs * Sl / S;

        r1 = Range;
        a1 = a_1;
        b1 = a_2;

        Php = pos51;
        Flp = pos52;
        Phs = pos21;
        Fls = pos22;

        Dlon = Fls - Flp;
        Sl = sin(Dlon);
        Cl = cos(Dlon);
        Sp = sin(Php);
        Cp = cos(Php);
        Ss = sin(Phs);
        Cs = cos(Phs);
        C = Ss * Sp + Cs * Cp * Cl;
        S = sqrt(1 - C ^ 2);
        U = atan(S / C);
        Dv = (U + 3 * S) / (1 - C);
        Dv = Dv * (Sp - Ss) ^ 2;
        Dw = (U - 3 * S) / (1 + C);
        Dw = Dw * (Sp + Ss) ^ 2;
        Range = Ae * (U - 0.25 * Fl * (Dv + Dw));
        a_1 = (-1) * Cp * Ss + Sp * Cs * Cl;
        a_1 = a_1 * Ae / S;
        a_2 = (-1) * Ae * Cp * Cs * Sl / S;

        r2 = Range;
        a2 = a_1;
        b2 = a_2;

        Php = pos51;
        Flp = pos52;
        Phs = pos31;
        Fls = pos32;

        Dlon = Fls - Flp;
        Sl = sin(Dlon);
        Cl = cos(Dlon);
        Sp = sin(Php);
        Cp = cos(Php);
        Ss = sin(Phs);
        Cs = cos(Phs);
        C = Ss * Sp + Cs * Cp * Cl;
        S = sqrt(1 - C ^ 2);
        U = atan(S / C);
        Dv = (U + 3 * S) / (1 - C);
        Dv = Dv * (Sp - Ss) ^ 2;
        Dw = (U - 3 * S) / (1 + C);
        Dw = Dw * (Sp + Ss) ^ 2;
        Range = Ae * (U - 0.25 * Fl * (Dv + Dw));
        a_1 = (-1) * Cp * Ss + Sp * Cs * Cl;
        a_1 = a_1 * Ae / S;
        a_2 = (-1) * Ae * Cp * Cs * Sl / S;

        r3 = Range;
        a3 = a_1;
        b3 = a_2;

        Php = pos51;
        Flp = pos52;
        Phs = pos41;
        Fls = pos42;

        Dlon = Fls - Flp;
        Sl = sin(Dlon);
        Cl = cos(Dlon);
        Sp = sin(Php);
        Cp = cos(Php);
        Ss = sin(Phs);
        Cs = cos(Phs);
        C = Ss * Sp + Cs * Cp * Cl;
        S = sqrt(1 - C ^ 2);
        U = atan(S / C);
        Dv = (U + 3 * S) / (1 - C);
        Dv = Dv * (Sp - Ss) ^ 2;
        Dw = (U - 3 * S) / (1 + C);
        Dw = Dw * (Sp + Ss) ^ 2;
        Range = Ae * (U - 0.25 * Fl * (Dv + Dw));
        a_1 = (-1) * Cp * Ss + Sp * Cs * Cl;
        a_1 = a_1 * Ae / S;
        a_2 = (-1) * Ae * Cp * Cs * Sl / S;

        r4 = Range;
        a4 = a_1;
        b4 = a_2;

        r2 = r1 - r2;
        r3 = r3 - r4;
        a2 = a1 - a2;
        b2 = b1 - b2;
        a3 = a3 - a4;
        b3 = b3 - b4;

        cr1 = r2 + base1;
        a1 = a2;
        b1 = b2;
        hyp1 = pa1 * wIdegth1 * 2;
        dr1 = hyp1 - cr1;

        cr2 = r3 + base2;
        a2 = a3;
        b2 = b3;
        hyp2 = pa2 * wIdegth2 * 2;
        dr2 = hyp2 - cr2;

        test = abs(dr1) + abs(dr2);
        det = a1 * b2 - a2 * b1;

        a1 = a1 / det;
        b1 = b1 / det;

        a2 = a2 / det;
        b2 = b2 / det;

        tmp = a1;
        a1 = b2;
        b2 = tmp;
        a2 = a2 * (-1);
        b1 = b1 * (-1);

        pos51 = pos51 + a1 * rlax * dr1 + b1 * rlax * dr2;
        pos52 = pos52 + a2 * rlax * dr2 + b2 * rlax * dr2;

        if (det == 0){
            test = 0
            pos51 = 0
            pos52 = 0
        }

        if (iter == 40) pos52 = 0
   }

   Loranc2 = pos52 * 180 / Pi
   return(Loranc2)
}

#' @describeIn loran2deg Clean up loran coordinate strings.
#' @export
as.loran <- function(x){
   # AS.LORAN - Reformats a character string to proper Loran-C numeric format.

   # Check input argument:
   if (!is.null(dim(x))) stop("'x' must be a vector.")
   if (!is.numeric(x) & !is.character(x)) stop("'x' must be a numeric or character vector.")

   full <- rep(NA, length(x))
   if (is.numeric(x)){
      x[x == 0] <- NA
      index <- which(!is.na(x))
      x <- x[index]
   }else{
      index <- rep(TRUE, length(x))
   }

   if (length(x) > 0){
      # Remove spaces, minus signs and leading zeroes, etc... :
      x <- as.character(x)
      x <- gsub("([A-Z])|(NA)|(N/A)|(n/a)|(-)|( )|(?)|[.]|[']", "", x)
      x <- gsub("^[0]*", "", x)

      # Initialize result variable:
      r <- rep(NA, length(x))

      # Identify which strings have the proper format:
      ii <- grep("^[123][0-9]{4,10}", x)
      if (length(ii) > 0) r[ii] <- as.numeric(paste0(substr(x[ii], 1, 5), ".", substr(x[ii], 6, nchar(x[ii]))))
      full[index] <- r
   }
   r <- full

   return(r)
}

#' @describeIn loran2deg Determine if character string is a Loran-type coordinate.
#' @export
is.loran <- function(x) return(!is.na(as.loran(x)))


