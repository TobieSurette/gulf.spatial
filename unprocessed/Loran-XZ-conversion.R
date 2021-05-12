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

loran_xz_lat(14508.27,	43316.88) # Return latitude
loran_xz_lon(14508.27,	43316.88) # Return longitude

loranxz2deg(14508.27,	43316.88)
