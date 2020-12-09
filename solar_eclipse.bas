  ' solar_eclipse.bas         July 13, 2018
  
  ' local circumstances of solar eclipses
  
  ' Micromite eXtreme and Nucleo H743ZI version
  
  '''''''''''''''''''''''''''''''''''''''''''''
  
  option default float
  
  option base 1
  
  ' dimension global arrays and variables
  
  dim xsl(50), xsr(50), xsa(50), xsb(50)
  
  dim jdleap(28), leapsec(28)
  
  dim month$(12) as string
  
  dim xnut(11, 13), trr, elev_minima
  
  dim jdsaved, jdprint, obslat, obslong, obsalt
  
  dim jdtdbi, i as integer, j as integer
  
  dim cmonth, cday, cyear, ndays, obliq
  
  ' global constants
  
  const pi2 = 2.0 * pi, pidiv2 = 0.5 * pi, dtr = pi / 180.0, rtd = 180.0 / pi
  
  const atr = pi / 648000.0, seccon = 206264.8062470964
  
  ' equatorial radius of the earth (kilometers)
  
  const reqm = 6378.14
  
  ' earth flattening factor (nd)
  
  const flat = 1.0 / 298.257
  
  ' astronomical unit (kilometers)
  
  const aunit = 149597870.691
  
  ' radius of the sun (kilometers)
  
  const radsun = 696000.0
  
  ' radius of the moon (kilometers)
  
  const radmoon = 1738.0
  
  ' read solar ephemeris data
  
  for i = 1 to 50
    
    read xsl(i), xsr(i), xsa(i), xsb(i)
    
  next i
  
  data 403406,      0, 4.721964,      1.621043
  data 195207, -97597, 5.937458,  62830.348067
  data 119433, -59715, 1.115589,  62830.821524
  data 112392, -56188, 5.781616,  62829.634302
  data   3891,  -1556, 5.5474 , 125660.5691
  data   2819,  -1126, 1.5120 , 125660.9845
  data   1721,   -861, 4.1897 ,  62832.4766
  data      0,    941, 1.163  ,      0.813
  data    660,   -264, 5.415  , 125659.310
  data    350,   -163, 4.315  ,  57533.850
  data    334,      0, 4.553  ,    -33.931
  data    314,    309, 5.198  , 777137.715
  data    268,   -158, 5.989  ,  78604.191
  data    242,      0, 2.911  ,      5.412
  data    234,    -54, 1.423  ,  39302.098
  data    158,      0, 0.061  ,    -34.861
  data    132,    -93, 2.317  , 115067.698
  data    129,    -20, 3.193  ,  15774.337
  data    114,      0, 2.828  ,   5296.670
  data     99,    -47, 0.52   ,  58849.27
  data     93,      0, 4.65   ,   5296.11
  data     86,      0, 4.35   ,  -3980.70
  data     78,    -33, 2.75   ,  52237.69
  data     72,    -32, 4.50   ,  55076.47
  data     68,      0, 3.23   ,    261.08
  data     64,    -10, 1.22   ,  15773.85
  data     46,    -16, 0.14   , 188491.03
  data     38,      0, 3.44   ,  -7756.55
  data     37,      0, 4.37   ,    264.89
  data     32,    -24, 1.14   , 117906.27
  data     29,    -13, 2.84   ,  55075.75
  data     28,      0, 5.96   ,  -7961.39
  data     27,     -9, 5.09   , 188489.81
  data     27,      0, 1.72   ,   2132.19
  data     25,    -17, 2.56   , 109771.03
  data     24,    -11, 1.92   ,  54868.56
  data     21,      0, 0.09   ,  25443.93
  data     21,     31, 5.98   , -55731.43
  data     20,    -10, 4.03   ,  60697.74
  data     18,      0, 4.27   ,   2132.79
  data     17,    -12, 0.79   , 109771.63
  data     14,      0, 4.24   ,  -7752.82
  data     13,     -5, 2.01   , 188491.91
  data     13,      0, 2.65   ,    207.81
  data     13,      0, 4.98   ,  29424.63
  data     12,      0, 0.93   ,     -7.99
  data     10,      0, 2.21   ,  46941.14
  data     10,      0, 3.59   ,    -68.29
  data     10,      0, 1.50   ,  21463.25
  data     10,     -9, 2.55   , 157208.40
  
  ' read subset of IAU2000 nutation data
  
  for j = 1 to 13
    
    for i = 1 to 11
      
      read xnut(i, j)
      
    next i
    
  next j
  
  data  0,  0, 0,  0, 1, -172064161, -174666, 92052331,  9086,  33386, 15377
  data  0,  0, 2, -2, 2,  -13170906,   -1675,  5730336, -3015, -13696, -4587
  data  0,  0, 2,  0, 2,   -2276413,    -234,   978459,  -485,   2796,  1374
  data  0,  0, 0,  0, 2,    2074554,     207,  -897492,   470,   -698,  -291
  data  0,  1, 0,  0, 0,    1475877,   -3633,    73871,  -184,  11817, -1924
  data  0,  1, 2, -2, 2,    -516821,    1226,   224386,  -677,   -524,  -174
  data  1,  0, 0,  0, 0,     711159,      73,    -6750,     0,   -872,   358
  data  0,  0, 2,  0, 1,    -387298,    -367,   200728,    18,    380,   318
  data  1,  0, 2,  0, 2,    -301461,     -36,   129025,   -63,    816,   367
  data  0, -1, 2, -2, 2,     215829,    -494,   -95929,   299,    111,   132
  data  0,  0, 2, -2, 1,     128227,     137,   -68982,    -9,    181,    39
  data -1,  0, 2,  0, 2,     123457,      11,   -53311,    32,     19,    -4
  data -1,  0, 0,  2, 0,     156994,      10,    -1235,     0,   -168,    82
  
  ' read leap second data
  
  for i = 1 to 28
    
    read jdleap(i), leapsec(i)
    
  next i
  
  data 2441317.5,  10.0
  data 2441499.5,  11.0
  data 2441683.5,  12.0
  data 2442048.5,  13.0
  data 2442413.5,  14.0
  data 2442778.5,  15.0
  data 2443144.5,  16.0
  data 2443509.5,  17.0
  data 2443874.5,  18.0
  data 2444239.5,  19.0
  data 2444786.5,  20.0
  data 2445151.5,  21.0
  data 2445516.5,  22.0
  data 2446247.5,  23.0
  data 2447161.5,  24.0
  data 2447892.5,  25.0
  data 2448257.5,  26.0
  data 2448804.5,  27.0
  data 2449169.5,  28.0
  data 2449534.5,  29.0
  data 2450083.5,  30.0
  data 2450630.5,  31.0
  data 2451179.5,  32.0
  data 2453736.5,  33.0
  data 2454832.5,  34.0
  DATA 2456109.5,  35.0
  data 2457204.5,  36.0
  data 2457754.5,  37.0
  
  ' calendar months
  
  month$(1) = "January"
  month$(2) = "February"
  month$(3) = "March"
  month$(4) = "April"
  month$(5) = "May"
  month$(6) = "June"
  month$(7) = "July"
  month$(8) = "August"
  month$(9) = "September"
  month$(10) = "October"
  month$(11) = "November"
  month$(12) = "December"
  
  print " "
  print "program solar_eclipse"
  print "====================="
  
  ' request initial calendar date (month, day, year)
  
  getdate(cmonth, cday, cyear)
  
  ' request observer coordinates
  
  print " "
  
  observer(obslat, obslong, obsalt)
  
  ' initial utc julian day
  
  julian(cmonth, cday, cyear, jdutc)
  
  ' compute initial tdb julian date
  
  utc2tdb(jdutc, jdtdb)
  
  jdtdbi = jdtdb
  
  ' search duration (days)
  
  print " "
  
  print "please input the search duration in days"
  
  input ndays
  
  print " "
  print "searching for solar eclipse ..."
  print " "
  
  ' define search parameters
  
  ti = 0.0
  
  tf = ndays
  
  dt = 0.25
  
  dtsml = 0.1
  
  ' find eclipse conditions
  
  ecl_event(ti, tf, dt, dtsml)
  
end
  
  '''''''''''''''
  '''''''''''''''
  
sub sefunc(x, fx)
  
  ' solar eclipse objective function
  
  ' input
  
  '  x = elapsed simulation time (days)
  
  ' output
  
  '  fx = objective function at x
  
  '''''''''''''''''''''''''''''''
  
  local rsun(3), rmoon(3)
  
  local rtmoon(3)
  
  local rm2s(3)
  
  local uaxis(3)
  
  local um2o(3)
  
  local dvec(5)
  
  LOCAL jdtdb, jdutc
  
  local cpsi, pangle
  
  LOCAL rm2smag, gast
  
  local rtmmoon, psi
  
  local i as integer
  
  ' current tdb julian date
  
  jdtdb = jdtdbi + x
  
  ' calculate geocentric position vector of the moon (km)
  
  moon(jdtdb, rmoon())
  
  ' calculate geocentric position vector of the sun (km)
  
  sun(jdtdb, rsun())
  
  ' calculate moon-to-sun position vector (km)
  
  for i = 1 to 3
    
    rm2s(i) = rsun(i) - rmoon(i)
    
  next i
  
  ' compute topocentric coordinates of the moon
  
  tdb2utc(jdtdb, jdutc)
  
  gast2(jdutc, gast)
  
  eci2topo(gast, rmoon(), dvec())
  
  rtmoon(1) = dvec(3)
  rtmoon(2) = dvec(4)
  rtmoon(3) = dvec(5)
  
  rtmmoon = vecmag(rtmoon())
  
  ' current topocentric elevation of the moon (radians)
  
  elev_minima = dvec(2)
  
  ' compute shadow axis unit position vector
  
  rm2smag = vecmag(rm2s())
  
  for i = 1 to 3
    
    uaxis(i) = -rm2s(i) / rm2smag
    
  next i
  
  ' compute moon-to-observer unit position vector
  
  for i = 1 to 3
    
    um2o(i) = -rtmoon(i) / rtmmoon
    
  next i
  
  ' calculate penumbra shadow angle
  
  pangle = asin(radmoon / rtmmoon) + asin((radsun + radmoon) / rm2smag)
  
  ' compute separation angle between anti-sun and moon-to-observer vectors
  
  cpsi = vdot(uaxis(), um2o())
  
  psi = acos(cpsi)
  
  ' compute objective function
  
  fx = psi - pangle
  
end sub
  
  '''''''''''''''''''''''''''''''
  '''''''''''''''''''''''''''''''
  
sub ecl_event (ti, tf, dt, dtsml)
  
  ' predict solar eclipse events
  
  ' input
  
  '  ti    = initial simulation time
  '  tf    = final simulation time
  '  dt    = step size used for bounding minima
  '  dtsml = small step size used to determine whether
  '          the function is increasing or decreasing
  
  '''''''''''''''''''''''''''''''''''''''''''''''''''
  
  LOCAL tolm, iend as integer
  
  local fmin1, tmin1
  
  LOCAL ftemp, df, dflft
  
  local el, er
  
  LOCAL t, ft
  
  local iter1 as integer, iter2 as integer
  
  local iter3 as integer
  
  ' initialization
  
  tolm = 1.0e-6
  
  iend = 0
  
  ' check the initial time for a minimum
  
  sefunc(ti, fmin1)
  
  tmin1 = ti
  
  sefunc(ti + dtsml, ftemp)
  
  df = ftemp - fmin1
  
  dflft = df
  
  el = ti
  
  er = el
  
  t = ti
  
  ' if the slope is positive and the minimum is
  ' negative, calculate event conditions at the initial time
  
  if (df > 0.0 and fmin1 < 0.0) then
    
    events1(ti, tf, tmin1)
    
  end if
  
  for iter1 = 1 to 100000
    
    ' find where function first starts decreasing
    
    for iter2 = 1 to 100000
      
      if (df <= 0.0) then
        
        exit for
        
      end if
      
      t = t + dt
      
      if (t >= tf) then
        
        ' check final time for a minimum
        
        if (iend = 1) then exit sub
        
        sefunc(tf, fmin1)
        
        sefunc(tf - dtsml, ftemp)
        
        df = fmin1 - ftemp
        
        ' set minimum time to final simulation time
        
        tmin1 = tf
        
        if (df < 0.0) then
          
          ' if both the slope and minimum are negative,
          ' calculate the event conditions at the final
          ' simulation time
          
          if (fmin1 < 0.0) then
            
            events1(ti, tf, tmin1)
            
          end if
          
          ' otherwise, we're finished
          
          exit sub
          
        end if
        
        if (dflft > 0.0) then exit for
        
        er = tf
        
        iend = 1
        
      end if
      
      sefunc(t, ft)
      
      sefunc(t - dtsml, ftemp)
      
      df = ft - ftemp
      
    next iter2
    
    ' function decreasing - find where function
    ' first starts increasing
    
    for iter3 = 1 to 100000
      
      el = t
      
      dflft = df
      
      t = t + dt
      
      if (t >= tf) then
        
        ' check final time for a minimum
        
        if (iend = 1) then exit sub
        
        sefunc(tf, fmin1)
        
        sefunc(tf - dtsml, ftemp)
        
        df = fmin1 - ftemp
        
        ' set minimum time to final simulation time
        
        tmin1 = tf
        
        if (df < 0.0) then
          
          ' if both the slope and minimum are negative,
          ' calculate the event conditions at the final
          ' simulation time
          
          if (fmin1 < 0.0) then
            
            events1(ti, tf, tmin1)
            
          end if
          
          ' otherwise, we're finished
          
          exit sub
          
        end if
        
        if (dflft > 0.0) then exit sub
        
        er = tf
        
        iend = 1
        
      end if
      
      sefunc(t, ft)
      
      sefunc(t - dtsml, ftemp)
      
      df = ft - ftemp
      
      if (df > 0.0) then exit for
      
    next iter3
    
    er = t
    
    ' calculate minimum using Brent's method
    
    minima(el, er, tolm, tmin1, fmin1)
    
    el = er
    
    dflft = df
    
    ' if the minimum is negative and the topocentric
    ' elevation angle of the moon is positive,
    ' calculate event conditions for this minimum
    
    if (fmin1 < 0.0 and elev_minima > 0.0) then
      
      events1(ti, tf, tmin1)
      
      t = trr
      
    end if
    
  next iter1
  
end sub
  
  ''''''''''''''''''''''''
  ''''''''''''''''''''''''
  
sub events1 (ti, tf, topt)
  
  ' compute and display eclipse events
  
  ' input
  
  '  ti   = initial simulation time
  '  tf   = final simulation time
  '  topt = extrema time
  
  ''''''''''''''''''''''
  
  ' define root-bracketing and root-finding control parameters
  
  LOCAL factor = 0.25            ' geometric acceleration factor
  
  LOCAL dxmax = 600.0 / 86400.0  ' rectification interval
  
  LOCAL rtol = 1.0e-8            ' root-finding convergence tolerance
  
  LOCAL t1in, t2in
  
  local t1out, t2out
  
  LOCAL troot, froot, jdate
  
  ' compute and display event start conditions
  
  t1in = topt
  
  t2in = t1in - 30.0 / 86400.0
  
  broot(t1in, t2in, factor, dxmax, t1out, t2out)
  
  brent(t1out, t2out, rtol, troot, froot)
  
  ' set to initial time if before ti
  
  if (troot < ti) then
    
    troot = ti
    
    sefunc(ti, froot)
    
  end if
  
  jdate = jdtdbi + troot
  
  eclprint(1, jdate)
  
  ' compute and display greatest eclipse conditions
  
  jdate = jdtdbi + topt
  
  eclprint(2, jdate)
  
  ' compute and display event end conditions
  
  t2in = t1in + 30.0 / 86400.0
  
  broot(t1in, t2in, factor, dxmax, t1out, t2out)
  
  brent(t1out, t2out, rtol, troot, froot)
  
  ' set to final time if after tf
  
  if (troot > tf) then
    
    troot = tf
    
    sefunc(tf, froot)
    
  end if
  
  jdate = jdtdbi + troot
  
  eclprint(3, jdate)
  
  ' save current value of root
  
  trr = troot
  
END sub
  
  ''''''''''''''''''''''''
  ''''''''''''''''''''''''
  
sub eclprint(iflag, jdtdb)
  
  ' print solar eclipse conditions
  
  ''''''''''''''''''''''''''''''''
  
  local rsun(3), rmoon(3), dvec(5)
  
  LOCAL jdutc, dms$ as string
  
  local deltat, gast
  
  if (iflag = 1) then
    
    PRINT " "
    print " "
    print "begin penumbral phase of solar eclipse"
    PRINT " "
    
    jdprint = jdtdb
    
  end if
  
  if (iflag = 2) then
    
    PRINT " "
    print " "
    print "greatest eclipse conditions"
    PRINT " "
    
  end if
  
  if (iflag = 3) then
    
    PRINT " "
    print " "
    print "end penumbral phase of solar eclipse"
    PRINT " "
    
  end if
  
  ' print UTC julian date
  
  tdb2utc(jdtdb, jdutc)
  
  jd2str(jdutc)
  
  PRINT " "
  
  print "UTC julian day     ", str$(jdutc, 0, 8)
  
  ' compute and display topocentric coordinates of the moon
  
  gast2(jdutc, gast)
  
  moon(jdtdb, rmoon())
  
  eci2topo(gast, rmoon(), dvec())
  
  PRINT " "
  
  print "topocentric coordinates"
  
  PRINT " "  

  deg2str(rtd * dvec(1), dms$)
  
  print "lunar azimuth angle    ", dms$
  
  PRINT " "
  
  deg2str(rtd * dvec(2), dms$)
  
  print "lunar elevation angle  ", dms$
  
  ' compute and display topocentric coordinates of the sun
  
  sun(jdtdb, rsun())
  
  eci2topo(gast, rsun(), dvec())
  
  PRINT " "
  
  deg2str(rtd * dvec(1), dms$)
  
  print "solar azimuth angle    ", dms$
  
  PRINT " "

  deg2str(rtd * dvec(2), dms$)
  
  print "solar elevation angle  ", dms$
  
  ' determine and display event duration
  
  if (iflag = 3) then
    
    deltat = 24.0 * (jdtdb - jdprint)
    
    PRINT " "
    
    print "event duration   ", str$(deltat, 0, 8) + " hours"
    
    print " "
    
  end if
  
END sub
  
  '''''''''''''''''
  '''''''''''''''''
  
sub sun(jd, rsun())
  
  ' precision ephemeris of the Sun
  
  ' input
  
  '  jd = julian ephemeris day
  
  ' output
  
  '  rlsun = ecliptic longitude of the sun
  '          (0 <= rlsun <= 2 pi)
  '  rasc  = right ascension of the Sun (radians)
  '          (0 <= rasc <= 2 pi)
  '  decl  = declination of the Sun (radians)
  '          (-pi/2 <= decl <= pi/2)
  
  ''''''''''''''''''''''''''''''''''
  
  local u, a1, a2, psi, deps, meps, eps, seps, ceps
  
  local dl, dr, w, srl, crl, srb, crb, sra, cra
  
  u = (jd - 2451545.0) / 3652500.0
  
  ' compute nutation in longitude
  
  a1 = 2.18 + u * (-3375.7 + u * 0.36)
  
  a2 = 3.51 + u * (125666.39 + u * 0.1)
  
  psi = 0.0000001 * (-834.0 * sin(a1) - 64.0 * sin(a2))
  
  ' compute nutation in obliquity
  
  deps = 0.0000001 * u * (-226938 + u * (-75 + u * (96926 + u * (-2491 - u * 12104))))
  
  meps = 0.0000001 * (4090928.0 + 446.0 * cos(a1) + 28.0 * cos(a2))
  
  eps = meps + deps
  
  obliq = eps
  
  seps = sin(eps)
  
  ceps = cos(eps)
  
  dl = 0.0
  
  dr = 0.0
  
  for i% = 1 to 50
    
    w = xsa(i%) + xsb(i%) * u
    
    dl = dl + xsl(i%) * sin(w)
    
    if (xsr(i%) <> 0.0) then
      
      dr = dr + xsr(i%) * cos(w)
      
    end if
    
  next i%
  
  dl = modulo(dl * 0.0000001 + 4.9353929 + 62833.196168 * u)
  
  dr = 149597870.691 * (dr * 0.0000001 + 1.0001026)
  
  rlsun = modulo(dl + 0.0000001 * (-993.0 + 17.0 * cos(3.1 + 62830.14 * u)) + psi)
  
  rb = 0.0
  
  ' compute geocentric declination and right ascension
  
  crl = cos(rlsun)
  srl = sin(rlsun)
  crb = cos(rb)
  srb = sin(rb)
  
  decl = asin(ceps * srb + seps * crb * srl)
  
  sra = -seps * srb + ceps * crb * srl
  
  cra = crb * crl
  
  rasc = atan3(sra, cra)
  
  ' geocentric equatorial position vector of the Sun (kilometers)
  
  rsun(1) = dr * cos(rasc) * cos(decl)
  
  rsun(2) = dr * sin(rasc) * cos(decl)
  
  rsun(3) = dr * sin(decl)
  
end sub
  
  ''''''''''''''''''''''''''''''''
  ''''''''''''''''''''''''''''''''
  
sub eci2topo(gast, robj(), dvec())
  
  ' convert eci position vector to topocentric coordinates
  
  ' input
  
  '  gast = Greenwich apparent sidereal time (radians)
  '  robj = eci position vector of object (kilometers)
  
  ' output
  
  '  dvec(1) = topocentric azimuth (radians)
  '  dvec(2) = topocentric elevation (radians)
  '  dvec(3) = x-component of topocentric position vector
  '  dvec(4) = y-component of topocentric position vector
  '  dvec(5) = z-component of topocentric position vector
  
  '''''''''''''''''''''''''''''''''''''''''''''''''''''''
  
  local rsite(3), rhoijk(3), rhohatijk(3), rhohatsez(3)
  
  local i as integer, tmatrix(3, 3)
  
  LOCAL obslst, srange, sobslat
  
  local cobslat, sobslst, cobslst
  
  local elevation, azimuth
  
  ' observer local sidereal time
  
  obslst = modulo(gast + obslong)
  
  gsite(obslst, rsite())
  
  ' eci vector from observer to moon
  
  for i = 1 to 3
    
    rhoijk(i) = robj(i) - rsite(i)
    
  next i
  
  ' observer-to-object slant range
  
  srange = vecmag(rhoijk())
  
  ' compute topocentric unit pointing vector
  
  uvector(rhoijk(), rhohatijk())
  
  ' compute eci-to-sez transformation matrix
  
  sobslat = sin(obslat)
  cobslat = cos(obslat)
  
  sobslst = sin(obslst)
  cobslst = cos(obslst)
  
  tmatrix(1, 1) = sobslat * cobslst
  tmatrix(1, 2) = sobslat * sobslst
  tmatrix(1, 3) = -cobslat
  
  tmatrix(2, 1) = -sobslst
  tmatrix(2, 2) = cobslst
  tmatrix(2, 3) = 0.0
  
  tmatrix(3, 1) = cobslat * cobslst
  tmatrix(3, 2) = cobslat * sobslst
  tmatrix(3, 3) = sobslat
  
  ' compute sez coordinates
  
  matxvec(tmatrix(), rhohatijk(), rhohatsez())
  
  ' topocentric elevation (radians)
  
  elevation = asin(rhohatsez(3))
  
  ' topocentric azimuth (radians)
  
  azimuth = atan3(rhohatsez(2), -rhohatsez(1))
  
  ' load information array
  
  dvec(1) = azimuth
  
  dvec(2) = elevation
  
  dvec(3) = rhoijk(1)
  dvec(4) = rhoijk(2)
  dvec(5) = rhoijk(3)
  
END sub
  
  ''''''''''''''''''''''
  ''''''''''''''''''''''
  
sub moon(jdate, rmoon())
  
  ' geocentric position of the moon subroutine
  
  ' input
  
  '  jdate = tdb julian day
  
  ' output
  
  '  rmoon = eci position vector of the moon (kilometers)
  
  '''''''''''''''''''''''''''''''''''''''''''''''''''''''
  
  local t1, t2, t3
  
  local t4, dpsi, deps
  
  LOCAL ll, d, lp
  
  local l, f, t
  
  LOCAL ve, ma, ju
  
  local rm, dv, pl
  
  local plat, plon
  
  LOCAL a, b
  
  local rasc, decl, obliq
  
  ' get nutations and obliquity
  
  obliq_lp(jdate, dpsi, deps, obliq)
  
  ' evaluate lunar ephemeris
  
  t1 = (jdate - 2451545.0) / 36525.0
  
  t2 = t1 * t1
  t3 = t1 * t1 * t1
  t4 = t1 * t1 * t1 * t1
  
  ' calculate fundamental arguments (radians)
  
  ll = dtr * (218 + (18 * 60 + 59.95571) / 3600)
  
  ll = modulo(ll + atr * (1732564372.83264 * t1 - 4.7763 * t2 + .006681 * t3 - 0.00005522 * t4))
  
  d = dtr * (297 + (51 * 60 + .73512) / 3600)
  
  d = modulo(d + atr * (1602961601.4603 * t1 - 5.8681 * t2 + .006595 * t3 - 0.00003184 * t4))
  
  lp = dtr * (357 + (31 * 60 + 44.79306) / 3600)
  
  lp = modulo(lp + atr * (129596581.0474 * t1 - .5529 * t2 + 0.000147 * t3))
  
  l = dtr * (134 + (57 * 60 + 48.28096) / 3600)
  
  l = modulo(l + atr * (1717915923.4728 * t1 + 32.3893 * t2 + .051651 * t3 - 0.0002447 * t4))
  
  f = dtr * (93 + (16 * 60 + 19.55755) / 3600)
  
  f = modulo(f + atr * (1739527263.0983 * t1 - 12.2505 * t2 - .001021 * t3 + 0.00000417 * t4))
  
  t = dtr * (100 + (27 * 60 + 59.22059) / 3600)
  
  t = modulo(t + atr * (129597742.2758 * t1 - .0202 * t2 + .000009 * t3 + 0.00000015 * t4))
  
  ve = dtr * (181 + (58 * 60 + 47.28305) / 3600)
  
  ve = modulo(ve + atr * 210664136.43355 * t1)
  
  ma = dtr * (355 + (25 * 60 + 59.78866) / 3600)
  
  ma = modulo(ma + atr * 68905077.59284 * t1)
  
  ju = dtr * (34 + (21 * 60 + 5.34212) / 3600)
  
  ju = modulo(ju + atr * 10925660.42861 * t1)
  
  ' compute geocentric distance (kilometers)
  
  ' a(c,0,r) series
  
  rm = 385000.52899
  rm = rm - 20905.35504 * sin(l + pidiv2)
  rm = rm - 3699.11092 * sin(2 * d - l + pidiv2)
  rm = rm - 2955.96756 * sin(2 * d + pidiv2)
  rm = rm - 569.92512 * sin(2 * l + pidiv2)
  rm = rm + 246.15848 * sin(2 * d - 2 * l + pidiv2)
  rm = rm - 204.58598 * sin(2 * d - lp + pidiv2)
  rm = rm - 170.73308 * sin(2 * d + l + pidiv2)
  rm = rm - 152.13771 * sin(2 * d - lp - l + pidiv2)
  rm = rm - 129.62014 * sin(lp - l + pidiv2)
  rm = rm + 108.7427 * sin(d + pidiv2)
  rm = rm + 104.75523 * sin(lp + l + pidiv2)
  rm = rm + 79.66056 * sin(l - 2 * f + pidiv2)
  rm = rm + 48.8883 * sin(lp + pidiv2)
  rm = rm - 34.78252 * sin(4 * d - l + pidiv2)
  rm = rm + 30.82384 * sin(2 * d + lp + pidiv2)
  rm = rm + 24.20848 * sin(2 * d + lp - l + pidiv2)
  rm = rm - 23.21043 * sin(3 * l + pidiv2)
  rm = rm - 21.63634 * sin(4 * d - 2 * l + pidiv2)
  rm = rm - 16.67471 * sin(d + lp + pidiv2)
  rm = rm + 14.40269 * sin(2 * d - 3 * l + pidiv2)
  rm = rm - 12.8314 * sin(2 * d - lp + l + pidiv2)
  rm = rm - 11.64995 * sin(4 * d + pidiv2)
  rm = rm - 10.44476 * sin(2 * d + 2 * l + pidiv2)
  rm = rm + 10.32111 * sin(2 * d - 2 * f + pidiv2)
  rm = rm + 10.0562 * sin(2 * d - lp - 2 * l + pidiv2)
  rm = rm - 9.88445 * sin(2 * d - 2 * lp + pidiv2)
  rm = rm + 8.75156 * sin(2 * d - l - 2 * f + pidiv2)
  rm = rm - 8.37911 * sin(d - l + pidiv2)
  rm = rm - 7.00269 * sin(lp - 2 * l + pidiv2)
  rm = rm + 6.322 * sin(d + l + pidiv2)
  rm = rm + 5.75085 * sin(lp + 2 * l + pidiv2)
  rm = rm - 4.95013 * sin(2 * d - 2 * lp - l + pidiv2)
  rm = rm - 4.42118 * sin(2 * l - 2 * f + pidiv2)
  rm = rm + 4.13111 * sin(2 * d + l - 2 * f + pidiv2)
  rm = rm - 3.95798 * sin(4 * d - lp - l + pidiv2)
  rm = rm + 3.25824 * sin(3 * d - l + pidiv2)
  rm = rm - 3.1483 * sin(2 * f + pidiv2)
  rm = rm + 2.61641 * sin(2 * d + lp + l + pidiv2)
  rm = rm + 2.35363 * sin(2 * d + 2 * lp - l + pidiv2)
  rm = rm - 2.11713 * sin(2 * lp - l + pidiv2)
  rm = rm - 1.89704 * sin(4 * d - lp - 2 * l + pidiv2)
  rm = rm - 1.73853 * sin(d - 2 * l + pidiv2)
  rm = rm - 1.57139 * sin(4 * d - lp + pidiv2)
  rm = rm - 1.42255 * sin(4 * d + l + pidiv2)
  rm = rm - 1.41893 * sin(3 * d + pidiv2)
  rm = rm + 1.16553 * sin(2 * lp + l + pidiv2)
  rm = rm - 1.11694 * sin(4 * l + pidiv2)
  rm = rm + 1.06567 * sin(2 * lp + pidiv2)
  rm = rm - .93332 * sin(d + lp + l + pidiv2)
  rm = rm + .86243 * sin(3 * d - 2 * l + pidiv2)
  rm = rm + .85124 * sin(d + lp - l + pidiv2)
  rm = rm - .8488 * sin(2 * d - lp + 2 * l + pidiv2)
  rm = rm - .79563 * sin(d - 2 * f + pidiv2)
  rm = rm + .77854 * sin(2 * d - 4 * l + pidiv2)
  rm = rm + .77404 * sin(2 * d - 2 * l + 2 * f + pidiv2)
  rm = rm - .66968 * sin(2 * d + 3 * l + pidiv2)
  rm = rm - .65753 * sin(2 * d - 2 * lp + l + pidiv2)
  rm = rm + .65706 * sin(2 * d - lp - 2 * f + pidiv2)
  rm = rm + .59632 * sin(2 * d - l + 2 * f + pidiv2)
  rm = rm + .57879 * sin(4 * d + lp - l + pidiv2)
  rm = rm - .51423 * sin(4 * d - 3 * l + pidiv2)
  rm = rm - .50792 * sin(4 * d - 2 * f + pidiv2)
  rm = rm + .49755 * sin(d - lp + pidiv2)
  rm = rm + .49504 * sin(2 * d - lp - 3 * l + pidiv2)
  rm = rm + .47262 * sin(2 * d - 2 * l - 2 * f + pidiv2)
  rm = rm - .4225 * sin(6 * d - 2 * l + pidiv2)
  rm = rm - .42241 * sin(lp - 3 * l + pidiv2)
  rm = rm - .41071 * sin(2 * d - 3 * lp + pidiv2)
  rm = rm + .37852 * sin(d + 2 * l + pidiv2)
  rm = rm + .35508 * sin(lp + 3 * l + pidiv2)
  rm = rm + .34302 * sin(2 * d - 2 * lp - 2 * l + pidiv2)
  rm = rm + .33463 * sin(lp - l + 2 * f + pidiv2)
  rm = rm + .33225 * sin(d + lp - 2 * l + pidiv2)
  rm = rm + .32334 * sin(2 * d - lp - l - 2 * f + pidiv2)
  rm = rm - .32176 * sin(4 * d - l - 2 * f + pidiv2)
  rm = rm - .28663 * sin(6 * d - l + pidiv2)
  rm = rm + .28399 * sin(2 * d + 2 * l - 2 * f + pidiv2)
  rm = rm - .27904 * sin(4 * d - 2 * lp - l + pidiv2)
  rm = rm + .2556 * sin(3 * d - lp - l + pidiv2)
  rm = rm - .2481 * sin(lp + l - 2 * f + pidiv2)
  rm = rm + .24452 * sin(4 * d + lp + pidiv2)
  rm = rm + .23695 * sin(4 * d + lp - 2 * l + pidiv2)
  rm = rm - .21258 * sin(3 * d + lp - l + pidiv2)
  rm = rm + .21251 * sin(2 * d + lp + 2 * l + pidiv2)
  rm = rm + .20941 * sin(2 * d - lp + l - 2 * f + pidiv2)
  rm = rm - .20285 * sin(4 * d - lp + l + pidiv2)
  rm = rm + .20099 * sin(3 * d - 2 * f + pidiv2)
  rm = rm - .18567 * sin(lp - 2 * f + pidiv2)
  rm = rm - .18316 * sin(6 * d - 3 * l + pidiv2)
  rm = rm + .16857 * sin(2 * d + lp - 3 * l + pidiv2)
  rm = rm - .15802 * sin(lp + 2 * f + pidiv2)
  rm = rm - .15707 * sin(3 * d - lp + pidiv2)
  rm = rm - .14806 * sin(2 * d - 3 * lp - l + pidiv2)
  rm = rm + .14763 * sin(2 * d + 2 * lp + pidiv2)
  rm = rm + .14368 * sin(2 * d + lp - 2 * l + pidiv2)
  rm = rm - .13922 * sin(4 * d + 2 * l + pidiv2)
  rm = rm - .13617 * sin(2 * lp - 2 * l + pidiv2)
  rm = rm - .13571 * sin(2 * d + lp - 2 * f + pidiv2)
  rm = rm - .12805 * sin(4 * d - 2 * lp + pidiv2)
  rm = rm + .11411 * sin(d - lp - l + pidiv2)
  rm = rm + .10998 * sin(d - lp + l + pidiv2)
  rm = rm - .10887 * sin(2 * d + 2 * lp - 2 * l + pidiv2)
  rm = rm - .10833 * sin(4 * d - 2 * lp - 2 * l + pidiv2)
  rm = rm - .10766 * sin(3 * d + lp + pidiv2)
  rm = rm - .10326 * sin(l + 2 * f + pidiv2)
  rm = rm - .09938 * sin(d - 3 * l + pidiv2)
  rm = rm - .08587 * sin(6 * d + pidiv2)
  rm = rm - .07982 * sin(4 * d - 2 * l - 2 * f + pidiv2)
  rm = rm - 6.678e-02 * sin(6 * d - lp - 2 * l + pidiv2)
  rm = rm - 6.545e-02 * sin(3 * d + l + pidiv2)
  rm = rm + .06055 * sin(d + l - 2 * f + pidiv2)
  rm = rm - .05904 * sin(d + lp + 2 * l + pidiv2)
  rm = rm - .05888 * sin(5 * l + pidiv2)
  rm = rm - .0585 * sin(2 * d - lp + 3 * l + pidiv2)
  rm = rm - .05789 * sin(4 * d - lp - 2 * f + pidiv2)
  rm = rm - .05527 * sin(2 * d + lp + l - 2 * f + pidiv2)
  rm = rm + .05293 * sin(3 * d - lp - 2 * l + pidiv2)
  rm = rm - .05191 * sin(6 * d - lp - l + pidiv2)
  rm = rm + .05072 * sin(2 * lp + 2 * l + pidiv2)
  rm = rm - .0502 * sin(lp - 2 * l + 2 * f + pidiv2)
  rm = rm - .04843 * sin(3 * d - 3 * l + pidiv2)
  rm = rm + .0474 * sin(2 * d - 5 * l + pidiv2)
  rm = rm - .04736 * sin(2 * d + lp - l - 2 * f + pidiv2)
  rm = rm - .04608 * sin(2 * d - 2 * lp + 2 * l + pidiv2)
  rm = rm + .04591 * sin(5 * d - 2 * l + pidiv2)
  rm = rm - .04422 * sin(2 * d + 4 * l + pidiv2)
  rm = rm - .04316 * sin(4 * d - lp - 3 * l + pidiv2)
  rm = rm - .04232 * sin(d - l - 2 * f + pidiv2)
  rm = rm - .03894 * sin(3 * lp - l + pidiv2)
  rm = rm + .0381 * sin(3 * d + lp - 2 * l + pidiv2)
  rm = rm + .03734 * sin(2 * d - lp - l + 2 * f + pidiv2)
  rm = rm + .03729 * sin(d + 2 * lp + pidiv2)
  rm = rm + .03682 * sin(4 * d + lp + l + pidiv2)
  rm = rm + .03379 * sin(d + lp - 2 * f + pidiv2)
  rm = rm + .03265 * sin(lp + 2 * l - 2 * f + pidiv2)
  rm = rm + .03143 * sin(2 * d + 2 * f + pidiv2)
  rm = rm + .03024 * sin(2 * d - lp - 2 * l + 2 * f + pidiv2)
  rm = rm - .02948 * sin(d - 2 * lp + pidiv2)
  rm = rm - .02939 * sin(4 * d - 4 * l + pidiv2)
  rm = rm + .0291 * sin(2 * d - 3 * l - 2 * f + pidiv2)
  rm = rm - .02855 * sin(2 * d - 3 * lp + l + pidiv2)
  rm = rm + .02839 * sin(2 * d - 2 * lp - 2 * f + pidiv2)
  rm = rm - .02698 * sin(4 * d - lp - l - 2 * f + pidiv2)
  rm = rm - .02674 * sin(lp - 4 * l + pidiv2)
  rm = rm + .02658 * sin(4 * d + 2 * lp - 2 * l + pidiv2)
  rm = rm - .02471 * sin(d - l + 2 * f + pidiv2)
  rm = rm - .02436 * sin(6 * d - lp - 3 * l + pidiv2)
  rm = rm - .02399 * sin(4 * d + lp - 3 * l + pidiv2)
  rm = rm + .02368 * sin(d + 3 * l + pidiv2)
  rm = rm + .02334 * sin(2 * d - lp - 4 * l + pidiv2)
  rm = rm + .02304 * sin(lp + 4 * l + pidiv2)
  rm = rm + .02127 * sin(3 * lp + pidiv2)
  rm = rm - .02079 * sin(4 * d - lp + 2 * l + pidiv2)
  rm = rm - .02008 * sin(2 * d - 3 * l + 2 * f + pidiv2)
  
  ' a(p,0,r) series
  
  rm = rm + 1.0587 * sin(2 * t - 2 * ju + 2 * d - l + 90.11969000000001 * dtr)
  rm = rm + .72783 * sin(18 * ve - 16 * t - 2 * l + 116.54311 * dtr)
  rm = rm + .68256 * sin(18 * ve - 16 * t + 296.54574 * dtr)
  rm = rm + .59827 * sin(3 * ve - 3 * t + 2 * d - l + 89.98187 * dtr)
  rm = rm + .45648 * sin(ll + l - f + 270.00126 * dtr)
  rm = rm + .45276 * sin(ll - l - f + 90.00128 * dtr)
  rm = rm + .41011 * sin(2 * t - 3 * ju + 2 * d - l + 280.06924 * dtr)
  rm = rm + .20497 * sin(t - ju - 2 * d + 91.79862 * dtr)
  rm = rm + .20473 * sin(18 * ve - 16 * t - 2 * d - l + 116.54222 * dtr)
  rm = rm + .20367 * sin(18 * ve - 16 * t + 2 * d - l + 296.54299 * dtr)
  rm = rm + .16644 * sin(2 * ve - 2 * t - 2 * d + 90.36386 * dtr)
  rm = rm + .1578 * sin(4 * t - 8 * ma + 3 * ju + l + 194.98833 * dtr)
  rm = rm + .1578 * sin(4 * t - 8 * ma + 3 * ju - l + 14.98841 * dtr)
  rm = rm + .15751 * sin(t - ju - 2 * d + l + 91.74578 * dtr)
  rm = rm + .1445 * sin(2 * t - 2 * ju - l + 89.97863 * dtr)
  rm = rm + .13811 * sin(ve - t - l + 270.00993 * dtr)
  rm = rm + .13477 * sin(18 * ve - 16 * t - 2 * d + 116.53978 * dtr)
  rm = rm + .12671 * sin(18 * ve - 16 * t + 2 * d - 2 * l + 296.54238 * dtr)
  rm = rm + .12666 * sin(t - ju - l + 91.22751 * dtr)
  rm = rm + .12362 * sin(ve - t - 2 * d + 269.98523 * dtr)
  rm = rm + .12047 * sin(2 * ve - 2 * t + 2 * d - l + 269.99692 * dtr)
  rm = rm + .11998 * sin(ve - t + l + 90.01606 * dtr)
  rm = rm + .11617 * sin(2 * ve - 2 * t - 2 * d + l + 90.31081 * dtr)
  rm = rm + .11256 * sin(4 * t - 8 * ma + 3 * ju + 2 * d - l + 197.11421 * dtr)
  rm = rm + .11251 * sin(4 * t - 8 * ma + 3 * ju - 2 * d + l + 17.11263 * dtr)
  rm = rm + .11226 * sin(4 * t - 8 * ma + 3 * ju + 2 * d + 196.69224 * dtr)
  rm = rm + .11216 * sin(4 * t - 8 * ma + 3 * ju - 2 * d + 16.68897 * dtr)
  rm = rm + .10689 * sin(ll + 2 * d - f + 270.00092 * dtr)
  rm = rm + .10504 * sin(t - ju + l + 271.06726 * dtr)
  rm = rm + .1006 * sin(ve - t - 2 * d + l + 269.98452 * dtr)
  rm = rm + 9.932e-02 * sin(3 * ve - 3 * t - l + 90.1054 * dtr)
  rm = rm + .09554 * sin(ll - 2 * d - f + 90.00096 * dtr)
  rm = rm + .08508 * sin(ll - f + 270.00061 * dtr)
  rm = rm + 7.9450e-02 * sin(4 * ve - 4 * t + 2 * d - l + 89.99224 * dtr)
  rm = rm + .07725 * sin(2 * t - 3 * ju - l + 280.16516 * dtr)
  rm = rm + 7.054e-02 * sin(6 * ve - 8 * t + 2 * d - l + 77.22087 * dtr)
  rm = rm + 6.313e-02 * sin(2 * ju - 5 * l + l + 256.56163 * dtr)
  
  ' a(p,1,r) series
  
  rm = rm + .51395 * t1 * sin(2 * d - lp + pidiv2)
  rm = rm + .38245 * t1 * sin(2 * d - lp - l + pidiv2)
  rm = rm + .32654 * t1 * sin(lp - l + pidiv2)
  rm = rm + .26396 * t1 * sin(lp + l + 270 * dtr)
  rm = rm + .12302 * t1 * sin(lp + 270 * dtr)
  rm = rm + .07754 * t1 * sin(2 * d + lp + 270 * dtr)
  rm = rm + .06068 * t1 * sin(2 * d + lp - l + 270 * dtr)
  rm = rm + .0497 * t1 * sin(2 * d - 2 * lp + pidiv2)
  rm = rm + .04194 * t1 * sin(d + lp + pidiv2)
  rm = rm + .03222 * t1 * sin(2 * d - lp + l + pidiv2)
  rm = rm + .02529 * t1 * sin(2 * d - lp - 2 * l + 270 * dtr)
  rm = rm + .0249 * t1 * sin(2 * d - 2 * lp - l + pidiv2)
  rm = rm + .00149 * t2 * sin(2 * d - lp + pidiv2)
  rm = rm + .00111 * t2 * sin(2 * d - lp - l + pidiv2)
  
  rmm = rm
  
  ' compute geocentric ecliptic longitude (arc seconds)
  
  ' a(c,0,v) series
  
  dv = 22639.58578 * sin(l)
  dv = dv + 4586.4383 * sin(2 * d - l)
  dv = dv + 2369.91394 * sin(2 * d)
  dv = dv + 769.02571 * sin(2 * l)
  dv = dv - 666.4171 * sin(lp)
  dv = dv - 411.59567 * sin(2 * f)
  dv = dv + 211.65555 * sin(2 * d - 2 * l)
  dv = dv + 205.43582 * sin(2 * d - lp - l)
  dv = dv + 191.9562 * sin(2 * d + l)
  dv = dv + 164.72851 * sin(2 * d - lp)
  dv = dv - 147.32129 * sin(lp - l)
  dv = dv - 124.98812 * sin(d)
  dv = dv - 109.38029 * sin(lp + l)
  dv = dv + 55.17705 * sin(2 * d - 2 * f)
  dv = dv - 45.0996 * sin(l + 2 * f)
  dv = dv + 39.53329 * sin(l - 2 * f)
  dv = dv + 38.42983 * sin(4 * d - l)
  dv = dv + 36.12381 * sin(3 * l)
  dv = dv + 30.77257 * sin(4 * d - 2 * l)
  dv = dv - 28.39708 * sin(2 * d + lp - l)
  dv = dv - 24.35821 * sin(2 * d + lp)
  dv = dv - 18.58471 * sin(d - l)
  dv = dv + 17.95446 * sin(d + lp)
  dv = dv + 14.53027 * sin(2 * d - lp + l)
  dv = dv + 14.3797 * sin(2 * d + 2 * l)
  dv = dv + 13.89906 * sin(4 * d)
  dv = dv + 13.19406 * sin(2 * d - 3 * l)
  dv = dv - 9.67905 * sin(lp - 2 * l)
  dv = dv - 9.36586 * sin(2 * d - l + 2 * f)
  dv = dv + 8.60553 * sin(2 * d - lp - 2 * l)
  dv = dv - 8.45310 * sin(d + l)
  dv = dv + 8.05016 * sin(2 * d - 2 * lp)
  dv = dv - 7.63015 * sin(lp + 2 * l)
  dv = dv - 7.44749 * sin(2 * lp)
  dv = dv + 7.37119 * sin(2 * d - 2 * lp - l)
  dv = dv - 6.38315 * sin(2 * d + l - 2 * f)
  dv = dv - 5.74161 * sin(2 * d + 2 * f)
  dv = dv + 4.37401 * sin(4 * d - lp - l)
  dv = dv - 3.99761 * sin(2 * l + 2 * f)
  dv = dv - 3.20969 * sin(3 * d - l)
  dv = dv - 2.91454 * sin(2 * d + lp + l)
  dv = dv + 2.73189 * sin(4 * d - lp - 2 * l)
  dv = dv - 2.56794 * sin(2 * lp - l)
  dv = dv - 2.5212 * sin(2 * d + 2 * lp - l)
  dv = dv + 2.48889 * sin(2 * d + lp - 2 * l)
  dv = dv + 2.14607 * sin(2 * d - lp - 2 * f)
  dv = dv + 1.97773 * sin(4 * d + l)
  dv = dv + 1.93368 * sin(4 * l)
  dv = dv + 1.87076 * sin(4 * d - lp)
  dv = dv - 1.75297 * sin(d - 2 * l)
  dv = dv - 1.43716 * sin(2 * d + lp - 2 * f)
  dv = dv - 1.37257 * sin(2 * l - 2 * f)
  dv = dv + 1.26182 * sin(d + lp + l)
  dv = dv - 1.22412 * sin(3 * d - 2 * l)
  dv = dv + 1.18683 * sin(4 * d - 3 * l)
  dv = dv + 1.177 * sin(2 * d - lp + 2 * l)
  dv = dv - 1.16169 * sin(2 * lp + l)
  dv = dv + 1.07769 * sin(d + lp - l)
  dv = dv + 1.0595 * sin(2 * d + 3 * l)
  dv = dv - .99022 * sin(2 * d + l + 2 * f)
  dv = dv + .94828 * sin(2 * d - 4 * l)
  dv = dv + .75168 * sin(2 * d - 2 * lp + l)
  dv = dv - .66938 * sin(lp - 3 * l)
  dv = dv - .63521 * sin(4 * d + lp - l)
  dv = dv - .58399 * sin(d + 2 * l)
  dv = dv - .58331 * sin(d - 2 * f)
  dv = dv + .57156 * sin(6 * d - 2 * l)
  dv = dv - .56064 * sin(2 * d - 2 * l - 2 * f)
  dv = dv - .55692 * sin(d - lp)
  dv = dv - .54592 * sin(lp + 3 * l)
  dv = dv - .53571 * sin(2 * d - 2 * l + 2 * f)
  dv = dv + .4784 * sin(2 * d - lp - 3 * f)
  dv = dv - .45379 * sin(2 * d + 2 * l - 2 * f)
  dv = dv - .42622 * sin(2 * d - lp - l + 2 * f)
  dv = dv + .42033 * sin(4 * f)
  dv = dv + .4134 * sin(lp + 2 * f)
  dv = dv + .40423 * sin(3 * d)
  dv = dv + .39451 * sin(6 * d - l)
  dv = dv - .38213 * sin(2 * d - lp + 2 * f)
  dv = dv - .37451 * sin(2 * d - lp + l - 2 * f)
  dv = dv - .35758 * sin(4 * d + lp - 2 * l)
  dv = dv + .34965 * sin(d + lp - 2 * l)
  dv = dv + .33979 * sin(2 * d - 3 * lp)
  dv = dv - .32866 * sin(3 * l + 2 * f)
  dv = dv + .30872 * sin(4 * d - 2 * lp - l)
  dv = dv + .30155 * sin(lp - l - 2 * f)
  dv = dv + .30086 * sin(4 * d - l - 2 * f)
  dv = dv + .2942 * sin(2 * d - 2 * lp - 2 * l)
  dv = dv + .29255 * sin(6 * d - 3 * l)
  dv = dv - .29022 * sin(2 * d + lp + 2 * l)
  
  ' a(p,2,v) series
  
  dv = dv + .00487 * t2 * sin(lp)
  dv = dv - .0015 * t2 * sin(2 * d - lp - l + pi)
  dv = dv - .0012 * t2 * sin(2 * d - lp + pi)
  dv = dv + .00108 * t2 * sin(lp - l)
  dv = dv + .0008 * t2 * sin(lp + l)
  
  ' a(p,0,v) series
  
  dv = dv + 14.24883 * sin(18 * ve - 16 * t - l + dtr * 26.54261)
  dv = dv + 7.06304 * sin(ll - f + dtr * .00094)
  dv = dv + 1.14307 * sin(2 * t - 2 * ju + 2 * d - l + dtr * 180.11977)
  dv = dv + .901140 * sin(4 * t - 8 * ma + 3 * ju + dtr * 285.98707)
  dv = dv + .82155 * sin(ve - t + dtr * 180.00988)
  dv = dv + .78811 * sin(18 * ve - 16 * t - 2 * l + dtr * 26.54324)
  dv = dv + .7393 * sin(18 * ve - 16 * t + dtr * 26.5456)
  dv = dv + .64371 * sin(3 * ve - 3 * t + 2 * d - l + dtr * 179.98144)
  dv = dv + .6388 * sin(t - ju + dtr * 1.2289)
  dv = dv + .56341 * sin(10 * ve - 3 * t - l + dtr * 333.30551)
  dv = dv + .49331 * sin(ll + l - f + .00127 * dtr)
  dv = dv + .49141 * sin(ll - l - f + .00127 * dtr)
  dv = dv + .44532 * sin(2 * t - 3 * ju + 2 * d - l + 10.07001 * dtr)
  dv = dv + .36061 * sin(ll + f + .00071 * dtr)
  dv = dv + .34355 * sin(2 * ve - 3 * t + 269.95393 * dtr)
  dv = dv + .32455 * sin(t - 2 * ma + 318.13776 * dtr)
  dv = dv + .30155 * sin(2 * ve - 2 * t + .20448 * dtr)
  dv = dv + .28938 * sin(t + d - f + 95.13523 * dtr)
  dv = dv + .28281 * sin(2 * t - 3 * ju + 2 * d - 2 * l + 10.03835 * dtr)
  dv = dv + .24515 * sin(2 * t - 2 * ju + 2 * d - 2 * l + .08642 * dtr)
  
  ' a(p,1,v) series
  
  dv = dv + 1.6768 * t1 * sin(lp)
  dv = dv + .51642 * t1 * sin(2 * d - lp - l + pi)
  dv = dv + .41383 * t1 * sin(2 * d - lp + pi)
  dv = dv + .37115 * t1 * sin(lp - l)
  dv = dv + .2756 * t1 * sin(lp + l)
  dv = dv + .25425 * t1 * sin(18 * ve - 16 * t - l + 114.5655 * dtr)
  dv = dv + 7.1178e-02 * t1 * sin(2 * d + lp - l)
  dv = dv + .06128 * t1 * sin(2 * d + lp)
  dv = dv + .04516 * t1 * sin(d + lp + pi)
  dv = dv + .04048 * t1 * sin(2 * d - 2 * lp + pi)
  dv = dv + .03747 * t1 * sin(2 * lp)
  dv = dv + .03707 * t1 * sin(2 * d - 2 * lp - l + pi)
  dv = dv + .03649 * t1 * sin(2 * d - lp + l + pi)
  dv = dv + .02438 * t1 * sin(lp - 2 * l)
  dv = dv + .02165 * t1 * sin(2 * d - lp - 2 * l + pi)
  dv = dv + .01923 * t1 * sin(lp + 2 * l)
  
  plon = modulo(ll + atr * dv + dpsi)
  
  ' compute geocentric ecliptic latitude (arc seconds)
  
  ' a(c,0,u) series
  
  pl = 18461.23868 * sin(f)
  pl = pl + 1010.16707 * sin(l + f)
  pl = pl + 999.69358 * sin(l - f)
  pl = pl + 623.65243 * sin(2 * d - f)
  pl = pl + 199.48374 * sin(2 * d - l + f)
  pl = pl + 166.5741 * sin(2 * d - l - f)
  pl = pl + 117.26069 * sin(2 * d + f)
  pl = pl + 61.91195 * sin(2 * l + f)
  pl = pl + 33.3572 * sin(2 * d + l - f)
  pl = pl + 31.75967 * sin(2 * l - f)
  pl = pl + 29.57658 * sin(2 * d - lp - f)
  pl = pl + 15.56626 * sin(2 * d - 2 * l - f)
  pl = pl + 15.12155 * sin(2 * d + l + f)
  pl = pl - 12.09414 * sin(2 * d + lp - f)
  pl = pl + 8.86814 * sin(2 * d - lp - l + f)
  pl = pl + 7.95855 * sin(2 * d - lp + f)
  pl = pl + 7.43455 * sin(2 * d - lp - l - f)
  pl = pl - 6.73143 * sin(lp - l - f)
  pl = pl + 6.57957 * sin(4 * d - l - f)
  pl = pl - 6.46007 * sin(lp + f)
  pl = pl - 6.29648 * sin(3 * f)
  pl = pl - 5.63235 * sin(lp - l + f)
  pl = pl - 5.3684 * sin(d + f)
  pl = pl - 5.31127 * sin(lp + l + f)
  pl = pl - 5.07591 * sin(lp + l - f)
  pl = pl - 4.83961 * sin(lp - f)
  pl = pl - 4.80574 * sin(d - f)
  pl = pl + 3.98405 * sin(3 * l + f)
  pl = pl + 3.67446 + sin(4 * d - f)
  pl = pl + 2.99848 * sin(4 * d - l + f)
  pl = pl + 2.79864 * sin(l - 3 * f)
  pl = pl + 2.41388 * sin(4 * d - 2 * l + f)
  pl = pl + 2.18631 * sin(2 * d - 3 * f)
  pl = pl + 2.14617 * sin(2 * d + 2 * l - f)
  pl = pl + 1.76598 * sin(2 * d - lp + l - f)
  pl = pl - 1.62442 * sin(2 * d - 2 * l + f)
  pl = pl + 1.5813 * sin(3 * l - f)
  pl = pl + 1.51975 * sin(2 * d + 2 * l + f)
  pl = pl - 1.51563 * sin(2 * d - 3 * l - f)
  pl = pl - 1.31782 * sin(2 * d + lp - l + f)
  pl = pl - 1.26427 * sin(2 * d + lp + f)
  pl = pl + 1.19187 * sin(4 * d + f)
  pl = pl + 1.13461 * sin(2 * d - lp + l + f)
  pl = pl + 1.08578 * sin(2 * d - 2 * lp - f)
  pl = pl - 1.01938 * sin(l + 3 * f)
  pl = pl - .822710 * sin(2 * d + lp + l - f)
  pl = pl + .80422 * sin(d + lp - f)
  pl = pl + .80259 * sin(d + lp + f)
  pl = pl - .79319 * sin(lp - 2 * l - f)
  pl = pl - .79101 * sin(2 * d + lp - l - f)
  pl = pl - .66741 * sin(d + l + f)
  pl = pl + .65022 * sin(2 * d - lp - 2 * l - f)
  pl = pl - .63881 * sin(lp + 2 * l + f)
  pl = pl + .63371 * sin(4 * d - 2 * l - f)
  pl = pl + .59577 * sin(4 * d - lp - l - f)
  pl = pl - .58893 * sin(d + l - f)
  pl = pl + .47338 * sin(4 * d + l - f)
  pl = pl - .42989 * sin(d - l - f)
  pl = pl + .41494 * sin(4 * d - lp - f)
  pl = pl + .3835 * sin(2 * d - 2 * lp + f)
  pl = pl - .35183 * sin(3 * d - f)
  pl = pl + .33881 * sin(4 * d - lp - l + f)
  pl = pl + .32906 * sin(2 * d - l - 3 * f)
  
  ' a(p,0,u) series
  
  pl = pl + 8.04508 * sin(ll + 180.00071 * dtr)
  pl = pl + 1.51021 * sin(t + d + 276.68007 * dtr)
  pl = pl + .63037 * sin(18 * ve - 16 * t - l + f + 26.54287 * dtr)
  pl = pl + .63014 * sin(18 * ve - 16 * t - l - f + 26.54272 * dtr)
  pl = pl + .45586 * sin(ll - l + .00075 * dtr)
  pl = pl + .41571 * sin(ll + l + 180.00069 * dtr)
  pl = pl + .32622 * sin(ll - 2 * f + .00086 * dtr)
  pl = pl + .29854 * sin(ll - 2 * d + .00072 * dtr)
  
  ' a(p,1,u) series
  
  pl = pl + .0743 * t1 * sin(2 * d - lp - f + pi)
  pl = pl + .03043 * t1 * sin(2 * d + lp - f)
  pl = pl + .02229 * t1 * sin(2 * d - lp - l + f + pi)
  pl = pl + .01999 * t1 * sin(2 * d - lp + f + pi)
  pl = pl + .01869 * t1 * sin(2 * d - lp - l - f + pi)
  pl = pl + .01696 * t1 * sin(lp - l - f)
  pl = pl + .01623 * t1 * sin(lp + f)
  
  plat = atr * pl
  
  ' compute geocentric right ascension and declination
  
  a = sin(plon) * cos(obliq) - tan(plat) * sin(obliq)
  
  b = cos(plon)
  
  rasc = atan3(a, b)
  
  decl = asin(sin(plat) * cos(obliq) + cos(plat) * sin(obliq) * sin(plon))
  
  ' compute the geocentric position vector of the moon (kilometers)
  
  rmoon(1) = rmm * cos(rasc) * cos(decl)
  
  rmoon(2) = rmm * sin(rasc) * cos(decl)
  
  rmoon(3) = rmm * sin(decl)
  
end sub
  
  ''''''''''''''''''''''''''''''''''''
  ''''''''''''''''''''''''''''''''''''
  
sub obliq_lp(jdate, dpsi, deps, obliq)
  
  ' nutations and true obliquity
  
  ' input
  
  '  jdate = julian date
  
  ' output
  
  '  dpsi = nutation in longitude in radians
  
  '  deps = nutation in obliquity in radians
  
  '  obliq = true obliquity of the ecliptic in radians
  
  ''''''''''''''''''''''''''''''''''''''''''''''''''''
  
  LOCAL t, t2, t3, eqeq
  
  LOCAL th, tl, obm, obt, st
  
  LOCAL tjdh, tjdl
  
  ' split the julian date
  
  tjdh = int(jdate)
  
  tjdl = jdate - tjdh
  
  ' fundamental time units
  
  th = (tjdh - 2451545.0) / 36525.0
  
  tl = tjdl / 36525.0
  
  t = th + tl
  
  t2 = t * t
  
  t3 = t2 * t
  
  ' obtain equation of the equinoxes
  
  eqeq = 0.0
  
  ' obtain nutations
  
  nut2000_lp(jdate, dpsi, deps)
  
  ' compute mean obliquity of the ecliptic in seconds of arc
  
  obm = 84381.4480 - 46.8150 * t - 0.00059 * t2 + 0.001813 * t3
  
  ' compute true obliquity of the ecliptic in seconds of arc
  
  obt = obm + deps
  
  ' return elements in radians
  
  deps = atr * deps
  
  dpsi = atr * dpsi
  
  obliq = atr * obt
  
END sub
  
  '''''''''''''''''''''''''''''''
  '''''''''''''''''''''''''''''''
  
sub nut2000_lp(jdate, dpsi, deps)
  
  ' low precison nutation based on iau 2000a
  
  ' this function evaluates a short nutation series and returns approximate
  ' values for nutation in longitude and nutation in obliquity for a given
  ' tdb julian date. in this mode, only the largest 13 terms of the iau 2000a
  ' nutation series are evaluated.
  
  ' input
  
  '  jdate = tdb julian date
  
  ' output
  
  '  dpsi = nutation in longitude in arcseconds
  
  '  deps = nutation in obliquity in arcseconds
  
  '''''''''''''''''''''''''''''''''''''''''''''
  
  LOCAL rev = 360.0 * 3600.0
  
  LOCAL el, elp, f, d, omega
  
  LOCAL i%, arg
  
  LOCAL t = (jdate - 2451545.0) / 36525.0
  
  ''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
  ' computation of fundamental (delaunay) arguments from simon et al. (1994)
  ''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
  
  ' mean anomaly of the moon
  
  el = (485868.249036 + t * (1717915923.2178 + t * (31.8792 + t * (0.051635 + t * (-0.00024470)))) mod rev) / seccon
  
  ' mean anomaly of the sun
  
  elp = (1287104.79305 + t * (129596581.0481 + t * (-0.5532 + t * (0.000136 + t * (- 0.00001149)))) mod rev) / seccon
  
  ' mean argument of the latitude of the moon
  
  f = (335779.526232 + t * (1739527262.8478 + t * (-12.7512 + t * (-0.001037 + t * (0.00000417)))) mod rev) / seccon
  
  ' mean elongation of the moon from the sun
  
  d = (1072260.70369 + t * (1602961601.2090 + t * (- 6.3706 + t * (0.006593 + t * (- 0.00003169)))) mod rev) / seccon
  
  ' mean longitude of the ascending node of the moon (from simon section 3.4(b.3), precession = 5028.8200 arcsec/cy)
  
  omega = (450160.398036 + t * (- 6962890.5431 + t * (7.4722 + t * (0.007702 + t * (- 0.00005939)))) mod rev) / seccon
  
  dpsi = 0.0
  
  deps = 0.0
  
  ' sum nutation series terms
  
  for i% = 13 to 1 step -1
    
    arg = xnut(1, i%) * el + xnut(2, i%) * elp + xnut(3, i%) * f + xnut(4, i%) * d + xnut(5, i%) * omega
    
    dpsi = (xnut(6, i%) + xnut(7, i%) * t) * sin(arg) + xnut(10, i%) * cos(arg) + dpsi
    
    deps = (xnut(8, i%) + xnut(9, i%) * t) * cos(arg) + xnut(11, i%) * sin(arg) + deps
    
  next i%
  
  dpsi = 1.0e-7 * dpsi
  
  deps = 1.0e-7 * deps
  
  ' add in out-of-phase component of principal (18.6-year) term
  ' (to avoid small but long-term bias in results)
  
  dpsi = dpsi + 0.0033 * cos(omega)
  
  deps = deps + 0.0015 * sin(omega)
  
END sub
  
  ''''''''''''''''''''''''
  ''''''''''''''''''''''''
  
sub tdb2utc (jdtdb, jdutc)
  
  ' convert TDB julian day to UTC julian day subroutine
  
  ' input
  
  '  jdtdb = TDB julian day
  
  ' output
  
  '  jdutc = UTC julian day
  
  '''''''''''''''''''''''''
  
  local x1, x2
  
  local xroot, froot
  
  jdsaved = jdtdb
  
  ' set lower and upper bounds
  
  x1 = jdsaved - 0.1
  
  x2 = jdsaved + 0.1
  
  ' solve for UTC julian day using Brent's method
  
  jbrent(x1, x2, 1.0e-8, xroot, froot)
  
  jdutc = xroot
  
end sub
  
  ''''''''''''''''''''''''
  ''''''''''''''''''''''''
  
sub gsite (angle, rsite())
  
  ' ground site position vector
  
  ' input
  
  '  angle  = local sidereal time or east longitude
  '           (radians; 0 <= angle <= 2*pi)
  
  ' input via global
  
  '  obslat = geodetic latitude (radians)
  '           (+north, -south; -pi/2 <= lat <= -pi/2)
  '  obsalt = geodetic altitude (meters)
  '           (+ above sea level, - below sea level)
  
  ' output
  
  '  rsite = ground site position vector (kilometers)
  
  ' special notes
  
  '  (1) eci coordinates if angle = local sidereal time
  
  '  (2) ecf coordinates if angle = east longitude
  
  '  (3) geocentric, equatorial coordinates
  
  '''''''''''''''''''''''''''''''''''''''''
  
  LOCAL slat, clat
  
  local sangle, cangle
  
  LOCAL b, c, d
  
  slat = sin(obslat)
  
  clat = cos(obslat)
  
  sangle = sin(angle)
  
  cangle = cos(angle)
  
  ' compute geodetic constants
  
  b = sqr(1.0 - (2.0 * flat - flat * flat) * slat * slat)
  
  c = reqm / b + 0.001 * obsalt
  
  d = reqm * (1.0 - flat) * (1.0 - flat) / b + 0.001 * obsalt
  
  ' compute x, y and z components of position vector (kilometers)
  
  rsite(1) = c * clat * cangle
  
  rsite(2) = c * clat * sangle
  
  rsite(3) = d * slat
  
END sub
  
  ''''''''''''''''''''
  ''''''''''''''''''''
  
sub gast2(jdate, gast)
  
  ' greenwich apparent sidereal time
  
  ' input
  
  '  jdate = julian date
  
  ' output
  
  '  gast = greenwich apparent sidereal time (radians)
  
  '''''''''''''''''''''''''''''''''''''''''''''''''''
  
  LOCAL t, t2, t3, eqeq, dpsi, deps
  
  LOCAL th, tl, obm, obt, st, x
  
  local tjdh as integer, tjdl
  
  tjdh = int(jdate)
  
  tjdl = jdate - tjdh
  
  th = (tjdh - 2451545.0) / 36525
  
  tl = tjdl / 36525.0
  
  t = th + tl
  
  t2 = t * t
  
  t3 = t2 * t
  
  ' obtain equation of the equinoxes
  
  eqeq = 0.0
  
  ' obtain nutation parameters in seconds of arc
  
  nut2000_lp(jdate, dpsi, deps)
  
  ' compute mean obliquity of the ecliptic in seconds of arc
  
  obm = 84381.4480 - 46.8150 * t - 0.00059 * t2 + 0.001813 * t3
  
  ' compute true obliquity of the ecliptic in seconds of arc
  
  obliq = obm + deps
  
  ' compute equation of the equinoxes in seconds of time
  
  eqeq = (dpsi / 15.0) * cos(obliq / seccon)
  
  st = eqeq - 6.2e-6 * t3 + 0.093104 * t2 + 67310.54841 + 8640184.812866 * tl + 3155760000.0 * tl + 8640184.812866 * th + 3155760000.0 * th
  
  ' modulo 24 hours
  
  x = st / 3600.0
  
  gast = x - 24.0 * fix(x / 24.0)
  
  if (gast < 0.0) then
    
    gast = gast + 24.0
    
  end if
  
  ' convert to radians
  
  gast = pi2 * (gast / 24.0)
  
END sub
  
  ''''''''''''''''''''''''''''
  ''''''''''''''''''''''''''''
  
sub getdate (month, day, year)
  
  ' interactive request calendar date subroutine
  
  do
    print " "
    print "please input the initial calendar date"
    print " "
    print "(month [1 - 12], day [1 - 31], year [yyyy])"
    print "< for example, october 21, 1986 is input as 10,21,1986 >"
    print "< b.c. dates are negative, a.d. dates are positive >"
    print "< the day of the month may also include a decimal part >"
    print " "
    input month, day, year
    
  loop until (month >= 1 and month <= 12) and (day >= 1 and day <= 31)
  
end sub
  
  '''''''''''''''''''''''''''''''''''
  '''''''''''''''''''''''''''''''''''
  
sub observer(obslat, obslong, obsalt)
  
  ' interactive request of latitude, longitude and altitude subroutine
  
  ' output
  
  '  obslat  = latitude (radians)
  '  obslong = longitude (radians)
  '  obsalt  = altitude (meters)
  
  ''''''''''''''''''''''''''''''
  
  do
    
    print "please input the geographic latitude of the observer"
    print "(degrees [-90 to +90], minutes [0 - 60], seconds [0 - 60])"
    print "(north latitudes are positive, south latitudes are negative)"
    
    input obslat.deg$, obslat.min, obslat.sec
    
  loop until (abs(val(obslat.deg$)) <= 90.0 and (obslat.min >= 0.0 and obslat.min <= 60.0) and (obslat.sec >= 0.0 and obslat.sec <= 60.0))
  
  if (left$(obslat.deg$, 2) = "-0") then
    
    obslat = -dtr * (obslat.min / 60.0 + obslat.sec / 3600.0)
    
  elseif (val(obslat.deg$) = 0.0) then
    
    obslat = dtr * (obslat.min / 60.0 + obslat.sec / 3600.0)
    
  else
    
    obslat = dtr * (sgn(val(obslat.deg$)) * (abs(val(obslat.deg$)) + obslat.min / 60.0 + obslat.sec / 3600.0))
    
  endif
  
  do
    
    print
    print "please input the geographic longitude of the observer"
    print "(degrees [0 - 360], minutes [0 - 60], seconds [0 - 60])"
    print "(east longitude is positive, west longitude is negative)"
    
    input obslong.deg$, obslong.min, obslong.sec
    
  loop until (abs(val(obslong.deg$)) >= 0.0 and abs(val(obslong.deg$)) <= 360.0) and (obslong.min >= 0.0 and obslong.min <= 60.0) and (obslong.sec >= 0.0 and obslong.sec <= 60.0)
  
  if (left$(obslong.deg$, 2) = "-0") then
    
    obslong = -dtr * (obslong.min / 60 + obslong.sec / 3600)
    
  elseif (val(obslong.deg$) = 0.0) then
    
    obslong = dtr * (obslong.min / 60.0 + obslong.sec / 3600.0)
    
  else
    
    obslong = dtr * (sgn(val(obslong.deg$)) * (abs(val(obslong.deg$)) + obslong.min / 60.0 + obslong.sec / 3600.0))
    
  endif
  
  print " "
  
  print "please input the altitude of the observer (meters)"
  print "(positive above sea level, negative below sea level)"
  
  input obsalt
  
end sub
  
  ''''''''''''''''''''''''''''''''
  ''''''''''''''''''''''''''''''''
  
sub minima(a, b, tolm, xmin, fmin)
  
  ' one-dimensional minimization
  
  ' Brent's method
  
  ' input
  
  '  a    = initial x search value
  '  b    = final x search value
  '  tolm = convergence criterion
  
  ' output
  
  '  xmin = minimum x value
  
  ' note
  
  '  user-defined objective subroutine
  '  coded as usr_func(x, fx)
  
  ' remember: a maximum is simply a minimum
  '           with a negative attitude!
  
  '''''''''''''''''''''''''''''''''''''
  
  ' machine epsilon
  
  LOCAL epsm = 2.23e-16
  
  ' golden number
  
  LOCAL c = 0.38196601125
  
  LOCAL iter as integer, d, e
  
  LOCAL t2, p, q
  
  local r, u, fu
  
  LOCAL x, xm, w
  
  local v, fx, fw
  
  LOCAL tol1, fv
  
  x = a + c * (b - a)
  
  w = x
  
  v = w
  
  e = 0.0
  p = 0.0
  q = 0.0
  r = 0.0
  
  sefunc(x, fx)
  
  fw = fx
  
  fv = fw
  
  for iter = 1 to 100
    
    if (iter > 50) then
      
      print ("error in function minima!")
      print ("(more than 50 iterations)")
      
      end
      
    end if
    
    xm = 0.5 * (a + b)
    
    tol1 = tolm * abs(x) + epsm
    
    t2 = 2.0 * tol1
    
    if (abs(x - xm) <= (t2 - 0.5 * (b - a))) then
      
      xmin = x
      
      fmin = fx
      
      exit sub
      
    end if
    
    if (abs(e) > tol1) then
      
      r = (x - w) * (fx - fv)
      
      q = (x - v) * (fx - fw)
      
      p = (x - v) * q - (x - w) * r
      
      q = 2.0 * (q - r)
      
      if (q > 0.0) then
        
        p = -p
        
      end if
      
      q = abs(q)
      
      r = e
      
      e = d
      
    end if
    
    if ((abs(p) >= abs(0.5 * q * r)) or (p <= q * (a - x)) or (p >= q * (b - x))) then
      
      if (x >= xm) then
        
        e = a - x
        
      else
        
        e = b - x
        
      end if
      
      d = c * e
      
    else
      
      d = p / q
      
      u = x + d
      
      if ((u - a) < t2) or ((b - u) < t2) then
        
        d = sgn(xm - x) * tol1
        
      end if
      
    end if
    
    if (abs(d) >= tol1) then
      
      u = x + d
      
    else
      
      u = x + sgn(d) * tol1
      
    end if
    
    sefunc(u, fu)
    
    if (fu <= fx) then
      
      if (u >= x) then
        
        a = x
        
      else
        
        b = x
        
      end if
      
      v = w
      
      fv = fw
      
      w = x
      
      fw = fx
      
      x = u
      
      fx = fu
      
    else
      
      if (u < x) then
        
        a = u
        
      else
        
        b = u
        
      end if
      
      if ((fu <= fw) Or (w = x)) then
        
        v = w
        
        fv = fw
        
        w = u
        
        fw = fu
        
      elseif ((fu <= fv) Or (v = x) Or (v = w)) then
        
        v = u
        
        fv = fu
        
      end if
      
    end if
    
  next iter
  
end sub
  
  ''''''''''''''''''''''''''''''''''''''''''''''''
  ''''''''''''''''''''''''''''''''''''''''''''''''
  
sub broot(x1in, x2in, factor, dxmax, x1out, x2out)
  
  ' bracket a single root of a nonlinear equation
  
  ' input
  
  '  x1in   = initial guess for first bracketing x value
  '  x2in   = initial guess for second bracketing x value
  '  factor = acceleration factor (non-dimensional)
  '  dxmax  = rectification interval
  
  ' output
  
  '  xout1 = final value for first bracketing x value
  '  xout2 = final value for second bracketing x value
  
  ''''''''''''''''''''''''''''''''''''''''''''''''''''
  
  LOCAL f1, f2
  
  local x3, dx
  
  ' evaluate objective function at initial value
  
  sefunc(x1in, f1)
  
  ' save initial value
  
  x3 = x1in
  
  ' save initial delta-x
  
  dx = x2in - x1in
  
  ' perform bracketing until the product of the
  ' two function values is negative
  
  do
    
    ' geometrically accelerate the second point
    
    x2in = x2in + factor * (x2in - x3)
    
    ' evaluate objective function at x2
    
    sefunc(x2in, f2)
    
    ' check to see if rectification is required
    
    if (abs(x2in - x3) > dxmax) then
      
      x3 = x2in - dx
      
    end if
    
    ' is the root bracketed?
    
    if ((f1 * f2) < 0.0) then exit do
    
  loop
  
  x1out = x1in
  
  x2out = x2in
  
END sub
  
  ''''''''''''''''''''''''''''''''''''''
  ''''''''''''''''''''''''''''''''''''''
  
sub brent(x1, x2, tol, xroot, froot)
  
  ' real root of a single non-linear function subroutine
  
  ' input
  
  '  x1  = lower bound of search interval
  '  x2  = upper bound of search interval
  '  tol = convergence criter%ia
  
  ' output
  
  '  xroot = real root of f(x) = 0
  '  froot = function value
  
  ' note: requires sub sefunc
  
  '''''''''''''''''''''''''''
  
  local eps, a, b
  
  local c, d, e
  
  local fa, fb, fcc
  
  local tol1, xm, p
  
  local q, r, s
  
  local xmin, tmp
  
  eps = 2.23e-16
  
  e = 0.0
  
  a = x1
  
  b = x2
  
  sefunc(a, fa)
  
  sefunc(b, fb)
  
  fcc = fb
  
  for iter% = 1 to 50
    
    if (fb * fcc > 0.0) then
      
      c = a
      
      fcc = fa
      
      d = b - a
      
      e = d
      
    end if
    
    if (abs(fcc) < abs(fb)) then
      
      a = b
      
      b = c
      
      c = a
      
      fa = fb
      
      fb = fcc
      
      fcc = fa
      
    end if
    
    tol1 = 2.0 * eps * abs(b) + 0.5 * tol
    
    xm = 0.5 * (c - b)
    
    if (abs(xm) <= tol1 or fb = 0.0) then exit for
    
    if (abs(e) >= tol1 and abs(fa) > abs(fb)) then
      
      s = fb / fa
      
      if (a = c) then
        
        p = 2.0 * xm * s
        
        q = 1.0 - s
        
      else
        
        q = fa / fcc
        
        r = fb / fcc
        
        p = s * (2.0 * xm * q * (q - r) - (b - a) * (r - 1.0))
        
        q = (q - 1.0) * (r - 1.0) * (s - 1.0)
        
      end if
      
      if (p > 0.0) then q = -q
      
      p = abs(p)
      
      min = abs(e * q)
      
      tmp = 3.0 * xm * q - abs(tol1 * q)
      
      if (min < tmp) then min = tmp
      
      if (2.0 * p < min) then
        
        e = d
        
        d = p / q
        
      else
        
        d = xm
        
        e = d
        
      end if
      
    else
      
      d = xm
      
      e = d
      
    end if
    
    a = b
    
    fa = fb
    
    if (abs(d) > tol1) then
      
      b = b + d
      
    else
      
      b = b + sgn(xm) * tol1
      
    end if
    
    sefunc(b, fb)
    
  next iter%
  
  froot = fb
  
  xroot = b
  
end sub
  
  ''''''''''''''''''''''''''''''''''''''
  ''''''''''''''''''''''''''''''''''''''
  
sub jbrent(x1, x2, tol, xroot, froot)
  
  ' real root of a single non-linear function subroutine
  
  ' input
  
  '  x1  = lower bound of search interval
  '  x2  = upper bound of search interval
  '  tol = convergence criter%ia
  
  ' output
  
  '  xroot = real root of f(x) = 0
  '  froot = function value
  
  ' note: requires sub jdfunc
  
  '''''''''''''''''''''''''''
  
  local eps, a, b
  
  local c, d, e
  
  local fa, fb, fcc
  
  local tol1, xm, p
  
  local q, r, s
  
  local xmin, tmp, iter as integer
  
  eps = 2.23e-16
  
  e = 0.0
  
  a = x1
  
  b = x2
  
  jdfunc(a, fa)
  
  jdfunc(b, fb)
  
  fcc = fb
  
  for iter = 1 to 50
    
    if (fb * fcc > 0.0) then
      
      c = a
      
      fcc = fa
      
      d = b - a
      
      e = d
      
    end if
    
    if (abs(fcc) < abs(fb)) then
      
      a = b
      
      b = c
      
      c = a
      
      fa = fb
      
      fb = fcc
      
      fcc = fa
      
    end if
    
    tol1 = 2.0 * eps * abs(b) + 0.5 * tol
    
    xm = 0.5 * (c - b)
    
    if (abs(xm) <= tol1 or fb = 0) then exit for
    
    if (abs(e) >= tol1 and abs(fa) > abs(fb)) then
      
      s = fb / fa
      
      if (a = c) then
        
        p = 2.0 * xm * s
        
        q = 1.0 - s
        
      else
        
        q = fa / fcc
        
        r = fb / fcc
        
        p = s * (2.0 * xm * q * (q - r) - (b - a) * (r - 1.0))
        
        q = (q - 1.0) * (r - 1.0) * (s - 1.0)
        
      end if
      
      if (p > 0) then q = -q
      
      p = abs(p)
      
      xmin = abs(e * q)
      
      tmp = 3.0 * xm * q - abs(tol1 * q)
      
      if (xmin < tmp) then xmin = tmp
      
      if (2.0 * p < xmin) then
        
        e = d
        
        d = p / q
        
      else
        
        d = xm
        
        e = d
        
      end if
      
    else
      
      d = xm
      
      e = d
      
    end if
    
    a = b
    
    fa = fb
    
    if (abs(d) > tol1) then
      
      b = b + d
      
    else
      
      b = b + sgn(xm) * tol1
      
    end if
    
    jdfunc(b, fb)
    
  next iter
  
  froot = fb
  
  xroot = b
  
end sub
  
  ''''''''''''''''''''''''''''''''
  ''''''''''''''''''''''''''''''''
  
sub julian(month, day, year, jday)
  
  ' Gregorian date to julian day subroutine
  
  ' input
  
  '  month = calendar month
  '  day   = calendar day
  '  year  = calendar year (all four digits)
  
  ' output
  
  '  jday = julian day
  
  ' special notes
  
  '  (1) calendar year must include all digits
  
  '  (2) will report October 5, 1582 to October 14, 1582
  '      as invalid calendar dates and exit
  
  '''''''''''''''''''''''''''''''''''''''''
  
  local a, b, c, m, y
  
  y = year
  
  m = month
  
  b = 0.0
  
  c = 0.0
  
  if (m <= 2.0) then
    
    y = y - 1.0
    
    m = m + 12.0
    
  end if
  
  if (y < 0.0) then c = -0.75
  
  if (year < 1582.0) then
    
    ' null
    
  elseif (year > 1582.0) then
    
    a = fix(y / 100.0)
    
    b = 2.0 - a + fix(a / 4.0)
    
  elseif (month < 10.0) then
    
    ' null
    
  elseif (month > 10.0) then
    
    a = fix(y / 100.0)
    
    b = 2.0 - a + fix(a / 4.0)
    
  elseif (day <= 4.0) then
    
    ' null
    
  elseif (day > 14.0) then
    
    a = fix(y / 100.0)
    
    b = 2.0 - a + fix(a / 4.0)
    
  else
    
    print "this date does not exist!!"
    
    exit
    
  end if
  
  jday = fix(365.25 * y + c) + fix(30.6001 * (m + 1.0)) + day + b + 1720994.5
  
end sub
  
  '''''''''''''''''''''''''''''''
  '''''''''''''''''''''''''''''''
  
sub gdate(jday, month, day, year)
  
  ' Julian day to Gregorian date subroutine
  
  ' input
  
  '  jday = julian day
  
  ' output
  
  '  month = calendar month
  '  day   = calendar day
  '  year  = calendar year
  
  ''''''''''''''''''''''''
  
  local a, b, c, d, e, f, z, alpha
  
  z = fix(jday + 0.5)
  
  f = jday + 0.5 - z
  
  if (z < 2299161) then
    
    a = z
    
  else
    
    alpha = fix((z - 1867216.25) / 36524.25)
    
    a = z + 1.0 + alpha - fix(alpha / 4.0)
    
  end if
  
  b = a + 1524.0
  
  c = fix((b - 122.1) / 365.25)
  
  d = fix(365.25 * c)
  
  e = fix((b - d) / 30.6001)
  
  day = b - d - fix(30.6001 * e) + f
  
  if (e < 13.5) then
    
    month = e - 1.0
    
  else
    
    month = e - 13.0
    
  end if
  
  if (month > 2.5) then
    
    year = c - 4716.0
    
  else
    
    year = c - 4715.0
    
  end if
  
end sub
  
  ''''''''''''''''''''''''
  ''''''''''''''''''''''''
  
sub utc2tdb (jdutc, jdtdb)
  
  ' convert UTC julian date to TDB julian date
  
  ' input
  
  '  jdutc   = UTC julian day
  '  tai_utc = TAI-UTC (seconds)
  
  ' output
  
  '  jdtdb = TDB julian day
  
  ' Reference Frames in Astronomy and Geophysics
  ' J. Kovalevsky et al., 1989, pp. 439-442
  
  '''''''''''''''''''''''''''''''''''''''''
  
  local corr, jdtt, t, leapsecond
  
  ' find current number of leap seconds
  
  findleap(jdutc, leapsecond)
  
  ' compute TDT julian date
  
  corr = (leapsecond + 32.184) / 86400.0
  
  jdtt = jdutc + corr
  
  ' time argument for correction
  
  t = (jdtt - 2451545.0) / 36525.0
  
  ' compute correction in microseconds
  
  corr = 1656.675 * sin(dtr * (35999.3729 * t + 357.5287))
  corr = corr + 22.418     * sin(dtr * (32964.467  * t + 246.199))
  corr = corr + 13.84      * sin(dtr * (71998.746  * t + 355.057))
  corr = corr +  4.77      * sin(dtr * ( 3034.906  * t +  25.463))
  corr = corr +  4.677     * sin(dtr * (34777.259  * t + 230.394))
  corr = corr + 10.216 * t * sin(dtr * (35999.373  * t + 243.451))
  corr = corr +  0.171 * t * sin(dtr * (71998.746  * t + 240.98 ))
  corr = corr +  0.027 * t * sin(dtr * ( 1222.114  * t + 194.661))
  corr = corr +  0.027 * t * sin(dtr * ( 3034.906  * t + 336.061))
  corr = corr +  0.026 * t * sin(dtr * (  -20.186  * t +   9.382))
  corr = corr +  0.007 * t * sin(dtr * (29929.562  * t + 264.911))
  corr = corr +  0.006 * t * sin(dtr * (  150.678  * t +  59.775))
  corr = corr +  0.005 * t * sin(dtr * ( 9037.513  * t + 256.025))
  corr = corr +  0.043 * t * sin(dtr * (35999.373  * t + 151.121))
  
  ' convert corrections to days
  
  corr = 0.000001 * corr / 86400.0
  
  ' TDB julian date
  
  jdtdb = jdtt + corr
  
end sub
  
  ''''''''''''''''''''''''''''
  ''''''''''''''''''''''''''''
  
sub findleap(jday, leapsecond)
  
  ' find number of leap seconds for utc julian day
  
  ' input
  
  '  jday = utc julian day
  
  ' input via global
  
  '  jdleap  = array of utc julian dates
  '  leapsec = array of leap seconds
  
  ' output
  
  '  leapsecond = number of leap seconds
  
  ''''''''''''''''''''''''''''''''''''''
  
  local i as integer
  
  if (jday <= jdleap(1)) then
    
    ' date is <= 1972; set to first data element
    
    leapsecond = leapsec(1)
    
    exit sub
    
  end if
  
  if (jday >= jdleap(28)) then
    
    ' date is >= end of current data
    ' set to last data element
    
    leapsecond = leapsec(28)
    
    exit sub
    
  end if
  
  ' find data within table
  
  for i = 1 to 27
    
    if (jday >= jdleap(i) and jday < jdleap(i + 1)) then
      
      leapsecond = leapsec(i)
      
      exit sub
      
    end if
    
  next i
  
end sub
  
  ''''''''''''''''''''
  ''''''''''''''''''''
  
sub jdfunc (jdutc, fx)
  
  ' objective function for tdb2utc
  
  ' input
  
  '  jdin = current value for UTC julian day
  
  ' output
  
  '  fx = delta julian day
  
  ''''''''''''''''''''''''
  
  local jdwrk
  
  utc2tdb(jdutc, jdwrk)
  
  fx = jdwrk - jdsaved
  
end sub
  
  ''''''''''''''''
  ''''''''''''''''
  
function modulo(x)
  
  ' modulo 2 pi function
  
  ''''''''''''''''''''''
  
  local a
  
  a = x - pi2 * fix(x / pi2)
  
  if (a < 0.0) then
    
    a = a + pi2
    
  end if
  
  modulo = a
  
end function
  
  ''''''''''''''''''
  ''''''''''''''''''
  
function atan3(a, b)
  
  ' four quadrant inverse tangent function
  
  ' input
  
  '  a = sine of angle
  '  b = cosine of angle
  
  ' output
  
  '  atan3 = angle (0 =< atan3 <= 2 * pi; radians)
  
  ''''''''''''''''''''''''''''''''''''''''''''''''
  
  local c
  
  if (abs(a) < 1.0e-10) then
    
    atan3 = (1.0 - sgn(b)) * pidiv2
    
    exit function
    
  else
    
    c = (2.0 - sgn(a)) * pidiv2
    
  endif
  
  if (abs(b) < 1.0e-10) then
    
    atan3 = c
    
    exit function
    
  else
    
    atan3 = c + sgn(a) * sgn(b) * (abs(atn(a / b)) - pidiv2)
    
  endif
  
end function
  
  ''''''''''''''''''
  ''''''''''''''''''
  
function vecmag(a())
  
  ' vector magnitude function
  
  ' input
  
  '  { a } = column vector ( 3 rows by 1 column )
  
  ' output
  
  '  vecmag = scalar magnitude of vector { a }
  
  vecmag = sqr(a(1) * a(1) + a(2) * a(2) + a(3) * a(3))
  
end function
  
  ''''''''''''''''''''
  ''''''''''''''''''''
  
sub uvector (a(), b())
  
  ' unit vector subroutine
  
  ' input
  
  '  a = column vector (3 rows by 1 column)
  
  ' output
  
  '  b = unit vector (3 rows by 1 column)
  
  '''''''''''''''''''''''''''''''''''''''
  
  local i as integer, amag
  
  amag = vecmag(a())
  
  for i = 1 to 3
    
    if (amag <> 0.0) then
      
      b(i) = a(i) / amag
      
    else
      
      b(i) = 0.0
      
    end if
    
  next i
  
end sub
  
  '''''''''''''''''''''
  '''''''''''''''''''''
  
function vdot(a(), b())
  
  ' vector dot product function
  
  ' c = { a } dot { b }
  
  ' input
  
  '  n%    = number of rows
  '  { a } = column vector with n rows
  '  { b } = column vector with n rows
  
  ' output
  
  '  vdot = dot product of { a } and { b }
  
  ''''''''''''''''''''''''''''''''''''''''
  
  local c = 0.0
  
  for i% = 1 to 3
    
    c = c + a(i%) * b(i%)
    
  next i%
  
  vdot = c
  
end function
  
  '''''''''''''''''''''''
  '''''''''''''''''''''''
  
sub vcross(a(), b(), c())
  
  ' vector cross product subroutine
  
  ' { c } = { a } x { b }
  
  ' input
  
  '  { a } = vector a ( 3 rows by 1 column )
  '  { b } = vector b ( 3 rows by 1 column )
  
  ' output
  
  '  { c } = { a } x { b } ( 3 rows by 1 column )
  
  c(1) = a(2) * b(3) - a(3) * b(2)
  c(2) = a(3) * b(1) - a(1) * b(3)
  c(3) = a(1) * b(2) - a(2) * b(2)
  
end sub
  
  ''''''''''''''''''''''''
  ''''''''''''''''''''''''
  
sub matxvec(a(), b(), c())
  
  ' matrix/vector multiplication subroutine
  
  ' { c } = [ a ] * { b }
  
  ' input
  
  '  a  = matrix a ( 3 rows by 3 columns )
  '  b  = vector b ( 3 rows )
  
  ' output
  
  '  c = vector c ( 3 rows )
  
  ''''''''''''''''''''''''''
  
  local s, i%, j%
  
  for i% = 1 to 3
    
    s = 0.0
    
    for j% = 1 to 3
      
      s = s + a(i%, j%) * b(j%)
      
    next j%
    
    c(i%) = s
    
  next i%
  
end sub
  
  ''''''''''''''''''''''
  ''''''''''''''''''''''
  
sub transpose (a(), b())
  
  ' matrix traspose subroutine
  
  ' input
  
  '  m = number of rows in matrix [ a ]
  '  n = number of columns in matrix [ a ]
  '  a = matrix a ( 3 rows by 3 columns )
  
  ' output
  
  '  b = matrix transpose ( 3 rows by 3 columns )
  
  '''''''''''''''''''''''''''''''''''''''''''''''
  
  local i%, j%
  
  for i% = 1 to 3
    
    for j% = 1 to 3
      
      b(i%, j%) = a(j%, i%)
      
    next j%
    
  next i%
  
end sub
  
  '''''''''''''''
  '''''''''''''''
  
sub jd2str(jdutc)
  
  ' convert julian day to calendar date and UTC time
  
  ''''''''''''''''''''''''''''''''''''''''''''''''''
  
  gdate (jdutc, cmonth, day, year)
  
  print "calendar date  ", month$(cmonth), " ", STR$(int(day)), " ", str$(year)
  
  print " "
  
  thr0 = 24.0 * (day - int(day))
  
  thr = int(thr0)
  
  tmin0 = 60.0 * (thr0 - thr)
  
  tmin = int(tmin0)
  
  tsec = 60.0 * (tmin0 - tmin)
  
  print "UTC time       ", str$(thr) + " hours " + str$(tmin) + " minutes " + str$(tsec, 0, 2) + " seconds"
  
end sub
  
  '''''''''''''''''''
  '''''''''''''''''''
  
sub deg2str(dd, dms$)
  
  ' convert decimal degrees to degrees,
  ' minutes, seconds string
  
  ' input
  
  '  dd = angle in decimal degrees
  
  ' output
  
  '  dms$ = string equivalent
  
  '''''''''''''''''''''''''''
  
  local d1, d, m, s
  
  d1 = abs(dd)
  
  d = fix(d1)
  
  d1 = (d1 - d) * 60.0
  
  m = fix(d1)
  
  s = (d1 - m) * 60.0
  
  if (dd < 0.0) then
    
    if (d <> 0.0) then
      
      d = -d
      
    elseif (m <> 0.0) then
      
      m = -m
      
    else
      
      s = -s
      
    end if
    
  end if
  
  dms$ = str$(d) + " deg " + str$(m) + " min " + str$(s, 0, 2) + " sec"
  
end sub
