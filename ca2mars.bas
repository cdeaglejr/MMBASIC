  ' program ca2mars.bas          May 24, 2020
  
  ' predict close approach between Earth and Mars
  
  ' double precision MMBASIC
  
  ''''''''''''''''''''''''''
  
  option default float
  
  option base 1
  
  const pi2 = 2.0 * pi, pidiv2 = 0.5 * pi, rtd = 180.0 / pi, dtr = pi / 180.0
  
  ' astronomical unit (kilometers)
  
  const aunit = 149597870.691
  
  dim jdleap(28), leapsec(28)
  
  dim sl(50), sr(50), sa(50), sb(50), cl(184), al(184), bl(184)
  
  dim jdtdbi, cmonth, cday, cyear, month$(12)
  
  ' read sun and planet data
  
  read_data
  
  ''''''''''''''''''
  ' begin simulation
  ''''''''''''''''''
  
  open "ca2mars.txt" for output as #1
  
  print " "
  print "closest approach between the Earth and Mars"
  print "==========================================="
  
  ' request initial calendar date (month, day, year)
  
  getdate(cmonth, cday, cyear)
  
  ' initial utc julian day
  
  julian(cmonth, cday, cyear, jdutc)
  
  ' compute initial tdb julian date
  
  utc2tdb(jdutc, jdtdb)
  
  jdtdbi = jdtdb
  
  ' request search duration (days)
  
  print " "
  print "please input the number of days to search"
  
  input ndays
  
  print " "
  print "searching for close approach conditions ..."
  print " "
  
  ' define search parameters
  
  ti = 0.0
  
  tf = ndays
  
  tisaved = ti
  
  dt = 10.0
  
  dtsml = 0.1
  
  ' find closest approach conditions
  
  ca_event(ti, tf, dt, dtsml)
  
  close #1
  
end
  
  ''''''''''''''''''''''''''''''
  ''''''''''''''''''''''''''''''
  
sub ca_event (ti, tf, dt, dtsml)
  
  ' predict closest approach events
  
  ' input
  
  '  ti    = initial simulation time
  '  tf    = final simulation time
  '  dt    = step size used for bounding minima
  '  dtsml = small step size used to determine whether
  '          the function is increasing or decreasing
  
  '''''''''''''''''''''''''''''''''''''''''''''''''''
  
  LOCAL tolm
  
  local fmin1, tmin1
  
  LOCAL ftemp, df, dflft
  
  local el, er
  
  LOCAL t, ft
  
  local iter1%, iter2%, iter3%
  
  ' initialization
  
  tolm = 1.0e-10
  
  df = 1.0
  
  for iter1% = 1 to 1000
    
    ' find where function first starts decreasing
    
    for iter2% = 1 to 1000
      
      if (df <= 0.0) then
        
        exit for
        
      end if
      
      t = t + dt
      
      ca_func(t, ft)
      
      ca_func(t- dtsml, ftemp)
      
      df = ft - ftemp
      
    next iter2%
    
    ' function decreasing - find where function
    ' first starts increasing
    
    for iter3% = 1 to 1000
      
      el = t
      
      dflft = df
      
      t = t + dt
      
      ca_func(t, ft)
      
      ca_func(t - dtsml, ftemp)
      
      df = ft - ftemp
      
      if (df > 0.0) then exit for
      
    next iter3%
    
    er = t
    
    ' calculate minimum using Brent's method
    
    minima(el, er, tolm, tmin1, fmin1)
    
    el = er
    
    ' print current conditions
    
    ca_print(tmin1)
    
    if (t >= tf) then exit for
    
  next iter1%
  
end sub
  
  ''''''''''''''''
  ''''''''''''''''
  
sub ca_print(topt)
  
  ' print close approach conditions
  
  '''''''''''''''''''''''''''''''''
  
  LOCAL jdutc, jdtdb, rpg(3), rph(3), rasc, decl
  
  print " "
  print #1, " "
  
  print "time and conditions at Earth-Mars close approach"
  print #1, "time and conditions at Earth-Mars close approach"
  
  print "================================================"
  print #1, "================================================"
  
  print " "
  print #1, " "
  
  ' TDB julian day
  
  jdtdb = jdtdbi + topt
  
  ' compute and display UTC julian date
  
  tdb2utc(jdtdb, jdutc)
  
  jd2str(jdutc)
  
  PRINT " "
  print #1, " "
  
  print "UTC julian day       ", str$(jdutc, 0, 8)
  print #1, "UTC julian day       ", str$(jdutc, 0, 8)
  
  PRINT " "
  print #1, " "
  
  ephem(4, jdtdb, rpg(), rph(), rasc, decl)
  
  print "geocentric distance  ", str$(vecmag(rpg()), 0, 10), " AU"
  print #1, "geocentric distance  ", str$(vecmag(rpg()), 0, 10), " AU"
  
  print "                     ", str$(aunit * vecmag(rpg()), 0, 4), " kilometers"
  print #1, "                     ", str$(aunit * vecmag(rpg()), 0, 4), " kilometers"
  
  print " "
  print #1, " "
  
END sub
  
  ''''''''''''''''
  ''''''''''''''''
  
sub ca_func(x, fx)
  
  ' closest approach objective function subroutine
  
  ''''''''''''''''''''''''''''''''''''''''''''''''
  
  local jdtdb, rpg(3), rph(3), rasc, decl
  
  ' current tdb julian day
  
  jdtdb = jdtdbi + x
  
  ' compute mars ephemeris
  
  ephem(4, jdtdb, rpg(), rph(), rasc, decl)
  
  ' objective function - scalar magnitude of Earth-to-Mars distance
  
  fx = vecmag(rpg())
  
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
  '  coded as ca_func(x, fx)
  
  ' remember: a maximum is simply a minimum
  '           with a negative attitude!
  
  '''''''''''''''''''''''''''''''''''''
  
  ' machine epsilon
  
  LOCAL epsm = 2.23e-16
  
  ' golden number
  
  LOCAL c = 0.38196601125
  
  LOCAL d, e
  
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
  
  ca_func(x, fx)
  
  fw = fx
  
  fv = fw
  
  for iter% = 1 to 100
    
    if (iter% > 50) then
      
      print ("error in function minima!")
      print ("(more than 50 iterations)")
      
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
    
    ca_func(u, fu)
    
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
    
  next iter%
  
end sub

  '''''''''''''''''''''''''''''''''''''
  '''''''''''''''''''''''''''''''''''''
  
sub realroot(x1, x2, tol, xroot, froot)
  
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
  
  local eps, a, b, c, d, e, fa, fb, fcc, tol1
  
  local xm, p, q, r, s, xmin, tmp
  
  eps = 2.23e-16
  
  e = 0.0
  
  a = x1
  
  b = x2
  
  jdfunc(a, fa)
  
  jdfunc(b, fb)
  
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
    
  next iter%
  
  froot = fb
  
  xroot = b
  
end sub
  
  ''''''''''''''''''''''''''''''''''''''''''''''''
  ''''''''''''''''''''''''''''''''''''''''''''''''
  
sub ephem(ibody%, jdtdb, rpg(), rph(), rasc, decl)
  
  ' sun and inner planet ephemeris subroutine
  
  ' input
  
  '  ibody% = celestial body index
  '  jdtdb  = tdb julian day
  
  ' output
  
  '  rpg() = geocentric position vector (au)
  '  rph() = heliocentric position vector (au)
  '  rasc  = geocentric right ascension (radians)
  '  decl  = geocentric declination (radians)
  
  '''''''''''''''''''''''''''''''''''''''''''
  
  local rsun(3)
  
  local dls, drs, gl, gb, pl, pb, pr, alon, alat
  
  ' compute coordinates of the sun
  
  sun(jdtdb, dls, drs, rasun, decsun, rsun())
  
  gl = dls
  
  ' compute coordinates of the planet
  
  select case ibody%
      
    case 1
      
      ' mercury(jdtdb, pl, pb, pr)
      
    case 2
      
      ' venus(jdtdb, pl, pb, pr)
      
    case 3
      
      ' sun/earth
      
      for i% = 1 to 3
        
        rph(i%) = -rsun(i%)
        
        rpg(i%) = 0.0
        
      next i%
      
      rasc = rasun
      
      decl = decsun
      
      exit sub
      
    case 4
      
      mars(jdtdb, pl, pb, pr)
      
  end select
  
  ' compute geocentric mean coordinates
  
  latlong(dls, drs, pl, pb, pr, gl, gb, rpgm)
  
  ' apparent geocentric equatorial right ascension and declination
  
  abernu(jdtdb, 4, gl, gb, alon, alat, rasc, decl)
  
  ' compute geocentric equatorial unit position vector of planet
  
  rpg(1) = rpgm * cos(rasc) * cos(decl)
  
  rpg(2) = rpgm * sin(rasc) * cos(decl)
  
  rpg(3) = rpgm * sin(decl)
  
  ' compute geocentric equatorial position vector
  
  for i% = 1 to 3
    
    rph(i%) = rpg(i%) - rsun(i%)
    
  next i%
  
end sub
  
  '''''''''''''''''''''''''''''''''''''''''''
  '''''''''''''''''''''''''''''''''''''''''''
  
sub latlong(sl, sr, pl, pb, pr, gl, gb, rpgm)
  
  ' geocentric mean coordinates subroutine
  
  ''''''''''''''''''''''''''''''''''''''''
  
  local xs, ys, xp, yp, zp, x, y, z
  
  ' heliocentric ecliptic position of the sun
  
  xs = sr * cos(sl)
  
  ys = sr * sin(sl)
  
  ' heliocentric ecliptic position of planet
  
  xp = pr * cos(pb) * cos(pl)
  
  yp = pr * cos(pb) * sin(pl)
  
  zp = pr * sin(pb)
  
  ' geocentric ecliptic position of planet
  
  x = xp + xs
  
  y = yp + ys
  
  z = zp
  
  ' mean geocentric longitude of planet (radians)
  
  gl = atan3(y, x)
  
  ' mean geocentric latitude of planet (radians)
  
  gb = atan3(z, sqr(x^2 + y^2))
  
  ' geocentric distance of planet
  
  rpgm = sqr(x * x + y * y + z * z)
  
end sub
  
  '''''''''''''''''''''''''''''''''''''''''''''''''''''''
  '''''''''''''''''''''''''''''''''''''''''''''''''''''''
  
sub abernu(jdtdb, ibody%, gl, gb, alon, alat, rasc, decl)
  
  ' aberration and nutation corrections subroutine
  
  ''''''''''''''''''''''''''''''''''''''''''''''''
  
  local u, a1, a2, dpsi, deps, epsi
  
  local xce, xse, xcl, xsl, xcb, xsb
  
  ' fundamental time argument
  
  u = (jdtdb - 2451545.0) / 3652500.0
  
  select case ibody%
      
    case 1
      
      ' mercury
      
      alon = gl + 0.0000001 * (-1261.0 + 1485.0 * cos(2.649 + 198048.273 * u))
      
      alon = alon + 0.0000001 * (305.0 * cos(5.71 + 458927.03 * u) + 230.0 * cos(5.3 + 396096.55 * u))
      
      alat = gb + 0.000019 * cos(0.42 + 260879.41 * u)
      
    case 2
      
      ' venus
      
      alon = gl + 0.0000001 * (-1304.0 + 1016.0 * cos(1.423 + 39302.097 * u))
      
      alon = alon + 0.0000001 * (224.0 * cos(2.85 + 78604.19 * u) + 98.0 * cos(4.27 + 117906.29 * u))
      
      alat = gb
      
    case 3
      
      ' earth
      
      alon = gl + 0.0000001 * (-993.0 + 17.0 * cos(3.1 + 62830.14 * u))
      
      alat = 0.0
      
    case 4
      
      ' mars
      
      alon = gl + 0.0000001 * (-1052.0 + 877.0 * cos(1.834 + 29424.634 * u))
      
      alon = alon + 0.0000001 * (187.0 * cos(3.67 + 58849.27 * u) + 84.0 * cos(3.49 + 33405.34 * u))
      
      alat = gb
      
  end select
  
  ' nutation corrections
  
  a1 = 2.18 + u * (-3375.7 + u * 0.36)
  
  a2 = 3.51 + u * (125666.39 + u * 0.1)
  
  dpsi = 0.0000001 * (-834.0 * sin(a1) - 64.0 * sin(a2))
  
  deps = 0.0000001 * u * (-226938.0 + u * (-75.0 + u * (96926.0 + u * (-2491.0 - u * 12104.0))))
  
  epsi = 0.0000001 * (4090928.0 + 446.0 * cos(a1) + 28.0 * cos(a2)) + deps
  
  alon = modulo(alon + dpsi)
  
  ' compute right ascension and declination (radians)
  
  xce = cos(epsi)
  
  xse = sin(epsi)
  
  xcl = cos(alon)
  
  xsl = sin(alon)
  
  xcb = cos(alat)
  
  xsb = sin(alat)
  
  decl = asin(xce * xsb + xse * xcb * xsl)
  
  rasc = atan3(-xse * xsb + xce * xcb * xsl, xcb * xcl)
  
end sub
    
  '''''''''''''''''''''''''
  '''''''''''''''''''''''''
  
sub mars(jdtdb, pl, pb, pr)
  
  '  computation of the heliocentric coordinates of mars
  
  ''''''''''''''''''''''''''''''''''''''''''''''''''''''
  
  local u, h1, h2, h3, w1, w3, w2
  
  local t5, t6, t7, t8, t9, t10, t11
  
  u = (jdtdb - 2451545.0) / 3652500.0
  
  ' longitude
  
  pl = 0.0
  
  for i% = 89 to 148
    
    pl = pl + cl(i%) * sin(al(i%) + bl(i%) * u)
    
  next i%
  
  pl = pl * 0.0000001 + 6.2458611 + 33408.5620646 * u
  
  h1 = 0.000001 * (186563.7 + u * (18135.0 + u * (-1332.0 + u * (-704.0 + u * (-65.0 - u * 89.0)))))
  
  t5 = 290.0 + u * 100.0
  
  w1 = 0.337967 + u * (33405.348759 + 0.000001 * u * (31676.0 - u * (7354.0 - u * (1143.0 - u * t5))))
  
  pl = modulo(pl + h1 * sin(w1))
  
  ' latitude
  
  pb = 0.0
  
  for i% = 149 to 155
    
    pb = pb + cl(i%) * sin(al(i%) + bl(i%) * u)
    
  next i%
  
  t6 = 5310.0 - u * 1050.0
  
  h1 = u * (-10277.0 + u * (24272.0 + u * (-2420.0 + u * (-10850.0 + u * (3880.0 + u * t6)))))
  
  h1 = 0.0000001 * (319714.0 + h1)
  
  w1 = u * (0.048 + u * (-0.04831 + u * (0.01402 + u * (0.029 + u * (-0.0073 - u * 0.0112)))))
  
  w1 = 5.339102 + u * (33407.21879 + w1)
  
  t7 = 220.0 + u * 270.0
  
  h2 = 0.0000001 * (29803.0 + u * (1904.0 + u * (1865.0 + u * (-60.0 + u * (-950.0 + u * t7)))))
  
  w2 = 5.67694 + u * (66812.5668 + u * (0.08030001 + u * (-0.0536.0 + u * (0.0147 + u * 0.028))))
  
  h3 = 0.0000001 * (3137.0 + u * (472.0 + u * (111.0 + u * 70.0)))
  
  w3 = 6.0173 + u * (100217.928 + u * (0.093 + u * (-8.6e-02 + u * 0.037)))
  
  pb = pb * 0.0000001 + h1 * sin(w1) + h2 * sin(w2) + h3 * sin(w3)
  
  ' radius
  
  pr = 0.0
  
  for i% = 156 to 184
    
    pr = pr + cl(i%) * cos(al(i%) + bl(i%) * u)
    
  next i%
  
  pr = pr * 0.0000001 + 1.529856
  
  t8 = -153.0 - u * 73.0
  
  h1 = 0.000001 * (141849.5 + u * (13651.8 + u * (-1230.0 + u * (-378.0 + u * (187.0 + u * t8)))))
  
  t9 = 83.0 - u * 48.0
  
  w1 = 3.479698 + u * (33405.34956 + 0.00001 * (u * (3066.9 + u * (-909.0 + u * (223.0 + u * t9)))))
  
  t10 = -12.0 + u * 99.0
  
  h2 = 0.000001 * (6607.8 + u * (1272.8 + u * (-53.0 + u * (-46.0 + u * (14.0 + u * t10)))))
  
  t11 = 0.0012 + u * 0.002
  
  w2 = 3.81781 + u * (66810.6991 + u * (0.0613 + u * (-0.0182 + u * (0.0044 + u * t11))))
  
  pr = pr + h1 * cos(w1) + h2 * cos(w2)
  
end sub
  
  ''''''''''''''''''''''''''''''''''''''''
  ''''''''''''''''''''''''''''''''''''''''
  
sub sun(jdtdb, dl, dr, rasc, decl, rsun())
  
  ' precision ephemeris of the Sun
  
  ' input
  
  '  jdtdb = julian ephemeris day
  
  ' output
  
  '  dl     = ecliptic longitude of the sun (radians)
  '           (0 <= dl <= 2 pi)
  '  dr     = geocentric distance of the sun (AU)
  '  rasc   = right ascension of the Sun (radians)
  '           (0 <= rasc <= 2 pi)
  '  decl   = declination of the Sun (radians)
  '           (-pi/2 <= decl <= pi/2)
  '  rsun() = geocentric position vector of the sun
  
  '''''''''''''''''''''''''''''''''''''''''''''''''
  
  local u, a1, a2, psi, deps, meps, eps, seps, ceps
  
  local w, srl, crl, srb, crb, sra, cra
  
  u = (jdtdb - 2451545.0) / 3652500.0
  
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
    
    w = sa(i%) + sb(i%) * u
    
    dl = dl + sl(i%) * sin(w)
    
    if (sr(i%) <> 0.0) then
      
      dr = dr + sr(i%) * cos(w)
      
    end if
    
  next i%
  
  dl = modulo(dl * 0.0000001 + 4.9353929 + 62833.196168 * u)
  
  dr = dr * 0.0000001 + 1.0001026
  
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
  
  ''''''''''''''''''''''''''''''''
  ''''''''''''''''''''''''''''''''
  
sub gdate (jday, month, day, year)
  
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

  ''''''''''''''''''''''''
  ''''''''''''''''''''''''
  
sub tdb2utc (jdtdb, jdutc)
  
  ' convert TDB julian day to UTC julian day subroutine
  
  ' input
  
  '  jdtdb = TDB julian day
  
  ' output
  
  '  jdutc = UTC julian day
  
  '''''''''''''''''''''''''
  
  local x1, x2, xroot, froot
  
  jdsaved = jdtdb
  
  ' set lower and upper bounds
  
  x1 = jdsaved - 0.1
  
  x2 = jdsaved + 0.1
  
  ' solve for UTC julian day using Brent's method
  
  realroot(x1, x2, 1.0e-8, xroot, froot)
  
  jdutc = xroot
  
end sub
  
  '''''''''''''''''''
  '''''''''''''''''''
  
sub jdfunc (jdin, fx)
  
  ' objective function for tdb2utc
  
  ' input
  
  '  jdin = current value for UTC julian day
  
  ' output
  
  '  fx = delta julian day
  
  ''''''''''''''''''''''''
  
  local jdwrk
  
  utc2tdb(jdin, jdwrk)
  
  fx = jdwrk - jdsaved
  
end sub
  
  '''''''''''''''
  '''''''''''''''
  
sub jd2str(jdutc)
  
  ' convert julian day to calendar date and UTC time
  
  ''''''''''''''''''''''''''''''''''''''''''''''''''
  
  gdate (jdutc, cmonth, day, year)
  
  print "calendar date  ", month$(cmonth); " ", STR$(int(day)); " "; str$(year)
  
  print " "
  
  thr0 = 24.0 * (day - int(day))
  
  thr = int(thr0)
  
  tmin0 = 60.0 * (thr0 - thr)
  
  tmin = int(tmin0)
  
  tsec = 60.0 * (tmin0 - tmin)
  
  ' fix seconds and minutes for rollover
  
  if (tsec >= 60.0) then
    
    tsec = 0.0
    
    tmin = tmin + 1.0
    
  end if
  
  ' fix minutes for rollover
  
  if (tmin >= 60.0) then
    
    tmin = 0.0
    
    thr = thr + 1.0
    
  end if
  
  print "UTC time       ", str$(thr) + " hours " + str$(tmin) + " minutes " + str$(tsec, 0, 2) + " seconds"
  
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
  
  for i% = 1 to 27
    
    if (jday >= jdleap(i%) and jday < jdleap(i% + 1)) then
      
      leapsecond = leapsec(i%)
      
      exit sub
      
    end if
    
  next i%
  
end sub

  
  '''''''''''''''''''''''''
  '''''''''''''''''''''''''
  
function modulo(x) as float
  
  ' modulo 2 pi function
  
  ''''''''''''''''''''''
  
  local a
  
  a = x - pi2 * fix(x / pi2)
  
  if (a < 0.0) then
    
    a = a + pi2
    
  end if
  
  modulo = a
  
end function
  
  '''''''''''''''''''''''''''
  '''''''''''''''''''''''''''
  
function atan3(a, b) as float
  
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
  
  '''''''''''
  '''''''''''
  
sub read_data
  
  ' read sun and planet data subroutine
  
  '''''''''''''''''''''''''''''''''''''
  
  for i% = 1 to 50
    
    read sl(i%), sr(i%), sa(i%), sb(i%)
    
  next i%
  
  for i% = 1 to 184
    
    read cl(i%), al(i%), bl(i%)
    
  next i%
  
  ' data for the sun - longitude and radius vector
  
  data 403406,      0, 4.721964,      1.621043
  data 195207, -97597, 5.937458,  62830.348067
  data 119433, -59715, 1.115589,  62830.821524
  data 112392, -56188, 5.781616,  62829.634302
  data   3891,  -1556, 5.5474  , 125660.5691
  data   2819,  -1126, 1.5120  , 125660.9845
  data   1721,   -861, 4.1897  ,  62832.4766
  data      0,    941, 1.163   ,      0.813
  data    660,   -264, 5.415   , 125659.310
  data    350,   -163, 4.315   ,  57533.850
  data    334,      0, 4.553   ,    -33.931
  data    314,    309, 5.198   , 777137.715
  data    268,   -158, 5.989   ,  78604.191
  data    242,      0, 2.911   ,      5.412
  data    234,    -54, 1.423   ,  39302.098
  data    158,      0, 0.061   ,    -34.861
  data    132,    -93, 2.317   , 115067.698
  data    129,    -20, 3.193   ,  15774.337
  data    114,      0, 2.828   ,   5296.670
  data     99,    -47, 0.52    ,  58849.27
  data     93,      0, 4.65    ,   5296.11
  data     86,      0, 4.35    ,  -3980.70
  data     78,    -33, 2.75    ,  52237.69
  data     72,    -32, 4.50    ,  55076.47
  data     68,      0, 3.23    ,    261.08
  data     64,    -10, 1.22    ,  15773.85
  data     46,    -16, 0.14    , 188491.03
  data     38,      0, 3.44    ,  -7756.55
  data     37,      0, 4.37    ,    264.89
  data     32,    -24, 1.14    , 117906.27
  data     29,    -13, 2.84    ,  55075.75
  data     28,      0, 5.96    ,  -7961.39
  data     27,     -9, 5.09    , 188489.81
  data     27,      0, 1.72    ,   2132.19
  data     25,    -17, 2.56    , 109771.03
  data     24,    -11, 1.92    ,  54868.56
  data     21,      0, 0.09    ,  25443.93
  data     21,     31, 5.98    , -55731.43
  data     20,    -10, 4.03    ,  60697.74
  data     18,      0, 4.27    ,   2132.79
  data     17,    -12, 0.79    , 109771.63
  data     14,      0, 4.24    ,  -7752.82
  data     13,     -5, 2.01    , 188491.91
  data     13,      0, 2.65    ,    207.81
  data     13,      0, 4.98    ,  29424.63
  data     12,      0, 0.93    ,     -7.99
  data     10,      0, 2.21    ,  46941.14
  data     10,      0, 3.59    ,    -68.29
  data     10,      0, 1.50    ,  21463.25
  data     10,     -9, 2.55    , 157208.40
  
  ' data for mercury - heliocentric longitude, latitude and radius vector
  
  data 510728, 6.09670 ,  521757.52364 , 404847, 4.72189,       1.62027
  data  91048, 2.8946  ,  782636.2744  ,  30594, 4.1535 ,  521758.6270
  data  15769, 5.8003  , 1043515.073   ,  13726, 0.4656 ,  521756.9570
  data  11582, 1.0266  ,  782637.2016  ,   7633, 3.517  ,  521759.335
  data   5247, 0.418   ,  782638.007   ,   4001, 3.993  , 1043516.352
  data   3299, 2.791   , 1304393.680   ,   3212, 0.209  , 1043514.724
  data   1690, 2.067   , 1304394.627   ,   1482, 6.174  , 1304395.168
  data   1233, 3.606   ,  782635.409   ,   1152, 5.856  , 1565272.646
  data    845, 2.63    , 1043516.88    ,    654, 3.40   , 1565273.50
  data    359, 2.66    , 1826151.56    ,    356, 3.08   ,   11094.77
  data    257, 6.27    , 1826152.20    ,    246, 2.89   ,       5.41
  data    180, 5.67    ,   56613.61    ,    159, 4.57   ,  250285.49
  data    137, 6.17    ,  271973.50
  data 680303, 3.82625 ,  260879.17693 , 538354, 3.30009,  260879.66625
  data 176935, 3.74070 ,       0.40005 , 143323, 0.58073,  521757.92658
  data 105214, 0.0545  ,  521758.44880 ,  91011, 3.3915 ,       0.9954
  data  47427, 1.9266  ,  260878.2610  ,  41669, 3.5084 ,  782636.7624
  data  19826, 3.1539  ,  782637.4813  ,  12963, 0.2455 , 1043515.6610
  data   8233, 4.886   ,  521756.972   ,   6399, 0.358  ,  782637.769
  data   3196, 3.253   , 1304394.380   ,   1536, 4.824  , 1043516.451
  data    824, 0.04    , 1565273.15    ,    819, 1.84   ,  782635.45
  data    324, 1.60    , 1304395.53    ,    201, 2.92   , 1826151.86
  data 780141, 6.202782,  260878.753962,  78942, 2.98062,  521757.50830
  data  12000, 6.0391  ,  782636.2640  ,   9839, 4.8422 ,  260879.3808
  data   2355, 5.062   ,       0.734   ,   2019, 2.898  , 1043514.987
  data   1974, 1.588   ,  521758.140   ,   1859, 0.805  ,  260877.716
  data    426, 4.601   ,  782636.915   ,    397, 5.976  , 1304393.735
  data    382, 3.86    ,  521756.47    ,    306, 1.87   , 1043515.34
  data    102, 0.62    ,  782635.28    ,     92, 2.60   , 1565272.52
  
  ' data for venus - heliocentric longitude, latitude and radius vector
  
  data 423015, 4.722173,      1.600752, 548  , 5.987  ,  78604.195
  data    346, 4.27    , 117906.29    , 253  , 2.95   ,      5.37
  data    237, 4.56    ,  39302.10    , 181  , 0.05   ,  15774.33
  data    153, 2.14    , 306400.25    , 144  , 5.73   ,  96835.94
  data     99, 0.09    ,    261.08    ,  98  , 6.18   , 306399.50
  data     89, 4.34    ,  15773.85    ,  85  , 2.86   ,  94378.51
  data     69, 2.85    ,   5296.67    ,  56  , 5.71   ,   1915.95
  data     55, 1.23    ,    264.89    ,  55  , 2.85   ,   7756.55
  data     50, 5.69    , 157208.38    ,  48  , 4.62   ,   5296.12
  data     43, 5.16    , 193671.89    ,  39  , 0.85   ,  94377.98
  data 590350, 1.759897, 102133.735253, 34737, 3.17478, 102133.01934
  data  13104, 0.2705  ,      2.0678  , 12910, 3.7446 , 102134.2721
  data   8591, 3.7878  ,      1.5631  ,  7015, 3.3730 ,      2.2248
  data   2101, 2.828   ,      0.361   ,   163, 2.85   ,  78604.20
  data    138, 1.13    , 117906.29    ,    50, 2.59   ,  96835.94
  data     37, 1.42    ,  39302.10
  
  ' data for mars - heliocentric longitude, latitude and radius vector
  
  data 424067, 4.725053,     1.599646 , 117053, 0.92177,  66810.71641
  data  31286, 5.17451 ,  66811.55202 ,  10248, 1.3885 , 100216.0920
  data   4933, 1.4302  ,  66813.4604  ,   4130, 5.4976 , 100216.8648
  data   2605, 1.382   ,    -33.896   ,   1334, 3.104  ,    -34.791
  data   1232, 2.420   ,  28109.218   ,   1180, 2.148  , 133621.421
  data    959, 4.404   ,  22813.140   ,    827, 6.053  , 133621.745
  data    778, 4.910   ,  56218.431   ,    692, 1.881  , 100218.894
  data    667, 1.267   ,  -3980.684   ,    416, 1.799  ,  29424.634
  data    354, 3.146   ,  25443.942   ,    341, 1.005  ,  66815.503
  data    339, 1.420   ,  33371.351   ,    310, 0.916  , -33439.371
  data    294, 2.57    ,   1915.95    ,    260, 2.69   ,  -7961.45
  data    245, 3.70    ,  33370.81    ,    242, 2.94   ,      5.41
  data    198, 2.78    ,   5296.69    ,    182, 3.09   , -33440.09
  data    167, 4.28    ,  17516.29    ,    153, 2.72   ,  61514.60
  data    148, 5.27    ,  22812.71    ,    141, 4.51   ,  21463.18
  data    128, 1.06    , 100220.45    ,    126, 5.19   ,  50921.57
  data    105, 0.77    ,   5296.06    ,    103, 5.26   ,  89623.78
  data     86, 3.97    ,  29140.97    ,     81, 5.38   ,   1559.53
  data     72, 0.90    , -37386.05    ,     71, 5.23   ,  10592.20
  data     66, 2.06    ,  31273.21    ,     64, 4.49   ,  84327.58
  data     63, 1.43    , 167027.36    ,     60, 1.77   , 133625.34
  data     55, 4.02    ,     -7.37    ,     55, 6.05   ,  17482.57
  data     47, 4.23    , -11942.07    ,     45, 1.23   ,   1914.57
  data     43, 6.24    ,  -3981.49    ,     43, 2.12   ,  62829.99
  data     42, 2.07    ,   2131.89    ,     38, 4.90   ,  -7962.27
  data     36, 0.02    ,  17514.95    ,     33, 2.37   , 167026.02
  data     31, 4.65    ,  25443.27    ,     31, 2.57   ,  66776.76
  data     30, 1.19    , -66844.78    ,     30, 6.06   ,  35321.38
  data     30, 4.35    ,  62546.28    ,     29, 5.79   ,    207.81
  data     28, 1.17    ,  13501.91    ,     28, 2.09   , -31489.48
  data  32962, 1.67255 ,   1.77247    ,   9705, 3.5531 ,    0.9198
  data    367, 0.353   , 133623.307   ,    101, 4.74   , 133624.11
  data     44, 0.94    ,  33373.21    ,     44, 2.83   , -33441.23
  data     40, 0.42    , 167028.67
  data  27946, 4.8846  ,    0.4677    ,   5147, 4.5968 , 100216.0535
  data   2196, 2.568   , 100216.663   ,    811, 5.560  ,  28109.218
  data    749, 1.772   ,  56218.430   ,    559, 5.112  , 133621.486
  data    503, 1.272   ,  22813.139   ,    332, 2.701  , 133621.941
  data    258, 4.56    ,  33371.35    ,    248, 4.93   ,  29424.63
  data    236, 0.92    , -33439.37    ,    231, 0.09   ,  25443.93
  data    186, 0.56    ,  33370.81    ,    138, 3.09   , -33440.09
  data    117, 2.11    ,  50921.57    ,    110, 1.27   ,  -3980.70
  data     99, 5.84    ,  61514.59    ,     90, 4.41   ,   5296.11
  data     90, 5.64    , 167026.95    ,     81, 2.10   ,  10592.24
  data     80, 2.83    ,  -7961.39    ,     74, 1.50   ,  21463.25
  data     73, 1.25    ,  84327.62    ,     71, 2.86   , 167027.21
  data     69, 2.10    ,  22812.70    ,     69, 2.13   ,  89623.77
  data     63, 1.29    ,  17516.40    ,     57, 0.83   ,  29140.97
  data     53, 0.90    , -37386.04
  
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
  
  ' read leap second data
  
  for i% = 1 to 28
    
    read jdleap(i%), leapsec(i%)
    
  next i%
  
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
  
end sub
  
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
  
  ''''''''''''''''''''''''''''
  ''''''''''''''''''''''''''''
  
sub getdate (month, day, year)
  
  ' request calendar date subroutine
  
  do
    print " "
    print "please input the calendar date"
    print " "
    print "(month [1 - 12], day [1 - 31], year [yyyy])"
    print "< for example, october 21, 1986 is input as 10,21,1986 >"
    print "< b.c. dates are negative, a.d. dates are positive >"
    print "< the day of the month may also include a decimal part >"
    print " "
    input month, day, year
    
  loop until (month >= 1 and month <= 12) and (day >= 1 and day <= 31)
  
end sub
  
  ''''''''''''''''''''''''''
  ''''''''''''''''''''''''''
  
sub getutc (thr, tmin, tsec)
  
  ' request time subroutine
  
  do
    print " "
    print "please input the UTC time"
    print " "
    print "(hours [0 - 24], minutes [0 - 60], seconds [0 - 60])"
    print " "
    input thr, tmin, tsec
    
  loop until ((thr >= 0.0 and thr <= 24.0) and (tmin >= 0.0 and tmin <= 60.0) and (tsec >= 0.0 and tsec <= 60.0))
  
end sub