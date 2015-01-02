unit SVGPathArc;
(*
    Original code is svg_extras.py from "Enable" library.
    https://svn.enthought.com/enthought/browser/Enable/trunk/enthought/savage/svg/svg_extras.py?rev=21150

    Translated to pascal by x2nie at yahoo dot com
    31 December 2014
*)


interface

  uses GR32,  GR32_Paths;

procedure elliptical_arc_to(path: TFlattenedPath; rx, ry, phi : TFloat;
      large_arc_flag, sweep_flag: Boolean;
      x1, y1, x2, y2: TFloat);

implementation

  uses Math;

procedure minest(var a,b : TFloat);
var c : TFloat;
begin
  if a > b then
  begin
    c := b;
    b := a;
    a := c;
  end;
end;

procedure maxest(var a,b : TFloat);
var c : TFloat;
begin
  if b < a then
  begin
    c := b;
    b := a;
    a := c;
  end;
end;

procedure append(var dst : TArrayOfArrayOfFloatPoint; src : array of TFloat);
var i, l, first : Integer;
begin
  l := Length(src) div 2;
  first := Length(dst);
  SetLength(dst, first+1);
  SetLength(dst[first], l);


  for i := 0 to l -1 do
  begin
     dst[first][i] := FloatPoint(src[i*2], src[i*2+1]);
  end;

end;

function bezier_arc(x1,y1, x2,y2: TFloat; start_angle:TFloat=0; extent:TFloat=90) : TArrayOfArrayOfFloatPoint;
    {Compute a cubic Bezier approximation of an elliptical arc.

    (x1, y1) and (x2, y2) are the corners of the enclosing rectangle.  The
    coordinate system has coordinates that increase to the right and down.
    Angles, measured in degress, start with 0 to the right (the positive X axis)
    and increase counter-clockwise.  The arc extends from start_angle to
    start_angle+extent.  I.e. start_angle=0 and extent=180 yields an openside-down
    semi-circle.

    The resulting coordinates are of the form (x1,y1, x2,y2, x3,y3, x4,y4)
    such that the curve goes from (x1, y1) to (x4, y4) with (x2, y2) and
    (x3, y3) as their respective Bezier control points.
    }
var
  frag_angle,x_cen,y_cen : TFloat;
  rx,ry,half_angle,kappa,sign : TFloat;
    theta0, theta1, c0,c1, s0,s1, signed_kappa : TFloat;
  nfrag,i : Integer;
  mi,ma : TFloatPoint;
begin
  SetLength(Result, 0);

    //x1,y1, x2,y2 = min(x1,x2), max(y1,y2), max(x1,x2), min(y1,y2)
    minest(x1,x2); minest(y2,y1);


    if abs(extent) <= 90 then
    begin
        frag_angle := extent;
        nfrag := 1;
    end
    else
    begin
        nfrag := ceil(abs(extent)/90);
        if nfrag = 0 then
        begin
            //warnings.warn('Invalid value for extent: %r' % extent)
            Exit;
        end;
        frag_angle := extent / nfrag;
    end;

    x_cen := (x1+x2)/2;
    y_cen := (y1+y2)/2;
    rx := (x2-x1)/2;
    ry := (y2-y1)/2;
    half_angle := DegToRad(frag_angle) / 2;
    kappa := abs(4 / 3 * (1 - cos(half_angle)) / sin(half_angle));

    if frag_angle < 0 then
        sign := -1
    else
        sign := 1;

    //point_list = []


    for i := 0 to nfrag -1 do
    begin
        theta0 := DegToRad(start_angle + i*frag_angle);
        theta1 := DegToRad(start_angle + (i+1)*frag_angle);
        c0 := cos(theta0);
        c1 := cos(theta1);
        s0 := sin(theta0);
        s1 := sin(theta1);
        if frag_angle > 0 then
            signed_kappa := -kappa
        else
            signed_kappa := kappa;
        //point_list.append((
        append(Result,    [
                          x_cen + rx * c0,
                          y_cen - ry * s0,
                          x_cen + rx * (c0 + signed_kappa * s0),
                          y_cen - ry * (s0 - signed_kappa * c0),
                          x_cen + rx * (c1 - signed_kappa * s1),
                          y_cen - ry * (s1 + signed_kappa * c1),
                          x_cen + rx * c1,
                          y_cen - ry * s1
                          ]
                          //))
        );
                          
    end;
    sign := 1;
    //return point_list
end;



function angle(x1,y1, x2,y2: TFloat): tfloat;
    //The angle in degrees between two vectors.
var
  sign, usign, num, den, ratio : TFloat;
begin
    sign := 1.0;
    usign := (x1*y2 - y1*x2);
    if usign < 0 then
        sign := -1.0;
    num := x1*x2 + y1*y2;
    den := hypot(x1,y1) * hypot(x2,y2);
    ratio := min(max(num/den, -1.0), 1.0);
    Result := sign * RadToDeg(ArcCos(ratio));
end;

function transform_from_local(xp,yp,cphi,sphi,mx,my : TFloat):  TFloatPoint; overload;
    //Transform from the local frame to absolute space.
var
  x,y : TFloat;
begin
    x := xp * cphi - yp * sphi + mx;
    y := xp * sphi + yp * cphi + my;
    Result := FloatPoint(x,y);
end;

function transform_from_local(p: TFloatPoint; cphi,sphi,mx,my : TFloat):  TFloatPoint; overload;
begin
  Result := transform_from_local(p.X, p.Y, cphi,sphi,mx,my );
end;

procedure elliptical_arc_to(path: TFlattenedPath; rx, ry, phi : TFloat;
      large_arc_flag, sweep_flag: Boolean;
      x1, y1, x2, y2: TFloat);

      
    {Add an elliptical arc to the kiva CompiledPath by approximating it with
    Bezier curves or a line segment.

    Algorithm taken from the SVG 1.1 Implementation Notes:
        http://www.w3.org/TR/SVG/implnote.html#ArcImplementationNotes
    }
var
  rphi, cphi,sphi, dx,dy, x1p,y1p,lam,scale,num,den,cxp,cyp, mx,my, a : TFloat;
  dx2, dy2, dtheta,theta1 : TFloat;
  arcs, args : TArrayOfFloatPoint;
  CARRAY,control_points : TArrayOfArrayOfFloatPoint;
  xp : TArrayOfFloatPoint;
  p1,p2,p3,p4 : TFloatPoint;
  i : integer;
begin
    // Basic normalization.
    rx := abs(rx);
    ry := abs(ry);
    while phi > 360 do
      phi := phi - 360;
    while phi < -360 do
      phi := phi + 360;

    // Check for certain special cases.
    if (x1=x2) and (y1=y2) then
    begin
        // Omit the arc.
        // x1 and y1 can obviously remain the same for the next segment.
        //return []
        exit;
    end;
    if (rx = 0) or (ry = 0) then
    begin
        // Line segment.
        Path.LineTo(x2,y2);
        //return []
        Exit;
    end;



    rphi := DegToRad(phi);
    cphi := cos(rphi);
    sphi := sin(rphi);

    // Step 1: Rotate to the local coordinates.
    dx := 0.5*(x1 - x2);
    dy := 0.5*(y1 - y2);
    x1p :=  cphi * dx + sphi * dy;
    y1p := -sphi * dx + cphi * dy;
    // Ensure that rx and ry are large enough to have a unique solution.
    lam := Sqr(x1p/rx) + Sqr(y1p/ry);
    if lam > 1.0 then
    begin
        scale := sqrt(lam);
        rx := rx * scale;
        ry := ry *scale;
    end;

    // Step 2: Solve for the center in the local coordinates.
    num := max(Sqr(rx*ry) - Sqr(rx*y1p) - Sqr(ry*x1p), 0.0);
    den := (Sqr(rx*y1p) + Sqr(ry*x1p));
    a := sqrt(num / den);
    cxp := a * rx*y1p/ry;
    cyp := -a * ry*x1p/rx;
    if large_arc_flag = sweep_flag then
    begin
        cxp := -cxp;
        cyp := -cyp;
    end;

    // Step 3: Transform back.
    mx := 0.5*(x1+x2);
    my := 0.5*(y1+y2);

    // Step 4: Compute the start angle and the angular extent of the arc.
    // Note that theta1 is local to the phi-rotated coordinate space.
    dx := (x1p-cxp) / rx;
    dy := (y1p-cyp) / ry;
    dx2 := (-x1p-cxp) / rx;
    dy2 := (-y1p-cyp) / ry;
    theta1 := angle(1,0,dx,dy);
    dtheta := angle(dx,dy,dx2,dy2);
    if (not sweep_flag) and (dtheta > 0) then
        dtheta := dtheta - 360
    else if sweep_flag and (dtheta < 0) then
        dtheta := dtheta + 360;



    // Step 5: Break it apart into Bezier arcs.
    SetLength(arcs,0);
    control_points := bezier_arc(cxp-rx,cyp-ry,cxp+rx,cyp+ry, theta1, dtheta);

    SetLength(CARRAY,0);

    //for x1p,y1p, x2p,y2p, x3p,y3p, x4p,y4p in control_points:
    for i := 0 to Length(control_points) -1 do
    begin
      xp := control_points[i];
        // Transform them back to asbolute space.

        p2 := transform_from_local(xp[1],cphi,sphi,mx,my);
        p3 := transform_from_local(xp[2],cphi,sphi,mx,my);
        p4 := transform_from_local(xp[3],cphi,sphi,mx,my);

        //arcs.append(args)
        Path.CurveTo(p2,p3,p4);
        append(CARRAY, [p2.X,p2.Y,  p3.X,p3.Y,  p4.X,p4.Y] );
    end;
end;    

  

end.
