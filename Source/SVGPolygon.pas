unit SVGPolygon;

interface
uses
  GR32, GR32_Polygons, GR32_Transforms;

  
procedure SVGPolyPolylineFS(Bitmap: TBitmap32; const Points: TArrayOfArrayOfFloatPoint;
  Filler: TCustomPolygonFiller; Closed: Boolean = False; StrokeWidth: TFloat = 1.0;
  JoinStyle: TJoinStyle = jsMiter; EndStyle: TEndStyle = esButt;
  MiterLimit: TFloat = 4.0; Transformation: TTransformation = nil);




implementation

uses
  Math, GR32_Math, GR32_Geometry, GR32_LowLevel;

const
  DEFAULT_MITER_LIMIT = 4.0;
  DEFAULT_MITER_LIMIT_FIXED = $40000;
  TWOPI = 2 * Pi;

  

function Grow(const Points: TArrayOfFloatPoint; const Normals: TArrayOfFloatPoint;
  const Delta: TFloat; JoinStyle: TJoinStyle; Closed: Boolean; MiterLimit: TFloat): TArrayOfFloatPoint; overload;
const
  BUFFSIZEINCREMENT = 128;
  MINDISTPIXEL = 1.414; // just a little bit smaller than sqrt(2),
  // -> set to about 2.5 for a similar output with the previous version
var
  ResSize, BuffSize: Integer;
  PX, PY: TFloat;
  AngleInv, RMin: TFloat;
  A, B, Dm: TFloatPoint;
  LastIsCusp : Boolean;

  procedure AddPoint(const LongDeltaX, LongDeltaY: TFloat);
  begin
    if ResSize = BuffSize then
    begin
      Inc(BuffSize, BUFFSIZEINCREMENT);
      SetLength(Result, BuffSize);
    end;
    Result[ResSize] := FloatPoint(PX + LongDeltaX, PY + LongDeltaY);
    Inc(ResSize);
  end;


  {procedure AddMiddle(const X1, Y1, X2, Y2: TFloat);
  var
    M: TFloatPoint;
  begin
    if Intersect(FloatPoint(X1,Y1), FloatPoint(X2,Y2), FloatPoint(X1,Y2), FloatPoint(X2, Y1), M) then
      AddPoint(M.X - Min(X1,X2), M.Y - Min(Y1,Y2));

  end;}
    
  procedure AddMitered(const X1, Y1, X2, Y2: TFloat);
  var
    R, R2, R3, CX, CY: TFloat;
  begin
    CX := X1 + X2;
    CY := Y1 + Y2;

    R := X1 * CX + Y1 * CY; //(1 - cos(ß))  (range: 0 <= R <= 2)
    if R < RMin then
    begin
      AddPoint(Delta * X1, Delta * Y1);
      AddPoint(Delta * X2, Delta * Y2);
    end
    else
    {if Delta > 0 then
      AddMiddle(X1, Y1, X2, Y2) //x2nie
    else}
    begin
      R := Delta / R;
      //if R > Delta then

      R2 := X1 * Y2 - X2 * Y1;
      R3 := X2 * Y1 - X1 * Y2;      
      if (R2 * Delta <= 0) {or ((Delta < 0) and ( (R3 * Delta <= 0)))} then
      begin
        //concave
        AddPoint(Delta * X1, Delta * Y1);
        AddPoint(Delta * X2, Delta * Y2);
        exit;
      end;

      {else
      begin
        if LastIsCusp then
        begin
          //AddMiddle(X1, Y1, X2, Y2); //x2nie
          LastIsCusp := False;
          //Exit;
        end
      end;}
      //
      AddPoint(CX * R, CY * R);
    end;
  end;

  procedure AddBevelled(const X1, Y1, X2, Y2: TFloat);
  var
    R: TFloat;
  begin
    R := X1 * Y2 - X2 * Y1; //cross product
    if R * Delta <= 0 then      //ie angle is concave
    begin
      AddMitered(X1, Y1, X2, Y2);
    end
    else
    begin
      AddPoint(Delta * X1, Delta * Y1);
      AddPoint(Delta * X2, Delta * Y2);
    end;
  end;


  procedure AddRoundedJoin(const X1, Y1, X2, Y2: TFloat);
  var
    R, tmp, da, half: TFloat;
    ArcLen: Integer;

    I: Integer;
    C: TFloatPoint;
  begin
    {if (Delta < 0) then
      R := X2 * Y1 - X1 * Y2
    else}
      R := X1 * Y2 - X2 * Y1;
    if (R * Delta <= 0) and (Delta < 0) then
    begin
      // check cross round
      {if (Delta > 0) and ( ((x1 < x2) and (A.X > B.X)) or ((Y1 < Y2) and (A.Y > B.Y)) ) then
        AddMiddle(X1, Y1, X2, Y2)
      else}
      //if Delta < 0 then
        AddMitered(X1, Y1, X2, Y2)
      //AddBevelled(X1, Y1, X2, Y2)
      //else AddMiddle(X1, Y1, X2, Y2)
    end
    else
    begin
      if R < 0 then
        Dm.Y := -Abs(Dm.Y)
      else
        Dm.Y := Abs(Dm.Y);

      half := 0.5;
      tmp := 1 - half * (Sqr(X2 - X1) + Sqr(Y2 - Y1));
      da := half * Pi - tmp * (1 + Sqr(tmp) * 0.1667); // should be ArcCos(tmp);
      ArcLen := Ceil(Abs(da * AngleInv)); // should be trunc instead of round

      C.X := X1 * Delta;
      C.Y := Y1 * Delta;
      AddPoint(C.X, C.Y);
      for I := 1 to ArcLen - 1 do
      begin
        C := FloatPoint(C.X * Dm.X - C.Y * Dm.Y, C.Y * Dm.X + C.X * Dm.Y);
        AddPoint(C.X, C.Y);
      end;

      C.X := X2 * Delta;
      C.Y := Y2 * Delta;
      AddPoint(C.X, C.Y);
    end;
  end;

  procedure AddJoin(const X, Y, X1, Y1, X2, Y2: TFloat);
  begin
    PX := X;
    PY := Y;
    case JoinStyle of
      jsMiter: AddMitered(A.X, A.Y, B.X, B.Y);
      jsBevel: AddBevelled(A.X, A.Y, B.X, B.Y);
      jsRound: AddRoundedJoin(A.X, A.Y, B.X, B.Y);
    end;
  end;

var
  I, L, H: Integer;

begin
  Result := nil;

  if Length(Points) <= 1 then Exit;

  //MiterLimit = Sqrt(2/(1 - cos(ß)))
  //Sqr(MiterLimit) = 2/(1 - cos(ß))
  //1 - cos(ß) = 2/Sqr(MiterLimit) = RMin;
  RMin := 2 / Sqr(MiterLimit);

  H := High(Points) - Ord(not Closed);
  while (H >= 0) and (Normals[H].X = 0) and (Normals[H].Y = 0) do Dec(H);

{** all normals zeroed => Exit }
  if H < 0 then Exit;

  L := 0;
  while (Normals[L].X = 0) and (Normals[L].Y = 0) do Inc(L);

  if Closed then
    A := Normals[H]
  else
    A := Normals[L];

  ResSize := 0;
  BuffSize := BUFFSIZEINCREMENT;
  SetLength(Result, BuffSize);

  // prepare
  if JoinStyle = jsRound then
  begin
    Dm.X := 1 - 0.5 * Min(3, Sqr(MINDISTPIXEL / Abs(Delta)));
    Dm.Y := Sqrt(1 - Sqr(Dm.X));
    AngleInv := 1 / ArcCos(Dm.X);
  end;

  LastIsCusp := False;

  for I := L to H do
  begin
    B := Normals[I];
    if (B.X = 0) and (B.Y = 0) then Continue;

    with Points[I] do AddJoin(X, Y, A.X, A.Y, B.X, B.Y);
    A := B;
  end;
  if not Closed then
    with Points[High(Points)] do AddJoin(X, Y, A.X, A.Y, A.X, A.Y);

  SetLength(Result, ResSize);
end;
function BuildNormals(const Points: TArrayOfFloatPoint): TArrayOfFloatPoint;
const
  EPSILON = 1E-4;
var
  I, Count, NextI: Integer;
  dx, dy, f: Double;
begin
  Count := Length(Points);
  SetLength(Result, Count);

  I := 0;
  NextI := 1;

  while I < Count do
  begin
    if NextI >= Count then NextI := 0;

    dx := Points[NextI].X - Points[I].X;
    dy := Points[NextI].Y - Points[I].Y;
    f := GR32_Math.Hypot(dx, dy);
    if (f > EPSILON) then
    begin
      f := 1 / f;
      dx := dx * f;
      dy := dy * f;
    end;

    Result[I].X := dy;
    Result[I].Y := -dx;

    Inc(I);
    Inc(NextI);
  end;
end;

function ReversePolygon(const Points: TArrayOfFloatPoint): TArrayOfFloatPoint;
var
  I, L: Integer;
begin
  L := Length(Points);
  SetLength(Result, L);
  Dec(L);
  for I := 0 to L do
    Result[I] := Points[L - I];
end;

function BuildArc(const P: TFloatPoint; StartAngle, EndAngle, Radius: TFloat;
  Steps: Integer): TArrayOfFloatPoint; overload;
var
  I: Integer;
  C, D: TFloatPoint;
begin
  SetLength(Result, Steps);
  SinCos(StartAngle, Radius, C.Y, C.X);
  Result[0] := OffsetPoint(P, C);

  GR32_Math.SinCos((EndAngle - StartAngle) / (Steps - 1), D.Y, D.X);
  for I := 1 to Steps - 1 do
  begin
    C := FloatPoint(C.X * D.X - C.Y * D.Y, C.Y * D.X + C.X * D.Y);
    Result[I] := OffsetPoint(P, C);
  end;
end;

function BuildArc(const P: TFloatPoint; StartAngle, EndAngle, Radius: TFloat): TArrayOfFloatPoint; overload;
const
  MINSTEPS = 6;
  SQUAREDMINSTEPS = Sqr(MINSTEPS);
var
  Temp: TFloat;
  Steps: Integer;
begin
  // The code below was previously:
  //
  // Steps := Max(MINSTEPS, System.Round(Sqrt(Abs(Radius)) *
  //   Abs(EndAngle - StartAngle)));
  //
  // However, for small radii, the square root calculation is performed with
  // the result that the output is set to 6 anyway. In this case (only a few
  // drawing operations), the performance spend for this calculation is dominant
  // for large radii (when a lot of CPU intensive drawing takes place), the
  // more expensive float point comparison (Temp < SQUAREDMINSTEPS) is not very
  // significant

  Temp := Abs(Radius) * Sqr(EndAngle - StartAngle);
  if Temp < SQUAREDMINSTEPS then
    Steps := 6
  else
    Steps := Round(Sqrt(Temp));
  Result := BuildArc(P, StartAngle, EndAngle, Radius, Steps);
end;

function BuildArc(const P: TFixedPoint; StartAngle, EndAngle, Radius: TFloat;
  Steps: Integer): TArrayOfFixedPoint; overload;
var
  I: Integer;
  C, D: TFloatPoint;
begin
  SetLength(Result, Steps);
  SinCos(StartAngle, Radius, C.Y, C.X);
  Result[0] := OffsetPoint(P, C);

  GR32_Math.SinCos((EndAngle - StartAngle) / (Steps - 1), D.Y, D.X);
  for I := 1 to Steps - 1 do
  begin
    C := FloatPoint(C.X * D.X - C.Y * D.Y, C.Y * D.X + C.X * D.Y);
    Result[I] := OffsetPoint(P, FixedPoint(C));
  end;
end;

function BuildArc(const P: TFixedPoint; StartAngle, EndAngle, Radius: TFloat): TArrayOfFixedPoint; overload;
const
  MINSTEPS = 6;
  SQUAREDMINSTEPS = Sqr(MINSTEPS);
var
  Temp: TFloat;
  Steps: Integer;
begin
  // The code below was previously:
  //
  // Steps := Clamp(System.Round(Sqrt(Abs(Radius)) *
  //   Abs(EndAngle - StartAngle)), MINSTEPS, $100000);
  //
  // However, for small radii, the square root calculation is performed with
  // the result that the output is set to 6 anyway. In this case (only a few
  // drawing operations), the performance spend for this calculation is dominant
  // for large radii (when a lot of CPU intensive drawing takes place), the
  // more expensive float point comparison (Temp < SQUAREDMINSTEPS) is not very
  // significant

  Temp := Abs(Radius) * Sqr(EndAngle - StartAngle);
  if Temp < SQUAREDMINSTEPS then
    Steps := MINSTEPS
  else
    Steps := Clamp(Round(Sqrt(Temp)), $100000);
  Result := BuildArc(P, StartAngle, EndAngle, Radius, Steps);
end;

function BuildLineEnd(const P, N: TFloatPoint; const W: TFloat;
  EndStyle: TEndStyle): TArrayOfFloatPoint; overload;
var
  a1, a2: TFloat;
begin
  case EndStyle of
    esButt:
      begin
        Result := nil;
      end;
    esSquare:
      begin
        SetLength(Result, 2);
        Result[0].X := P.X + (N.X - N.Y) * W;
        Result[0].Y := P.Y + (N.Y + N.X) * W;
        Result[1].X := P.X - (N.X + N.Y) * W;
        Result[1].Y := P.Y - (N.Y - N.X) * W;
      end;
    esRound:
      begin
        a1 := ArcTan2(N.Y, N.X);
        a2 := ArcTan2(-N.Y, -N.X);
        if a2 < a1 then a2 := a2 + TWOPI;
        Result := BuildArc(P, a1, a2, W);
      end;
  end;
end;

function BuildPolyline(const Points: TArrayOfFloatPoint; StrokeWidth: TFloat;
  JoinStyle: TJoinStyle = jsMiter; EndStyle: TEndStyle = esButt;
  MiterLimit: TFloat = DEFAULT_MITER_LIMIT
  ): TArrayOfFloatPoint;
var
  L, H: Integer;
  Normals: TArrayOfFloatPoint;
  P1, P2, E1, E2: TArrayOfFloatPoint;
  V: TFloat;
  P: PFloatPoint;
begin
  V := StrokeWidth * 0.5;
  Normals := BuildNormals(Points);

  H := High(Points) - 1;
  while (H >= 0) and (Normals[H].X = 0) and (Normals[H].Y = 0) do Dec(H);
  if H < 0 then Exit;
  L := 0;
  while (Normals[L].X = 0) and (Normals[L].Y = 0) do Inc(L);

  P1 := Grow(Points, Normals, V, JoinStyle, False, MiterLimit);
  P2 := ReversePolygon(Grow(Points, Normals, -V, JoinStyle, False, MiterLimit));

  E1 := BuildLineEnd(Points[0], Normals[L], -V, EndStyle);
  E2 := BuildLineEnd(Points[High(Points)], Normals[H], V, EndStyle);

  SetLength(Result, Length(P1) + Length(P2) + Length(E1) + Length(E2));
  P := @Result[0];
  Move(E1[0], P^, Length(E1) * SizeOf(TFloatPoint)); Inc(P, Length(E1));
  Move(P1[0], P^, Length(P1) * SizeOf(TFloatPoint)); Inc(P, Length(P1));
  Move(E2[0], P^, Length(E2) * SizeOf(TFloatPoint)); Inc(P, Length(E2));
  Move(P2[0], P^, Length(P2) * SizeOf(TFloatPoint));
end;

function BuildPolyPolyLine(const Points: TArrayOfArrayOfFloatPoint;
  Closed: Boolean; StrokeWidth: TFloat; JoinStyle: TJoinStyle;
  EndStyle: TEndStyle; MiterLimit: TFloat): TArrayOfArrayOfFloatPoint;
var
  I: Integer;
  P1, P2: TArrayOfFloatPoint;
  Dst: TArrayOfArrayOfFloatPoint;
  Normals: TArrayOfFloatPoint;
begin
  if Closed then
  begin
    SetLength(Dst, Length(Points) * 2);
    for I := 0 to High(Points) do
    begin
      Normals := BuildNormals(Points[I]);
      P1 := Grow(Points[I], Normals, StrokeWidth * 0.5, JoinStyle, True, MiterLimit);
      P2 := Grow(Points[I], Normals, -StrokeWidth * 0.5, JoinStyle, True, MiterLimit);
      Dst[I * 2] := P1;
      Dst[I * 2 + 1] := ReversePolygon(P2);
    end;
  end
  else
  begin
    SetLength(Dst, Length(Points));
    for I := 0 to High(Points) do
      Dst[I] := BuildPolyline(Points[I], StrokeWidth, JoinStyle, EndStyle);
  end;
  Result := Dst;
end;


procedure SVGPolyPolylineFS(Bitmap: TBitmap32; const Points: TArrayOfArrayOfFloatPoint;
  Filler: TCustomPolygonFiller; Closed: Boolean = False; StrokeWidth: TFloat = 1.0;
  JoinStyle: TJoinStyle = jsMiter; EndStyle: TEndStyle = esButt;
  MiterLimit: TFloat = 4.0; Transformation: TTransformation = nil);
var
  Dst: TArrayOfArrayOfFloatPoint;
begin
  Dst := BuildPolyPolyLine(Points, Closed, StrokeWidth, JoinStyle, EndStyle, MiterLimit);
  PolyPolygonFS(Bitmap, Dst, Filler, pfWinding, Transformation);
end;

end.
