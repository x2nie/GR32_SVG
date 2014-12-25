      {******************************************************************}
      { Parse of SVG properties                                          }
      {                                                                  }
      { home page : http://www.mwcs.de                                   }
      { email     : martin.walter@mwcs.de                                }
      {                                                                  }
      { date      : 05-04-2008                                           }
      {                                                                  }
      { Use of this file is permitted for commercial and non-commercial  }
      { use, as long as the author is credited.                          }
      { This file (c) 2005, 2008 Martin Walter                           }
      {                                                                  }
      { This Software is distributed on an "AS IS" basis, WITHOUT        }
      { WARRANTY OF ANY KIND, either express or implied.                 }
      {                                                                  }
      { *****************************************************************}

unit SVGProperties;

interface

uses
  MSXML2_TLB_Light,
  SVGTypes, Matrix;

procedure LoadLength(const Node: IXMLDOMNode; const S: WideString;
  var X: TFloat);

procedure LoadTFloat(const Node: IXMLDOMNode; const S: WideString;
  var X: TFloat);

procedure LoadString(const Node: IXMLDOMNode; const S: WideString;
  var X: WideString);

procedure LoadTransform(const Node: IXMLDOMNode; const S: WideString;
  var Matrix: TMatrix);

procedure LoadPercent(const Node: IXMLDOMNode; const S: WideString;
  var X: TFloat); overload;
procedure LoadPercent(const Node: IXMLDOMNode; const S: WideString;
  Max: Integer; var X: TFloat); overload;
procedure LoadBytePercent(const Node: IXMLDOMNode; const S: WideString;
  var X: Integer);

procedure LoadBoolean(const Node: IXMLDOMNode; const S: WideString;
  var X: Boolean);

procedure LoadDisplay(const Node: IXMLDOMNode; var X: Integer);

procedure LoadVisible(const Node: IXMLDOMNode; var X: Integer);

procedure LoadGradientUnits(const Node: IXMLDOMNode; var Units: TGradientUnits);

implementation

uses
  SVGCommon, SVGParse;

procedure LoadLength(const Node: IXMLDOMNode; const S: WideString;
  var X: TFloat);
var
  Attribute: IXMLDOMNode;
begin
  Attribute := Node.attributes.getNamedItem(S);
  if Assigned(Attribute) then
    X := ParseLength(Attribute.nodeValue);
end;

procedure LoadTFloat(const Node: IXMLDOMNode; const S: WideString;
  var X: TFloat);
var
  Attribute: IXMLDOMNode;
begin
  Attribute := Node.attributes.getNamedItem(S);
  if Assigned(Attribute) then
    X := StrToTFloat(Attribute.nodeValue);
end;

procedure LoadString(const Node: IXMLDOMNode; const S: WideString;
  var X: WideString);
var
  Attribute: IXMLDOMNode;
begin
  Attribute := Node.attributes.getNamedItem(S);
  if Assigned(Attribute) then
    X := Attribute.nodeValue;
end;

procedure LoadTransform(const Node: IXMLDOMNode; const S: WideString;
  var Matrix: TMatrix);
var
  Attribute: IXMLDOMNode;
begin
  Attribute := Node.attributes.getNamedItem(S);
  if Assigned(Attribute) then
    Matrix := ParseTransform(Attribute.nodeValue);
end;

procedure LoadPercent(const Node: IXMLDOMNode; const S: WideString;
  var X: TFloat);
var
  Attribute: IXMLDOMNode;
begin
  Attribute := Node.attributes.getNamedItem(S);
  if Assigned(Attribute) then
    X := ParsePercent(Attribute.nodeValue);
end;

procedure LoadPercent(const Node: IXMLDOMNode; const S: WideString;
  Max: Integer; var X: TFloat);
var
  Attribute: IXMLDOMNode;
begin
  Attribute := Node.attributes.getNamedItem(S);
  if Assigned(Attribute) then
    X := Max * ParsePercent(Attribute.nodeValue);
end;

procedure LoadBytePercent(const Node: IXMLDOMNode; const S: WideString;
  var X: Integer);
var
  Attribute: IXMLDOMNode;
begin
  Attribute := Node.attributes.getNamedItem(S);
  if Assigned(Attribute) then
    X := Round(255 * ParsePercent(Attribute.nodeValue));
end;

procedure LoadBoolean(const Node: IXMLDOMNode; const S: WideString;
  var X: Boolean);
var
  Attribute: IXMLDOMNode;
begin
  Attribute := Node.attributes.getNamedItem(S);
  if Assigned(Attribute) then
    X := Boolean(ParseInteger(Attribute.nodeValue));
end;

procedure LoadDisplay(const Node: IXMLDOMNode; var X: Integer);
var
  S: WideString;
  Attribute: IXMLDOMNode;
begin
  Attribute := Node.attributes.getNamedItem('display');
  if Assigned(Attribute) then
  begin
    S := Attribute.nodeValue;
    if S = 'inherit' then
      X := -1
    else
      if S = 'none' then
        X := 0
      else
        X := 1;
  end;
end;

procedure LoadVisible(const Node: IXMLDOMNode; var X: Integer);
var
  S: WideString;
  Attribute: IXMLDOMNode;
begin
  Attribute := Node.attributes.getNamedItem('visibility');
  if Assigned(Attribute) then
  begin
    S := Attribute.nodeValue;
    if S = 'inherit' then
      X := -1
    else
      if S = 'visible' then
        X := 1
      else
        X := 0;
  end;
end;

procedure LoadGradientUnits(const Node: IXMLDOMNode; var Units: TGradientUnits);
var
  S: WideString;
  Attribute: IXMLDOMNode;
begin
  Units := guObjectBoundingBox;
  Attribute := Node.attributes.getNamedItem('gradientUnits');
  if Assigned(Attribute) then
  begin
    S := Attribute.nodeValue;
    if S = 'userSpaceOnUse' then
      Units := guUserSpaceOnUse
    else
      if S = 'objectBoundingBox' then
        Units := guObjectBoundingBox;
  end;
end;

end.
