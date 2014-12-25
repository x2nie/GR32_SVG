program Project1;

uses
  Forms,
  Unit1 in 'Unit1.pas' {Form1},
  SVG in '..\SVGlib-v0.69\SVG.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
