program linegrow;

uses
  Forms,
  uMain in 'uMain.pas' {frmMain},
  acc_single_data in 'acc_single_data.pas',
  uInfo in 'uInfo.pas' {Form1};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrmMain, frmMain);
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
