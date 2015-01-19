program linegrow;

uses
  Forms,
  uMain in 'uMain.pas' {frmMain},
  acc_data in 'acc_data.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
