program LargeD7;

uses
  Forms,
  Main in 'Main.pas' {frmMain},
  Options in 'Options.pas' {frmOptions};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrmMain, frmMain);
  Application.CreateForm(TfrmOptions, frmOptions);
  Application.Run;
end.

