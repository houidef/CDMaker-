program LargeD4;

uses
  Forms,
  Main in 'Main.pas' {frmMain},
  Options in 'Options.pas' {frmOptions};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TfrmMain, frmMain);
  Application.CreateForm(TfrmOptions, frmOptions);
  Application.Run;
end.
