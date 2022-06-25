program SmallD9;

{%File 'C:\Documents and Settings\Administrator\My Documents\Borland Studio Projects\ModelSupport\Main\Main.txvpck'}
{%File 'C:\Documents and Settings\Administrator\My Documents\Borland Studio Projects\ModelSupport\default.txvpck'}

uses
  Forms,
  Main in 'Main.pas' {frmMain};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
