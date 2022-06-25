unit mbpReg;

interface

procedure Register;

implementation

uses
  {$IFDEF DELPHI6+}
  //DesignIntf,
  {$ELSE}
  //dsgnintf,
  {$ENDIF}
  Classes,
  mbpMCDBPro {, mbpPropertyEditor};
{$I mbpDEFINES.INC}

procedure Register;
begin
  //RegisterPropertyEditor(TypeInfo(AnsiString), TMCDBurnerPro, 'BootImage', TmbFileNameProperty);

  RegisterComponents('Magic CD Burner (Pro)', [TMCDBurnerPro]);
end;

end.
