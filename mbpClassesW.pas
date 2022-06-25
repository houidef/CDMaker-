unit mbpClassesW;

interface

uses
  Classes;

{$I mbpDEFINES.INC}
  
type
  TFileStreamW = class(THandleStream)
  public
    constructor Create(const FileName: WideString; Mode: Word);
    destructor Destroy; override;
  end;

implementation

uses
  SysUtils,
  {$IFDEF DELPHI6+}
  RTLConsts,
  {$ELSE}
  Consts,
  {$ENDIF}
  mbpSysUtilsW;

{ TFileStreamW }

constructor TFileStreamW.Create(const FileName: WideString; Mode: Word);
var
  CreateHandle: Integer;
begin
  if Mode = fmCreate then
  begin
    CreateHandle := FileCreateEx(FileName);
    if CreateHandle < 0 then
      raise EFCreateError.CreateFmt(SFCreateError, [FileName]);
  end
  else
  begin
    CreateHandle := FileOpenEx(FileName, Mode);
    if CreateHandle < 0 then
      raise EFOpenError.CreateFmt(SFOpenError, [FileName]);
  end;
  inherited Create(CreateHandle);
end;

destructor TFileStreamW.Destroy;
begin
  FileClose(Handle);
end;

end.

