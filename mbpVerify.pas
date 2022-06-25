unit mbpVerify;

interface

uses
  Windows, Messages, SysUtils, Classes, Math, SyncObjs, Dialogs, mbpCommonLib, mbpDeviceTypes, mbpTreeTypes, mbpSCSILib, mbpConsts;

type

  TVerifyDiscThread = class(TThread)
  private
    CDFiles: PCDFiles;
    Device: PSCSIDevice;
    procedure OnTerminateEvent(Sender: TObject);
  protected
    procedure Execute; override;
  public
    StopOnMismatch, FireVerifyEventOnAllFiles: Boolean;
    constructor Create(var CDFiles_: TCDFiles; var Device_: TSCSIDevice); overload;
  end;
  function VerifyDisc(var CDFiles_: TCDFiles; var Device_: TSCSIDevice; StopOnMismatch_, FireVerifyEventOnAllFiles_: Boolean; VerifyEvent: TVerifyDoneEvent): Boolean;

implementation

uses mbpHash;

var
  VerifyDiscThread: TVerifyDiscThread;


(*******************************************************************************)

(*******************************************************************************)
procedure TVerifyDiscThread.OnTerminateEvent;
begin

end;

(*******************************************************************************)

(*******************************************************************************)
function CalcHash(CDFiles: PCDFiles; FileEntry: pFileEntry): Boolean;
var
  md5: TMD5Stream;
  fs: TFileStream;
  BytesRead: Integer;
begin
  result := false;
  fs := TFileStream.Create(FileEntry.Path, fmShareDenyNone or fmOpenRead);
  md5 := TMD5Stream.Create;
  while true do
  begin
    BytesRead := fs.Read(CDFiles.tmpBuffer1[0], SizeOf(CDFiles.tmpBuffer1));
    if BytesRead <> 0 then
    begin
      md5.Write(CDFiles.tmpBuffer1[0], BytesRead);
      //CDFiles.VerifyProgress := CDFiles.VerifyProgress + Sectors(BytesRead);
    end;
    if (BytesRead <> SizeOf(CDFiles.tmpBuffer1)) then
    begin
      FileEntry.Hash := md5.DigestString;
      break;
    end;
    if CDFiles.Aborted then
    begin
      CDFiles.ComponentStatus := CS_ABORTING;
      Break;
    end;
  end;
  md5.Destroy;
  fs.Destroy;
end;
(*******************************************************************************)

(*******************************************************************************)
function CalcHashCDFile(CDFiles: PCDFiles; Device: PSCSIDevice; FileEntry: pFileEntry): AnsiString;
var
  md5: TMD5Stream;
  fs: TFileStream;
  BytesToRead, BytesLeft: LongInt;
  res: Boolean;
  StartSector: LongInt;
  SectorsToRead: Integer;
begin
  fs := TFileStream.Create(FileEntry.Path, fmShareDenyNone or fmOpenRead);
  md5 := TMD5Stream.Create;
  StartSector := FileEntry.Address + CDFiles.FirstDataSector + CDFiles.FirstWritableSector;
  BytesLeft := FileEntry.FileSize;
  while (BytesLeft > 0) do
  begin
    if (BytesLeft > 65536) then
      BytesToRead := 65536
    else
      BytesToRead := BytesLeft;
    SectorsToRead := Sectors(BytesToRead);
    res := Read10(Device^, StartSector, SectorsToRead, @CDFiles.tmpBuffer1[0]);
    StartSector := StartSector + SectorsToRead;
    if not res then
    begin
      result := '-';
      CDFiles.VerifyProgress := CDFiles.VerifyProgress + Sectors(BytesLeft);
      break;
    end
    else
    begin
      md5.Write(CDFiles.tmpBuffer1[0], BytesToRead);
      CDFiles.VerifyProgress := CDFiles.VerifyProgress + Sectors(BytesToRead);
    end;
    BytesLeft := BytesLeft - BytesToRead;
    if (BytesLeft = 0) then
       result := md5.DigestString;
    if CDFiles.Aborted then
    begin
      CDFiles.ComponentStatus := CS_ABORTING;
      Break;
    end;
  end;
  md5.Destroy;
  fs.Destroy;

end;
(*******************************************************************************)

(*******************************************************************************)
constructor TVerifyDiscThread.Create(var CDFiles_: TCDFiles; var Device_: TSCSIDevice);
begin
  inherited Create(True);
  FreeOnTerminate := True;
  CDFiles := @CDFiles_;
  Device := @Device_;
  OnTerminate := OnTerminateEvent;
  QPerfDeviceInit(Device_);
  Priority := tpLower;
  CDFiles.NoOfFiles := CDFiles.FileCounter;
  CDFiles.hSmartReferenceThread := Self.Handle;
  CDFiles.IdSmartReferenceThread := Self.ThreadID;
  CDFiles.VerifyProgress := 0;
  if (CDFiles.hCacheThread = INVALID_HANDLE_VALUE) then
  begin
    {$IFDEF CONDEBUG}
    WriteLn('Thread Creation Failed');
    {$ENDIF}
    Exit;
  end;
  CDFiles.CacheThreadTerminated := False;
  //Resume;
end;

procedure TVerifyDiscThread.Execute;
var
  i: Integer;
  f1: pFileEntry;
  Hash: AnsiString;
  Error: Boolean;
begin
  Error := False;
  CDFiles.VerifyProgress := 0;
  for i := 0 to CDFiles.FileCounter-1 do
  begin
    f1 := CDFiles.Files[i];
    if (f1.Path <> nil) and (f1.Imported <> true) then
    begin
      if f1.FileSize = 0 then
      begin
        Hash := '-';
        f1.Hash := '-';
      end
      else
      begin
        if f1.Hash = '' then
          CalcHash(CDFiles, f1);
        hash := CalcHashCDFile(CDFiles, Device, f1);
      end;
      if (hash = f1.Hash) then
      begin
        if Assigned(CDFiles.cbVerifyDone) then
        begin
          if CDFiles.hWNDFE <> 0 then
            PostMessage(CDFiles.hWNDFE, WM_VERIFYDONE, i, 0)
          else
            CDFiles.cbVerifyDone(Self, Device.SelectedDevice, i, False);
        end;
      end
      else
      begin
        Error := True;
        if Assigned(CDFiles.cbVerifyDone) then
        begin
          if CDFiles.hWNDFE <> 0 then
            PostMessage(CDFiles.hWNDFE, WM_VERIFYDONE, i, -1)
          else
            CDFiles.cbVerifyDone(Self, Device.SelectedDevice, i, True);
        end;
      end;
    end;
    if CDFiles.Aborted then
    begin
      CDFiles.ComponentStatus := CS_ABORTING;
      CDFiles.ErrorNumber := ERR_ABORTED_BY_USER;
      Break;
    end;
  end;
  CDFiles.ComponentStatus := CS_IDLE;
  if Assigned(CDFiles.cbVerifyDone) then
  begin
    if CDFiles.hWNDFE <> 0 then
      PostMessage(CDFiles.hWNDFE, WM_VERIFYDONE, -1, Integer(Error))
    else
      CDFiles.cbVerifyDone(Self, Device.SelectedDevice, -1, Error);
  end;

end;

function VerifyDisc(var CDFiles_: TCDFiles; var Device_: TSCSIDevice; StopOnMismatch_, FireVerifyEventOnAllFiles_: Boolean; VerifyEvent: TVerifyDoneEvent): Boolean;
begin
  CDFiles_.ComponentStatus := CS_VERIFYING;
  VerifyDiscThread := TVerifyDiscThread.Create(CDFiles_, Device_);
  VerifyDiscThread.FreeOnTerminate := True;
  VerifyDiscThread.StopOnMismatch := StopOnMismatch_;
  CDFiles_.cbVerifyDone := VerifyEvent;
  VerifyDiscThread.FireVerifyEventOnAllFiles := FireVerifyEventOnAllFiles_;
  VerifyDiscThread.Resume;
  result := true;
end;

end.
