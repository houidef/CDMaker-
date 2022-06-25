(*******************************************************************************
  Unit        : mbErase.PAS
  Date        : Feb 2005 - Oct 2006
  Author      : Ehsan Khan
  Description :
  Copyright   : 2002-06 Binary Magic, All rights reserved.
*******************************************************************************)

unit mbpErase;

interface

uses
  Classes, mbpDeviceTypes, mbpTreeTypes;

{$I mbpDEFINES.INC}

type
  TEraseThread = class(TThread)
  protected
    procedure Execute; override;
  public
    CDFiles: PCDFiles;
    Device: PSCSIDEVICE;
    constructor Create(var CDFiles_: TCDFiles; var Device_: TSCSIDevice); overload;
  end;

function EraseDisc(var CDFiles: TCDFiles; var Device: TSCSIDevice; Quick: Boolean; EraseDone: TEraseDoneEvent): Boolean;

implementation

uses
  Windows, SysUtils, mbpConsts, mbpSCSILib;

(*******************************************************************************
*                                                                              *
*******************************************************************************)
constructor TEraseThread.Create(var CDFiles_: TCDFiles; var Device_: TSCSIDevice);
begin
  inherited Create(True);
  FreeOnTerminate := true;
  Device := @Device_;
  CDFiles := @CDFiles_;
end;

(*******************************************************************************
*                                                                              *
*******************************************************************************)
procedure TEraseThread.Execute;
const
  buf1: array [0..12] of Byte = ($00, $82, $00, $08, $00, $22, $21, $20, $00, $00, $08, $00, $00);
  buf2: array [0..12] of Byte = ($00, $82, $00, $08, $00, $00, $00, $00, $98, $00, $00, $00, $00);
  buf3: array [0..11] of Byte = ($00, $82, $00, $08, $00, $23, $05, $40, $98, $00, $00, $00);
  buf4: array [0..11] of Byte = ($00, $82, $00, $08, $00, $00, $02, $80, $C0, $00, $10, $00);
var
  i: Integer;
  Medium: Byte;
  Error: Boolean;
begin
  Medium := Ord(DiscIs(Device^));
  Error := False;

  SetCDSpeed(Device^, Device.ReadSpeed, Device.WriteSpeed);
  SetWriteParams(Device^, False, False, False, Medium);

  if ((Integer(Medium) <> Ord(mtDVDRAM)) and (Integer(Medium) <> Ord(mtDVDPLUSRW)) and (Integer(Medium) <> Ord(mtBDRE))) then
  begin
    if (Erase(Device^, Device.QuickErase) <> TRUE) then
    begin
      if Assigned(Device.cbEraseDone) then
      begin
        if Device.hWNDFE <> 0 then
          PostMessage(Device.hWNDFE, WM_ERASEDONE, 0, -1)
        else
          Device.cbEraseDone(Self, 0, True);
      end;

      Terminate;
      Exit;
    end;

    repeat
      Sleep(5000);
      ReadDiscInformation(Device^, Device.tmpBuffer, $2a);
    until (Device.LastSenseInfo.AddSenseCode <> 04);

    if ((Integer(Medium) = Ord(mtDVDRWSR)) or (Integer(Medium) = Ord(mtDVDRWRO))) then
    begin
    end;
  end
  else if (Integer(Medium) = Ord(mtBDRE)) then
  begin
    if (Device.QuickErase) then
    begin
      Move(buf4, Device.tmpBuffer, 12);
      if FormatUnit(Device^, $11, Device.tmpBuffer, 12) then
      begin
        repeat
          Sleep(5000);
          ReadDiscInformation(Device^, Device.tmpBuffer, $2a);
        until (Device.LastSenseInfo.AddSenseCode <> 04);
      end;

      FillChar(Device.tmpBuffer, SizeOf(Device.tmpBuffer), 0);
      for i := 0 to Pred(16) do
      begin
        Device.CloseFH := False;
        if (Write10(Device^, i * 32, 32, Device.tmpBuffer, 32 * 2048) = False) then
        begin
          Error := True;
          Device.CloseFH := True;
          Break;
        end;
      end;
      FlushCache(Device^, False);
      CloseDVDTrack(Device^, False, 3, 0, 0, 0);
      Device.CloseFH := True;
    end
  end
  else if (Integer(Medium) = Ord(mtDVDPLUSRW)) then
  begin
    if (Device.QuickErase) then
    begin
      Move(buf3, Device.tmpBuffer, 12);
      if FormatUnit(Device^, $11, Device.tmpBuffer, 12) then
      begin
        repeat
          Sleep(5000);
          ReadDiscInformation(Device^, Device.tmpBuffer, $2a);
        until (Device.LastSenseInfo.AddSenseCode <> 04);
      end;

      FillChar(Device.tmpBuffer, SizeOf(Device.tmpBuffer), 0);
      for i := 0 to Pred(16) do
      begin
        Device.CloseFH := False;
        if (Write10(Device^, i * 32, 32, Device.tmpBuffer, 32 * 2048) = False) then
        begin
          Error := True;
          Device.CloseFH := True;
          Break;
        end;
      end;
      FlushCache(Device^, False);
      CloseDVDTrack(Device^, False, 3, 0, 0, 0);
      Device.CloseFH := True;
    end
    else if (Device.QuickErase = False) then
    begin
      Move(buf3, Device.tmpBuffer, 12);
      if (FormatUnit(Device^, $11, Device.tmpBuffer, 12) = True) then
      begin
        repeat
          Sleep(5000);
          ReadDiscInformation(Device^, @Device.BGFDI, $2a);
        until (Device.LastSenseInfo.AddSenseCode <> 04);
        CDFiles.ComponentStatus := CS_BACKGROUNDFORMAT;
        Device.CloseFH := False;
        {$IFDEF TRIAL}
        for i := 0 to Pred(71000) do
        {$ELSE}
        for i := 0 to Pred(2048) do
        {$ENDIF}
        begin
          if (Write10(Device^, i * 32, 32, Device.tmpBuffer, 32 * 2048) = False) then
          begin
            Error := True;
            Device.CloseFH := True;
            Break;
          end;
        end;
        FlushCache(Device^, False);
        Device.CloseFH := True;
      end
      else
      begin

        if Assigned(Device.cbEraseDone) then
        begin
          if Device.hWNDFE <> 0 then
            PostMessage(Device.hWNDFE, WM_ERASEDONE, 0, -1)
          else
            Device.cbEraseDone(Self, 0, True);
        end;
      end;
    end;
  end
  else if (Integer(Medium) = Ord(mtDVDRAM)) then
  begin
    Move(buf2, Device.tmpBuffer, 12);
    if (Device.QuickErase = True) then
    begin
      FillChar(Device.tmpBuffer, SizeOf(Device.tmpBuffer), 0);
      Device.CloseFH := False;
      for i := 0 to Pred(128) do
      begin
        if (Write10(Device^, i * 32, 32, Device.tmpBuffer, 32 * 2048) = False) then
        begin
          Error := True;
          Device.CloseFH := True;
          Break;
        end;
      end;
      Device.CloseFH := True;
      FlushCache(Device^, False);
    end
    else
    begin
      Move(buf1, Device.tmpBuffer, 12);
      if (FormatUnit(Device^, $11, Device.tmpBuffer, 12) = True) then
      begin
        repeat
          Sleep(5000);
          TestUnitReady(Device^);
        until (Device.LastSenseInfo.AddSenseCode <> 04);
      end
      else
      begin
        if Assigned(Device.cbEraseDone) then
        begin
          if Device.hWNDFE <> 0 then
            PostMessage(Device.hWNDFE, WM_ERASEDONE, 0, -1)
          else
            Device.cbEraseDone(Self, 0, True);
        end;
      end;
    end;
  end;

  if Assigned(Device.cbEraseDone) then
  begin
    if Device.hWNDFE <> 0 then
      PostMessage(Device.hWNDFE, WM_ERASEDONE, 0, Integer(Error))
    else
      Device.cbEraseDone(Self, 0, Error);
  end;
  CDFiles.ComponentStatus := CS_IDLE;
  Terminate;
end;

(*******************************************************************************
*                                                                              *
*******************************************************************************)
function EraseDisc(var CDFiles: TCDFiles; var Device: TSCSIDevice; Quick: Boolean; EraseDone: TEraseDoneEvent): Boolean;
var
  EraseThread: TEraseThread;
begin
  CDFiles.ComponentStatus := CS_ERASING;

  Device.cbEraseDone := EraseDone;
  Device.QuickErase := Quick;

  EraseThread := TEraseThread.Create(CDFiles, Device);
  EraseThread.Resume;

  Result := True;
end;

end.
