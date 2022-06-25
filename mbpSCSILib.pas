unit mbpSCSILib;

interface

uses
  {$IFNDEF LINUX}
  windows,
  {$ELSE}
  Linux,
  {$ENDIF}
  mbpWnASPI32, mbpDeviceTypes, mbpCommonLib;

{$I mbpDEFINES.INC}
{$IFDEF LINUX}

const
  INVALID_HANDLE_VALUE = $FFFFFFFE;

type
  TSGHeader = packed record
    PackLen,
    ReplyLen,
    PackID,
    Result: Integer;
    Params: cardinal;
    Sense: array[0..15] of byte;
  end;
  PSGHeader = ^TSGHeader;

const
  SCSI_OFF = sizeof(TSGHeader);

var
  tmpWorkingBufferToDevice: array[0..$FFFF*2] of AnsiChar;
  tmpWorkingBufferFromDevice: array[0..$FFFF*2] of AnsiChar;

{$ELSE}
var
  SendASPI32Command : function(LPSRB: Pointer):DWORD; cdecl;
  GetASPI32SupportInfo : function: DWORD; cdecl;
{$ENDIF}
type
  DWORD = CARDINAL;

function OpenDevice(var Device: TSCSIDevice): Boolean;
procedure CloseDevice(var Device: TSCSIDevice);
function InitializeASPI(var device: TSCSIDEVICE; NoInternalASPI: Boolean; wnASPIDLLPath: AnsiString = ''; Reserved1: AnsiString = ''): Boolean;
function ExecSCSICommand(var Device: TSCSIDevice; srbExec: PSRB_ExecSCSICmd): Byte;
function TestUnitReady(var Device: TSCSIDevice): Boolean;
function Inquiry(var Device: TSCSIDevice; InquiryData: PAnsiChar): Boolean;
function SelectDevice(var Device: TSCSIDevice; Id: Byte): Boolean;
function SelectDeviceByLetter(var Device: TSCSIDevice; DriveLetter: AnsiChar): Boolean;
function GetDeviceName(var Device: TSCSIDevice; Id: Byte): PAnsiChar;
function LoadMedium(var Device: TSCSIDevice; Immediate: Boolean): Boolean;
function EjectMedium(var Device: TSCSIDevice; Immediate: Boolean): Boolean;
function Write10(var Device: TSCSIDevice; Sector: LongWord; NoOfSectors: Word; Buffer: PAnsiChar; BufferLen: DWORD): Boolean;
function Read10(var Device: TSCSIDevice; Sector: LongWord; NoOfSectors: Integer; Buffer: Pointer): Boolean;
function ReadBufferCapacity(var Device: TSCSIDevice): Boolean;
function ReadTOC(var Device: TSCSIDevice; Format: Byte; TrackNumber: Byte; Time: Boolean; Buffer: PAnsiChar; BufferLen: Byte): Boolean;
function ReadDiscInformation(var Device: TSCSIDevice; Buffer: PAnsiChar; BufferLen: Word): Boolean;
function DiscIs(var Device: TSCSIDevice): TMedium;
function FormatUnit(var Device: TSCSIDevice; FormatCode: Byte; Buffer: PAnsiChar; BufferLen: Word): Boolean;
function CloseDVDTrack(var Device: TSCSIDevice; Immediate: Boolean; b2, b3, b4, b5: Byte): Boolean;
function UnlockMedium(var Device: TSCSIDevice): Boolean;
function SessionsOnDisc(var Device: TSCSIDevice): Integer;
function ReadTrackInformation(var Device: TSCSIDevice; TrackNumber: Byte): Boolean;
function TracksOnDisc(var Device: TSCSIDevice): Integer;
function GetDeviceCapabilities(var Device: TSCSIDevice): Boolean;
function ModeSelect(var Device: TSCSIDevice; Buffer: PAnsiChar; BufferLength: Word): Boolean;
function ModeSense10(var Device: TSCSIDevice; Buffer: PAnsiChar; BufferLength: Word; PageCode: Byte; PS: Boolean): Boolean;
function JustLinkCapable(var Device: TSCSIDevice): Boolean;
function LockMedium(var Device: TSCSIDevice): Boolean;
function ModeSelect10(var Device: TSCSIDevice; Buffer: PAnsiChar; BufferLen: Word ): Boolean;
function GetConfiguration(var Device: TSCSIDevice; Buffer: PAnsiChar; BufferLen: Word; ProfileCode: Word): Boolean;
function RequestSense(var Device: TSCSIDevice; Buffer: PAnsiChar; BufferLen: Byte): Boolean;
function CloseTrack(var Device: TSCSIDevice; Track, Immediate: Boolean; TrackNumber: Byte = 0): Boolean;
function UnlockDrive(var Device: TSCSIDevice): Boolean;
function SetWriteParams(var Device: TSCSIDevice; TestWrite: Boolean; BUF: Boolean; AllowNextSession: Boolean; Medium: Byte): Boolean;
function SetWriteParams2(var Device: TSCSIDevice): Boolean;
function SetWriteParams3(var Device: TSCSIDevice): Boolean;
function SetCDSpeed(var Device: TSCSIDevice; ReadSpeed: Word; WriteSpeed: Word): Boolean;
function FlushCache(var Device: TSCSIDevice; Immed: Boolean): Boolean;
function Rewind(var Device: TSCSIDevice): Boolean;
function Erasable(var Device: TSCSIDevice): Boolean;
function GetFormatCapacity(var Device: TSCSIDevice): LongWord;
function GetFreeBlocksOnDisc(var Device: TSCSIDevice): LongWord;
function GetTotalBlocksOnDisc(var Device: TSCSIDevice): LongWord;
// WS
function GetDeviceMaxReadSpeed(var Device: TSCSIDevice): Word;
function GetReadSpeed(var Device: TSCSIDevice): Word;
function GetDeviceMaxWriteSpeed(var Device: TSCSIDevice): Word;
function GetMaxWriteSpeed(var Device: TSCSIDevice): Word;
function GetWriteSpeed(var Device: TSCSIDevice): Word;
//function SetWriteSpeedDVD(var Device: TSCSIDevice): Word;
//function GetMaxWriteSpeedDVD(var Device: TSCSIDevice): Word;
// WS.
function Erase(var Device: TSCSIDevice; Quick: Boolean): Boolean;
function DeviceIsBurner(var Device: TSCSIDevice): Boolean;
function SendDVDStructure(var Device: TSCSIDevice; Format: Byte; Buffer: Pointer; BufferLen: Word): Boolean;
function SendDVDStructureTimeStamp(var Device: TSCSIDevice; time: TSystemTime): Boolean;
function ReserveTrack(var Device: TSCSIDevice; TrackSize: LongWord): Boolean;
// WS
function SetPerformance(var Device: TSCSIDevice; StartLBA, EndLBA, ReadSize, WriteSize, ReadTime, WriteTime: LongWord): Boolean;
function SetStreaming(var Device: TSCSIDevice; Buffer: Pointer; BufferLen: Word): Boolean;
// WS.
function GetDeviceBufferPosition(var Device: TSCSIDevice): LongWord;
function Writable(var Device: TSCSIDevice): Boolean;
function DeviceCan(var Device: TSCSIDevice; cId: Byte): Boolean;
function GetTestWrite(var Device: TSCSIDevice): Boolean;
function GetBurnProof(var Device: TSCSIDevice): Boolean;
function GetPerformOPC(var Device: TSCSIDevice): Boolean;
function GetFinalizeDisc(var Device: TSCSIDevice): Boolean;
function GetDeviceCount(var Device: TSCSIDevice): Word;
function GetSelectedDevice(var Device: TSCSIDevice): Byte;
function GetDeviceBufferSize(var Device: TSCSIDevice): LongWord;
// WS
procedure SetDeviceSpeed(var Device: TSCSIDevice; ReadSpeed, WriteSpeed: Word);
// WS.
procedure SetTestWrite(var Device: TSCSIDevice; TestWrite: Boolean);
procedure SetBurnProof(var Device: TSCSIDevice; BurnProof: Boolean);
procedure SetPerformOPC(var Device: TSCSIDevice; PerformOPC: Boolean);
procedure SetFinalizeDisc(var Device: TSCSIDevice; FinalizeDisc: Boolean);
{$IFNDEF LINUX}
function SPTISendASPI32Command(var Device: TSCSIDevice; srb: PSRB_ExecSCSICmd; Retry: Boolean): LongWord;
function GetDeviceLetter(Device: TSCSIDevice): AnsiChar;
{$ENDIF}
function GetLastRecordedAddress(var Device: TSCSIDevice): LongWord;

procedure QPerfDeviceInit(var Device: TSCSIDevice);
Function QPerfDeviceGet(Device: TSCSIDevice): Integer;
procedure QPerfDeviceAdd(var Device: TSCSIDevice; time: Int64; bytes: int64);

implementation

uses
  Math, mbpSCSIDefs, SysUtils, mbpConsts, Classes, mbpISO9660, mbpTree;

const
  IOCTL_SCSI_GET_ADDRESS         = 266264;
  SCSI_IOCTL_DATA_OUT            =      0;
  SCSI_IOCTL_DATA_IN             =      1;
  SCSI_IOCTL_DATA_UNSPECIFIED    =      2;
  IOCTL_SCSI_PASS_THROUGH_DIRECT = 315412;

{*******************************************************************************
*                                                                              *
*******************************************************************************}
procedure CloseDevice(var Device: TSCSIDevice);
begin
  if (Device.fdbg <> nil) then
    Device.fdbg.Free;
  if (Device.Handle <> INVALID_HANDLE_VALUE) then
  begin
    CloseHandle(Device.Handle);
    Device.Handle := INVALID_HANDLE_VALUE;
  end;
end;

{*******************************************************************************
*                                                                              *
*******************************************************************************}
function SessionsOnDisc(var Device: TSCSIDevice): Integer;
begin
  if ReadDiscInformation(device, @device.DiscInformation, SizeOf(device.DiscInformation)) then
  begin //
    if (((device.DiscInformation.info and 12) = 0) or // StateOfLastSession
        ((device.DiscInformation.info and 12) = 1)) then
    begin
      Dec(device.DiscInformation.Sessions);
    end;
    Result := device.DiscInformation.Sessions;
  end
  else
  begin
    if (DiscIs(device) = mtDVDRAM) then
      Result := Integer(True)
    else
      Result := Integer(False);
  end;
end;

{*******************************************************************************
*                                                                              *
*******************************************************************************}
function ReadTrackInformation(var Device: TSCSIDevice; TrackNumber: Byte): Boolean;
var
  srbExec: TSRB_ExecSCSICmd;
begin
  FillChar(srbExec, SizeOf(TSRB_ExecSCSICmd), 0);
  srbExec.SRB_Flags := SRB_DIR_IN or SRB_EVENT_NOTIFY;
  srbExec.SRB_CDBLen := $0a;
  srbExec.SRB_BufPointer := @device.TrackInformation;
  srbExec.SRB_BufLen := SizeOf(device.TrackInformation);
  srbExec.CDBByte[0] := $52;
  srbExec.CDBByte[1] := $01;
  srbExec.CDBByte[5] := TrackNumber;
  srbExec.CDBByte[8] := SizeOf(device.TrackInformation);
  ExecSCSICommand(device, @srbExec);
  if (srbExec.SRB_Status = SS_COMP) then
  begin
    device.TrackInformation.DataLength := cew(device.TrackInformation.DataLength);
    device.TrackInformation.FreeBlocks := cedw(device.TrackInformation.FreeBlocks);
    device.TrackInformation.LastRecordedAddress := cedw(device.TrackInformation.LastRecordedAddress);
    device.TrackInformation.TrackStartAddress := cedw(device.TrackInformation.TrackStartAddress);
    device.TrackInformation.NextWritableAddress := cedw(device.TrackInformation.NextWritableAddress);
    device.TrackInformation.TrackSize := cedw(device.TrackInformation.TrackSize);
    device.TrackInformation.FixedPacketSize := cedw(device.TrackInformation.FixedPacketSize);
  end;
  Result := (srbExec.SRB_Status = SS_COMP);
end;

{*******************************************************************************
*                                                                              *
*******************************************************************************}
function TracksOnDisc(var Device: TSCSIDevice): Integer;
begin
  if ReadDiscInformation(device, @device.DiscInformation, SizeOf(device.DiscInformation)) then
    Result := device.DiscInformation.LastTrackOfLastSession
  else
    Result := 0;
end;

{*******************************************************************************
*                                                                              *
*******************************************************************************}
function OpenDevice(var Device: TSCSIDevice): Boolean;
begin
  Device.cn[$00] := 'TEST UNIT READY       ';
  Device.cn[$01] := 'REWIND UNIT           ';
  Device.cn[$03] := 'REQUEST SENSE         ';
  Device.cn[$04] := 'FORMAT UNIT           ';
  Device.cn[$12] := 'INQUIRY               ';
  Device.cn[$1B] := 'START STOP UNIT       ';
  Device.cn[$1e] := 'PREVENT/ALLOW REMOVAL ';
  Device.cn[$23] := 'READ FORMAT CAPACITIES';
  Device.cn[$25] := 'READ CAPACITY         ';
  Device.cn[$28] := 'READ (10)             ';
  Device.cn[$2a] := 'WRITE (10)            ';
  Device.cn[$2b] := 'SEEK (10)             ';
  Device.cn[$2e] := 'WRITE AND VERIFY (10) ';
  Device.cn[$2f] := 'VERIFY (10)           ';
  Device.cn[$35] := 'SYNCHRONIZE CACHE     ';
  Device.cn[$42] := 'READ SUB-CHANNEL      ';
  Device.cn[$43] := 'READ TOC/PMA/ATIP     ';
  Device.cn[$45] := 'PLAY AUDIO (10)       ';
  Device.cn[$46] := 'GET CONFIGURATION     ';
  Device.cn[$47] := 'PLAY AUDIO MSF        ';
  Device.cn[$4a] := 'GET EVENT/STATUS NOTIF';
  Device.cn[$4b] := 'PAUSE/RESUME          ';
  Device.cn[$4e] := 'STOP PLAY/SCAN        ';
  Device.cn[$51] := 'READ DISC INFORMATION ';
  Device.cn[$52] := 'READ TRACK INFORMATION';
  Device.cn[$53] := 'RESERVE TRACK         ';
  Device.cn[$54] := 'SEND OPC INFORMATION  ';
  Device.cn[$55] := 'MODE SELECT (10)      ';
  Device.cn[$58] := 'REPAIR TRACK          ';
  Device.cn[$5a] := 'MODE SENSE (10)       ';
  Device.cn[$5b] := 'CLOSE TRACK/SESSION   ';
  Device.cn[$5c] := 'READ BUFFER CAPACITY  ';
  Device.cn[$5d] := 'SEND CUE SHEET        ';
  Device.cn[$a1] := 'BLANK                 ';
  Device.cn[$a2] := 'SEND EVENT            ';
  Device.cn[$a3] := 'SEND KEY              ';
  Device.cn[$a4] := 'REPORT KEY            ';
  Device.cn[$a5] := 'PLAY AUDIO (12)       ';
  Device.cn[$a6] := 'LOAD/UNLOAD CD/DVD    ';
  Device.cn[$a7] := 'SET READ AHEAD        ';
  Device.cn[$a8] := 'READ (12)             ';
  Device.cn[$aa] := 'WRITE (12)            ';
  Device.cn[$ac] := 'GET PERFORMANCE       ';
  Device.cn[$ad] := 'READ DVD STRUCTURE    ';
  Device.cn[$b6] := 'SET STREAMING         ';
  Device.cn[$b9] := 'READ CD MSF           ';
  Device.cn[$ba] := 'SCAN                  ';
  Device.cn[$bb] := 'SET CD SPEED          ';
  Device.cn[$bd] := 'MECHANISM STATUS      ';
  Device.cn[$be] := 'READ CD               ';
  Device.cn[$bf] := 'SEND DVD STRUCTURE    ';
  result := False;
end;

{$IFNDEF LINUX}
function GetDeviceLetter(device: TSCSIDEVICE): AnsiChar;
var
 fh: THandle;
 i: Integer;
 Resulted: Longword;
 buf: AnsiString;
begin
  buf := '\\.\X:'#0;
  for i:= ord('C') to ord('Z') do
  begin
    buf[5] := AnsiChar(i);
    fh := CreateFileA(@buf[1], GENERIC_WRITE or GENERIC_READ, FILE_SHARE_READ or FILE_SHARE_WRITE, nil, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, 0);
    if (fh = INVALID_HANDLE_VALUE) then
      fh := CreateFileA(@buf[1], GENERIC_READ, FILE_SHARE_READ, nil, OPEN_EXISTING, 0, 0);

    if (fh <> INVALID_HANDLE_VALUE) then
    begin
      device.SCSI_ADDRESS.Length := sizeof(device.SCSI_ADDRESS);
      if (DeviceIoControl(fh, 266264, nil, 0, @device.SCSI_ADDRESS, sizeof(device.SCSI_ADDRESS), Resulted, nil) <> False) then
      begin
        if (device.Devices[device.NoOfDevices].HaID = device.SCSI_ADDRESS.PortNumber) and
           (device.Devices[device.NoOfDevices].Target = device.SCSI_ADDRESS.TargetId) and
           (device.Devices[device.NoOfDevices].Lun = device.SCSI_ADDRESS.Lun) then
        begin
          CloseHandle(fh);
          result := AnsiChar(i);
          exit;
        end;
      end;
    end;
    if (fh <> INVALID_HANDLE_VALUE) then
      CloseHandle(fh);
  end;

  result := AnsiChar(-127);
end;
{
*******************************************************************************
*                                                                             *
*******************************************************************************
}
function SPTISendASPI32Command(var device: TSCSIDEVICE; srb: PSRB_ExecSCSICmd; Retry: Boolean): DWORD; //Windows
var
  swb: TSCSI_PASS_THROUGH_DIRECT_WITH_BUFFER; //wr;
  length: Integer;
  status: Boolean;
  Resulted, LastError: Longword;
  buf: AnsiString;
begin
  if (device.Handle = INVALID_HANDLE_VALUE) then
  begin
    buf := '\\.\X:'#0;
    buf[5] := device.DriveLetter;
    device.Handle := CreateFileA(@buf[1], GENERIC_WRITE or GENERIC_READ, FILE_SHARE_READ or FILE_SHARE_WRITE, nil, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, 0);
    if (device.Handle = INVALID_HANDLE_VALUE) then
      device.Handle := CreateFileA(@buf[1], GENERIC_READ, FILE_SHARE_READ, nil, OPEN_EXISTING, 0, 0);
  end;
  fillchar(swb, sizeof(swb), 0);
  if ((srb.SRB_Flags and SRB_DIR_IN) <> 0) then
    swb.spt.DataIn := SCSI_IOCTL_DATA_IN
  else if ((srb.SRB_Flags and SRB_DIR_OUT) <> 0) then
    swb.spt.DataIn := SCSI_IOCTL_DATA_OUT
  else
    swb.spt.DataIn := SCSI_IOCTL_DATA_UNSPECIFIED;

  swb.spt.Length             := sizeof(swb.spt);
  swb.spt.CdbLength          := srb.SRB_CDBLen;
  swb.spt.SenseInfoLength    := srb.SRB_SenseLen;
  swb.spt.DataTransferLength := srb.SRB_BufLen;
  swb.spt.TimeOutValue       := 12000;
  swb.spt.DataBuffer         := PAnsiChar(srb.SRB_BufPointer);
  swb.spt.SenseInfoOffset    := Integer(@swb.ucSenseBuf[0]) - Integer(@swb);
  move(srb.CDBByte[0], swb.spt.Cdb[0], srb.SRB_CDBLen);
  length := sizeof(swb);
  status := DeviceIoControl(device.Handle, IOCTL_SCSI_PASS_THROUGH_DIRECT, @swb, length, @swb, length, Resulted, nil);
  device.LastWinError := GetLastError;
  if ((swb.spt.ScsiStatus = 0) and status) then
    srb.SRB_Status := SS_COMP
  else
  begin
    srb.SRB_Status := SS_ERR;
    move(swb.ucSenseBuf[0], srb.SenseArea, 24);
    srb.SRB_TargStat := swb.spt.ScsiStatus;
  end;

  if ((device.CloseFH = TRUE) and (device.Handle <> INVALID_HANDLE_VALUE)) then
  begin
    if CloseHandle(device.Handle) then
      device.Handle := INVALID_HANDLE_VALUE;
  end;
  LastError := GetLastError;
  if (Retry and ((LastError = ERROR_MEDIA_CHANGED) or (LastError = ERROR_INVALID_HANDLE))) then
  begin
    if (LastError <> ERROR_INVALID_HANDLE) then
    begin
      CloseHandle(device.Handle);
      device.Handle := INVALID_HANDLE_VALUE;
      result := SPTISendASPI32Command(device, srb, TRUE);
      exit;
    end;
  end;
  result := srb.SRB_Status;
end;
{
*******************************************************************************
*                                                                             *
*******************************************************************************
}

function InitializeASPI(var device: TSCSIDEVICE; NoInternalASPI: Boolean; wnASPIDLLPath: AnsiString = ''; Reserved1: AnsiString = ''): Boolean;
var
  scsiaddress: Tscsiaddress;
  Resulted, SupportInfo: Longword;
  hWNASPI32: HMODULE;
  heventSRB: THandle;
  dwASPIStatus: Longword;
  fh: thandle;
  srbExec: TSRB_ExecSCSICmd;
  srbHaInquiry: TSRB_HAInquiry;
  buf: array [0..10] of AnsiChar;
  HaID, Target, Lun, MaxHaId: Byte;
  i, j, extdrives, drives, index: Integer;
  productid: array[0..16] of AnsiChar;
  vendor: array[0..16] of AnsiChar;
  DeviceType: Cardinal;
  revision: array[0..16] of AnsiChar;
  BurnProof: Boolean;
  sdbg: AnsiString;
  tmpstr: AnsiString;
begin
  device.HaID := 255;
  device.Target := 255;
  device.Lun := 255;
  device.DriveLetter := '?';

  BurnProof := Device.BurnProof;
  FillChar(device, sizeof(device), 0);
  Device.BurnProof := BurnProof;

  if (Reserved1 <> '') then
  begin
    if (Device.fdbg <> nil) then
      Device.fdbg.Free;
    Device.fdbg := TFileStream.Create(String(Reserved1), fmCreate);
    if (Device.fdbg <> nil) then
    begin
      sdbg := GetMCDBVersion + #13#10;
      Device.fdbg.Write(sdbg[1], Length(sdbg));
    end;
  end;

  OpenDevice(device);
  strcopy(buf,'\\.\X:');
  device.NoOfDevices := 0;
  extdrives := 25;
  drives := 0;
  device.DoNotUseSPTI := NoInternalASPI{False};
  if (not device.DoNotUseSPTI) then
  begin
    for i := ord('C') to ord('Z') do
    begin
      buf[4] := AnsiChar(i);
      DeviceType := GetDriveTypeA(@buf[4]);
      if (DeviceType = DRIVE_CDROM) then
      begin
        fh := CreateFileA(buf, GENERIC_WRITE or GENERIC_READ, FILE_SHARE_READ or FILE_SHARE_WRITE, nil, OPEN_EXISTING, {0}FILE_ATTRIBUTE_NORMAL, 0);
        if (fh = INVALID_HANDLE_VALUE) then
        begin
          sdbg := AnsiString('System Error Code: ' + IntToStr(GetLastError) + #13#10);
          Device.fdbg.Write(sdbg[1], Length(sdbg));

          fh := CreateFileA(buf, GENERIC_READ, FILE_SHARE_READ or FILE_SHARE_WRITE, nil, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, 0);
          if (fh = INVALID_HANDLE_VALUE) then
          begin
            sdbg := AnsiString('System Error Code: ' + IntToStr(GetLastError) + #13#10);
            Device.fdbg.Write(sdbg[1], Length(sdbg));
          end;

          fh := CreateFileA(buf, GENERIC_READ, FILE_SHARE_READ, nil, OPEN_EXISTING, 0, 0);
        end;
        if (fh <> INVALID_HANDLE_VALUE) then
        begin
          fillchar(scsiaddress, sizeof(scsiaddress), 0);
          if (DeviceIoControl(fh, IOCTL_SCSI_GET_ADDRESS, nil, 0, @scsiaddress, {sizeof(SCSI_ADDRESS)}8, Resulted, nil)) then
          begin
            device.Devices[device.NoOfDevices].HaID := scsiaddress.PortNumber;
            device.Devices[device.NoOfDevices].Target := scsiaddress.TargetId;
            device.Devices[device.NoOfDevices].Lun := scsiaddress.Lun;
            device.Devices[device.NoOfDevices].DriveLetter := AnsiChar(i);
            device.Devices[device.NoOfDevices].DeviceType := Byte(DeviceType);
            index := device.NoOfDevices;
            inc(device.NoOfDevices);
            inc(drives);
          end
          else
          begin
             device.Devices[extdrives].HaID := scsiaddress.PortNumber;
             device.Devices[extdrives].Target := 0;
             device.Devices[extdrives].Lun := 0;
             device.Devices[extdrives].DriveLetter := AnsiChar(i);
             device.Devices[device.NoOfDevices].DeviceType := Byte(DeviceType);
             index := extdrives;
             dec(extdrives);
             inc(drives);
          end;
          device.DriveLetter := AnsiChar(i);
          device.Handle := fh;
          FillChar(srbExec, sizeof(TSRB_ExecSCSICmd), 0);
          srbExec.SRB_Flags := SRB_DIR_IN or SRB_EVENT_NOTIFY;
          srbExec.SRB_BufLen := 64;
          srbExec.SRB_BufPointer := @device.tmpBuffer[0];
          srbExec.SRB_CDBLen := 6;
          srbExec.CDBByte[0] := SCSI_INQUIRY;
          srbExec.CDBByte[4] := 64;
          srbExec.SRB_SenseLen := SENSE_LEN;
          srbExec.SRB_Cmd := SC_EXEC_SCSI_CMD;
          SPTISendASPI32Command(device, @srbExec, FALSE);
          if (Device.fdbg <> nil) then
          begin
            sdbg := AnsiString(buf[4] + ': ' + IntToStr(GetDriveTypeA(@buf[4])) + ' - ( ');
            Device.fdbg.Write(sdbg[1], Length(sdbg));
          end;
          if ((srbExec.SRB_Status = 1) and (device.tmpBuffer[0] = #5)) then
          begin
            for j := 8 to 47 do
              if (device.tmpBuffer[j] < #32) then device.tmpBuffer[j] := #32;
            Move(device.tmpBuffer[8], vendor, 8);
            vendor[8] := AnsiChar(0);
            Move(device.tmpBuffer[16], productid, 16);
            productid[16] := AnsiChar(0);
            Move(device.tmpBuffer[32], revision, 16);
            revision[4] := AnsiChar(0);
            tmpstr := AnsiString(Format('%d:%d:%d,%s: %s %s %s ', [scsiaddress.PortNumber, scsiaddress.TargetId, scsiaddress.Lun, device.DriveLetter, vendor, productid, revision]));
            StrCopy(device.Devices[index].Data, PAnsiChar(tmpstr));
          end;
          if (Device.fdbg <> nil) then
          begin
            sdbg := Device.Devices[index].Data;
            Device.fdbg.Write(sdbg[1], Length(sdbg));
          end;
          tmpstr := AnsiString(Format('%d:%d:%d,%s: %s %s %s ', [scsiaddress.PortNumber, scsiaddress.TargetId, scsiaddress.Lun, device.DriveLetter, vendor, productid, revision]));
          StrCopy(device.Devices[index].Data, PAnsiChar(tmpstr));
          CloseHandle(device.Handle);
          device.Handle := INVALID_HANDLE_VALUE;
        end
        else
        begin
          sdbg := AnsiString('System Error Code: ' + IntToStr(GetLastError) + #13#10);
          Device.fdbg.Write(sdbg[1], Length(sdbg));
        end;
      end
      else
      begin
        if (Device.fdbg <> nil) then
        begin
          sdbg := AnsiString(buf[4] + ': ( ' + IntToStr(GetDriveTypeA(@buf[4])));
          Device.fdbg.Write(sdbg[1], Length(sdbg));
        end;
      end;

      if (Device.fdbg <> nil) then
      begin
        sdbg := ')'#13#10;
        Device.fdbg.Write(sdbg[1], Length(sdbg));
      end;
    end;
  end;

  if (drives <> 0) then
  begin
    device.ASPIInitialized := TRUE;
    device.UsingSPTI := TRUE;
    extdrives := 25-extdrives;
    if (extdrives <> 25) then
    begin
      MaxHaId := 0;
      for i:=0 to Pred(device.NoOfDevices) do
      begin
        if (device.Devices[i].HaID > MaxHaId) then
          MaxHaId := device.Devices[i].HaID;
      end;
      inc(MaxHaId);
      for i:=25 downto Succ(25-extdrives) do
      begin
        device.Devices[i].HaID := MaxHaId;
        inc(MaxHaId);
        device.Devices[i].Data[0] := AnsiChar(ord('0')+device.Devices[i].HaID);
        device.Devices[i].Data[2] := '0';
        device.Devices[i].Data[4] := '0';
        Move(device.Devices[i], device.Devices[device.NoOfDevices+(25-i)], sizeof(device.Devices[0]));
      end;
      //Move(device.Devices[25-extdrives+1], device.Devices[device.NoOfDevices], sizeof(device.Devices[0])*extdrives);
    end;
    device.NoOfDevices := device.NoOfDevices + extdrives;
    result := TRUE;
    exit;
  end;

  device.NoOfDevices := 0;
  if (wnASPIDLLPath = '') then
  begin
    hWNASPI32 := LoadLibrary('WNASPI32.DLL');
  end
  else
  begin
    hWNASPI32 := LoadLibrary(@wnASPIDLLPath[1]);
  end;
  if (hWNASPI32 = 0) then
  begin
    Result := FALSE;
    Exit;
  end;

  GetASPI32SupportInfo :=  GetProcAddress(hWNASPI32,'GetASPI32SupportInfo');
  SendASPI32Command := GetProcAddress(hWNASPI32,'SendASPI32Command');
  device.SendASPI32Command := @SendASPI32Command;

  if (not Assigned(GetASPI32SupportInfo)) or (not Assigned(SendASPI32Command)) then
  begin
    Result := FALSE;
    Exit;
  end;
  SupportInfo := GetASPI32SupportInfo;
  if (SupportInfo = 0) then
  begin
    Result := FALSE;
    Exit;
  end;
  device.ASPIInitialized := TRUE;
  //--------------------------------------------------------------------------
  FillChar(srbHaInquiry, sizeof(srbHaInquiry), 0);
  srbHaInquiry.SRB_Cmd := SC_HA_INQUIRY;
  Resulted := SendASPI32Command(@srbHaInquiry);
  if (srbHaInquiry.HA_Count = 0) then
    srbHaInquiry.HA_Count := 7;
  for HaID:=0 to Pred(srbHaInquiry.HA_Count) do
  begin
    for Target:=0 to Pred(8) do
    begin
      for Lun:=0 to Pred(7) do
      begin
        heventSRB := CreateEvent(nil, TRUE, FALSE, nil);

        FillChar(srbExec, 0, sizeof(TSRB_ExecSCSICmd));
        srbExec.SRB_Flags := SRB_DIR_IN or SRB_EVENT_NOTIFY;
        srbExec.SRB_BufLen := 64;
        srbExec.SRB_BufPointer := @device.tmpBuffer[0];
        srbExec.SRB_CDBLen := 6;
        srbExec.CDBByte[0] := SCSI_INQUIRY;
        srbExec.CDBByte[4] := 64;
        srbExec.SRB_HaId := HaID;
        srbExec.SRB_Target := Target;
        srbExec.SRB_Lun := Lun;
        srbExec.SRB_SenseLen := SENSE_LEN;
        srbExec.SRB_Cmd := SC_EXEC_SCSI_CMD;
        srbExec.SRB_PostProc := Pointer(heventSRB);
        dwASPIStatus := SendASPI32Command(@srbExec);

        if(dwASPIStatus = SS_PENDING) then
        begin
          WaitForSingleObject(heventSRB, {1000*10}INFINITE);
          ResetEvent(heventSRB);
          CloseHandle(heventSRB);
        end
        else
        begin
          ResetEvent(heventSRB);
          CloseHandle(heventSRB);
        end;
        if ((srbExec.SRB_Status = 1) and (device.tmpBuffer[0] = #5)) then
        begin
          for i:=8 to Pred(48) do
            if (device.tmpBuffer[i] < #32) then device.tmpBuffer[i] := #32;
          Move(device.tmpBuffer[8], vendor, 8);
          vendor[8] := AnsiChar(0);
          move(device.tmpBuffer[16], productid, 16);
          productid[16] := AnsiChar(0);
          move(device.tmpBuffer[32], revision, 16);
          revision[4] := AnsiChar(0);
          device.Devices[device.NoOfDevices].HaID := HaID;
          device.Devices[device.NoOfDevices].Target := Target;
          device.Devices[device.NoOfDevices].Lun := Lun;
          device.Devices[device.NoOfDevices].DeviceType := Ord(device.tmpBuffer[0]);
          device.DriveLetter := GetDeviceLetter(device);
          if ((device.DriveLetter = #128) or (device.DriveLetter = #129)) then
            device.DriveLetter := '?';
          device.Devices[device.NoOfDevices].DriveLetter := device.DriveLetter;
          if (device.DriveLetter <> #0) then
          begin
            device.srbdi.SRB_Cmd := SC_GET_DISK_INFO;
            device.srbdi.SRB_HaId := HaID;
            device.srbdi.SRB_Target := Target;
            device.srbdi.SRB_Lun := Lun;
            dwASPIStatus := SendASPI32Command(@device.srbdi);
            if (dwASPIStatus = SS_COMP) then
            begin
              device.DriveLetter := AnsiChar(device.srbdi.SRB_Int13HDriveInfo+65);
              device.Devices[device.NoOfDevices].DriveLetter := device.DriveLetter;
            end;
            tmpstr := AnsiString(Format('%d:%d:%d,%s: %s %s %s ', [HaID, Target, Lun, device.DriveLetter, vendor, productid, revision]));
            StrCopy(device.Devices[device.NoOfDevices].Data, PAnsiChar(tmpstr));
            inc(device.NoOfDevices);
          end;

        end;
      end;
    end;
  end;
  Result := TRUE;
end;
{
*******************************************************************************
*                                                                             *
*******************************************************************************
}
function ExecSCSICommand(var device: TSCSIDEVICE; srbExec: PSRB_ExecSCSICmd): Byte; //Windows
var
  heventSRB: THANDLE;
  dwASPIStatus: BYTE;
  st: TSYSTEMTIME;
  i: Integer;
  sdbg: AnsiString;
begin
  if (not Device.ASPIInitialized) then
  begin
    //.. set error and
    srbExec.SRB_Status := 4;
    result := 4;
    exit;
  end;
  if (device.HaID = 255) or (device.Target = 255){ or (device.DeviceType <> 5)} then
  begin
    //.. set error and
    srbExec.SRB_Status := 4;
    result := 4;
    exit;
  end;
  dwASPIStatus := 0;
  heventSRB := CreateEvent(nil, TRUE, FALSE, nil);
  {$IFDEF CONDEBUG}
  if(not heventSRB)
    writeln('Event creation failed');
  {$ENDIF}
  srbExec.SRB_HaId := device.HaID;
  srbExec.SRB_Target := device.Target;
  srbExec.SRB_Lun := device.Lun;
  srbExec.SRB_SenseLen := SENSE_LEN;
  srbExec.SRB_Cmd := SC_EXEC_SCSI_CMD;
  srbExec.SRB_PostProc := Tprocedure(heventSRB);
  inc(srbExec.CDBByte[0]);
  if (device.TV = TRUE) and (srbExec.CDBByte[0] = $2b) then
  begin
    srbExec.CDBByte[2] := 0;
    srbExec.CDBByte[3] := 0;
  end;
  dec(srbExec.CDBByte[0]);
  {if (((srbExec.CDBByte[0] = $2a) or (srbExec.CDBByte[0] = $5b) or (srbExec.CDBByte[0] = $53))) then
  begin
    dwASPIStatus := 1;
    srbExec.SRB_Status := 1;
  end;}
  if (dwASPIStatus <> 1) then
  begin
    if (device.UsingSPTI = TRUE) then
      dwASPIStatus := SPTISendASPI32Command(device, srbExec, FALSE)
    else
      dwASPIStatus := SendASPI32Command(srbExec);
  end;
  if (dwASPIStatus = SS_PENDING) then
  begin
    WaitForSingleObject(heventSRB, INFINITE);
    ResetEvent(heventSRB);
    CloseHandle(heventSRB);
  end
  else
  begin
    ResetEvent(heventSRB);
    CloseHandle(heventSRB);
  end;

  if (Device.fdbg <> nil) then
  begin
    GetSystemTime(st);
    if (Integer(srbExec.SRB_BufPointer) mod 4 <> 0) then
      sdbg := '- NAL -> ' + AnsiString(Integer(srbExec.SRB_BufPointer)) + ' ';
    sdbg := sdbg + AnsiString(Format('%0.2d:%0.2d:%0.3d %s (%0.2x) {%0.2x:%0.2x} - %0.1x:%0.1x:%0.1x <%0.2x> [%0.5x] ', [st.wMinute, st.wSecond, st.wMilliseconds, Device.cn[srbExec.CDBByte[0]], srbExec.CDBByte[0], srbExec.SRB_Flags, srbExec.SRB_CDBLen, srbExec.SRB_HaId, srbExec.SRB_Target, srbExec.SRB_Lun, srbExec.SRB_Status, srbExec.SRB_BufLen]));
    for i := 1 to Pred(16) do
      sdbg := sdbg + AnsiString(Format('%0.2x ', [srbExec.CDBByte[i]]));
    sdbg := sdbg + '| ';
    for i := 0 to Pred(Min(100, srbExec.SRB_BufLen)) do
      sdbg := sdbg + AnsiString(Format('%0.2x ', [Ord(srbExec.SRB_BufPointer[i])]));
    sdbg := sdbg + AnsiString(Format('%0.2x:%0.2x', [srbExec.SenseArea.AddSenseCode, srbExec.SenseArea.AddSenQual]));
    if (Device.UsingSPTI) then
      sdbg := sdbg + ', ' + AnsiString(IntToStr(Device.LastWinError));
    sdbg := sdbg + AnsiString(#13#10);
    Device.fdbg.Write(sdbg[1], Length(sdbg));
  end;
  if (((dwASPIStatus <> SS_PENDING) and (dwASPIStatus <> SS_COMP)) and (srbExec.SRB_TargStat = 8)) then
    device.TargetBusy := TRUE
  else
    device.TargetBusy := FALSE;
  move(srbExec.SenseArea, device.LastSenseInfo, sizeof(TSENSE));
  if (device.LastSenseInfo.AddSenseCode <> 0) then
  begin
    device.DeviceError := device.LastSenseInfo.AddSenseCode * 256 + device.LastSenseInfo.AddSenQual;
  end;
  Result := srbExec.SRB_Status;
end;
{$ELSE}
function ExecSCSICommand(var device: TSCSIDEVICE; srbExec: PSRB_ExecSCSICmd): Byte; // Linux
var
  status: Integer;
  sg_hd: PSGHeader;
  buf: array[0..12] of AnsiChar;
begin
  result := 4;
  if (device.Handle = INVALID_HANDLE_VALUE) then
  begin
    strcopy(buf,'/dev/sgX');
    buf[7] := device.Devices[device.SelectedDevice].DriveLetter;

    device.Handle := fileopen(buf, fmOpenReadWrite);
  end;
  fillchar(tmpWorkingBufferToDevice[0], 128, 0);
  fillchar(tmpWorkingBufferFromDevice[0], 128, 0);
  sg_hd := @tmpWorkingBufferToDevice;
  sg_hd.ReplyLen := SCSI_OFF + srbExec.SRB_BufLen + srbExec.SRB_CDBLen;
  sg_hd.Params := 0;
  move(srbExec.CDBByte[0], tmpWorkingBufferToDevice[SCSI_OFF], srbExec.SRB_CDBLen);
  if (srbExec.SRB_Flags = $10) or (srbExec.SRB_Flags = $50) then
    move(srbExec.SRB_BufPointer[0], tmpWorkingBufferToDevice[SCSI_OFF], srbExec.SRB_BufLen);
  status := filewrite(device.Handle, tmpWorkingBufferToDevice, sg_hd.ReplyLen);
  if (status < 0) or (status <> sg_hd.ReplyLen) or (sg_hd.result <> 0) then
  begin
    exit;
  end;
  status := fileread(device.Handle, tmpWorkingBufferFromDevice[0], SCSI_OFF + srbExec.SRB_BufLen);
  sg_hd := @tmpWorkingBufferFromDevice;
  move(sg_hd.Sense, srbExec.SenseArea, 16);
  if (srbExec.SenseArea.ErrorCode <> 0) then
  begin
    result := 4;
    srbExec.SRB_Status := result;
    exit;
  end;
  if (status < 0) or (status <> SCSI_OFF + srbExec.SRB_BufLen) or (sg_hd.result <> 0) then
  begin
    exit;
  end
  else
  begin
    if (srbExec.SRB_Flags = $08) or (srbExec.SRB_Flags = $48) then
      move(tmpWorkingBufferFromDevice[SCSI_OFF], srbExec.SRB_BufPointer[0], srbExec.SRB_BufLen);
  end;
  result := 1;
  srbExec.SRB_Status := result;
end;
{
*******************************************************************************
*                                                                             *
*******************************************************************************
}
function InitializeASPI(var device: TSCSIDEVICE; Reserved1: AnsiString = ''): Boolean;  // Linux
var
  scsiaddress: Tscsiaddress;
  Resulted, SupportInfo: Longword;
  hWNASPI32: HMODULE;
  heventSRB: THandle;
  dwASPIStatus: Longword;
  fh: thandle;
  srbExec: TSRB_ExecSCSICmd;
  srbHaInquiry: TSRB_HAInquiry;
  buf: array [0..10] of AnsiChar;
  HaID, Target, Lun, MaxHaId: Byte;
  hid, i, j, extdrives, index: Integer;
  productid: array[0..16] of AnsiChar;
  vendor: array[0..16] of AnsiChar;
  revision: array[0..16] of AnsiChar;
  pProc: Pointer;
  tmpstr: AnsiString;
begin
  FillChar(device, sizeof(device), 0);
  strcopy(buf,'/dev/sgX');
  device.NoOfDevices := 0;
  extdrives := 25;
  device.DoNotUseSPTI := FALSE;
  Target := 0;
  Lun := 0;
  hid := 0;
  if (not device.DoNotUseSPTI) then
  begin
    for haID := ord('a') to ord('z') do
    begin
      buf[7] := AnsiChar(haID);
      fh := fileopen(buf, fmOpenReadWrite);
      device.Handle := fh;
      FillChar(srbExec, sizeof(TSRB_ExecSCSICmd), 0);
      srbExec.SRB_Flags := SRB_DIR_IN or SRB_EVENT_NOTIFY;
      srbExec.SRB_BufLen := 64;
      srbExec.SRB_BufPointer := @device.tmpBuffer[0];
      srbExec.SRB_CDBLen := 6;
      srbExec.CDBByte[0] := SCSI_INQUIRY;
      srbExec.CDBByte[4] := 64;
      srbExec.SRB_HaId := HaID;
      srbExec.SRB_Target := Target;
      srbExec.SRB_Lun := Lun;
      srbExec.SRB_SenseLen := SENSE_LEN;
      srbExec.SRB_Cmd := SC_EXEC_SCSI_CMD;
      srbExec.SRB_PostProc := Pointer(heventSRB);
      dwASPIStatus := ExecSCSICommand(device, @srbExec);
      if ((srbExec.SRB_Status = 1) and (device.tmpBuffer[0] = #5)) then
      begin
        for i:=8 to Pred(48) do
          if (device.tmpBuffer[i] < #32) then device.tmpBuffer[i] := #32;
        Move(device.tmpBuffer[8], vendor, 8);
        vendor[8] := AnsiChar(0);
        move(device.tmpBuffer[16], productid, 16);
        productid[16] := AnsiChar(0);
        move(device.tmpBuffer[32], revision, 16);
        revision[4] := AnsiChar(0);
        device.Devices[device.NoOfDevices].HaID := HaID;
        device.Devices[device.NoOfDevices].Target := Target;
        device.Devices[device.NoOfDevices].Lun := Lun;
        device.DriveLetter := ord(HaID);
        if (ord(device.DriveLetter) = -127) then
          device.DriveLetter := ord('?');
        device.Devices[device.NoOfDevices].DriveLetter := AnsiChar(device.DriveLetter);
        tmpstr := Format('%d:%d:%d,%s: %s %s %s ', [hid, Target, Lun, AnsiChar(device.DriveLetter), vendor, productid, revision]);
        StrCopy(device.Devices[device.NoOfDevices].Data, PAnsiChar(tmpstr)); inc(device.NoOfDevices);
        fileclose(fh);
        fh := INVALID_HANDLE_VALUE;
        device.Handle := fh;
        inc(Target);
        if (Target > 7) then
        begin
          Target := 0;
          inc(hid);
        end;
      end;
    end;
  end;
  result := True;
end;
{$ENDIF}
{
*******************************************************************************
*                                                                             *
*******************************************************************************
}
function SelectDevice(var device: TSCSIDEVICE; Id: Byte): Boolean;
var
  inqdata: PAnsiChar;
begin
  device.HaID := 255;
  device.Target := 255;
  device.Lun := 255;
  device.DeviceType := 255;
  device.DriveLetter := '?';
  device.bmWTotal := 0;
  device.bmWSize := 0;
  device.bmRTotal := 0;
  device.bmRSize := 0;

  if (Id >= device.NoOfDevices) then
  begin
    Result := FALSE;
    Exit;
  end;
  inqdata := AllocMem(256);
  device.SelectedDevice := Id;

  device.HaID := device.Devices[Id].HaID;
  device.Target := device.Devices[Id].Target;
  device.Lun := device.Devices[Id].Lun;
  device.DriveLetter := device.Devices[Id].DriveLetter;
  device.DeviceType := device.Devices[Id].DeviceType;
  if ((device.Handle <> 0) and (device.Handle <> INVALID_HANDLE_VALUE)) then
  begin
    {$IFNDEF LINUX}
    CloseHandle(device.Handle);
    {$ELSE}
    fileclose(device.Handle);
    {$ENDIF}
  end;
  device.Handle := INVALID_HANDLE_VALUE;
  if (Inquiry(device, inqdata) = True) then
  begin
    move(inqdata[8], device.Vendor, 8);
    device.Vendor[8] := AnsiChar(0);
    move(inqdata[16], device.ProductID, 16);
    device.ProductID[16] := AnsiChar(0);
    GetDeviceCapabilities(device);

    freemem(inqdata);
    Result := TRUE;
  end
  else
  begin
    GetDeviceCapabilities(device);
    if (inqdata <> nil)  then
      freemem(inqdata);
    Result := FALSE;
  end;
  
end;

{
*******************************************************************************
*                                                                             *
*******************************************************************************
}
function SelectDeviceByLetter(var Device: TSCSIDevice; DriveLetter: AnsiChar): Boolean;
var
  DeviceID: Byte;
begin
  Result := False;

  if ((Device.NoOfDevices = 0) or (Ord(DriveLetter) = 0)) then
  begin
    Exit;
  end;

  for DeviceID := 0 to Pred(Device.NoOfDevices) do
  begin
    if (Device.Devices[DeviceID].DriveLetter = DriveLetter) then
    begin
      Result := SelectDevice(Device, DeviceID);
    end;
  end;
end;

{
*******************************************************************************
*                                                                             *
*******************************************************************************
}
function GetDeviceName(var device: TSCSIDEVICE; Id: Byte): PAnsiChar;
begin
  if (Id >= device.NoOfDevices) then
    Result := nil
  else
    Result := device.Devices[Id].Data;
end;

{*******************************************************************************
*                                                                              *
*******************************************************************************}
function TestUnitReady(var Device: TSCSIDevice): Boolean;
var
  srbExec: TSRB_ExecSCSICmd;
begin
  fillchar( srbExec, sizeof(TSRB_ExecSCSICmd), 0 );
  srbExec.SRB_Flags :=  SRB_EVENT_NOTIFY;
  srbExec.SRB_CDBLen := 6;
  srbExec.CDBByte[0] := 0;
  ExecSCSICommand(Device, @srbExec );
  result := (srbExec.SRB_Status = SS_COMP);

end;

{*******************************************************************************
*                                                                              *
*******************************************************************************}
function Inquiry(var Device: TSCSIDevice; InquiryData: PAnsiChar): Boolean;
var
  srbExec: TSRB_ExecSCSICmd;
begin
  fillchar( srbExec, sizeof(TSRB_ExecSCSICmd), 0 );
  srbExec.SRB_Flags := SRB_DIR_IN or SRB_EVENT_NOTIFY;
  srbExec.SRB_BufLen := 64;
  srbExec.SRB_BufPointer := InquiryData;
  srbExec.SRB_CDBLen := 6;
  srbExec.CDBByte[0] := SCSI_INQUIRY;
  srbExec.CDBByte[4] := 64;
  ExecSCSICommand(Device, @srbExec);
  result := srbExec.SRB_Status = SS_COMP;
end;

{*******************************************************************************
*                                                                              *
*******************************************************************************}
function LoadMedium(var Device: TSCSIDevice; Immediate: Boolean): Boolean;
var
  srbExec: TSRB_ExecSCSICmd;
begin
  fillchar( srbExec, sizeof(TSRB_ExecSCSICmd), 0 );
  srbExec.SRB_Flags :=  SRB_EVENT_NOTIFY;
  srbExec.SRB_CDBLen := 6;
  srbExec.CDBByte[0] := SCSI_LOAD_UN;
  srbExec.CDBByte[1] := IfThen((Immediate <> False), 1, 0);
  srbExec.CDBByte[4] := 3;
  ExecSCSICommand(Device, @srbExec );
  result := srbExec.SRB_Status = SS_COMP;

end;

{*******************************************************************************
*                                                                              *
*******************************************************************************}
function EjectMedium(var Device: TSCSIDevice; Immediate: Boolean): Boolean;
var
  srbExec: TSRB_ExecSCSICmd;
begin
  fillchar(srbExec, sizeof(TSRB_ExecSCSICmd), 0 );
  srbExec.SRB_Flags :=  SRB_EVENT_NOTIFY;
  srbExec.SRB_CDBLen := 6;
  srbExec.CDBByte[0] := SCSI_LOAD_UN;
  srbExec.CDBByte[1] := IfThen((Immediate <> False), 1, 0);
  srbExec.CDBByte[4] := 2;
  ExecSCSICommand(Device, @srbExec );
  result := srbExec.SRB_Status = SS_COMP;
end;

{*******************************************************************************
*                                                                              *
*******************************************************************************}
function ModeSelect(var Device: TSCSIDevice; Buffer: PAnsiChar; BufferLength: Word): Boolean;
var
  srbExec: TSRB_ExecSCSICmd;
begin
  fillchar( srbExec, sizeof(TSRB_ExecSCSICmd), 0 );
  srbExec.SRB_Flags :=  SRB_DIR_OUT or SRB_EVENT_NOTIFY;
  srbExec.SRB_CDBLen := 10;
  srbExec.SRB_BufPointer := Buffer;
  srbExec.SRB_BufLen := BufferLength;
  srbExec.CDBByte[0] := SCSI_MODE_SEL10;
  srbExec.CDBByte[1] := $10;
  CvtEndians2(@BufferLength, @srbExec.CDBByte[7]);
  ExecSCSICommand(Device, @srbExec );
  result := (srbExec.SRB_Status = SS_COMP) or (srbExec.SRB_Status = SS_PENDING)

end;

{*******************************************************************************
*                                                                              *
*******************************************************************************}
function ModeSense10(var Device: TSCSIDevice; Buffer: PAnsiChar; BufferLength: Word; PageCode: Byte; PS: Boolean): Boolean;
var
  srbExec: TSRB_ExecSCSICmd;
begin
  fillchar( srbExec, sizeof(TSRB_ExecSCSICmd), 0 );
  srbExec.SRB_Flags :=  SRB_DIR_IN or SRB_EVENT_NOTIFY;
  srbExec.SRB_CDBLen := 10;
  srbExec.SRB_BufLen := BufferLength;
  srbExec.SRB_BufPointer := Buffer;
  srbExec.CDBByte[0] := SCSI_MODE_SEN10;
  srbExec.CDBByte[2] := PageCode;
  if PS then
    inc(srbExec.CDBByte[2], 128);
  CvtEndians2(@BufferLength, @srbExec.CDBByte[7]);
  ExecSCSICommand(Device, @srbExec );
  result := (srbExec.SRB_Status = SS_COMP) or (srbExec.SRB_Status = SS_PENDING)

end;

{*******************************************************************************
*                                                                              *
*******************************************************************************}
function JustLinkCapable(var Device: TSCSIDevice): Boolean;
begin
  result := ModeSense10(Device, @Device.tmpBuffer, 128, $30, True);
end;

{*******************************************************************************
*                                                                              *
*******************************************************************************}
function CloseDVDTrack(var Device: TSCSIDevice; Immediate: Boolean; b2, b3, b4, b5: Byte): Boolean;
var
  srbExec: TSRB_ExecSCSICmd;
begin
  fillchar( srbExec, sizeof(TSRB_ExecSCSICmd), 0 );
  srbExec.SRB_Flags :=  SRB_EVENT_NOTIFY;
  srbExec.SRB_CDBLen := 10;
  srbExec.CDBByte[0] := SCSI_CLOSETRK;
  srbExec.CDBByte[1] := IfThen((Immediate <> False), 1, 0);
  srbExec.CDBByte[2] := b2;
  srbExec.CDBByte[3] := b3;
  srbExec.CDBByte[4] := b4;
  srbExec.CDBByte[5] := b5;
  ExecSCSICommand(Device, @srbExec );
  result := (srbExec.SRB_Status = SS_COMP) or (srbExec.SRB_Status = SS_PENDING)

end;

{*******************************************************************************
*                                                                              *
*******************************************************************************}
function LockMedium(var Device: TSCSIDevice): Boolean;
var
  srbExec: TSRB_ExecSCSICmd;
begin
  fillchar( srbExec, sizeof(TSRB_ExecSCSICmd), 0 );
  srbExec.SRB_Flags :=  SRB_EVENT_NOTIFY;
  srbExec.SRB_CDBLen := 6;
  srbExec.CDBByte[0] := SCSI_MED_REMOVL;
  srbExec.CDBByte[4] := 1;
  ExecSCSICommand(Device, @srbExec );
  result := (srbExec.SRB_Status = SS_COMP);

end;

{*******************************************************************************
*                                                                              *
*******************************************************************************}
function UnlockMedium(var Device: TSCSIDevice): Boolean;
var
  srbExec: TSRB_ExecSCSICmd;
begin
  fillchar( srbExec, sizeof(TSRB_ExecSCSICmd), 0 );
  srbExec.SRB_Flags :=  SRB_EVENT_NOTIFY;
  srbExec.SRB_CDBLen := 6;
  srbExec.CDBByte[0] := SCSI_MED_REMOVL;
  srbExec.CDBByte[4] := 0;
  ExecSCSICommand(Device, @srbExec );
  result := (srbExec.SRB_Status = SS_COMP);

end;

{*******************************************************************************
*                                                                              *
*******************************************************************************}
function ReadBufferCapacity(var Device: TSCSIDevice): Boolean;
var
  srbExec: TSRB_ExecSCSICmd;
begin
  fillchar( srbExec, sizeof(TSRB_ExecSCSICmd), 0 );
  srbExec.SRB_Flags :=  SRB_DIR_IN or SRB_EVENT_NOTIFY;
  srbExec.SRB_CDBLen := 10;
  srbExec.SRB_BufLen := $20;
  srbExec.SRB_BufPointer := Device.tmpBuffer;
  srbExec.CDBByte[0] := SCSI_RD_BUFCAP;
  srbExec.CDBByte[8] := $20;
  ExecSCSICommand(Device, @srbExec );
  CvtEndians4(@Device.tmpBuffer[4], @Device.DeviceBufferSize);
  CvtEndians4(@Device.tmpBuffer[8], @Device.DeviceBufferPosition);
  result := (srbExec.SRB_Status = SS_COMP);

end;

{*******************************************************************************
*                                                                              *
*******************************************************************************}
function Read10(var Device: TSCSIDevice; Sector: LongWord; NoOfSectors: Integer; Buffer: Pointer): Boolean;
var
  srbExec: TSRB_ExecSCSICmd;
  tl: Word;
  i: Double;
  StartTime, EndTime: Int64;
begin
  Device.bmRStart := GetTickCount;
  if Device.bmAvgReadSpeed = 0 then
    Device.bmAvgReadSpeed := GetReadSpeed(Device);
  QueryPerformanceCounter(StartTime);
  tl := NoOfSectors;
  FillChar(srbExec, SizeOf(TSRB_ExecSCSICmd), 0);
  srbExec.SRB_Flags := SRB_DIR_IN or SRB_EVENT_NOTIFY;
  srbExec.SRB_BufLen := NoOfSectors * 2048;
  srbExec.SRB_BufPointer := Buffer;
  srbExec.SRB_CDBLen := $a;
  srbExec.CDBByte[0] := SCSI_READ10;
  CvtEndians4(@Sector, @srbExec.CDBByte[2]);
  CvtEndians2(@tl, @srbExec.CDBByte[7]);
  device.bmRSize := device.bmRSize + srbExec.SRB_BufLen;
  ExecSCSICommand(device, @srbExec);
  Result := (srbExec.SRB_Status = SS_COMP);
  Device.bmRStop := GetTickCount;
  Device.bmRTotal := Device.bmRTotal + (Device.bmRStop - Device.bmRStart);
  if ((Device.bmRTotal / 1000) > 0) then
    i := (((device.bmRSize / 1024) / (Device.bmRTotal / 1000)) / 150) * 150
  else
    i := 0;
  Device.bmAvgReadSpeed := Round(i);
  Device.bmRCounter := Device.bmRCounter + 1;
  QueryPerformanceCounter(EndTime);
  QPerfDeviceAdd(Device, EndTime-StartTime, NoOfSectors * 2048);

end;

{*******************************************************************************
*                                                                              *
*******************************************************************************}
function Write10(var Device: TSCSIDevice; Sector: LongWord; NoOfSectors: Word; Buffer: PAnsiChar; BufferLen: DWORD ): Boolean;
var
  srbExec: TSRB_ExecSCSICmd;
  i: Double;
  StartTime, EndTime: Int64;
{$IFDEF TRIAL}
  W: Word;

{$ENDIF}
begin
  if Device.bmAvgWriteSpeed = 0 then
    Device.bmAvgWriteSpeed := GetWriteSpeed(Device);;
  Device.bmWStart := GetTickCount;
  QueryPerformanceCounter(StartTime);
  fillchar( srbExec, sizeof(TSRB_ExecSCSICmd), 0 );
  {$IFDEF TRIAL}
  w := LoWord(Sector);
  {$ENDIF}
  srbExec.SRB_Flags :=  SRB_DIR_OUT or SRB_EVENT_NOTIFY;
  srbExec.SRB_CDBLen := 10;
  srbExec.SRB_BufLen := BufferLen;
  device.bmWSize := device.bmWSize + BufferLen;
  srbExec.SRB_BufPointer := Buffer;
  srbExec.CDBByte[0] := SCSI_WRITE10;
  {$IFDEF TRIAL}
  CvtEndians2(@w, @srbExec.CDBByte[4]);
  if ((Sector > $FFFF) and (Sector < $FFFF0000)) then
  begin
    srbExec.CDBByte[0] := SCSI_SYNC_CACHE;
  end;
  {$ELSE}
  CvtEndians4(@Sector , @srbExec.CDBByte[2]);
  {$ENDIF}
  CvtEndians2(@NoOfSectors, @srbExec.CDBByte[7]);
  ExecSCSICommand(Device, @srbExec );
  result := (srbExec.SRB_Status = SS_COMP);
  QueryPerformanceCounter(EndTime);
  QPerfDeviceAdd(device, EndTime-StartTime, BufferLen);
  Device.bmWStop := GetTickCount;
  Device.bmWTotal := Device.bmWTotal + (Device.bmWStop - Device.bmWStart);
  if ((Device.bmWTotal / 1000) > 0) then
    i := (((device.bmWSize / 1024) / (Device.bmWTotal / 1000)) / 150) * 150
  else
    i := 0;
  Device.bmAvgWriteSpeed := Round(i);
  Device.bmWCounter := Device.bmWCounter + 1;
  {  if ((Device.bmCounter > 100) or (Device.bmCounter < 0)) then
  begin
    Writeln(i:2:2, ' KBytes/Seconds         ');
    Device.bmCounter := 0;
    Device.bmTotal := 0;
    device.bmSize := 0;
  end;}
end;

{********************************************************************************
*                                                                               *
********************************************************************************}
function ModeSelect10(var Device: TSCSIDevice; Buffer: PAnsiChar; BufferLen: Word ): Boolean;
var
  srbExec: TSRB_ExecSCSICmd;
begin
  fillchar( srbExec, sizeof(TSRB_ExecSCSICmd), 0 );
  srbExec.SRB_Flags :=  SRB_DIR_OUT or SRB_EVENT_NOTIFY;
  srbExec.SRB_CDBLen := 10;
  srbExec.SRB_BufLen := BufferLen;
  srbExec.SRB_BufPointer := Buffer;
  srbExec.CDBByte[0] := SCSI_MODE_SEL10;
  srbExec.CDBByte[1] := $10;
  CvtEndians2(@BufferLen, @srbExec.CDBByte[7]);
  ExecSCSICommand(Device, @srbExec );
  result := (srbExec.SRB_Status = SS_COMP);

end;

{*******************************************************************************
*                                                                              *
*******************************************************************************}
function GetConfiguration(var Device: TSCSIDevice; Buffer: PAnsiChar; BufferLen: Word; ProfileCode: Word): Boolean;
var
  srbExec: TSRB_ExecSCSICmd;
begin
  fillchar( srbExec, sizeof(TSRB_ExecSCSICmd), 0 );
  srbExec.SRB_Flags :=  SRB_DIR_IN or SRB_EVENT_NOTIFY;
  srbExec.SRB_CDBLen := 10;
  srbExec.SRB_BufLen := BufferLen;
  srbExec.SRB_BufPointer := Buffer;
  srbExec.CDBByte[0] := SCSI_GET_CONFIG;
  srbExec.CDBByte[1] := 2;
  CvtEndians2(@ProfileCode, @srbExec.CDBByte[2]);
  CvtEndians2(@BufferLen, @srbExec.CDBByte[7]);
  ExecSCSICommand(Device, @srbExec );
  result := (srbExec.SRB_Status = SS_COMP);

end;

{*******************************************************************************
*                                                                              *
*******************************************************************************}
function FormatUnit(var Device: TSCSIDevice; FormatCode: Byte; Buffer: PAnsiChar; BufferLen: Word): Boolean;
var
  srbExec: TSRB_ExecSCSICmd;
begin
  fillchar( srbExec, sizeof(TSRB_ExecSCSICmd), 0 );
  srbExec.SRB_Flags :=  SRB_DIR_OUT or SRB_EVENT_NOTIFY;
  srbExec.SRB_CDBLen := 6;
  srbExec.SRB_BufLen := BufferLen;
  srbExec.SRB_BufPointer := Buffer;
  srbExec.CDBByte[0] := SCSI_FORMAT;
  srbExec.CDBByte[1] := FormatCode;
  ExecSCSICommand(Device, @srbExec );
  Result := (srbExec.SRB_Status = SS_COMP) or (srbExec.SRB_Status = SS_PENDING);

end;

{*******************************************************************************
*                                                                              *
*******************************************************************************}
function RequestSense(var Device: TSCSIDevice; Buffer: PAnsiChar; BufferLen: Byte): Boolean;
var
  srbExec: TSRB_ExecSCSICmd;
begin
  fillchar( srbExec, sizeof(TSRB_ExecSCSICmd), 0 );
  srbExec.SRB_Flags :=  SRB_DIR_IN or SRB_EVENT_NOTIFY;
  srbExec.SRB_CDBLen := 6;
  srbExec.SRB_BufLen := BufferLen;
  srbExec.SRB_BufPointer := Buffer;
  srbExec.CDBByte[0] := SCSI_REQ_SENSE;
  srbExec.CDBByte[4] := BufferLen;
  ExecSCSICommand(Device, @srbExec );
  result := (srbExec.SRB_Status = SS_COMP);

end;

{*******************************************************************************
*                                                                              *
*******************************************************************************}
function ReadDiscInformation(var Device: TSCSIDevice; Buffer: PAnsiChar; BufferLen: Word): Boolean;
var
  srbExec: TSRB_ExecSCSICmd;
begin
  fillchar( srbExec, sizeof(TSRB_ExecSCSICmd), 0 );
  srbExec.SRB_Flags :=  SRB_DIR_IN or SRB_EVENT_NOTIFY;
  srbExec.SRB_CDBLen := 10;
  srbExec.SRB_BufLen := BufferLen;
  srbExec.SRB_BufPointer := Buffer;
  srbExec.CDBByte[0] := SCSI_READ_DSKI;
  CvtEndians2(@BufferLen, @srbExec.CDBByte[7]);
  ExecSCSICommand(Device, @srbExec );
  result := (srbExec.SRB_Status = SS_COMP);
end;

{*******************************************************************************
*                                                                              *
*******************************************************************************}
function ReadTOC(var Device: TSCSIDevice; Format: Byte; TrackNumber: Byte; Time: Boolean; Buffer: PAnsiChar; BufferLen: Byte): Boolean;
var
  srbExec: TSRB_ExecSCSICmd;
begin
  fillchar( srbExec, sizeof(TSRB_ExecSCSICmd), 0 );
  srbExec.SRB_Flags :=  SRB_DIR_IN or SRB_EVENT_NOTIFY;
  srbExec.SRB_CDBLen := 10;
  srbExec.SRB_BufLen := BufferLen;
  srbExec.SRB_BufPointer := Buffer;
  srbExec.CDBByte[0] := SCSI_READ_TOC;
  if Time then
    srbExec.CDBByte[1] := 2
  else
    srbExec.CDBByte[1] := 0;
  srbExec.CDBByte[2] := Format;
  srbExec.CDBByte[6] := TrackNumber;
  srbExec.CDBByte[8] := BufferLen;

  ExecSCSICommand(Device, @srbExec );
  result := (srbExec.SRB_Status = SS_COMP);

end;

{*******************************************************************************
*                                                                              *
*******************************************************************************}
function GetDeviceCapabilities(var Device: TSCSIDevice): Boolean;
begin
    FillChar(Device.ModePage2A, SizeOf(Device.ModePage2A), 0);
    FillChar(Device.dc, SizeOf(Device.dc), 0);
    if ModeSense10(Device, @Device.tmpBuffer[0], 128, $2a, True) then
    begin
        if (Ord(Device.tmpBuffer[8]) = $2a) then
          Move(Device.tmpBuffer[8], Device.ModePage2A, sizeof(Device.ModePage2A)-12)
        else if (Ord(Device.tmpBuffer[16]) = $2a) then
          Move(Device.tmpBuffer[16], Device.ModePage2A, sizeof(Device.ModePage2A)-12);
        // WS
        Device.ModePage2A.DeviceMaxReadSpeed := ceil(cew(Device.ModePage2A.MaxReadSpeed));
        Device.ModePage2A.DeviceMaxWriteSpeed := ceil(cew(Device.ModePage2A.MaxWriteSpeed));
        // WS.

        ModeSense10(Device, Device.tmpBuffer, 128, $2a, False);
        if (Device.tmpBuffer[8] = #$2a) then
          move(Device.tmpBuffer[8], Device.ModePage2A, sizeof(Device.ModePage2A)-12)
        else if (Device.tmpBuffer[16] = #$2a) then
          move(Device.tmpBuffer[16], Device.ModePage2A, sizeof(Device.ModePage2A)-12);

        // WS
        Device.MaxReadSpeed := ceil(cew(Device.ModePage2A.MaxReadSpeed));
        Device.CurReadSpeed := ceil(cew(Device.ModePage2A.CurReadSpeed));
        Device.MaxWriteSpeed := ceil(cew(Device.ModePage2A.MaxWriteSpeed));
        Device.CurWriteSpeed := ceil(cew(Device.ModePage2A.CurWriteSpeed));

        Device.ModePage2A.MaxWriteSpeed := Ceil(cew(Device.ModePage2A.MaxWriteSpeed));
        Device.ModePage2A.CurWriteSpeed := Ceil(cew(Device.ModePage2A.CurWriteSpeed));
        Device.ModePage2A.MaxReadSpeed := Ceil(cew(Device.ModePage2A.MaxReadSpeed));
        Device.ModePage2A.CurReadSpeed := Ceil(cew(Device.ModePage2A.CurReadSpeed));
        // WS.
        Device.ModePage2A.BufferSize := cew(Device.ModePage2A.BufferSize);

        Device.dc.Read_CDR := Boolean(BitTest(Device.ModePage2A.ReadInfo, 0));
        Device.dc.Read_CDRW := Boolean(BitTest(Device.ModePage2A.ReadInfo, 1));
        Device.dc.Read_Method2 := Boolean(BitTest(Device.ModePage2A.ReadInfo, 2));
        Device.dc.Read_DVDROM := Boolean(BitTest(Device.ModePage2A.ReadInfo, 3));
        Device.dc.Read_DVDR := Boolean(BitTest(Device.ModePage2A.ReadInfo, 4));
        Device.dc.Read_DVDRAM := Boolean(BitTest(Device.ModePage2A.ReadInfo, 5));

        Device.dc.Write_CDR := Boolean(BitTest(Device.ModePage2A.WriteInfo, 0));
        Device.dc.Write_CDRW := Boolean(BitTest(Device.ModePage2A.WriteInfo, 1));
        Device.dc.TestWrite := Boolean(BitTest(Device.ModePage2A.WriteInfo, 2));
        Device.dc.Write_DVDR := Boolean(BitTest(Device.ModePage2A.WriteInfo, 4));
        Device.dc.Write_DVDRAM := Boolean(BitTest(Device.ModePage2A.WriteInfo, 5));
        Device.dc.Read_DVDPLUSR := False;
        Device.dc.Read_DVDPLUSRDL := False;
        Device.dc.Read_DVDPLUSRW := False;
        Device.dc.Write_DVDPLUSR := False;
        Device.dc.Write_DVDPLUSRDL := False;
        Device.dc.Write_DVDPLUSRW := False;
        Device.dc.Read_DVDRW := False;
        Device.dc.Write_DVDRW := False;

        with Device.dc do
        begin
          Read_BDROM := False;
          Read_BDR := False;
          Read_BDRE := False;
          Read_HDDVDROM := False;
          Read_HDDVDR := False;
          Read_HDDVDRAM := False;
          Write_BDR := False;
          Write_BDRE := False;
          Write_HDDVDR := False;
          Write_HDDVDRAM := False;
        end;

        Device.dc.UnderRunProtection := Boolean(BitTest(Device.ModePage2A.OtherInfo, 7));
        if (JustLinkCapable(Device) = True) then Device.dc.UnderRunProtection := True;

        if (GetConfiguration(Device, @Device.ConfigHeader, 112, $2a) = True) then
        begin
            if (((Device.ConfigHeader.CurrentProfile = 0) or (Device.ConfigHeader.FeatureCode = $2a00))) then //if (((Device.ConfigHeader.CurrentProfile = 0) or (Device.ConfigHeader.CurrentProfile = $2a00)) and (Device.ConfigHeader.FeatureCode = $2a00)) then
            begin
                Device.dc.Read_DVDPLUSR := True;
                Device.dc.Read_DVDPLUSRW := True;
                if (BitTest(Device.ConfigHeader.info, 0)= 1) then
                begin
                    Device.dc.Write_DVDPLUSR := True;
                    Device.dc.Write_DVDPLUSRW := True;
                end;
            end;
        end;
        if (GetConfiguration(Device, @Device.ConfigHeader, 112, $3b) = True) then
        begin
          if (Device.ConfigHeader.FeatureCode = $3b00) then
          begin
            Device.dc.Read_DVDPLUSRDL := True;
            if (BitTest(Device.ConfigHeader.info, 0) = 1) then
            begin
              Device.dc.Write_DVDPLUSRDL := True;
            end;
          end;
        end;
        if (GetConfiguration(Device, @Device.ConfigHeader, 112, $2f) = True) then
        begin
            if (((Device.ConfigHeader.CurrentProfile = 0) or (Device.ConfigHeader.FeatureCode = $2f00))) then //if (((Device.ConfigHeader.CurrentProfile = 0) or (Device.ConfigHeader.CurrentProfile = $2f00)) and (Device.ConfigHeader.FeatureCode = $2f00)) then
            begin
                //Device.dc.Read_DVDR := True;
                //Device.dc.Write_DVDR := True;
                if (BitTest(Device.ConfigHeader.info, 1) = 1) then
                begin
                    Device.dc.Read_DVDRW := True;
                    Device.dc.Write_DVDRW := True;
                end;
            end;
        end;
        if (GetConfiguration(Device, @Device.ConfigHeader, 112, $40) = True) then
        begin
            if (((Device.ConfigHeader.CurrentProfile = 0) or (Device.ConfigHeader.FeatureCode = $4000))) then
            begin
              Device.dc.Read_BDROM := True;
              Device.dc.Read_BDR := True;
              Device.dc.Read_BDRE := True;
            end;
        end;
        if (GetConfiguration(Device, @Device.ConfigHeader, 112, $41) = True) then
        begin
            if (((Device.ConfigHeader.CurrentProfile = 0) or (Device.ConfigHeader.FeatureCode = $4100))) then
            begin
              Device.dc.Write_BDR := True;
              Device.dc.Write_BDRE := True;
            end;
        end;
        if (GetConfiguration(Device, @Device.ConfigHeader, 112, $50) = True) then
        begin
            if (((Device.ConfigHeader.CurrentProfile = 0) or (Device.ConfigHeader.FeatureCode = $5000))) then
            begin
              Device.dc.Read_HDDVDROM := True;
              Device.dc.Read_HDDVDR := True;
              Device.dc.Read_HDDVDRAM := True;
            end;
        end;
        if (GetConfiguration(Device, @Device.ConfigHeader, 112, $51) = True) then
        begin
            if (((Device.ConfigHeader.CurrentProfile = 0) or (Device.ConfigHeader.FeatureCode = $5100))) then
            begin
              Device.dc.Write_HDDVDR := True;
              Device.dc.Write_HDDVDRAM := True;
            end;
        end;

        result := True;
    end
    else
      result := False;
end;

function DiscIs(var Device: TSCSIDevice): TMedium;
begin
   if (GetConfiguration(Device, @Device.ConfigHeader, sizeof(Device.ConfigHeader), 0)) then
   begin
     Device.ConfigHeader.CurrentProfile := cew(Device.ConfigHeader.CurrentProfile);
     case Device.ConfigHeader.CurrentProfile of
       $0008: result := mtCDROM;
       $0009: result := mtCDR;
       $000A: result := mtCDRW;
       $0010: result := mtDVDROM;
       $0011: result := mtDVDR;
       $0012: result := mtDVDRAM;
       $0013: result := mtDVDRWRO;
       $0014: result := mtDVDRWSR;
       $001A: result := mtDVDPLUSRW;
       $001B: result := mtDVDPLUSR;
       $002B: result := mtDVDPLUSRDL;
       $0040: result := mtBDROM;
       $0041: result := mtBDRSR;
       $0042: result := mtBDRRR;
       $0043: result := mtBDRE;
       $0050: result := mtHDDVDROM;
       $0051: result := mtHDDVDR;
       $0052: result := mtHDDVDRAM;
       $0020: result := mtDDCDROM;
       $0021: result := mtDDCDR;
       $0022: result := mtDDCDRW;
      else    result := mtUNKNOWN;
     end;
     Exit;
   end
   else
   begin
     if (ReadTOC(Device, 4, 0, True, Device.tmpBuffer, 30)) then
     begin
        if ((Ord(Device.tmpBuffer[6]) and 128) = 128) then
        begin
          if ((Ord(Device.tmpBuffer[6]) and 64) = 64) then
          begin
            result := mtCDRW;
            Exit;
          end
          else
          begin
            result := mtCDR;
            Exit;
          end;
        end
        else
        begin
          result := mtCDROM;
          Exit;
        end;
     end;
     if (ReadDiscInformation(Device, @Device.DiscInformation, sizeof(Device.DiscInformation)) = True) then
     begin
       if (Boolean(BitTest(Device.DiscInformation.info{Erasable}, 4)) = True) then
       begin
         result := mtCDRW;
         Exit;
       end
       else
       begin
         result := mtCDR;
         Exit;
       end;
     end
     else if (TestUnitReady(Device) = True) then
     begin
       result := mtCDROM;
       Exit;
     end;
   end;
   result := mtUnknown;
end;

(*******************************************************************************

*******************************************************************************)
function CloseTrack(var Device: TSCSIDevice; Track, Immediate: Boolean; TrackNumber: Byte): Boolean;
var
  srbExec: TSRB_ExecSCSICmd;
begin
  FillChar(srbExec, SizeOf(TSRB_ExecSCSICmd), 0);
  srbExec.SRB_Flags  :=  SRB_EVENT_NOTIFY;
  srbExec.SRB_CDBLen := 10;
  srbExec.CDBByte[0] := $5b;
  srbExec.CDBByte[1] := IfThen(Immediate, 1, 0);
  srbExec.CDBByte[2] := IfThen(Track, 1, 2);
  srbExec.CDBByte[5] := TrackNumber;
  ExecSCSICommand(Device, @srbExec);
  Result := (srbExec.SRB_Status = SS_COMP);
end;

(*******************************************************************************

*******************************************************************************)
function UnlockDrive(var Device: TSCSIDevice): Boolean;
begin
  TestUnitReady(Device);
  Result := True;
end;

(*******************************************************************************
*                              Set Write Parametes (TEMP)                            *
*******************************************************************************)
function SetWriteParams2(var Device: TSCSIDevice): Boolean;
var
  i: Integer;
begin

  FillChar(Device.ModePage05, SizeOf(Device.ModePage05), 0);
  if ModeSense10(Device, Device.tmpBuffer, 512, 5, False) then
  begin
    if (Device.tmpBuffer[8] = #5) then
      i := 8
    else
      i := 16;
    //Move(Device.tmpBuffer[i], Device.ModePage05,, SizeOf(Device.ModePage05));
    Device.ModePage05.PageCode := 5;
    Device.ModePage05.PageLen := Ord(Device.tmpBuffer[Succ(i)]);

    // setting first 4 MSBs to value 8
    Device.ModePage05.DBType := Device.ModePage05.DBType and $F;
    Device.ModePage05.DBType := Device.ModePage05.DBType or $80;
    Device.ModePage05.SessionFormat := 0;
    begin
      Device.ModePage05.WriteType := $0;
      Device.ModePage05.TrackMode := $25;
      //Device.ModePage05.TrackMode := Device.ModePage05.TrackMode or $40;
      Device.ModePage05.DBType := 8;
      Device.ModePage05.LinkSize := 0;
      Device.ModePage05.Res6 := 0;

      Device.ModePage05.PacketSize := $10000000;
    end;
    Device.ModePage05.PauseLength := $9600;
    Move(Device.ModePage05, Device.tmpBuffer[i], Device.ModePage05.PageLen);
    if ModeSelect(Device, Device.tmpBuffer, Device.ModePage05.PageLen+i+2) then
    begin
      Result := True;
      Exit;
    end;
  end;
  Result := False;
end;

(*******************************************************************************
*                              Set Write Parametes (TEMP)                            *
*******************************************************************************)
function SetWriteParams3(var Device: TSCSIDevice): Boolean;
var
  i: Integer;
begin

  FillChar(Device.ModePage05, SizeOf(Device.ModePage05), 0);
  if ModeSense10(Device, Device.tmpBuffer, 512, 5, False) then
  begin
    if (Device.tmpBuffer[8] = #5) then
      i := 8
    else
      i := 16;

    Device.ModePage05.PageCode := 5;
    Device.ModePage05.PageLen := Ord(Device.tmpBuffer[Succ(i)]);
    // setting first 4 MSBs to value 8
    Device.ModePage05.DBType := Device.ModePage05.DBType and $F;
    Device.ModePage05.DBType := Device.ModePage05.DBType or $80;
    Device.ModePage05.SessionFormat := 32;
    begin
      Device.ModePage05.WriteType := $1;
      Device.ModePage05.TrackMode := $C4;
      //Device.ModePage05.TrackMode := Device.ModePage05.TrackMode or $40;
      Device.ModePage05.DBType := 10;
      Device.ModePage05.LinkSize := 0;
      Device.ModePage05.Res6 := 0;

      Device.ModePage05.PacketSize := $10000000;
    end;
    Device.ModePage05.PauseLength := $9600;

    Move(Device.ModePage05, Device.tmpBuffer[i], Device.ModePage05.PageLen);
    if ModeSelect(Device, Device.tmpBuffer, Device.ModePage05.PageLen+i+2) then
    begin
      Result := True;
      Exit;
    end;
  end;
  Result := False;
end;

(*******************************************************************************
*                              Set Write Parametes                             *
*******************************************************************************)
function SetWriteParams(var Device: TSCSIDevice; TestWrite: Boolean; BUF: Boolean; AllowNextSession: Boolean; Medium: Byte): Boolean;
const
  buf2: array [0..30] of Byte = ($05, $32, $42, $C4, $08, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $96, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00);
var
  i: Integer;
  DAO: Boolean;

begin
  DAO := False;

  FillChar(Device.ModePage05, SizeOf(Device.ModePage05), 0);
  if ModeSense10(Device, Device.tmpBuffer, 512, 5, False) then
  begin

    if (Device.tmpBuffer[8] = #5) then
      i := 8
    else
      i := 16;
    Move(Buf2, Device.ModePage05, SizeOf(Device.ModePage05));
    Device.ModePage05.PageCode := 5;
    Device.ModePage05.PageLen := Ord(Device.tmpBuffer[Succ(i)]);

    // setting 4 LSBs to value 1
    Device.ModePage05.WriteType := Device.ModePage05.WriteType and $F0;  // TAO
    Device.ModePage05.WriteType := Device.ModePage05.WriteType or $1;  // TAO
    // setting 4 LSBs to value 8
    Device.ModePage05.DBType := Device.ModePage05.DBType and $F0;
    Device.ModePage05.DBType := Device.ModePage05.DBType or $8;
    Device.ModePage05.SessionFormat := 0;
    if Device.SonyPowerBurn <> #0 then
    begin
      // setting 6th bit to value 1
      Device.ModePage05.WriteType := Device.ModePage05.WriteType or $20;
    end;
    // setting 4 LSBs to value 4
    Device.ModePage05.TrackMode := Device.ModePage05.TrackMode and $F0;
    Device.ModePage05.TrackMode := Device.ModePage05.TrackMode or $4;
    if ((DAO and (TMedium(Medium) = mtDVDR)) or (TMedium(Medium) = mtDDCDR)  or (TMedium(Medium) = mtDDCDRW)) then
    begin
      // setting 4 LSBs to value 2
      Device.ModePage05.WriteType := Device.ModePage05.WriteType and $F0;
      Device.ModePage05.WriteType := Device.ModePage05.WriteType or $2;
      // setting 4 LSBs to value 5
      Device.ModePage05.TrackMode := Device.ModePage05.TrackMode and $F0;
      Device.ModePage05.TrackMode := Device.ModePage05.TrackMode or $5;
    end
    else
    if ((not DAO) and (TMedium(Medium) = mtDVDR)) then
    begin
      // setting 4 LSBs to value $20
      Device.ModePage05.WriteType := Device.ModePage05.WriteType and $F0;
      // setting 4 LSBs to value $05
      Device.ModePage05.TrackMode := Device.ModePage05.TrackMode and $F0;
      Device.ModePage05.TrackMode := Device.ModePage05.TrackMode or $5;
    end
    else
    if ((TMedium(Medium) = mtDVDRW) or (TMedium(Medium) = mtDVDRWRO) or (TMedium(Medium) = mtDVDRWSR)) then
    begin
      // setting 4 LSBs to value $0
      Device.ModePage05.WriteType := Device.ModePage05.WriteType and $F0;
      // setting 4 LSBs to value $5
      Device.ModePage05.TrackMode := Device.ModePage05.TrackMode and $F0;
      Device.ModePage05.TrackMode := Device.ModePage05.TrackMode or $5;
    end;

    if (not TestWrite) then
    begin
      if AllowNextSession then
      begin
        // setting 2 MSBs to value 3
        Device.ModePage05.TrackMode := Device.ModePage05.TrackMode or $C0;
      end
      else
      begin
        // setting last 2 MSBs to value 0
        Device.ModePage05.TrackMode := Device.ModePage05.TrackMode and $3F;
      end;
    end;
    if ((not DAO) and (TMedium(Medium) = mtDVDR)) then
    begin
      // setting 2 MSBs to value 3
      Device.ModePage05.TrackMode := Device.ModePage05.TrackMode or $C0;
      // setting 6th bit to value 1
      Device.ModePage05.TrackMode := Device.ModePage05.TrackMode or $20;

      Device.ModePage05.PacketSize := $10000000;
      Device.ModePage05.LinkSize := $10;

      // setting 6th bit to value 1
      Device.ModePage05.WriteType := Device.ModePage05.WriteType or $20;
    end
    else
    if ((not DAO) and ((TMedium(Medium) = mtDVDRWSR) or (TMedium(Medium) = mtDVDRWRO))) then
    begin
      // setting 2 MSBs to value 3
      Device.ModePage05.TrackMode := Device.ModePage05.TrackMode or $C0;
      // setting 6th bit to value 0
      Device.ModePage05.TrackMode := Device.ModePage05.TrackMode and $DF;

      Device.ModePage05.PacketSize := $10000000;
      Device.ModePage05.LinkSize := $10;

      // setting 6th bit to value 1
      Device.ModePage05.WriteType := Device.ModePage05.WriteType or $20;
    end;
    // setting 4 LSBs to value 8
    Device.ModePage05.DBType := Device.ModePage05.DBType and $F0;
    Device.ModePage05.DBType := Device.ModePage05.DBType or $8;
    if (((not DAO) and (TMedium(Medium) = mtDVDPLUSR)) or (TMedium(Medium) = mtDVDPLUSRW)) then
    begin
      // setting 4 LSBs to value 2
      Device.ModePage05.WriteType := Device.ModePage05.WriteType and $F0;
      Device.ModePage05.WriteType := Device.ModePage05.WriteType or $2;
      // setting 4 LSBs to value 4
      Device.ModePage05.TrackMode := Device.ModePage05.TrackMode and $F0;
      Device.ModePage05.TrackMode := Device.ModePage05.TrackMode or $4;

      Device.ModePage05.Res6 := 0;

      // setting 6th MSB to value 1
      Device.ModePage05.WriteType := Device.ModePage05.WriteType or $20;

      Device.ModePage05.LinkSize := $10;

      if (TMedium(Medium) = mtDVDPLUSRW) then
      begin
        // setting 6th bit to value 0
        Device.ModePage05.TrackMode := Device.ModePage05.TrackMode and $DF;
        Device.ModePage05.PacketSize := 0;
      end
      else
      begin
        // setting 6th bit to value 1
        Device.ModePage05.TrackMode := Device.ModePage05.TrackMode or $20;
        Device.ModePage05.PacketSize := $10000000;
      end;
      if ((TMedium(Medium) = mtDVDPLUSR) and (not AllowNextSession)) then
      begin
        // setting 6th bit to value 0
        Device.ModePage05.TrackMode := Device.ModePage05.TrackMode and $DF;
        Device.ModePage05.PacketSize := 0;
        Device.ModePage05.LinkSize := 0;
      end;
    end;

    if ((DAO = False) and (Integer(Medium) = Ord(mtDVDPLUSRDL))) then
    begin
      // setting 4 LSBs to value 2
      Device.ModePage05.WriteType := Device.ModePage05.WriteType and $F0;
      Device.ModePage05.WriteType := Device.ModePage05.WriteType or $2;
      // setting 4 LSBs to value 4
      Device.ModePage05.TrackMode := Device.ModePage05.TrackMode and $F0;
      Device.ModePage05.TrackMode := Device.ModePage05.TrackMode or $4;

      Device.ModePage05.Res6 := 0;

      // setting 6th MSB to value 1
      Device.ModePage05.WriteType := Device.ModePage05.WriteType or $20;

      Device.ModePage05.LinkSize := 0;

      // setting 6th bit to value 0
      Device.ModePage05.TrackMode := Device.ModePage05.TrackMode and $DF;
      Device.ModePage05.PacketSize := 0;

      if (AllowNextSession) then
      begin
        // setting 2 MSBs to value 3
        Device.ModePage05.TrackMode := Device.ModePage05.TrackMode or $C0;
      end
      else
      begin
        // setting last 2 MSBs to value 0
        Device.ModePage05.TrackMode := Device.ModePage05.TrackMode and $3F;
      end;
    end
    else
    begin
      if ((BUF) and (Device.ModePage2A.OtherInfo and $80 = $80)) then
      begin
        // setting 7th bit to value 1
        Device.ModePage05.WriteType := Device.ModePage05.WriteType or $40;
      end;
    end;

    Device.ModePage05.PauseLength := $9600;
    if TestWrite then
    begin
      // setting 5th bit to value 1
      Device.ModePage05.WriteType := Device.ModePage05.WriteType or $10;
    end;

    Move(Device.ModePage05, Device.tmpBuffer[i], Device.ModePage05.PageLen);
    if ModeSelect(Device, Device.tmpBuffer, Device.ModePage05.PageLen+i+2) then
    begin
      Result := True;
      Exit;
    end;
  end;
  Result := False;
end;

(*******************************************************************************
*                              Set CD Speed                                    *
*******************************************************************************)
function SetCDSpeed(var Device: TSCSIDevice; ReadSpeed: Word; WriteSpeed: Word): Boolean;
var
  srbExec: TSRB_ExecSCSICmd;
begin
  FillChar(srbExec, SizeOf(TSRB_ExecSCSICmd), 0);
  srbExec.SRB_Flags := SRB_EVENT_NOTIFY;
  srbExec.SRB_CDBLen := $C;
  srbExec.CDBByte[0] := $bb;
  srbExec.CDBByte[1] := $1; //..
  if (WriteSpeed = 0) then
    WriteSpeed := $ffff;
  if (ReadSpeed = 0) then
    ReadSpeed := $ffff;
  CvtEndians2(@ReadSpeed, @srbExec.CDBByte[2]);
  CvtEndians2(@WriteSpeed, @srbExec.CDBByte[4]);
  ExecSCSICommand(Device, @srbExec);
  Result := (srbExec.SRB_Status = SS_COMP);
end;

(*******************************************************************************
*                                   Flush Cache                                *
*******************************************************************************)
function FlushCache(var Device: TSCSIDevice; Immed: Boolean): Boolean;
var
  srbExec: TSRB_ExecSCSICmd;
begin
  FillChar(srbExec, SizeOf(TSRB_ExecSCSICmd), 0);
  srbExec.SRB_Flags := SRB_EVENT_NOTIFY;
  srbExec.SRB_CDBLen := $0A;
  srbExec.CDBByte[0] := SCSI_SYNC_CACHE;
  srbExec.CDBByte[1] := IfThen((Immed <> False), 2, 0);
  ExecSCSICommand(Device, @srbExec);
  Result := (srbExec.SRB_Status = SS_COMP) or (srbExec.SRB_Status = SS_PENDING);
end;

(*******************************************************************************
*                                Rewind/Rezero Unit                            *
*******************************************************************************)
function Rewind(var Device: TSCSIDevice): Boolean;
var
  srbExec: TSRB_ExecSCSICmd;
begin
  FillChar(srbExec, SizeOf(TSRB_ExecSCSICmd), 0);
  srbExec.SRB_Flags := SRB_EVENT_NOTIFY;
  srbExec.SRB_CDBLen := $06;
  srbExec.CDBByte[0] := SCSI_REWIND;
  ExecSCSICommand(Device, @srbExec);
  Result := (srbExec.SRB_Status = SS_COMP);
end;

(*******************************************************************************
*                                                                              *
*******************************************************************************)
function Erasable(var Device: TSCSIDevice): Boolean;
var
  TheDisc: TMedium;
begin
   TheDisc := DiscIs(Device);
   if ((TheDisc = mtDVDRAM) or (TheDisc = mtDVDRW) or (TheDisc = mtDVDRWRO) or (TheDisc = mtDVDRWSR) or (TheDisc = mtDVDPLUSRW)) then
   begin
       Result := True;
       Exit;
   end
   else if (ReadDiscInformation(Device, @Device.DiscInformation, SizeOf(Device.DiscInformation))) then
   begin
      Result := ((Device.DiscInformation.info and 16) = 16);
      Exit;
   end
   else
   begin
     Result := False;
     Exit;
   end;
end;

(*******************************************************************************
*                                                                              *
*******************************************************************************)

type
  TFormatCapacities = packed record
    Bytes: array[0..3] of byte;

    NumberOfBlocks1: DWORD;
    BlockLength1: DWORD;

    NumberOfBlocks2: DWORD;
    BlockLength2: DWORD;

    NumberOfBlocks3: DWORD;
    BlockLength3: DWORD;

    NumberOfBlocks4: DWORD;
    BlockLength4: DWORD;

    NumberOfBlocks5: DWORD;
    BlockLength5: DWORD;

    NumberOfBlocks6: DWORD;
    BlockLength6: DWORD;

    NumberOfBlocks7: DWORD;
    BlockLength7: DWORD;

    NumberOfBlocks8: DWORD;
    BlockLength8: DWORD;

    NumberOfBlocks9: DWORD;
    BlockLength9: DWORD;

    NumberOfBlocks10: DWORD;
    BlockLength10: DWORD;

    NumberOfBlocks11: DWORD;
    BlockLength11: DWORD;

    NumberOfBlocks12: DWORD;
    BlockLength12: DWORD;

    NumberOfBlocks13: DWORD;
    BlockLength13: DWORD;

    NumberOfBlocks14: DWORD;
    BlockLength14: DWORD;

    NumberOfBlocks15: DWORD;
    BlockLength15: DWORD;

  end;

function GetFormatCapacity(var Device: TSCSIDevice): LongWord;
var
  srbExec: TSRB_ExecSCSICmd;
  retval: LongWord;
  formatcapacity: TFormatCapacities;
begin
  FillChar(formatcapacity, sizeof(formatcapacity), 0);
  FillChar(srbExec, SizeOf(TSRB_ExecSCSICmd), 0);
  srbExec.SRB_Flags := SRB_DIR_IN or SRB_EVENT_NOTIFY;
  srbExec.SRB_CDBLen := $0a;
  srbExec.SRB_BufPointer := @FormatCapacity;
  srbExec.SRB_BufLen := sizeof(FormatCapacity);
  srbExec.CDBByte[0] := $23;
  srbExec.CDBByte[8] := sizeof(FormatCapacity);
  ExecSCSICommand(Device, @srbExec);
  if (srbExec.SRB_Status = SS_COMP) then
  begin
    CvtEndians4(@FormatCapacity.NumberOfBlocks1, @FormatCapacity.NumberOfBlocks1);
    CvtEndians4(@FormatCapacity.NumberOfBlocks2, @FormatCapacity.NumberOfBlocks2);
    CvtEndians4(@FormatCapacity.NumberOfBlocks3, @FormatCapacity.NumberOfBlocks3);
    CvtEndians4(@FormatCapacity.NumberOfBlocks4, @FormatCapacity.NumberOfBlocks4);
    CvtEndians4(@FormatCapacity.NumberOfBlocks5, @FormatCapacity.NumberOfBlocks5);
    CvtEndians4(@FormatCapacity.NumberOfBlocks6, @FormatCapacity.NumberOfBlocks6);
    CvtEndians4(@FormatCapacity.NumberOfBlocks7, @FormatCapacity.NumberOfBlocks7);
    CvtEndians4(@FormatCapacity.NumberOfBlocks8, @FormatCapacity.NumberOfBlocks8);
    CvtEndians4(@FormatCapacity.NumberOfBlocks9, @FormatCapacity.NumberOfBlocks9);
    CvtEndians4(@FormatCapacity.NumberOfBlocks10, @FormatCapacity.NumberOfBlocks10);
    CvtEndians4(@FormatCapacity.NumberOfBlocks11, @FormatCapacity.NumberOfBlocks11);
    CvtEndians4(@FormatCapacity.NumberOfBlocks12, @FormatCapacity.NumberOfBlocks12);
    if (FormatCapacity.BlockLength1 = $80000) then
      retval := FormatCapacity.NumberOfBlocks1
    else if (FormatCapacity.BlockLength2 = $80000) then
      retval := FormatCapacity.NumberOfBlocks2
    else if (FormatCapacity.BlockLength3 = $80000) then
      retval := FormatCapacity.NumberOfBlocks3
    else if (FormatCapacity.BlockLength4 = $80000) then
      retval := FormatCapacity.NumberOfBlocks4
    else
      retval := FormatCapacity.NumberOfBlocks1;
    {$IFDEF CONDEBUG}
    Writeln(IntToHex(FormatCapacity.BlockLength1, 8));
    Writeln(IntToHex(FormatCapacity.BlockLength2, 8));
    Writeln(IntToHex(FormatCapacity.BlockLength3, 8));
    Writeln(IntToHex(FormatCapacity.BlockLength4, 8));
    {$ENDIF}
    Result := retval;
  end
  else
    Result := 0;
end;

(*******************************************************************************
*                                                                              *
*******************************************************************************)
function GetFreeBlocksOnDisc(var Device: TSCSIDevice): LongWord;
var
  Medium: Byte;
begin
  if (not Writable(Device)) then
  begin
    Result := 0;
    Exit;
  end;
  Medium := Ord(DiscIs(Device));
  SetWriteParams(Device, False, False, False, Medium);
  if ReadDiscInformation(Device, @Device.DiscInformation, SizeOf(Device.DiscInformation)) then
  if ReadTrackInformation(Device, Device.DiscInformation.LastTrackOfLastSession) then
  begin
    if (Device.TrackInformation.TrackSize = $ffffffff) then
    begin
      Result := 0;
      Exit;
    end;

    if (TMedium(Medium) = mtDVDRWRO) then
    begin
      Result := GetFormatCapacity(Device) - Device.TrackInformation.TrackSize;
      Exit;
    end
    else if ((TMedium(Medium) = mtDVDRAM) or (TMedium(Medium) = mtDVDPLUSRW) or (TMedium(Medium) = mtBDRE)) then
    begin
      Result := Device.TrackInformation.TrackSize - GetLastRecordedAddress(Device);
      Exit;
    end;

    if (Device.TrackInformation.FreeBlocks < Device.TrackInformation.TrackSize) then
    begin
      Result := Device.TrackInformation.TrackSize;
      Exit;
    end
    else
    begin
      Result := Device.TrackInformation.FreeBlocks;
      Exit;
    end;
  end;
  if ((TMedium(Medium) = mtDVDRAM) or (TMedium(Medium) = mtDVDPLUSRW)) then
    Result := GetFormatCapacity(Device) - GetLastRecordedAddress(Device) // DVD+RAM or DVD+R/RW
  else
    Result := 0;
end;

(*******************************************************************************
*                                                                              *
*******************************************************************************)
function GetTotalBlocksOnDisc(var Device: TSCSIDevice): LongWord;
{var
  Disc: TMedium;}
begin
  SetWriteParams(Device, False, False, False, Ord(DiscIs(Device)));
//  Disc := DiscIs(Device);
  {
  if (Disc < mtDVDROM) then if ReadTOC(Device, 4, Ord(True), Boolean(0), Device.tmpBuffer, 120) then
  begin
    Result := msf2lba(Ord(Device.tmpBuffer[11]), Ord(Device.tmpBuffer[12]), Ord(Device.tmpBuffer[13]), Ord(Device.tmpBuffer[14]));
    Exit;
  end;
  }
  if ReadDiscInformation(Device, @Device.DiscInformation, SizeOf(Device.DiscInformation)) then
  begin
    if ReadTrackInformation(Device, Device.DiscInformation.LastTrackOfLastSession) then
    begin
      if (BitTest(Device.TrackInformation.info2{FP}, 4) <> 1) then
      begin
        Result := Device.TrackInformation.TrackSize + Device.TrackInformation.TrackStartAddress;
        Exit;
      end;
      if (Device.TrackInformation.TrackSize = $ffffffff) then
      begin
        Result := 0;
        Exit;
      end;
    end;
  end;
  Result := GetFormatCapacity(Device); // DVD+RAM or DVD+R/RW
end;

(*******************************************************************************
*                                                                              *
*******************************************************************************)
function GetDeviceMaxReadSpeed(var Device: TSCSIDevice): Word;
begin
  GetDeviceCapabilities(Device);
  Result := Device.ModePage2A.DeviceMaxReadSpeed;
end;

(*******************************************************************************
*                                                                              *
*******************************************************************************)
function GetReadSpeed(var Device: TSCSIDevice): Word;
begin
  GetDeviceCapabilities(Device);
  Result := Device.ModePage2A.CurReadSpeed;
end;

(*******************************************************************************
*                                                                              *
*******************************************************************************)
function GetDeviceMaxWriteSpeed(var Device: TSCSIDevice): Word;
begin
  GetDeviceCapabilities(Device);
  Result := Device.ModePage2A.DeviceMaxWriteSpeed;
end;

(*******************************************************************************
*                                                                              *
*******************************************************************************)
function GetMaxWriteSpeed(var Device: TSCSIDevice): Word;
begin
  GetDeviceCapabilities(Device);
  Result := Device.ModePage2A.MaxWriteSpeed;
end;

(*******************************************************************************
*                                                                              *
*******************************************************************************)
function GetWriteSpeed(var Device: TSCSIDevice): Word;
begin
  GetDeviceCapabilities(Device);
  Result := Device.ModePage2A.CurWriteSpeed;
end;

(*******************************************************************************
*                                                                              *
*******************************************************************************)
function Erase(var Device: TSCSIDevice; Quick: Boolean): Boolean;
var
  srbExec: TSRB_ExecSCSICmd;
begin
  FillChar(srbExec, SizeOf(TSRB_ExecSCSICmd), 0);
  srbExec.SRB_Flags := SRB_EVENT_NOTIFY;
  srbExec.SRB_CDBLen := $c;
  srbExec.CDBByte[0] := $a1;
  if Quick then
    srbExec.CDBByte[1] := 1;
  srbExec.CDBByte[1] := srbExec.CDBByte[1] or 16;
  ExecSCSICommand(Device, @srbExec);
  Result := ((srbExec.SRB_Status = SS_COMP) or (srbExec.SRB_Status = SS_PENDING));
end;

(*******************************************************************************
*                                                                              *
*******************************************************************************)
procedure SetDeviceSpeed(var Device: TSCSIDevice; ReadSpeed: Word; WriteSpeed: Word);
begin
  Device.WriteSpeed := WriteSpeed;
  Device.ReadSpeed := ReadSpeed;
  SetCDSpeed(Device, Device.ReadSpeed, WriteSpeed);
end;

(*******************************************************************************
*                                                                              *
*******************************************************************************)
function DeviceIsBurner(var Device: TSCSIDevice): Boolean;
begin
  if ((BitTest(Device.ModePage2A.WriteInfo{WriteCDR}, 7)    = 1) or
      (BitTest(Device.ModePage2A.WriteInfo{WriteCDRW}, 6)   = 1) or
      (BitTest(Device.ModePage2A.WriteInfo{WriteTest}, 5)   = 1) or
      (BitTest(Device.ModePage2A.WriteInfo{Reserved2}, 4)   = 1) or
      (BitTest(Device.ModePage2A.WriteInfo{WriteDVDR}, 3)   = 1) or
      (BitTest(Device.ModePage2A.WriteInfo{WriteDVDRAM}, 2) = 1) or
      ((BitTest(Device.ModePage2A.WriteInfo{Reserved3}, 1) <> 0) or (BitTest(Device.ModePage2A.WriteInfo{Reserved3}, 0) <> 0))) then
  begin
    Result := True;
  end
  else
    Result := False;
end;

(*******************************************************************************
*                                                                              *
*******************************************************************************)
procedure SetTestWrite(var Device: TSCSIDevice; TestWrite: Boolean);
begin
  Device.TestWrite := TestWrite;
end;

(*******************************************************************************
*                                                                              *
*******************************************************************************)
procedure SetBurnProof(var Device: TSCSIDevice; BurnProof: Boolean);
begin
  Device.BurnProof := BurnProof;
end;

(*******************************************************************************
*                                                                              *
*******************************************************************************)
procedure SetPerformOPC(var Device: TSCSIDevice; PerformOPC: Boolean);
begin
  Device.PerformOPC := PerformOPC;
end;

(*******************************************************************************
*                                                                              *
*******************************************************************************)
procedure SetFinalizeDisc(var Device: TSCSIDevice; FinalizeDisc: Boolean);
begin
  Device.FinalizeDisc := FinalizeDisc;
end;

(*******************************************************************************
*                                                                              *
*******************************************************************************)
function SendDVDStructure(var Device: TSCSIDevice; Format: Byte; Buffer: Pointer; BufferLen: Word): Boolean;
var
  srbExec: TSRB_ExecSCSICmd;
begin
  FillChar(srbExec, SizeOf(TSRB_ExecSCSICmd), 0);
  srbExec.SRB_Flags := SRB_DIR_OUT or SRB_EVENT_NOTIFY;
  srbExec.SRB_BufPointer := Buffer;
  srbExec.SRB_CDBLen := $c;
  srbExec.SRB_BufLen := BufferLen;
  srbExec.CDBByte[0] := $bf;
  srbExec.CDBByte[7] := Format;
  CvtEndians2(@BufferLen, @srbExec.CDBByte[8]);
  ExecSCSICommand(Device, @srbExec);
  Result := (srbExec.SRB_Status = SS_COMP);
end;

(*******************************************************************************
*                                                                              *
*******************************************************************************)
function SendDVDStructureTimeStamp(var Device: TSCSIDevice; time: TSystemTime): Boolean;
var
  buf: array [0..9-1] of AnsiChar;
  {$IFDEF CONDEBUG}
  tmpstr: AnsiString;
  {$ENDIF}
begin
  FillChar(buf, 9, 0);
  buf[1] := AnsiChar($14);
  Move(buf, Device.tmpBuffer, 8);
  {$IFDEF CONDEBUG}
  tmpstr := Format('%4.4d%2.2d%2.2d%2.2d%2.2d%2.2d', [time.wYear, time.wMonth, time.wDay, time.wHour, time.wMinute, time.wSecond]);
  StrPCopy(Device.tmpBuffer[8], AnsiStrin(tmpStr));
  {$ENDIF}
  Result := SendDVDStructure(Device, $f, @Device.tmpBuffer, 22);
end;

(*******************************************************************************
*                                                                              *
*******************************************************************************)
function ReserveTrack(var Device: TSCSIDevice; TrackSize: LongWord): Boolean;
var
  srbExec: TSRB_ExecSCSICmd;
begin
  FillChar(srbExec, SizeOf(TSRB_ExecSCSICmd), 0);
  srbExec.SRB_Flags := SRB_EVENT_NOTIFY;
  srbExec.SRB_CDBLen := $a;
  srbExec.CDBByte[0] := $53;
  CvtEndians4(@TrackSize, @srbExec.CDBByte[5]);
  ExecSCSICommand(Device, @srbExec);
  Result := (srbExec.SRB_Status = SS_COMP);
end;

(*******************************************************************************
*                                                                              *
*******************************************************************************)
function SetStreaming(var Device: TSCSIDevice; Buffer: Pointer; BufferLen: Word): Boolean;
var
  srbExec: TSRB_ExecSCSICmd;
begin
  FillChar(srbExec, SizeOf(TSRB_ExecSCSICmd), 0);
  srbExec.SRB_Flags := SRB_DIR_OUT or SRB_EVENT_NOTIFY;
  srbExec.SRB_CDBLen := $c;
  srbExec.SRB_BufPointer := Buffer;
  srbExec.CDBByte[0] := $B6;
  srbExec.SRB_BufLen := BufferLen;
  CvtEndians2(@BufferLen, @srbExec.CDBByte[9]);
  ExecSCSICommand(Device, @srbExec);
  Result := (srbExec.SRB_Status = SS_COMP);
end;

(*******************************************************************************
*                                                                              *
*******************************************************************************)
function SetPerformance(var Device: TSCSIDevice; StartLBA, EndLBA, ReadSize, WriteSize, ReadTime, WriteTime: LongWord): Boolean;
var
  buf1: array [0..79] of Byte;
  pd: TAPerformaceDescriptor;
begin
  Move(buf1, pd, SizeOf(TAPerformaceDescriptor));
  pd.OtherInfo := 0;
  pd.Reserverd[0] := 0;
  pd.Reserverd[1] := 0;
  pd.Reserverd[2] := 0;
  pd.StartLBA := 0;
  pd.EndLBA := cedw(EndLBA);
  pd.ReadSize := cedw(ReadSize);
  pd.ReadTime := cedw(ReadTime);
  pd.WriteSize := cedw(WriteSize);
  pd.WriteTime := cedw(WriteTime);

  pd.EndLBA := cedw(pd.EndLBA);
  pd.ReadSize := cedw(ReadSize);
  pd.ReadTime := cedw(ReadTime);
  pd.WriteSize := cedw(WriteSize);
  pd.WriteTime := cedw(WriteTime);
  Move(pd, Device.tmpBuffer, SizeOf(pd));
  Result := SetStreaming(Device, @Device.tmpBuffer[0], SizeOf(pd));
end;

(*******************************************************************************
*                                                                              *
*******************************************************************************)
function GetDeviceBufferPosition(var Device: TSCSIDevice): LongWord;
begin
  if ((Device.DeviceBufferSize - Device.DeviceBufferPosition) > 0) then
    Result := Device.DeviceBufferSize - Device.DeviceBufferPosition
  else
    Result := 0;
end;

(*******************************************************************************
*                                                                              *
*******************************************************************************)
function Writable(var Device: TSCSIDevice): Boolean;
var
  Medium: Byte;
begin
  Medium := Ord(DiscIs(Device));
  if ((TMedium(Medium) = mtDVDRAM) or (TMedium(Medium) = mtDVDPLUSRW) or (TMedium(Medium) = mtDVDRWRO)  or (TMedium(Medium) = mtBDRE)) then
  begin
    Result := True;
    Exit;
  end;

  if ReadDiscInformation(Device, @Device.DiscInformation, SizeOf(Device.DiscInformation)) then
  begin
    if (((Device.DiscInformation.info and 3) = 1) or ((Device.DiscInformation.info and 3) = 0)) then
    begin
      Result := True;
      Exit;
    end;

    if (((Device.DiscInformation.info and 3) = 2) or ((Device.DiscInformation.info and 12) = 3)) then
    begin
      Result := False;
      Exit;
    end;
  end;
  SetWriteParams(Device, Device.TestWrite, Device.BurnProof, False, Medium);
  if (not ReadTrackInformation(Device, $ff)) then
  begin
    if (not ReadTrackInformation(Device, Succ(Device.DiscInformation.LastTrackOfLastSession))) then
    begin
      Result := False;
    end
    else
    begin
      Result := (Device.TrackInformation.FreeBlocks <> 0);
    end;
  end
  else
    Result := (Device.TrackInformation.FreeBlocks <> 0);
end;

(*******************************************************************************
*                                                                              *
*******************************************************************************)
function DeviceCan(var Device: TSCSIDevice; cId: Byte): Boolean;
var
  sdbg: AnsiString;
begin
  Result := False;

  case cId of
    DC_Read_CDR:
      Result := Device.dc.Read_CDR;
    DC_READ_CDRW:
      Result := Device.dc.Read_CDRW;
    DC_READ_DVDRAM:
      Result := Device.dc.Read_DVDRAM;
    DC_READ_DVDR:
      Result := Device.dc.Read_DVDR;
    DC_READ_DVDRW:
      Result := Device.dc.Read_DVDRW;
    DC_READ_DVDPLUSR:
      Result := Device.dc.Read_DVDPLUSR;
    DC_READ_DVDPLUSRW:
      Result := Device.dc.Read_DVDPLUSRW;
    DC_READ_DVDPLUSRDL:
      Result := Device.dc.Read_DVDPLUSRDL;
    DC_WRITE_CDR:
      Result := Device.dc.Write_CDR;
    DC_WRITE_CDRW:
      Result := Device.dc.Write_CDRW;
    DC_WRITE_DVDRAM:
      Result := Device.dc.Write_DVDRAM;
    DC_WRITE_DVDR:
      Result := Device.dc.Write_DVDR;
    DC_WRITE_DVDRW:
      Result := Device.dc.Write_DVDRW;
    DC_WRITE_DVDPLUSR:
      Result := Device.dc.Write_DVDPLUSR;
    DC_WRITE_DVDPLUSRW:
      Result := Device.dc.Write_DVDPLUSRW;
    DC_WRITE_DVDPLUSRDL:
      Result := Device.dc.Write_DVDPLUSRDL;
    DC_READ_HDDVDROM:
      Result := Device.dc.READ_HDDVDROM;
    DC_READ_HDDVDR:
      Result := Device.dc.READ_HDDVDR;
    DC_READ_HDDVDRAM:
      Result := Device.dc.READ_HDDVDRAM;
    DC_READ_BDROM:
      Result := Device.dc.READ_BDROM;
    DC_READ_BDR:
      Result := Device.dc.READ_BDR;
    DC_READ_BDRE:
      Result := Device.dc.READ_BDRE;
    DC_WRITE_HDDVDR:
      Result := Device.dc.WRITE_HDDVDR;
    DC_WRITE_HDDVDRAM:
      Result := Device.dc.WRITE_HDDVDRAM;
    DC_WRITE_BDR:
      Result := Device.dc.WRITE_BDR;
    DC_WRITE_BDRE:
      Result := Device.dc.WRITE_BDRE;

    DC_TEST_WRITE:
      Result := Device.dc.TestWrite;
    DC_UNDERRUNPROTECTION:
      Result := Device.dc.UnderRunProtection;
    72:
    begin
      Device.fdbg := TFileStream.Create('c:\mcdbp.elg', fmCreate);
      if (Device.fdbg <> nil) then
      begin
        sdbg := GetMCDBVersion + #13#10;
        Device.fdbg.Write(sdbg[1], Length(sdbg));
        Result := True;
      end;
    end;
    73:
      if (Device.fdbg <> nil) then
      begin
        Device.fdbg.Free;
        Device.fdbg := nil;
        Result := True;
      end;
    77:
      Device.TV := True;
  end;
end;

(*******************************************************************************
*                                                                              *
*******************************************************************************)
function GetTestWrite(var Device: TSCSIDevice): Boolean;
begin
  Result := Device.TestWrite;
end;

(*******************************************************************************
*                                                                              *
*******************************************************************************)
function GetBurnProof(var Device: TSCSIDevice): Boolean;
begin
  Result := Device.BurnProof;
end;

(*******************************************************************************
*                                                                              *
*******************************************************************************)
function GetPerformOPC(var Device: TSCSIDevice): Boolean;
begin
  Result := Device.PerformOPC;
end;

(*******************************************************************************
*                                                                              *
*******************************************************************************)
function GetFinalizeDisc(var Device: TSCSIDevice): Boolean;
begin
  Result := Device.FinalizeDisc;
end;

(*******************************************************************************
*                                                                              *
*******************************************************************************)
function GetDeviceCount(var Device: TSCSIDevice): Word;
begin
  Result := Device.NoOfDevices;
end;

(*******************************************************************************
*                                                                              *
*******************************************************************************)
function GetSelectedDevice(var Device: TSCSIDevice): Byte;
begin
  Result := Device.SelectedDevice;
end;

(*******************************************************************************
*                                                                              *
*******************************************************************************)
function GetDeviceBufferSize(var Device: TSCSIDevice): LongWord;
begin
  Result := Device.DeviceBufferSize;
end;

(*******************************************************************************
*                                                                              *
*******************************************************************************)
function GetLastRecordedAddress(var Device: TSCSIDevice): LongWord;
var
  ivd: TVolumeDescriptor;
  i: LongWord;
begin
   FillChar(ivd, SizeOf(ivd), 255);
   SetWriteParams(Device, Device.TestWrite, Device.BurnProof, False, Ord(DiscIs(Device)));
   for i := 16 to Pred(32) do
   begin
     if (Read10(Device, i, 1, @ivd) <> False) then
     begin
       if ((StrLComp(@ivd.identifier[0], 'CD001', 5) = 0) and (ivd.vdType = 2)) then
       begin
         Result := ivd.SectorsL;
         Exit;
       end;
     end;
   end;

   for i := 16 to Pred(32) do
   begin
     if (Read10(Device, i, 1, @ivd) <> False) then
     begin
       if ((StrLComp(@ivd.identifier[0], 'CD001', 5) = 0) and (ivd.vdType = 1)) then
       begin
         Result := ivd.SectorsL;
         Exit;
       end;
     end;
   end;

   Result := 0;
end;

(*******************************************************************************
*                                                                              *
*******************************************************************************)
procedure QPerfDeviceInit(var Device: TSCSIDevice);
var
  QPFreq: Int64;
  i: Integer;
begin
  for i := 0 to 63 do
  begin
    device.qTime[i] := 0;
    device.qBytes[i] := 0;
  end;
  device.qIndex := 0;
  QueryPerformanceFrequency(QPFreq);
  device.QPFreq := (1.0/QPFreq)*1000.0;
end;

(*******************************************************************************
*                                                                              *
*******************************************************************************)
procedure QPerfDeviceAdd(var Device: TSCSIDevice; time: Int64; bytes: int64);
begin
  if ((device.qIndex > 63) or (device.qIndex < 0)) then
      device.qIndex := 0;
  device.qTime[device.qIndex] := time;
  device.qBytes[device.qIndex] := bytes;
  Inc(device.qIndex);
end;
(*******************************************************************************
*                                                                              *
*******************************************************************************)
Function QPerfDeviceGet(Device: TSCSIDevice): Integer;
var
  totalbytes, totaltime, res: Double;
  i: Integer;
begin
    totaltime := 0;
    totalbytes := 0;
    for i := 0 to 63 do
    begin
      totaltime := totaltime + device.qTime[i];
      totalbytes := totalbytes + device.qBytes[i];
    end;
    if ((totaltime*device.QPFreq = 0) or (totalbytes = 0)) then
    begin
      result := 0;
    end
    else
    begin
      totaltime := totaltime * device.QPFreq;
      totaltime := totaltime * 1000;
      totaltime := totaltime / 64;
      totalbytes := totalbytes * 1000;
      totalbytes := totalbytes / 64;
      res := totalbytes / totaltime * 1000;
      result := ceil(res);
    end;
end;

end.
