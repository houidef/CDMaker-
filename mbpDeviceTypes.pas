unit mbpDeviceTypes;

interface

uses
  Windows, Classes, mbpWnASPI32, mbpTreeTypes;

{$I mbpDEFINES.INC}

type
  TDevices = record
    DriveLetter: AnsiChar;
    DeviceType: Byte;
    HaID, Target, Lun: Byte;
    Data: array[0..127] of AnsiChar;
  end;

type
  TEraseDoneEvent = procedure (Sender: TObject; DeviceId: Byte; EraseFailed: Boolean) of Object;

type
  TTrackInformation = packed record
    DataLength: Word;
    TrackNumber, SessionNumber, Reserved1: Byte;
    info1: Byte; //TrackMode = 4; Copy = 1; Damadge = 1; Reserved = 2
    info2: Byte; //DataMode = 4; FP = 1; PacketInc = 1; Blank = 1; RT = 1
    Reserved2: Byte;
    TrackStartAddress, NextWritableAddress, FreeBlocks, FixedPacketSize, TrackSize, LastRecordedAddress: Longword;
    TrackNumber2, SessionNumber2, Res3, Res4, Res5, Res6: Byte;
  end;

type
  TSCSI_ADDRESS = packed record
    Length: Longword;
    PortNumber, PathId, TargetId, Lun: Byte;
  end;

type
  TDiscInformation = packed record
    DataLen: Word;
    info: Byte; // DiscStatus:2, StateOfLastSession:2, Erasable: 1, Resereved: 3
    FirstTrack, Sessions, FirstTrackOfLastSession, LastTrackOfLastSession: Byte;
    info2: Byte; // BGFormat:2, DBit:1, Reserved2:2, URU:1, DBC_V:1, DID_V:1
    DiscType, Resvd1, Resvd2, Resvd3: Byte;
    DiscIdentification: Longword;
    Resvd4: array[0..47] of AnsiChar;
  end;

type
  TConfigHeader = packed record
    DataLength: Longword;
    Reseverd1, CurrentProfile, FeatureCode: Word;
    Version, AdditionalLength: Byte;
    info: Byte; // OtherDataBit1:1, OtherDataBit2:1, OtherDataBit3:1, OtherDataBit4:1, OtherDataBit5:1, OtherDataBit6:1, OtherDataBit7:1, OtherDataBit8:1
    OtherData: array[0..100] of Byte;
  end;

type
  TModePage2A = packed record
    PageCode, PageLen: Byte;
    ReadInfo: Byte; // ReadCDR:1, ReadCDRW:1, ReadMethod2:1, ReadDVDROM:1, ReadDVDR:1, ReadDVDRAM:1, Reserverd1:2
    WriteInfo: Byte; // WriteCDR:1, WriteCDRW:1, WriteTest:1, Reserved2:1, WriteDVDR:1, WriteDVDRAM:1, Reserved3:2
    OtherInfo: Byte; // AudioPlay:1, RES1:5, MultiSession:1, BUF:1
    CDDASupported, Lock, SepChanVol: Byte;
    MaxReadSpeed, NumVolLevels, BufferSize, CurReadSpeed: Word;
    Reserved16, BCK: Byte;
    MaxWriteSpeed, CurWriteSpeed: Word;
    Others: array[0..49] of AnsiChar;
    DeviceMaxReadSpeed, DeviceMaxWriteSpeed: Word;
  end;

type
  TModePage05 = packed record
    PageCode,
    PageLen: Byte;
    WriteType: Byte; // WriteType: 4, TestWrite: 1, LSV: 1, BUFE: 1, Reserver1: 1
    TrackMode: Byte; // TrackMode: 4, Copy:1, FP:1, MultiSession:2
    DBType: Byte; // DBType: 4, Res3: 4;
    LinkSize: Byte;
    Res6: Byte;
    Init_App_Code: Byte;
    SessionFormat: Byte;
    Res9: Byte;
    PacketSize: Longword;
    PauseLength: Word;
    MediaCatNo: array[0..15] of AnsiChar;
    ISRC: array[0..15] of AnsiChar;
    SubHeader: array[0..3] of AnsiChar;
    Vendors: array[0..19] of AnsiChar;
  end;

type
  TDC = record
    Read_CDR, Read_CDRW, Read_Method2, Read_DVDR, Read_DVDRW, Read_DVDRAM, Read_DVDPLUSR, Read_DVDPLUSRDL, Read_DVDPLUSRW, Read_DVDROM, Read_BDROM, Read_BDR, Read_BDRE, Read_HDDVDROM, Read_HDDVDR, Read_HDDVDRAM,
    Write_CDR, Write_CDRW, Write_DVDR, Write_DVDRW, Write_DVDRAM, Write_DVDPLUSR, Write_DVDPLUSRDL, Write_DVDPLUSRW, Write_BDR, Write_BDRE, Write_HDDVDR, Write_HDDVDRAM,
    TestWrite, UnderRunProtection: Boolean;
  end;

type
  TSPT = record
    Length: Word;
    ScsiStatus, PathId, TargetId, Lun, CdbLength, SenseInfoLength, DataIn: Byte;
    DataTransferLength, TimeOutValue: Longword;
    DataBuffer: PAnsiChar;
    SenseInfoOffset: Longword;
    Cdb: array[0..15] of AnsiChar;
  end;

type
  PSCSI_PASS_THROUGH_DIRECT_WITH_BUFFER = ^TSCSI_PASS_THROUGH_DIRECT_WITH_BUFFER;
  TSCSI_PASS_THROUGH_DIRECT_WITH_BUFFER = record
    spt: TSPT;
    Filler: Longword;
    ucSenseBuf: array[0..31] of AnsiChar;
  end;

type
  TSCSIAddress = record
    Length: Longword;
    PortNumber, PathId, TargetId, Lun: Byte;
  end;

type
  PAPerformaceDescriptor = ^TAPerformaceDescriptor;
  TAPerformaceDescriptor = packed record
    OtherInfo: Byte; // RA:1, Exact:1, RDD:1, WRC:2, Reserverd0:3
    Reserverd: array [0..2] of Byte;
    StartLBA, EndLBA, ReadSize, ReadTime, WriteSize, WriteTime: LongWord;
  end;

type TMedium = (mtUNKNOWN, mtCDROM, mtCDR, mtCDRW, mtDVDROM, mtDVDR, mtDVDRAM, mtDVDRW,
               mtDVDRWRO, mtDVDRWSR, mtDVDPLUSRW, mtDVDPLUSR, mtDVDPLUSRDL,
               mtBDROM, mtBDRSR, mtBDRRR, mtBDRE, mtHDDVDROM, mtHDDVDR, mtHDDVDRAM, mtDDCDROM, mtDDCDR, mtDDCDRW);
//{$ALIGN 8}
{$A+}
type
  pSCSIDevice = ^TSCSIDevice;
  TSCSIDevice = record
    Handle: THandle;
    hWNDFE: HWND;
    tmpBuffer: array[0..$ffff-1] of AnsiChar;
    tmpBuffer2: array[0..$ffff-1] of AnsiChar;
    cn: array [0..254] of AnsiString;
    SCSI_ADDRESS: TSCSI_ADDRESS;
    Devices: array[0..25] of TDevices;
    NoOfDevices: Byte;
    DriveLetter: AnsiChar;
    DeviceType: Byte;
    LastWinError: LongWord;
    CloseFH: Boolean;
    DoNotUseSPTI: Boolean;
    dumm1: Byte;
    TheObject: TObject;
    ASPIInitialized: Boolean;
    UsingSPTI: Boolean;
    SendASPI32Command: Pointer;
    srbdi: TSRB_GetDiskInfo;
    SelectedDevice: Byte;
    HaID : Byte;
    Target : Byte;
    Lun : Byte;
    Vendor: array[0..8] of AnsiChar;
    ProductID: array[0..16] of AnsiChar;
    Medium: TMedium;
    LastSenseInfo: TSENSE;
    TargetBusy: Boolean;
    BurnProof: Boolean;
    fdbg: TFileStream;
    DeviceError: Word;
    DeviceBufferSize: Longword;
    DeviceBufferPosition: Longword;
    hEraseThread: THANDLE;
    IdEraseThread: Word;
    IdEraseThread2: Word;
    TrackInformation: TTrackInformation;
    IdEraseThread3: Word;
    DiscInformation, BGFDI: TDiscInformation;
    ConfigHeader: TConfigHeader;

    bmWStart, bmWStop,
    bmWTotal, bmWSize: Double;
    bmWCounter: Byte;
    bmWriteSpeed: WORD;
    bmAvgWriteSpeed: WORD;

    bmRStart, bmRStop,
    bmRTotal, bmRSize: Double;
    bmRCounter: Byte;
    bmAvgReadSpeed: WORD;
    bmReadSpeed: WORD;

    ModePage2A: TModePage2A;
    TV: Boolean;
    dc: TDC;
    ErrStr: array[0..255] of AnsiChar;
    ErrStrW: array[0..255] of WideChar;
    cbEraseDone: TEraseDoneEvent; 
    QuickErase: Boolean;
    ModePage05: TModePage05;
    SonyPowerBurn: AnsiChar;
    WriteSpeed, ReadSpeed: Byte;
    MaxReadSpeed, CurReadSpeed: Word;
    MaxWriteSpeed, CurWriteSpeed: Word;
    TestWrite: Boolean;
    PerformOPC: Boolean;
    FinalizeDisc: Boolean;
    ProductRev: array[0..4] of AnsiChar;
    FinalizeTrack: Boolean;
    pPtr1: Pointer;
    pPtr2: Pointer;
    gPtr: array [0..9] of Pointer;
    qTime: array[0..64] of Int64;
    qBytes: array[0..64] of Int64;
    qIndex: Integer;
    QPFreq: double;
  end;

implementation

end.
