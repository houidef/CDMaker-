{$I mbpDEFINES.INC}

unit mbpTreeTypes;

interface

uses
{$IFNDEF LINUX}
  Windows,
{$ELSE}
  Linux,
{$ENDIF}
  Classes, SyncObjs, mbpUDFTypes, mbpECMA_167;

Const
  MAXFILESONDISC = 200000;

type
  pFileEntry = ^TFileEntry;
  //TAddFileEvent = procedure (Sender: TObject; const FullPath: AnsiString; var LongFileName, ShortFileName: AnsiString; var DateTime: TDateTime; Attr: Integer; FileSize: Int64; var Skip: Boolean) of object;
  TAddFileEvent = procedure (Sender: TObject; Path: PWideChar; var fd: TWin32FindDataW; var Skip: Boolean) of object;
  TGetDataEvent = procedure(Sender: TObject; FileEntry: PFileEntry; Position: LongWord; Buffer: Pointer; var Size: LongWord) of object;
  TVerifyDoneEvent = procedure(Sender: TObject; DeviceId: Byte; FileId: Integer; Failed: Boolean) of object;
  TWriteDoneEvent = procedure(Sender: TObject; DeviceId: Byte; WriteFailed: Boolean) of object;
  TSmartRefGenDoneEvent = procedure(Sender: TObject; Failed: Boolean; CanSaveBytes: LongInt) of object;

  FileDirWritingStatus = (WritingDirICB, WritingDirData, WritingFileICB);

  PUDFDirectoryInformation = ^UDFDirectoryInformation;
  UDFDirectoryInformation = record
    FileLinkCount: Uint16;
    InformationLength: Uint64;
    CurrentFileEntry: pFileEntry; // being processed
    DirICB_LSOffsetWRTSession, DirData_LSOffsetWRTSession: Uint32;
  end;

  PUDFFileInformation = ^UDFFileInformation;
  UDFFileInformation = record
    FileICB_LSOffsetWRTSession: Uint32;
  end;

  pDirEntry = ^TDirEntry;
  PUDFBridgeInformation = ^UDFBridgeInformation;
  UDFBridgeInformation = record
    VolumeIdentifier: array [0..30] of BYTE;
    FIDContainer: array [0..295] of BYTE;
    UDFVolumeIdentifierLength: Int8;
    PartialFIDLengthRecovered, PartialFIDLengthToRecover: Uint16;
    NumberOfDirectories, NumberOfFiles, VolumeRecognitionSequence_LSAWRTSession, AnchorVolumeDescriptorPointer_LSAWRTSession,
    MainVolumeDescriptorSequenceExtent_LSAWRTSession, ReserveVolumeDescriptorSequenceExtent_LSAWRTSession,
    LogicalVolumeIntegrityDescriptor_LSAWRTSession, PartitionStarting_LSA, LengthOfPartitionPortionInSession_LBS, FileSetDescriptor_LSAWRTSession,
    RootDirectoryICB_LSAWRTSession, NumberOfBlocksRequiredForFileDirStructures, NextAvailableLSOffsetWRTSession, BlockOffsetWritingDirData,
    ISODataPortionStarting_LSAWRTSession, LastAnchorVolumeDescriptorPointer_LSAWRTSession, SessionStarting_LSA: Uint32;
    ICB_UniqueID: Uint64;
    BackTrack, IsPartialFID, IsDiscLayoutExhausted, IsNewPacketRequiredForLastAVDP: Boolean;
    CurrentDirEntry: pDirEntry;
    Status: FileDirWritingStatus;
    UDFTimeStamp: timestamp;
  end;

  TFileEntry = record
    ShortName: array [0..15] of AnsiChar;
    LongName: PWideChar;
    LongNameWIN32: PWideChar;
    Path: PWideChar;
    Time: TSystemTime;
    FileId: Integer;
    FileSize: Int64;
    SpaceRequired: int64;
    Address: Cardinal;
    Imported: Boolean;
    attributes: Integer;
    ResetArchiveBit: AnsiChar;
    Extended: AnsiChar;
    Buffer: PAnsiChar;
    FileNameLength: Byte;
    FileNameLengthJ: Byte;
    Unicode: Boolean;
    mfcbFunction: TGetDataEvent;
    NextFile: pFileEntry;
    DirRecord: pDirEntry;
    Hash: AnsiString;
    ReferenceEntry: pFileEntry;
    UDFFileInfo: UDFFileInformation;
  end;

  TDirEntry = record
    Parent: pDirEntry;
    LongName: PWideChar;
    ShortName: array [0..14] of AnsiChar;
    DirSize: Integer;
    DirSizeJ: Integer; // file sizes
    Address, AddressJ: Cardinal;
    depth, order, number: Word;
    DirNameLength: Byte;
    DirNameLengthJ: Byte;
    extended: AnsiChar;
    Files: pFileEntry;
    UDFDirInfo: UDFDirectoryInformation;
  end;

type
  TPacket = record
    Packet: PAnsiChar;
    Size: Integer;
    WritingFile: Integer;
    Mode: Byte;
    New: Boolean;
  end;

type
  PCDFiles = ^TCDFiles;
  TCDFiles = record
    Aborted: Boolean;
    Files: array [0.. 200000-1] of pFileEntry;
    Dirs: array [0..65000-1] of pDirEntry;
    PathTableEntries: array [0..65000-1] of pDirEntry;
    FileCounter: Integer;
    DirCounter: Integer;
    Blank: Integer;
    RootDir: pDirEntry;
    VolumeLabelW: array [0..31] of WideChar;
    VolumeLabel: array [0..31] of AnsiChar;
    Version: array [0..63] of AnsiChar;
    FirstWritableSector: LongWord;
    VDImageSize: LongWord;
    PathTableWidth, PathTableWidthJ: LongWord;
    FileDirDescriptorWidth: LongWord;
    FileDirDescriptorExtent: LongWord;
    file_and_dir_descriptor_extent_j: LongWord;
    file_and_dir_descriptor_width_j: LongWord;
    PathTableExtentL, path_table_extent_m: LongWord;
    path_table_extent_j_l, path_table_extent_j_m: LongWord;
    ReadingFile, WritingFile: Integer;
    FirstDataSector, Cursor: LongWord;
    FilesSize: LongWord;
    TotalImageSize: LongWord;
    BlocksWritten: LongWord;
    Overwrite: Boolean;
    ReplaceFile: Boolean;
    Prepared: Boolean;
    PostGap: Boolean;
    JolietFS,
    UDFBridge,
    ISO9660FS,
    ISO9660v1999FS,
    UDF12 : Boolean;
    hWNDFE: HWND;
    ComponentStatus: Word;
    TimeZoneDifference: Integer;
    TimeZoneInfo: TTimeZoneInformation;
    TimeZoneDaylight: AnsiChar;
    isom: PAnsiChar;
    fisoheader: TFileStream;
    ISOImage: THandle;
    IsoHeaderSize: Integer;
    CacheSize: LongWord;
    SmartReferencesProgress: LongWord;
    SmartReferencesSize: LongWord;
    VerifySize: LongWord;
    VerifyProgress: LongWord;
    MaxPackets: Integer;
    IndexCount: Integer;
    vPriority: TThreadPriority;
    PacketCount: Integer;
    NoOfFiles: Integer;
    Finished: AnsiChar;
    CanStart: AnsiChar;
    FileName: PWideChar;
    BootImage: array [0..2047] of AnsiChar;
    BootImageSize: Integer;
    BootCatalogLocation: LongWord;
    BootImageLocation: LongWord;
    ErrorNumber: LongWord;
    ISOHeader: PAnsiChar;
    BytesAvailable: int64;
    BufferSize: int64;
    RemainingBlocks: int64;
    ISOHeaderSizeW: int64;
    IdCacheThread: Cardinal;
    hSmartReferenceThread,
    hVerifyThread,
    hCacheThread: THandle;
    IdSmartReferenceThread: Cardinal;
    IdVerifyThread: Cardinal;
    BusyInGetFirst, BusyInReading: Boolean;
    ISOFileStream: TFileStream;
    Packets: array [0..10000-1] of TPacket;
    tmpBuffer1: array [0..2048*32-1] of AnsiChar;
    tmpBuffer2: array [0..2048*32-1] of AnsiChar;
    ISOFileName: array [0..4096-1] of WideChar;
    SaveToISOImage: Boolean;
    BurnISOImage: Boolean;
    CriticalSection: TCriticalSection;
    UDFBridgeInfo: UDFBridgeInformation;
    cbWriteDone: TWriteDoneEvent;
    cbVerifyDone: TVerifyDoneEvent;
    cbAddFileEvent: TAddFileEvent;
    //cbAddFileWEvent: TAddFileWEvent;
    cbGenSmRefEvent: TSmartRefGenDoneEvent;
    CacheThreadTerminated: Boolean;
  end;

implementation

end.
