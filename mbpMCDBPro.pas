unit mbpMCDBPro;

interface

uses
  Windows, Classes, Messages, Math,
  Forms,
  mbpDeviceTypes, mbpTreeTypes, mbpVerify, mbpErase, mbpConsts;
{$I mbpDEFINES.INC}
type
  TMCDBurnerPro = class(TComponent)
  private
    { Private declarations }
    CDFiles: TCDFiles;
    Device: TSCSIDevice;
    FOnAddFile: TAddFileEvent;
    FOnEraseDone: TEraseDoneEvent;
    FOnSmartRefGenDone: TSmartRefGenDoneEvent;
    FOnWriteDone: TWriteDoneEvent;
    FOnVerifyDone: TVerifyDoneEvent;
    function GetDeviceMaxReadSpeed: Word;
    function GetDeviceMaxWriteSpeed: Word;
    function GetReadSpeed: Word;
    function GetWriteSpeed: Word;
    function GetMaxWriteSpeed: Word;
    function GetDeviceBufferSize: LongWord;
    function GetDeviceBufferPosition: LongWord;
    function GetFreeBlocksOnDisc: LongWord;
    function GetTotalBlocksOnDisc: LongWord;
    function GetFileCount: LongWord;
    function GetDirCount: LongWord;
    function GetCachePosition: LongWord;
    function GetVolumeIDW: WideString;
    procedure SetVolumeIDW(VolumeLabel: WideString);
    function GetDeviceCount: Word;
    function GetErrorNumber: LongWord;
    function GetErrorString: AnsiString;
    function GetErrorStringW: WideString;
    function GetComponentStatus: Word;
    function GetImageSize: LongWord;
    function GetBlocksWritten: LongWord;
    function GetBurnProof: Boolean;
    procedure SetBurnProof(BufferUnderrunProtection: Boolean);
    function GetTestWrite: Boolean;
    procedure SetTestWrite(TestWriteDisc: Boolean);
    function GetFinalizeDisc: Boolean;
    procedure SetFinalizeDisc(CloseDisc: Boolean);
    function GetReplaceFile: Boolean;
    procedure SetReplaceFile(ReplaceFileInDiscLayout: Boolean);
    function GetVolumeID: AnsiString;
    procedure SetVolumeID(VolumeLabel: AnsiString);
    function GetCacheSize: LongWord;
    procedure SetCacheSize(ComponentCacheSize: LongWord);
    function GetJolietFileSystem: Boolean;
    procedure SetJolietFileSystem(JolietFileSystem: Boolean);
    function GetUDFBridge: Boolean;
    procedure SetUDFBridge(UDFBridgeDisc: Boolean);
    function GetVersion: AnsiString;
    function GetBootImage: AnsiString;
    procedure SetBootImage(BootableImage: AnsiString);
    function GetISO9660v1999: Boolean;
    procedure SetISO9660v1999(ISO9660v1999: Boolean);

  protected
    { Protected declarations }
    procedure WndProc(var Mssg: TMessage);
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    //procedure WMREFGENDONE(var Message: TWMChar); message WM_REFGENDONE;
    procedure ClearAll;
    procedure AbortOperation;
    function InitializeASPI(NoInternalASPI: Boolean = False; ASPIDLLPath: AnsiString = ''; Reserved1: AnsiString = ''): Boolean;
    function GetDeviceName(DeviceIndex: Byte): AnsiString;
    function SelectDevice(DeviceIndex: Byte): Boolean;
    function SelectDeviceByLetter(DriveLetter: AnsiChar): Boolean;
    function DeviceCan(DeviceCapabilityID: Byte): Boolean;
    function LockMedium: Boolean;
    function UnlockMedium: Boolean;
    function UnlockDrive: Boolean;
    function FlushCache: Boolean;
    function LoadMedium(Immediate: Boolean): Boolean;
    function EjectMedium(Immediate: Boolean): Boolean;
    function Erasable: Boolean;
    function Writable: Boolean;
    function TestUnitReady: Boolean;
    function DeviceIsBurner: Boolean;
    function SessionsOnDisc: Integer;
    function CreateDir(DestPath: AnsiString; DirName: AnsiString): PDirEntry;
    function CreateDirW(DestPath: WideString; DirName: WideString): PDirEntry;
    function InsertFile(DestPath: AnsiString; FileName: AnsiString): PFileEntry;
    function InsertFileW(DestPath: WideString; FileName: WideString): PFileEntry;
    function InsertFileWithName(DestPath: AnsiString; FileName: AnsiString; LongNameOnDisc: AnsiString; ShortNameOnDisc: AnsiString): PFileEntry;
    function InsertFileWithNameW(DestPath: WideString; FileName: WideString; LongNameOnDisc: WideString; ShortNameOnDisc: WideString): PFileEntry;
    function InsertMemoryFile(DestPath: AnsiString; FindFileData: TWin32FindDataA; OnGetData: TGetDataEvent): PFileEntry;
    function InsertMemoryFileW(DestPath: WideString; FindFileData: TWin32FindDataW; OnGetData: TGetDataEvent): PFileEntry;
    function InsertDir(DestPath: AnsiString; DirPath: AnsiString; FileSpecs: AnsiString; Recursive: Boolean; SavePath: Boolean): PDirEntry;
    function InsertDirW(DestPath: WideString; DirPath: WideString; FileSpecs: WideString; Recursive: Boolean; SavePath: Boolean): PDirEntry;
    function EraseDisc(Quick: Boolean): Boolean;
    function ImportSession(SessionNo: Byte; DestPath: AnsiString): Integer;
    function ImportSessionW(SessionNo: Byte; DestPath: WideString): Integer;
    function ReadVolumeLabelOfDisc(SessionNo: Byte): WideString;
    function Prepare: LongWord;
    function Burn: Boolean;
    function BurnISOImage(FileName: AnsiString): Boolean;
    function BurnISOImageW(FileName: WideString): Boolean;
    function BuildISOImage(FileName: AnsiString): Boolean;
    function BuildISOImageW(FileName: WideString): Boolean;
    function VerifyDisc(StopOnMismatch, FireVerifyEventOnAllFiles: Boolean): Boolean;
    function DiscIs: TMedium;

    function BMAvgWriteSpeed: WORD;
    function BMAvgReadSpeed: WORD;
    function BMWriteSpeed: WORD;
    function BMReadSpeed: WORD;

    function GenerateSmartReferences: Boolean;
    function SmartReferencesProgress: LongWord;
    function SmartReferencesSize: LongWord;
    function VerifyProgress: LongWord;
    function VerifySize: LongWord;

    function FilesCount: LongWord;
    function SourcePath(FileId: Integer): WideString;
    function DestinationPath(FileId: Integer): WideString;
    function FileName(FileId: Integer): WideString;
    function ShortFileName(FileId: Integer): AnsiString;
    function FileSize(FileId: Integer): Int64;
    function DirsCount: LongWord;
    function DirName(DirId: Integer): WideString;
    function IsDirectory(FileId: Integer): Boolean;
    function MountISOImage(ISOFileName: AnsiString): Boolean;
    function FindFile(Directory: AnsiString; FileToFind: AnsiString): Int64;
    function FileAddress(FileId: Integer): Int64;
    function ReadingFile: Integer;
    function WritingFile: Integer;
    procedure SetDeviceSpeed(ReadSpeed, WriteSpeed: Word);
    property DeviceMaxReadSpeed: Word read GetDeviceMaxReadSpeed;
    property DeviceMaxWriteSpeed: Word read GetDeviceMaxWriteSpeed;
    property ReadSpeed: Word read GetReadSpeed;
    property WriteSpeed: Word read GetWriteSpeed;
    property MaxWriteSpeed: Word read GetMaxWriteSpeed;
    property DeviceBufferSize: LongWord read GetDeviceBufferSize;
    property DeviceBufferPosition: LongWord read GetDeviceBufferPosition;
    property FreeBlocksOnDisc: LongWord read GetFreeBlocksOnDisc;
    property TotalBlocksOnDisc: LongWord read GetTotalBlocksOnDisc;
    property FileCount: LongWord read GetFileCount;
    property DirCount: LongWord read GetDirCount;
    property CachePosition: LongWord read GetCachePosition;
    property VolumeIDW: WideString read GetVolumeIDW write SetVolumeIDW;
    property DeviceCount: Word read GetDeviceCount;
    property ErrorNumber: LongWord read GetErrorNumber;
    property ErrorString: AnsiString read GetErrorString;
    property ErrorStringW: WideString read GetErrorStringW;
    property ComponentStatus: Word read GetComponentStatus;
    property ImageSize: LongWord read GetImageSize;
    property BlocksWritten: LongWord read GetBlocksWritten;

  published
    { Published declarations }
    property BurnProof: Boolean read GetBurnProof write SetBurnProof default True;
    property TestWrite: Boolean read GetTestWrite write SetTestWrite default False;
    property FinalizeDisc: Boolean read GetFinalizeDisc write SetFinalizeDisc default False;
    property ReplaceFile: Boolean read GetReplaceFile write SetReplaceFile default False;
    property VolumeID: AnsiString read GetVolumeID write SetVolumeID;
    property CacheSize: LongWord read GetCacheSize write SetCacheSize default 2;
    property JolietFileSystem: Boolean read GetJolietFileSystem write SetJolietFileSystem default True;
    property ISO9660v1999FS: Boolean read GetISO9660v1999 write SetISO9660v1999 default False;
    property UDFBridge: Boolean read GetUDFBridge write SetUDFBridge default True;
    property Version: AnsiString read GetVersion stored False;
    property BootImage: AnsiString read GetBootImage write SetBootImage;

    property OnAddFile: TAddFileEvent read FOnAddFile write FOnAddFile;
    property OnWriteDone: TWriteDoneEvent read FOnWriteDone write FOnWriteDone;
    property OnEraseDone: TEraseDoneEvent read FOnEraseDone write FOnEraseDone;
    property OnVerifyDone: TVerifyDoneEvent read FOnVerifyDone write FOnVerifyDone;
    property OnGenerateSmartReferencesDone: TSmartRefGenDoneEvent read FOnSmartRefGenDone write FOnSmartRefGenDone;
  end;

implementation

uses
  SysUtils, SyncObjs,
  mbpSCSILib, mbpTree, mbpCDBurner, mbpISO9660, mbpSmartReferences ;
  
procedure TMCDBurnerPro.WndProc(var Mssg: TMessage);
begin
  if Mssg.Msg = WM_REFGENDONE then
  begin
    FOnSmartRefGenDone(Self, Boolean(Mssg.wParam), Mssg.lParam);
  end
  else if Mssg.Msg = WM_WRITEDONE then
  begin
    FOnWriteDone(Self, 0, Boolean(Mssg.lParam));
  end
  else if Mssg.Msg = WM_ERASEDONE then
  begin
    FOnEraseDone(Self, 0, Boolean(Mssg.wParam));
  end
  else if Mssg.Msg = WM_VERIFYDONE then
  begin
    FOnVerifyDone(Self, 0, Mssg.wParam, Boolean(Mssg.lParam));
  end;
  Mssg.Result := DefWindowProc(CDFiles.hWNDFE, Mssg.Msg, Mssg.wParam, Mssg.lParam);
end;

constructor TMCDBurnerPro.Create(AOwner: TComponent);
begin
  Inherited;
  CDFiles.CriticalSection := TCriticalSection.Create;

  SetBurnProof(True);
  SetTestWrite(False);
  SetFinalizeDisc(False);
  SetReplaceFile(False);
  SetCacheSize(32);
  SetJolietFileSystem(True);
  SetUDFBridge(False);
  SetBootImage('');

  FOnWriteDone := nil;
  FOnEraseDone := nil;
  if (AOwner <> nil) then
  begin
    CDFiles.hWNDFE := AllocateHWnd(WndProc);
    Device.hWNDFE := CDFiles.hWNDFE;
  end;
  Device.CloseFH := True;
end;
//---------------------------------------------------------------------------

destructor TMCDBurnerPro.Destroy;
begin
  mbpSCSILib.CloseDevice(Device);
  ClearAll;
  CDFiles.CriticalSection.Free;
  DeallocateHWnd(CDFiles.hWNDFE);
  inherited;
end;
//---------------------------------------------------------------------------
function TMCDBurnerPro.BMAvgWriteSpeed: WORD;
begin
  result := Device.bmAvgWriteSpeed;
end;

//---------------------------------------------------------------------------
function TMCDBurnerPro.BMWriteSpeed: WORD;
begin
  result := Device.bmWriteSpeed;
  Device.bmWriteSpeed := Ceil(QPerfDeviceGet(device) / 1024.0);
end;

//---------------------------------------------------------------------------
function TMCDBurnerPro.BMReadSpeed: WORD;
begin
  result := Device.bmReadSpeed;
  Device.bmReadSpeed := Ceil(QPerfDeviceGet(device) / 1024.0);
end;

//---------------------------------------------------------------------------
function TMCDBurnerPro.BMAvgReadSpeed: WORD;
begin
  result := Device.bmAvgReadSpeed;
end;

//---------------------------------------------------------------------------
function TMCDBurnerPro.GetDeviceMaxWriteSpeed: Word;
begin
  Result := mbpSCSILib.GetDeviceMaxWriteSpeed(Device);
end;
//---------------------------------------------------------------------------

function TMCDBurnerPro.GetDeviceMaxReadSpeed: Word;
begin
  Result := mbpSCSILib.GetDeviceMaxReadSpeed(Device);
end;
//---------------------------------------------------------------------------

function TMCDBurnerPro.GetDeviceBufferSize: LongWord;
begin
  Result := mbpSCSILib.GetDeviceBufferSize(Device);
end;
//---------------------------------------------------------------------------

function TMCDBurnerPro.GetDeviceBufferPosition: LongWord;
begin
  Result := mbpSCSILib.GetDeviceBufferPosition(Device);
end;
//---------------------------------------------------------------------------

function TMCDBurnerPro.GetReadSpeed: Word;
begin
  Result := mbpSCSILib.GetReadSpeed(Device);
end;
//---------------------------------------------------------------------------

function TMCDBurnerPro.GetWriteSpeed: Word;
begin
  Result := mbpSCSILib.GetWriteSpeed(Device);
end;
//---------------------------------------------------------------------------

procedure TMCDBurnerPro.SetDeviceSpeed(ReadSpeed, WriteSpeed: Word);
begin
  mbpSCSILib.SetDeviceSpeed(Device, ReadSpeed, WriteSpeed);
end;
//---------------------------------------------------------------------------

function TMCDBurnerPro.GetFreeBlocksOnDisc: LongWord;
begin
  Result := mbpSCSILib.GetFreeBlocksOnDisc(Device);
end;
//---------------------------------------------------------------------------

function TMCDBurnerPro.GetTotalBlocksOnDisc: LongWord;
begin
  Result := mbpSCSILib.GetTotalBlocksOnDisc(Device);
end;
//---------------------------------------------------------------------------

function TMCDBurnerPro.GetFileCount: LongWord;
begin
  Result := mbpTree.GetFileCount(CDFiles);
end;
//---------------------------------------------------------------------------

function TMCDBurnerPro.GetDirCount: LongWord;
begin
  Result := mbpTree.GetDirCount(CDFiles);
end;
//---------------------------------------------------------------------------

function TMCDBurnerPro.GetCachePosition: LongWord;
begin
  Result := mbpCDBurner.GetCachePosition(CDFiles);
end;
//---------------------------------------------------------------------------

function TMCDBurnerPro.GetVolumeIDW: WideString;
begin
  Result := mbpISO9660.GetVolumeIDW(CDFiles);
end;
//---------------------------------------------------------------------------

procedure TMCDBurnerPro.SetVolumeIDW(VolumeLabel: WideString);
begin
  mbpISO9660.SetVolumeIDW(CDFiles, @VolumeLabel[1]);
end;
//---------------------------------------------------------------------------

function TMCDBurnerPro.GetDeviceCount: Word;
begin
  Result := mbpSCSILib.GetDeviceCount(Device); 
end;
//---------------------------------------------------------------------------

function TMCDBurnerPro.GetErrorNumber: LongWord;
begin
  Result := mbpCDBurner.GetErrorNumber(CDFiles, Device);
end;
//---------------------------------------------------------------------------

function TMCDBurnerPro.GetErrorString: AnsiString;
begin
  Result := mbpCDBurner.GetErrorString(CDFiles, Device);
end;
//---------------------------------------------------------------------------

function TMCDBurnerPro.GetErrorStringW: WideString;
begin
  Result := mbpCDBurner.GetErrorStringW(CDFiles, Device);
end;
//---------------------------------------------------------------------------

function TMCDBurnerPro.GetMaxWriteSpeed: Word;
begin
  Result := mbpSCSILib.GetMaxWriteSpeed(Device);
end;
//---------------------------------------------------------------------------

function TMCDBurnerPro.GetComponentStatus: Word;
begin
  Result := mbpTree.GetComponentStatus(CDFiles);
end;
//---------------------------------------------------------------------------

function TMCDBurnerPro.GetImageSize: LongWord;
begin
  Result := mbpISO9660.GetImageSize(CDFiles);
end;
//---------------------------------------------------------------------------

function TMCDBurnerPro.GetBlocksWritten: LongWord;
begin
  Result := mbpISO9660.GetBlocksWritten(CDFiles);
end;
//---------------------------------------------------------------------------
function TMCDBurnerPro.FlushCache: Boolean;
begin
  Result := mbpSCSILib.FlushCache(Device, True);
end;
//---------------------------------------------------------------------------
function TMCDBurnerPro.GetBurnProof: Boolean;
begin
  Result := mbpSCSILib.GetBurnProof(Device);
end;
//---------------------------------------------------------------------------

procedure TMCDBurnerPro.SetBurnProof(BufferUnderrunProtection: Boolean);
begin
  mbpSCSILib.SetBurnProof(Device, BufferUnderrunProtection);
end;
//---------------------------------------------------------------------------

function TMCDBurnerPro.GetTestWrite: Boolean;
begin
  Result := mbpSCSILib.GetTestWrite(Device);
end;
//---------------------------------------------------------------------------

procedure TMCDBurnerPro.SetTestWrite(TestWriteDisc: Boolean);
begin
  mbpSCSILib.SetTestWrite(Device, TestWriteDisc);
end;
//---------------------------------------------------------------------------

function TMCDBurnerPro.GetFinalizeDisc: Boolean;
begin
  Result := mbpSCSILib.GetFinalizeDisc(Device);
end;
//---------------------------------------------------------------------------

procedure TMCDBurnerPro.SetFinalizeDisc(CloseDisc: Boolean);
begin
  mbpSCSILib.SetFinalizeDisc(Device, CloseDisc);
end;
//---------------------------------------------------------------------------

function TMCDBurnerPro.GetReplaceFile: Boolean;
begin
  Result := mbpTree.GetReplaceFile(CDFiles);
end;
//---------------------------------------------------------------------------

procedure TMCDBurnerPro.SetReplaceFile(ReplaceFileInDiscLayout: Boolean);
begin
  mbpTree.SetReplaceFile(CDFiles, ReplaceFileInDiscLayout);
end;
//---------------------------------------------------------------------------

function TMCDBurnerPro.GetVolumeID: AnsiString;
begin
  Result := mbpISO9660.GetVolumeID(CDFiles);
end;
//---------------------------------------------------------------------------

procedure TMCDBurnerPro.SetVolumeID(VolumeLabel: AnsiString);
begin
  mbpISO9660.SetVolumeID(CDFiles, @VolumeLabel[1]);
end;
//---------------------------------------------------------------------------

function TMCDBurnerPro.GetCacheSize: LongWord;
begin
  Result := mbpISO9660.GetCacheSize(CDFiles) div (1024 * 1024);
end;
//---------------------------------------------------------------------------

procedure TMCDBurnerPro.SetCacheSize(ComponentCacheSize: LongWord);
begin
  mbpCDBurner.SetCacheSize(CDFiles, ComponentCacheSize * 1024 * 1024);
end;
//---------------------------------------------------------------------------

function TMCDBurnerPro.GetJolietFileSystem: Boolean;
begin
  Result := mbpISO9660.GetJolietFS(CDFiles);
end;
//---------------------------------------------------------------------------

procedure TMCDBurnerPro.SetJolietFileSystem(JolietFileSystem: Boolean);
begin
  mbpISO9660.SetJolietFS(CdFiles, JolietFileSystem);
end;
//---------------------------------------------------------------------------
function TMCDBurnerPro.GetISO9660v1999: Boolean;
begin
  result := CDFiles.ISO9660v1999FS;
end;
procedure TMCDBurnerPro.SetISO9660v1999(ISO9660v1999: Boolean);
begin
  CDFiles.ISO9660v1999FS := ISO9660v1999;
end;
function TMCDBurnerPro.GetUDFBridge: Boolean;
begin
  Result := mbpISO9660.GetUDFBridge(CDFiles);
end;
//---------------------------------------------------------------------------

procedure TMCDBurnerPro.SetUDFBridge(UDFBridgeDisc: Boolean);
begin
  mbpISO9660.SetUDFBridge(CDFiles, UDFBridgeDisc);
end;
//---------------------------------------------------------------------------

function TMCDBurnerPro.GetVersion: AnsiString;
begin
  Result := mbpTree.GetMCDBVersion;
end;
//---------------------------------------------------------------------------

function TMCDBurnerPro.GetBootImage: AnsiString;
begin
  Result := mbpTree.GetBootImage(CDFiles);
end;
//---------------------------------------------------------------------------

procedure TMCDBurnerPro.SetBootImage(BootableImage: AnsiString);
var
  FileHandle: Integer;
  FileSize: Int64;
  FileSizeLow, FileSizeHigh: LongWord;
begin
  if (BootableImage = '') then
    exit;
  if ((not (csLoading in ComponentState)) and (not (csDesigning in ComponentState)) ) then
  begin
    if (BootableImage <> '') then
    begin
      FileHandle := FileOpen(String(BootableImage), fmOpenRead or fmShareDenyNone);
      FileSizeLow := GetFileSize(FileHandle, @FileSizeHigh);
      FileSize := (FileSizeHigh shl 32) + FileSizeLow;
      FileClose(FileHandle);
      if (not ((FileSize = 2048) or (FileSize = 1228800) or (FileSize = 1474560) or (FileSize = 2949120))) then
      begin
        Exit;
      end;
    end;
  end;
  	
  mbpTree.SetBootImage(CDFiles, @BootableImage[1]);
end;
//---------------------------------------------------------------------------

procedure TMCDBurnerPro.ClearAll;
begin
  mbpTree.ClearAll(CDFiles);
end;
//---------------------------------------------------------------------------

procedure TMCDBurnerPro.AbortOperation;
begin
  CDFiles.ErrorNumber := ERR_ABORTED_BY_USER;
  CDFiles.ComponentStatus := CS_ABORTING;
  CDFiles.Aborted := True;

end;
//---------------------------------------------------------------------------

function TMCDBurnerPro.InitializeASPI(NoInternalASPI: Boolean = False; ASPIDLLPath: AnsiString = ''; Reserved1: AnsiString = ''): Boolean;
begin
  Result := mbpSCSILib.InitializeASPI(Device, NoInternalASPI, ASPIDLLPath, Reserved1);
end;
//---------------------------------------------------------------------------

function TMCDBurnerPro.GetDeviceName(DeviceIndex: Byte): AnsiString;
begin
  Result := mbpSCSILib.GetDeviceName(Device, DeviceIndex);
end;
//---------------------------------------------------------------------------

function TMCDBurnerPro.SelectDevice(DeviceIndex: Byte): Boolean;
begin
  Result := mbpSCSILib.SelectDevice(Device, DeviceIndex);
end;
//---------------------------------------------------------------------------

function TMCDBurnerPro.SelectDeviceByLetter(DriveLetter: AnsiChar): Boolean;
begin
  Result := mbpSCSILib.SelectDeviceByLetter(Device, DriveLetter);
end;
//---------------------------------------------------------------------------

function TMCDBurnerPro.DeviceCan(DeviceCapabilityID: Byte): Boolean;
begin
  Result := mbpSCSILib.DeviceCan(Device, DeviceCapabilityID);
end;
//---------------------------------------------------------------------------

function TMCDBurnerPro.LockMedium: Boolean;
begin
  Result := mbpSCSILib.LockMedium(Device);
end;
//---------------------------------------------------------------------------

function TMCDBurnerPro.UnlockMedium: Boolean;
begin
  Result := mbpSCSILib.UnlockMedium(Device);
end;
//---------------------------------------------------------------------------

function TMCDBurnerPro.UnlockDrive: Boolean;
begin
  Result := mbpSCSILib.UnlockDrive(Device);
end;
//---------------------------------------------------------------------------

function TMCDBurnerPro.LoadMedium(Immediate: Boolean): Boolean;
begin
  Result := mbpSCSILib.LoadMedium(Device, Immediate);
end;
//---------------------------------------------------------------------------

function TMCDBurnerPro.EjectMedium(Immediate: Boolean): Boolean;
begin
  Result := mbpSCSILib.EjectMedium(Device, Immediate);
end;
//---------------------------------------------------------------------------

function TMCDBurnerPro.Erasable: Boolean;
begin
  Result := mbpSCSILib.Erasable(Device);
end;
//---------------------------------------------------------------------------

function TMCDBurnerPro.Writable: Boolean;
begin
  Result := mbpSCSILib.Writable(Device);
end;
//---------------------------------------------------------------------------

function TMCDBurnerPro.TestUnitReady: Boolean;
begin
  Result := mbpSCSILib.TestUnitReady(Device);
end;
//---------------------------------------------------------------------------

function TMCDBurnerPro.DeviceIsBurner: Boolean;
begin
  Result := mbpSCSILib.DeviceIsBurner(Device);
end;
//---------------------------------------------------------------------------

function TMCDBurnerPro.SessionsOnDisc: Integer;
begin
  Result := mbpSCSILib.SessionsOnDisc(Device);
end;
//---------------------------------------------------------------------------

function TMCDBurnerPro.CreateDir(DestPath: AnsiString; DirName: AnsiString): PDirEntry;
begin
  Result := mbpTree.CreateDir(CDFiles, @DestPath[1], @DirName[1]);
end;
//---------------------------------------------------------------------------

function TMCDBurnerPro.CreateDirW(DestPath: WideString; DirName: WideString): PDirEntry;
begin
  Result := mbpTree.CreateDirW(CDFiles, @DestPath[1], @DirName[1]);
end;
//---------------------------------------------------------------------------

function TMCDBurnerPro.InsertFile(DestPath: AnsiString; FileName: AnsiString): PFileEntry;
begin
  CDFiles.cbAddFileEvent := FOnAddFile;
  Result := mbpTree.InsertFile(CDFiles, @DestPath[1], @FileName[1]);
end;
//---------------------------------------------------------------------------

function TMCDBurnerPro.InsertFileW(DestPath: WideString; FileName: WideString): PFileEntry;
begin
  CDFiles.cbAddFileEvent := FOnAddFile;
  Result := mbpTree.InsertFileW(CDFiles, @DestPath[1], @FileName[1]);
end;
//---------------------------------------------------------------------------

function TMCDBurnerPro.InsertFileWithName(DestPath: AnsiString; FileName: AnsiString; LongNameOnDisc: AnsiString; ShortNameOnDisc: AnsiString): PFileEntry;
begin
  CDFiles.cbAddFileEvent := FOnAddFile;
  Result := mbpTree.InsertFileWithName(CDFiles, @DestPath[1], @FileName[1], @LongNameOnDisc[1], @ShortNameOnDisc[1]);
end;
//---------------------------------------------------------------------------

function TMCDBurnerPro.InsertFileWithNameW(DestPath: WideString; FileName: WideString; LongNameOnDisc: WideString; ShortNameOnDisc: WideString): PFileEntry;    // DarkRapt0r was here ;-)
begin
  CDFiles.cbAddFileEvent := FOnAddFile;
  Result := mbpTree.InsertFileWithNameW(CDFiles, @DestPath[1], @FileName[1], @LongNameOnDisc[1], @ShortNameOnDisc[1]);
end;
//---------------------------------------------------------------------------

function TMCDBurnerPro.InsertMemoryFile(DestPath: AnsiString; FindFileData: TWin32FindDataA; OnGetData: TGetDataEvent): PFileEntry;
begin
  CDFiles.cbAddFileEvent := FOnAddFile;
  Result := mbpTree.InsertMemoryFile(CDFiles, @DestPath[1], FindFileData, OnGetData);
end;
//---------------------------------------------------------------------------

function TMCDBurnerPro.InsertMemoryFileW(DestPath: WideString; FindFileData: TWin32FindDataW; OnGetData: TGetDataEvent): PFileEntry;
begin
  CDFiles.cbAddFileEvent := FOnAddFile;
  Result := mbpTree.InsertMemoryFileW(CDFiles, @DestPath[1], FindFileData, OnGetData);
end;
//---------------------------------------------------------------------------

function TMCDBurnerPro.InsertDir(DestPath: AnsiString; DirPath: AnsiString; FileSpecs: AnsiString; Recursive: Boolean; SavePath: Boolean): PDirEntry;
begin
  CDFiles.cbAddFileEvent := FOnAddFile;
  Result := mbpTree.InsertDir(CDFiles, PAnsiChar(DestPath), PAnsiChar(DirPath), PAnsiChar(FileSpecs), Recursive, SavePath);
end;
//---------------------------------------------------------------------------

function TMCDBurnerPro.InsertDirW(DestPath: WideString; DirPath: WideString; FileSpecs: WideString; Recursive: Boolean; SavePath: Boolean): PDirEntry;
begin
  CDFiles.cbAddFileEvent := FOnAddFile;
  Result := mbpTree.InsertDirW(CDFiles, PWideChar(DestPath), PWideChar(DirPath), PWideChar(FileSpecs), Recursive, SavePath);
end;
//---------------------------------------------------------------------------

function TMCDBurnerPro.EraseDisc(Quick: Boolean): Boolean;
begin
  Result := mbpErase.EraseDisc(CDFiles, Device, Quick, FOnEraseDone);
end;
//---------------------------------------------------------------------------

function TMCDBurnerPro.ImportSession(SessionNo: Byte; DestPath: AnsiString): Integer;
begin
  Result := mbpCDBurner.ImportSession(CDFiles, Device, SessionNo, @DestPath[1]);
end;
//---------------------------------------------------------------------------

function TMCDBurnerPro.ImportSessionW(SessionNo: Byte; DestPath: WideString): Integer;
begin
  Result := mbpCDBurner.ImportSessionW(CDFiles, Device, SessionNo, @DestPath[1]);
end;
//---------------------------------------------------------------------------

function TMCDBurnerPro.Prepare: LongWord;
begin
  Result := mbpISO9660.Prepare(CDFiles);
end;
//---------------------------------------------------------------------------

function TMCDBurnerPro.Burn: Boolean;
begin
  {$IFDEF TRIAL}
  mbpSCSILib.DeviceCan(Device, 77);
  {$ENDIF}
  Result := mbpCDBurner.Burn(CDFiles, Device, FOnWriteDone, Self);
end;
//---------------------------------------------------------------------------

function TMCDBurnerPro.VerifyDisc(StopOnMismatch, FireVerifyEventOnAllFiles: Boolean): Boolean;
begin
  result := true;
  mbpVerify.VerifyDisc(CDFiles, Device, StopOnMismatch, FireVerifyEventOnAllFiles, FOnVerifyDone);
end;
//---------------------------------------------------------------------------

function TMCDBurnerPro.BurnISOImage(FileName: AnsiString): Boolean;
begin
  {$IFDEF TRIAL}
  mbpSCSILib.DeviceCan(Device, 77);
  {$ENDIF}

  Result := mbpCDBurner.BurnISOImage(CDFiles, Device, @FileName[1], FOnWriteDone, Self);
end;

//---------------------------------------------------------------------------
function TMCDBurnerPro.BurnISOImageW(FileName: WideString): Boolean;
begin
  {$IFDEF TRIAL}
  mbpSCSILib.DeviceCan(Device, 77);
  {$ENDIF}

  Result := mbpCDBurner.BurnISOImageW(CDFiles, Device, @FileName[1], FOnWriteDone, Self);
end;

//---------------------------------------------------------------------------
function TMCDBurnerPro.DiscIs: TMedium;
begin
  Result := mbpSCSILib.DiscIs(Device);
end;
//---------------------------------------------------------------------------

function TMCDBurnerPro.BuildISOImage(FileName: AnsiString): Boolean;
begin
  {$IFDEF TRIAL}
  mbpSCSILib.DeviceCan(Device, 77);
  {$ENDIF}
  Result := mbpCDBurner.BuildISOImage(CDFiles, @FileName[1], FOnWriteDone, Self);
end;

//---------------------------------------------------------------------------

function TMCDBurnerPro.BuildISOImageW(FileName: WideString): Boolean;
begin
  {$IFDEF TRIAL}
  mbpSCSILib.DeviceCan(Device, 77);
  {$ENDIF}
  Result := mbpCDBurner.BuildISOImageW(CDFiles, @FileName[1], FOnWriteDone, Self);
end;
//---------------------------------------------------------------------------
function TMCDBurnerPro.GenerateSmartReferences: Boolean;
begin
  CDFiles.cbGenSmRefEvent := FOnSmartRefGenDone;
  Result := mbpSmartReferences.GenerateSmartReferences(CDFiles);

end;

function TMCDBurnerPro.ReadVolumeLabelOfDisc(SessionNo: Byte): WideString;
begin
  Result := mbpCDBurner.ReadVolumeLabelOfDisc(device, SessionNo);


end;

function TMCDBurnerPro.SmartReferencesProgress;
begin
  Result := CDFiles.SmartReferencesProgress;
end;

function TMCDBurnerPro.SmartReferencesSize;
begin
  Result := CDFiles.SmartReferencesSize;
end;

function TMCDBurnerPro.VerifyProgress;
begin
  Result := CDFiles.VerifyProgress;
end;

function TMCDBurnerPro.VerifySize;
begin
  Result := CDFiles.VerifySize;
end;

function TMCDBurnerPro.FilesCount: LongWord;
begin
  result := CDFiles.FileCounter;
end;

function TMCDBurnerPro.SourcePath(FileId: Integer): WideString;
begin
  if (FileId >= CDFiles.FileCounter) then
  begin
    result := '';
  end
  else
  begin
    result := CDFiles.Files[FileId].Path;
  end;
end;

function TMCDBurnerPro.DestinationPath(FileId: Integer): WideString;
begin
  if (FileId >= CDFiles.FileCounter) then
  begin
    result := '';
  end
  else
  begin
    //result := GetDestinationPath;
  end;
end;

function TMCDBurnerPro.FileName(FileId: Integer): WideString;
begin
  if (FileId >= 0) and (FileId <=CDFiles.FileCounter) then
  begin
    result := CDFiles.Files[FileId].LongName;
  end
  else
  begin
    result := '';
  end;
end;

function TMCDBurnerPro.ShortFileName(FileId: Integer): AnsiString;
begin
  if (FileId >= CDFiles.FileCounter) then
  begin
    result := '';
  end
  else
  begin
    result := CDFiles.Files[FileId].ShortName;
  end;
end;

function TMCDBurnerPro.FileSize(FileId: Integer): Int64;
begin
  if (FileId >= CDFiles.FileCounter) then
  begin
    result := -1;
  end
  else
  begin
    result := CDFiles.Files[FileId].FileSize;
  end;
end;

function TMCDBurnerPro.DirsCount: LongWord;
begin
  result := CDFiles.DirCounter;
end;

function TMCDBurnerPro.DirName(DirId: Integer): WideString;
begin
  if (DirId >= CDFiles.DirCounter) then
  begin
    result := '';
  end
  else
  begin
    result := CDFiles.Dirs[DirId].LongName;
  end;
end;

function TMCDBurnerPro.IsDirectory(FileId: Integer): Boolean;
begin
  if (FileId >= CDFiles.FileCounter) then
  begin
    result := False;
  end
  else
  begin
    result := CDFiles.Files[FileId].DirRecord <> nil;
  end;
end;

function TMCDBurnerPro.MountISOImage(ISOFileName: AnsiString): Boolean;
begin
  result := mbpCDBurner.MountISOImage(CDFiles, Device, ISOFileName);
end;

function TMCDBurnerPro.FindFile(Directory: AnsiString; FileToFind: AnsiString): Int64;
begin
  //mbpCDBurner.MountISOImage(CDFiles, Device, ISOFileName, Directory, FileToFind);
  result := mbpTree.FindFileInternal(CDFiles, Directory, FileToFind);
end;

function TMCDBurnerPro.FileAddress(FileId: Integer): Int64;
begin
  if (FileId >= 0) and (FileId <= CDFiles.FileCounter) then
    result := CDFiles.Files[FileId].Address * 2048
  else
    result := -1
end;

function TMCDBurnerPro.ReadingFile: Integer;
begin
  result := cdfiles.ReadingFile;
end;
function TMCDBurnerPro.WritingFile: Integer;
begin
  result := cdfiles.WritingFile;
end;
end.

