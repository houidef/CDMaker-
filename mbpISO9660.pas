(*******************************************************************************
  Unit        : bmISO9660.PAS
  Date        : Sep 2002 - Sep 2008
  Author(s)   : Ehsan Khan
  Co-Author(s): Wasim Sabir
  Description :
  Copyright   : 2002-08 Binary Magic, All rights reserved.
*******************************************************************************)

unit mbpISO9660;

interface

uses
  {$IFDEF LINUX}
  Linux,
  {$ELSE}
  Windows,
  {$ENDIF}
  mbpTreeTypes, mbpTree, mbpCommonLib, mbpConsts;

{$I mbpDEFINES.INC}

function cew(w: WORD) : WORD;
function cedw(dw: Cardinal): Cardinal;
function msf2lba(h, m, s, f: Byte): Cardinal;

type
  TDirectoryDescriptor = packed record
    LenDr: Byte;                        // 01-01
    extended: Byte;                     // 02-02
    AddressL, AddressM: LongWord;       // 03-06
    DataLengthL: LongWord;              // 11-14
    DataLengthM: LongWord;              // 15-18
    year, month, day: Byte;             // 19-21
    hour, min, sec: Byte;               // 22-24
    TimeDiffernce: AnsiChar;                // 25-25
    FileFlag: Byte;                     // 26-26
    FileUnitSize: Byte;                 // 27-27
    InterleaveGap: Byte;                // 28-28
    VolSeqnumberL: WORD;                // 29-30
    VolSeqnumberM: WORD;                // 21-32
    FileIdentifierLen: Byte;            // 33-33
    FileName: array [0..255-1] of AnsiChar; // 34+
  end;

type
  TDirectoryDescriptor2 = packed record 
    LenDr: Byte;             // 01-01
    extended: Byte;          // 02-02
    AddressL: LongWord;      // 03-06
    AddressM: LongWord;      // 07-10
    DataLengthL: LongWord;   // 11-14
    DataLengthM: LongWord;   // 15-18
    year, month, day: Byte;  // 19-21
    hour, min, sec: Byte;    // 22-24
    timeDiffernce: AnsiChar;     // 25-25
    FileFlag: Byte;          // 26-26
    FileUnitSize: Byte;      // 27-27
    InterleaveGap: Byte;     // 28-28
    VolSeqnumberL: WORD;
    VolSeqnumberM: WORD;     // 29-32
    FileIdentifierLen: Byte; // 33-33
    dummy: Byte;             // 34-34
  end;

type
  TPathTableEntry = packed record
    LenDi,
    ExtAttr: Byte;
    Address: LongWord;
    ParentNumber: WORD;
    Name: array [0..255-1] of AnsiChar;
end;

type
  TVolumeDescriptor = packed record
    vdType: Byte;                                //   1 -   1
    identifier: array [0..5-1] of AnsiChar;          //   2 -   6
    version: Byte;                               //   7 -   7
    volume_flag: Byte;                           //   8 -   8
    IDSystem: array [0..32-1] of Byte;           //   9 -  40
    IDVolume: array [0..32-1] of AnsiChar;           //  41 -  72
    res1: array [0..8-1] of Byte;                //  73 -  80
    SectorsL: LongWord;                          //  81 -  84
    SectorsM: LongWord;                          //  85 -  88
    EscapeChars: array [0..32-1] of Byte;        //  89 - 120
    VolSetSizeL: WORD;                           // 121 - 124
    VolSetSizeM: WORD;                           // 121 - 124
    VolSeqNumberL: WORD;                         // 125 - 128
    VolSeqNumberM: WORD;                         // 125 - 128
    SectorSizeL: WORD;                           // 129 - 132
    SectorSizeM: WORD;                           // 129 - 132
    PathTableSizeL: LongWord;                    // 133 - 136
    PathTableSizeM: LongWord;                    // 137 - 140
    TypeLPathTable: LongWord;                    // 141 - 144
    TypeLPathEableO: LongWord;                   // 145 - 148
    TypeMPathTable: LongWord;                    // 149 - 152
    TypeMPathTableO: LongWord;                   // 153 - 156
    RootRec: TDirectoryDescriptor2;              // 157 - 190
    IDVolumeSet: array [0..128-1] of AnsiChar;       // 191 - 318
    IDPublisher: array [0..128-1] of AnsiChar;       //
    IDPreparer: array [0..128-1] of AnsiChar;        // 447 - 574
    IDApplication: array [0..128-1] of AnsiChar;     // 575 - 702
    FileCopyright: array [0..37-1] of AnsiChar;      // 703 - 739
    FileAbstract: array [0..37-1] of AnsiChar;       // 740 - 776
    FileBibliographic: array [0..37-1] of AnsiChar;  // 777 - 813
    DateCreation: array [0..17-1] of AnsiChar;       // 814 - 830
    DateModification: array [0..17-1] of AnsiChar;   // 831 - 847
    DateExpiration: array [0..17-1] of AnsiChar;     // 848 - 864
    DateEffective: array [0..17-1] of AnsiChar;      // 865 - 881
    FileStructureVer: Byte;                      // 882
    res2: Byte;                                  // 883
    ApplicationData: array [0..443-1] of AnsiChar;   // 884 - 1395
    ApplicationData2: array [0..7-1] of AnsiChar;    //
    ApplicationData3: array [0..62-1] of AnsiChar;
    res3: array [0..653-1] of AnsiChar;              // 1396 - 2048
end;

type
  TBootTVolumeDescriptor = packed record
    bvdType: Byte;
    Identifier: array [0..5-1] of AnsiChar;
    Version: AnsiChar;
    Ident: array [0..32-1] of AnsiChar;
    Unused1: array [0..32-1] of AnsiChar;
    BootCatLocation: LongWord;
    Unused2: array [0..1973-1] of AnsiChar;
  end;

type
  TBootCatalog = packed record
    Header: AnsiChar;
    PlatformID: AnsiChar;
    Reserved1: WORD;
    Developer: array [0..24-1] of AnsiChar;
    Checksum: WORD;
    KeyByte1, KeyByte2,
    BootIndicator,
    BootMediaType: Byte;
    LoadSegment: WORD;
    SystemType,
    Unused1: AnsiChar;
    SectorCount: WORD;
    LoadRBA: LongWord;
    Unused2: array [0..20-1] of AnsiChar;
    Unused3: array [0..1984-1] of AnsiChar;
  end;

//---------------------------------------------------------------------------
procedure WriteFiles(var CDFiles: TCDFiles);
function  Prepare(var CDFiles: TCDFiles): LongWord;
function  BuildHeader(var CDFiles: TCDFiles): Boolean;
procedure SetDirsDepth(d: pDirEntry; depth: Integer);
procedure SortPathTable(var CDFiles: TCDFiles);
function  CalcPathTableWidth(var CDFiles: TCDFiles): LongWord;
function  CalcPathTableWidthJ(var CDFiles: TCDFiles): LongWord;
function  CalcFileDirDescriptorWidthJ(var CDFiles: TCDFiles): LongWord;
function  CalcFileDirDescriptorWidth(var CDFiles: TCDFiles): LongWord;
function  GetImageSize(var CDFiles: TCDFiles): LongWord;
function  GetBlocksWritten(var CDFiles: TCDFiles): LongWord;
function  GetCacheSize(var CDFiles: TCDFiles): LongWord;
procedure SetVolumeID(var CDFiles: TCDFiles; VolumeID: PAnsiChar);
procedure SetVolumeIDW(var CDFiles: TCDFiles; VolumeID: PWideChar);
function  GetVolumeID(var CDFiles: TCDFiles): PAnsiChar;
function  GetVolumeIDW(var CDFiles: TCDFiles): PWideChar;
procedure SetFileExtents(var CDFiles: TCDFiles);
procedure WritePVD(var CDFiles: TCDFiles);
procedure WriteSVD(var CDFiles: TCDFiles);
procedure WriteJVD(var CDFiles: TCDFiles);
procedure WriteBVD(var CDFiles: TCDFiles);
procedure WriteTVD(var CDFiles: TCDFiles);
procedure WriteBootCatalog(var CDFiles: TCDFiles);
procedure WritePathTable(var CDFiles: TCDFiles; Most: Boolean);
procedure WritePathTableJ(var CDFiles: TCDFiles; Most: Boolean);
procedure WriteFileDirDescriptor(var CDFiles: TCDFiles);
procedure WriteFileDirDescriptorJ(var CDFiles: TCDFiles);
function  GetPostGap(var CDFiles: TCDFiles): Boolean;
procedure SetPostGap(var CDFiles: TCDFiles; PostGap: Boolean);
function  GetJolietFS(var CDFiles: TCDFiles): Boolean;
procedure SetJolietFS(var CDFiles: TCDFiles; JolietFS: Boolean);
function  GetUDFBridge(var CDFiles: TCDFiles): Boolean;
procedure SetUDFBridge(var CDFiles: TCDFiles; UDFBridge: Boolean);

var
  DirectoryDescriptor: TDirectoryDescriptor;
  DirectoryDescriptor2: TDirectoryDescriptor2;
  PathTableEntry: TPathTableEntry;
  VolumeDescriptor: TVolumeDescriptor;
  BootTVolumeDescriptor: TBootTVolumeDescriptor;
  BootCatalog: TBootCatalog;

implementation

uses
{$IFDEF WINDOWS}
  Windows,
{$ENDIF}
  SysUtils, mbpSysUtilsW, mbpClassesW, mbpCache, mbpUDFBridge;

var
  ZEROS: array [0..2352-1] of AnsiChar;

function cew(w: WORD) : WORD;
begin
  Result := ((w shl 8) and $FF00) or ((W shr 8) and $00FF);
end;

function cedw(dw: Cardinal): Cardinal;
begin
  Result := ((dw shl 24) and $FF000000) or
            ((dw shl  8) and $00FF0000) or
            ((dw shr  8) and $0000FF00) or
            ((dw shr 24) and $000000FF);
end;

function msf2lba(h, m, s, f: Byte): Cardinal;
begin
  Result := (h * 60 * 60 * 75) + (m * 60 * 75)+(s * 75) + (f);
end;

(*******************************************************************************

*******************************************************************************)
procedure copy_Char_upcased(src: PAnsiChar; dest: PAnsiChar; l: Integer);
var
  i, sl: Integer;
begin
  i:=0;
  sl := StrLen(src);
  while (i < l) do
  begin
    if (i < sl) then
    begin
      dest[i] := UpCase(src[i]);
      Inc(i);
    end
    else
    begin
      dest[i] := #0;
      Inc(i);
    end;
  end;
  dest[i] := #0;
end;

(*******************************************************************************

*******************************************************************************)
procedure copy_Char(src: PAnsiChar; dest: PAnsiChar; l: Integer);
var
  i, sl: Integer;
begin
  i:=0;
  sl := StrLen(src);
  while (i < l) do
  begin
    if (i < sl) then
    begin
      dest[i] := src[i];
      Inc(i);
    end
    else
    begin
      dest[i] := #0;
      Inc(i);
    end;
  end;
  dest[i] := #0;
end;

(*******************************************************************************

*******************************************************************************)
// Commented temporarily for killing D4 hints
(*
procedure copy_Char_j(src: PAnsiChar; dest: PAnsiChar; l: Integer);
var
  i, sl, j: Integer;
begin
  i:=0;
  j:=0;
  sl := StrLen(src);
  while (j < l) do
  begin
    if (j < sl)then
    begin
      dest[i] := #0;
      Inc(i);
      dest[i] := src[j];
      Inc(i);
      Inc(j);
    end
    else
    begin
      dest[i] := #0;
      Inc(i);
      Inc(j);
    end;
  end;
  dest[i] := #0;
end;
*)

(*******************************************************************************

*******************************************************************************)
procedure copy_CharW(src: PWideChar; dest: PAnsiChar; l: Integer);
var
  i, sl, j: Integer;
begin
  i:=0;
  j:=0;
  sl := StrLenW(src);
  while (j < l) do
  begin
    if (j < sl) then
    begin
      dest[i] := AnsiChar(Ord(src[j]) shr 8);
      Inc(i);
      dest[i] := AnsiChar(Ord(src[j]) mod 256);
      Inc(i);
      Inc(j);
    end
    else
    begin
      dest[i] := #0;
      Inc(i);
      Inc(j);
    end;
  end;
  dest[i] := #0;
  Inc(i);
  dest[i] := #0;
end;

(*******************************************************************************

*******************************************************************************)
procedure display_dir_depth(var d: TDirEntry);
var
  f: pFileEntry;
begin
  f := d.files;
  {$IFDEF CONDEBUG}
  WriteLn(inttostr(d.depth) + ' - ' + d.ShortName + ' - ' + d.LongName);
  {$ENDIF}
  while (f <> nil) do
  begin
    if ((f.attributes and FILE_ATTRIBUTE_DIRECTORY) = FILE_ATTRIBUTE_DIRECTORY) then
      display_dir_depth(f.dirrecord^);
    f := f.nextfile;
  end;
end;

(*******************************************************************************
 
*******************************************************************************)
// Commented temporarily for killing D4 hints
(*
procedure display_dir(var d: TDirEntry; depth: Integer);
var
  f: pFileEntry;
  {$IFDEF CONDEBUG}
  i: Integer;
  {$ENDIF}
begin
  f := d.files;
  while (f <> nil) do
  begin
    {$IFDEF CONDEBUG}
    for i:=0 to Pred(depth) do
      WriteLn('   ');
    {$IFDEF DELPHI6+}
      WriteLn(f.LongName);
    {$ELSE}
      WriteLn(WideCharToString(f.LongName));
    {$ENDIF}
    {$ENDIF}

    if ((f.attributes and FILE_ATTRIBUTE_DIRECTORY) = FILE_ATTRIBUTE_DIRECTORY) then
      display_dir(f.DirRecord^, Succ(depth));
    f := f.NextFile;
  end;
end;
*)

(*******************************************************************************

*******************************************************************************)
procedure WritePVD(var CDFiles: TCDFiles);
var
  vd: TVolumeDescriptor;
  d:  pDirEntry;
begin
  d := CDFiles.dirs[0];
  FillChar(vd, SizeOf(vd), 0);
  vd.vdType := 1;
  Move(CDFiles.VolumeLabel, vd.IDVolume, 32);
  StrUpper(vd.IDVolume);
  copy_Char_upcased('CD001', vd.identifier, 5);
  vd.version := 1;
  vd.volume_flag := 0;
  vd.FileStructureVer := 1;
  vd.VolSetSizeL := 1;
  vd.VolSetSizeM := 256;
  vd.VolSeqNumberL := 1;
  vd.VolSeqNumberM := 256;
  if (not CDFiles.ISO9660v1999FS) then
  begin
    vd.EscapeChars[0] := Ord('%');
    vd.EscapeChars[1] := Ord('/');
    vd.EscapeChars[2] := Ord('@');
  end;
  vd.PathTableSizeL := CDFiles.PathTableWidth;
  vd.PathTableSizeM := cedw(CDFiles.PathTableWidth);
  vd.TypeLPathTable := CDFiles.PathTableExtentL+CDFiles.FirstWritableSector;
  vd.TypeMPathTable := cedw(CDFiles.path_table_extent_m+CDFiles.FirstWritableSector);
  vd.SectorsL := CDFiles.TotalImageSize+CDFiles.VDImageSize; //+ CDFiles.FirstWritableSector;
  vd.SectorsM := cedw(CDFiles.TotalImageSize+CDFiles.VDImageSize); //+ CDFiles.FirstWritableSector;
  vd.SectorSizeL := 2048;
  vd.SectorSizeM := 8;
  vd.RootRec.LenDr := 34;
  vd.RootRec.extended := 0;
  vd.RootRec.FileFlag := 2;
  vd.FileStructureVer := 1;
  vd.RootRec.AddressL := CDFiles.FileDirDescriptorExtent+CDFiles.FirstWritableSector;
  vd.RootRec.AddressM := cedw(CDFiles.FileDirDescriptorExtent+CDFiles.FirstWritableSector);
  vd.RootRec.DataLengthL := d.DirSize;
  vd.RootRec.DataLengthM := cedw(d.DirSize);
  vd.RootRec.VolSeqnumberL := 1;
  vd.RootRec.VolSeqnumberM := 256;
  vd.RootRec.FileIdentifierLen := 1;
  mwrite(@vd, 2048, CDFiles);
end;

(*******************************************************************************

*******************************************************************************)
procedure WriteSVD(var CDFiles: TCDFiles);
var
  vd: TVolumeDescriptor;
  d:  pDirEntry;
begin
  d := CDFiles.dirs[0];
  FillChar(vd, SizeOf(vd), 0);
  vd.vdType := 2;
  Move(CDFiles.VolumeLabel, vd.IDVolume, 32);
  StrUpper(vd.IDVolume);
  copy_Char_upcased('CD001', vd.identifier, 5);
  vd.version := 2;
  vd.volume_flag := 0;
  vd.FileStructureVer := 1;
  vd.VolSetSizeL := 1;
  vd.VolSetSizeM := 256;
  vd.VolSeqNumberL := 1;
  vd.VolSeqNumberM := 256;
  vd.PathTableSizeL := CDFiles.PathTableWidth;
  vd.PathTableSizeM := cedw(CDFiles.PathTableWidth);
  vd.TypeLPathTable := CDFiles.PathTableExtentL+CDFiles.FirstWritableSector;
  vd.TypeMPathTable := cedw(CDFiles.path_table_extent_m+CDFiles.FirstWritableSector);
  vd.SectorsL := CDFiles.TotalImageSize+CDFiles.VDImageSize; //+ CDFiles.FirstWritableSector;
  vd.SectorsM := cedw(CDFiles.TotalImageSize+CDFiles.VDImageSize); //+ CDFiles.FirstWritableSector;
  vd.SectorSizeL := 2048;
  vd.SectorSizeM := 8;
  vd.RootRec.LenDr := 34;
  vd.RootRec.extended := 0;
  vd.RootRec.FileFlag := 2;
  vd.FileStructureVer := 2;
  vd.RootRec.AddressL := CDFiles.FileDirDescriptorExtent+CDFiles.FirstWritableSector;
  vd.RootRec.AddressM := cedw(CDFiles.FileDirDescriptorExtent+CDFiles.FirstWritableSector);
  vd.RootRec.DataLengthL := d.DirSize;
  vd.RootRec.DataLengthM := cedw(d.DirSize);
  vd.RootRec.VolSeqnumberL := 1;
  vd.RootRec.VolSeqnumberM := 256;
  vd.RootRec.FileIdentifierLen := 1;
  mwrite(@vd, 2048, CDFiles);
end;
(*******************************************************************************

*******************************************************************************)
procedure WriteBVD(var CDFiles: TCDFiles);
var
  vd: TBootTVolumeDescriptor;
begin
  FillChar(vd, SizeOf(vd), 0);
  vd.bvdType := 0;
  copy_Char_upcased('CD001', vd.Identifier, 5);
  copy_Char_upcased('EL TORITO SPECIFICATION', vd.Ident, 32);
  vd.BootCatLocation := CDFiles.BootCatalogLocation;
  vd.Version := #1;
  mwrite(@vd, 2048, CDFiles);
end;

(*******************************************************************************

*******************************************************************************)
procedure WriteBootCatalog(var CDFiles: TCDFiles);
var
  bc: TBootCatalog;
  w: array [0..32-1] of WORD;
  t: WORD;
  i: Integer;
begin
  t:=0;
  FillChar(bc, SizeOf(bc), 0);
  bc.Header := #1;
  StrCopy(bc.Developer, 'Magic CD/DVD Burner');
  bc.KeyByte1 := $55;
  bc.KeyByte2 := $AA;
  bc.BootIndicator := $88;
  bc.SectorCount := 1;
  if (CDFiles.BootImageSize = 2048) then
  begin
    bc.BootMediaType := 0;
    bc.SectorCount := 4;
  end
  else if (CDFiles.BootImageSize = 1228800) then
    bc.BootMediaType := 1
  else if (CDFiles.BootImageSize = 1474560) then
    bc.BootMediaType := 2
  else if (CDFiles.BootImageSize = 2949120) then
    bc.BootMediaType := 3
  else
    bc.BootMediaType := 4;
  bc.LoadRBA := CDFiles.BootImageLocation;
  i := 0;
  Move(bc, w, 64);
  while (i < 16) do
  begin
    t := t + w[i];
    Inc(i);
  end;
  w[0] := Succ(65535) - t;
  bc.Checksum := w[0];
  mwrite(@bc, SizeOf(bc), CDFiles);
end;

(*******************************************************************************

*******************************************************************************)
procedure WriteJVD(var CDFiles: TCDFiles);
var
  vd: TVolumeDescriptor;
  d: pDirEntry;
begin
  d := CDFiles.dirs[0];
  FillChar(vd, SizeOf(vd), 0);
  vd.vdType := 2;
  copy_Char_upcased('CD001', vd.identifier, 5);
  copy_CharW(CDFiles.VolumeLabelW, vd.IDVolume, 16);
  vd.version := 1;
  vd.volume_flag := 0;
  vd.FileStructureVer := 1;
  vd.VolSetSizeL := 1;
  vd.VolSetSizeM := 256;
  vd.VolSeqNumberL := 1;
  vd.VolSeqNumberM := 256;
  vd.EscapeChars[0] := Ord('%');
  vd.EscapeChars[1] := Ord('/');
  vd.EscapeChars[2] := Ord('@');
  vd.PathTableSizeL := CDFiles.PathTableWidthJ;
  vd.PathTableSizeM := cedw(CDFiles.PathTableWidthJ);
  vd.TypeLPathTable := CDFiles.path_table_extent_j_l+CDFiles.FirstWritableSector;
  vd.TypeMPathTable := cedw(CDFiles.path_table_extent_j_m+CDFiles.FirstWritableSector);
  vd.SectorsL := CDFiles.TotalImageSize+CDFiles.VDImageSize; //+ CDFiles.FirstWritableSector;
  vd.SectorsM := cedw(CDFiles.TotalImageSize+CDFiles.VDImageSize); //+ CDFiles.FirstWritableSector;
  vd.SectorSizeL := 2048;
  vd.SectorSizeM := 8;
  vd.RootRec.LenDr := 34;
  vd.RootRec.extended := 0;
  vd.RootRec.FileFlag := 2;
  vd.RootRec.AddressL := CDFiles.file_and_dir_descriptor_extent_j+CDFiles.FirstWritableSector;
  vd.RootRec.AddressM := cedw(CDFiles.file_and_dir_descriptor_extent_j+CDFiles.FirstWritableSector);
  vd.RootRec.DataLengthL := d.DirSizeJ;
  vd.RootRec.DataLengthM := cedw(d.DirSizeJ);
  vd.RootRec.VolSeqnumberL := 1;
  vd.RootRec.VolSeqnumberM := 256;
  vd.RootRec.FileIdentifierLen := 1;
  mwrite(@vd, 2048, CDFiles);
end;

(*******************************************************************************

*******************************************************************************)
procedure WriteTVD(var CDFiles: TCDFiles);
var
  vd: TVolumeDescriptor;
begin
  FillChar(vd, SizeOf(vd), 0);
  vd.vdType := 255;
  StrCopy(vd.identifier, 'CD001');
  vd.version := 1;
  mwrite(@vd, 2048, CDFiles);
end;

(*******************************************************************************

*******************************************************************************)
procedure WritePathTable(var CDFiles: TCDFiles; Most: Boolean);
var
  pt: TPathTableEntry;
  d: pDirEntry;
  i, l, len: Integer;
  str2: AnsiString;
begin
  len:=10;
  pt.Name[0] := #0;
  pt.Name[1] := #0;
  pt.ExtAttr := 0;
  if (Most) then
    pt.Address := cedw(CDFiles.FileDirDescriptorExtent+CDFiles.FirstWritableSector)
  else
    pt.Address := CDFiles.FileDirDescriptorExtent+CDFiles.FirstWritableSector;

  if (Most) then
    pt.ParentNumber := cew(1)
  else
    pt.ParentNumber := 1;

  pt.LenDi := 1;
  mwrite(@pt, 10, CDFiles);

  for i := 1 to Pred(CDFiles.DirCounter) do
  begin
    d := CDFiles.PathTableEntries[i];
    if (Most) then
    begin
       pt.Address := cedw(CDFiles.FileDirDescriptorExtent + d.Address + CDFiles.FirstWritableSector);
       pt.ParentNumber := cew(d.parent.number);
    end
    else
    begin
      pt.Address := CDFiles.FileDirDescriptorExtent + d.Address + CDFiles.FirstWritableSector;
      pt.ParentNumber := d.parent.number;
    end;
    if CDFiles.ISO9660v1999FS then
    begin
      str2 := AnsiString(WideCharToString(d.LongName));
      //CharUpperBuffW(Pointer(d.LongName), d.DirNameLengthJ);
      copy_Char(@str2[1], pt.Name, d.DirNameLengthJ);
//      copy_Char_upcased(d.LongName, pt.Name, d.DirNameLengthJ);
      //len := len + d.DirNameLengthJ+8;
      pt.LenDi := d.DirNameLengthJ;
      l := d.DirNameLengthJ;
      if (pt.LenDi mod 2 <> 0) then
        Inc(l);
    end
    else
    begin
      //len := len + d.DirNameLength+8;
      pt.LenDi := d.DirNameLength;
      copy_Char_upcased(d.ShortName, pt.Name, d.DirNameLength);
      l := d.DirNameLength;
      if (pt.LenDi mod 2 <> 0) then
        Inc(l);
    end;
    mwrite(@pt, l+8, CDFiles);
    len := len +l+8;
  end;
  if ((len mod 2048) <> 0) then
    mwrite(@ZEROS, 2048 - (len mod 2048), CDFiles);
end;

(*******************************************************************************

*******************************************************************************)
function CalcPathTableWidth(var CDFiles: TCDFiles): LongWord;
var
  d: pDirEntry;
  i, len: Integer;
begin
  len:=10;
  for i := 1 to Pred(CDFiles.DirCounter) do
  begin
    d := CDFiles.dirs[i];
    if CDFiles.ISO9660v1999FS then
      len := len + d.DirNameLengthJ+8
    else
      len := len + d.DirNameLength+8;
    if (d.DirNameLength mod 2 <> 0) then
      Inc(len);
  end;
  Result := len;
end;

(*******************************************************************************

*******************************************************************************)
procedure WritePathTableJ(var CDFiles: TCDFiles; Most: Boolean);
var
  pt: TPathTableEntry;
  d: pDirEntry;
  i, l, len: Integer;
begin
  len:=10;
  pt.Name[0] := #0;
  pt.Name[1] := #0;
  pt.ExtAttr := 0;
  if (Most) then
    pt.Address := cedw(CDFiles.file_and_dir_descriptor_extent_j + CDFiles.FirstWritableSector)
  else
    pt.Address := CDFiles.file_and_dir_descriptor_extent_j + CDFiles.FirstWritableSector;
  if (Most) then
  begin
    pt.ParentNumber := cew(1);
  end
  else
    pt.ParentNumber := 1;
  pt.LenDi := 1;
  mwrite(@pt, 10, CDFiles);
  for i := 1 to Pred(CDFiles.DirCounter) do
  begin
    d := CDFiles.PathTableEntries[i];
    if (Most) then
    begin
       pt.Address := cedw(CDFiles.file_and_dir_descriptor_extent_j + d.AddressJ + CDFiles.FirstWritableSector);
       pt.ParentNumber := cew(d.parent.number);
    end
    else
    begin
      pt.Address := CDFiles.file_and_dir_descriptor_extent_j + d.AddressJ + CDFiles.FirstWritableSector;
      pt.ParentNumber := d.parent.number;
    end;
    copy_CharW(d.LongName, pt.Name, d.DirNameLengthJ);
    pt.LenDi := d.DirNameLengthJ*2;
    l := d.DirNameLengthJ * 2;
    mwrite(@pt, l+8, CDFiles);
    len := len+l+8;
  end;
  if ((len mod 2048) <> 0 ) then
    mwrite(@ZEROS, 2048 - (len mod 2048), CDFiles);
end;

(*******************************************************************************

*******************************************************************************)
function  CalcPathTableWidthJ(var CDFiles: TCDFiles): LongWord;
var
  i, len: Integer;
begin
  len:=10;
  for i := 1 to Pred(CDFiles.DirCounter) do
    len := len+(CDFiles.dirs[i].DirNameLengthJ * 2)+8;
  Result := len;
end;

(*******************************************************************************

*******************************************************************************)
function calc_file_dir_descriptor_width_j_r(d: pDirEntry): LongWord;
var
  f: pFileEntry;
  l, bytes, total: Integer;
begin
  total := 68;
  bytes := 2048 - total;
  f := d.files;
  while (f <> nil) do
  begin
    l := f.FileNameLengthJ*2 + 33;
    if ((f.FileNameLengthJ*2) mod 2 = 0) then
      Inc(l);
    if (bytes - l < 0) then
    begin
      total := total +bytes + l;
      bytes := 2048 - l;
    end
    else
    begin
      bytes := bytes -l;
      total := total +l;
    end;
    f := f.NextFile;
  end;
  Result := Sectors(total+ bytes);
end;

(*******************************************************************************

*******************************************************************************)
function CalcFileDirDescriptorWidthJ(var CDFiles: TCDFiles): LongWord;
var
  i, total, w, a: Integer;
  d: pDirEntry;
begin
  total := 0;
  a := 0;
  for i:=0 to Pred(CDFiles.DirCounter) do
  begin
    d := CDFiles.dirs[i];
    w := calc_file_dir_descriptor_width_j_r(d);
    total := total +w;
    d.AddressJ := a;
    d.DirSizeJ := w * 2048;
    a := a + w;
  end;
  Result := total;
end;

(*******************************************************************************

*******************************************************************************)
function calc_file_dir_descriptor_width_r(ISO9660v1999FS: Boolean; d: pDirEntry): LongWord;
var
  f: pFileEntry;
  l, bytes, total: Integer;
begin
  total := 68;
  bytes := 2048 - total;
  f := d.files;
  while (f <> nil) do
  begin
    if (ISO9660v1999FS) then
    begin
      l := f.FileNameLengthJ + 33;
      if ((f.FileNameLengthJ) mod 2 = 0) then
        Inc(l);
    end
    else
    begin
      l := f.FileNameLength + 33;
      if (f.FileNameLength mod 2 = 0) then
        Inc(l);
    end;
    if (bytes - l < 0) then
    begin
      total := total +bytes + l;
      bytes := 2048 - l;
    end
    else
    begin
      bytes := bytes -l;
      total := total +l;
    end;
    f := f.NextFile;
  end;
  Result := Sectors(total+ bytes);
end;

(*******************************************************************************

*******************************************************************************)
function  CalcFileDirDescriptorWidth(var CDFiles: TCDFiles): LongWord;
var
  i, total, w, a: Integer;
  d: pDirEntry;
begin
  total := 0;
  a := 0;
  for i:=0 to Pred(CDFiles.DirCounter) do
  begin
    d := CDFiles.dirs[i];
    w := calc_file_dir_descriptor_width_r(CDFiles.ISO9660v1999FS, d);
    total := total +w;
    d.Address := a;
    d.DirSize := w * 2048;
    a := a + w;
  end;
  Result := total;
end;

procedure set_time(var CDFiles: TCDFiles; f: pFileEntry; var fd: TDirectoryDescriptor);
begin
  fd.year := f.time.wYear - 1900;
  fd.month := f.time.wMonth;
  fd.day := f.time.wDay;
  fd.hour := f.time.wHour;
  fd.min := f.time.wMinute;
  fd.sec := f.time.wSecond;
  fd.TimeDiffernce := AnsiChar(CDFiles.TimeZoneDifference);
end;

(*******************************************************************************

*******************************************************************************)
procedure write_file_dir_descriptor_r(var CDFiles: TCDFiles; d: pDirEntry);
var
  f: pFileEntry;
  dd: pDirEntry;
  fd: TDirectoryDescriptor;
  l, bytes, total: Integer;
  str2: AnsiString;
begin
  total := 68;
  bytes := 2048 - total;
  FillChar(fd, SizeOf(fd), 0);
  fd.LenDr := 34;
  fd.FileIdentifierLen := 1;
  fd.FileFlag := 2;
  fd.AddressL := d.Address + CDFiles.FileDirDescriptorExtent + CDFiles.FirstWritableSector;
  fd.AddressM := cedw(d.Address + CDFiles.FileDirDescriptorExtent + CDFiles.FirstWritableSector);
  fd.DataLengthL := d.DirSize;
  fd.DataLengthM := cedw(d.DirSize);
  fd.VolSeqnumberL := 1;
  fd.VolSeqnumberM := 256;
  fd.FileName[0] := #0;
  mwrite(@fd, 34, CDFiles);  // Current

  fd.FileName[0] := #1;
  fd.AddressL := d.parent.Address + CDFiles.FileDirDescriptorExtent + CDFiles.FirstWritableSector;
  fd.AddressM := cedw(d.parent.Address + CDFiles.FileDirDescriptorExtent + CDFiles.FirstWritableSector);
  fd.DataLengthL := d.parent.DirSize;
  fd.DataLengthM := cedw(d.parent.DirSize);
  mwrite(@fd, 34, CDFiles);  // Parent
  f := d.files;
  while (f <> nil) do
  begin
    set_time(CDFiles, f, fd);
    if ((f.attributes and FILE_ATTRIBUTE_DIRECTORY) <> FILE_ATTRIBUTE_DIRECTORY) then
    begin
      fd.FileFlag := 0;
      if (f.imported) then
      begin
        fd.AddressL := f.Address;
        fd.AddressM := cedw(f.Address);
      end
      else
      begin
        fd.AddressL := f.Address + CDFiles.FirstDataSector + CDFiles.FirstWritableSector;
        fd.AddressM := cedw(f.Address + CDFiles.FirstDataSector + CDFiles.FirstWritableSector);
      end;
      if (f.FileSize = 0) then
      begin
        fd.AddressL := 0;
        fd.AddressM := 0;
        fd.DataLengthL := 0;
        fd.DataLengthM := 0;
      end
      else
      begin
        fd.DataLengthL := f.FileSize;
        fd.DataLengthM := cedw(f.FileSize);
      end
    end
    else
    begin
      fd.FileFlag := 2;
      dd := f.DirRecord;
      if (d.DirSize = 0) then
      begin
        fd.AddressL := 0;
        fd.AddressM := 0;
      end
      else
      begin
        fd.AddressL := dd.Address + CDFiles.FileDirDescriptorExtent + CDFiles.FirstWritableSector;
        fd.AddressM := cedw(dd.Address + CDFiles.FileDirDescriptorExtent + CDFiles.FirstWritableSector);
      end;
      fd.DataLengthL := dd.DirSize;
      fd.DataLengthM := cedw(dd.DirSize);
    end;
    if (f.attributes and FILE_ATTRIBUTE_HIDDEN) = f.attributes then
      fd.FileFlag := fd.FileFlag or 1;
    if (CDFiles.ISO9660v1999FS) then
    begin
//      StrUpperCaseW(f.LongNam);
      //WideCharToMultiByte()
      str2 := AnsiString(WideCharToString(f.LongName));
      //CharUpperBuffW(Pointer(f.LongName), f.FileNameLengthJ);
      copy_Char(@str2[1], fd.FileName, f.FileNameLengthJ);
      l := f.FileNameLengthJ + 33;
      if (f.FileNameLengthJ mod 2 = 0) then
        Inc(l);
      fd.FileIdentifierLen := f.FileNameLengthJ;
    end
    else
    begin
      copy_Char_upcased(f.ShortName, fd.FileName, f.FileNameLength);
      l := f.FileNameLength + 33;
      if (f.FileNameLength mod 2 = 0) then
        Inc(l);
      fd.FileIdentifierLen := f.FileNameLength;
    end;

    if (bytes - l < 0) then
    begin
      mwrite(@ZEROS, bytes, CDFiles);  // Current
      bytes := 2048 - l;
    end
    else
    begin
      bytes := bytes -l;
    end;
    fd.LenDr := l;
    fd.VolSeqnumberL := 1;
    fd.VolSeqnumberM := 256;

    if (fd.LenDr <> l) then
      f := f;
    mwrite(@fd, fd.LenDr, CDFiles);
    f := f.NextFile;
  end;
  mwrite(@ZEROS, bytes, CDFiles);  // Current
end;

(*******************************************************************************

*******************************************************************************)
procedure WriteFileDirDescriptor(var CDFiles: TCDFiles);
var
  i: Integer;
  d: pDirEntry;
begin
  for i:=0 to Pred(CDFiles.DirCounter) do
  begin
    d := CDFiles.dirs[i];
    write_file_dir_descriptor_r(CDFiles, d);
  end;
end;

(*******************************************************************************

*******************************************************************************)
procedure write_file_dir_descriptor_j_r(var CDFiles: TCDFiles; d: pDirEntry);
var
  f: pFileEntry;
  dd: pDirEntry;
  fd: TDirectoryDescriptor;
  l, bytes, total: Integer;
begin
  total := 68;
  bytes := 2048 - total;
  FillChar(fd, SizeOf(fd), 0);
  fd.LenDr := 34;
  fd.FileIdentifierLen := 1;
  fd.FileFlag := 2;
  fd.AddressL := d.AddressJ + CDFiles.file_and_dir_descriptor_extent_j + CDFiles.FirstWritableSector;
  fd.AddressM := cedw(d.AddressJ + CDFiles.file_and_dir_descriptor_extent_j + CDFiles.FirstWritableSector);
  fd.DataLengthL := d.DirSizeJ;
  fd.DataLengthM := cedw(d.DirSizeJ);
  fd.VolSeqnumberL := 1;
  fd.VolSeqnumberM := 256;
  fd.FileName[0] := #0;
  mwrite(@fd, 34, CDFiles);  // Current

  fd.FileName[0] := #1;
  fd.AddressL := d.parent.AddressJ + CDFiles.file_and_dir_descriptor_extent_j + CDFiles.FirstWritableSector;
  fd.AddressM := cedw(d.parent.AddressJ + CDFiles.file_and_dir_descriptor_extent_j + CDFiles.FirstWritableSector);
  fd.DataLengthL := d.parent.DirSizeJ;
  fd.DataLengthM := cedw(d.parent.DirSizeJ);
  mwrite(@fd, 34, CDFiles);  // Parent
  f := d.files;
  while (f <> nil) do
  begin
    set_time(CDFiles, f, fd);
    if ((f.attributes and FILE_ATTRIBUTE_DIRECTORY) <> FILE_ATTRIBUTE_DIRECTORY) then
    begin
      fd.FileFlag := 0;
      if (f.imported) then
      begin
        fd.AddressL := f.Address;
        fd.AddressM := cedw(f.Address);
      end
      else
      begin
        fd.AddressL := f.Address + CDFiles.FirstDataSector + CDFiles.FirstWritableSector;
        fd.AddressM := cedw(f.Address + CDFiles.FirstDataSector + CDFiles.FirstWritableSector);
      end;
      if (f.FileSize = 0) then
      begin
        fd.AddressL := 0;
        fd.AddressM := 0;
        fd.DataLengthL := 0;
        fd.DataLengthM := 0;
      end
      else
      begin
        fd.DataLengthL := f.FileSize;
        fd.DataLengthM := cedw(f.FileSize);
      end;
    end
    else
    begin
      fd.FileFlag := 2;
      dd := f.DirRecord;
      if (d.DirSizeJ = 0) then
      begin
        fd.AddressL := 0;
        fd.AddressM := 0;
      end
      else
      begin
        fd.AddressL := dd.AddressJ + CDFiles.file_and_dir_descriptor_extent_j + CDFiles.FirstWritableSector;
        fd.AddressM := cedw(dd.AddressJ + CDFiles.file_and_dir_descriptor_extent_j + CDFiles.FirstWritableSector);
      end;
      fd.DataLengthL := dd.DirSizeJ;
      fd.DataLengthM := cedw(dd.DirSizeJ);
    end;
    if (f.attributes and FILE_ATTRIBUTE_HIDDEN) = f.attributes then
      fd.FileFlag := fd.FileFlag or 1;
    copy_CharW(f.LongName, fd.FileName, f.FileNameLengthJ);
    l := (f.FileNameLengthJ*2) + 33;
    if ((f.FileNameLengthJ*2) mod 2 = 0) then
      Inc(l);
    if (bytes - l < 0) then
    begin
      mwrite(@ZEROS, bytes, CDFiles);  // Current
      bytes := 2048 - l;
    end
    else
      bytes := bytes -l;
    fd.LenDr := l;
    fd.VolSeqnumberL := 1;
    fd.VolSeqnumberM := 256;
    fd.FileIdentifierLen := f.FileNameLengthJ*2;

    if (fd.LenDr <> l) then
      f := f;
    mwrite(@fd, fd.LenDr, CDFiles);
    f := f.NextFile;
  end;
  mwrite(@ZEROS, bytes, CDFiles);  // Current
end;

(*******************************************************************************

*******************************************************************************)
procedure WriteFileDirDescriptorJ(var CDFiles: TCDFiles);
var
  i: Integer;
  d: pDirEntry;
begin
  for i:=0 to Pred(CDFiles.DirCounter) do
  begin
    d := CDFiles.dirs[i];
    write_file_dir_descriptor_j_r(CDFiles, d);
  end;
end;

(*******************************************************************************

*******************************************************************************)
procedure SetFileExtents(var CDFiles: TCDFiles);
var
  i: Integer;
  extent: LongWord;
  str: array [0..1000-1] of AnsiChar;
  f: pFileEntry;
  SREntries : Boolean;
begin
  extent:=0;
  SREntries := False;
  for i:=0 to Pred(CDFiles.FileCounter) do
  begin
    f := CDFiles.files[i];
    if ((f.attributes and FILE_ATTRIBUTE_DIRECTORY) <> FILE_ATTRIBUTE_DIRECTORY) then
    begin
      if (f.FileSize = 0) then
        f.Address := 0
      else if (f.ReferenceEntry <> nil) then
        SREntries := True
      else if (not f.imported) then
      begin
        f.Address := extent; //CDFiles.FirstWritableSector;
        StrCopy(str, StrPCopy(str, AnsiString(WideCharToString(f.path))));
        {$IFDEF CONDEBUG}
        WriteLn(inttostr(extent+CDFiles.FirstWritableSector+150)+' - '+str+' '#9' '+inttostr(f.FileSize)+' ');
        {$ENDIF}
        extent := extent +Sectors(f.FileSize);
      end;
    end;
  end;
  if (SREntries) then for i := 0 to CDFiles.FileCounter-1 do
  begin
    f := CDFiles.files[i];
    if f.ReferenceEntry <> nil then
      f.Address := f.ReferenceEntry.Address;
  end;
  CDFiles.FilesSize := extent;
end;

(*******************************************************************************

*******************************************************************************)
procedure SetDirsDepth(d: pDirEntry; depth: Integer);
var
  f: pFileEntry;
begin
  if (d <> nil) then
  begin
    f := d.files;
    d.depth := depth;
    while (f <> nil) do
    begin
      if ((f.attributes and FILE_ATTRIBUTE_DIRECTORY) = FILE_ATTRIBUTE_DIRECTORY) then
      SetDirsDepth(f.DirRecord, Succ(depth));
      f := f.NextFile;
    end;
  end;
end;

(*******************************************************************************

*******************************************************************************)
function SortFunction( aa: pDirEntry; bb: pDirEntry): Integer;
begin
  if (aa.parent.number < bb.parent.number) then
  begin
    Result := -1;
    exit;
  end;
  if (aa.parent.number > bb.parent.number) then
  begin
    Result := 1;
    exit;
  end;
  Result := (StrCompW(aa.LongName, bb.LongName));
end;

(*******************************************************************************

*******************************************************************************)
procedure SortFiles(var List: PFileEntry; Joliet: Boolean);
var
  node, node2, List2: PFileEntry;
  //DirectoryFirst: Boolean;
begin
  if (List = nil) or (List.NextFile = nil) then exit;
  new(List2);
  list2.NextFile := list;
  list := list.NextFile;
  list2.NextFile.NextFile := nil;
  While List <> nil do
  begin
    Node  := List;
    list  := List.NextFile;
    Node2 := List2;
    //writeln('Res:',lstrcmpW( Node.LongName, Node2.NextFile.LongName),' ');
//    str1 := StrPasW(Node.LongName): WideString

//    While (Node2.NextFile <> nil) and (StrCompW( Node.LongName, Node2.NextFile.LongName) > 0) do
    While (Node2.NextFile <> nil) and (StrIComp(Node.ShortName, Node2.NextFile.ShortName) > 0) do
      Node2 := Node2.NextFile;
    Node.NextFile  := Node2.NextFile;
    Node2.NextFile := Node;
  end;
  List := List2.NextFile;
//  node := list;
  Dispose(List2);
end;

(*******************************************************************************

*******************************************************************************)
procedure QuickSort(var base: array of pDirEntry; L, R: Integer);
var
  I, J: Integer;
  P, T: Pointer;
begin
  repeat
    I := L;
    J := R;
    P := base[(L + R) shr 1];
    repeat
      while SortFunction(base[I], P) < 0 do
        Inc(I);
      while (SortFunction(base[J], P) > 0) do
        Dec(J);
      if I <= J then
      begin
        T := base[I];
        base[I] := base[J];
        base[J] := T;
        Inc(I);
        Dec(J);
      end;
    until I > J;
    if L < J then
      QuickSort(base, L, J);
    L := I;
  until I >= R;
end;

(*******************************************************************************

*******************************************************************************)
procedure SortPathTable(var CDFiles: TCDFiles);
var
  i, j, lpn, ln: Integer;
  p: pDirEntry;
label
  again;
begin
  if (CDFiles.DirCounter < 2) then
    exit;
  j := 0;
again:
  Inc(j);
  for i:=0 to Pred(CDFiles.DirCounter) do
    CDFiles.PathTableEntries[i].number := Succ(i);
  if (j > 1000) then
  begin
    {$IFDEF CONDEBUG}
    WriteLn('failed to sort path table');
    {$ENDIF}
    Beep;
    exit;
  end;

  QuickSort(CDFiles.PathTableEntries, 1, CDFiles.DirCounter-1);
  ln := CDFiles.PathTableEntries[0].number;
  lpn := CDFiles.PathTableEntries[0].parent.number;
  for i:=0 to Pred(CDFiles.DirCounter) do
  begin
    SortFiles(CDFiles.Dirs[i].Files, true);
  end;
  for i:=0 to Pred(CDFiles.DirCounter) do
  begin

    p := CDFiles.PathTableEntries[i];
    if (lpn > p.parent.number) then
      goto again;
    if (ln > p.number) then
      goto again;
    lpn := p.parent.number;
    ln  := p.number;
  end;
end;

(*******************************************************************************

*******************************************************************************)
function Prepare(var CDFiles: TCDFiles): LongWord;
{$IFDEF CONDEBUG}
var
  i: Integer;
{$ENDIF}
begin
  CDFiles.ComponentStatus := CS_PREPARING;
  SetDirsDepth(CDFiles.RootDir, 0);
  CDFiles.ISO9660FS := True;
  CDFiles.UDF12 := FALSE;
  {$IFDEF CONDEBUG}
  for i:=0 to Pred(CDFiles.DirCounter) do
    WriteLn(inttostr(Succ(i))+' - '+inttostr(CDFiles.PathTableEntries[i].parent.number)+' - '+inttostr(CDFiles.PathTableEntries[i].number)+' - '+inttostr(CDFiles.PathTableEntries[i].depth)+' - '+CDFiles.PathTableEntries[i].ShortName+' ('+CDFiles.PathTableEntries[i].parent.ShortName+')' );
  {$ENDIF}
  SortPathTable(CDFiles);
  {$IFDEF CONDEBUG}
  for i:=0 to Pred(CDFiles.DirCounter) do
    WriteLn('Index:'+inttostr(Succ(i))+' - Parent#:'+inttostr(CDFiles.PathTableEntries[i].parent.number)+' - Number:'+inttostr(CDFiles.PathTableEntries[i].number)+' - Depth:'+inttostr(CDFiles.PathTableEntries[i].depth)+' - '+CDFiles.PathTableEntries[i].ShortName+' ('+CDFiles.PathTableEntries[i].parent.ShortName+')');
  {$ENDIF}

  //display_dir_depth(CDFiles.RootDir^);
  CDFiles.FileDirDescriptorWidth := 0;
  FillChar(ZEROS, 2048, 0);
  CDFiles.Cursor := 16; // first 16 sec
  if (CDFiles.ISO9660FS) then
    Inc(CDFiles.Cursor);    // PVD
  if (CDFiles.ISO9660v1999FS) then
    Inc(CDFiles.Cursor);    // SVD
  if (CDFiles.BootImage[0] <> #0) then
    Inc(CDFiles.Cursor);  // BVD
  if (CDFiles.JolietFS) then
    Inc(CDFiles.Cursor);    // JVD
  Inc(CDFiles.Cursor);    // TVD

  if (CDFiles.UDFBridge) then
  begin
    PrepareUDFBridge_1(CDFiles, CDFiles.Cursor);
    CDFiles.Cursor := CDFiles.UDFBridgeInfo.FileSetDescriptor_LSAWRTSession + 2;
  end;

  {if (CDFiles.UDF12) then
    Inc(CDFiles.Cursor, 300);}

  if (CDFiles.ISO9660FS) then
  begin
    CDFiles.PathTableWidth := CalcPathTableWidth(CDFiles);
    CDFiles.PathTableExtentL := CDFiles.Cursor;
    CDFiles.Cursor := CDFiles.Cursor + Sectors(CDFiles.PathTableWidth);    // Path Table least
    CDFiles.path_table_extent_m := CDFiles.Cursor;
    CDFiles.Cursor := CDFiles.Cursor + Sectors(CDFiles.PathTableWidth);    // Path Table most
  end;
  if (CDFiles.JolietFS) then
  begin
    CDFiles.PathTableWidthJ := CalcPathTableWidthJ(CDFiles);
    CDFiles.path_table_extent_j_l := CDFiles.Cursor;
    CDFiles.Cursor := CDFiles.Cursor + Sectors(CDFiles.PathTableWidthJ);    // Path Table least
    CDFiles.path_table_extent_j_m := CDFiles.Cursor;
    CDFiles.Cursor := CDFiles.Cursor + Sectors(CDFiles.PathTableWidthJ);    // Path Table least
  end;
  if (CDFiles.ISO9660FS) then
  begin
    CDFiles.FileDirDescriptorExtent := CDFiles.Cursor;
    CDFiles.FileDirDescriptorWidth := CalcFileDirDescriptorWidth(CDFiles);
    CDFiles.Cursor := CDFiles.Cursor + CDFiles.FileDirDescriptorWidth;
  end;
  if (CDFiles.JolietFS) then
  begin
    CDFiles.file_and_dir_descriptor_extent_j := CDFiles.Cursor;
    CDFiles.file_and_dir_descriptor_width_j := CalcFileDirDescriptorWidthJ(CDFiles);
    CDFiles.Cursor := CDFiles.Cursor + CDFiles.file_and_dir_descriptor_width_j;
  end;
  if (CDFiles.BootImage[0] <> #0) then
  begin
    CDFiles.BootCatalogLocation := CDFiles.Cursor;
    Inc(CDFiles.Cursor);
  end;
  CDFiles.Blank := 150 - CDFiles.Cursor;
  if (CDFiles.Cursor < 150) then
  begin
    CDFiles.Cursor := 150;
  end;

  SetFileExtents(CDFiles);
  if (CDFiles.BootImage[0] <> #0) then
  begin
    CDFiles.BootImageLocation := CDFiles.Cursor;
    CDFiles.Cursor := CDFiles.Cursor + Sectors(CDFiles.BootImageSize);
  end;

  if (CDFiles.UDFBridge) then
  begin
    PrepareUDFBridge_2(CDFiles, CDFiles.Cursor);
    Inc(CDFiles.Cursor, CDFiles.UDFBridgeInfo.NumberOfBlocksRequiredForFileDirStructures);
  end;

  CDFiles.FirstDataSector := CDFiles.Cursor;
  CDFiles.TotalImageSize := CDFiles.FilesSize + CDFiles.FirstDataSector;

  if (CDFiles.UDFBridge) then
  begin
    if (((CDFiles.TotalImageSize * BlockSize) mod PacketSize) = 0) then
    begin
      CDFiles.UDFBridgeInfo.IsNewPacketRequiredForLastAVDP := True;
    end;

    Inc(CDFiles.TotalImageSize); // for last AVDP
  end;

  if (CDFiles.TotalImageSize mod $20 <> 0) then
    CDFiles.TotalImageSize := CDFiles.TotalImageSize + (32 - (CDFiles.TotalImageSize mod $20));

  if (CDFiles.UDFBridge) then
  begin
    PrepareUDFBridge_3(CDFiles, CDFiles.TotalImageSize - 1, CDFiles.FirstDataSector);
  end;

  CDFiles.Prepared := TRUE;
  CDFiles.ComponentStatus := CS_IDLE;

  Result := IfThen(CDFiles.UDFBridge, CDFiles.FilesSize + CDFiles.FirstDataSector + 1, CDFiles.FilesSize + CDFiles.FirstDataSector);
end;

(*******************************************************************************
*******************************************************************************)
procedure WriteVRS(var CDFiles: TCDFiles);
var
  Buffer: PAnsiChar;
begin
  Buffer := AllocMem(3 * BlockSize);

  BuildVRS(Buffer);
  mwrite(Buffer, 3 * BlockSize, CDFiles);

  FreeMem(Buffer);
end;
//---------------------------------------------------------------------------

procedure WriteAVDP(var CDFiles: TCDFiles);
var
  Buffer: PAnsiChar;
begin
  Buffer := AllocMem(BlockSize);

  BuildAVDP(CDFiles, Buffer, CDFiles.UDFBridgeInfo.AnchorVolumeDescriptorPointer_LSAWRTSession);
  mwrite(Buffer, BlockSize, CDFiles);

  FreeMem(Buffer);
end;
//---------------------------------------------------------------------------

procedure WriteVDSs(var CDFiles: TCDFiles);
var
  Buffer: PAnsiChar;
begin
  Buffer := AllocMem(32 * BlockSize);

  BuildVDSs(CDFiles, Buffer, CDFiles.VolumeLabelW);
  mwrite(Buffer, 32 * BlockSize, CDFiles);

  FreeMem(Buffer);
end;
//---------------------------------------------------------------------------

procedure WriteLVIS(var CDFiles: TCDFiles);
var
  Buffer: PAnsiChar;
begin
  Buffer := AllocMem(4 * BlockSize);

  BuildLVIS(CDFiles, Buffer);
  mwrite(Buffer, 4 * BlockSize, CDFiles);

  FreeMem(Buffer);
end;
//---------------------------------------------------------------------------

procedure WriteFSDS(var CDFiles: TCDFiles);
var
  Buffer: PAnsiChar;
begin
  Buffer := AllocMem(2 * BlockSize);

  BuildFSDS(CDFiles, Buffer);
  mwrite(Buffer, 2 * BlockSize, CDFiles);

  FreeMem(Buffer);
end;
(*******************************************************************************
*******************************************************************************)

(*******************************************************************************

*******************************************************************************)
function BuildHeader(var CDFiles: TCDFiles): Boolean;
var
  i: Integer;
  UDFBridgeInfo: PUDFBridgeInformation;
begin

  if (CDFiles.Prepared = FALSE) then
  begin
    //..SetError(CDFiles, ERR_NOT_PREPARED);
    Result := FALSE;
    exit;
  end;
  CDFiles.isom := allocmem(CDFiles.FirstDataSector * 2048);
  for i:=0 to Pred(16) do
    mwrite(@ZEROS, 2048, CDFiles);
  if (CDFiles.ISO9660FS) then
    WritePVD(CDFiles);
  if (CDFiles.BootImage[0] <> #0) then
    WriteBVD(CDFiles);
  if (CDFiles.ISO9660v1999FS) then
    WriteSVD(CDFiles);
  if (CDFiles.JolietFS) then
    WriteJVD(CDFiles);
  WriteTVD(CDFiles);

  if (CDFiles.UDFBridge) then
  begin
    UDFBridgeInfo := @CDFiles.UDFBridgeInfo;

    WriteVRS(CDFiles);
    for i := 0 to Pred(Int64(UDFBridgeInfo.MainVolumeDescriptorSequenceExtent_LSAWRTSession - (UDFBridgeInfo.VolumeRecognitionSequence_LSAWRTSession + 3))) do
    begin
      mwrite(@ZEROS[0], BlockSize, CDFiles);
    end;
    WriteVDSs(CDFiles);
    for i := 0 to Pred(Int64(UDFBridgeInfo.LogicalVolumeIntegrityDescriptor_LSAWRTSession - (UDFBridgeInfo.MainVolumeDescriptorSequenceExtent_LSAWRTSession + 32))) do
    begin
      mwrite(@ZEROS[0], BlockSize, CDFiles);
    end;
    WriteLVIS(CDFiles);
    for i := 0 to Pred(Int64(UDFBridgeInfo.AnchorVolumeDescriptorPointer_LSAWRTSession - (UDFBridgeInfo.LogicalVolumeIntegrityDescriptor_LSAWRTSession + 4))) do
    begin
      mwrite(@ZEROS[0], BlockSize, CDFiles);
    end;
    WriteAVDP(CDFiles);
    WriteFSDS(CDFiles);
  end;

  if (CDFiles.ISO9660FS) then
  begin
    WritePathTable(CDFiles, False); // L
    WritePathTable(CDFiles, True);  // M
  end;
  if (CDFiles.JolietFS) then
  begin
    WritePathTableJ(CDFiles, False); // L
    WritePathTableJ(CDFiles, True);  // M
  end;
  if (CDFiles.ISO9660FS) then
    WriteFileDirDescriptor(CDFiles);
  if (CDFiles.JolietFS) then
    WriteFileDirDescriptorJ(CDFiles);
  if (CDFiles.BootImage[0] <> #0) then
    WriteBootCatalog(CDFiles);
  if (CDFiles.Blank > 0) then
  begin
    for i:=0 to Pred(CDFiles.Blank) do
      mwrite(@ZEROS, 2048, CDFiles);
  end;
  Result := TRUE;
end;

(*******************************************************************************)
procedure SetVolumeID(var CDFiles: TCDFiles; VolumeID: PAnsiChar);
begin
  if (VolumeID = nil) then
    CDFiles.VolumeLabel[0] := #0
  else
  begin
    StrCopy(CDFiles.VolumeLabel, VolumeID);
    MultiByteToWideChar(0, 0, VolumeID, -1, CDFiles.VolumeLabelW, 32);
  end;
end;

(*******************************************************************************)
procedure SetVolumeIDW(var CDFiles: TCDFiles; VolumeID: PWideChar);
begin
  if (VolumeID = nil) then
    CDFiles.VolumeLabel[0] := #0
  else
  begin
    StrCopyW(CDFiles.VolumeLabelW, VolumeID);
    WideCharToMultiByte(0, 0, VolumeID, -1, CDFiles.VolumeLabel, Succ(StrLenW(VolumeID)), nil, nil);
  end;
end;

(*******************************************************************************)
function GetVolumeID(var CDFiles: TCDFiles): PAnsiChar;
begin
  Result := CDFiles.VolumeLabel;
end;

(*******************************************************************************)
function GetVolumeIDW(var CDFiles: TCDFiles): PWideChar;
begin
  Result := CDFiles.VolumeLabelW;
end;

(*******************************************************************************)
function GetCacheSize(var CDFiles: TCDFiles): LongWord;
begin
  Result := CDFiles.CacheSize;
end;

(*******************************************************************************)
function GetImageSize(var CDFiles: TCDFiles): LongWord;
begin
  Result := CDFiles.TotalImageSize;
end;

(*******************************************************************************)
function GetBlocksWritten(var CDFiles: TCDFiles): LongWord;
begin
  Result := CDFiles.BlocksWritten;
end;

(*******************************************************************************)
function GetPostGap(var CDFiles: TCDFiles): Boolean;
begin
  Result := CDFiles.PostGap;
end;

(*******************************************************************************)
procedure SetPostGap(var CDFiles: TCDFiles; PostGap: Boolean);
begin
  CDFiles.PostGap := PostGap;
end;

(*******************************************************************************)
function GetJolietFS(var CDFiles: TCDFiles): Boolean;
begin
  Result := CDFiles.JolietFS;
end;

(*******************************************************************************)
procedure SetJolietFS(var CDFiles: TCDFiles; JolietFS: Boolean);
begin
  CDFiles.JolietFS := JolietFS;
end;

(*******************************************************************************)
function  GetUDFBridge(var CDFiles: TCDFiles): Boolean;
begin
  Result := CDFiles.UDFBridge;
end;

(*******************************************************************************)
procedure SetUDFBridge(var CDFiles: TCDFiles; UDFBridge: Boolean);
begin
  CDFiles.UDFBridge := UDFBridge;
end;

(*******************************************************************************
For Integerernal testing only, do not use this
*******************************************************************************)
procedure WriteFiles(var CDFiles: TCDFiles);
var
  i: Integer;
  bytesleft{, pos}: Int64;
  src: TFileStreamW;//CDFILEStreamW;
  buf: array [0..2048-1] of AnsiChar;
  {$IFDEF CONDEBUG}
  extent: LongWord;
  {$ENDIF}
  str: array [0..2048-1] of AnsiChar;
  f: pFileEntry;
begin
  //extent:=0;
  {$IFDEF CONDEBUG}
  WriteLn('');
  extent := CDFiles.FirstDataSector;
  {$ENDIF}
  for i:=0 to Pred(CDFiles.FileCounter) do
  begin
    f := CDFiles.files[i];
    //for i:=0 to Pred(CDFiles.FileCounter) do WriteLn(#13+inttostr(i)+' / '+inttostr(CDFiles.FileCounter)+' - '+f.path);
    if i = CDFiles.FileCounter-2 then
      f := f;
    if (not f.imported) then
    begin
      if ((f.attributes and FILE_ATTRIBUTE_DIRECTORY) <> FILE_ATTRIBUTE_DIRECTORY) then
      begin
        //pos := CDFiles.fisoheader.Position;
        StrCopy(str, StrPCopy(str, AnsiString(WideCharToString(f.path))));
        {$IFDEF CONDEBUG}
        WriteLn(inttostr(f.Address+CDFiles.FirstDataSector)+' - '+inttostr(extent)+' - '+str+' '#9' '+inttostr(f.FileSize));
        {$ENDIF}
        ///src := CDFILEStreamW.Create(f.path, fmOpenRead);
        src := TFileStreamW.Create(f.path, fmOpenRead);
        if (src = nil) then
        begin
          {$IFDEF CONDEBUG}
          WriteLn('can''t open '+ f.path);
          {$ENDIF}
        end
        else
        begin
          bytesleft := f.FileSize;
          if (bytesleft <> 0) then
            repeat
              src.read(buf, 2048);
              mwrite(@buf, 2048, CDFiles);
              //f.Address := extent;
              bytesleft := bytesleft -2048;
              {$IFDEF CONDEBUG}
              Inc(extent);
              {$ENDIF}
            until not (bytesleft > 0);
          src.free;
        end;
      end;
    end;
  end;
end;

end.
