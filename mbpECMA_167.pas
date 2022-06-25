//---------------------------------------------------------------------------

unit mbpECMA_167;
//---------------------------------------------------------------------------

interface

uses
  mbpUDFTypes;

{$I mbpDEFINES.INC}
  
const
  // Character Set Type
  CHARSPEC_TYPE_CS0 = $00;
  CHARSPEC_TYPE_CS1 = $01;
  CHARSPEC_TYPE_CS2 = $02;
  CHARSPEC_TYPE_CS3 = $03;
  CHARSPEC_TYPE_CS4 = $04;
  CHARSPEC_TYPE_CS5 = $05;
  CHARSPEC_TYPE_CS6 = $06;
  CHARSPEC_TYPE_CS7 = $07;
  CHARSPEC_TYPE_CS8 = $08;

  // Type and Time Zone
  TIMESTAMP_TYPE_MASK      = $F000;
  TIMESTAMP_TYPE_CUT       = $0000;
  TIMESTAMP_TYPE_LOCAL     = $1000;
  TIMESTAMP_TYPE_AGREEMENT = $2000;
  TIMESTAMP_TIMEZONE_MASK  = $0FFF;

  // Entity Identifier's Flags
  ENTITYID_FLAGS_DIRTY     = $00;
  ENTITYID_FLAGS_PROTECTED = $01;

  // Volume Structure Descriptor
  VSD_STD_ID_LEN = 5;

  // Standard Identifier
  VSD_STD_ID_NSR02 = 'NSR02';

  // Standard Identifier
  VSD_STD_ID_BEA01 = 'BEA01';
  VSD_STD_ID_BOOT2 = 'BOOT2';
  VSD_STD_ID_CD001 = 'CD001';
  VSD_STD_ID_CDW02 = 'CDW02';
  VSD_STD_ID_NSR03 = 'NSR03';
  VSD_STD_ID_TEA01 = 'TEA01';

  BOOT_FLAGS_ERASE = $01;

  // Tag Identifier
  TAG_IDENT_PVD  = 1;
  TAG_IDENT_AVDP = 2;
  TAG_IDENT_VDP  = 3;
  TAG_IDENT_IUVD = 4;
  TAG_IDENT_PD   = 5;
  TAG_IDENT_LVD  = 6;
  TAG_IDENT_USD  = 7;
  TAG_IDENT_TD   = 8;
  TAG_IDENT_LVID = 9;
  TAG_IDENT_FSD  = $0100;
  TAG_IDENT_FID  = $0101;
  TAG_IDENT_AED  = $0102;
  TAG_IDENT_IE   = $0103;
  TAG_IDENT_TE   = $0104;
  TAG_IDENT_FE   = $0105;
  TAG_IDENT_EAHD = $0106;
  TAG_IDENT_USE  = $0107;
  TAG_IDENT_SBD  = 264;
  TAG_IDENT_PIE  = $0109;
  TAG_IDENT_EFE  = $010A;

  PVD_FLAGS_VSID_COMMON = $0001;

  // Partition Flags
  PD_PARTITION_FLAGS_ALLOC = $0001;

  // Partition Contents
  PD_PARTITION_CONTENTS_NSR02 = '+NSR02';

  // Partition Contents
  PD_PARTITION_CONTENTS_FDC01 = '+FDC01';
  PD_PARTITION_CONTENTS_CD001 = '+CD001';
  PD_PARTITION_CONTENTS_CDW02 = '+CDW02';
  PD_PARTITION_CONTENTS_NSR03 = '+NSR03';

  // Access Type
  PD_ACCESS_TYPE_NONE         = $00000000;
  PD_ACCESS_TYPE_READ_ONLY    = $00000001;
  PD_ACCESS_TYPE_WRITE_ONCE   = $00000002;
  PD_ACCESS_TYPE_REWRITABLE   = $00000003;
  PD_ACCESS_TYPE_OVERWRITABLE = $00000004;

  // Partition Map Type
  GP_PARTITION_MAP_TYPE_UNDEF = $00;
  GP_PARTIITON_MAP_TYPE_1     = $01;
  GP_PARTITION_MAP_TYPE_2     = $02;

  // Integrity Type
  LVID_INTEGRITY_TYPE_OPEN  = $00000000;
  LVID_INTEGRITY_TYPE_CLOSE = $00000001;

  // File Characteristics
  FID_FILE_CHAR_HIDDEN    = $01;
  FID_FILE_CHAR_DIRECTORY = $02;
  FID_FILE_CHAR_DELETED   = $04;
  FID_FILE_CHAR_PARENT    = $08;
  FID_FILE_CHAR_METADATA  = $10;

  // Strategy Type
  ICBTAG_STRATEGY_TYPE_UNDEF = $0000;
  ICBTAG_STRATEGY_TYPE_1     = $0001;
  ICBTAG_STRATEGY_TYPE_2     = $0002;
  ICBTAG_STRATEGY_TYPE_3     = $0003;
  ICBTAG_STRATEGY_TYPE_4     = $0004;

  // File Type
  ICBTAG_FILE_TYPE_UNDEF     = $00;
  ICBTAG_FILE_TYPE_USE       = $01;
  ICBTAG_FILE_TYPE_PIE       = $02;
  ICBTAG_FILE_TYPE_IE        = $03;
  ICBTAG_FILE_TYPE_DIRECTORY = $04;
  ICBTAG_FILE_TYPE_REGULAR   = $05;
  ICBTAG_FILE_TYPE_BLOCK     = $06;
  ICBTAG_FILE_TYPE_CHAR      = $07;
  ICBTAG_FILE_TYPE_EA        = $08;
  ICBTAG_FILE_TYPE_FIFO      = $09;
  ICBTAG_FILE_TYPE_SOCKET    = $0A;
  ICBTAG_FILE_TYPE_TE        = $0B;
  ICBTAG_FILE_TYPE_SYMLINK   = $0C;
  ICBTAG_FILE_TYPE_STREAMDIR = $0D;

  // ICB Tag Flags
  ICBTAG_FLAG_AD_MASK        = $0007;
  ICBTAG_FLAG_AD_SHORT       = $0000;
  ICBTAG_FLAG_AD_LONG        = $0001;
  ICBTAG_FLAG_AD_EXTENDED    = $0002;
  ICBTAG_FLAG_AD_IN_ICB      = $0003;
  ICBTAG_FLAG_SORTED         = $0008;
  ICBTAG_FLAG_NONRELOCATABLE = $0010;
  ICBTAG_FLAG_ARCHIVE        = $0020;
  ICBTAG_FLAG_SETUID         = $0040;
  ICBTAG_FLAG_SETGID         = $0080;
  ICBTAG_FLAG_STICKY         = $0100;
  ICBTAG_FLAG_CONTIGUOUS     = $0200;
  ICBTAG_FLAG_SYSTEM         = $0400;
  ICBTAG_FLAG_TRANSFORMED    = $0800;
  ICBTAG_FLAG_MULTIVERSIONS  = $1000;
  ICBTAG_FLAG_STREAM         = $2000;

  // ICB Permissions
  FE_PERM_O_EXEC   = $00000001;
  FE_PERM_O_WRITE  = $00000002;
  FE_PERM_O_READ   = $00000004;
  FE_PERM_O_CHATTR = $00000008;
  FE_PERM_O_DELETE = $00000010;
  FE_PERM_G_EXEC   = $00000020;
  FE_PERM_G_WRITE  = $00000040;
  FE_PERM_G_READ   = $00000080;
  FE_PERM_G_CHATTR = $00000100;
  FE_PERM_G_DELETE = $00000200;
  FE_PERM_U_EXEC   = $00000400;
  FE_PERM_U_WRITE  = $00000800;
  FE_PERM_U_READ   = $00001000;
  FE_PERM_U_CHATTR = $00002000;
  FE_PERM_U_DELETE = $00004000;

  // ICB Record Format
  FE_RECORD_FMT_UNDEF          = $00;
  FE_RECORD_FMT_FIXED_PAD      = $01;
  FE_RECORD_FMT_FIXED          = $02;
  FE_RECORD_FMT_VARIABLE8      = $03;
  FE_RECORD_FMT_VARIABLE16     = $04;
  FE_RECORD_FMT_VARIABLE16_MSB = $05;
  FE_RECORD_FMT_VARIABLE32     = $06;
  FE_RECORD_FMT_PRINT          = $07;
  FE_RECORD_FMT_LF             = $08;
  FE_RECORD_FMT_CR             = $09;
  FE_RECORD_FMT_CRLF           = $0A;
  FE_RECORD_FMT_LFCR           = $0B;

  // ICB Record Display Attributes
  FE_RECORD_DISPLAY_ATTR_UNDEF = $00;
  FE_RECORD_DISPLAY_ATTR_1     = $01;
  FE_RECORD_DISPLAY_ATTR_2     = $02;
  FE_RECORD_DISPLAY_ATTR_3     = $03;

  // File Time Existence
  FTE_CREATION  = $00000001;
  FTE_DELETION  = $00000004;
  FTE_EFFECTIVE = $00000008;
  FTE_BACKUP    = $00000002;

  EXTATTR_CHAR_SET   = 1;
  EXTATTR_ALT_PERMS  = 3;
  EXTATTR_FILE_TIMES = 5;
  EXTATTR_INFO_TIMES = 6;
  EXTATTR_DEV_SPEC   = 12;
  EXTATTR_IMP_USE    = 2048;
  EXTATTR_APP_USE    = 65536;

  // Extent Length
  EXT_RECORDED_ALLOCATED         = $00000000;
  EXT_NOT_RECORDED_ALLOCATED     = $40000000;
  EXT_NOT_RECORDED_NOT_ALLOCATED = $80000000;
  EXT_NEXT_EXTENT_ALLOCDECS      = $C0000000;
//---------------------------------------------------------------------------

type
  // Character Set Specification
  charspec = packed record
    charSetType: Uint8;
    charSetInfo: array [0..62] of Uint8;
  end;

  // Recorded Address
  lb_addr = packed record
    logicalBlockNum: Uint32;
    partitionReferenceNum: Uint16;
  end;

  // Short Allocation Descriptor
  Pshort_ad = ^short_ad;
  short_ad = packed record
    extLength: Uint32;
    extPosition: Uint32;
  end;

  // Long Allocation Descriptor
  long_ad = packed record
    extLength: Uint32;
    extLocation: lb_addr;
    impUse: array [0..5] of Uchar;
  end;

  // Extended Allocation Descriptor
  ext_ad = packed record
    extLength: Uint32;
    recordedLength: Uint32;
    informationLength: Uint32;
    extLocation: lb_addr;
  end;


  // Timestamp
  Ptimestamp = ^timestamp;
  timestamp = packed record
    typeAndTimezone: Uint16;
    year: int16;
    month: Uint8;
    day: Uint8;
    hour: Uint8;
    minute: Uint8;
    second: Uint8;
    centiseconds: Uint8;
    hundredsOfMicroseconds: Uint8;
    microseconds: Uint8;
  end;

  // Entity Identifier
  regid = packed record
    flags: Uint8;
    ident: array [0..22] of AnsiChar;
    identSuffix: array [0..7] of AnsiChar;
  end;

  volStructDesc = packed record
    structType: Uchar;
    stdIdent: array [0..VSD_STD_ID_LEN - 1] of Uchar;
    structVersion: Uchar;
    structData: array [0..2040] of Uchar;
  end;

  // Beginning Extended Area Descriptor
  beginningExtendedAreaDesc = packed record
    structType: Uchar;
    stdIdent: array [0..VSD_STD_ID_LEN - 1] of Uchar;
    structVersion: Uchar;
    structData: array [0..2040] of Uchar;
  end;

  // Terminating Extended Area Descriptor
  terminatingExtendedAreaDesc = packed record
    structType: Uchar;
    stdIdent: array [0..VSD_STD_ID_LEN - 1] of Uchar;
    structVersion: Uchar;
    structData: array [0..2040] of Uchar;
  end;

  // Boot Descriptor
  bootDesc = packed record                 
    structType: Uchar;
    stdIdent: array [0..VSD_STD_ID_LEN - 1] of Uchar;
    structVersion: Uchar;
    reserved1: Uchar;
    archType: regid;
    bootIdent: regid;
    bootExtLocation: Uint32;
    bootExtLength: Uint32;
    loadAddress: Uint64;
    startAddress: Uint64;
    descCreationDateAndTime: timestamp;
    flags: Uint16;
    reserved2: array [0..31] of Uchar;
    bootUse: array [0..1905] of Uchar;
  end;

  // Extent Descriptor
  extent_ad = packed record
    extLength: Uint32;
    extLocation: Uint32;
  end;

  // Descriptor Tag
  PTag = ^Tag;
  Tag = packed record
    tagIdent: Uint16;
    descVersion: Uint16;
    tagChecksum: Uint8;
    reserved: Uchar;
    tagSerialNum: Uint16;
    descCRC: Uint16;
    descCRCLength: Uint16;
    tagLocation: Uint32;
  end;

  TDB = packed record
    T: Uchar;
    D: Uchar;
    I: Uchar;
    len: Uint16;
    res: Uchar;
    lowestTrack: Uchar;
    highestTrack: Uchar;
    trackNumber: Uchar;
    recordingMethod: Uchar;
    packetSize: array [0..2] of Uchar;
    res1: array [0..10] of Uchar;
  end;

  // NSR Descriptor
  NSRDesc = packed record
    structType: Uchar;
    stdIdent: array [0..VSD_STD_ID_LEN - 1] of Uchar;
    structVersion: Uchar;
    reserved: Uchar;
    structData: array [0..2039] of Uchar;
  end;

  // Primary Volume Descriptor
  PprimaryVolDesc = ^primaryVolDesc;
  primaryVolDesc = packed record
    descTag: Tag;
    volDescSeqNum: Uint32;
    primaryVolDescNum: Uint32;
    volIdent: array [0..31] of dstring;
    volSeqNum: Uint16;
    maxVolSeqNum: Uint16;
    interchangeLvl: Uint16;
    maxInterchangeLvl: Uint16;
    charSetList: Uint32;
    maxCharSetList: Uint32;
    volSetIdent: array [0..127] of dstring;
    descCharSet: charspec;
    explanatoryCharSet: charspec;
    volAbstract: extent_ad;
    volCopyright: extent_ad;
    appIdent: regid;
    recordingDateAndTime: timestamp;
    impIdent: regid;
    impUse: array [0..63] of Uchar;
    predecessorVolDescSeqLocation: Uint32;
    flags: Uint16;
    reserved: array [0..21] of Uchar;
  end;

  // Anchor Volume Descriptor Pointer
  PanchorVolDescPtr = ^anchorVolDescPtr;
  anchorVolDescPtr = packed record
    descTag: Tag;
    mainVolDescSeqExt: extent_ad;
    reserveVolDescSeqExt: extent_ad;
    reserved: array [0..479] of Uchar;
  end;

  // Volume Descriptor Pointer
  volDescPtr = packed record
    descTag: Tag;
    volDescSeqNum: Uint32;
    nextVolDescSeqExt: extent_ad;
    reserved: array [0..483] of Uchar;
  end;

  // LV Information
  LVInfo = packed record
    LVICharSet: charspec;
    LVId: array [0..127] of dstring;
    LVInfo1: array [0..35] of dstring;
    LVInfo2: array [0..35] of dstring;
    LVInfo3: array [0..35] of dstring;
    impIdent: regid;
    impUse: array [0..127] of Uint8;
  end;

  // Implementation Use Volume Descriptor
  PimpUseVolDesc = ^impUseVolDesc;
  impUseVolDesc = packed record
    descTag: Tag;
    volDescSeqNum: Uint32;
    impIdent: regid;
    impUse: LVInfo;
  end;

  // Partition Descriptor
  PpartitionDesc = ^partitionDesc;
  partitionDesc = packed record
    descTag: Tag;
    volDescSeqNum: Uint32;
    partitionFlags: Uint16;
    partitionNumber: Uint16;
    partitionContents: regid;
    partitionContentsUse: array [0..127] of Uchar;
    accessType: Uint32;
    partitionStartingLocation: Uint32;
    partitionLength: Uint32;
    impIdent: regid;
    impUse: array [0..127] of Uchar;
    reserved: array [0..155] of Uchar;
  end;

  PartitionMapType1 = packed record
    PartitionMapType: Uint8;
    PartitionMapLength: Uint8;
    VolumeSequenceNumber: Uint16;
    PartitionNumber: Uint16;
  end;

  PartitionMapType2 = packed record
    PartitionMapType: Uchar;
    len: Uchar;
    res: array [0..1] of Uchar;
    ident: regid;
    volSeqNumber: Uint16;
    partitionNumber: Uint16;
    reserved: array [0..23] of Uchar;
  end;

  // Generic Partition Map
  GenericPartitionMap = packed record
    partitionMapType: Uchar;
    partitionMapLength: Uchar;
    partitionMapping1stByte: Uchar;
  end;

  // Type 1 Partition Map
  GenericPartitionMap1 = packed record
    partitionMapType: Uchar;
    partitionMapLength: Uchar;
    volSeqNum: Uint16;
    partitionNum: Uint16;
  end;

  // Type 2 Partition Map
  GenericPartitionMap2 = packed record
    partitionMapType: Uchar;
    partitionMapLength: Uchar;
    partitionIdent: array [0..61] of Uchar;
  end;

  // Unallocated Space Descriptor
  PUnallocSpaceDesc = ^UnallocSpaceDesc;
  UnallocSpaceDesc = packed record
    descTag: Tag;
    volDescSeqNum: Uint32;
    numAllocDescs: Uint32;
  end;

  // Terminating Descriptor
  PTerminatingDesc = ^TerminatingDesc;
  TerminatingDesc = packed record
    descTag: Tag;
    reserved: array [0..495] of Uchar;
  end;

  // Logical Volume Integrity Descriptor
  PLogicalVolIntegrityDesc = ^LogicalVolIntegrityDesc;
  LogicalVolIntegrityDesc = packed record
    descTag: Tag;
    recordingDateAndTime: timestamp;
    integrityType: Uint32;
    nextIntegrityExt: extent_ad;
    logicalVolContentsUse: array [0..31] of Uchar;
    numOfPartitions: Uint32;
    lengthOfImpUse: Uint32;
    freeSpaceTable: Uint32;
    sizeTable: Uint32;
    impUse: array [0..47] of Uchar;
  end;

  // File Set Descriptor
  PFileSetDesc = ^FileSetDesc;
  FileSetDesc = packed record
    descTag: Tag;
    recordingDateAndTime: timestamp;
    interchangeLvl: Uint16;
    maxInterchangeLvl: Uint16;
    charSetList: Uint32;
    maxCharSetList: Uint32;
    fileSetNum: Uint32;
    fileSetDescNum: Uint32;
    logicalVolIdentCharSet: charspec;
    logicalVolIdent: array [0..127] of dstring;
    fileSetCharSet: charspec;
    fileSetIdent: array [0..31] of dstring;
    copyrightFileIdent: array [0..31] of dstring;
    abstractFileIdent: array [0..31] of dstring;
    rootDirectoryICB: long_ad;
    domainIdent: regid;
    nextExt: long_ad;
    streamDirectoryICB: long_ad;
    reserved: array [0..31] of Uchar;
  end;

  // Partition Header Descriptor
  PpartitionHeaderDesc = ^partitionHeaderDesc;
  partitionHeaderDesc = packed record
    unallocSpaceTable: short_ad;
    unallocSpaceBitmap: short_ad;
    partitionIntegrityTable: short_ad;
    freedSpaceTable: short_ad;
    freedSpaceBitmap: short_ad;
    reserved: array [0..87] of Uchar;
  end;

  // File Identifier Descriptor
  PfileIdentDesc = ^fileIdentDesc;
  fileIdentDesc = packed record
    descTag: Tag;
    fileVersionNum: Uint16;
    fileCharacteristics: Uint8;
    lengthFileIdent: Uint8;
    icb: long_ad;
    lengthOfImpUse: Uint16;
    // File Identifier
    // Padding: 4 * Integer((lengthFileIdent + 38 + 3) / 4) - (lengthFileIdent + 38)
  end;

  // Allocation Ext Descriptor
  PallocExtDesc = ^allocExtDesc;
  allocExtDesc = packed record
    descTag: Tag;
    previousAllocExtLocation: Uint32;
    lengthAllocDescs: Uint32;
  end;

  // ICB Tag
  icbtag = packed record
    priorRecordedNumDirectEntries: Uint32;
    strategyType: Uint16;
    strategyParameter: Uint16;
    numEntries: Uint16;
    reserved: Uchar;
    fileType: Uint8;
    parentICBLocation: lb_addr;
    flags: Uint16;
  end;

  // Indirect Entry
  indirectEntry = packed record
    descTag: Tag;
    icbTag: icbtag;
    indirectICB: long_ad;
  end;

  // Terminal Entry
  terminalEntry = packed record
    descTag: Tag;
    icbTag: icbtag;
  end;

  // File Entry
  PUDFFileEntry = ^UDFFileEntry;
  UDFFileEntry = packed record
    descTag: Tag;
    icbTag: icbtag;
    uid: Uint32;
    gid: Uint32;
    permissions: Uint32;
    fileLinkCount: Uint16;
    recordFormat: Uchar;
    recordDisplayAttr: Uchar;
    recordLength: Uint32;
    informationLength: Uint64;
    logicalBlocksRecorded: Uint64;
    accessTime: timestamp;
    modificationTime: timestamp;
    attrTime: timestamp;
    checkpoint: Uint32;
    extendedAttrICB: long_ad;
    impIdent: regid;
    uniqueID: Uint64;
    lengthExtendedAttr: Uint32;
    lengthAllocDescs: Uint32;
    AllocationDescriptors: array [0..(2048 - 176) - 1] of Uchar;
  end;

  // Extended Attribute Header Descriptor
  extendedAttrHeaderDesc = packed record
    descTag: Tag;
    impAttrLocation: Uint32;
    appAttrLocation: Uint32;
  end;

  // Generic Format
  genericFormat = packed record
    attrType: Uint32;
    attrSubtype: Uchar;
    reserved: array [0..2] of Uchar;
    attrLength: Uint32;
    attrData1stByte: Uchar;
  end;

  extenedAttributes = packed record
    header: extendedAttrHeaderDesc;
    content: genericFormat;
  end;

  // Character Set Information
  charSetInfo = packed record
    attrType: Uint32;
    attrSubtype: Uchar;
    reserved: array [0..2] of Uchar;
    attrLength: Uint32;
    escapeSeqLength: Uint32;
    charSetType: Uchar;
    escapeSeq1stByte: Uchar;
  end;

  // Alternate Permissions
  altPerms = packed record
    attrType: Uint32;
    attrSubtype: Uchar;
    reserved: array [0..2] of Uchar;
    attrLength: Uint32;
    ownerIdent: Uint16;
    groupIdent: Uint16;
    permission: Uint16;
  end;

  // File Times Extended Attribute
  fileTimesExtAttr = packed record
    attrType: Uint32;
    attrSubtype: Uchar;
    reserved: array [0..2] of Uchar;
    attrLength: Uint32;
    dataLength: Uint32;
    fileTimeExistence: Uint32;
    fileTimes: timestamp;
  end;

  // Information Times Extended Attribute
  infoTimesExtAttr = packed record
    attrType: Uint32;
    attrSubtype: Uchar;
    reserved: array [0..2] of Uchar;
    attrLength: Uint32;
    dataLength: Uint32;
    infoTimeExistence: Uint32;
    infoTimes1stByte: Uchar;
  end;

  // Device Specification
  deviceSpec = packed record
    attrType: Uint32;
    attrSubtype: Uchar;
    reserved: array [0..2] of Uchar;
    attrLength: Uint32;
    impUseLength: Uint32;
    majorDeviceIdent: Uint32;
    minorDeviceIdent: Uint32;
    impUse1stByte: Uchar;
  end;

  // Implementation Use Extended Attribute
  impUseExtAttr = packed record
    attrType: Uint32;
    attrSubtype: Uchar;
    reserved: array [0..2] of Uchar;
    attrLength: Uint32;
    impUseLength: Uint32;
    impIdent: regid;
    impUse1stByte: Uchar;
  end;

  // Application Use Extended Attribute
  appUseExtAttr = packed record
    attrType: Uint32;
    attrSubtype: Uchar;
    reserved: array [0..2] of Uchar;
    attrLength: Uint32;
    appUseLength: Uint32;
    appIdent: regid;
    appUse1stByte: Uchar;
  end;

  // Unallocated Space Entry
  unallocSpaceEntry = packed record
    descTag: Tag;
    icbTag: icbtag;
    lengthAllocDescs: Uint32;
    allocDescs1stByte: Uchar;
  end;

  // Space Bitmap Descriptor
  PSpaceBitmapDescriptor = ^SpaceBitmapDescriptor;
  SpaceBitmapDescriptor = packed record
    DescriptorTag: Tag;
    NumberOfBits: Uint32;
    NumberOfBytes: Uint32;
    // Bitmap
  end;

  // Partition Integrity Entry
  partitionIntegrityEntry = packed record
    descTag: Tag;
    icbTag: icbtag;
    recordingDateAndTime: timestamp;
    integrityType: Uchar;
    reserved: array [0..174] of Uchar;
    impIdent: regid;
    impUse: array [0..255] of Uchar;
  end;

  // Logical Volume Header Descriptor
  PlogicalVolHeaderDesc = ^logicalVolHeaderDesc;
  logicalVolHeaderDesc = packed record
    uniqueID: Uint64;
    reserved: array [0..23] of Uchar;
  end;

  // Path Component
  pathComponent = packed record
    componentType: Uchar;
    lengthComponentIdent: Uchar;
    componentFileVersionNum: Uint16;
    componentIdent1stByte: dstring;
  end;

  // File Entry
  extendedFileEntry = packed record
    descTag: Tag;
    icbTag: icbtag;
    uid: Uint32;
    gid: Uint32;
    permissions: Uint32;
    fileLinkCount: Uint16;
    recordFormat: Uchar;
    recordDisplayAttr: Uchar;
    recordLength: Uint32;
    informationLength: Uint64;
    objectSize: Uint64;
    logicalBlocksRecorded: Uint64;
    accessTime: timestamp;
    modificationTime: timestamp;
    createTime: timestamp;
    attrTime: timestamp;
    checkpoint: Uint32;
    reserved: Uint32;
    extendedAttrICB: long_ad;
    streamDirectoryICB: long_ad;
    impIdent: regid;
    uniqueID: Uint64;
    lengthExtendedAttr: Uint32;
    lengthAllocDescs: Uint32;
    extendedAttr1stByte: Uchar;
  end;
//---------------------------------------------------------------------------

implementation

end.
