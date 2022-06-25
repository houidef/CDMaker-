//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

USERES("MCDBP100C5.res");
USEPACKAGE("vcl50.bpi");
USEUNIT("mbpCache.pas");
USEUNIT("mbpCDBurner.pas");
USEUNIT("mbpClassesW.pas");
USEUNIT("mbpCommonLib.pas");
USEUNIT("mbpConsts.pas");
USEUNIT("mbpDeviceTypes.pas");
USEUNIT("mbpECMA_167.pas");
USEUNIT("mbpErase.pas");
USEUNIT("mbpExDDPro.pas");
USERES("mbpExDDPro.dcr");
USEUNIT("mbpHash.pas");
USEUNIT("mbpISO9660.pas");
USEUNIT("mbpMCDBPro.pas");
USERES("mbpMCDBPro.dcr");
USEUNIT("mbpOSTA_UDF_150.pas");
USEUNIT("mbpPropertyEditor.pas");
USEUNIT("mbpReg.pas");
USEUNIT("mbpSCSIDefs.pas");
USEUNIT("mbpSCSILib.pas");
USEUNIT("mbpSmartReferences.pas");
USEUNIT("mbpSysUtilsW.pas");
USEUNIT("mbpTree.pas");
USEUNIT("mbpTreeTypes.pas");
USEUNIT("mbpUDF_CRC.pas");
USEUNIT("mbpUDFBridge.pas");
USEUNIT("mbpUDFTime.pas");
USEUNIT("mbpUDFTypes.pas");
USEUNIT("mbpUDFUnicode.pas");
USEUNIT("mbpUDFUtils.pas");
USEUNIT("mbpVerify.pas");
USEUNIT("mbpWnASPI32.pas");
//---------------------------------------------------------------------------
#pragma package(smart_init)
//---------------------------------------------------------------------------

//   Package source.
//---------------------------------------------------------------------------

#pragma argsused
int WINAPI DllEntryPoint(HINSTANCE hinst, unsigned long reason, void*)
{
        return 1;
}
//---------------------------------------------------------------------------
