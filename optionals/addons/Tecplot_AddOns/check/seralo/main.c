#include "TECADDON.h"
#include "ADDGLBL.h"
#include "GUIDEFS.h"
#include "ENGINE.h"


AddOn_pa AddOnID;





/**
 * When Tecplot first loads an add-on, it makes a 
 * call to initialize the add-on. This function
 * must be named InitTecAddOn, as shown below.
 */
EXPORTFROMADDON void STDCALL InitTecAddOn(void)
{


  /*
   * NOTE:  TecUtilLockOn MUST be used for InitTecAddOn instead
   *        of TecUtilLockStart because AddonID has yet to be
   *        established.  TecUtilLockOn is in effect an "anonymous"
   *        locking of Tecplot (old style).
   */

  TecUtilLockOn();

  /*
   * The function TecUtilAddOnRegister() is the
   * only function that is REQUIRED to be called from
   * the initialization function.
   *
   * The information you give Tecplot by calling
   * this function will show up in the Help/About Add-ons
   * dialog box.
   */

  /*
   * Note that if your add-on requires a specific version of Tecplot,
   * you would check for that here using TecUtilGetTecplotVersion()
   */

  AddOnID = TecUtilAddOnRegister(110,
                                 ADDON_NAME,
                                 "V"ADDON_VERSION"("TecVersionId") "ADDON_DATE,
                                 "EDF");

  /*
   * Initialize the Tecplot GUI Builder libraries.
   */
  InitTGB();

  TecUtilImportAddLoader(LoaderCallback,
                         ADDON_NAME,
                         LoaderSelectedCallback,
                         NULL);




  /*
   * See note on TecUtilLockOn at start of this function.
   */
  TecUtilLockOff();
}

