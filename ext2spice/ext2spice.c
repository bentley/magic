/*
 * ext2spice.c --
 *
 * Program to flatten hierarchical .ext files and produce
 * a .spice file, suitable for use as input to simulators
 * such as spice and hspice.
 *
 * Flattens the tree rooted at file.ext, reading in additional .ext
 * files as specified by "use" lines in file.ext.  The output is left
 * in file.spice, unless '-o esSpiceFile' is specified, in which case the
 * output is left in 'esSpiceFile'.
 *
 */

#ifndef lint
static char rcsid[] __attribute__ ((unused)) = "$Header: /usr/cvsroot/magic-8.0/ext2spice/ext2spice.c,v 1.8 2010/08/25 17:33:56 tim Exp $";
#endif  /* not lint */

#include <stdio.h>
#include <stdlib.h>		/* for atof() */
#include <string.h>
#include <ctype.h>

#include "tcltk/tclmagic.h"
#include "utils/magic.h"
#include "utils/malloc.h"
#include "utils/geometry.h"
#include "utils/hash.h"
#include "utils/dqueue.h"
#include "utils/utils.h"
#include "tiles/tile.h"
#ifdef MAGIC_WRAPPER
#include "database/database.h"
#include "windows/windows.h"
#include "textio/textio.h"
#include "dbwind/dbwind.h"	/* for DBWclientID */
#include "commands/commands.h"  /* for module auto-load */
#include "textio/txcommands.h"
#endif
#include "extflat/extflat.h"
#include "extflat/EFint.h"
#include "extract/extract.h"	/* for extDevTable */
#include "utils/runstats.h"

#include "ext2spice/ext2spice.h"

/* Options specific to ext2spice */
bool esDoExtResis = FALSE;
bool esDoPorts = TRUE;
bool esDoHierarchy = FALSE;
bool esDoResistorTee = FALSE;
int  esDoSubckt = AUTO;
bool esDevNodesOnly = FALSE;
bool esNoAttrs = FALSE;
bool esHierAP = FALSE;
char spcesDefaultOut[FNSIZE];
int  esCapAccuracy = 1;
char esCapFormat[FNSIZE];
char *spcesOutName = spcesDefaultOut;
FILE *esSpiceF = NULL;
float esScale = -1.0 ; /* negative if hspice the EFScale/100 otherwise */

unsigned short esFormat = SPICE3 ;

int esCapNum, esDevNum, esResNum, esDiodeNum;
int esNodeNum;  /* just in case we're extracting spice2 */
int esSbckNum; 	/* used in hspice node name shortening   */
int esNoModelType;  /* index for device type "None" (model-less device) */

/*
 * The following hash table and associated functions are used only if 
 * the format is hspice, to keep the translation between the hierarchical 
 * prefix of a node and the x num that we use to output valid hspice 
 * which also are meaningful.
 */
HashTable subcktNameTable ; /* the hash table itself */
DQueue    subcktNameQueue ; /* q used to print it sorted at the end*/

fetInfoList fetInfo[MAXDEVTYPES];


unsigned long	initMask = 0;

bool esMergeDevsA = FALSE; /* aggresive merging of devs L1=L2 merge them */
bool esMergeDevsC = FALSE; /* conservative merging of devs L1=L2 and W1=W2 */
			   /* used with the hspice multiplier */
bool esDistrJunct = FALSE;

/* 
 *---------------------------------------------------------
 * Variables & macros used for merging parallel devs       
 * The merging of devs is based on the fact that spcdevVisit 
 * visits the devs in the same order all the time so the 
 * value of esFMult[i] keeps the multiplier for the ith dev
 *---------------------------------------------------------
 */

float	*esFMult = NULL;         /* the array itself */
int	 esFMIndex = 0;          /* current index to it */
int	 esFMSize = FMULT_SIZE ; /* its current size (growable) */
int	 esDevsMerged;

/* macro to add a dev's multiplier to the table and grow it if necessary */
#define addDevMult(f) \
{  \
	if ( esFMult == NULL ) { \
	  esFMult = (float *) mallocMagic((unsigned) (esFMSize*sizeof(float)));  \
	} else if ( esFMIndex >= esFMSize ) {  \
	  int i;  \
	  float *op = esFMult ;  \
	  esFMult = (float *) mallocMagic((unsigned) ((esFMSize = esFMSize*2)*sizeof(float))); \
	  for ( i = 0 ; i < esFMSize/2 ; i++ ) esFMult[i] = op[i]; \
	  if (op) freeMagic(op); \
	}  \
	esFMult[esFMIndex++] = (float)(f); \
}

devMerge *devMergeList = NULL ;

#ifdef MAGIC_WRAPPER

#ifdef EXT2SPICE_AUTO
/*
 * ----------------------------------------------------------------------------
 *
 * Tcl package initialization function
 *
 * ----------------------------------------------------------------------------
 */

int
Exttospice_Init(interp)
    Tcl_Interp *interp;
{
    /* Sanity checks! */
    if (interp == NULL) return TCL_ERROR;
    if (Tcl_PkgRequire(interp, "Tclmagic", MAGIC_VERSION, 0) == NULL)
	return TCL_ERROR;
    if (Tcl_InitStubs(interp, "8.1", 0) == NULL) return TCL_ERROR;

    TxPrintf("Auto-loading EXTTOSPICE module\n");
    TxFlushOut();

    /* Replace the auto-load function with the one defined in	*/
    /* this package in the command functions list.		*/

    if (WindReplaceCommand(DBWclientID, "exttospice", CmdExtToSpice) < 0)
	return TCL_ERROR;

    /* Add "ext2spice" as an alias for "exttospice" */
    if (WindReplaceCommand(DBWclientID, "ext2spice", CmdExtToSpice) < 0)
	return TCL_ERROR;

    Tcl_PkgProvide(interp, "Exttospice", MAGIC_VERSION);
    return TCL_OK;
}
#endif /* EXT2SPICE_AUTO */

/*
 * ----------------------------------------------------------------------------
 *
 * Main callback for command "magic::exttospice"
 *
 * ----------------------------------------------------------------------------
 */

#define EXTTOSPC_RUN		0
#define EXTTOSPC_DEFAULT	1
#define EXTTOSPC_FORMAT		2
#define EXTTOSPC_RTHRESH	3
#define EXTTOSPC_CTHRESH	4
#define EXTTOSPC_MERGE		5
#define EXTTOSPC_EXTRESIST	6
#define EXTTOSPC_RESISTORTEE	7
#define EXTTOSPC_SCALE		8
#define EXTTOSPC_SUBCIRCUITS	9
#define EXTTOSPC_HIERARCHY	10
#define EXTTOSPC_HELP		11

void
CmdExtToSpice(w, cmd)
    MagWindow *w;
    TxCommand *cmd;
{
    int i,flatFlags;
    char *inName;
    FILE *f;

    int value;
    int option = EXTTOSPC_RUN;
    int argc = cmd->tx_argc;
    char **argv = cmd->tx_argv;
    char **msg;
    bool err_result, locDoSubckt;
    char *resstr;

    short sd_rclass;
    short sub_rclass;
    char *devname;
    char *subname;
    int idx, idx2;

    static EFCapValue LocCapThreshold = 2;
    static int LocResistThreshold = INFINITE_THRESHOLD; 

    static char *cmdExtToSpcOption[] = {
	"[run] [options]	run exttospice on current cell\n"
	"			use \"run -help\" to get standard options",
	"default		reset to default values",
	"format [<type>]	set output format",
	"rthresh [<value>]	set resistance threshold value",
	"cthresh [<value>]	set capacitance threshold value",
	"merge [<type>]		merge parallel transistors",
	"extresist [on|off]	incorporate information from extresist",
	"resistor tee [on|off]	model resistor capacitance as a T-network",
	"scale [on|off]		use .option card for scaling",
	"subcircuits [on|off]	standard cells become subcircuit calls",
	"hierarchy [on|off]	output hierarchical spice for LVS",
	"help			print help information",
	NULL
    };

    static char *cmdMergeTypes[] = {
	"none			don't merge parallel devices",
	"conservative		merge devices with same L, W",
	"aggressive		merge devices with same L",
	NULL
    };

    static char *cmdExtToSpcFormat[] = {
	"spice2",
	"spice3",
	"hspice",
	NULL
    };

    static char *yesno[] = {
	"yes",
	"true",
	"on",
	"no",
	"false",
	"off",
	NULL
    };

    static char *subcktopts[] = {
	"yes",
	"true",
	"on",
	"no",
	"false",
	"off",
	"automatic",
	"top",
	"descend",
	NULL
    };

    typedef enum {
	IDX_YES, IDX_TRUE, IDX_ON, IDX_NO, IDX_FALSE, IDX_OFF,
	IDX_AUTO, IDX_TOP, IDX_DESCEND
    } yesnoType;  

    esNoModelType = -1;

    if (cmd->tx_argc > 1)
    {
	option = Lookup(cmd->tx_argv[1], cmdExtToSpcOption);
	if (option < 0) option = EXTTOSPC_RUN;
	else argv++;
    }

    switch (option)
    {
	case EXTTOSPC_EXTRESIST:
	    if (cmd->tx_argc == 2)
	    {
		Tcl_SetResult(magicinterp, (esDoExtResis) ? "on" : "off", NULL);
		return;
	    }
	    else if (cmd->tx_argc != 3)
		goto usage;
	    idx = Lookup(cmd->tx_argv[2], yesno);
	    if (idx < 0) goto usage;
	    else if (idx < 3) esDoExtResis = TRUE;
	    else esDoExtResis = FALSE;
	    break;

	case EXTTOSPC_RESISTORTEE:
	    if (cmd->tx_argc == 3)
	    {
		Tcl_SetResult(magicinterp, (esDoResistorTee) ? "on" : "off", NULL);
		return;
	    }
	    else if (cmd->tx_argc != 4)
		goto usage;
	    idx = Lookup(cmd->tx_argv[3], yesno);
	    if (idx < 0) goto usage;
	    else if (idx < 3) esDoResistorTee = TRUE;
	    else esDoResistorTee = FALSE;
	    break;

	case EXTTOSPC_SCALE:
	    if (cmd->tx_argc == 2)
	    {
		Tcl_SetResult(magicinterp, (esScale < 0) ? "on" : "off", NULL);
		return;
	    }
	    else if (cmd->tx_argc != 3)
		goto usage;
	    idx = Lookup(cmd->tx_argv[2], yesno);
	    if (idx < 0) goto usage;
	    else if (idx < 3) esScale = -1.0;
	    else esScale = 0.0;
	    break;

	case EXTTOSPC_HIERARCHY:
	    if (cmd->tx_argc == 2)
	    {
		Tcl_SetResult(magicinterp, (esDoHierarchy) ? "on" : "off", NULL);
		return;
	    }
	    idx = Lookup(cmd->tx_argv[2], yesno);
	    if (idx < 0) goto usage;
	    else if (idx < 3)	/* yes */
		esDoHierarchy = TRUE;
	    else	 /* no */
		esDoHierarchy = FALSE;
	    break;

	case EXTTOSPC_SUBCIRCUITS:
	    if (cmd->tx_argc == 2)
	    {
		Tcl_SetResult(magicinterp, (esDoPorts) ? "on" : "off", NULL);
		return;
	    }
	    idx = Lookup(cmd->tx_argv[2], subcktopts);
	    switch (idx) {
		case IDX_YES: case IDX_TRUE: case IDX_ON:
		    esDoPorts = TRUE;
		    return;
		    break;
		case IDX_NO: case IDX_FALSE: case IDX_OFF:
		    esDoPorts = FALSE;
		    return;
		    break;
		case IDX_DESCEND:
		    if (cmd->tx_argc == 3)
		    {
			Tcl_SetResult(magicinterp, (esDoPorts) ? "on" : "off", NULL);
			return;
		    }
		    break;
		case IDX_TOP:
		    if (cmd->tx_argc == 3)
		    {
			Tcl_SetResult(magicinterp, (esDoSubckt) ? "on" : "off", NULL);
			return;
		    }
		    break;
		default:
		    goto usage;
		    break;
	    }

	    if (cmd->tx_argc != 4) goto usage;
	    idx2 = Lookup(cmd->tx_argv[3], subcktopts);
	    switch (idx2) {
		case IDX_YES: case IDX_TRUE: case IDX_ON:
		    if (idx == IDX_DESCEND)
			esDoPorts = TRUE;
		    else
			esDoSubckt = TRUE;
		    break;
		case IDX_NO: case IDX_FALSE: case IDX_OFF:
		    if (idx == IDX_DESCEND)
			esDoPorts = FALSE;
		    else
			esDoSubckt = FALSE;
		    break;
		case IDX_AUTO:
		    esDoSubckt = AUTO;
		    break;
		default:
		    goto usage;
	    }
	    break;

	case EXTTOSPC_FORMAT:
	    if (cmd->tx_argc == 2)
	    {
		Tcl_SetResult(magicinterp, cmdExtToSpcFormat[esFormat], NULL);
		return;
	    }
	    else if (cmd->tx_argc < 3) goto usage;
	    idx = Lookup(cmd->tx_argv[2], cmdExtToSpcFormat);
	    if (idx < 0)
	    {
		Tcl_SetResult(magicinterp, "Bad format type.  Formats are:"
			"spice2, spice3, and hspice.", NULL);
		return;
	    }
	    else
	    {
		esFormat = idx;
		/* By default, use .option to declare scale in HSPICE mode */
		if (esFormat == HSPICE) esScale = -1.0;
	    }
	    break;

	case EXTTOSPC_CTHRESH:
	    if (cmd->tx_argc == 2)
	    {
		if (LocCapThreshold == (EFCapValue)INFINITE_THRESHOLD)
		    Tcl_SetResult(magicinterp, "infinite", NULL);
		else
		    Tcl_SetObjResult(magicinterp,
			Tcl_NewDoubleObj((double)LocCapThreshold));
		return;
	    }
	    else if (cmd->tx_argc < 3) goto usage;
	
	    /* Note that strtod() (called by StrIsNumeric()) accepts	*/
	    /* "infinite" as a valid numerical value;  however, the	*/
	    /* conversion to C type INF is *not* INFINITE_THRESHOLD, so	*/
	    /* we need to check this case first. . . 			*/

	    if (!strncmp(cmd->tx_argv[2], "inf", 3))
		LocCapThreshold = (EFCapValue)INFINITE_THRESHOLD;
	    else if (StrIsNumeric(cmd->tx_argv[2]))
		LocCapThreshold = atoCap(cmd->tx_argv[2]);
	    else 
		TxError("exttospice: numeric value or \"infinite\" expected.\n");
	    break;

	case EXTTOSPC_RTHRESH:
	    if (cmd->tx_argc == 2)
	    {
		if (LocResistThreshold == INFINITE_THRESHOLD)
		    Tcl_SetResult(magicinterp, "infinite", NULL);
		else
		    Tcl_SetObjResult(magicinterp,
			Tcl_NewIntObj(LocResistThreshold));
		return;
	    }
	    else if (cmd->tx_argc < 3) goto usage;
	    if (StrIsInt(cmd->tx_argv[2]))
		LocResistThreshold = atoi(cmd->tx_argv[2]);
	    else if (!strncmp(cmd->tx_argv[2], "inf", 3))
		LocResistThreshold = INFINITE_THRESHOLD;
	    else 
		TxError("exttospice: integer value or \"infinite\" expected.\n");
	    break;

	case EXTTOSPC_MERGE:
	    if (cmd->tx_argc == 2)
	    {
		if (esMergeDevsA)
		    Tcl_SetResult(magicinterp, "aggressive", NULL);
		else if (esMergeDevsC)
		    Tcl_SetResult(magicinterp, "conservative", NULL);
		else
		    Tcl_SetResult(magicinterp, "none", NULL);
		return;
	    }
	    else if (cmd->tx_argc < 3) goto usage;
	    value = Lookup(cmd->tx_argv[2], cmdMergeTypes);
	    if (value < 0)
	    {
		TxError("Merge types are:\n");
		for (msg = &(cmdMergeTypes[0]); *msg != NULL; msg++)
		    TxPrintf("    %s\n", *msg);
	    }
	    else switch (value) {
		case 0:
		    esMergeDevsA = FALSE;
		    esMergeDevsC = FALSE;
		    break;
		case 1:
		    esMergeDevsA = FALSE;
		    esMergeDevsC = TRUE;
		    break;
		case 2:
		    esMergeDevsA = TRUE;
		    esMergeDevsC = FALSE;
		    break;
	    }
	    break;
	    
	case EXTTOSPC_DEFAULT:
	    LocCapThreshold = 2;
	    LocResistThreshold = INFINITE_THRESHOLD;
	    EFTrimFlags = 0;
	    EFScale = 0.0;
	    if (EFArgTech)
	    {
		freeMagic(EFArgTech);
		EFArgTech = NULL;
	    }
	    if (EFSearchPath)
	    {
		freeMagic(EFSearchPath);
		EFSearchPath = NULL;
	    }
	    break;

	case EXTTOSPC_RUN:
	    goto runexttospice;
	    break;

	case EXTTOSPC_HELP:
usage:
	    for (msg = &(cmdExtToSpcOption[0]); *msg != NULL; msg++)
	    {
		TxPrintf("    %s\n", *msg);
	    }
	    break;
    }
    return;

runexttospice:

    /* Reset the device indices */
    esCapNum  = 0;
    esDevNum = 1000;
    esResNum = 0;
    esDiodeNum = 0;
    esSbckNum = 0;
    esNodeNum = 10; /* just in case we're extracting spice2 */
    esFMIndex = 0;
    esDevsMerged = 0;

    EFInit();

    EFResistThreshold = LocResistThreshold;
    EFCapThreshold = LocCapThreshold;

    /* Process command line arguments */
    
    inName = EFArgs(argc, argv, &err_result, spcmainArgs, (ClientData) NULL);
    if (err_result == TRUE)
    {
	EFDone();
	return;
    }

    if (inName == NULL)
    {
	/* Assume that we want to do exttospice on the currently loaded cell */
	
	if (w == (MagWindow *) NULL)
	{
	    windCheckOnlyWindow(&w, DBWclientID);
	}

	if (w == (MagWindow *) NULL)
	{
	    TxError("Point to a window or specify a cell name.\n");
	    return;
	}
	if ((inName = ((CellUse *) w->w_surfaceID)->cu_def->cd_name) == NULL)
	{
	    TxError("No cell present\n");
	    return;
	}
    }

    /*
     * Initializations specific to this program.
     * Make output name inName.spice if they weren't 
     * explicitly specified
     */

    if (spcesOutName == spcesDefaultOut)
	sprintf(spcesDefaultOut, "%s.spice", inName);

    if ((esSpiceF = fopen(spcesOutName, "w")) == NULL)
    {
	char *tclres = Tcl_Alloc(128);
	sprintf(tclres, "exttospice: Unable to open file %s for writing\n",
		spcesOutName);
	Tcl_SetResult(magicinterp, tclres, TCL_DYNAMIC);
	EFDone();
        return;
    }

    /* Read the hierarchical description of the input circuit */
    if (EFReadFile(inName, TRUE, esDoExtResis, FALSE) == FALSE)
    {
	EFDone();
        return;
    }

    if (EFStyle == NULL)
    {
        TxError("Warning:  Current extraction style does not match .ext file!\n");
        TxError("Area/Perimeter values and parasitic values will be zero.\n");
    }

    /* create default devinfo entries (MOSIS) which can be overridden by
       the command line arguments */

    for ( i = 0 ; i < MAXDEVTYPES ; i++ ) {
	fetInfo[i].resClassSD = NO_RESCLASS;
	fetInfo[i].resClassSub = NO_RESCLASS;
	fetInfo[i].defSubs = NULL;
    }

    /* Get fetInfo information from the current extraction style 	 */
    /* (this works only for the Tcl version with the embedded exttospice */
    /* command)								 */

    idx = 0;
    while (ExtGetDevInfo(idx++, &devname, &sd_rclass, &sub_rclass, &subname))
    {
	if (idx == MAXDEVTYPES)
	{
	    TxError("Error:  Ran out of space for device types!\n");
	    break;
	}
	i = efBuildAddStr(EFDevTypes, &EFDevNumTypes, MAXDEVTYPES, devname);
	if (!strcmp(devname, "None"))
	    esNoModelType = i;
	if (EFStyle != NULL)
	{
	    fetInfo[i].resClassSD = sd_rclass;
	    fetInfo[i].resClassSub = sub_rclass;
	    fetInfo[i].defSubs = subname;
	}

	/* Tcl variable substitution for substrate node names */
	if (subname && (subname[0] == '$'))
	{
	    resstr = (char *)Tcl_GetVar(magicinterp, &subname[1],
			TCL_GLOBAL_ONLY);
	    if (resstr != NULL) fetInfo[i].defSubs = resstr;
	}
    }

    /* Keep a pointer to the "GND" variable, if it exists. */

    resstr = (char *)Tcl_GetVar(magicinterp, "GND", TCL_GLOBAL_ONLY);
    if (resstr == NULL) resstr = "GND";		/* default value */

    /* Write the output file */

    fprintf(esSpiceF, "* %s file created from %s.ext - technology: %s\n\n",
	    (esFormat == SPICE2) ? "SPICE2" :  
	      ( (esFormat == SPICE3) ? "SPICE3" : "HSPICE" ),  
	    inName, EFTech);
    if (esScale < 0) 
    	fprintf(esSpiceF,".option scale=%gu\n\n", EFScale / 100.0);
    else
	esScale = EFScale / 100.0;

    /* Convert the hierarchical description to a flat one */
    flatFlags = EF_FLATNODES;
    EFTrimFlags |= EF_TRIMGLOB;
    if (EFCapThreshold < INFINITE_THRESHOLD) flatFlags |= EF_FLATCAPS;
    if (esFormat == HSPICE ) {
	EFTrimFlags |= EF_TRIMLOCAL ;
	HashInit(&subcktNameTable, 32, HT_STRINGKEYS);
#ifndef UNSORTED_SUBCKT
	DQInit(&subcktNameQueue, 64);
#endif
    }
    locDoSubckt = FALSE;
    if (esDoHierarchy)
    {
	ESGenerateHierarchy(inName);
    }
    else
    {
	EFFlatBuild(inName, flatFlags);

	/* Determine if this is a subcircuit */
	if (esDoSubckt == AUTO) {
	    if (efFlatRootDef->def_flags & DEF_SUBCIRCUIT)
		locDoSubckt = TRUE;
	}
	if ((esDoSubckt == TRUE) || (locDoSubckt == TRUE))
	    topVisit(efFlatRootDef);

	/* When generating subcircuits, remove the subcircuit	*/
	/* flag from the top level cell.  Other than being	*/
	/* used to generate the subcircuit wrapper, it should	*/
	/* not prevent descending into its own hierarchy.	*/

	efFlatRootDef->def_flags &= ~(DEF_SUBCIRCUIT);

	/* If we don't want to write subcircuit calls, remove	*/
	/* the subcircuit flag from all cells at this time.	*/

	if (!esDoPorts)
	    EFVisitSubcircuits(subcktUndef, (ClientData) NULL);

	initMask = ( esDistrJunct  ) ? (unsigned long)0 : DEV_CONNECT_MASK;

	if (esMergeDevsA || esMergeDevsC)
	{
	    EFVisitDevs(devMergeVisit, (ClientData) NULL);
	    TxPrintf("Devs merged: %d\n", esDevsMerged);
	    esFMIndex = 0;
	    {
		devMerge *p;

		for (p = devMergeList; p != NULL; p = p->next)
		    freeMagic(p);
		devMergeList = NULL;
	    }
	}
	else if (esDistrJunct)
     	    EFVisitDevs(devDistJunctVisit, (ClientData) NULL);
	EFVisitDevs(spcdevVisit, (ClientData) NULL);
	initMask = (unsigned long) 0;
	if (flatFlags & EF_FLATCAPS)
	{
	    (void) sprintf( esCapFormat,  "C%%d %%s %%s %%.%dlffF\n", esCapAccuracy);
	    EFVisitCaps(spccapVisit, (ClientData) NULL);
	}
	EFVisitResists(spcresistVisit, (ClientData) NULL);
	EFVisitSubcircuits(subcktVisit, (ClientData) NULL);
	(void) sprintf( esCapFormat, "C%%d %%s %s %%.%dlffF%%s", resstr, esCapAccuracy);
	EFVisitNodes(spcnodeVisit, (ClientData) NULL);

	if ((esDoSubckt == TRUE) || (locDoSubckt == TRUE))
	    fprintf(esSpiceF, ".ends\n");

	if (esFormat == HSPICE) 
	    printSubcktDict();

	EFFlatDone(); 
    }
    EFDone();

    if (esSpiceF) fclose(esSpiceF);

    TxPrintf("exttospice finished.\n");
    return;
}

#else	/* MAGIC_WRAPPER */

/*
 * ----------------------------------------------------------------------------
 *
 * main --
 *
 * Top level of ext2spice.
 *
 * ----------------------------------------------------------------------------
 */

int
main(argc, argv)
    int argc;
    char *argv[];
{

    int i,flatFlags;
    char *inName;
    FILE *f;
    bool locDoSubckt;

    esDevsMerged = 0;

    EFInit();
    EFResistThreshold = INFINITE_THRESHOLD ;
    /* create default devinfo entries (MOSIS) which can be overriden by
       the command line arguments */
    for ( i = 0 ; i < MAXDEVTYPES ; i++ ) {
	fetInfo[i].resClassSD = NO_RESCLASS;
	fetInfo[i].resClassSub = NO_RESCLASS;
	fetInfo[i].defSubs = NULL;
    }
    i = efBuildAddStr(EFDevTypes, &EFDevNumTypes, MAXDEVTYPES, "ndev");
    fetInfo[i].resClassSD = 0 ;
    fetInfo[i].resClassSub = NO_RESCLASS ;
    fetInfo[i].defSubs = "Gnd!";
    i = efBuildAddStr(EFDevTypes, &EFDevNumTypes, MAXDEVTYPES, "pdev");
    fetInfo[i].resClassSD = 1 ;
    fetInfo[i].resClassSub = 8 ;
    fetInfo[i].defSubs = "Vdd!";
    i = efBuildAddStr(EFDevTypes, &EFDevNumTypes, MAXDEVTYPES, "nmos");
    fetInfo[i].resClassSD = 0 ;
    fetInfo[i].resClassSub = NO_RESCLASS ;
    fetInfo[i].defSubs = "Gnd!";
    i = efBuildAddStr(EFDevTypes, &EFDevNumTypes, MAXDEVTYPES, "pmos");
    fetInfo[i].resClassSD = 1 ;
    fetInfo[i].resClassSub = 8 ;
    fetInfo[i].defSubs = "Vdd!";
    /* Process command line arguments */

    inName = EFArgs(argc, argv, NULL, spcmainArgs, (ClientData) NULL);
    if (inName == NULL)
	exit (1);

    /*
     * Initializations specific to this program.
     * Make output name inName.spice if they weren't 
     * explicitly specified
     */

    if (spcesOutName == spcesDefaultOut)
	sprintf(spcesDefaultOut, "%s.spice", inName);

    if ((esSpiceF = fopen(spcesOutName, "w")) == NULL)
    {
	perror(spcesOutName);
	exit (1);
    }

    /* Read the hierarchical description of the input circuit */
    if (EFReadFile(inName, TRUE, esDoExtResis, FALSE) == FALSE)
    {
	exit (1);
    }

    fprintf(esSpiceF, "* %s file created from %s.ext - technology: %s\n\n",
	    (esFormat == SPICE2) ? "SPICE2" :  
	      ( (esFormat == SPICE3) ? "SPICE3" : "HSPICE" ),  
	    inName, EFTech);
    if (esScale < 0) 
    	fprintf(esSpiceF,".option scale=%gu\n\n", EFScale / 100.0);
    else
	esScale = EFScale / 100.0;

    /* Convert the hierarchical description to a flat one */
    flatFlags = EF_FLATNODES;
    EFTrimFlags |= EF_TRIMGLOB ;
    if (EFCapThreshold < INFINITE_THRESHOLD) flatFlags |= EF_FLATCAPS;
    if (esFormat == HSPICE ) {
	EFTrimFlags |= EF_TRIMLOCAL ;
	HashInit(&subcktNameTable, 32, HT_STRINGKEYS);
#ifndef UNSORTED_SUBCKT
	DQInit(&subcktNameQueue, 64);
#endif
    }
    EFFlatBuild(inName, flatFlags);

    /* Determine if this is a subcircuit */
    locDoSubckt = FALSE;
    if (esDoSubckt == AUTO) {
	if (efFlatRootDef->def_flags & DEF_SUBCIRCUIT)
	    locDoSubckt = TRUE;
    }
    if ((esDoSubckt == TRUE) || (locDoSubckt == TRUE))
	topVisit(efFlatRootDef);

    /* If we don't want to write subcircuit calls, remove the	*/
    /* subcircuit flag from all cells at this time.		*/

    if (!esDoPorts)
	EFVisitSubcircuits(subcktUndef, (ClientData) NULL);

    initMask = ( esDistrJunct  ) ? (unsigned long)0 : DEV_CONNECT_MASK ;

    if ( esMergeDevsA || esMergeDevsC ) {
     	EFVisitDevs(devMergeVisit, (ClientData) NULL);
	TxPrintf("Devs merged: %d\n", esDevsMerged);
	esFMIndex = 0 ;
	{
	  devMerge *p;

	  for ( p = devMergeList ; p != NULL ; p=p->next ) freeMagic(p);
	}
    } else if ( esDistrJunct )
     	EFVisitDevs(devDistJunctVisit, (ClientData) NULL);
    EFVisitDevs(spcdevVisit, (ClientData) NULL);
    initMask = (unsigned long) 0;
    if (flatFlags & EF_FLATCAPS) {
	(void) sprintf( esCapFormat,  "C%%d %%s %%s %%.%dlffF\n",esCapAccuracy);
	EFVisitCaps(spccapVisit, (ClientData) NULL);
    }
    EFVisitResists(spcresistVisit, (ClientData) NULL);
    EFVisitSubcircuits(subcktVisit, (ClientData) NULL);
    (void) sprintf( esCapFormat, "C%%d %%s GND %%.%dlffF%%s", esCapAccuracy);
    EFVisitNodes(spcnodeVisit, (ClientData) NULL);

    if ((esDoSubckt == TRUE) || (locDoSubckt == TRUE))
	fprintf(esSpiceF, ".ends\n");

    if ( esFormat == HSPICE ) 
	printSubcktDict();

    EFFlatDone(); 
    EFDone();

    if (esSpiceF) fclose(esSpiceF);

    TxPrintf("Memory used: %s\n", RunStats(RS_MEM, NULL, NULL));
    exit (0);
}

#endif	/* MAGIC_WRAPPER */


/*
 * ----------------------------------------------------------------------------
 *
 * spcmainArgs --
 *
 * Process those arguments that are specific to ext2spice.
 * Assumes that *pargv[0][0] is '-', indicating a flag
 * argument.
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	After processing an argument, updates *pargc and *pargv
 *	to point to after the argument.
 *
 *	May initialize various global variables based on the
 *	arguments given to us.
 *
 *	Exits in the event of an improper argument.
 *
 * ----------------------------------------------------------------------------
 */

int
spcmainArgs(pargc, pargv)
    int *pargc;
    char ***pargv;
{
    char **argv = *pargv, *cp;
    int argc = *pargc;

    switch (argv[0][1])
    {
	case 'd':
	    esDistrJunct = TRUE;
	    break;
	case 'M':
	    esMergeDevsA = TRUE;
	    break;
	case 'm':
	    esMergeDevsC = TRUE;
	    break;
	case 'B':
	    esNoAttrs = TRUE;
	    break;
	case 'F':
	    esDevNodesOnly = TRUE;
	    break;
	case 'o':
	    if ((spcesOutName = ArgStr(&argc, &argv, "filename")) == NULL)
		goto usage;
	    break;
	case 'f': {
	     char *ftmp ;

	     if ((ftmp = ArgStr(&argc, &argv, "format")) == NULL)
		goto usage;
	     if ( strcasecmp(ftmp,"SPICE2") == 0 )
	        esFormat = SPICE2 ;
	     else if ( strcasecmp(ftmp,"SPICE3") == 0 )
		esFormat = SPICE3 ;
	     else if ( strcasecmp(ftmp,"HSPICE") == 0 ) 
	     {
		esFormat = HSPICE ;
		esScale = -1.0;
	     }
	     else goto usage;
	     break;
	     }
	case 'J':
	     {
	     char *ftmp ;

	     if ((ftmp = ArgStr(&argc, &argv, "hierAP_SD")) == NULL)
		goto usage;
	     if ( strcasecmp(ftmp, "HIER") == 0 )
		esHierAP = TRUE ;
	     else if ( strcasecmp(ftmp, "FLAT") == 0 )
		esHierAP = FALSE ;
	     else goto usage;

	     break;
	     }
	case 'y': {
	      char *t;

	      if (( t =  ArgStr(&argc, &argv, "cap-accuracy") ) == NULL)
		goto usage;
	      esCapAccuracy = atoi(t);
	      break;
	      }
#ifndef MAGIC_WRAPPER
	case 'j':
	    {
	    char *rp,  subsNode[80] ;
	    int   ndx, rClass, rClassSub = -1;

	    if ((cp = ArgStr(&argc,&argv,"resistance class")) == NULL)
		goto usage;
	    if ( (rp = (char *) index(cp,':')) == NULL )
		goto usage;
	    else *rp++ = '\0';
	    if ( sscanf(rp, "%d/%d/%s", &rClass, &rClassSub, subsNode) != 3 ) {
		rClassSub = -1 ;
	    	if ( sscanf(rp, "%d/%s",  &rClass, subsNode) != 2 ) goto usage;
	    }
	    ndx = efBuildAddStr(EFDevTypes, &EFDevNumTypes, MAXDEVTYPES, cp);
	    fetInfo[ndx].resClassSD = rClass;
	    fetInfo[ndx].resClassSub = rClassSub;
	    if ( ((1<<rClass) & DEV_CONNECT_MASK) ||
	         ((1<<rClass) & DEV_CONNECT_MASK)   ) {
	        TxError("Oops it seems that you have 31\n");
	        TxError("resistance classes. You will need to recompile");
	        TxError("the extflat package and change ext2sim/spice\n");
	        TxError("DEV_CONNECT_MASK and or nodeClient\n");
		exit (1);
	    }
	    fetInfo[ndx].defSubs = (char *) mallocMagic((unsigned) (strlen(subsNode)+1));
	    strcpy(fetInfo[ndx].defSubs,subsNode);
	    TxError("info: dev %s(%d) sdRclass=%d subRclass=%d dSub=%s\n", 
	    cp, ndx, fetInfo[ndx].resClassSD, fetInfo[ndx].resClassSub, 
            fetInfo[ndx].defSubs);
	    break;
	    }
#endif			/* MAGIC_WRAPPER */
	default:
	    TxError("Unrecognized flag: %s\n", argv[0]);
	    goto usage;
    }

    *pargv = argv;
    *pargc = argc;
    return 0;

usage:
    TxError("Usage: ext2spice [-B] [-o spicefile] [-M|-m] [-y cap_digits] "
		"[-J flat|hier]\n"
		"[-f spice2|spice3|hspice] [-M] [-m] "
#ifdef MAGIC_WRAPPER
		"[file]\n"
#else
		"[-j device:sdRclass[/subRclass]/defaultSubstrate]\n"
		"file\n\n    or else see options to extcheck(1)\n"
#endif
		);

#ifdef MAGIC_WRAPPER
    return 1;
#else
    exit (1);
#endif
}

/*
 * ----------------------------------------------------------------------------
 *
 * SpiceGetNode --
 *
 * function to find a node given its hierarchical prefix and suffix
 *
 * Results:
 *  a pointer to the node struct or NULL
 *
 * ----------------------------------------------------------------------------
 */
EFNode *
SpiceGetNode(prefix, suffix)
HierName *prefix;
HierName *suffix;
{
    HashEntry *he;
    EFNodeName *nn;

    he = EFHNConcatLook(prefix, suffix, "output");
    if (he == NULL) return NULL;
    nn = (EFNodeName *) HashGetValue(he);
    if (nn == NULL) return NULL;
    return(nn->efnn_node);
}

/*
 * ----------------------------------------------------------------------------
 *
 * extHierSDAttr --
 * Check if the attribute of the argument dev_terminal or the global
 * settings are such that we want a hierarchical extraction of its S/D
 * 
 *
 * Results:
 *  TRUE or FALSE
 *
 * Side effects:
 *  None.
 *
 * ----------------------------------------------------------------------------
 */

bool extHierSDAttr(term)
    DevTerm *term;
{
    bool r = esHierAP;

    if (term->dterm_attrs)
    {
	if (Match(ATTR_HIERAP, term->dterm_attrs) != FALSE) 
	    r = TRUE;
	else if (Match(ATTR_FLATAP, term->dterm_attrs) != FALSE)
	    r = FALSE;
    }
    return r;
}

/*
 * ----------------------------------------------------------------------------
 *
 * subcktVisit --
 *
 * Procedure to output a subcircuit definition to the .spice file.
 * Called by EFVisitSubcircuits().
 *
 * Results:
 *	Returns 0 always.
 *
 * Side effects:
 *	Writes to the file esSpiceF.
 *
 * Format of a .spice subcircuit call:
 *
 *	X%d node1 node2 ... noden name
 *
 * where
 *	node1 node2 ... noden are the nodes connecting to the ports of
 *	the subcircuit.  "name" is the name of the subcircuit.  It is
 *	assumed that the definition of "name" (.defs name ... .ends)
 *	exists elsewhere and will be appended to the SPICE deck prior
 *	to simulation (as is also assumed for device models).
 *
 * ----------------------------------------------------------------------------
 */
int
subcktVisit(use, hierName, is_top)
    Use *use;
    HierName *hierName;
    bool is_top;		/* TRUE if this is the top-level cell */
{
    EFNode *snode;
    Def *def = use->use_def;
    EFNodeName *nodeName;
    int portorder, portmax;

    if (is_top == TRUE) return 0;	/* Ignore the top-level cell */

    fprintf(esSpiceF, "X%d", esSbckNum++);

    /* This is not a DEV, but "spcdevOutNode" is a general-purpose routine that */
    /* turns a local name in the use's def to a hierarchical name in the     */
    /* calling def.							     */

    /* Note that the ports of the subcircuit will not necessarily be	*/
    /* ALL the entries in the hash table, so we have to check.		*/

    portmax = -1;
    for (snode = (EFNode *) def->def_firstn.efnode_next;
		snode != &def->def_firstn;
		snode = (EFNode *) snode->efnode_next)
    {
	if (!(snode->efnode_flags & EF_PORT)) continue;
	for (nodeName = snode->efnode_name; nodeName != NULL; nodeName =
			nodeName->efnn_next)
	{
	    portorder = nodeName->efnn_port;
	    if (portorder > portmax) portmax = portorder;
	}
    }

    if (portmax < 0)
    {
	/* No port order declared; print them as we see them.	*/
	/* This shouldn't happen for proper .ext files written	*/
	/* by the magic extractor.				*/

        for (snode = (EFNode *) def->def_firstn.efnode_next;
		snode != &def->def_firstn;
		snode = (EFNode *) snode->efnode_next)
	    if (snode->efnode_flags & EF_PORT)
		for (nodeName = snode->efnode_name; nodeName != NULL;
			nodeName = nodeName->efnn_next)
		    if (nodeName->efnn_port >= 0)
			spcdevOutNode(hierName, nodeName->efnn_hier,
					"subcircuit", esSpiceF); 
    }
    else
    {
	/* Port numbers need not start at zero or be contiguous. */
	/* They will be printed in numerical order.		 */

	portorder = 0;
	while (portorder <= portmax)
	{
	    for (snode = (EFNode *) def->def_firstn.efnode_next;
			snode != &def->def_firstn;
			snode = (EFNode *) snode->efnode_next)
	    {
		if (!(snode->efnode_flags & EF_PORT)) continue;
		for (nodeName = snode->efnode_name; nodeName != NULL;
			nodeName = nodeName->efnn_next)
		{
		    int portidx = nodeName->efnn_port;
		    if (portidx == portorder)
		    {
			spcdevOutNode(hierName, nodeName->efnn_hier,
					"subcircuit", esSpiceF); 

			break;
		    }
		}
		if (nodeName != NULL) break;
	    }
	    portorder++;
	}
    }

    fprintf(esSpiceF, " %s\n", def->def_name);	/* subcircuit model name */

    return 0;
}

/*
 * ----------------------------------------------------------------------------
 *
 * subcktUndef --
 *
 * Procedure to remove the DEF_SUBCIRCUIT flag from all subcells.
 * Called by EFVisitSubcircuits().
 *
 * Results:
 *	Returns 0 always.
 *
 * Side effects:
 *	Undefines the DEF_SUBCIRCUIT flag in each encountered Def.
 *
 * ----------------------------------------------------------------------------
 */
int
subcktUndef(use, hierName, is_top)
    Use *use;
    HierName *hierName;
    bool is_top;	/* TRUE if this is the top-level cell */
{
    Def *def = use->use_def;

    def->def_flags &= ~(DEF_SUBCIRCUIT);
    return 0;
}

/*
 * ----------------------------------------------------------------------------
 *
 * topVisit --
 *
 * Procedure to output a subcircuit definition to the .spice file.
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	Writes to the file esSpiceF.
 *
 * Format of a .spice subcircuit definition:
 *
 *	.subckt name node1 node2 ... noden
 *
 * where
 *	node1 node2 ... noden are the nodes connecting to the ports of
 *	the subcircuit.  "name" is the name of the cell def.
 *
 * ----------------------------------------------------------------------------
 */
 
void
topVisit(def)
    Def *def;
{
    EFNode *snode;
    EFNodeName *sname, *nodeName;
    HashSearch hs;
    HashEntry *he;
    int portorder, portmax;

    fprintf(esSpiceF, ".subckt %s", def->def_name);

    /* Note that the ports of the subcircuit will not necessarily be	*/
    /* ALL the entries in the hash table, so we have to check.		*/

    HashStartSearch(&hs);
    portmax = -1;

    while (he = HashNext(&def->def_nodes, &hs))
    {
	sname = (EFNodeName *) HashGetValue(he);
	if (sname == NULL) continue;
	snode = sname->efnn_node;
	if (!(snode->efnode_flags & EF_PORT)) continue;
	for (nodeName = sname; nodeName != NULL; nodeName = nodeName->efnn_next)
	{
	    portorder = nodeName->efnn_port;
	    if (portorder > portmax) portmax = portorder;
	}
    }
    
    if (portmax < 0)
    {
	/* No port order declared; print them and number them	*/
	/* as we encounter them.  This normally happens only	*/
	/* when writing hierarchical decks for LVS.		*/

	portorder = 0;

	HashStartSearch(&hs);
	while (he = HashNext(&def->def_nodes, &hs))
	{
	    sname = (EFNodeName *) HashGetValue(he);
	    if (sname == NULL) continue;
	    snode = sname->efnn_node;

	    if (snode->efnode_flags & EF_PORT)
		if (snode->efnode_name->efnn_port < 0)
		{
		    // fprintf(esSpiceF, " %s", he->h_key.h_name);
		    // sname->efnn_port = portorder++;
		    fprintf(esSpiceF, " %s",
				nodeSpiceName(snode->efnode_name->efnn_hier));
		    snode->efnode_name->efnn_port = portorder++;
		}
	}
    }
    else
    {
	/* Port numbers need not start at zero or be contiguous. */
	/* They will be printed in numerical order.              */

	portorder = 0;
	while (portorder <= portmax)
	{
	    HashStartSearch(&hs);
	    while (he = HashNext(&def->def_nodes, &hs))
	    {
		int portidx;

		sname = (EFNodeName *) HashGetValue(he);
		snode = sname->efnn_node;

		if (!(snode->efnode_flags & EF_PORT)) continue;
		for (nodeName = sname; nodeName != NULL; nodeName = nodeName->efnn_next)
		{
		    portidx = nodeName->efnn_port;
		    if (portidx == portorder)
		    {
			// fprintf(esSpiceF, " %s", he->h_key.h_name);
			fprintf(esSpiceF, " %s",
				nodeSpiceName(snode->efnode_name->efnn_hier));
			break;
		    }
		    else if (portidx < 0)
			nodeName->efnn_port = ++portmax;
		}
		if (nodeName != NULL) break;
	    }
	    portorder++;
	}
    }
    fprintf(esSpiceF, "\n");
}

/*
 * ----------------------------------------------------------------------------
 *
 * esOutputResistor ---
 *
 * Routine used by spcdevVisit to print a resistor device.  This
 * is broken out into a separate routine so that each resistor
 * device may be represented (if the option is selected) by a
 * "tee" network of two resistors on either side of the central
 * node, which then has a capacitance to ground.
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	Output to the SPICE deck.
 *
 * ----------------------------------------------------------------------------
 */

void
esOutputResistor(dev, hierName, trans, term1, term2, has_model, l, w, dscale)
    Dev *dev;			/* Dev being output */
    HierName *hierName;		/* Hierarchical path down to this dev */
    Transform *trans;		/* Coordinate transform for output */
    DevTerm *term1, *term2;	/* Terminals of the device */
    bool has_model;		/* Is this a modeled resistor? */
    int l, w;			/* Device length and width */
    int dscale;			/* Device scaling (for split resistors) */
{
    int scale;
    Rect r;
    float sdM ; 
    char name[12], devchar;

    /* Resistor is "Rnnn term1 term2 value" 		 */
    /* extraction sets two terminals, which are assigned */
    /* term1=gate term2=source by the above code.	 */
    /* extracted units are Ohms; output is in Ohms 	 */

    spcdevOutNode(hierName, term1->dterm_node->efnode_name->efnn_hier,
			name, esSpiceF);
    spcdevOutNode(hierName, term2->dterm_node->efnode_name->efnn_hier,
			name, esSpiceF);

    sdM = getCurDevMult();

    /* SPICE has two resistor types.  If the "name" (EFDevTypes) is */
    /* "None", the simple resistor type is used, and a value given. */
    /* If not, the "semiconductor resistor" is used, and L and W    */
    /* and the device name are output.				    */

    if (!has_model)
    {
	fprintf(esSpiceF, " %f", ((double)(dev->dev_res)
			/ (double)(dscale)) / (double)sdM);
    }
    else
    {
	fprintf(esSpiceF, " %s", EFDevTypes[dev->dev_type]);

	GeoTransRect(trans, &dev->dev_rect, &r);
	scale = GeoScale(trans);

	if (esScale < 0) 
	{
	    fprintf(esSpiceF, " w=%d l=%d", w * scale, (l * scale) / dscale);
	    if (sdM != 1.0)
		fprintf(esSpiceF, " M=%g", sdM);
	}
	else
	{
	    fprintf(esSpiceF, " w=%gu l=%gu",
		(float)w * scale * esScale * sdM,
		(float)((l * scale * esScale) / dscale));
	}
    }
}

/*
 * ----------------------------------------------------------------------------
 *
 * spcdevVisit --
 *
 * Procedure to output a single dev to the .spice file.
 * Called by EFVisitDevs().
 *
 * Results:
 *	Returns 0 always.
 *
 * Side effects:
 *	Writes to the file esSpiceF.
 *
 * Format of a .spice dev line:
 *
 *	M%d drain gate source substrate type w=w l=l * x y
 *      + ad= pd= as= ps=  * asub= psub=  
 *      **devattr g= s= d= 
 *
 * where
 *	type is a name identifying this type of transistor
 *      other types of transistors are extracted with
 *      an M card but it should be easy to turn them to whatever 
 *      you want.
 *	gate, source, and drain are the nodes to which these three
 *		terminals connect
 *	l, w are the length and width of the channel
 *	x, y are the x, y coordinates of a point within the channel.
 *	g=, s=, d= are the (optional) attributes; if present, each
 *		is followed by a comma-separated list of attributes.
 *
 * ----------------------------------------------------------------------------
 */

int
spcdevVisit(dev, hierName, trans)
    Dev *dev;		/* Dev being output */
    HierName *hierName;	/* Hierarchical path down to this dev */
    Transform *trans;	/* Coordinate transform for output */
{
    DevParam *plist, *pptr;
    DevTerm *gate, *source, *drain;
    EFNode  *subnode, *snode, *dnode, *subnodeFlat = NULL;
    int scale, l, w, i;
    Rect r;
    bool subAP= FALSE, hierS, hierD, extHierSDAttr() ;
    float sdM; 
    char name[12], devchar;
    bool has_model = TRUE;

    sprintf(name, "output");

    /* If no terminals, can't do much of anything */
    if (dev->dev_nterm < 1 ) 
	return 0;

    if ( (esMergeDevsA || esMergeDevsC) && devIsKilled(esFMIndex++) )
	    return 0;

    /* Get L and W of device */
    EFGetLengthAndWidth(dev, &l, &w);

    /* If only two terminals, connect the source to the drain */
    gate = &dev->dev_terms[0];
    if (dev->dev_nterm >= 2)
	source = drain = &dev->dev_terms[1];
    if (dev->dev_nterm >= 3)
	drain = &dev->dev_terms[2];
    subnode = dev->dev_subsnode;

    /* Original hack for BiCMOS, Tim 10/4/97, is deprecated.	*/
    /* Use of "device bjt" preferred to "fet" with model="npn".	*/

    if (!strcmp(EFDevTypes[dev->dev_type], "npn")) dev->dev_class = DEV_BJT;

    /* For resistor and capacitor classes, set a boolean to	*/
    /* denote whether the device has a model or not, so we	*/
    /* don't have to keep doing a string compare on EFDevTypes.	*/

    switch(dev->dev_class)
    {
	case DEV_RES:
	case DEV_CAP:
	    if (dev->dev_type == esNoModelType)
		has_model = FALSE;
	    break;
    }

    /* Flag shorted devices---this should probably be an option */
    switch(dev->dev_class)
    {
	case DEV_MOSFET:
	case DEV_ASYMMETRIC:
	case DEV_FET:
	    if (source == drain)
		fprintf(esSpiceF, "** SOURCE/DRAIN TIED\n");
	    break;

	default:
	    if (gate == source)
		fprintf(esSpiceF, "** SHORTED DEVICE\n");
	    break;
    }

    /* Generate SPICE device name */
    switch(dev->dev_class)
    {
	case DEV_MOSFET:
	case DEV_ASYMMETRIC:
	case DEV_FET:
	    devchar = 'M';
	    break;
	case DEV_BJT:
	    devchar = 'Q';
	    break;
	case DEV_DIODE:
	    devchar = 'D';
	    break;
	case DEV_RES:
	    devchar = 'R';
	    break;
	case DEV_CAP:
	    devchar = 'C';
	    break;
	case DEV_SUBCKT:
	case DEV_RSUBCKT:
	    devchar = 'X';
	    break;
    }
    fprintf(esSpiceF, "%c", devchar);

    /* Device index is taken from gate attributes if attached;	*/
    /* otherwise, the device is numbered in sequence.		*/

    if (gate->dterm_attrs)
	fprintf(esSpiceF, "%s", gate->dterm_attrs);
    else
    {
	switch (dev->dev_class)
	{
	    case DEV_RES:
		fprintf(esSpiceF, "%d", esResNum++);
		/* For resistor tee networks, use, e.g.,	*/
		/* "R1A" and "R1B", for clarity			*/
		if (esDoResistorTee) fprintf(esSpiceF, "A");
		break;
	    case DEV_DIODE:
		fprintf(esSpiceF, "%d", esDiodeNum++);
		break;
	    case DEV_CAP:
		fprintf(esSpiceF, "%d", esCapNum++);
		break;
	    case DEV_SUBCKT:
	    case DEV_RSUBCKT:
		fprintf(esSpiceF, "%d", esSbckNum++);
		break;
	    default:
		fprintf(esSpiceF, "%d", esDevNum++);
		break;
	}
    }
    /* Order and number of nodes in the output depends on the device class */

    switch (dev->dev_class)
    {
	case DEV_BJT:

	    /* BJT is "Qnnn collector emitter base model" 			*/
	    /* extraction sets collector=subnode, emitter=gate, base=drain	*/

	    sprintf(name, "fet");
	    spcdevOutNode(hierName, subnode->efnode_name->efnn_hier, name, esSpiceF); 
	    spcdevOutNode(hierName, gate->dterm_node->efnode_name->efnn_hier,
			name, esSpiceF);

	    /* fix mixed up drain/source for bjts hace 2/2/99 */
	    if (gate->dterm_node->efnode_name->efnn_hier ==
			source->dterm_node->efnode_name->efnn_hier)
		spcdevOutNode(hierName, drain->dterm_node->efnode_name->efnn_hier,
			name, esSpiceF);
	    else
		spcdevOutNode(hierName, source->dterm_node->efnode_name->efnn_hier,
			name, esSpiceF);

	    fprintf(esSpiceF, " %s", EFDevTypes[dev->dev_type]);
	    break;

	case DEV_SUBCKT:

	    /* Subcircuit is "Xnnn gate [source [drain [sub]]]"		*/
	    /* Subcircuit .subckt record must be ordered to match!	*/

	    spcdevOutNode(hierName, gate->dterm_node->efnode_name->efnn_hier,
			name, esSpiceF);

	    /* Drop through to below (no break statement) */

	case DEV_RSUBCKT:
	    /* RC-like subcircuits are exactly like other subcircuits	*/
	    /* except that the "gate" node is treated as an identifier	*/
	    /* only and is not output.					*/

	    if (dev->dev_nterm > 1)
		spcdevOutNode(hierName, source->dterm_node->efnode_name->efnn_hier,
			name, esSpiceF);
	    if (dev->dev_nterm > 2)
		spcdevOutNode(hierName, drain->dterm_node->efnode_name->efnn_hier,
			name, esSpiceF);

	    /* The following only applies to DEV_SUBCKT*, which may define as	*/
	    /* many terminal types as it wants.					*/

	    for (i = 4; i < dev->dev_nterm; i++)
	    {
		drain = &dev->dev_terms[i - 1];
		spcdevOutNode(hierName, drain->dterm_node->efnode_name->efnn_hier,
			name, esSpiceF);
	    }

	    /* Get the device parameters now, and check if the substrate is	*/
	    /* passed as a parameter rather than as a node.			*/

	    plist = efGetDeviceParams(EFDevTypes[dev->dev_type]);
	    for (pptr = plist; pptr != NULL; pptr = pptr->parm_next)
		if (pptr->parm_type == 's')
		    break;

	    if ((pptr == NULL) && subnode)
	    {
		fprintf(esSpiceF, " ");
		subnodeFlat = spcdevSubstrate(hierName,
			subnode->efnode_name->efnn_hier,
			dev->dev_type, esSpiceF);
	    }
	    fprintf(esSpiceF, " %s", EFDevTypes[dev->dev_type]);

	    /* Write all requested parameters to the subcircuit call.	*/

	    GeoTransRect(trans, &dev->dev_rect, &r);
	    scale = GeoScale(trans);

	    sdM = getCurDevMult();
	    while (plist != NULL)
	    {
		fprintf(esSpiceF, " %s=", plist->parm_name);
		switch (plist->parm_type)
		{
		    case 'a':
			if (esScale < 0)
			    fprintf(esSpiceF, "%d", dev->dev_area * scale * scale);
			else
			    fprintf(esSpiceF, "%gu", dev->dev_area * scale * scale
					* esScale * esScale * sdM);
			break;
		    case 'p':
			if (esScale < 0)
			    fprintf(esSpiceF, "%d", dev->dev_perim * scale);
			else
			    fprintf(esSpiceF, "%gu", dev->dev_perim * scale
					* esScale * sdM);
			break;
		    case 'l':
			if (esScale < 0)
			    fprintf(esSpiceF, "%d", l * scale);
			else
			    fprintf(esSpiceF, "%gu", l * scale * esScale);
			break;
		    case 'w':
			if (esScale < 0)
			    fprintf(esSpiceF, "%d", w * scale);
			else
			    fprintf(esSpiceF, "%gu", w * scale * esScale * sdM);
			break;
		    case 's':
			subnodeFlat = spcdevSubstrate(hierName,
				subnode->efnode_name->efnn_hier,
				dev->dev_type, esSpiceF);
			break;
		    case 'x':
			if (esScale < 0)
			    fprintf(esSpiceF, "%d", dev->dev_rect.r_xbot * scale);
			else
			    fprintf(esSpiceF, "%gu", dev->dev_rect.r_xbot * scale
					* esScale);
			break;
		    case 'y':
			if (esScale < 0)
			    fprintf(esSpiceF, "%d", dev->dev_rect.r_ybot * scale);
			else
			    fprintf(esSpiceF, "%gu", dev->dev_rect.r_ybot * scale
					* esScale);
			break;
		    case 'r':
			fprintf(esSpiceF, "%f", (double)(dev->dev_res) /
					(double)sdM);
			break;
		    case 'c':
			fprintf(esSpiceF, "%ff", (double)sdM *
					(double)(dev->dev_cap));
			break;
		}
		plist = plist->parm_next;
	    }
	    if ((esScale < 0) && (sdM != 1.0))
		fprintf(esSpiceF, " M=%g", sdM);
	    break;

	case DEV_RES:
	    if (esDoResistorTee)
	    {
		/* There are three ways of handling capacitance	*/
		/* on resistor networks.  One is to ignore it	*/
		/* (the default; generates "floating" nodes in	*/
		/* the SPICE output) which is okay for LVS. 	*/
		/* Another way is the Pi network, in which the	*/
		/* capacitance is split evenly between the	*/
		/* terminals.  Again, the resistor node is left	*/
		/* floating.  The third is the Tee network, in	*/
		/* which the resistance is split in two parts,	*/
		/* connecting to a capacitor to ground in the	*/
		/* middle.  This is the best solution but plays	*/
		/* havoc with LVS.  So, the choice is a command	*/
		/* line option.					*/

		esOutputResistor(dev, hierName, trans, gate, source, has_model,
			l, w, 2);
		fprintf(esSpiceF, "\n%c", devchar);
		if (gate->dterm_attrs)
		    fprintf(esSpiceF, "%sB", gate->dterm_attrs);
		else
		    fprintf(esSpiceF, "%dB", esResNum - 1);
		esOutputResistor(dev, hierName, trans, gate, drain, has_model,
			l, w, 2);
	    }
	    else
	    {
		esOutputResistor(dev, hierName, trans, source, drain, has_model,
			l, w, 1);
	    }
	    break;

	case DEV_DIODE:

	    /* Diode is "Dnnn top bottom model"	*/

	    spcdevOutNode(hierName, gate->dterm_node->efnode_name->efnn_hier,
			name, esSpiceF);
	    spcdevOutNode(hierName, source->dterm_node->efnode_name->efnn_hier,
			name, esSpiceF);
	    fprintf(esSpiceF, " %s", EFDevTypes[dev->dev_type]);
	    break;

	case DEV_CAP:

	    /* Capacitor is "Cnnn top bottom value"	*/
	    /* extraction sets top=gate bottom=source	*/
	    /* extracted units are fF; output is in fF */

	    spcdevOutNode(hierName, gate->dterm_node->efnode_name->efnn_hier,
			name, esSpiceF);
	    spcdevOutNode(hierName, source->dterm_node->efnode_name->efnn_hier,
			name, esSpiceF);

	    sdM = getCurDevMult();

	    /* SPICE has two capacitor types.  If the "name" (EFDevTypes) is */
	    /* "None", the simple capacitor type is used, and a value given. */
	    /* If not, the "semiconductor capacitor" is used, and L and W    */
	    /* and the device name are output.				     */

	    if (!has_model)
	    {
		fprintf(esSpiceF, " %ffF", (double)sdM *
				(double)(dev->dev_cap));
	    }
	    else
	    {
		fprintf(esSpiceF, " %s", EFDevTypes[dev->dev_type]);

		GeoTransRect(trans, &dev->dev_rect, &r);
		scale = GeoScale(trans);

		if (esScale < 0)
		{
		    fprintf(esSpiceF, " w=%d l=%d", w*scale, l*scale);
		    if (sdM != 1.0)
			fprintf(esSpiceF, " M=%g", sdM);
		}
		else
		{
		    fprintf(esSpiceF, " w=%gu l=%gu",
			(float)w * scale * esScale * sdM,
			(float)l * scale * esScale);
		    sdM = 1.0;
		}
	    }
	    break;

	case DEV_FET:
	case DEV_MOSFET:
	case DEV_ASYMMETRIC:

	    /* MOSFET is "Mnnn drain gate source [L=x W=x [attributes]]" */

	    spcdevOutNode(hierName, drain->dterm_node->efnode_name->efnn_hier,
			name, esSpiceF);
	    spcdevOutNode(hierName, gate->dterm_node->efnode_name->efnn_hier,
			name, esSpiceF);
	    spcdevOutNode(hierName, source->dterm_node->efnode_name->efnn_hier,
			name, esSpiceF);
	    if (subnode)
	    {
		fprintf(esSpiceF, " ");
		subnodeFlat = spcdevSubstrate(hierName,
			subnode->efnode_name->efnn_hier,
			dev->dev_type, esSpiceF);
	    }
	    fprintf(esSpiceF, " %s", EFDevTypes[dev->dev_type]);

	    /*
	     * Scale L and W appropriately by the same amount as distance
	     * values in the transform.  The transform will have a scale
	     * different from 1 only in the case when the scale factors of
	     * some of the .ext files differed, making it necessary to scale
	     * all dimensions explicitly instead of having a single scale
	     * factor at the beginning of the .spice file.
	     */
	    GeoTransRect(trans, &dev->dev_rect, &r);
	    scale = GeoScale(trans);

	    sdM = getCurDevMult();

	    if (esScale < 0)
	    {
		fprintf(esSpiceF, " w=%d l=%d", w*scale, l*scale);
		if (sdM != 1.0)
		    fprintf(esSpiceF, " M=%g", sdM);
	    }
	    else
	    {
		fprintf(esSpiceF, " w=%gu l=%gu",
			(float)w * scale * esScale * sdM,
			(float)l * scale * esScale);
		sdM = 1.0;
	    }

	    /*
	     * Check controlling attributes and output area and perimeter. 
	     */
	    hierS = extHierSDAttr(source);
	    hierD = extHierSDAttr(drain);
	    if ( gate->dterm_attrs ) 
		subAP = Match(ATTR_SUBSAP, gate->dterm_attrs ) ;

	    fprintf(esSpiceF, "\n+ ");
	    if (hierD) 
        	spcnAPHier(drain, hierName, fetInfo[dev->dev_type].resClassSD,
			scale, "d", sdM, esSpiceF);
	    else
	    {
		dnode = SpiceGetNode(hierName, drain->dterm_node->efnode_name->efnn_hier);
        	spcnAP(dnode, fetInfo[dev->dev_type].resClassSD, scale,
			"d", sdM, esSpiceF, w);
	    }
	    if (hierS)
		spcnAPHier(source, hierName, fetInfo[dev->dev_type].resClassSD,
			scale, "s", sdM, esSpiceF);
	    else {
		snode= SpiceGetNode(hierName, source->dterm_node->efnode_name->efnn_hier);
		spcnAP(snode, fetInfo[dev->dev_type].resClassSD, scale,
			"s", sdM, esSpiceF, w);
	    }
	    if (subAP)
	    {
		fprintf(esSpiceF, " * ");
		if (fetInfo[dev->dev_type].resClassSub < 0)
		{
		    TxError("error: subap for devtype %d unspecified\n",
				dev->dev_type);
		    fprintf(esSpiceF, "asub=0 psub=0");
		}
		else if (subnodeFlat) 
		    spcnAP(subnodeFlat, fetInfo[dev->dev_type].resClassSub, scale, 
	       			"sub", sdM, esSpiceF, -1);
		else
		    fprintf(esSpiceF, "asub=0 psub=0");
	    }

	    /* Now output attributes, if present */
	    if (!esNoAttrs)
	    {
		if (gate->dterm_attrs || source->dterm_attrs || drain->dterm_attrs)
		    fprintf(esSpiceF,"\n**devattr");
		if (gate->dterm_attrs)
		    fprintf(esSpiceF, " g=%s", gate->dterm_attrs);
		if (source->dterm_attrs) 
		    fprintf(esSpiceF, " s=%s", source->dterm_attrs);
		if (drain->dterm_attrs) 
		    fprintf(esSpiceF, " d=%s", drain->dterm_attrs);
	    }
	    break;
    }
    fprintf(esSpiceF, "\n");
    return 0;
}

/*
 * ----------------------------------------------------------------------------
 *
 * spcdevSubstrate -
 *
 * Output the node name of the substrate of a dev. If the suffix is the 
 * same as the default dont go looking for it just output the default
 * (trimmed appropriately). Otherwise look it up ..
 *
 * Results:
 *   NULL if not found or the default substrate or the node pointer
 *   otherwise (might be reused to output area and perimeter of
 *   the substrate).
 *
 * Side effects:
 *  Might allocate the nodeClient for the node through nodeSpiceName.
 *
 * ----------------------------------------------------------------------------
 */
EFNode *spcdevSubstrate( prefix, suffix, type,  outf)
HierName *prefix, *suffix;
int type;
FILE *outf;
{
    HashEntry *he;
    EFNodeName *nn;
    char *suf ;
    int  l ;

    suf = EFHNToStr(suffix);
    if (fetInfo[type].defSubs && strcasecmp(suf,fetInfo[type].defSubs) == 0) {
	if ( outf ) {
    	   l = strlen(suf) - 1;
	   if (  ( EFTrimFlags & EF_TRIMGLOB ) && suf[l] =='!' ||
	         ( EFTrimFlags & EF_TRIMLOCAL ) && suf[l] == '#'  )
	        suf[l] = '\0' ;
	   fprintf(outf, "%s", suf);
	}
	return NULL;
    }
    else {
    	he = EFHNConcatLook(prefix, suffix, "substrate");
    	if (he == NULL)
    	{
		if (outf) 
		   fprintf(outf, "errGnd!");
		return NULL;
    	}
    	/* Canonical name */
    	nn = (EFNodeName *) HashGetValue(he);
	if (outf) 
	   fprintf(outf, nodeSpiceName(nn->efnn_node->efnode_name->efnn_hier));
        return nn->efnn_node;
   }
}


/*
 * ----------------------------------------------------------------------------
 *
 * spcnAP, spcnAPHier --
 *
 * Output the area perimeter of the node with type type if it has not 
 * been visited.
 * The spcnAPHier version outputs the area and perimeter only within the
 * local subcell with hierarchical name hierName.
 *
 * Side effects:
 *     Set the visited flags so that the node A/P will not be output multiple
 *     times
 *
 * ----------------------------------------------------------------------------
 */
int spcnAP(node, resClass, scale, sterm, m, outf, w)
    EFNode *node;
    int  resClass, scale;
    char *sterm;
    float m;
    FILE *outf;
    int w;
{
	char fmt[30];
	float dsc ;

	if ((node == NULL) || (node->efnode_client == (ClientData)NULL))
	{
	    TxError("spcnAP: major internal inconsistency\n");
	    return;
	}

	if ( esScale < 0 )
	     sprintf(fmt,"a%s=%%g p%s=%%g ", sterm, sterm);
	else sprintf(fmt,"a%s=%%gp p%s=%%gu ", sterm, sterm);

	if ( !esDistrJunct || w == -1 ) goto oldFmt;

	dsc = w/((nodeClient*)node->efnode_client)->m_w.widths[resClass];
	if ( esScale < 0 )
	   fprintf(outf,fmt,
		node->efnode_pa[resClass].pa_area*scale*scale*dsc,
		node->efnode_pa[resClass].pa_perim*scale*dsc);
	else 
	   fprintf(outf,fmt,
	    ((float)node->efnode_pa[resClass].pa_area*scale*scale)
	    *esScale*esScale*dsc,
	    ((float)node->efnode_pa[resClass].pa_perim*scale)*esScale*dsc);
	return;

oldFmt:
	if ( resClass == NO_RESCLASS ||
	     beenVisited((nodeClient *)node->efnode_client, resClass) ) 
		scale = 0 ;
	else   markVisited((nodeClient *)node->efnode_client, resClass);
	if ( esScale < 0 )
	   fprintf(outf,fmt,
		node->efnode_pa[resClass].pa_area*scale*scale/m,
		node->efnode_pa[resClass].pa_perim*scale/m);
	else 
	   fprintf(outf,fmt,
	    ((float)node->efnode_pa[resClass].pa_area*scale*scale)*esScale*esScale,
	    ((float)node->efnode_pa[resClass].pa_perim*scale)*esScale);
}

int spcnAPHier(dterm, hierName, resClass, scale, sterm, m, outf)
    DevTerm *dterm;
    HierName *hierName;
    int  resClass, scale;
    float m;
    char *sterm;
    FILE *outf;
{
	EFNode *node = dterm->dterm_node;
	nodeClientHier   *nc ;
	char fmt[30];

	if ( esScale < 0 )
	     sprintf(fmt,"a%s=%%g p%s=%%g ", sterm, sterm);
	else sprintf(fmt,"a%s=%%gp p%s=%%gu ", sterm, sterm);
	if ( node->efnode_client == (ClientData) NULL ) 
	if ( node->efnode_client == (ClientData) NULL ) 
		initNodeClientHier(node);
	nc = (nodeClientHier *)node->efnode_client;
	if ( nc->lastPrefix != hierName ) {
		clearVisited(nc);
		nc->lastPrefix = hierName;
	}
	if (resClass == NO_RESCLASS ||
	    beenVisited((nodeClientHier *)node->efnode_client, resClass) ) 
		scale = 0;
	else markVisited((nodeClientHier *)node->efnode_client, resClass);
	if ( esScale < HSPICE )
	   fprintf(outf,fmt,
		node->efnode_pa[resClass].pa_area*scale*scale/m,
		node->efnode_pa[resClass].pa_perim*scale/m);
	else
	   fprintf(outf,fmt,
	    ((float)node->efnode_pa[resClass].pa_area*scale)*esScale*esScale,
	    ((float)node->efnode_pa[resClass].pa_perim*scale)*esScale);
}

/*
 * ----------------------------------------------------------------------------
 *
 * spcdevOutNode --
 *
 * Output the name of the node whose hierarchical prefix down to this
 * point is 'prefix' and whose name from the end of suffix down to the
 * leaves is 'suffix', just as in the arguments to EFHNConcat().
 *
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	Writes to the file 'outf'.
 *	Sets the efnode_client field as described above.
 *
 * ----------------------------------------------------------------------------
 */

int
spcdevOutNode(prefix, suffix, name, outf)
    HierName *prefix;
    HierName *suffix;
    char *name;
    FILE *outf;
{
    HashEntry *he;
    EFNodeName *nn;

    he = EFHNConcatLook(prefix, suffix, name);
    if (he == NULL)
    {
	fprintf(outf, " errGnd!");
	return;
    }
    nn = (EFNodeName *) HashGetValue(he);
    fprintf(outf, " %s", nodeSpiceName(nn->efnn_node->efnode_name->efnn_hier));
}


/*
 * ----------------------------------------------------------------------------
 *
 * spccapVisit --
 *
 * Procedure to output a single capacitor to the .spice file.
 * Called by EFVisitCaps().
 *
 * Results:
 *	Returns 0 always.
 *
 * Side effects:
 *	Writes to the file esSpiceF. Increments esCapNum.
 *
 * Format of a .spice cap line:
 *
 *	C%d node1 node2 cap
 *
 * where
 *	node1, node2 are the terminals of the capacitor
 *	cap is the capacitance in femtofarads (NOT attofarads).
 *
 * ----------------------------------------------------------------------------
 */

int
spccapVisit(hierName1, hierName2, cap)
    HierName *hierName1;
    HierName *hierName2;
    double cap;
{
    cap = cap / 1000;
    if (cap <= EFCapThreshold)
	return 0;

    fprintf(esSpiceF, esCapFormat ,esCapNum++,nodeSpiceName(hierName1),
                                          nodeSpiceName(hierName2), cap);
    return 0;
}

/*
 * ----------------------------------------------------------------------------
 *
 * spcresistVisit --
 *
 * Procedure to output a single resistor to the .spice file.
 * Called by EFVisitResists().
 *
 * Results:
 *	Returns 0 always.
 *
 * Side effects:
 *	Writes to the file esSpiceF. Increments esResNum.
 *
 * Format of a .spice resistor line:
 *
 *	R%d node1 node2 res
 *
 * where
 *	node1, node2 are the terminals of the resistor
 *	res is the resistance in ohms (NOT milliohms)
 *
 *
 * ----------------------------------------------------------------------------
 */
int
spcresistVisit(hierName1, hierName2, res)
    HierName *hierName1;
    HierName *hierName2;
    int res;
{
    res = (res + 500) / 1000;

    fprintf(esSpiceF, "R%d %s %s %d\n", esResNum++, nodeSpiceName(hierName1),
                                          nodeSpiceName(hierName2), res);

    return 0;
}

/*
 * ----------------------------------------------------------------------------
 *
 * spcnodeVisit --
 *
 * Procedure to output a single node to the .spice file along with its 
 * attributes and its dictionary (if present). Called by EFVisitNodes().
 *
 * Results:
 *	Returns 0 always.
 *
 * Side effects:
 *	Writes to the files esSpiceF
 *
 * ----------------------------------------------------------------------------
 */

int
spcnodeVisit(node, res, cap)
    EFNode *node;
    int res; 
    double cap;
{
    EFNodeName *nn;
    HierName *hierName;
    bool isConnected = FALSE;
    char *fmt, *nsn;
    EFAttr *ap;

    if (node->efnode_client)
    {
	isConnected = (esDistrJunct) ?
		(((nodeClient *)node->efnode_client)->m_w.widths != NULL) :
        	((((nodeClient *)node->efnode_client)->m_w.visitMask
		& DEV_CONNECT_MASK) != 0);
    }
    if (!isConnected && esDevNodesOnly) 
	return 0;

    /* Don't mark known ports as "FLOATING" nodes */
    if (!isConnected && node->efnode_flags & EF_PORT) isConnected = TRUE;

    hierName = (HierName *) node->efnode_name->efnn_hier;
    nsn = nodeSpiceName(hierName);

    if (esFormat == SPICE2 || esFormat == HSPICE && strncmp(nsn, "z@", 2)==0 ) {
	static char ntmp[MAX_STR_SIZE];

	EFHNSprintf(ntmp, hierName);
	fprintf(esSpiceF, "** %s == %s\n", ntmp, nsn);
    }
    cap = cap  / 1000;
    if (cap > EFCapThreshold)
    {
	fprintf(esSpiceF, esCapFormat, esCapNum++, nsn, cap,
			  (isConnected) ?  "\n" : " **FLOATING\n");
    }
    if (node->efnode_attrs && !esNoAttrs)
    {
	fprintf(esSpiceF, "**nodeattr %s :",nsn );
	for (fmt = " %s", ap = node->efnode_attrs; ap; ap = ap->efa_next)
	{
	    fprintf(esSpiceF, fmt, ap->efa_text);
	    fmt = ",%s";
	}
	putc('\n', esSpiceF);
    }

    return 0;
}

/* a debugging procedure */
int
nodeVisitDebug(node, res, cap)
    EFNode *node;
    int res; 
    double cap;
{
    EFNodeName *nn;
    HierName *hierName;
    char *fmt, *nsn;
    EFAttr *ap;
    
    hierName = (HierName *) node->efnode_name->efnn_hier;
    nsn = nodeSpiceName(hierName);
    TxError("** %s (%x)\n", nsn, node);

    printf("\t client.name=%s, client.m_w=%x\n", 
    ((nodeClient *)node->efnode_client)->spiceNodeName,
    ((nodeClient *)node->efnode_client)->m_w.widths);
   return 0;
}

/*
 * ----------------------------------------------------------------------------
 *
 * nodeSpiceName --
 * Find the real spice name for the node with hierarchical name hname.
 *   SPICE2 ==> numeric
 *   SPICE3 ==> full magic path
 *   HSPICE ==> less than 15 characters long
 *
 * Results:
 *	Returns the spice node name.
 *
 * Side effects:
 *      Allocates nodeClients for the node.
 *
 * ----------------------------------------------------------------------------
 */
static char esTempName[MAX_STR_SIZE];

char *nodeSpiceName(hname)
    HierName *hname;
{
    EFNodeName *nn;
    HashEntry *he;
    EFNode *node;

    he = EFHNLook(hname, (char *) NULL, "nodeName");
    if ( he == NULL )
	return "errGnd!";
    nn = (EFNodeName *) HashGetValue(he);
    node = nn->efnn_node;

    if ( (nodeClient *) (node->efnode_client) == NULL ) {
    	initNodeClient(node);
	goto makeName;
    } else if ( ((nodeClient *) (node->efnode_client))->spiceNodeName == NULL)
	goto makeName;
    else goto retName;


makeName:
    if ( esFormat == SPICE2 ) 
	sprintf(esTempName, "%d", esNodeNum++);
    else {
       EFHNSprintf(esTempName, node->efnode_name->efnn_hier);
       if ( esFormat == HSPICE ) /* more processing */
	nodeHspiceName(esTempName);
    }
   ((nodeClient *) (node->efnode_client))->spiceNodeName = 
	    StrDup(NULL, esTempName);

retName:
    return ((nodeClient *) (node->efnode_client))->spiceNodeName;
}

/*
 * ----------------------------------------------------------------------------
 *
 * EFHNSprintf --
 *
 * Create a hierarchical node name.
 * The flags in EFTrimFlags control whether global (!) or local (#)
 * suffixes are to be trimmed. Also substitutes \. with \@ if the 
 * format is hspice.
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	Changes the area pointed to by str
 *
 * ----------------------------------------------------------------------------
 */

int 
EFHNSprintf(str, hierName)
    char *str;
    HierName *hierName;
{
    bool trimGlob, trimLocal;
    char *s, *cp, c;
    char *efHNSprintfPrefix(HierName *, char *);

    s = str;
    if (hierName->hn_parent) str = efHNSprintfPrefix(hierName->hn_parent, str);
    if (EFTrimFlags)
    {
	cp = hierName->hn_name; 
	trimGlob = (EFTrimFlags & EF_TRIMGLOB);
	trimLocal = (EFTrimFlags & EF_TRIMLOCAL);
	while (c = *cp++)
	{
	    switch (c)
	    {
		case '!':	if (!trimGlob) *str++ = c; break;
		case '.':	*str++ = (esFormat == HSPICE)?'@':'.'; break;
		case '#':	if (trimLocal) break;
		default:	*str++ = c; break;
	    }
	}
	*str++ = '\0';
    }
    else strcpy(str, hierName->hn_name);
}

char *efHNSprintfPrefix(hierName, str)
    HierName *hierName;
    char *str;
{
    char *cp, c;

    if (hierName->hn_parent)
	str = efHNSprintfPrefix(hierName->hn_parent, str);

    cp = hierName->hn_name;
    while (*str++ = *cp++) ;
    *(--str) = '/';
    return ++str;
}

/*
 * ----------------------------------------------------------------------------
 *
 * nodeHspiceName --
 *
 * Convert the hierarchical node name used in Berkeley spice 
 * to a name understodd by hspice and hopefully by the user.
 *
 * Results:
 *	A somewhat meaningfull node name
 *
 * Side effects:
 *	Mucks with the hash table above.
 *
 * ----------------------------------------------------------------------------
 */

int nodeHspiceName(s)
    char *s;
{
    char *p, *sf;
    int l, snum = -1;
    HashEntry *he;
    static char map[MAX_STR_SIZE];

    /*
     * find the suffix 
     */
    l = strlen(s);
    for (p = s + l; (p > s) && *p != '/'; p--); 
    if (p == s)
    {
	sprintf(map, s);
	goto topLevel;
    }

    /* 
     * break it into prefix '/0' suffix 
     */
    if (*p == '/') 
	*p = 0; 
    sf = p + 1;

    /* 
     * look up prefix in the hash table ad create it if doesnt exist 
     */
    if ((he = HashLookOnly(&subcktNameTable, s)) == NULL)
    {
        snum = esSbckNum++;
	he = HashFind(&subcktNameTable, s);
	HashSetValue(he, (ClientData)(pointertype) snum);
#ifndef UNSORTED_SUBCKT
	DQPushRear(&subcktNameQueue, he);
#endif
    }
    else 
	snum = (spointertype) HashGetValue(he);
    sprintf(map, "x%d/%s", snum, sf);

topLevel:
    strcpy(s, map);
    if (strlen(s) > 15)
    {
	/* still hspice does not get it */
	sprintf(s, "z@%d", esNodeNum++);
	if (strlen(s) > 15)
	{
	    /* screw it: hspice will not work */
	    TxError("Error: too many nodes in this circuit to be "
			"output as names\n");
	    TxError("       use spice2 format or call and complain "
			"to Meta software about their stupid parser\n");
#ifdef MAGIC_WRAPPER
	    return TCL_ERROR;
#else
	    exit(1);
#endif
	}
    }
}

/*
 * ----------------------------------------------------------------------------
 *
 * printSubcktDict --
 *
 * Print out the hspice subcircuit dictionary. Ideally this should go to a
 * pa0 file but uncfortunately hspice crashes if the node names contain
 * dots so we just append it at the end of the spice file
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	Writes to the output file
 *
 * ----------------------------------------------------------------------------
 */

int printSubcktDict()
{
    HashSearch  hs;
    HashEntry  *he;

    fprintf(esSpiceF,"\n** hspice subcircuit dictionary\n");

#ifndef UNSORTED_SUBCKT
    while ((he = (HashEntry *)DQPopFront(&subcktNameQueue)) != NULL)
#else
    HashStartSearch(&hs);
    while ((he = HashNext(&subcktNameTable, &hs)) != NULL) 
#endif
	fprintf(esSpiceF,"* x%d\t%s\n", HashGetValue(he), he->h_key.h_name);
}

/*
 * ----------------------------------------------------------------------------
 *
 * mkDevMerge --
 * Create a new devMerge structure.
 *
 * Results:
 *    Obvious
 *
 * Side effects:
 *	Allocates memory and sets the fields of the structure.
 *
 * ----------------------------------------------------------------------------
 */

devMerge *mkDevMerge(l, w, g, s, d, b, hn, dev)
    int     l, w;
    EFNode *g, *s, *d, *b;
    HierName *hn;
    Dev    *dev;
{
    devMerge *fp;

    fp = (devMerge *) mallocMagic((unsigned) (sizeof(devMerge)));
    fp->l = l; fp->w = w;
    fp->g = g; fp->s = s;
    fp->d = d; fp->b = b;
    fp->dev = dev;
    fp->esFMIndex = esFMIndex;
    fp->hierName = hn;
    fp->next = NULL;
    addDevMult(1.0);

    return fp;
}

/*
 * ----------------------------------------------------------------------------
 *
 * parallelDevs --
 *
 * Determine if two devs are in parallel
 *
 * Results:
 *    NOT_PARALLEL  if not in parallel
 *    PARALLEL      if parallel and an exact match
 *    ANTIPARALLEL  if parallel but reversed source<->drain nodes
 *
 * Side effects:
 *	None.
 *
 * ----------------------------------------------------------------------------
 */

int
parallelDevs(f1, f2)
    devMerge *f1, *f2;
{
    /* If the devices are not in the same class, then	*/
    /* they cannot be parallel.				*/

    if (f1->dev->dev_class != f2->dev->dev_class)
	return NOT_PARALLEL;

    /* Can't merge devices with different models */
    if (f1->dev->dev_type != f2->dev->dev_type)
	return NOT_PARALLEL;

    /* Class-dependent action */
    switch(f1->dev->dev_class)
    {
	case DEV_MOSFET:
	case DEV_FET:

	    if (f1->b != f2->b) return NOT_PARALLEL;
	    if ((f1->g == f2->g) && (f1->l == f2->l)
			&& (esMergeDevsA || (f1->w == f2->w)))
	    {
		if ((f1->d == f2->d) && (f1->s == f2->s))
		    return PARALLEL;
		else if ((f1->s == f2->d) && (f1->d == f2->s))
		    return ANTIPARALLEL;
	    }
	    break;

	case DEV_ASYMMETRIC:

	    if (f1->b != f2->b) return NOT_PARALLEL;
	    if ((f1->g == f2->g) && (f1->d == f2->d) && (f1->s == f2->s)
			&& (f1->l == f2->l) && (esMergeDevsA || (f1->w == f2->w)))
	    {
		    return PARALLEL;
	    }
	    break;

	/* Capacitors match if top ("gate") and bottom ("source") are	*/
	/* the same.  Do not attempt to swap top and bottom, as we do	*/
	/* not know when it is safe to do so.				*/

	case DEV_CAP:
	    if ((f1->g != f2->g) || (f1->s != f2->s))
		return NOT_PARALLEL;

	    else if (f1->dev->dev_type == esNoModelType)
	    {
		/* Unmodeled capacitor */
		if (esMergeDevsA || (f1->dev->dev_cap == f2->dev->dev_cap))
		    return PARALLEL;
	    }
	    else if (esMergeDevsA || ((f1->l == f2->l) && (f1->w == f2->w)))
		return PARALLEL;
	    break;

	/* We can't merge resistors because we accumulate capacitance	*/
	/* on the central ("gate") node.  Merging the devices would	*/
	/* cause nodes to disappear.					*/

	case DEV_RES:
	    break;

	/* For the remaining devices, it might be possible to merge	*/
	/* if we know that the device model level accepts length and	*/
	/* width parameters.  However, at this time, no such		*/
	/* information is passed to the SPICE deck, so we don't try to	*/
	/* merge these devices.						*/

	case DEV_BJT:
	case DEV_DIODE:
	    break;

	/* There is no way to merge subcircuit devices */

	case DEV_SUBCKT:
	case DEV_RSUBCKT:
	    break;
    }
    return NOT_PARALLEL;
}

/*
 * ----------------------------------------------------------------------------
 *
 * mergeAttr --
 *
 * merge two attribute strings
 *
 * Results:
 *  The merged strings
 *
 * Side effects:
 *  Might allocate and free memory.
 *
 * ----------------------------------------------------------------------------
 */

void
mergeAttr(a1, a2)
    char **a1, **a2;
{
    if (*a1 == NULL)
	*a1 = *a2;
    else
    {
	char *t;
	int l1 = strlen(*a1);
	int l2 = strlen(*a2);
	t = (char *) mallocMagic((unsigned int)((l1 + l2) + 1));
	t = (char *) strcat(*a1, *a2);
	freeMagic(*a1);
	*a1 = t;
    }
}

/*
 * ----------------------------------------------------------------------------
 *
 * devMergeVisit --
 * Visits each dev throu EFVisitDevs and finds if it is in parallel with
 * any previously visited dev.
 *
 * Results:
 *  0 always to keep the caller going.
 *
 * Side effects:
 *  Numerous.
 *
 * ----------------------------------------------------------------------------
 */

int
devMergeVisit(dev, hierName, trans)
    Dev *dev;			/* Dev to examine */
    HierName *hierName;		/* Hierarchical path down to this dev */
    Transform *trans;		/* Coordinate transform not used */
{
    DevTerm *gate, *source, *drain;
    Dev     *cf;
    DevTerm *cg, *cs, *cd;
    EFNode *subnode, *snode, *dnode, *gnode;
    int      scale, pmode, l, w;
    bool     hS, hD, chS, chD;
    Rect r;
    devMerge *fp, *cfp;
    float m;

    if (esDistrJunct)
	devDistJunctVisit(dev, hierName, trans);

    if (dev->dev_nterm < 2)
    {
	TxError("outPremature\n");
	return 0;
    }

    gate = &dev->dev_terms[0];
    source = drain = &dev->dev_terms[1];
    if (dev->dev_nterm >= 3)
	drain = &dev->dev_terms[2];


    gnode = SpiceGetNode(hierName, gate->dterm_node->efnode_name->efnn_hier);
    snode = SpiceGetNode(hierName, source->dterm_node->efnode_name->efnn_hier);
    dnode = SpiceGetNode(hierName, drain->dterm_node->efnode_name->efnn_hier);
    if (dev->dev_subsnode)
	subnode = spcdevSubstrate(hierName,
			dev->dev_subsnode->efnode_name->efnn_hier, 
			dev->dev_type, NULL);
    else
	subnode = NULL;

    GeoTransRect(trans, &dev->dev_rect, &r);
    scale = GeoScale(trans);

    /* Get length and width of the device */
    EFGetLengthAndWidth(dev, &l, &w);

    fp = mkDevMerge(l * scale, w * scale, gnode, snode, 
			dnode, subnode, hierName, dev);
    hS = extHierSDAttr(source);
    hD = extHierSDAttr(drain);

    /*
     * run the list of devs. compare the current one with 
     * each one in the list. if they fullfill the matching requirements
     * merge them only if:
     * 1) they have both apf S, D attributes
     * or 
     * 2) one of them has aph S, D attributes and they have the same 
     *    hierarchical prefix
     * If one of them has apf and the other aph print a warning.
     */

    for (cfp = devMergeList; cfp != NULL; cfp = cfp->next)
    {
	if ((pmode = parallelDevs(fp, cfp)) != NOT_PARALLEL)
	{
	    cf = cfp->dev;
	    cg = &cfp->dev->dev_terms[0];
	    cs = cd = &cfp->dev->dev_terms[1];
	    if (cfp->dev->dev_nterm >= 3)
	    {
		if (pmode == PARALLEL) 
		    cd = &cfp->dev->dev_terms[2];
		    else if (pmode == ANTIPARALLEL) 
			cs = &cfp->dev->dev_terms[2];
	    }

	    chS = extHierSDAttr(cs); chD = extHierSDAttr(cd);
	    if (!(chS || chD || hS || hD)) /* all flat S, D */
		goto mergeThem;
	    if (cfp->hierName != hierName && 
			((chS && !hS) || (chD && !hD) ||
			(!chS && hS) || (!chD && hD)))
	    { 

		efHNSprintfPrefix((cfp->hierName)?cfp->hierName:hierName,
				     esTempName);
		TxError("Warning: conflicting SD attributes of parallel"
			" devs in cell: %s\n", esTempName);
		break;
	    }
	    else if (cfp->hierName == hierName)
	    {
		if (hS && !chS)
		{
		    mergeAttr(&cs->dterm_attrs, &source->dterm_attrs);
		}
		if (hD && !chD)
		{
		    mergeAttr(&cd->dterm_attrs, &drain->dterm_attrs);
		}
	    }
	    else /* cfp->hierName != hierName */
		 break;
mergeThem:
	    switch(dev->dev_class)
	    {
		case DEV_MOSFET:
		case DEV_ASYMMETRIC:
		case DEV_FET:
		    m = esFMult[cfp->esFMIndex] + ((float)fp->w / (float)cfp->w);
		    break;
		case DEV_RES:
		    if (fp->dev->dev_type == esNoModelType)
		        m = esFMult[cfp->esFMIndex] + (fp->dev->dev_res
				/ cfp->dev->dev_res);
		    else
		        m = esFMult[cfp->esFMIndex] + ((float)fp->l / (float)cfp->l);
		    break;
		case DEV_CAP:
		    if (fp->dev->dev_type == esNoModelType)
		        m = esFMult[cfp->esFMIndex] + (fp->dev->dev_cap
				/ cfp->dev->dev_cap);
		    else
		        m = esFMult[cfp->esFMIndex] +
				(((float)fp->l  * (float)fp->w)
				/ ((float)cfp->l * (float)cfp->w));
		    break;
	    }
	    setDevMult(fp->esFMIndex, DEV_KILLED); 
	    setDevMult(cfp->esFMIndex, m);
	    esDevsMerged++;
	    /* Need to do attribute stuff here */
	    freeMagic(fp);
	    return 0;
	}
    }

    /* No parallel devs to it yet */
    fp->next = devMergeList;
    devMergeList = fp;
    return 0;
}

/*
 * ----------------------------------------------------------------------------
 *
 * update_w --
 * Updates the width client of node n with the current dev width
 *
 * Results:
 *  N/A
 *
 * Side effects:
 *  might allocate node client and widths
 *
 * ----------------------------------------------------------------------------
 */

int 
update_w(resClass, w,  n)
    short  resClass;
    int w;
    EFNode *n;
{
    nodeClient *nc;
    int i;

    if (n->efnode_client == (ClientData)NULL) 
	initNodeClient(n);
    nc = (nodeClient *) n->efnode_client;
    if (nc->m_w.widths == NULL)
    {
	(nc->m_w.widths) = (float *)mallocMagic((unsigned)sizeof(float)
		* efNumResistClasses);
	for (i = 0; i < EFDevNumTypes; i++)
	    nc->m_w.widths[i] = 0.0;
    }
    nc->m_w.widths[resClass] += (float)w;
}

/*
 * ----------------------------------------------------------------------------
 *
 * devDistJunctVisit --
 *  Called for every dev and updates the nodeclients of its terminals
 *
 * Results:
 *  0 to keep the calling procedure going
 *
 * Side effects:
 *  calls update_w which might allocate stuff
 *
 * ----------------------------------------------------------------------------
 */

int 
devDistJunctVisit(dev, hierName, trans)
    Dev *dev;			/* Dev to examine */
    HierName *hierName;		/* Hierarchical path down to this dev */
    Transform *trans;		/* Coordinate transform not used */
{
    EFNode  *n;
    int      scale, i;
    int l, w;
    Rect r;

    if (dev->dev_nterm < 2)
    {
	TxError("outPremature\n");
	return 0;
    }

    GeoTransRect(trans, &dev->dev_rect, &r);
    scale = GeoScale(trans);
    w *= scale;
    EFGetLengthAndWidth(dev, &l, &w);

    for (i = 1; i<dev->dev_nterm; i++)
    {
	n = SpiceGetNode(hierName,
		dev->dev_terms[i].dterm_node->efnode_name->efnn_hier);
	update_w(fetInfo[dev->dev_type].resClassSD, w, n);
    }
    return 0;
}

