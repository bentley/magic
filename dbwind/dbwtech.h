/*
 * dbwtech.h --
 *
 * Style information for display.
 * MAXTILESTYLES is the maximum number of styles usable for display
 * of tiles.
 */

#ifndef _DBWTECH_H
#define	_DBWTECH_H

extern TileTypeBitMask	*DBWStyleToTypesTbl;

#define	DBWStyleToTypes(s)	(DBWStyleToTypesTbl + s)

/* forward declarations */
int  DBWTechParseStyle();

#endif /* _DBWTECH_H */
