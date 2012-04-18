#!/bin/sh
#
# For installation, put this file (magic.sh) in a known executable path.
# Put startup script "magic.tcl", shared library "tclmagic.so", and
# "wish" replacement "magicexec" in ${CAD_ROOT}/magic/tcl/.
#
# This script starts magic under the Tcl interpreter,
# reading commands from a special startup script which
# launches magic and retains the Tcl interactive interpreter.

# Parse for the argument "-c[onsole]".  If it exists, run magic
# with the TkCon console.  Strip this argument from the argument list.

TKCON=true
DNULL=
MAGIC_WISH=/usr/local/bin/wish8.5
export MAGIC_WISH

# Hacks for Cygwin
if [ ${TERM:=""} = "cygwin" ]; then
   export PATH="$PATH:/usr/local/lib"
   export DISPLAY=${DISPLAY:=":0"}
fi

for i in $@ ; do
   case $i in
      -noc*) TKCON=;;
      -dnull) DNULL=true;;
   esac
done

if [ $TKCON ]; then

   if [ $DNULL ]; then
      exec /usr/local/lib/magic/tcl/tkcon.tcl -eval "source /usr/local/lib/magic/tcl/console.tcl" \
	   -slave "set argc $#; set argv [list $*]; source /usr/local/lib/magic/tcl/magic.tcl"
   else
      exec /usr/local/lib/magic/tcl/tkcon.tcl -eval "source /usr/local/lib/magic/tcl/console.tcl" \
	   -slave "package require Tk; set argc $#; set argv [list $*]; \
	   source /usr/local/lib/magic/tcl/magic.tcl"
   fi

else

#
# Run the stand-in for wish (magicexec), which acts exactly like "wish"
# except that it replaces ~/.wishrc with magic.tcl.  This executable is
# *only* needed when running without the console; the console itself is
# capable of sourcing the startup script.
#
# With option "-dnull" we set up for operation without Tk (simple interpreter
# only, efficient for running in batch mode).
#
   if [ $DNULL ]; then
      exec /usr/local/lib/magic/tcl/magicdnull -nowrapper $@
   else
      exec /usr/local/lib/magic/tcl/magicexec -- $@
   fi
fi
