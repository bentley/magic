# This is the "Magic wrapper".
# It's main purpose is to redefine the "openwindow" command in magic so that
# opening a new window creates a window wrapped by a GUI interface.
#
# Written by Tim Edwards, August 23, 2002.

# revision A: proof-of-concept.  Add whatever you want to this basic wrapper.
# revision B: Adds Tk scrollbars and caption
# revision C: Adds a layer manager toolbar on the left side
# revision D: Adds a menubar on top with cell and tech manager tools

global windowsopen
global tk_version
global Glyph
global Opts
global Winopts

set tk_version $::tk_version
# Simple console commands (like TkCon, but much simpler)

if {[lsearch [namespace children] ::tkshell] < 0} {
   catch {source ${CAD_ROOT}/magic/tcl/tkshell.tcl}
}

# Button images

set Glyph(up) [image create bitmap \
	-file ${CAD_ROOT}/magic/tcl/bitmaps/up.xbm \
	-background gray -foreground steelblue]
set Glyph(down) [image create bitmap \
	-file ${CAD_ROOT}/magic/tcl/bitmaps/down.xbm \
	-background gray -foreground steelblue]
set Glyph(left) [image create bitmap \
	-file ${CAD_ROOT}/magic/tcl/bitmaps/left.xbm \
	-background gray -foreground steelblue]
set Glyph(right) [image create bitmap \
	-file ${CAD_ROOT}/magic/tcl/bitmaps/right.xbm \
	-background gray -foreground steelblue]
set Glyph(zoom) [image create bitmap \
	-file ${CAD_ROOT}/magic/tcl/bitmaps/zoom.xbm \
	-background gray -foreground steelblue]

# Menu button callback functions

proc magic::promptload {type} {
   global CAD_ROOT

   switch $type {
      cif { set Layoutfilename [ tk_getOpenFile -filetypes \
		{{CIF {.cif {.cif}}} {"All files" {*}}}]
	    if {$Layoutfilename != ""} {
		set cifname [file tail [file root $Layoutfilename]]
		magic::cellname create cif_temp
		magic::load cif_temp
		magic::cif read [file root $Layoutfilename]
		set childcells [magic::cellname list children cif_temp]
		magic::load [lindex $childcells 0]
		magic::cellname delete cif_temp -noprompt
		if {[llength $childcells] > 1} {
		   puts stdout "Cells read from GDS file: $childcells"
		}
	     }
	  }
      gds { set Layoutfilename [ tk_getOpenFile -filetypes \
		{{GDS {.gds .strm .cal {.gds .strm .cal}}} {"All files" {*}}}]
	    if {$Layoutfilename != ""} {
		set origlist [magic::cellname list top]
		magic::gds read [file root $Layoutfilename]
		set newlist [magic::cellname list top]

		# Find entries in newlist that are not in origlist.
		# If there's only one, load it into the window.

		set newtopcells {}
		foreach n $newlist {
		   if {[lsearch $origlist $n] < 0} {
		      lappend newtopcells $n
		   }
		}
		if {[llength $newtopcells] == 1} {
		   magic::load [lindex $newtopcells 0]
		} elseif {[llength $newtopcells] != 0} {
		   puts stdout "Top-level cells read from GDS file: $newtopcells"
		}
	    }
	  }
      magic { set Layoutfilename [ tk_getOpenFile -filetypes \
		{{Magic {.mag {.mag}}} {"All files" {*}}}]
	    if {$Layoutfilename != ""} {
		magic::load [file root $Layoutfilename]
	    }
	  }

      getcell { set Layoutfilename [ tk_getOpenFile -filetypes \
		{{Magic {.mag {.mag}}} {"All files" {*}}}]
	    if {$Layoutfilename != ""} {
		set fdir [file dirname $Layoutfilename]
		set lidx [lsearch [path search] $fdir]
		if {$lidx < 0} {path search +$fdir}
		magic::getcell [file tail $Layoutfilename]

		# Append path to cell search path if it's not there already

		if {[string index $Layoutfilename 0] != "/"} {
		   set $Layoutfilename "./$Layoutfilename"
		}
		set sidx [string last "/" $Layoutfilename]
		if {$sidx > 0} {
		   set cellpath [string range $Layoutfilename 0 $sidx]
		   magic::path cell +$cellpath
		}
	    }
	  }
   }
}

proc magic::promptsave {type} {
   global CAD_ROOT

   switch $type {
      cif { set Layoutfilename [ tk_getSaveFile -filetypes \
		{{CIF {.cif {.cif}}} {"All files" {*}}}]
	    if {$Layoutfilename != ""} {
	       magic::cif write $Layoutfilename
	    }
	  }
      gds { set Layoutfilename [ tk_getSaveFile -filetypes \
		{{GDS {.gds .strm .cal {.gds .strm .cal}}} {"All files" {*}}}]
	    if {$Layoutfilename != ""} {
		magic::gds write $Layoutfilename
	    }
	  }
      magic {
	    set CellList [ magic::cellname list window ]
	    if {[lsearch $CellList "(UNNAMED)"] >= 0} {
	       set Layoutfilename [ tk_getSaveFile -filetypes \
		   {{Magic {.mag {.mag}}} {"All files" {*}}}]
	       if {$Layoutfilename != ""} {
		   set cellpath [file dirname $Layoutfilename]
		   if {$cellpath == [pwd]} {
		      set Layoutfilename [file tail $Layoutfilename]
		   } else {
		      magic::path cell +$cellpath
		   }
		   magic::save $Layoutfilename
	       }
	    }
	    magic::writeall
	  }
   }
}

# Callback functions used by the DRC.

proc magic::drcupdate { option } {
   global Opts
   if {[info level] <= 1} {
      switch $option {
         on {set Opts(drc) 1}
         off {set Opts(drc) 0}
      }
   }
}

proc magic::drcstate { status } {
   set winlist [*bypass windownames layout]
   foreach lwin $winlist {
      set framename [winfo parent $lwin]
      switch $status {
         idle { 
		set dct [*bypass drc count total]
		if {$dct > 0} {
	           ${framename}.titlebar.drcbutton configure -selectcolor red
		} else {
	           ${framename}.titlebar.drcbutton configure -selectcolor green
		}
	        ${framename}.titlebar.drcbutton configure -text "DRC=$dct"
	      }
         busy { ${framename}.titlebar.drcbutton configure -selectcolor yellow }
      }
   }
}

# Create the menu of windows.  This is kept separate from the cell manager,
# and linked into it by the "clone" command.

menu .winmenu -tearoff 0

proc magic::setgrid {gridsize} {
   set techlambda [magic::tech lambda]
   set tech1 [lindex $techlambda 1]
   set tech0 [lindex $techlambda 0]
   set tscale [expr {$tech1 / $tech0}]
   set lambdaout [expr {[magic::cif lambda output] * $tscale}]
   set gridlambda [expr {$gridsize/$lambdaout}]
   magic::grid ${gridlambda}l
   magic::snap on
}

# Technology manager callback functions

proc magic::techparseunits {} {
   set techlambda [magic::tech lambda]
   set tech1 [lindex $techlambda 1]
   set tech0 [lindex $techlambda 0]

   set target0 [.techmgr.lambda1.lval0 get]
   set target1 [.techmgr.lambda1.lval1 get]

   set newval0 [expr {$target0 * $tech0}]
   set newval1 [expr {$target1 * $tech1}]

   magic::scalegrid $newval1 $newval0
   magic::techmanager update
}

# The technology manager

proc magic::maketechmanager { mgrpath } {
   toplevel $mgrpath
   wm withdraw $mgrpath

   frame ${mgrpath}.title
   label ${mgrpath}.title.tlab -text "Technology: "
   menubutton ${mgrpath}.title.tname -text "(none)" -foreground red3 \
	-menu ${mgrpath}.title.tname.menu
   label ${mgrpath}.title.tvers -text "" -foreground blue
   label ${mgrpath}.subtitle -text "" -foreground sienna4

   frame ${mgrpath}.lambda0
   label ${mgrpath}.lambda0.llab -text "Microns per lambda (CIF): "
   label ${mgrpath}.lambda0.lval -text "1" -foreground blue

   frame ${mgrpath}.lambda1
   label ${mgrpath}.lambda1.llab -text "Internal units per lambda: "
   entry ${mgrpath}.lambda1.lval0 -foreground red3 -background white -width 3
   label ${mgrpath}.lambda1.ldiv -text " / "
   entry ${mgrpath}.lambda1.lval1 -foreground red3 -background white -width 3

   frame ${mgrpath}.cif0
   label ${mgrpath}.cif0.llab -text "CIF input style: "
   menubutton ${mgrpath}.cif0.lstyle -text "" -foreground blue \
	-menu ${mgrpath}.cif0.lstyle.menu
   label ${mgrpath}.cif0.llab2 -text " Microns/lambda="
   label ${mgrpath}.cif0.llambda -text "" -foreground red3

   frame ${mgrpath}.cif1
   label ${mgrpath}.cif1.llab -text "CIF output style: "
   menubutton ${mgrpath}.cif1.lstyle -text "" -foreground blue \
	-menu ${mgrpath}.cif1.lstyle.menu
   label ${mgrpath}.cif1.llab2 -text " Microns/lambda="
   label ${mgrpath}.cif1.llambda -text "" -foreground red3

   frame ${mgrpath}.extract
   label ${mgrpath}.extract.llab -text "Extract style: "
   menubutton ${mgrpath}.extract.lstyle -text "" -foreground blue \
	-menu ${mgrpath}.extract.lstyle.menu

   frame ${mgrpath}.drc
   label ${mgrpath}.drc.llab -text "DRC style: "
   menubutton ${mgrpath}.drc.lstyle -text "" -foreground blue \
	-menu ${mgrpath}.drc.lstyle.menu

   pack ${mgrpath}.title.tlab -side left
   pack ${mgrpath}.title.tname -side left
   pack ${mgrpath}.title.tvers -side left
   pack ${mgrpath}.lambda0.llab -side left
   pack ${mgrpath}.lambda0.lval -side left
   pack ${mgrpath}.lambda1.llab -side left
   pack ${mgrpath}.lambda1.lval0 -side left
   pack ${mgrpath}.lambda1.ldiv -side left
   pack ${mgrpath}.lambda1.lval1 -side left
   pack ${mgrpath}.cif0.llab -side left
   pack ${mgrpath}.cif0.lstyle -side left
   pack ${mgrpath}.cif0.llab2 -side left
   pack ${mgrpath}.cif0.llambda -side left
   pack ${mgrpath}.cif1.llab -side left
   pack ${mgrpath}.cif1.lstyle -side left
   pack ${mgrpath}.cif1.llab2 -side left
   pack ${mgrpath}.cif1.llambda -side left
   pack ${mgrpath}.extract.llab -side left
   pack ${mgrpath}.extract.lstyle -side left
   pack ${mgrpath}.drc.llab -side left
   pack ${mgrpath}.drc.lstyle -side left

   pack ${mgrpath}.title -side top -fill x
   pack ${mgrpath}.subtitle -side top -fill x
   pack ${mgrpath}.lambda0 -side top -fill x
   pack ${mgrpath}.lambda1 -side top -fill x
   pack ${mgrpath}.cif0 -side top -fill x
   pack ${mgrpath}.cif1 -side top -fill x
   pack ${mgrpath}.extract -side top -fill x

   bind ${mgrpath}.lambda1.lval0 <Return> magic::techparseunits
   bind ${mgrpath}.lambda1.lval1 <Return> magic::techparseunits

   #Withdraw the window when the close button is pressed
   wm protocol ${mgrpath} WM_DELETE_WINDOW  "set Opts(techmgr) 0 ; wm withdraw ${mgrpath}"
}

# Generate the cell manager

catch {source ${CAD_ROOT}/magic/tcl/cellmgr.tcl}

# Generate the text helper

catch {source ${CAD_ROOT}/magic/tcl/texthelper.tcl}

# Create or redisplay the technology manager

proc magic::techmanager {{option "update"}} {
   global CAD_ROOT

   if {[catch {wm state .techmgr}]} {
      if {$option == "create"} {
	 magic::maketechmanager .techmgr
      } else {
	 return
      }
   } elseif { $option == "create"} {
      return
   }

   if {$option == "create"} {
      menu .techmgr.title.tname.menu -tearoff 0
      menu .techmgr.cif0.lstyle.menu -tearoff 0
      menu .techmgr.cif1.lstyle.menu -tearoff 0
      menu .techmgr.extract.lstyle.menu -tearoff 0
      menu .techmgr.drc.lstyle.menu -tearoff 0
    
   }

   if {$option == "init"} {
	.techmgr.title.tname.menu delete 0 end
	.techmgr.cif0.lstyle.menu delete 0 end
	.techmgr.cif1.lstyle.menu delete 0 end
	.techmgr.extract.lstyle.menu delete 0 end
	.techmgr.drc.lstyle.menu delete 0 end
   }

   if {$option == "init" || $option == "create"} {
      set tlist [magic::cif listall istyle]
      foreach i $tlist {
	 .techmgr.cif0.lstyle.menu add command -label $i -command \
		"magic::cif istyle $i ; \
		 magic::techmanager update"
      }

      set tlist [magic::cif listall ostyle]
      foreach i $tlist {
	 .techmgr.cif1.lstyle.menu add command -label $i -command \
		"magic::cif ostyle $i ; \
		 magic::techmanager update"
      }

      set tlist [magic::extract listall style]
      foreach i $tlist {
	 .techmgr.extract.lstyle.menu add command -label $i -command \
		"magic::extract style $i ; \
		 magic::techmanager update"
      }

      set tlist [magic::drc listall style]
      foreach i $tlist {
	 .techmgr.drc.lstyle.menu add command -label $i -command \
		"magic::drc style $i ; \
		 magic::techmanager update"
      }

      set dirlist [subst [magic::path sys]]
      set tlist {}
      foreach i $dirlist {
	 lappend tlist [glob -nocomplain ${i}/*.tech]
	 lappend tlist [glob -nocomplain ${i}/*.tech27]
      }
      foreach i [join $tlist] {
	 set j [file tail [file rootname ${i}]]
	 .techmgr.title.tname.menu add command -label $j -command \
		"magic::tech load $j ; \
		 magic::techmanager update"
      }
   }

   set techlambda [magic::tech lambda]
   set tech1 [lindex $techlambda 1]
   set tech0 [lindex $techlambda 0]
   set tscale [expr {$tech1 / $tech0}]

   .techmgr.title.tname configure -text [magic::tech name]
   set techstuff [magic::tech version]
   .techmgr.title.tvers configure -text "(version [lindex $techstuff 0])"
   .techmgr.subtitle configure -text [lindex $techstuff 1]
   set lotext [format "%g" [expr {[magic::cif lambda output] * $tscale}]]
   .techmgr.lambda0.lval configure -text $lotext
   .techmgr.cif0.lstyle configure -text [magic::cif list istyle]
   set litext [format "%g" [expr {[magic::cif lambda input] * $tscale}]]
   .techmgr.cif0.llambda configure -text $litext
   .techmgr.cif1.lstyle configure -text [magic::cif list ostyle]
   .techmgr.cif1.llambda configure -text $lotext
   .techmgr.extract.lstyle configure -text [magic::extract list style]
   .techmgr.drc.lstyle configure -text [magic::drc list style]

   .techmgr.lambda1.lval0 delete 0 end
   .techmgr.lambda1.lval1 delete 0 end
   .techmgr.lambda1.lval0 insert end $tech1
   .techmgr.lambda1.lval1 insert end $tech0
}

proc magic::captions {{subcommand {}}} {
   global Opts

   if {$subcommand != {} && $subcommand != "writeable" && $subcommand != "load"} {
      return
   }
   set winlist [magic::windownames layout]
   foreach winpath $winlist {
      set framename [winfo parent $winpath]
      set caption [$winpath windowcaption]
      set subcaption1 [lindex $caption 0]
      set techname [tech name]
      if {[catch {set Opts(tool)}]} {
         set Opts(tool) unknown
      }
      if {[lindex $caption 1] == "EDITING"} {
         set subcaption2 [lindex $caption 2]
      } else {
         # set subcaption2 [join [lrange $caption 1 end]]
         set subcaption2 $caption
      }
      ${framename}.titlebar.caption configure -text \
	   "Loaded: ${subcaption1} Editing: ${subcaption2} Tool: $Opts(tool) \
	   Technology: ${techname}"
   }
}

# Allow captioning in the title window by tagging the "load" and "edit" commands
# Note that the "box" tag doesn't apply to mouse-button events, so this function
# is duplicated by Tk binding of mouse events in the layout window.

magic::tag load "[magic::tag load]; magic::captions"
magic::tag edit "magic::captions"
magic::tag save "magic::captions"
magic::tag down "magic::captions"
magic::tag box "magic::boxview %W %1"
magic::tag move "magic::boxview %W"
magic::tag scroll "magic::scrollupdate %W"
magic::tag view "magic::scrollupdate %W"
magic::tag zoom "magic::scrollupdate %W"
magic::tag findbox "magic::scrollupdate %W"
magic::tag see "magic::toolupdate %W %1 %2"
magic::tag tech "magic::techrebuild %W %1; magic::captions %1"
magic::tag drc "magic::drcupdate %1"
magic::tag path "magic::techmanager update"
magic::tag cellname "magic::mgrupdate %W %1"
magic::tag cif      "magic::mgrupdate %W %1"
magic::tag gds      "magic::mgrupdate %W %1"

# This should be a list. . . do be done later
set windowsopen 0

set Opts(techmgr) 0
set Opts(target)  default
set Opts(netlist) 0
set Opts(colormap) 0
set Opts(wind3d) 0
set Opts(crosshair) 0
set Opts(drc) 1

# Update cell and tech managers in response to a cif or gds read command

proc magic::mgrupdate {win {cmdstr ""}} {
   if {${cmdstr} == "read"} {
      catch {magic::cellmanager}
      magic::captions
      magic::techmanager update
   } elseif {${cmdstr} == "delete" || ${cmdstr} == "rename"} {
      catch {magic::cellmanager}
      magic::captions
   } elseif {${cmdstr} == "writeable"} {
      magic::captions
   }
}

# Set default width and height to be 3/4 of the screen size.
set Opts(geometry) \
"[expr 3 * [winfo screenwidth .] / 4]x[expr 3 * [winfo screenheight .] \
/ 4]+100+100"

# Procedures for the layout scrollbars, which are made from canvas
# objects to avoid the problems associated with Tk's stupid scrollbar
# implementation.

# Repainting function for scrollbars, title, etc., to match the magic
# Change the colormap (most useful in 8-bit PseudoColor)

proc magic::repaintwrapper { win } {
   set bgcolor [magic::magiccolor -]
   ${win}.xscroll configure -background $bgcolor
   ${win}.xscroll configure -highlightbackground $bgcolor
   ${win}.xscroll configure -highlightcolor [magic::magiccolor K]

   ${win}.yscroll configure -background $bgcolor
   ${win}.yscroll configure -highlightbackground $bgcolor
   ${win}.yscroll configure -highlightcolor [magic::magiccolor K]

   ${win}.titlebar.caption configure -background [magic::magiccolor w]
   ${win}.titlebar.caption configure -foreground [magic::magiccolor c]

   ${win}.titlebar.message configure -background [magic::magiccolor w]
   ${win}.titlebar.message configure -foreground [magic::magiccolor c]

   ${win}.titlebar.pos configure -background [magic::magiccolor w]
   ${win}.titlebar.pos configure -foreground [magic::magiccolor c]

}

# Coordinate display callback function
# Because "box" calls "box", use the "info level" command to avoid
# infinite recursion.

proc magic::boxview {win {cmdstr ""}} {
   if {${cmdstr} == "exists" || ${cmdstr} == "help" || ${cmdstr} == ""} {
      # do nothing. . . informational only, no change to the box
   } elseif {[info level] <= 1} {
      # For NULL window, find all layout windows and apply update to each.
      if {$win == {}} {
         set winlist [magic::windownames layout]
         foreach lwin $winlist {
	    magic::boxview $lwin
         }
         return
      }

      set framename [winfo parent $win]
      set lambda [${win} tech lambda]
      set lr [expr {(0.0 + [lindex $lambda 0]) / [lindex $lambda 1]}]
      set bval [${win} box values]
      set bllx [expr {[lindex $bval 0] * $lr }]
      set blly [expr {[lindex $bval 1] * $lr }]
      set burx [expr {[lindex $bval 2] * $lr }]
      set bury [expr {[lindex $bval 3] * $lr }]
      if {[expr {$bllx == int($bllx)}]} {set bllx [expr {int($bllx)}]}
      if {[expr {$blly == int($blly)}]} {set blly [expr {int($blly)}]}
      if {[expr {$burx == int($burx)}]} {set burx [expr {int($burx)}]}
      if {[expr {$bury == int($bury)}]} {set bury [expr {int($bury)}]}
      ${framename}.titlebar.pos configure -text \
		"box ($bllx, $blly) to ($burx, $bury) lambda"
   }
}

proc magic::cursorview {win} {
   global Opts
   if {$win == {}} {
      return
   }
   set framename [winfo parent $win]
   set lambda [${win} tech lambda]
   if {[llength $lambda] != 2} {return}
   set lr [expr {(0.0 + [lindex $lambda 0]) / [lindex $lambda 1]}]
   set olst [${win} cursor lambda]
   set olstx [lindex $olst 0]
   set olsty [lindex $olst 1]

   if {$Opts(crosshair)} {
      *bypass crosshair ${olstx}l ${olsty}l
   }

   if {[${win} box exists]} {
      set dlst [${win} box position]
      set dx [expr {$olstx - ([lindex $dlst 0]) * $lr }]
      set dy [expr {$olsty - ([lindex $dlst 1]) * $lr }]
      if {[expr {$dx == int($dx)}]} {set dx [expr {int($dx)}]}
      if {[expr {$dy == int($dy)}]} {set dy [expr {int($dy)}]}
      if {$dx >= 0} {set dx "+$dx"}
      if {$dy >= 0} {set dy "+$dy"}
      set titletext [format "(%g %g) %g %g lambda" $olstx $olsty $dx $dy]
      ${framename}.titlebar.pos configure -text $titletext
   } else {
      set titletext [format "(%g %g) lambda" $olstx $olsty]
      ${framename}.titlebar.pos configure -text $titletext
   }
}

proc magic::toolupdate {win {yesno "yes"} {layerlist "none"}} {
   global Winopts

   # For NULL window, get current window
   if {$win == {}} {
      set win [magic::windownames]
   }

   # Wind3d has a "see" function, so make sure this is not a 3d window
   if {$win == [magic::windownames wind3d]} {
      return
   }

   set framename [winfo parent $win]

   # Don't do anything if toolbar is not present
   if { $Winopts(${framename},toolbar) == 0 } { return }

   if {$layerlist == "none"} {
	set layerlist $yesno
	set yesno "yes"
   }
   if {$layerlist == "*"} {
      set layerlist [magic::tech layer "*"]
   }
   foreach layer $layerlist {
      switch $layer {
	 none {}
	 errors {set canon $layer}
	 subcell {set canon $layer}
	 labels {set canon $layer}
	 default {set canon [magic::tech layer $layer]}
      }
      if {$layer != "none"} {
         if {$yesno == "yes"} {
	    ${framename}.toolbar.b$canon configure -image img_$canon 
         } else {
	    ${framename}.toolbar.b$canon configure -image img_space
	 }
      }
   }
}

# Generate the toolbar for the wrapper

proc magic::maketoolbar { framename } {

   # Destroy any existing toolbar before starting
   set alltools [winfo children ${framename}.toolbar]
   foreach i $alltools {
      destroy $i
   }

   # All toolbar commands will be passed to the appropriate window
   set win ${framename}.magic

   # Make sure that window has been created so we will get the correct
   # height value.

   update idletasks
   set winheight [expr {[winfo height ${framename}] - \
		[winfo height ${framename}.titlebar]}]

   # Generate a layer image for "space" that will be used when layers are
   # invisible.

   image create layer img_space -name none

   # Generate layer images and buttons for toolbar

   set all_layers [concat {errors labels subcell} [magic::tech layer "*"]]
   foreach layername $all_layers {
      image create layer img_$layername -name $layername
      button ${framename}.toolbar.b$layername -image img_$layername -command \
		"$win see $layername"

      # Bindings:  Entering the button puts the canonical layer name in the 
      # message window.
      bind ${framename}.toolbar.b$layername <Enter> \
		[subst {focus %W ; ${framename}.titlebar.message configure \
		 -text "$layername"}]
      bind ${framename}.toolbar.b$layername <Leave> \
		[subst {${framename}.titlebar.message configure -text ""}]

      # 3rd mouse button makes layer invisible; 1st mouse button restores it.
      # 2nd mouse button paints the layer color.  Key "p" also does paint, esp.
      # for users with 2-button mice.  Key "e" erases, as does Shift-Button-2.

      bind ${framename}.toolbar.b$layername <ButtonPress-2> \
		"$win paint $layername"
      bind ${framename}.toolbar.b$layername <KeyPress-p> \
		"$win paint $layername"
      bind ${framename}.toolbar.b$layername <Shift-ButtonPress-2> \
		"$win erase $layername"
      bind ${framename}.toolbar.b$layername <KeyPress-e> \
		"$win erase $layername"
      bind ${framename}.toolbar.b$layername <ButtonPress-3> \
		"$win see no $layername"
      bind ${framename}.toolbar.b$layername <KeyPress-s> \
		"$win select more area $layername"
      bind ${framename}.toolbar.b$layername <KeyPress-S> \
		"$win select less area $layername"
   }

   # Figure out how many columns we need to fit all the layer buttons inside
   # the toolbar without going outside the window area.

   set ncols 1
   while {1} {
      incr ncols
      set i 0
      set j 0
      foreach layername $all_layers {
         grid ${framename}.toolbar.b$layername -row $i -column $j -sticky news
         incr j
         if {$j == $ncols} {
	    set j 0
	    incr i
         }
      }
      # tkwait visibility ${framename}.toolbar
      update idletasks
      set toolheight [lindex [grid bbox ${framename}.toolbar] 3]
      if {$toolheight <= $winheight} {break}
   }
}

# Delete and rebuild the toolbar buttons in response to a "tech load"
# command.

proc magic::techrebuild {winpath {cmdstr ""}} {

   # For NULL window, find all layout windows and apply update to each.
   if {$winpath == {}} {
      set winlist [magic::windownames layout]
      foreach lwin $winlist {
	 magic::techrebuild $lwin $cmdstr
      }
      return
   }

   set framename [winfo parent $winpath]
   if {${cmdstr} == "load"} {
      maketoolbar ${framename}
      magic::techmanager init
   }
}

# Scrollbar callback procs

# Procedure to return the effective X and Y scrollbar bounds for the
# current view in magic (in pixels)

proc magic::setscrollvalues {win} {
   set svalues [${win} view get]
   set bvalues [${win} view bbox]

   set bwidth [expr {[lindex $bvalues 2] - [lindex $bvalues 0]}]
   set bheight [expr {[lindex $bvalues 3] - [lindex $bvalues 1]}]

   set framename [winfo parent ${win}]
   set wwidth [winfo width ${framename}.xscroll.bar]  ;# horizontal scrollbar
   set wheight [winfo height ${framename}.yscroll.bar]  ;# vertical scrollbar

   # Note that adding 0.0 here forces floating-point

   set xscale [expr {(0.0 + $wwidth) / $bwidth}]
   set yscale [expr {(0.0 + $wheight) / $bheight}]

   set xa [expr {$xscale * ([lindex $svalues 0] - [lindex $bvalues 0]) }]
   set xb [expr {$xscale * ([lindex $svalues 2] - [lindex $bvalues 0]) }]
   set ya [expr {$yscale * ([lindex $svalues 1] - [lindex $bvalues 1]) }]
   set yb [expr {$yscale * ([lindex $svalues 3] - [lindex $bvalues 1]) }]

   # Magic's Y axis is inverted with respect to X11 window coordinates
   set ya [expr { $wheight - $ya }]
   set yb [expr { $wheight - $yb }]

   ${framename}.xscroll.bar coords slider $xa 2 $xb 15
   ${framename}.yscroll.bar coords slider 2 $ya 15 $yb

   set xb [expr { 1 + ($xa + $xb) / 2 }]
   set xa [expr { $xb - 2 }]
   ${framename}.xscroll.bar coords centre $xa 4 $xb 13

   set yb [expr { 1 + ($ya + $yb) / 2 }]
   set ya [expr { $yb - 2 }]
   ${framename}.yscroll.bar coords centre 4 $ya 13 $yb
}

# Procedure to update scrollbars in response to an internal command
# "view" calls "view", so avoid infinite recursion.

proc magic::scrollupdate {win} {

   if {[info level] <= 1} {

      # For NULL window, find current window
      if {$win == {}} {
	 set win [magic::windownames]
      }

      # Make sure we're not a 3D window, which doesn't take window commands
      # This is only necessary because the 3D window declares a "view"
      # command, too.

      if {$win != [magic::windownames wind3d]} {
 	 magic::setscrollvalues $win
      }
   }
}

# scrollview:  update the magic display to match the
# scrollbar positions.

proc magic::scrollview { w win orient } {
   global scale
   set v1 $scale($orient,origin)
   set v2 $scale($orient,update)
   set delta [expr {$v2 - $v1}]

   set bvalues [${win} view bbox]
   set wvalues [${win} windowpositions]

   # Note that adding 0.000 in expression forces floating-point

   if {"$orient" == "x"} {

      set bwidth [expr {[lindex $bvalues 2] - [lindex $bvalues 0]}]
      set wwidth [expr {0.000 + [lindex $wvalues 2] - [lindex $wvalues 0]}]
      set xscale [expr {$bwidth / $wwidth}]
      ${win} scroll e [expr {$delta * $xscale}]i

   } else {

      set bheight [expr {[lindex $bvalues 3] - [lindex $bvalues 1]}]
      set wheight [expr {0.000 + [lindex $wvalues 3] - [lindex $wvalues 1]}]
      set yscale [expr {$bheight / $wheight}]
      ${win} scroll s [expr {$delta * $yscale}]i
   }
}

# setscroll: get the current cursor position and save it as a
# reference point.

proc magic::setscroll { w v orient } {
   global scale
   set scale($orient,origin) $v
   set scale($orient,update) $v
}

proc magic::dragscroll { w v orient } {
   global scale
   set v1 $scale($orient,update)
   set scale($orient,update) $v
   set delta [expr {$v - $v1}]

   if { "$orient" == "x" } {
      $w move slider $delta 0
      $w move centre $delta 0
   } else {
      $w move slider 0 $delta
      $w move centre 0 $delta
   }
}

# Scrollbar generator for the wrapper window

proc magic::makescrollbar { fname orient win } {
   global scale
   global Glyph

   set scale($orient,update) 0
   set scale($orient,origin) 0

   # To be done:  add glyphs for the arrows

   if { "$orient" == "x" } {
      canvas ${fname}.bar -height 13 -relief sunken -borderwidth 1
      button ${fname}.lb -image $Glyph(left) -borderwidth 1 \
		-command "${win} scroll left .1 w"
      button ${fname}.ub -image $Glyph(right) -borderwidth 1 \
		-command "${win} scroll right .1 w"
      pack ${fname}.lb -side left
      pack ${fname}.bar -fill $orient -expand true -side left
      pack ${fname}.ub -side right
   } else {
      canvas ${fname}.bar -width 13 -relief sunken -borderwidth 1
      button ${fname}.lb -image $Glyph(down) -borderwidth 1 \
		-command "${win} scroll down .1 w"
      button ${fname}.ub -image $Glyph(up) -borderwidth 1 \
		-command "${win} scroll up .1 w"
      pack ${fname}.ub
      pack ${fname}.bar -fill $orient -expand true
      pack ${fname}.lb
   }

   # Create the bar which controls the scrolling and bind actions to it
   ${fname}.bar create rect 2 2 15 15 -fill steelblue -width 0 -tag slider
   ${fname}.bar bind slider <Button-1> "magic::setscroll %W %$orient $orient"
   ${fname}.bar bind slider <ButtonRelease-1> "magic::scrollview %W $win $orient"
   ${fname}.bar bind slider <B1-Motion> "magic::dragscroll %W %$orient $orient"

   # Create a small mark in the center of the scrolling rectangle which aids
   # in determining how much the window is being scrolled when the full
   # scrollbar extends past the window edges.
   ${fname}.bar create rect 4 4 13 13 -fill black -width 0 -tag centre
   ${fname}.bar bind centre <Button-1> "magic::setscroll %W %$orient $orient"
   ${fname}.bar bind centre <ButtonRelease-1> "magic::scrollview %W $win $orient"
   ${fname}.bar bind centre <B1-Motion> "magic::dragscroll %W %$orient $orient"
}

# Create the wrapper and open up a layout window in it.

proc magic::openwrapper {{cell ""} {framename ""}} {
   global windowsopen
   global tk_version
   global Glyph
   global Opts
   global Winopts
   
   # Disallow scrollbars and title caption on windows---we'll do these ourselves

   if {$windowsopen == 0} {
      windowcaption off
      windowscrollbars off
      windowborder off
   }

   incr windowsopen 
   if {$framename == ""} {
      set framename .layout${windowsopen}
   }
   set winname ${framename}.magic
   
   toplevel $framename
   tkwait visibility $framename

   frame ${framename}.xscroll -height 13
   frame ${framename}.yscroll -width 13

   magic::makescrollbar ${framename}.xscroll x ${winname}
   magic::makescrollbar ${framename}.yscroll y ${winname}
   button ${framename}.zb -image $Glyph(zoom) -borderwidth 1 -command "${winname} zoom 2"

   # Add bindings for mouse buttons 2 and 3 to the zoom button
   bind ${framename}.zb <Button-3> "${winname} zoom 0.5"
   bind ${framename}.zb <Button-2> "${winname} view"

   frame ${framename}.titlebar
   label ${framename}.titlebar.caption -text "Loaded: none Editing: none Tool: box" \
	-foreground white -background sienna4 -anchor w -padx 15
   label ${framename}.titlebar.message -text "" -foreground white \
	-background sienna4 -anchor w -padx 5
   label ${framename}.titlebar.pos -text "" -foreground white \
	-background sienna4 -anchor w -padx 5

   # Menu buttons
   frame ${framename}.titlebar.mbuttons

   menubutton ${framename}.titlebar.mbuttons.file -text File -relief raised \
		-menu ${framename}.titlebar.mbuttons.file.filemenu -borderwidth 2
   menubutton ${framename}.titlebar.mbuttons.edit -text Edit -relief raised \
		-menu ${framename}.titlebar.mbuttons.edit.editmenu -borderwidth 2
   menubutton ${framename}.titlebar.mbuttons.view -text View -relief raised \
		-menu ${framename}.titlebar.mbuttons.view.viewmenu -borderwidth 2
   menubutton ${framename}.titlebar.mbuttons.opts -text Options -relief raised \
		-menu ${framename}.titlebar.mbuttons.opts.optsmenu -borderwidth 2
   pack ${framename}.titlebar.mbuttons.file -side left
   pack ${framename}.titlebar.mbuttons.edit -side left
   pack ${framename}.titlebar.mbuttons.view -side left
   pack ${framename}.titlebar.mbuttons.opts -side left

   # DRC status button
   checkbutton ${framename}.titlebar.drcbutton -text "DRC" -anchor w \
	-borderwidth 2 -variable Opts(drc) \
	-foreground white -background sienna4 -selectcolor green \
	-command [subst { if { \$Opts(drc) } { drc on } else { drc off } }]

   magic::openwindow $cell $winname

   # Create toolbar frame.  Make sure it has the same visual and depth as
   # the layout window, so there will be no problem using the GCs from the
   # layout window to paint into the toolbar.
   frame ${framename}.toolbar \
	-visual "[winfo visual ${winname}] [winfo depth ${winname}]"

   # Repaint to magic colors
   magic::repaintwrapper ${framename}

   grid ${framename}.titlebar -row 0 -column 0 -columnspan 3 -sticky news
   grid ${framename}.yscroll -row 1 -column 0 -sticky ns
   grid $winname -row 1 -column 1 -sticky news
   grid ${framename}.zb -row 2 -column 0
   grid ${framename}.xscroll -row 2 -column 1 -sticky ew
   # The toolbar is not attached by default

   grid rowconfigure ${framename} 1 -weight 1
   grid columnconfigure ${framename} 1 -weight 1

   grid ${framename}.titlebar.mbuttons -row 0 -column 0 -sticky news
   grid ${framename}.titlebar.drcbutton -row 0 -column 1 -sticky news
   grid ${framename}.titlebar.caption -row 0 -column 2 -sticky news
   grid ${framename}.titlebar.message -row 0 -column 3 -sticky news
   grid ${framename}.titlebar.pos -row 0 -column 4 -sticky news
   grid columnconfigure ${framename}.titlebar 2 -weight 1

   bind $winname <Enter> "focus %W ; set Opts(focus) $framename"

   # Note: Tk binding bypasses the event proc, so it is important to
   # set the current point;  otherwise, the cursor will report the
   # wrong position and/or the wrong window.  HOWEVER we should wrap
   # this command with the "bypass" command such that it does not
   # reset any current input redirection to the terminal.

   bind ${winname} <Motion> "*bypass setpoint %x %y ${winname}; \
	magic::cursorview ${winname}"

   # Resize the window
   if {[catch {wm geometry ${framename} $Winopts(${framename},geometry)}]} {
      catch {wm geometry ${framename} $Opts(geometry)}
   }

   set Winopts(${framename},toolbar) 0
   set Winopts(${framename},cmdentry) 0

   # Generate the file menu

   set m [menu ${framename}.titlebar.mbuttons.file.filemenu -tearoff 0]
   $m add command -label "New window" -command "magic::openwrapper \
		[${winname} cellname list window]"
   $m add command -label "Close" -command "magic::closewrapper ${framename}"
   $m add separator
   $m add command -label "Place Cell" -command {magic::promptload getcell}
   $m add separator
   $m add command -label "Load layout" -command {magic::promptload magic}
   $m add command -label "Read CIF" -command {magic::promptload cif}
   $m add command -label "Read GDS" -command {magic::promptload gds}
   $m add separator
   $m add command -label "Save layout" -command {magic::promptsave magic}
   $m add command -label "Write CIF" -command {magic::promptsave cif}
   $m add command -label "Write GDS" -command {magic::promptsave gds}
   $m add separator
   $m add command -label "Flush current" -command {magic::flush}
   $m add command -label "Delete current" -command {magic::cellname delete \
		[magic::cellname list window]}
   $m add separator
   $m add command -label "Quit" -command {magic::quit}

   # Generate the edit menu

   set m [menu ${framename}.titlebar.mbuttons.edit.editmenu -tearoff 0]
   $m add command -label "Undo          (u)" -command {magic::undo}
   $m add command -label "Redo          (U)" -command {magic::redo}
   $m add separator
   $m add command -label "Rotate 90 degree " -command {magic::clock}
   $m add command -label "Flip Horizontal" -command {magic::sideways}
   $m add command -label "Flip Vertical"   -command {magic::upsidedown}
   $m add separator
   $m add command -label "Text ..." \
		-command [subst { magic::update_texthelper; \
		wm deiconify .texthelper ; raise .texthelper } ]
   $m add separator
   $m add command -label "Lock   Cell      " -command {magic::instance lock}
   $m add command -label "Unlock Cell      " -command {magic::instance unlock}
   $m add separator
   $m add command -label "Unlock Base Layers" -command {magic::tech layer unlock *}

   # Generate the view menu

   set m [menu ${framename}.titlebar.mbuttons.view.viewmenu -tearoff 0]
   $m add command -label "Zoom In   " -command {magic::zoom 0.5}
   $m add command -label "Zoom Out      (Z)" -command {magic::zoom 2}
   $m add command -label "Zoom Sel      (z)" -command {magic::findbox zoom}
   $m add command -label "Full" -command {magic::select top cell ; findbox zoom}
   $m add separator
   $m add command -label "Grid 0.10u        " -command {magic::setgrid 0.1}
   $m add command -label "Grid 0.20u        " -command {magic::setgrid 0.2}
   $m add command -label "Grid 0.25u        " -command {magic::setgrid 0.25}
   $m add command -label "Grid 0.40u        " -command {magic::setgrid 0.4}
   $m add command -label "Grid 0.5u        " -command {magic::setgrid 0.5}
   $m add command -label "Grid 1.0u        " -command {magic::setgrid 1}
   $m add command -label "Grid 2.0u        " -command {magic::setgrid 2}
   $m add command -label "Grid 4.0u        " -command {magic::setgrid 4}
   $m add command -label "Grid 5.0u        " -command {magic::setgrid 5}
   $m add command -label "Grid 10.0u        " -command {magic::setgrid 10}
   $m add command -label "Grid 50.0u        " -command {magic::setgrid 50}
   $m add separator
   $m add command -label "Expand Toggle (/)" -command {magic::expand toggle}
   $m add command -label "Expand        (x)" -command {magic::expand }
   $m add command -label "Unexpand      (X)" -command {magic::unexpand }

   # Generate the options menu

   set m [menu ${framename}.titlebar.mbuttons.opts.optsmenu -tearoff 0]
   $m add check -label "Toolbar" -variable Winopts(${framename},toolbar) \
	-command [subst {if { \$Winopts(${framename},toolbar) } { \
		magic::maketoolbar ${framename} ; \
		grid ${framename}.toolbar -row 1 -column 2 -rowspan 2 -sticky new ; \
		} else { \
		grid forget ${framename}.toolbar } }]

   .winmenu add radio -label ${framename} -variable Opts(target) -value ${winname}
   if {$tk_version >= 8.5} {
     $m add check -label "Cell Manager" -variable Opts(cellmgr) \
	-command [subst { magic::cellmanager create; \
	if { \$Opts(cellmgr) } { \
	   wm deiconify .cellmgr ; raise .cellmgr \
	} else { \
	   wm withdraw .cellmgr } }]
      .winmenu entryconfigure last -command ".cellmgr.target.list configure \
         -text ${framename}"
   }

   $m add check -label "Tech Manager" -variable Opts(techmgr) \
	-command [subst { magic::techmanager create; \
		if { \$Opts(techmgr) } { \
		wm deiconify .techmgr ; raise .techmgr \
		} else { \
		wm withdraw .techmgr } }]

   $m add check -label "Netlist Window" -variable Opts(netlist) \
	-command [subst { if { \[windownames netlist\] != {}} { \
		   set Opts(netlist) 0 ; closewindow \[windownames netlist\] \
		} else { \
		   set Opts(netlist) 1 ; specialopen netlist \
		} }]

   $m add check -label "Colormap Window" -variable Opts(colormap) \
	-command [subst { if { \[windownames color\] != {}} { \
		   set Opts(colormap) 0 ; closewindow \[windownames color\] \
		} else { \
		   set Opts(colormap) 1 ; specialopen color \
		} }]

   $m add check -label "3D Display" -variable Opts(wind3d) \
	-command [subst { if { \[windownames wind3d\] != {}} { \
		   set Opts(wind3d) 0 ; .render.magic closewindow ; \
		   destroy .render \
		} else { \
		   set Opts(wind3d) 1 ; \
		   magic::render3d \[${winname} cellname list window\] \
		} }]

   $m add check -label "Window Command Entry" \
	-variable Winopts(${framename},cmdentry) \
	-command [subst { if { \$Winopts(${framename},cmdentry) } { \
		addcommandentry $framename \
		} else { \
		deletecommandentry $framename } }]

   $m add check -label "Crosshair" \
	-variable Opts(crosshair) \
	-command "if {$Opts(crosshair) == 0} {crosshair off}"

   catch {addmazehelper $m}

   # Set the default view

   update idletasks
   ${winname} magic::view

   magic::captions

   # Remove "open" and "close" macros so they don't generate non-GUI
   # windows or (worse) blow away the window inside the GUI frame
   if {[magic::macro list o] == "openwindow"} {
      magic::macro o "openwrapper"
   }
   if {[magic::macro list O] == "closewindow"} {
      magic::macro O {closewrapper $Opts(focus)}
   }

   # Make sure that closing from the window manager is equivalent to
   # the command "closewrapper"
   wm protocol ${framename} WM_DELETE_WINDOW "closewrapper $framename"

   # If the variable $Opts(callback) is defined, then attempt to execute it.
   catch {eval $Opts(callback)}

   # If the variable $Winopts(callback) is defined, then attempt to execute it.
   catch {eval $Winopts(${framename}, callback)}

   return ${winname}
}

# Delete the wrapper and the layout window in it.

proc magic::closewrapper { framename } {
   globa tk_version
   global Opts

   # Remove this window from the target list in .winmenu
   # (used by, e.g., cellmanager)

   if { $Opts(target) == "${framename}.magic" } {
      set Opts(target) "default"
      if {$tk_version >= 8.5} {
	 if {![catch {wm state .cellmgr}]} {
	    .cellmgr.target.list configure -text "default"
         } 
      }
   }

   set idx [.winmenu index $framename]
   .winmenu delete $idx

   ${framename}.magic magic::closewindow
   destroy $framename
}

# This procedure adds a command-line entry window to the bottom of
# a wrapper window (rudimentary functionality---incomplete)

proc magic::addcommandentry { framename } {
   if {![winfo exists ${framename}.eval]} {
      tkshell::YScrolled_Text ${framename}.eval -height 5
      tkshell::MakeEvaluator ${framename}.eval.text \
		"${framename}> " "${framename}.magic "
      tkshell::MainInit
   }
   set rc [grid size ${framename}]  
   set cols [lindex $rc 0]
   grid ${framename}.eval -row 3 -column 0 -columnspan $cols -sticky ew
   bind ${framename}.eval.text <Enter> {focus %W}
}

# Remove the command entry window from the bottom of a frame.

proc magic::deletecommandentry { framename } {
   grid forget ${framename}.eval
}

namespace import magic::openwrapper
puts "Use openwrapper to create a new GUI-based layout window"
namespace import magic::closewrapper
puts "Use closewrapper to remove a new GUI-based layout window"

# Create a simple wrapper for the 3D window. . . this can be
# greatly expanded upon.

proc magic::render3d {{cell ""}} {
   global Opts

   toplevel .render
   tkwait visibility .render
   magic::specialopen wind3d $cell .render.magic
   .render.magic cutbox box
   set Opts(cutbox) 1
   wm protocol .render WM_DELETE_WINDOW "set Opts(wind3d) 0 ; \
		.render.magic closewindow ; destroy .render"

   frame .render.title
   pack .render.title -expand true -fill x -side top
   checkbutton .render.title.cutbox -text "Cutbox" -variable Opts(cutbox) \
	-foreground white -background sienna4 -anchor w -padx 15 \
	-command [subst { if { \$Opts(cutbox) } { .render.magic cutbox box \
	} else { \
	.render.magic cutbox none } }]
	
   if {$cell == ""} {set cell default}
   label .render.title.msg -text "3D Rendering window  Cell: $cell" \
	-foreground white -background sienna4 -anchor w -padx 15
   pack .render.title.cutbox -side left
   pack .render.title.msg -side right -fill x -expand true
   pack .render.magic -expand true -fill both -side bottom
   bind .render.magic <Enter> {focus %W}
}

