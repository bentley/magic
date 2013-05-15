# Text helper window

proc magic::make_texthelper { mgrpath } {
   global typedflt typesticky
   toplevel ${mgrpath}
   wm withdraw ${mgrpath}

   frame ${mgrpath}.title
   frame ${mgrpath}.text
   frame ${mgrpath}.font
   frame ${mgrpath}.size
   frame ${mgrpath}.just
   frame ${mgrpath}.rotate
   frame ${mgrpath}.offset
   frame ${mgrpath}.layer

   frame ${mgrpath}.buttonbar

   label ${mgrpath}.title.tlab -text "New label: "

   label ${mgrpath}.text.tlab -text "Text string: "
   label ${mgrpath}.font.tlab -text "Font: "
   label ${mgrpath}.size.tlab -text "Size: "
   label ${mgrpath}.just.tlab -text "Justification: "
   label ${mgrpath}.rotate.tlab -text "Rotation: "
   label ${mgrpath}.offset.tlab -text "Offset from reference: "
   label ${mgrpath}.layer.tlab -text "Attach to layer: "

   entry ${mgrpath}.text.tent -background white
   entry ${mgrpath}.size.tent -background white
   entry ${mgrpath}.rotate.tent -background white
   entry ${mgrpath}.offset.tent -background white
   entry ${mgrpath}.layer.tent -background white

   set typedflt 1
   set typesticky 0
   checkbutton ${mgrpath}.layer.btn1 -text "default" -variable typedflt \
	-command [subst {if {\$typedflt} {pack forget ${mgrpath}.layer.tent \
		} else {pack forget ${mgrpath}.layer.btn2; \
		pack ${mgrpath}.layer.tent -side left -fill x -expand true; \
		pack ${mgrpath}.layer.btn2 -side left}}]
   checkbutton ${mgrpath}.layer.btn2 -text "sticky" -variable typesticky

   menubutton ${mgrpath}.just.btn -text "default" -menu ${mgrpath}.just.btn.menu
   menubutton ${mgrpath}.font.btn -text "default" -menu ${mgrpath}.font.btn.menu
   
   button ${mgrpath}.buttonbar.cancel -text "Cancel" -command "wm withdraw ${mgrpath}"
   button ${mgrpath}.buttonbar.apply -text "Apply"

   pack ${mgrpath}.title.tlab
   pack ${mgrpath}.text.tlab -side left
   pack ${mgrpath}.text.tent -side right -fill x -expand true
   pack ${mgrpath}.font.tlab -side left
   pack ${mgrpath}.font.btn -side left
   pack ${mgrpath}.size.tlab -side left
   pack ${mgrpath}.size.tent -side right -fill x -expand true
   pack ${mgrpath}.just.tlab -side left
   pack ${mgrpath}.just.btn -side left
   pack ${mgrpath}.rotate.tlab -side left
   pack ${mgrpath}.rotate.tent -side right -fill x -expand true
   pack ${mgrpath}.offset.tlab -side left
   pack ${mgrpath}.offset.tent -side right -fill x -expand true
   pack ${mgrpath}.layer.tlab -side left
   pack ${mgrpath}.layer.btn1 -side left
   pack ${mgrpath}.layer.btn2 -side left
   pack ${mgrpath}.buttonbar.apply -side left
   pack ${mgrpath}.buttonbar.cancel -side right

   pack ${mgrpath}.title -side top
   pack ${mgrpath}.text -side top -anchor w -expand true
   pack ${mgrpath}.font -side top -anchor w
   pack ${mgrpath}.size -side top -anchor w -expand true
   pack ${mgrpath}.just -side top -anchor w
   pack ${mgrpath}.rotate -side top -anchor w -expand true
   pack ${mgrpath}.offset -side top -anchor w -expand true
   pack ${mgrpath}.layer -side top -anchor w -expand true
   pack ${mgrpath}.buttonbar -side bottom -fill x -expand true

   # Create menus for Font and Justification records

   menu ${mgrpath}.just.btn.menu -tearoff 0
   menu ${mgrpath}.font.btn.menu -tearoff 0

   ${mgrpath}.just.btn.menu add command -label "default" -command \
		"${mgrpath}.just.btn config -text default"
   ${mgrpath}.just.btn.menu add command -label "N" -command \
		"${mgrpath}.just.btn config -text N"
   ${mgrpath}.just.btn.menu add command -label "NE" -command \
		"${mgrpath}.just.btn config -text NE"
   ${mgrpath}.just.btn.menu add command -label "E" -command \
		"${mgrpath}.just.btn config -text E"
   ${mgrpath}.just.btn.menu add command -label "SE" -command \
		"${mgrpath}.just.btn config -text SE"
   ${mgrpath}.just.btn.menu add command -label "S" -command \
		"${mgrpath}.just.btn config -text S"
   ${mgrpath}.just.btn.menu add command -label "SW" -command \
		"${mgrpath}.just.btn config -text SW"
   ${mgrpath}.just.btn.menu add command -label "W" -command \
		"${mgrpath}.just.btn config -text W"
   ${mgrpath}.just.btn.menu add command -label "NW" -command \
		"${mgrpath}.just.btn config -text NW"
   ${mgrpath}.just.btn.menu add command -label "center" -command \
		"${mgrpath}.just.btn config -text center"

   set flist [magic::setlabel fontlist]
   ${mgrpath}.font.btn.menu add command -label default -command \
		"${mgrpath}.font.btn config -text default"
   foreach i $flist {
      ${mgrpath}.font.btn.menu add command -label $i -command \
		"${mgrpath}.font.btn config -text $i"
   }

   # Set up tag callbacks

   magic::tag select "[magic::tag select]; magic::update_texthelper"
}

proc magic::analyze_labels {} {
   global typedflt typesticky
   set tlist [lsort -uniq [setlabel text]]
   set jlist [lsort -uniq [setlabel justify]]
   set flist [lsort -uniq [setlabel font]]
   set slist [lsort -uniq [setlabel size]]
   set rlist [lsort -uniq [setlabel rotate]]
   set olist [lsort -uniq [setlabel offset]]
   set llist [lsort -uniq [setlabel layer]]
   set klist [lsort -uniq [setlabel sticky]]

   .texthelper.text.tent delete 0 end
   if {[llength $tlist] == 1} {
      .texthelper.text.tent insert 0 $tlist
   }
   if {[llength $jlist] == 1} {
      set jbtn [string map {NORTH N WEST W SOUTH S EAST E CENTER center} $jlist]
      .texthelper.just.btn.menu invoke $jbtn
   } else {
      .texthelper.just.btn config -text ""
   }
   if {[llength $flist] == 1} {
      .texthelper.font.btn.menu invoke $flist
   } else {
      .texthelper.font.btn config -text ""
   }
   .texthelper.size.tent delete 0 end
   if {[llength $slist] == 1} {
      .texthelper.size.tent insert 0 $slist
   }
   .texthelper.offset.tent delete 0 end
   if {[llength $olist] == 1} {
      .texthelper.offset.tent insert 0 [join $olist]
   }
   .texthelper.rotate.tent delete 0 end
   if {[llength $rlist] == 1} {
      .texthelper.rotate.tent insert 0 $rlist
   }
   .texthelper.layer.tent delete 0 end
   if {[llength $llist] == 1} {
      .texthelper.layer.tent insert 0 $llist
   }
   if {[llength $klist] == 1} {
      set typesticky $klist
   }
}

proc magic::change_label {} {
   global typedflt typesticky
   set ltext [.texthelper.text.tent get]
   set lfont [.texthelper.font.btn cget -text]
   set lsize [.texthelper.size.tent get]
   set lrot  [.texthelper.rotate.tent get]
   set loff  [.texthelper.offset.tent get]
   set ljust [.texthelper.just.btn cget -text]
   set ltype [.texthelper.layer.tent get]

   if {$ltext != ""} {
      setlabel text $ltext
   }
   if {$lfont != ""} {
      if {$lfont == "default"} {
         setlabel font -1
      } else {
         setlabel font $lfont
      }
   }
   if {$ljust != ""} {
      if {$ljust == "default"} {
         setlabel justify -1
      } else {
         setlabel justify $ljust
      }
   }
   if {$lsize != ""} {
      setlabel size ${lsize}i
   }
   if {$loff != ""} {
      set oldsnap [snap list]
      snap internal
      setlabel offset [join $loff]
      snap $oldsnap
   }
   if {$lrot != ""} {
      setlabel rotate $lrot
   }
   if {$ltype != ""} {
      setlabel layer $ltype
   }
   setlabel sticky $typesticky
}

proc magic::make_new_label {} {
   global typedflt typesticky
   set ltext [.texthelper.text.tent get]
   set lfont [.texthelper.font.btn cget -text]
   set lsize [.texthelper.size.tent get]
   set lrot  [.texthelper.rotate.tent get]
   set loff  [.texthelper.offset.tent get]
   set ljust [.texthelper.just.btn cget -text]
   set ltype [.texthelper.layer.tent get]

   if {$ltext == ""} return	;# don't generate null label strings!

   if {$lfont == {} || $lfont == "default"} {
      label $ltext $ljust
      return
   }

   if {$lsize == "0" || $lsize == ""} {set lsize 1}
   if {$lrot == ""} {set lrot 0}
   if {$loff == ""} {set loff "0 0"}

   if {$typedflt == 1 || $ltype == ""} {
      if {$ljust == "default"} {
         label $ltext $lfont $lsize $lrot [join $loff]
      } else {
         label $ltext $lfont $lsize $lrot [join $loff] $ljust
      }
   } else {
      if {$typesticky == 1} {set ltype "-$ltype"}
      if {$ljust == "default"} {
         label $ltext $lfont $lsize $lrot [join $loff] center $ltype
      } else {
         label $ltext $lfont $lsize $lrot [join $loff] $ljust $ltype
      }
   }

   # puts stdout "label $ltext $lfont $lsize $lrot $loff $ljust"
}

#--------------------------------------------------------------------
# Update and redisplay the text helper
#--------------------------------------------------------------------

proc magic::update_texthelper {} {
   global CAD_ROOT

   if {[catch {wm state .texthelper}]} {
      magic::make_texthelper .texthelper
   }

   # Update:  Determine if a label has been selected or not.  If so,
   # analyze the label list to determine which properties are common
   # to all the selected labels

   set slist [lindex [what -list] 1]
   if {$slist == {}} {
      .texthelper.title.tlab configure -text "New label: "
      .texthelper.text.tent delete 0 end
      .texthelper.font.btn.menu invoke 0
      .texthelper.just.btn.menu invoke 0
      .texthelper.size.tent delete 0 end
      .texthelper.size.tent insert 0 "1"
      .texthelper.rotate.tent delete 0 end
      .texthelper.rotate.tent insert 0 "0"
      .texthelper.offset.tent delete 0 end
      .texthelper.offset.tent insert 0 "0 1"
      .texthelper.buttonbar.apply configure -command magic::make_new_label
   } else {
      if {[llength $slist] == 1} {
         .texthelper.title.tlab configure -text "Selected label: "
      } else {
         .texthelper.title.tlab configure -text "Selected labels: "
      }
      magic::analyze_labels
      .texthelper.buttonbar.apply configure -command magic::change_label
   }
}

