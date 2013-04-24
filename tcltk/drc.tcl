#!/bin/tclsh
#----------------------------------------------
# Dump a file of DRC errors from magic
#----------------------------------------------
namespace path {::tcl::mathop ::tcl::mathfunc}

.layout1.magic update suspend
set fout [open "drc.out" w]
set oscale [cif lambda out]

select top cell
set origcell [cellname list self]
set celllist [drc list count]
foreach pair $celllist {
   set cellname [lindex $pair 0]
   set count [lindex $pair 1]
   load $cellname
   select top cell
   puts $fout "$cellname $count"
   puts $fout "----------------------------------------"
   for {set i 1} {$i <= $count} {incr i} {
      set drcwhy [drc list find $i]
      set bvals [box values]
      set bllx [* $oscale [lindex $bvals 0]]
      set blly [* $oscale [lindex $bvals 1]]
      set burx [* $oscale [lindex $bvals 2]]
      set bury [* $oscale [lindex $bvals 3]]
      set coords [format "%.3f %.3f %.3f %.3f" $bllx $blly $burx $bury]
      puts $fout "$coords \"$drcwhy\""
   }
   puts $fout "----------------------------------------"
}
close $fout
load $origcell
.layout1.magic update resume
