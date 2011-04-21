#!/usr/bin/wish
#
# A Tcl shell in a text widget
# Brent Welch, from "Practical Programming in Tcl and Tk"
#

package provide tkshell 1.0

# "namespace eval" is needed to force the creation of the namespace,
# even if it doesn't actually evaluate anything.  Otherwise, the use
# of "proc tkshell::..." generates an undefined namespace error.

namespace eval tkshell {}

#-----------------------------------------------
# Create a simple text widget with a Y scrollbar
#-----------------------------------------------

proc tkshell::YScrolled_Text { f args } {
	frame $f
	eval {text $f.text -wrap none \
		-yscrollcommand [list $f.yscroll set]} $args
	scrollbar $f.yscroll -orient vertical \
		-command [list $f.text yview]
	::grid $f.text $f.yscroll -sticky news
	::grid rowconfigure $f 0 -weight 1
	::grid columnconfigure $f 0 -weight 1
	return $f.text
}

#-----------------------------------------------
# Add an X scrollbar
#-----------------------------------------------

proc tkshell::XScrolled_Text { f } {
        ::set t $f.text
	$t configure -xscrollcommand [list $f.xscroll set]
	catch {scrollbar $f.xscroll -orient horizontal \
		-command [list $t xview]}
	::grid $f.xscroll -sticky news
	return $t
}

#-----------------------------------------------
# Remove an X scrollbar
#-----------------------------------------------

proc tkshell::NoXScrolled_Text { f } {
 	::set t $f.text
	$t configure -xscrollcommand {}
	::grid forget $f.xscroll
}

#---------------------------------------------------------
# Create the shell window in Tk
#---------------------------------------------------------

proc tkshell::MakeEvaluator { {t .eval} {prompt "tcl> "} {prefix ""}} {
  global eval

  # Text tags give script output, command errors, command
  # results, and the prompt a different appearance

  $t tag configure prompt -foreground brown3
  $t tag configure result -foreground purple
  $t tag configure stderr -foreground red
  $t tag configure stdout -foreground blue

  # Insert the prompt and initialize the limit mark

  ::set eval(prompt) $prompt
  ::set eval(prefix) $prefix
  $t insert insert $eval(prompt) prompt
  $t mark set limit insert
  $t mark gravity limit left
  focus $t
  ::set eval(text) $t

  # Key bindings that limit input and eval things. The break in
  # the bindings skips the default Text binding for the event.

  bind $t <Return> {tkshell::EvalTypein ; break}
  bind $t <BackSpace> {
	if {[%W tag nextrange sel 1.0 end] != ""} {
		%W delete sel.first sel.last
	} elseif {[%W compare insert > limit]} {
		%W delete insert-1c
		%W see insert
	}
	break
  }
  bind $t <Key> {
	if [%W compare insert < limit] {
		%W mark set insert end
	}
  }
}

#-----------------------------------------------------------
# Evaluate everything between limit and end as a Tcl command
#-----------------------------------------------------------

proc tkshell::EvalTypein {} {
	global eval
	$eval(text) insert insert \n
	::set command [$eval(text) get limit end]
	if [info complete $command] {
		$eval(text) mark set limit insert
		tkshell::Eval $command
	}
}

#-----------------------------------------------------------
# Echo the command and evaluate it
#-----------------------------------------------------------

proc tkshell::EvalEcho {command} {
	global eval
	$eval(text) mark set insert end
	$eval(text) insert insert $command\n
	tkshell::Eval $command
}

#-----------------------------------------------------------
# Evaluate a command and display its result
#-----------------------------------------------------------

proc tkshell::Eval {command} {
	global eval
	$eval(text) mark set insert end
	::set fullcommand $eval(prefix)
	append fullcommand $command
	if [catch {interp eval $eval(slave) $fullcommand} result] {
		$eval(text) insert insert $result error
	} else {
		$eval(text) insert insert $result result
	}
	if {[$eval(text) compare insert != "insert linestart"]} {
		$eval(text) insert insert \n
	}
	$eval(text) insert insert $eval(prompt) prompt
	$eval(text) see insert
	$eval(text) mark set limit insert
	return
}

#-----------------------------------------------------------
# Create and initialize the slave interpreter
#-----------------------------------------------------------

proc tkshell::SlaveInit {slave} {
	global eval

	interp create $slave
	load {} Tk $slave
	interp alias $slave reset {} tkshell::ResetAlias $slave
	interp alias $slave puts {} tkshell::PutsAlias $slave
	::set eval(slave) $slave
	return $slave
}

#-----------------------------------------------------------
# Evaluate commands in the master interpreter
#-----------------------------------------------------------

proc tkshell::MainInit {} {
	global eval

	interp alias {} puts {} tkshell::PutsAlias {}
	::set eval(slave) {}
}
	

#-----------------------------------------------------------
# The "reset" alias deletes the slave and starts a new one
#-----------------------------------------------------------

proc tkshell::ResetAlias {slave} {
	interp delete $slave
	SlaveInit $slave
}

#--------------------------------------------------------------
# The "puts" alias puts stdout and stderr into the text widget
#--------------------------------------------------------------

proc tkshell::PutsAlias {slave args} {
	if {[llength $args] > 3} {
		error "invalid arguments"
	}
	::set newline "\n"
	if {[string match "-nonewline" [lindex $args 0]]} {
		::set newline ""
		::set args [lreplace $args 0 0]
	}
	if {[llength $args] == 1} {
		::set chan stdout
		::set string [lindex $args 0]$newline
	} else {
		::set chan [lindex $args 0]
		::set string [lindex $args 1]$newline
	}
	if [regexp (stdout|stderr) $chan] {
		global eval
		$eval(text) mark gravity limit right
		$eval(text) insert limit $string $chan
		$eval(text) see limit
		$eval(text) mark gravity limit left
	} else {
		puts -nonewline $chan $string
	}
}

#--------------------------------------------------------------
# A few lines is all that's needed to run this thing.
#--------------------------------------------------------------
# tkshell::YScrolled_Text .eval -width 90 -height 5
# pack .eval -fill both -expand true
# tkshell::MakeEvaluator .eval.text "magic> "
# tkshell::MainInit
#--------------------------------------------------------------
