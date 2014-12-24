# maintain a hidden .dirhist file which holds the directory stack
# if(missing(path))  allow the "top two swap" feature
# if(rot) will do rotation of stack
# and if(!dn), then  setwd(path) 
pushd <- function(path, dn=FALSE,rot=0) { 
# store where we are before moving
newhist<-getwd() #the full path
if(missing(path) && !dn &&!rot ) {
# just swap the top two directories
	if(exists('.dirhist') && length(.dirhist)>1) .dirhist[1:2]<-.dirhist[2:1]
	return(invisible(0))
	}
if (!missing(path) && !dn)  {
	setwd(path) 
	}
#pretzel logic to get a value into ddirhist
if(exists('.dirhist') ) 	ddirhist<-.dirhist  else ddirhist<-vector()
if( !dn ) ddirhist <- c(newhist,ddirhist) 
	
# bash adds directory to stack when dn is true
# I believe this next stacked "if" does what bash does, i.e. don't add current
# path to stack if just going to rotate
# looks like bash does change dir when rot!=0 so long as dn is false
	# control how "far" to rotate stack
#Don't rotate if stack is 1 or 0
if(exists('ddirhist') && length(ddirhist)>1 && rot) {
	rot <- rot%%length(ddirhist)
	ddirhist[] <- ddirhist[ c( (rot+1):length(ddirhist),1:(rot) ) ]
# don't need this here: 	.dirhist<<-ddirhist
} 
if( (!exists('ddirhist') ||(exists('ddirhist')&&length(ddirhist)<2)) &&rot ) {
	#just a warning if rot was requested
	warning('Nothing to rotate.')
}
#always write something...
 .dirhist<<-ddirhist
return(invisible(0)) # status value
}

###
# arg "pull" is like +/-n in bash version, to remove items from stack
popd <- function(dn=FALSE, pull=0) { 
if( !(exists('.dirhist')) || length(.dirhist)==0 ) {
	print('Nothing to pop.')
	return(invisible(1)) # an error value
	}
if(!dn & !pull ) {
	setwd(.dirhist[1])
	.dirhist<<- .dirhist[-1]
	}
#  
# In bash-land, looks like any "pull" value suppresses changing directory,
# not quite what the documentation claims. Could change this by
# adding an else here, with !dn allowing a setwd() to happen
if(pull) {
	# "fix" negative values
	if(pull<0) pull <- length(.dirhist)+pull
	.dirhist<<-.dirhist[-pull]
	}
return(invisible(0)) #status value
}

############ bash documentation (from cygwin)
  # Adds  a  directory to the top of the directory stack, or rotates
              # the stack, making the new top of the stack the  current  working
              # directory.  With no arguments, exchanges the top two directories
              # and returns 0, unless the directory stack is empty.   Arguments,
              # if supplied, have the following meanings:
              # -n     Suppresses  the  normal  change  of directory when adding
                     # directories to the stack,  so  that  only  the  stack  is
                     # manipulated.
              # +n     Rotates  the  stack  so  that the nth directory (counting
                     # from the left of the list shown by  dirs,  starting  with
                     # zero) is at the top.
              # -n     Rotates  the  stack  so  that the nth directory (counting
                     # from the right of the list shown by dirs,  starting  with
                     # zero) is at the top.
              # dir    Adds dir to the directory stack at the top, making it the
                     # new current working directory.
# popd [-n] [+n] [-n]
              # Removes entries from the directory stack.   With  no  arguments,
              # removes  the  top directory from the stack, and performs a cd to
              # the new top directory.  Arguments, if supplied, have the follow‐
              # ing meanings:
              # -n     Suppresses  the  normal change of directory when removing
                     # directories from the stack, so that  only  the  stack  is
                     # manipulated.
              # +n     Removes  the nth entry counting from the left of the list
                     # shown by dirs, starting with zero.  For  example:  ``popd
                     # +0'' removes the first directory, ``popd +1'' the second.
              # -n     Removes the nth entry counting from the right of the list
                     # shown by dirs, starting with zero.  For  example:  ``popd
                     # -0''  removes the last directory, ``popd -1'' the next to
                     # last.
