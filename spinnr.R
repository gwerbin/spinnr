
###############################################
#              _           ___                #
#    ___ ___  (_)__  ___  / _ \               #
#   (_-</ _ \/ / _ \/ _ \/ , _/               #
#  /___/ .__/_/_//_/_//_/_/|_|  0.2.0         #
#     /_/                                     #
#           Gregory Werbin                    #
#             github.com/gwerbin/spinnr       #
###############################################
#
# README 
#==============
#   spinnr is a package designed to parsimoniously track the
# progress of long-running loops and programs. The code is
# (loosely) based on utils::txtProgressBar(), but the
# syntax is much more concise. Instead of a progress bar,
# spinnr produces an ASCII image on one line that changes 
# in-place, making it possible to mimic effects like the 
# OSX "spinning beach ball of doom" while you're looping
# over data import steps, or have Kirby dance while you
# draw from an ill-conditioned posterior.
#
#   This is a functional, working release but it's probably
# buggy. In particular, this masks stats::step(), which I
# don't think should cause a problem on its own but could
# adversely interact with other packages. Feel free to send
# me a pull request on Github with any changes. Note that I
# haven't implemented any real error catching yet, and I
# can't say what might happen if you pass in the wrong data
# types.
# 
#   Future versions could include a wrapper for building
# progress bars out of spinners (a messy prospect right
# now) and a function to call a loop with a spinner
# or txtProgressBar built into the call, as in Hadley
# Wickham's excellent plyr package.
#
# INSTRUCTIONS
#==============
#
#   To "install," just run source() on this file. This is
# crude, and rm(list=ls()) will force you to source
# the file again. Eventually this will be a loadable
# package so you won't have to do that.
#
#   Basic use is simple. First use `sp <- spinner()` to
# create a new "spinner" object sp. Then, whenever you want
# to advance a frame in that spinner (e.g. every iteration of
# a loop), you just call `sp <- step(object)`. For example:
#
#     sp <- spinner()
#             # creates spinner sp
#     for( i in 1:30 ){ sp <- step.spinner(sp); Sys.sleep(0.2) }
#             # spins at 5 frames per second
#     close(sp)
#             # closes the spinner
#
#   Let's say you're not a fan of the "kirby" spinner. You'd
# rather watch him shake his pink butt for half an hour while
# Saint Gelman summons the ghost of Thomas Bayes to do your
# minimally-informative bidding. Then you can do:
#
#     newSpinner("kb2", c( "(  ,  )>" , "^(  ,  )^" , "<(  ,  )" ))
#
# and that will automatically create a new spinner style
# called "kb2" that you can use right away.
#
#   Alternatively, you think you have a better idea for the
# kirby spinner. Then you can do:
#
#     editSpinner("kirby", c( "(>\*-\*)>" , "^(\*-\*)^" , "<(\*-\*<)" )
#
# because you're pretty sure you just saw Matt Damon at
# Trader Joe's and you can't even think about finishing
# your paper anymore. Might as well head out to 1020 for
# a bit while this simulation runs.
#
#   Other features that I will document eventually
# include an argument to modify or turn off the percent
# progress display, and more powerful spinner-editing
# functions.
# 
# CHANGELOG
#==============
#
# 0.2.0
#   - added functionality to step() by a fraction,
#      e.g step(1/5) to step every 5th loop
#   - basic error catching: steps must be positive
#      (this restriction will be removed in the future)
#
# 0.1.0-alpha:
#   - initial release
#
# TO DO 
#==============
# - finish documentation and look into making a real package / requirements for CRAN
# - "lady-and-the-tramp" :-) @o@ (-:  :-)~@o@~(-:  :-)~~~~(-:  :-)~~(-: :-)~(-: :-**-:  
# - "dio" \m/ -m-
# - Figure out how to get Unicode fonts in RGUI
#
# Class `spinner` has following elements:
# getStep 	-- current step
# kill 	-- from pb
# pct		-- vector of current, min, max, by
#
# usage:
# initialize with spinner()
# update with step.spinner()
#
# SOURCE
#==============

step.default = stats::step
step = function(...) UseMethod("step")

cat(.maskedMsg("step", "‘package:stats’", by=F),"\n")

paste00 = function(...) paste0(...,collapse="")

.default.styles <- .spinner.styles <- list( "basic"=c( "-" , "\\" , "|" , "/" ),
		   		"kirby"=c( "(>\'-\')>" , "^(\'-\')^" , "<(\'-\'<)" )
			)

newSpinner = function( name, frames ){
	nc = nchar(frames)
	if( !all( nc == nc[1] ) ){
		.t = readline(sprintf("Not all frames are the same length. This could look ugly.\nAre you sure you want to add spinner ‘%s’? y/n: ",name))
		.t = grepl("^y",.t)
	} else .t = TRUE
	if( .t == TRUE ) .spinner.styles[[name]] <<- frames
	cat(sprintf("Created spinner ‘%s’."))
}

editSpinner = function( name, indices=NULL, newframes ){
	# TO ADD: args new_name and delete_old=TRUE, for copying/moving spinners
	nc_new = nchar(newframes)
	nc1 = nchar(.spinner.styles[["name"]][1])
	if( is.null(indices) ) indices = seq_along(.spinner.styles[["name"]])
	if( !all( nc_new == nc1 ) ){
		.t = readline(sprintf("Not all new frames are the same length as the old frame 1. This could look ugly.\nAre you sure you want to add spinner ‘%s’? y/n: ",name))
		.t = grepl("^y",.t)
	} else .t = readline(sprintf("Are you sure you want to edit spinner ‘%s’? y/n: ",name))
	if( .t == TRUE ) .spinner.styles[[name]][indices] <<- newframes
}

rmSpinner = function( name , force=FALSE ){
	.t = readline(sprintf("Are you sure you want to remove spinner ‘%s’? y/n: ",name))
	if( force==TRUE || grepl("^y",.t) ) .spinner.styles[[name]] <<- NULL
	else warn("Nothing changed.\n")
}

spinner = function(style="basic", initial=1, percent=TRUE, pmin=0, pmax=1, pby=NULL, pinitial=0, file=""){

	if(!identical(file, "") && !(inherits(file, "connection") && isOpen(file))) 
		stop("'file' must be \"\" or an open connection object")
	
	if( !style %in% seq_along(.spinner.styles) )
		style = pmatch(style,names(.spinner.styles))
	if( is.na(style) )
		style = 1

	if( is.null(pby) )
		pby = (pmax - pmin) / length(.spinner.styles[[style]])

	.killed = F
	kill = function(){
		if(!.killed){
			cat("\n", file = file)
			flush.console()
			.killed <<- TRUE
		}}

	if( percent==T ) percent = c(getPct=pinitial,pmin=pmin,pmax=pmax,pby=pby)

	structure( list(getStep = initial,
			    pct 	= percent,
			    kill 	= kill
			),
			style = names(.spinner.styles)[style],
			class = "spinner" )
}

step.spinner = function(.spinner_object, nsteps=1){
	if(nsteps<0) stop("nsteps must be non-negative (for now)")
	style = attr(.spinner_object,"style")
	spinner_frames = .spinner.styles[[style]]
	.n = length(spinner_frames)
	step_to = ( trunc(.spinner_object$getStep) + nsteps ) %% .n + 1

	if( !identical(.spinner_object$pct, FALSE) ){
		newpct = (.spinner_object$pct["getPct"] + .spinner_object$pct["pby"])
		newpct = newpct
		display_percent = round(100*newpct/.spinner_object$pct["pmax"],2)
		cat(sprintf( "\r%s    %s%%",spinner_frames[step_to],display_percent ))
		flush.console()
	} else{
		cat(sprintf( "\r%s",spinner_frames[step_to] ))
		flush.console()
	}

	.spinner_object$getStep <- .spinner_object$getStep + nsteps
	if( !identical(.spinner_object$pct, FALSE) )
		.spinner_object$pct["getPct"] <- newpct
	return(.spinner_object)
}
spinner() -> sp1

close.spinner = function( con, ... ){
    con$kill()
    invisible(NULL)
}



