# Copyright 2011 Cornell University
#
#
# This file is part of EventML - a tool aiming at specifying
# distributed protocols in an ML like language.  It is an interface
# to the logic of events and is compiled into Nuprl.  It is written
# by the NUPRL group of Cornell University, Ithaca, NY.
#
# EventML is a free software: you can redistribute it and/or modify it
# under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# EventML is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
# General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with EventML.  If not, see <http://www.gnu.org/licenses/>.
#
#  o Authors:     Vincent Rahli
#  o Affiliation: Cornell University, NUPRL group
#  o Date:        20 May 2011
#  o File name:   Makefile
#  o Description: Makefile for the EventML Emacs UI.
#


PREF="/usr/local"

emacs-ui:
	touch eventml.el
	echo "(defvar eventml-emacs-directory   \"${PREF}/share/emacs/site-lisp/eventml\")"  >  eventml.el
	echo "(defvar eventml-bin-directory     \"${PREF}/bin\")"                            >> eventml.el
	echo "(defvar eventml-lib-directory     \"${PREF}/lib/eventml\")"                    >> eventml.el
	echo "(defvar eventml-sources-directory nil)"                                        >> eventml.el
	echo "(load-file (expand-file-name \"eventml-mode.el\" eventml-emacs-directory))"    >> eventml.el
	echo "(load-file (expand-file-name \"eventml-main.el\" eventml-emacs-directory))"    >> eventml.el

clean:
	rm eventml.el

default: emacs-ui
