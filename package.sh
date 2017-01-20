#!/bin/bash

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
#  o File name:   package.sh
#  o Description: Packaging script
#


VERSION=0.4
TMP_NUPRL=/tmp/nuprl
ABOVE=`(cd ..;pwd)`

echo "Packaging starting."
echo

# Clone repo to a tmp directory
echo " ------ Cloning repository..."
git clone -q ../. ${TMP_NUPRL}
echo

# Remove the papers dir from the copied repo
echo " ------ Removing unecessary files from cloned repository..."
rm -rf ${TMP_NUPRL}/eventml/papers
rm -rf ${TMP_NUPRL}/eventml/docs/mutex
rm -rf ${TMP_NUPRL}/eventml/docs/darpa
rm -rf ${TMP_NUPRL}/eventml/docs/nuprlprint
rm -rf ${TMP_NUPRL}/eventml/docs/paxos
rm -rf ${TMP_NUPRL}/eventml/tex/jbw*
rm -rf ${TMP_NUPRL}/eventml/evaluators/erlang
rm -rf ${TMP_NUPRL}/eventml/evaluators/scala
echo

# Rename the eventml dir so that it contains the version number
# and so that it is called eventml
echo " ------ Appending version number to the directory to package..."
mv ${TMP_NUPRL}/eventml ${TMP_NUPRL}/eventml-${VERSION}
echo

# Generate configure from configure.ac
echo " ------ Running autoconf..."
(cd ${TMP_NUPRL}/eventml-${VERSION}; autoconf)
echo

# Archive the eventml dir
echo " ------ Archiving directory..."
(cd ${TMP_NUPRL}; tar -czf eventml_${VERSION}-src.tar.gz eventml-${VERSION})
echo

# Move the archive to the repo
echo " ------ Moving the archive to ${ABOVE}..."
mv ${TMP_NUPRL}/eventml_${VERSION}-src.tar.gz ..
echo

# Remove the tmp copied repo
echo " ------ Removing the clones repository..."
rm -rf ${TMP_NUPRL}
echo

echo "Packaging done."
