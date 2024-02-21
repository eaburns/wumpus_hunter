# Copyright (C) 2010 Ethan Burns
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
OCAMLMKTOP = ocamlmktop
OCAMLC = ocamlc -g
OCAMLDEP = ocamldep
OCAMLOPT = ocamlopt

ML_NOTOP =		\
	hunter.ml	\
	harness.ml

ML =			\
	prio_queue.ml	\
	world.ml	\
	knowledge.ml	\
	plan.ml


MLI =			\
	prio_queue.mli	\
	world.mli	\
	knowledge.mli	\
	plan.mli


CMX=$(ML:.ml=.cmx)
CMO=$(ML:.ml=.cmo)
CMI=$(MLI:.mli=.cmi)

all: .depend toplvl hunter harness

hunter: $(CMI) $(CMO) hunter.ml
	$(OCAMLC) unix.cma $(CMO) hunter.ml -o hunter

harness: $(CMI) $(CMX) harness.ml
	$(OCAMLOPT) unix.cmxa $(CMX) harness.ml -o harness

toplvl: $(CMI) $(CMO)
	$(OCAMLMKTOP) unix.cma $(CMO) -o toplvl

%.cmi: %.mli
	$(OCAMLC) -c $*.mli

%.cmo: %.ml
	$(OCAMLC) -c $*.ml

%.cmx: %.ml
	$(OCAMLOPT) -c $*.ml

.depend: $(ML) $(MLI) $(ML_NOTOP)
	$(OCAMLDEP) $(ML) $(MLI) > .depend

clean:
	rm -f harness
	rm -f hunter
	rm -f toplvl
	rm -f *.cmx *.cmo *.cmi *.o

-include .depend
