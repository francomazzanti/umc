
UMC is a verification framework developed at the FM&T Laboratory of ISTI-CNR for the 
definition, exploration, analysis and model checking of system designs represented 
as a set of (UML) state machines.

UMC is one of the tools of the KandISTI verification framework.
See http://fmt.isti.cnr.it/kandisti/
See http://fmt.isti.cnr.it/umc

All the tools of the KandISTI framework are directly executable online through their
open-access web interface.

In these directories we make available the umc sources and their precompiled versions
for macOS and Linux systems.
These executables are the same used by the online versions, but when called from the
Unix command line allow a greater selection of comman-line options
(e.g.  see umc -v)

The source code is written in Ada2012, and to compile it is advisable to use
the free GNU/Adacore GNAT Ada compiler (see https://www.gnu.org/software/gnat).

Once downloaded and installed the compiler, the umc executable can be built by just
calling:  "gnatmake -O3 umc"

In the prebuilt-packages is also make available a complete macOS KandISTI application which
includes, beyond the command line umc related tools, also a local web server, that 
allows to use to full graphical (browser based) GUI to the verification environment.
To use all the graphical functionalities of this application you need to download and
install the Graphviz packgage from http://www.http://graphviz.org/ 
(http://www.graphviz.org/pub/graphviz/stable/macos/mountainlion/graphviz-2.36.0.pkg)

