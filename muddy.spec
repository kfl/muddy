Distribution: IT University of Copenhagen
Vendor: IT University of Copenhagen
Packager: Jakob Lichtenberg <jl@itu.dk>
URL: http://www.itu.dk/research/muddy
Summary: MuDDy
Name: muddy
Version: 2.01
Release: 1
Source: muddy.tgz 
#Patch: muddy_libname.patch
Copyright: LGLP
Group: Development/Languages
BuildRoot: /tmp/%{name}-%{version}-root
%description
MuDDy is an ML interface to the Binary Decision Diagrams package BuDDy
written in C.

%package buddy
Summary: The BuDDy C library
Group: Development/Languages
%description buddy
MuDDy is an ML interface to the Binary Decision Diagrams package BuDDy
written in C.

%package ocaml
Summary: The Muddy O'Caml library
Group: Development/Languages
Requires: ocaml
%description ocaml
MuDDy is an ML interface to the Binary Decision Diagrams package BuDDy
written in C.

%package sml
Summary: The Muddy Moscow ML library
Group: Development/Languages
Requires: mosml
%description sml
MuDDy is an ML interface to the Binary Decision Diagrams package BuDDy
written in C.



%prep
%setup -n muddy
#%patch
# I am getting sick and tired of 'out of sync' patch files that I do not know where to put so sed is my friend...
cd muddy-sml
  mv MuddyCore.sml MuddyCore.sml.orig
  mv Makefile Makefile.orig
  sed "s/muddy\\.so/libmuddy\\.so/g" <MuddyCore.sml.orig >MuddyCore.sml
  sed "s/MUDDYLIBNAME=muddy/MUDDYLIBNAME=libmuddy/g" <Makefile.orig >Makefile
cd ..


%build
make

%install
rm -rf ${RPM_BUILD_ROOT}

# buddy:
mkdir -p ${RPM_BUILD_ROOT}/usr/include
mkdir -p ${RPM_BUILD_ROOT}/usr/lib
cd buddy/src
  cp bdd.h fdd.h bvec.h ${RPM_BUILD_ROOT}/usr/include
  cp libbdd.a -p ${RPM_BUILD_ROOT}/usr/lib
cd ../..

# muddy-sml:
mkdir -p ${RPM_BUILD_ROOT}/usr/mosml/lib
cd muddy-sml
  mkdir -p ${RPM_BUILD_ROOT}/usr/mosml/lib
  cp -a *.{ui,uo,sig} libmuddy.so ${RPM_BUILD_ROOT}/usr/mosml/lib
cd ..

# muddy-ocaml:
mkdir -p ${RPM_BUILD_ROOT}/usr/lib/ocaml
cd muddy-ocaml
  make MUDDYOCAML_INSTALLDIR=${RPM_BUILD_ROOT}/usr/lib/ocaml install
cd ..

%clean
#rm -rf ${RPM_BUILD_ROOT}

%post sml
/sbin/ldconfig

%postun sml
/sbin/ldconfig

%files
%defattr(-,root,root)
%doc LICENSE README TODO Changes ChangeLog

%files buddy
/usr/include/bdd.h
/usr/include/fdd.h
/usr/include/bvec.h
/usr/lib/libbdd.a
%doc buddy/README buddy/doc/bddnotes.ps buddy/doc/buddy.ps

%files sml
%doc muddy-sml/README
/usr/mosml/lib/bdd.sig
/usr/mosml/lib/bdd.ui
/usr/mosml/lib/bdd.uo
/usr/mosml/lib/bvec.sig
/usr/mosml/lib/bvec.ui
/usr/mosml/lib/bvec.uo
/usr/mosml/lib/fdd.sig
/usr/mosml/lib/fdd.ui
/usr/mosml/lib/fdd.uo
/usr/mosml/lib/libmuddy.so
/usr/mosml/lib/MuddyCore.ui
/usr/mosml/lib/MuddyCore.uo

%files ocaml
%doc muddy-ocaml/README
/usr/lib/ocaml/bdd.cmi
/usr/lib/ocaml/bdd.mli
/usr/lib/ocaml/fdd.cmi
/usr/lib/ocaml/fdd.mli
/usr/lib/ocaml/bvec.cmi
/usr/lib/ocaml/bvec.mli
/usr/lib/ocaml/libmuddy.a
/usr/lib/ocaml/muddy.cma
/usr/lib/ocaml/muddy.cmxa
/usr/lib/ocaml/muddy.a

%changelog

* Wed Jan 26 2002 Jakob Lichtenberg <jl@itu.dk>
- updated to 2.01 beta

* Wed Sep 26 2001 Jakob Lichtenberg <jl@itu.dk>
- updated to 2.0

* Thu May 3 2001 Jakob Lichtenberg <jl@itu.dk>
- updated to 1.9
