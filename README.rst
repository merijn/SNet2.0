=========================================
 S-Net' - an alternative vision to S-Net
=========================================

:Author: Merijn Verstraaten

Overview
========

This software is a new implementation of the S-Net coordination system
( http://snet-home.org ). 

The S-Net vision is to offer composable, hierarchical coordination
between 3rd party software components to run them efficiently on
parallel hardware platforms.

The focus of the original ("main") S-Net project is to implement a
production-grade software infrastructure to run S-Net applications on
multi-core, shared memory architectures.

This software, SNet', preserves the original vision but instead
focuses on implementing a lightweight, easily maintainable
implementation suitable to perform research on distributed systems
(therefore with less emphasis on multi-cores). The justification for
this divergence is that future large many-core chips will behave more
like distributed systems, and the "interesting" issues of distributed
coordination are best researched using a higher-level framework.

Using the software
==================

Prerequisites
-------------

This program requires the following:
- GHC 7.6 or later (uses GHC extensions not available in earlier versions)
- c2hsc 0.6.2 or later (run ``cabal install c2hsc``)

Installation
------------

Note: c2hsc cannot be found "automatically" by this program's Cabal
script as it is a utility program
and not a library. Be sure to include the ``bin`` directory where
c2hsc is installed in your ``PATH`` environment variable. Typically,
c2hsc is installed in
``<HaskellBase>/ghc-<GHCVersion>/lib/c2hsc-<C2HSCVersion>/bin``.

The run from this directory: ``cabal install``.

Example program
---------------

(TBD)

