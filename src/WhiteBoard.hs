{- |

WhiteBoard is a database of objects used to store intermediate
computations.  It is multi-threaded, can save and resume partial
calculations, and reuse calculations from previous runs automatically.

It works on the idea of a "push" system. You create an object and add
it to the whiteboard system, called an "anchor object", and this will
kick off a network of calculations. After the calcuations are
finished, you can read the results by querying the objects.

This works in contrast to a "pull" system, where you would run a
function and expect a result.

The reason for this is so that many different types of calculations
can be done in parallel, without the user having to wait for all of
them to finish before getting results. Another reason is that by
looking at which objects changed and which did not change after
running a calculation for one object, it can determine whether it
needs to rerun calculations on referred to objects (or even written to
objects), thereby saving a lot of work.

Each object in the system has a key associated with it, and refers to
other objects using that key. When an object is first created, it gets
a chance to read from and write / create other objects. Any object it writes
to, gets its own chance to do the same, recursively.

If an object that was read from, changes, the object that read it
becomes dirty, and the calculation for it is redone.

All old versions of objects are saved. The old versions can be
periodically cleared out.
-}

module WhiteBoard(module WhiteBoard.Core,
                  module WhiteBoard.Types) where

import WhiteBoard.Core
import WhiteBoard.Types
