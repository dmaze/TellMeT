Random Implementation Notes
===========================

**Miso vs. Reflex vs. Other:** My last attempt to use Reflex ended in
a messy chain of "we don't do releases because the one blessed version
is in a nix bundle and you use nix, right?"  Googling the problem
suggests that Miso is a relative newcomer, with a simpler data model,
and the approachability and long-term challenges that brings.  It also
looks an awful lot like the Redux data model (which says something
about JavaScript land, including giving credit where it's due).  And
so...

**GHCJS versions:** Use _exactly_ the version specified in the [Stack
documentation](https://docs.haskellstack.org/en/stable/ghcjs/).  Past
experience is that nothing newer works.  (Miso, unlike Reflex, is on
Hackage, so you can add a current version of it in.)

**Template Haskell:** Trying to use Template Haskell results in a hung
GHCJS.  In particular this means that the `profunctors` package and
the full-blown `lens` package don't work; hence this using `microlens`.

**Components:** I call a "component" a prepackaged set of actions,
fractional state, reducer function, and view(s).  The views are pure
functions on the state; but the other parts are easiest to implement
with lenses.  In particular the actions would be best represented as
prisms (which in turn are most useful for decomposing sum types) but
microlens doesn't support them, so we do unholy things with traversals
instead.

**...and CPP:** Miso, irritatingly, defines GHC and GHCJS versions of
the library, and the "reducer" side of the loop is only defined on the
GHCJS side.  This means either moving the reducers into somewhere else
in the code ("cleanest" in some sense is moving the reducers into the
frontend implementation and out of the shared implementation), or to
use `{-# LANGUAGE CPP #-}` and only define the reducers `#ifdef
__GHCJS__`.  That feels mildly hacky, but much more navigable.
