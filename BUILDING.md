Setting up a working ghcjs toolchain isn't totally trivial, in part
because ghcjs depends on sharing some tools from the native ghc.  This
sequence should come close to working:

```sh
stack --stack-yaml server/stack.yaml setup
stack --stack-yaml server/stack.yaml install cabal-install alex happy
stack --stack-yaml ui/stack.yaml setup
(cd webpack && yarn install)
make
```

(The last line of course depends on your architecture and the actual
candidate versions of GHC and this package, but it should also
tab-complete well.)

Also see the [stack
documentation](https://docs.haskellstack.org/en/stable/ghcjs/) on
stack-based GHCJS and dual-compiler builds.

Within this package:

* We try to keep the front- and back-end packages in sync, so if
  something doesn't work well within GHCJS, we'll avoid it on the
  back-end too.

* More specifically, Template Haskell and GHCJS don't play along, and
  this impacts the standard `lens` package.

* Only a somewhat older version of the `miso` browser framework is
  available in the Stackage LTS releases, so we get that directly from
  Hackage (also recommended in the Miso docs).

As of this writing the most recent working GHCJS Stack setup I know of
is the one at https://github.com/matchwood/ghcjs-stack-dist (which is
still somewhat behind current).  But, that specific build requires a
Cabal 2.x; you can add it to the `server/stack.yaml`, install it, and
then run

```sh
cp ~/.local/bin/cabal $(stack --stack-yaml server/stack.yaml path --snapshot-install-root)/bin
```

to make it actually available to the GHCJS build; but then that build
hangs trying to compile Template Haskell in Aeson, at which point you
give up and just write out a large number of dependencies in the
`stack.yaml` files.

There is a pre-release version of the Servant client that uses the
GHCJS XMLHttpRequest scheme, but trying to use it will send you down
this path.  (You need the git version of Servant, which needs a bunch
of newer libraries, so a newer resolver looks good, but...)
