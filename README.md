TellMeT
=======

This is an experimental project in full-stack Haskell, using the
[Miso](https://haskell-miso.org) framework for the UI.

This consumes and displays data files in the
[GTFS](https://developers.google.com/transit/gtfs/) format.  This
format ultimately comes down to zip file containing a set of an RFC
4180 comma-separated values (CSV) files with known names and formats.

Building
--------

See [BUILDING.md](./BUILDING.md).  Note that getting GHCJS set up can
be a little bit hairy, and doubly so when you need both a plain GHC
and GHCJS in the same environment.

Running
-------

Find a GTFS feed from your favorite transit agency.  For example, the
Massachusetts Bay Transportation Authority (Boston, MA, USA: "The T")
has [published GTFS data](https://www.mbta.com/developers/gtfs).
Google's GTFS documentation has [links to lists of GTFS
data](https://developers.google.com/transit/gtfs/community#other_resources)
as well.  Download, and do not unpack, the GTFS zip file.

Now run:

```sh
stack --stack-yaml server/stack.yaml exec -- \
  server -p 3003 -g MBTA_GTFS.zip
```

`-p` gives the port number to listen on, and defaults to 3003.  `-g`
gives the path to the GTFS zip file and is required.

Open a browser at `http://localhost:3003/` and enjoy.

Future
------

As mentioned initially, this is mostly intended as an experiment and
launching pad to see if I can build an interactive Web application
without using React/Redux and the corresponding mish-mash of
JavaScript tools.  (To which the answer seems to be, yes, but getting
the build chain installed is tricky, but if you ever looked under the
hood of your yarn/npm/babel/Flow/JSX stack then GHCJS is definitely
less alarming.)

As of this writing TellMeT only displays a simple route list fetched
via a REST API.  An obvious next step is to get it to display actual
schedules: the data is already there and just needs to be parsed and
served.

I have ambitions of also making this consume
[GTFS-Realtime](https://developers.google.com/transit/gtfs-realtime/)
Protobuf data, though that's much further out, and as a spare-time
project this may not get there.
