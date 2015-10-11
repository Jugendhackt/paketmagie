# Quick documentation
## Building

	cabal sandbox init
	cabal install --only-dependencies
	cabal build

## Make it do something

	./demo.sh

Currently under developement. Might start an older function or might not work at all

## Main.hs
Takes a `startNode`, a `endNode` and `maxTicks` (maximum number of ticks the route should take). It reads a graph encoded in JSON from stdin, like `graph.json`. It prints a JSON encoded Graph to stdout.

## Graph.hs
The internals and the library Main.hs uses to calculate everything.
