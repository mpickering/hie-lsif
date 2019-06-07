This repo implements a LSIF indexed for Haskell source files.

It relies on HIE files which will be released in GHC 8.8.

What works:

1. References and definitions
2. Indexing for a single complete project
3. Hover to show types

Things which are not implemented yet and their status are:

1. Cross-project references (because I don't understand the spec)
   https://github.com/Microsoft/language-server-protocol/issues/680
2. `textDocument/documentSymbol` just TODO
  https://microsoft.github.io/language-server-protocol/specification#textDocument_documentSymbol
3. `textDocument/foldingRange` -  This is easy when you decide what the
   folding ranges should be. I just don't really like the feature so haven't
   done it yet.
4. `textDocument/documentLink` - I'm not sure how you would implement this.
  https://microsoft.github.io/language-server-protocol/specification#textDocument_documentLink
5. Work out what the difference between definition/declaration is
6. Work out how type definition is different to just normal definition.
7. Work out how implementation is different to declaration to definition.


Other improvements that have to be made.

1. It might be good to try to compress the JSON output, the JSON output for
the groups example is about 25000 lines long for a 100 line text file.
This will surely be a problem when trying to index a big project.
This can be done by inlining certain edges but I thought the whole point
was not to buffer anything.

2. ~Switch to Zubin's encoding. This will make it easier to generate the right
thing and also make 1. easier probably.~

3. Set up a nix example which turns on `-fwrite-interface-file` and then
  generates the lsif.json file for the project.

4. Integrate LSIF generation into hadrian.

# Instructions about how to use

The integration is not very good yet but if you're brave... The instructions
are mainly here so that I can make them simpler.

Pre-requestite: A suffifiently new version of GHC (anything called 8.7 will
probably work), I use
[ghc-artefact-nix](https://github.com/mpickering/ghc-artefact-nix) to enter a
shell with a recent enough version.

You will need to generate some `.hie` files by compiling a file with the `-fwrite-ide-info`
command.

Then running `cabal new-run exe:hie-lsif` will produce a file called
`test.json`. This is the LSIF file.

## Commandline arguments

`hie-lsif` accepts the following arguments

- `--root <dir>`: specify the directory all the paths in the hie files are
  to be considered relative too. This is usually the root directory of
  your project, or the working directory of ghc when it is used to generate
  the hie files

  Defaults to the current working directory
- `--hie <dir>`: the location of the directory where the hie files are stored

  Defaults to the current working directory
- `--include-contents`: flag which controls whether the contents of the haskell
  source files(as stored in `.hie` files) should be included in the generated
  lsif file.

## Using the file

First, copy the `test.json` file to the root of the project you want to use it
with. Rename it to `lsif.json`.

You probably should open the file and check the paths are correct in it to
the source file as the language server will not do anything unless they are
right. All the paths will be at the start of the file.

Then, you have to install the LSIF language server from my fork so that
it starts up with Haskell files.  https://github.com/mpickering/vscode-lsif-extension
There are instructions in the README there on how to do that.

Once that is installed, in vscode, open the folder where the source files and the
`lsif.json` files are located.

Things to note about the language server:

1. It only reads the lsif.json file once on startup. If you change the
lsif.json file you have to close and reopen the folder for it to read the new
changes.
2. You can't use a symlink for the lsif.json file.

It can be useful to open the debugging pane to see what the language
server is doing, if anything.

## Using the file in the editor

If you hover over an identifier now, it should show you the type of the
identifier. If you right click on an identifier you should be able to find
all references, jump to definition and so on.

