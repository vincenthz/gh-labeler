# gh-labeler

This is a simple CLI tool to create, delete and synchronise labels
on a github repo.

## Install

The simplest method is to install `stack` and then:

```
stack install gh-labeler
```

## Usage

Create a github OAuth token with the `public_repo` priviledge (in `repo` section),
and copy the token into a file in your HOME directory called `.gh-labeler`.

Then:

* Listing labels:

```
gh-labeler vincenthz gh-labeler list
```

* Create a label

```
gh-labeler vincenthz gh-labeler create myLabel eeb2d3
```

* Delete labels

```
gh-labeler vincenthz gh-labeler delete myLabel myLabel2
```

* Rename labels

```
gh-labeler vincenthz gh-labeler rename myOldName myNewName myOld2 myNew2
```

* Synchronise from a file (See Example label file for the format):

```
gh-labeler vincenthz gh-labeler sync my-labels.txt
```

## Caveats

* The tools doesn't yet support label description, since the underlying library doesn't support them.

## Example label file

Very simple syntax of `<color> <name>`:

Example:

```
b60205 B - Bug
0e8a16 D - easy
d93f0b D - hard
fbca04 D - medium
006b75 P - high
006b75 P - low
cccccc R - duplicate
cccccc R - invalid
cccccc R - wontfix
4ef47d X - WIP
fef2c0 X - code-structure
fef2c0 X - for-discussion
fef2c0 X - help wanted
fef2c0 X - question
fef2c0 X - voting
```
