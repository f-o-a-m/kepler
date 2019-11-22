# hs-abci-sdk

## Introduction
This package lays out an SDK for rapidly developing blockchain applications in haskell backed by the Tendermint replication engine. It relies on the [hs-abci-server](https://github.com/f-o-a-m/hs-abci/tree/master/hs-abci-server) to communicate to Tendermint core via the ABCI protocol.

## Requirements

### libsecp256k
You will need to have the `libsecp256k` `C` library installed on your machine to build this package, or anything depedning on it, as it is not statically linked to its haskell wrapper. You
can find instructions for this [here](https://github.com/f-o-a-m/hs-abci#libsecp256k1).

## Archetecture

The SDK makes heavy use of an effects system to separate different components of your application. Specifically it is using the [polysemy](https://hackage.haskell.org/package/polysemy) effects library in its implementation.

### BaseApp Effects

`BaseApp` is the set of effects that the SDK is written in. Every other module developed during the course of application development must eventually be compiled to this set of effects. As of now, `BaseApp` effects allows for things like access to storage, error handling, event logging, console logging, etc. 

### Application Specific Effects

It is assumed that you will want to define your own application specific effects, for example
in the way that the [Nameservice example app](https://github.com/f-o-a-m/hs-abci/tree/master/hs-abci-examples/nameservice) does. Application specific effects are useful for defining module level storage capabilities, custom errors and handling, and explicit dependencies on other modules' effects. There are many hooks in this SDK to facilitate compiling application effects
to `BaseApp`. For examples, see `Tendermint.SDK.Errors` or `Tendermint.SDK.Store`.

### Core Effects

The `CoreEffects` system is what's used to interpret `BaseApp` to `IO`, which is where the application must end up at eventually. It provides things like a loging context (e.g. `Katip`),
a context for executing database transactions, and various buffers and vars to facilitate ABCI message handling.

## Example Applications
There are currenlty two official example applications

1. [Simple Storage](https://github.com/f-o-a-m/hs-abci/tree/master/hs-abci-examples/simple-storage): This is a trivial application developed around a single module that allows get and set operations on an integer value.

2. [Nameservice](https://github.com/f-o-a-m/hs-abci/tree/master/hs-abci-examples/nameservice): This is an implementation of the official example application for the [cosmos-sdk](https://github.com/cosmos/sdk-tutorials/tree/master/nameservice). It is built to support a simple name resolution market place.
