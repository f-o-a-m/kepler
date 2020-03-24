---
title: Foundations - Overview
---

# Overview

The SDK relies heavily on two abstractions to facilitate application development, effects systems and Modules. 

## Effects Systems

The effects system is backed by a library called `polysemy` which we mentioned in the introduction. An application basically has three layers of effects

1. **Application level effects**: These are introduced by the application developer in order to customize application behavior. Examples include things like `AuthEffs` from `Tendermint.SDK.Modules.Auth` that allow for manipulating accounts and throwing custom `Auth` errors.
2. **Transaction effects**: These are the effects that allow you to interpret transactions, emit events, meter gas, and handle storage requests.
3. **Base effects**: These include things like logging, metrics, exception handling, and some error handling.
4. **Store effects**: These are the effects that describe the possible interactions with an abstract merkelized key-value database.
5. **Core effects**: These are largely internal and used to interpet the other effects to `IO`. There are two different core options available in the SDK (distinguished by an in-memory versus production database), but the more advanced developer might wish to write their own.

The tutorial explains the multiple points at which you can hook your application specific effects and types into the SDK.

## Modules

The core building block of an application is a `Module`. There are some modules that ship with the SDK and make up a kind of standard library. These modules are of general utility, like dealing with things like authentication or coins, and are considered to be safe.

The most useful part of the SDK is that you are free to define your own modules, or depend on other third party modules outside the SDK. Since they all have the same type, they all easily compose into larger applications as standalone components or dependencies.
