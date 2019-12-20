# Overview

The SDK relies heavily on two abstractions to facilitate application development, effects systems and Modules. 

## Effects Systems

The effects system is backed by a library called `polysemy` which we mentioned in the introduction. An application basically has three layers of effects

1. **Application level effects**: These are introduced by the application developer in order to customize application behavior.
2. **BaseApp effects**: These are effects into which you must interpret your application in order for it to be runnable by the SDK.
3. **Core effects**: These are largely internal and used to interpet the BaseApp effects to `IO`. There are a few different core options available in the SDK, but the more advanced developer might wish to use their own.

The tutorial explains the multiple points at which you can hook your application specific effects and types into the SDK.

## Modules

The core building block of an application is a Module. There are some modules that ship with the SDK and make up a kind of standard library. These modules are of general utility, like dealing with things like authentication or tokens, and are considered to be safe.

The most useful part of the SDK is that you are free to define your own modules, or depend on other third party modules outside the SDK. Since they all have the same type, they all easily fit into larger applications as standalone components or dependencies.
