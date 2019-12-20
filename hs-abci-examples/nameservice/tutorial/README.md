## Introduction

We're going to build an example application that mirrors the `golang` [cosmos-sdk](https://github.com/cosmos/cosmos-sdk) example application called [Nameservice](https://github.com/cosmos/sdk-tutorials/tree/master/nameservice). There is also a tutorial for that application which you can find [here](https://tutorials.cosmos.network/nameservice/tutorial/00-intro.html) for comparison.

## Contents
1. [Introduction](README.md)
    - [Application Specification](README.md#application-specification)
    - [How to Read this Tutorial](README.md#how-to-read-this-tutorial)
    - [Tutorial Goals](README.md#tutorial-goals)
2. Foundations
    - [Overview](Foundations/Overview.md)
    - [BaseApp](Foundations/BaseApp.md)
    - [Modules](Foundations/Modules.md)
        1. [Definition](Foundations/Modules.md#definition)
        2. [Composition](Foundations/Modules.md#composition)
3. Nameservice
    - [Overview](Tutorial/Nameservice/Overview.md)
    - [Types](Tutorial/Nameservice/Types.md)
        1. [Using A Typed Key Value Store](Tutorial/Nameservice/Types.md#using-a-typed-key-value-store)
        2. [Tutorial.Nameservice.Types](Tutorial/Nameservice/Types.md#tutorialnameservicetypes)
    - [Message](Tutorial/Nameservice/Message.md)
        1. [Message Types](Tutorial/Nameservice/Message.md#message-types)
        2. [Tutorial.Nameservice.Message](Tutorial/Nameservice/Message.md#tutorialnameservicemessage)
    - [Keeper](Tutorial/Nameservice/Keeper.md)
        1. [Definition](Tutorial/Nameservice/Keeper.md#definition)
        2. [Tutorial.Nameservice.Keeper](Tutorial/Nameservice/Keeper.md#tutorialnameservicekeeper)
    - [Query](Tutorial/Nameservice/Query.md)
        1. [Definition](Tutorial/Nameservice/Query.md#definition)
        2. [Tutorial.Nameservice.Query](Tutorial/Nameservice/Query.md#tutorialnameservicequery)
    - [Module](Tutorial/Nameservice/Module.md)
        1. [Tutorial.Nameservice.Module](Tutorial/Nameservice/Module.md#tutorialnameservicemodule)
    - [Application](Tutorial/Nameservice/Application.md)
        1. [From Modules To App](Tutorial/Nameservice/Application.md#from-modules-to-app)
        2. [Tutorial.Nameservice.Application](Tutorial/Nameservice/Application.md#tutorialnameserviceapplication)


## Application Specification
The Nameservice application is a simple marketplace for a name resolution service. Let us say that a `Name` resolves to type called `Whois` where 

~~~ haskell ignore
data Whois = Whois
  { whoisValue :: Text
  , whoisOwner :: Address
  , whoisPrice :: Amount
  }
~~~

This means that users can buy and sell entries in a shared mapping of type `Name -> Whois` where:
1. An unclaimed `Name` can be bought by a user and set to an arbitrary value.
2. Existing `(Name, Whois)` pairs can be updated by their owner or sold to a new owner for the price.
3. Existing `(Name, Whois)` pairs can be deleted by their owner and the owner receives a refund for the purchase price.

The application consists of three modules:
1. `Auth` - Manages accounts for users, things like nonces and token balances.
2. `Token` - Allows users manage their tokens, things like transfering or burning.
3. `Nameservice` - Controls the shared `Name -> Value` mapping described above.

## How to Read this Tutorial

This tutorial is largely written as a literate haskell file to simulate developing the Nameservice app from scratch. The file structure is similar to the actual app. We will partially develop a haskell module corresponding to what you find in the app, but possibly not the whole thing. Thus whenever we depend on a haskell module in the tutorial, rather than importing from the tutorial itself we will import from the app.

The benefit of this is that we don't have to develop the entire application in this tutorial. Any breaking changes in the app will (hopefully) break the tutorial and so if you can read this, the tutorial is correct.

## Tutorial Goals
The goal of this tutorial is to explain how the Nameservice app is constructed using the `hs-abci-sdk` package. Nameservice is a relatively simple but still non-trivial application.
If you would like to start with something simpler, you can view the tutorial for the [simple-storage](https://github.com/f-o-a-m/hs-abci/tree/master/hs-abci-examples/simple-storage) example application.

This tutorial should teach you:
1. How to construct application specific modules.
2. How to enable a module to receive application specific transactions. 
3. How to compose modules and wire up an application.
4. How to add event logging, console logging, and other effects to module.
4. How to use the type system to control the capabilities of a module.

The SDK makes heavy use of the effects system brought to haskell by the [polysemy](https://hackage.haskell.org/package/polysemy-1.2.3.0) library. We're not going to explain how this library works here, there are several existing tutorials that do this already. Suffice it to say that polysemy encourages the application developer to develop modules that have well defined roles and scopes, and to prohibit certain modules from interfering with the roles and scopes of other modules unless explicitly allowed by the type system. 

It is also allows the application developer to construct modules without much regard for how they will plug into the SDK, leaving that job to the SDK itself.

(This tutorial is integrated as a literate haskell file, meaning that the following is necessary to ensure it compiles.) 
~~~ haskell
main :: IO ()
main = pure ()
~~~

[Next: BaseApp](Foundations/Overview.md)
