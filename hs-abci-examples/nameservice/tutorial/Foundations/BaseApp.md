# BaseApp

`BaseApp` is the set of effects that the SDK operates with and are freely available
for an application developer to make use of in any part of their application code. It is expected
(in fact required) that any application code can be rewritten in terms of the `BaseApp`
effects. Let's look at the `BaseApp` type:

~~~ haskell ignore
type BaseAppEffs =
  [ RawStore
  , Output Event
  , Logger
  , Resource
  , Error AppError
  ]
~~~

These effects are:

1. `RawStore` - allows for basic storage operations, e.g. get, put, delete, prove etc.
2. `Output Event` - allows for emitting events in the course of transaction processing.
3. `Logger` - allows for console loging with log levels.
4. `Resource` - allows for bracketing and resource management in the presence of exeptions.
5. `Error AppError` -- allows for errors of type `AppError` to be thrown or caught.

`BaseApp` acts as an intermediate effect system for specifying applications, it does not make any assumptions about how these effects will be interpreted at runtime. For example, `RawStore` could eventualy be interpeted by any persistent or in-memory storage capable of handling the commands `Put`, `Get` etc.

Most of the work in writing modules involves plugging into `BaseApp` at various points. For example, your module can create custom errors to throw or catch, but you must tell the SDK how to translate this custom error into an `AppError`. Likewise your module can define custom events to log during transaction execution, but you must describe to the SDK how to translate these custom events types into the type `Event`.

[Next: Modules](Modules.md)
