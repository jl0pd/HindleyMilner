# Hindley-Milner type inference

Hindley-Milner type inference algorithm based on [implementation](https://dysphoria.net/2009/06/28/hindley-milner-type-inference-in-scala/)
by Andrew Forrest, originally written in Scala.

This is almost one-to-one rewriting to F#.

## Building

### Dotnet

* net6 sdk is required, even though program can run on earlier version of dotnet.
Change `<TargetFramework>` tag inside project file if you have older sdk.

Type `dotnet run` in current directory to build and run program

### Scala

*I've never used Scala and haven't tried to run original code.
You'll need to find a way to run Scala code on your own*
