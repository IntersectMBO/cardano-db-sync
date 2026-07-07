
# Tracer transformers


usually have a type 

```Haskell
transformer :: Tracer m b -> Tracer m a
```

applied as a contravariant functor they transform from a `Tracer m a` to a `Tracer m b`.

