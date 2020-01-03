# Synchron

*Synchron* is a synchronous programming DSL for Haskell. **Synchronous reactive concurrent programming** allows for running multiple *trails* simultaneously at logical time steps in reaction to external or internal *events*. The execution model, contrary to actor based or multithreaded models, is deterministic and does not require synchronisation primitives<sup id="a1">[1](#f1)</sup>.

*Synchron* is inspired by Ceu<sup id="a2">[2](#f2)</sup> and Concur<sup id="a3">[3](#f3)</sup> and is meant as an exploration of the synchronous paradigm in Haskell; it is by no means efficient and so far lacks a formal specification or operational semantics.

# Example

```
example :: Syn Int
example = local $ \e -> do              ➊
  (_, a) <- andd (emit e 5, await e)    ➋
  (_, b) <- andd (emit e 6, await e)    ➌
  pure (a + b)
```

`local` (➊) creates a new *internal* event (the difference between external and internal events will become clearer later). The `andd` (➋) combinator starts the execution of *both* trails (`emit e 5` and `await e`) *simultaneously* and continues only when *both* are finished -- `emit` fires the specified event with the given value, while `await` blocks until a given event fires.

# Problems with Ceu

# Bibliography

<b id="f1">1</b> [The Ceu Manual v0.30](https://github.com/ceu-lang/ceu/blob/master/docs/manual/v0.30/ceu-v0.30.pdf) [↩](#a1)

<b id="f2">2</b> [The Programming Language Ceu](http://www.ceu-lang.org) [↩](#a3)

<b id="f3">3</b> [An unusual Web UI Framework](https://github.com/ajnsit/concur) [↩](#a3)
