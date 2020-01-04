# Synchron

**Synchron** is a synchronous programming DSL for Haskell. *Synchronous reactive concurrent programming* allows for running multiple *trails* simultaneously at logical time steps in reaction to external or internal *events*. The execution model, contrary to actor based or multithreaded models, is deterministic and does not require synchronisation primitives<sup id="a1">[1](#f1)</sup>.

**Synchron** is inspired by **Ceu**<sup id="a2">[2](#f2)</sup> and **Concur**<sup id="a3">[3](#f3)</sup> and is meant as an exploration of the synchronous paradigm in Haskell; it is by no means an efficient implementation and so far lacks a formal specification or operational semantics.

# Example

```
example :: Syn Int
example = local $ \e -> do              ➊
  (_, a) <- andd (emit e 5, await e)    ➋
  (_, b) <- andd (emit e 6, await e)    ➌
  pure (a + b)
```

`local` (➊) creates a new *internal* event (the difference between external and internal events will become clearer later). The `andd` (➋) combinator forks the execution and starts *both* trails (`emit e 5` and `await e`) *simultaneously* and only continues with the next line (➌) when *all* trails are finished.

`emit` fires the specified event with the given value, while `await` blocks until a given event fires.

`andd` is overloaded for tuples of arity up to 6, yielding tuples of the same arity in the `Syn` monad. This allows for trails with differing types, in contrast to `orr` (alternatively, `andd'` takes a monomorphic list if that is desired).

# TODO Talk about state machine in data vs code; "reification of time"; "reinversion of control"

# External events

# Connectors

# Low level details

## Problems with Ceu

From *The Ceu Manual* (v0.30, p. 3):

> When multiple trails are active at a logical time (i.e. awaking from the same event), Céu schedules them in the order they appear in the program text. This policy is arbitrary, but provides a priority scheme for trails, and also ensures deterministic and reproducible execution for programs.

In practice this means that the programs

```
local $ \e -> andd (await e, emit e 5)
```

and

```
local $ \e -> andd (emit e 5, await e)
```

are **not** equivalent. Due to **Ceu**'s operational semantics, the former will terminate, as expected, while the latter will block forever on `await e`. In the context of Haskell this seems unintuitive and non-compositional: a trail definition, which might come from a completely different module or package, must be aware of (or alternatively, specify) the execution order in parallel statetements to ensure correctness.

## Informal semantics

To combat this problem, **Synchron** takes another route. Program execution is divided into three distinct phases:

* *Advance* - in this phase, program execution is advanced until either an `emit`, `await` or a parallel combinations thereof (i.e. `andd` and `orr`) is hit. In other words, execute all straightforward statements, like `local` or `async`.
* *Gather* - gather all events and values specified in all current `emit` statements, combined with all currently firing external events. Don't advance the program further.
* *Unblock* - unblock all `await` statements affected by the fired events gathered in the previous phase. Advance all `emit` statements. Advance parallel statements in the following manner:
  * `orr` - if one of the trails is finished, advance to next statement
  * `orr` - if all trails are finished, advance to next statement

The *advance - gather - unblock* phases are repeated in that order until the *unblock* phase can't advance the program further (at which point it's either finished or blocked).

# References

<span id="f1">[1]</span> [The Ceu Manual v0.30](https://github.com/ceu-lang/ceu/blob/master/docs/manual/v0.30/ceu-v0.30.pdf)

<span id="f2">[2]</span> [The Programming Language Ceu](http://www.ceu-lang.org)

<span id="f3">[3]</span> [Concur - An unusual Web UI Framework](https://github.com/ajnsit/concur)
