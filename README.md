# Synchron

**Synchron** is a *synchronous reactive concurrent programming* DSL for Haskell.

It is inspired by [**Céu**](http://www.ceu-lang.org) and [**Concur**](https://github.com/ajnsit/concur) and is meant to be an exploration of the synchronous paradigm in Haskell; it is by no means an efficient implementation and so far lacks a formal specification or operational semantics.

In contrast to e.g. FRP or the observer pattern, synchronous programming reifies the program's state machine implicitly in the program flow, as opposed to advancing execution through short lived, disconnected callbacks (which "eliminate any vestige of structured programming, such as support for long-lasting loops and automatic variables"<sup id="a1">[1](#f1)</sup>). Additionally, the execution model, contrary to actor based or multithreaded models, is deterministic and does not require synchronisation primitives<sup id="a2">[2](#f2)</sup>.

Especially [**Concur**](https://github.com/ajnsit/concur) has shown that the synchronous programming paradigm need not stay confined to the embedded or hard real time domain, as it seemingly has so far -- indeed, it lends itself extraordinarily well to programming UIs consisting of complex workflows and user interactions. So much so, that it's arguably one of the most interesting advancements in UI programming since Elm/React.

**Synchron** explores the synchronous paradigm in a more general setting.

## Example

In synchronous programming, reactions take no time with respect to the external environment, i.e. an event can both be caused and perceived at the same time.

**Synchron** allows for running multiple *trails* simultaneously at logical time steps in reaction to external or internal *events*. 

```
example :: Syn Int
example = local $ \e -> do              ➊
  (_, a) <- andd (emit e 5, await e)    ➋
  (_, b) <- andd (emit e 6, await e)    ➌
  pure (a + b)
```

`local` (➊) creates a new *internal* event (the difference between external and internal events will become clearer later). The `andd` (➋) combinator forks the execution and starts *both* trails (`emit e 5` and `await e`) *simultaneously* and only continues with the next line (➌) when *all* trails have finished.

`emit` fires the specified event with the given value, while `await` blocks until a given event fires.

`andd` is overloaded for tuples of arity up to 6, yielding tuples of the same arity in the `Syn` monad. This allows for trails with differing types, in contrast to `orr` (alternatively, `andd'` takes a monomorphic list if that is desired).

*TODO* State machine in data vs code; "reification of time"; "reinversion of control"

*TODO* Interesting and unexplored applications of synchronous programming

*TODO* Contrast with FRP

*TODO* External events

*TODO* Connectors

## Low level details

This section explains some low level technicalities of **Synchron** and can be skipped safely.

### Problems with Céu

From *The Céu Manual* (v0.30, p. 3):

> When multiple trails are active at a logical time (i.e. awaking from the same event), Céu schedules them in the order they appear in the program text. This policy is arbitrary, but provides a priority scheme for trails, and also ensures deterministic and reproducible execution for programs.

In practice, this means that the programs

```
local $ \e -> andd (await e, emit e 5)
```

and

```
local $ \e -> andd (emit e 5, await e)
```

are **not** equivalent. Due to **Céu**'s operational semantics, the former will terminate, as expected, while the latter will block forever on `await e`. In the context of Haskell this seems unintuitive and non-compositional: a trail definition, which might come from a completely different module or package, must be aware of (or alternatively, specify) the execution order in parallel statements to ensure correctness.

### Informal semantics

To combat this problem, **Synchron** takes a different route. Program execution is divided into three distinct phases:

* *Advance* - in this phase, program execution is advanced until either an `emit`, `await` or a parallel combination thereof (in `andd` or `orr`) is encountered. In other words, perform all straightforward statements, like `local` or `async`.
* *Gather* - gather all events and values from all current `emit` statements and combine with all currently firing external events. Don't advance the program further.
* *Unblock* - unblock all `await` statements affected by the firing events gathered in the previous phase. Advance all `emit` statements. Advance `orr` if at least one of its trails has finished. Advance `andd` if and only if all of its trails have finished.

The *advance - gather - unblock* phases are repeated in that order until the *unblock* phase can not advance the program further (at which point the program has either finished or is blocked). Although not formally proven, this execution model seems to facilitate trail order invariance in `andd` while remaining deterministic and reproducible.

## References

<span id="f1">[1]</span> F. Sant’Anna et al. [Structured Synchronous Reactive Programming with Céu](http://www.ceu-lang.org/chico/ceu_mod15_pre.pdf)

<span id="f2">[2]</span> [The Céu Manual v0.30](https://github.com/ceu-lang/ceu/blob/master/docs/manual/v0.30/ceu-v0.30.pdf)

