## reflex-stripe

This is an integration between [Stripe Elements](https://stripe.com/docs/elements) and [Reflex-DOM](https://github.com/reflex-frp/reflex-dom) so you can use Stripe Elements within a Reflex-DOM application.

### Troubles

Stripe Elements likes to inject hidden `iframe`s into your DOM and they are critically important. What's more, it doesn't need all of them all the time, and the error messaging is poor, so if anything bad happens to them the error messages might be totally inscrutable, such as:

* Blocked frame from accessing frame with other origin.
* undefined in `p.forEach`

To work around this, make sure that the `body` element contents are never replaced directly by Reflex. To do this, don't use `mainWidget` but instead create a top-level `div` or similar and use `attachWidget`, as done in the example code. In a future release, it would be nice to inject the stripe.js script tag after initial build to avoid this problem, but so far this isn't done.

### Reflex-DOM fork warning

As of writing, this depends on a [fork of Reflex-DOM](https://github.com/ConferHealth/reflex-dom) which has not yet been [upstreamed](https://github.com/reflex-frp/reflex-dom/pull/152). If you see complaints about missing `getMountStatus` / `MonadMountStatus`, that's why.

## Contributing

Contributions and feedback welcome! File and issue or make a PR.

## Chat

Asa (@asa) and Ross (@dridus) hang out on [fpchat](https://fpchat.com). Ross also hangs out in #reflex-dom on Freenode.

