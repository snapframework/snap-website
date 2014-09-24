| title: Announcing: Heist v0.14
| author: Doug Beardsley <mightybyte@gmail.com>
| published: 2014-09-24T06:10:00-0400
| updated:   2014-09-24T06:10:00-0400
| summary: Release notes for Heist 0.14

The Snap team is happy to announce the release of version 0.14 of Heist.  

## Major Changes

### Namespace Support

Heist now has support for namespaces.  This allows you to configure Heist so
that all of your splices require a namespace.  Requiring a namespace allows
Heist to be more aggressive with errors for unbound splices.  For instance,
imagine you set the hcNamespace field in your HeistConfig to "h", and you bind
two splices.

``` haskell
mySplices = do
    "foo" #! fooSplice
    "bar" #! barSplice
```

With this setup, you would put the "h" namespace on all of the splice tags in
your templates.  Instead of calling those splices with "&lt;foo&gt;" and
"&lt;bar&gt;", you would use "&lt;h:foo&gt;" and "&lt;h:bar&gt;".  So why go
to all this trouble so you have to type more?  Because it allows Heist to do
more error checking.  Without namespaces there is no way for Heist to know
whether a tag is supposed to be a splice or not.  We could use the list of
valid HTML tags to infer it, but that significantly constrains the scope of
what we intended for Heist.  This approach allows the user to be explicit
about which tags should be splices.  If you do not want to use namespaces, set
the namespace to the empty string.

Along with the namespace field, we introduced the `hcErrorNotBound` for
controlling the error checking.  When `hcErrorNotBound` is `True`, Heist will
generate an error message if it encounters any namespaced tags that do not
have splices bound for them.  This eliminates a large class of bugs where
users were using a splice in their templates, but forgot to bind it in their
code.  The intent is that users will usually want to have this error checking
turned on.  But we felt it was also important to be able to use namespacing
without the strict enforcement, so we included this flag to give users full
control.

### Generalized Error Reporting

Since this release is doing more error checking, we decided to expose error
facilities to the user.  This release exposes a new function `tellSpliceError`
that you can use when error conditions are detected in your splices.  If you
are using compiled Heist, then all your templates will be processed up front
at load time.  If any of your load time or compiled splices detect an error
condition that the user needs to fix, you can call `tellSpliceError` with an
error message.  If there are any errors, Heist initialization will fail and
all the errors will be returned.

### Restructured HeistConfig

The addition of `hcNamespace` and `hcErrorNotBound` to HeistConfig required
some restructuring.  Previously HeistConfig had a Monoid instance, but we
removed that since the new fields make it unclear which instance should be
used.  But we also did not want to completely get rid of the monoid
functionality either.  So in order to get the best of both worlds, we
refactored all of HeistConfig's previous fields into another data structure
called SpliceConfig.  This way we can keep the Monoid instance for
SpliceConfig and still avoid imposing a weird set of semantics on the user.

Unfortunately, given the use of field accessors it was impossible to make this
change without breaking backwards compatibility.  What seems like it should
have been a simple addition of a couple parameters ended up being a more
significant refactoring.  To make these kinds of changes easier in the future
Heist now exports lenses to all of the HeistConfig fields as well as an
`emptyHeistConfig` value to use as a starting point.  These lenses work with
both the lens and lens-family-core packages and we export them without
incurring a dependency on either lens package.  

The HeistConfig constructor and field accessors have been moved to the
Heist.Internal.Types module, so if you really need them, they are still
available.  However, be aware that Internal modules are subject to change
without the deprecation cycle that we use for other modules.

## Minor improvements

  - Factored out SpliceAPI module into separate map-syntax package and
    deprecated the old module which will be removed in the next major release.

  - Snap has been updated to support this Heist 0.14.


