## Heist Templates

Heist is a templating engine based loosely on ideas from the Lift
Web Framework.  It functions as a bridge between your web application
and its views.  Heist templates are HTML (or XML) fragments that are
not required to have a single root element.

Heist is being developed by the Snap Team in conjunction with Snap,
but Heist is a completely standalone library with no dependencies on
Snap.  Snap itself is also template-agnostic and does not depend on
Heist.  In the future, we are planning to use Heist in higher-level
Snap functionality, but the core Snap infrastructure and web server
will remain free of Heist dependencies, leaving the choice of
templating engine up to the user.  That said, we strongly encourage
you to use Heist in your Snap applications so you can take full
advantage of Snap's capabilities.

Heist templates serve two primary design goals.  First, they
facilitate the separation of the view from the other aspects of your
application.  Second, they provide abstraction capabilities that allow
you to avoid repeated template code.  This allows you to follow the
DRY principle (Don't Repeat Yourself) in the development of your
application views.

Heist has two primary template abstraction constructs: bind and apply.
They are implemented as specialized tags.

### The `<bind ...>` tag

The `bind` tag allows you to bind content to a single tag.  Whenever
the bound tag is used, the template engine will substitute the 'bind'
tag's child nodes in its place.  This allows you to essentially create
your own higher-level markup language that Heist transforms into whatever
markup language is native to your application.

#### Attributes

The `bind` tag has a single required attribute called `tag` specifying the
name of the bound tag.  If this attribute is not present, then the
`bind` tag has no effect.

#### Example

Here's a simple example demonstrating the use of bind.

~~~~~~~~~~~~~~~ {.html}
  <bind tag="longname">
    Einstein, Feynman, Heisenberg, and Newton Research Corporation
    Ltd.<sup>TM</sup>
  </bind>
  <p>
    We at <longname/> have research expertise in many areas of physics.
    Employment at <longname/> carries significant prestige.  The rigorous
    hiring process developed by <longname/> is leading the industry.
  </p>
~~~~~~~~~~~~~~~

The full company name will be substituted at every occurrance of the
`<longname/>` tag.  This eliminates repetition and makes it easier to
make changes.

#### Attribute Substitution

Heist also provides attribute string substitution using the same mechanism as
tag substitution.  The syntax is different for attributes since angle bracket
tag syntax is not generally found there.  Within the value of an attribute, use
the delimiter `$(...)` for variable substitution.

~~~~~~~~~~~~~~~ {.html}
  <bind tag="foo">dynamic_name</bind>
  <p name="$(foo)">A paragraph</p>
~~~~~~~~~~~~~~~

The foo identifier is substituted into the name attribute of the paragraph
tag.  The output looks like this:

~~~~~~~~~~~~~~~ {.html}
  <p name="dynamic_name">A paragraph</p>
~~~~~~~~~~~~~~~

If there are non-text nodes in the bind tag's children, the markup is removed,
and just the concatenation of the text inside will be substituted.

### The `<apply ...>` tag

The `apply` tag loads one of your application templates and inserts it
into the current template's node tree.  If the target template does not
have any special tags, then the contents of the `apply` tag are
ignored.

#### Attributes

The `apply` tag has one required attribute called `template`.  This
attribute specifies the name of the template being applied.  Heist
template names work a little differently from traditional paths and
filenames.

If the template name contains a '/' character, then it will behave
like traditional relative and absolute paths.  The root directory will
be the root of your template directory tree, and the current directory
will be the directory containing whatever template is currently being
processed.

If the template name does not have any '/' characters, then Heist
searches in the current directory for a template with that name.  If
it finds one, then Heist applies that template.  The different
behavior is that if the named template is not found in the current
directory, Heist recursively searches up the directory hierarchy
looking for the name.  Heist uses the first template it finds on the
way up that has that name.  If no template is found, then you'll get
an error.

This cascading behavior allows you to put site-wide templates in the
top-level directory and selectively override them in subdirectories
for certain parts of your site.

#### Example

Consider a navigation menu that is used on many different pages of
your site.  You want to avoid duplicating the HTML code in multiple
different page templates, so you might put it in a template file by
itself called `nav.tpl`:

~~~~~~~~~~~~~~~ {.html}
  <ul>
    <li><a href="/">Home</a></li>
    <li><a href="/faq">FAQ</a></li>
    <li><a href="/contact">Contact</a></li>
  </ul>
~~~~~~~~~~~~~~~

Then to include this nav template in your front page template, you
would use the `apply` tag.  Here is what a simple home page template
`home.tpl` might look like:

~~~~~~~~~~~~~~~ {.html}
  <html>
    <head>
      <title>Home Page</title>
    </head>
    <body>
      <h1>Home Page</h1>
      <apply template="nav"/>
      <p>Welcome to our home page</p>
    </body>
  </html>
~~~~~~~~~~~~~~~

When a user requests the `/home` URL, Heist would serve `home.tpl`,
and the nav template would automatically be inserted into the page.
Here is what the HTML will look like after Heist processes the
template:

~~~~~~~~~~~~~~~ {.html}
  <html>
    <head>
      <title>Home Page</title>
    </head>
    <body>
      <h1>Home Page</h1>
      <ul>
        <li><a href="/">Home</a></li>
        <li><a href="/faq">FAQ</a></li>
        <li><a href="/contact">Contact</a></li>
      </ul>
      <p>Welcome to our home page</p>
    </body>
  </html>
~~~~~~~~~~~~~~~


### The `<content>` tag

Sometimes it is useful to pass information (usually in the form of HTML
data) into the template when it is applied so the template can insert
it in useful places.  This allows you to build page templates that are
not just static blocks of code.

In our previous example, we did not pass any parameters to the `nav`
template when it was applied, so the `<apply>` tag was empty.  If we
include data inside the body of the `<apply>` tag, the template being
called can access this data with the `<content>` tag.  The following
simple example illustrates this concept.  We create a site template
called `default.tpl`:

~~~~~~~~~~~~~~~ {.html}
<html>
  <head>
    <title>Home Page</title>
  </head>
  <body>
    <div id="header">
      <h1>XYZ Inc.</h1>
    </div>
    <div id="content">
      <content />
    </div>
    <div id="footer">
      <p>Copyright XYZ Inc</p>
    </div>
  </body>
</html>
~~~~~~~~~~~~~~~


The `<content>` tag "pulls in" the page content from the calling
template and inserts it into the content `<div>`.

Now we have a template for our home page called home.tpl:

~~~~~~~~~~~~~~~ {.html}
<apply template="default">
  <h1>Home Page</h1>
  <p>Welcome to XYZ Inc</p>
</apply>
~~~~~~~~~~~~~~~

And when Heist receives a request to `/home`, it will serve the
following:

~~~~~~~~~~~~~~~ {.html}
<html>
  <head>
    <title>Home Page</title>
  </head>
  <body>
    <div id="header">
      <h1>XYZ Inc.</h1>
    </div>
    <div id="content">
      <h1>Home Page</h1>
      <p>Welcome to XYZ Inc</p>
    </div>
    <div id="footer">
      <p>Copyright XYZ Inc</p>
    </div>
  </body>
</html>
~~~~~~~~~~~~~~~

The two lines from inside the `<apply>` tag have been substituted into
the content div in `default.tpl`.  Notice the difference between these
two examples.  In the first example we pulled in a template
(`nav.tpl`) that went inside the page being served (`home.tpl`).  In
the second example, `home.tpl` is still the intended target of
requests, but the `default.tpl` template surrounds the content that
home.tpl supplies as an argument.  This seems like different behavior,
but it is just a different use of the same `apply` tag.  This
illustrates the power of a simple concept like `apply`.

### Bind and Apply with Multiple Parameters

What if, in the above example, we decided that the contents of the
header div should be different for different pages?  To do this, we
need a way to pass multiple parameters into a template.  Heist
provides this capability with the `<bind>` tag.  Inside the body of a
`<apply>` tag, you can have multiple bind tags surrounding data to be
passed as separate parameters.  Each `<bind>` tag must have a `tag`
attribute that provides a name for its contents just as described
above.  Then, inside the template, those tags will be substituted with
the appropriate data.

The previous example only needs a few modifications to `default.tpl`
to allow multiple parameters.

~~~~~~~~~~~~~~~ {.html}
<html>
  <head>
    <title>Home Page</title>
  </head>
  <body>
    <div id="header">
      <header/>
    </div>
    <div id="content">
      <main/>
    </div>
    <div id="footer">
      <p>Copyright XYZ Inc</p>
    </div>
  </body>
</html>
~~~~~~~~~~~~~~~


And `home.tpl` uses the `<bind>` tag with a name attribute to define
values for the `<header/>` and `<main/>` tags:

~~~~~~~~~~~~~~~ {.html}
<apply template="default">
  <bind tag="header">
    <h1>XYZ Inc.</h1>
  </bind>
  Some in-between text.
  <bind tag="main">
    <h1>Home Page</h1>
    <p>Welcome to XYZ Inc</p>
  </bind>
</apply>
~~~~~~~~~~~~~~~

The result template for this example is the same as the previous
example.

NOTE: In this example the `<content/>` tag is still bound as described
above.  The `<content/>` tag is always bound to the complete contents
of the calling `apply` tag.  However, any `bind` tags inside the apply
will disappear.  If we changed `default.tpl` to the following:

~~~~~~~~~~~~~~~ {.html}
<foo>
  <content/>
</foo>
~~~~~~~~~~~~~~~

Then the above `home.tpl` template would render like this:

~~~~~~~~~~~~~~~ {.html}
<foo>
  Some in-between text.
</foo>
~~~~~~~~~~~~~~~


### The `<ignore>` tag

In some cases you may want to include example data in a Heist template
that should not be rendered when the site is active.  Heist provides
the `<ignore>` tag for this purpose.  All `<ignore>` tags and their
contents will be eliminated in a template's output.


### HTML and XML Syntax

By default, and throughout this tutorial, Heist templates have an extension
of `.tpl`.  When used this way, Heist understands and makes use of HTML 5
syntax.  This is the easiest way to use Heist; your experience writing HTML
will generally serve you well.  It is, however, also possible to name
templates with the extension `.xtpl`.  In this case, Heist will process the
template as an XML document, without any HTML-specific rules.

When templates are written in HTML, Heist is a little less forgiving than
your web browser.  That's because you may write splices that depend on a
certain structure of your document, so when the structure is unclear, Heist
will tell you.  For example, consider this piece of HTML code.

~~~~~~~~~~~~~~~ {.html}
<p>
    These are a few of my favorite things.
    <ul>
        <li>Raindrops on roses and whiskers on kittens
        <li>Bright copper kettles and warm woolen mittens
        <li>Brown paper packages tied up with strings.
    </ul>
</p>
~~~~~~~~~~~~~~~

First, notice that none of the `<li>` tags have end tags.  This is okay.  HTML
allows you to omit the end tags for list items.  However, there is an error
here.  A paragraph in HTML is not allowed to contain a list.  Since you are
also allowed to omit the end tag for `<p>`, Heist ends the paragraph when the
list starts.  But then the explicit closing `</p>` tag is mismatched.  A web
browser will often ignore this, so many people don't even realize it is
incorrect; but since it affects the structure of your document and therefore
might affect a splice, Heist does not ignore it, and reports an error.  You can
fix the error by not using closing tags on `<p>` elements, or by moving the
closing tag to before the list.

HTML mode also includes special rules for tags that are always empty (like
`<br>` and `<input>`), and tags whose contents contain other languages besides
HTML (like `<script>` and `<style>`), among other things.  Heist is also
careful to write its output in a way that web browsers will understand.  For
example, it will always write `<textarea></textarea>` rather than using an
empty tag style, and it will normally avoid escaping characters in attributes
unless it's absolutely needed.  In general, you can not worry about these
things and trust that Heist is writing your output in the most compatible way
it can.

On the other hand, some applications may want to provide templates in
well-formed XML, and need to produce well-formed XML output.  Perhaps you're
using XHTML, or not using HTML at all!  In this case, templates should use
Heist's XML mode by using the `.xtpl` extension on templates.  XML mode
disables Heist's understanding of the HTML language, and treats the template
as plain XML.  There are a few restrictions: for example, processing
instructions are dropped from the document, and you may not define your own
entities and use them in the document; but the output will always be
well-formed XML.


## Heist Programming

Heist lets you bind tags to Haskell code with a splice.  A `Splice` takes
the input node from the template and outputs a list of nodes that get
"spliced" back into the template.  This lets you call haskell functions
from your templates, while ensuring that business logic does not creep into
the presentation layer.

Splices that you write must have the type `mySplice :: Splice m` where
m is the monad of your choice (usually Snap).  `Splice m` is a type
synonym for `TemplateMonad m Template`.  The `getParamNode` function
is a TemplateMonad computation that lets you get the contents of the
splice node.  You can then do any processing you want using
functionality provided by
[xmlhtml](http://hackage.haskell.org/package/xmlhtml).

#### Example

The following is code for a splice that calculates the factorial of a
number.

~~~~~~~~~~~~~~~ {.haskell}
import            Text.Templating.Heist
import qualified  Data.Text as T
import qualified  Text.XmlHtml as X

factSplice :: Splice Snap
factSplice = do
    input <- getParamNode
    let text = T.unpack $ nodeText input
        n = read text :: Int
    return [X.TextNode $ T.pack $ show $ product [1..n]]
~~~~~~~~~~~~~~~

You must tell Heist to bind the splice to a tag using the `bindSplice`
function: `bindSplice "fact" factSplice templateState`.  This returns a
new `TemplateState` with factSplice bound to the `<fact>` tag.  Now, in
any of your templates the snippet `<fact>5</fact>` will render as `120`.

The contents of splices can also be used in attributes using the `$()` syntax
mentioned above.  As above, any markup is ignored, and just the text is
included.  This works because a `<bind>` tag is really just a splice that
returns a constant node list.

### Hooks

Heist provides three hooks to give you more control over the template
rendering process.  They are `onLoad`, `preRun`, and `postRun`.  They
are monadic filter functions that you can use to read or transform
templates at different points in time.  The `onLoad` hook is called
once for every template immediately after it is loaded from disk.
`preRun` and `postRun` are called immediately before and after each
template is rendered.  


### Setting up TemplateState

All of Heist's splices, templates, and hooks are stored in
`TemplateState`.  Heist provides `TemplateState` modifier functions
for configuration.  `emptyTemplateState` gives you reasonable defaults
that you build on to suit your needs.  It takes one argument: the path
to the root of the directory tree where templates are stored.  Let's
look at an example to illustrate.

~~~~~~~~~~~~~~~ {.haskell}
myHeistState =
    addOnLoadHook onLoad $
    addPreRunHook preRun $
    addPostRunHook postRun $
    bindSplice "fact" factSplice (emptyTemplateState "templates")

main = do
    ets <- loadTemplates "templates" myHeistState
    let ts = either error id ets
~~~~~~~~~~~~~~~

In this example we added the custom factSplice function from the last
section to the default set of splices as well as our own hooks.  Then
we called loadTemplates to add templates from the `templates`
directory in the filesystem to our template state.

The three addXYZHook functions above add hooks to the TemplateState.
Previous hooks are not overwritten--the new hooks are appended to any
other hooks that have already been added.  

### Generating Pages with Heist

Once you have built a `TemplateState`, you are ready to generate pages
with Heist.  The two functions you will use most frequently are
`renderTemplate` and `callTemplate`.  `renderTemplate` renders a
template by name and returns it as a `Maybe (Builder, MIMEType)`.  The
`Builder` is from the `blaze-builder` package, and can be converted into
a `ByteString` using `blaze-builder`'s `toByteString` function.  The
`MIMEType` is also a `ByteString`, and contains the MIME content type of
the result (either `text/html` or `text/xml`, with the same character
encoding as the template).

`callTemplate` is similar but is designed for templates that require
parameters to be bound by the caller.  When you're calling the template
from another template with the apply tag, this is easily accomplished by
using `<bind>` within the body of the `<apply>`.  But when you are
"calling" the template from code, binding splices for each of the template's
parameters is cumbersome.  `callTemplate` does this for you.

[This website](http://github.com/snapframework/snap-website) is a good
example of how Heist can be used.  It demonstrates some of the more
sophisticated things you can do with Heist such as custom splices,
dynamic template reloading, and content caching.  In the future much
of the general purpose code in snap-website will be integrated into
Snap.  Another good way to learn about programming Heist is to look at
the [code for the built-in splices](/docs/api/heist).  The code
for these splices is in the Text.Templating.Heist.Splices module.

NOTE: It should be emphasized that Heist is still in the early stages
of development, and its interfaces are subject to change.  



