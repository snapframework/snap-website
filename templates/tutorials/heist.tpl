<snap:apply template="page">
<div id="snap-templates"
><h1
  >Snap Templates</h1
  ><p
  >Snap templates serve two primary design goals. First, they facilitate the separation of the view from the other aspects of your application. Second, they provide abstraction capabilities that allow you to avoid repeated template code. This allows you to follow the DRY principle (Don't Repeat Yourself) in the development of your application views.</p
  ><p
  >Snap has two primary template abstraction constructs: bind and apply. They are implemented as specialized XML tags in the snap namespace.</p
  ><div id="snap:bind-..."
  ><h2
    ><code
      >&lt;snap:bind ...&gt;</code
      ></h2
    ><p
    >The <code
      >snap:bind</code
      > tag allows you to bind XML content to a single tag. Whenever the bound tag is used, the template engine will substitute the content in its place. This allows you to essentially create your own higher-level markup language that snap transforms into whatever XML-based markup language is native to your application.</p
    ><div id="attributes"
    ><h3
      >Attributes</h3
      ><p
      >The <code
	>snap:bind</code
	> tag has a single required attribute called <code
	>tag</code
	> specifying the name of the bound tag. If this attribute is not present, then the <code
	>snap:bind</code
	> tag has no effect.</p
      ></div
    ><div id="example"
    ><h3
      >Example</h3
      ><p
      >Here's a simple example demonstrating the use of bind.</p
      ><pre class="html"
      ><code
	>  &lt;snap:bind tag=&quot;longname&quot;&gt;
    Einstein, Feynman, Heisenberg, and Newton Reasearch Corporation
Ltd.&lt;sup&gt;TM&lt;/sup&gt;
  &lt;/snap:bind&gt;
  We at &lt;longname/&gt; have research expertise in many areas of physics.
Employment at &lt;longname/&gt; carries significant prestige.  The rigorous
hiring process developed by &lt;longname/&gt; is leading the industry.
</code
	></pre
      ><p
      >The full company name will be substituted at every occurrance of the <code
	>&lt;longname/&gt;</code
	> tag. This eliminates repetition and makes it easier to make changes.</p
      ></div
    ></div
  ><div id="snap:apply-..."
  ><h2
    ><code
      >&lt;snap:apply ...&gt;</code
      ></h2
    ><p
    >The <code
      >snap:apply</code
      > tag loads one of your application templates and inserts it into the current template's XML tree. If the target template does not have any special tags, then the contents of the <code
      >snap:apply</code
      > tag are ignored.</p
    ><div id="attributes-1"
    ><h3
      >Attributes</h3
      ><p
      >The <code
	>snap:apply</code
	> tag has one required attribute called <code
	>template</code
	>. This attribute specifies the path to the template being applied. <strong
	><code
	  >TODO</code
	  ></strong
	>: describe template path behavior.</p
      ></div
    ><div id="example-1"
    ><h3
      >Example</h3
      ><p
      >Let's look at a simple example to demonstrate the most basic use of the <code
	>snap:apply</code
	> tag. Say you have a navigation menu that is used on many different pages of your site. You want to avoid duplicating the HTML code in multiple different page templates, so you might put it in a template file by itself called <code
	>nav.tpl</code
	> that looks like this:</p
      ><pre class="html"
      ><code
	>  &lt;ul&gt;
    &lt;li&gt;&lt;a href=&quot;/&quot;&gt;Home&lt;/a&gt;&lt;/li&gt;
    &lt;li&gt;&lt;a href=&quot;/faq&quot;&gt;FAQ&lt;/a&gt;&lt;/li&gt;
    &lt;li&gt;&lt;a href=&quot;/contact&quot;&gt;Contact&lt;/a&gt;&lt;/li&gt;
  &lt;/ul&gt;
</code
	></pre
      ><p
      >Then to include this nav template in your front page template, you would use the <code
	>snap:apply</code
	> tag. Here is what a simple home page template <code
	>home.tpl</code
	> might look like:</p
      ><pre class="html"
      ><code
	>  &lt;html&gt;
    &lt;head&gt;
      &lt;title&gt;Home Page&lt;/title&gt;
    &lt;/head&gt;
    &lt;body&gt;
      &lt;h1&gt;Home Page&lt;/h1&gt;
      &lt;snap:apply template=&quot;nav&quot;/&gt;
      &lt;p&gt;Welcome to our home page&lt;/p&gt;
    &lt;/body&gt;
  &lt;/html&gt;
</code
	></pre
      ><p
      >When a user requests the <code
	>/home</code
	> URL, Snap would serve <code
	>home.tpl</code
	> (<strong
	><code
	  >FIXME</code
	  ></strong
	>: depends on the snaplet in question), and the nav template would automatically be inserted into the page. Here is what the HTML will look like after the Snap processes the template:</p
      ><pre class="html"
      ><code
	>  &lt;html&gt;
    &lt;head&gt;
      &lt;title&gt;Home Page&lt;/title&gt;
    &lt;/head&gt;
    &lt;body&gt;
      &lt;h1&gt;Home Page&lt;/h1&gt;
      &lt;ul&gt;
        &lt;li&gt;&lt;a href=&quot;/&quot;&gt;Home&lt;/a&gt;&lt;/li&gt;
        &lt;li&gt;&lt;a href=&quot;/faq&quot;&gt;FAQ&lt;/a&gt;&lt;/li&gt;
        &lt;li&gt;&lt;a href=&quot;/contact&quot;&gt;Contact&lt;/a&gt;&lt;/li&gt;
      &lt;/ul&gt;
      &lt;p&gt;Welcome to our home page&lt;/p&gt;
    &lt;/body&gt;
  &lt;/html&gt;
</code
	></pre
      ></div
    ></div
  ><div id="snap:get-..."
  ><h2
    ><code
      >&lt;snap:get ...&gt;</code
      ></h2
    ><p
    >Sometimes it is useful to pass information (usually in the form of XML data) into the template when it is applied so the template can insert it in useful places. This allows you to build page templates that are not just static blocks of code. If you are a programmer, you can think of a template as if it was a function that could have any number of parameters.</p
    ><p
    >In our previous example, we did not pass any parameters to the <code
      >nav</code
      > template when it was applied, so the <code
      >&lt;snap:apply&gt;</code
      > tag was empty. If we include data inside the body of the <code
      >&lt;snap:apply&gt;</code
      > tag, the template being called can access this data with the <code
      >&lt;snap:get&gt;</code
      > tag. The following simple example illustrates this concept. We create a site template called <code
      >default.tpl</code
      >:</p
    ><pre class="html"
    ><code
      >&lt;html&gt;
  &lt;head&gt;
    &lt;title&gt;Home Page&lt;/title&gt;
  &lt;/head&gt;
  &lt;body&gt;
    &lt;div id=&quot;header&quot;&gt;
      &lt;h1&gt;XYZ Inc.&lt;/h1&gt;
    &lt;/div&gt;
    &lt;div id=&quot;content&quot;&gt;
      &lt;snap:get /&gt;
    &lt;/div&gt;
    &lt;div id=&quot;footer&quot;&gt;
      &lt;p&gt;Copyright XYZ Inc&lt;/p&gt;
    &lt;/div&gt;
  &lt;/body&gt;
&lt;/html&gt;
</code
      ></pre
    ><p
    >The <code
      >&lt;snap:get&gt;</code
      > tag &quot;pulls in&quot; the page content from the calling template and inserts it into the content <code
      >&lt;div&gt;</code
      >.</p
    ><p
    >Now we have a template for our home page called home.tpl:</p
    ><pre class="html"
    ><code
      >&lt;snap:apply template=&quot;default&quot;&gt;
  &lt;h1&gt;Home Page&lt;/h1&gt;
  &lt;p&gt;Welcome to XYZ Inc&lt;/p&gt;
&lt;/snap:apply&gt;
</code
      ></pre
    ><p
    >And when Snap receives a request to <code
      >/home</code
      >, it will serve the following:</p
    ><pre class="html"
    ><code
      >&lt;html&gt;
  &lt;head&gt;
    &lt;title&gt;Home Page&lt;/title&gt;
  &lt;/head&gt;
  &lt;body&gt;
    &lt;div id=&quot;header&quot;&gt;
      &lt;h1&gt;XYZ Inc.&lt;/h1&gt;
    &lt;/div&gt;
    &lt;div id=&quot;content&quot;&gt;
      &lt;h1&gt;Home Page&lt;/h1&gt;
      &lt;p&gt;Welcome to XYZ Inc&lt;/p&gt;
    &lt;/div&gt;
    &lt;div id=&quot;footer&quot;&gt;
      &lt;p&gt;Copyright XYZ Inc&lt;/p&gt;
    &lt;/div&gt;
  &lt;/body&gt;
&lt;/html&gt;
</code
      ></pre
    ><p
    >The two lines from inside the <code
      >&lt;snap:apply&gt;</code
      > tag have been substituted into the content div in <code
      >default.tpl</code
      >. Notice the difference between these two examples. In the first example we pulled in a template (<code
      >nav.tpl</code
      >) that went inside the page being served (<code
      >home.tpl</code
      >). In the second example, <code
      >home.tpl</code
      > is still the intended target of requests, but the <code
      >default.tpl</code
      > template surrounds the content that home.tpl supplies as an argument. This seems like different behavior, but it is just a different use of the same <code
      >apply</code
      > tag. This illustrates the power of a simple concept like <code
      >apply</code
      >.</p
    ></div
  ><div id="using-bind-and-apply"
  ><h2
    >Using Bind and Apply</h2
    ><p
    >What if, in the above example, we decided that the contents of the header div should be different for different pages? To do this, we need a way to pass multiple parameters into a template. Snap provides this capability with the <code
      >&lt;snap:bind&gt;</code
      > tag. Inside the body of a <code
      >&lt;snap:apply&gt;</code
      > tag, you can have multiple snap:bind tags surrounding data to be passed as separate parameters. Each <code
      >&lt;snap:bind&gt;</code
      > tag must have a name attribute that provides a name for its contents. Then, inside the template, each <code
      >&lt;snap:get&gt;</code
      > tag should have a name attribute indicating the name of the parameter to which it corresponds.</p
    ><p
    >The previous example only needs a few modifications to default.tpl to allow multiple parameters. The <code
      >&lt;snap:get&gt;</code
      > tags now have name attributes:</p
    ><pre class="html"
    ><code
      >&lt;html&gt;
  &lt;head&gt;
    &lt;title&gt;Home Page&lt;/title&gt;
  &lt;/head&gt;
  &lt;body&gt;
    &lt;div id=&quot;header&quot;&gt;
      &lt;header/&gt;
    &lt;/div&gt;
    &lt;div id=&quot;content&quot;&gt;
      &lt;content/&gt;
    &lt;/div&gt;
    &lt;div id=&quot;footer&quot;&gt;
      &lt;p&gt;Copyright XYZ Inc&lt;/p&gt;
    &lt;/div&gt;
  &lt;/body&gt;
&lt;/html&gt;
</code
      ></pre
    ><p
    >And <code
      >home.tpl</code
      > uses the <code
      >&lt;snap:bind&gt;</code
      > tag with a name attribute to define values for the parameters:</p
    ><pre class="html"
    ><code
      >&lt;snap:apply name=&quot;default&quot;&gt;
  &lt;snap:bind tag=&quot;header&quot;&gt;
    &lt;h1&gt;XYZ Inc.&lt;/h1&gt;
  &lt;/snap:bind&gt;
  &lt;snap:bind tag=&quot;content&quot;&gt;
    &lt;h1&gt;Home Page&lt;/h1&gt;
    &lt;p&gt;Welcome to XYZ Inc&lt;/p&gt;
  &lt;/snap:bind&gt;
&lt;/snap:apply&gt;
</code
      ></pre
    ><p
    >The result template for this example is the same as the previous example.</p
    ></div
  ><div id="snap:ignore-..."
  ><h2
    ><code
      >&lt;snap:ignore ...&gt;</code
      ></h2
    ><p
    >In some cases you may want to include example data in a Snap template that should not be rendered when the site is active. Snap provides the <code
      >&lt;snap:ignore&gt;</code
      > tag for this purpose. All <code
      >&lt;snap:ignore&gt;</code
      > tags and their contents will be eliminated in a template's output.</p
    ></div
 ></div
>
</snap:apply>
