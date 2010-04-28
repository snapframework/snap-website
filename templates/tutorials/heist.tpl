<apply template="page">

  <markdown><![CDATA[

FIXME: replace document below with original  *markdown*-formatted text

## TESTING 1-2-3
This should be a [link](http://google.com)

  ]]>
  </markdown>

<div id="TOC"
><ul
  ><li
    ><a href="#heist-templates"
      >Heist Templates</a
      ><ul
      ><li
	><a href="#the-bind-...-tag"
	  >The <code
	    >&lt;bind ...&gt;</code
	    > tag</a
	  ><ul
	  ><li
	    ><a href="#attributes"
	      >Attributes</a
	      ></li
	    ><li
	    ><a href="#example"
	      >Example</a
	      ></li
	    ></ul
	  ></li
	><li
	><a href="#the-apply-...-tag"
	  >The <code
	    >&lt;apply ...&gt;</code
	    > tag</a
	  ><ul
	  ><li
	    ><a href="#attributes-1"
	      >Attributes</a
	      ></li
	    ><li
	    ><a href="#example-1"
	      >Example</a
	      ></li
	    ></ul
	  ></li
	><li
	><a href="#the-content-tag"
	  >The <code
	    >&lt;content&gt;</code
	    > tag</a
	  ></li
	><li
	><a href="#using-bind-and-apply"
	  >Using Bind and Apply</a
	  ></li
	><li
	><a href="#the-ignore-tag"
	  >The <code
	    >&lt;ignore&gt;</code
	    > tag</a
	  ></li
	><li
	><a href="#the-children-tag"
	  >The <code
	    >&lt;children&gt;</code
	    > tag</a
	  ></li
	></ul
      ></li
    ></ul
  ></div
>
<div id="heist-templates"
><h2
  ><a href="#TOC"
    >Heist Templates</a
    ></h2
  ><p
  >Heist templates serve two primary design goals. First, they facilitate the separation of the view from the other aspects of your application. Second, they provide abstraction capabilities that allow you to avoid repeated template code. This allows you to follow the DRY principle (Don't Repeat Yourself) in the development of your application views.</p
  ><p
  >Heist has two primary template abstraction constructs: bind and apply. They are implemented as specialized XML tags.</p
  ><div id="the-bind-...-tag"
  ><h3
    ><a href="#TOC"
      >The <code
	>&lt;bind ...&gt;</code
	> tag</a
      ></h3
    ><p
    >The <code
      >bind</code
      > tag allows you to bind XML content to a single tag. Whenever the bound tag is used, the template engine will substitute the 'bind' tag's child nodes in its place. This allows you to essentially create your own higher-level markup language that Heist transforms into whatever XML-based markup language is native to your application.</p
    ><div id="attributes"
    ><h4
      ><a href="#TOC"
	>Attributes</a
	></h4
      ><p
      >The <code
	>bind</code
	> tag has a single required attribute called <code
	>tag</code
	> specifying the name of the bound tag. If this attribute is not present, then the <code
	>bind</code
	> tag has no effect.</p
      ></div
    ><div id="example"
    ><h4
      ><a href="#TOC"
	>Example</a
	></h4
      ><p
      >Here's a simple example demonstrating the use of bind.</p
      ><pre class="html"
      ><code
	>  &lt;bind tag=&quot;longname&quot;&gt;
    Einstein, Feynman, Heisenberg, and Newton Reasearch Corporation
Ltd.&lt;sup&gt;TM&lt;/sup&gt;
  &lt;/bind&gt;
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
  ><div id="the-apply-...-tag"
  ><h3
    ><a href="#TOC"
      >The <code
	>&lt;apply ...&gt;</code
	> tag</a
      ></h3
    ><p
    >The <code
      >apply</code
      > tag loads one of your application templates and inserts it into the current template's XML tree. If the target template does not have any special tags, then the contents of the <code
      >apply</code
      > tag are ignored.</p
    ><div id="attributes-1"
    ><h4
      ><a href="#TOC"
	>Attributes</a
	></h4
      ><p
      >The <code
	>apply</code
	> tag has one required attribute called <code
	>template</code
	>. This attribute specifies the name of the template being applied. Heist template names work a little differently from traditional paths and filenames.</p
      ><p
      >If the template name contains a '/' character, then it will behave like traditional relative and absolute paths. The root directory will be the root of your template directory tree, and the current directory will be the directory containing whatever template is currently being processed. Absolute template path names start at the root directory. Relative template path names start at the current directory.</p
      ><p
      >If the template name does not have any '/' characters, then Heist searches in the current directory for a template with that name. If it finds one, then Heist applies the template just like you would expect. The different behavior is that if the named template is not found in the current directory, Heist recursively searches up the directory hierarchy looking for the name. Heist uses the first template it finds on the way up that has that name. If no template is found, then you'll get an error.</p
      ><p
      >This cascading behavior allows you to put site-wide templates in the top-level directory and selectively override them in subdirectories for certain parts of your site.</p
      ></div
    ><div id="example-1"
    ><h4
      ><a href="#TOC"
	>Example</a
	></h4
      ><p
      >Let's look at a simple example to demonstrate the most basic use of the <code
	>apply</code
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
	>apply</code
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
      &lt;apply template=&quot;nav&quot;/&gt;
      &lt;p&gt;Welcome to our home page&lt;/p&gt;
    &lt;/body&gt;
  &lt;/html&gt;
</code
	></pre
      ><p
      >When a user requests the <code
	>/home</code
	> URL, Heist would serve <code
	>home.tpl</code
	>, and the nav template would automatically be inserted into the page. Here is what the HTML will look like after Heist processes the template:</p
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
  ><div id="the-content-tag"
  ><h3
    ><a href="#TOC"
      >The <code
	>&lt;content&gt;</code
	> tag</a
      ></h3
    ><p
    >Sometimes it is useful to pass information (usually in the form of XML data) into the template when it is applied so the template can insert it in useful places. This allows you to build page templates that are not just static blocks of code. If you are a programmer, you can think of a template as if it was a function that could have any number of parameters.</p
    ><p
    >In our previous example, we did not pass any parameters to the <code
      >nav</code
      > template when it was applied, so the <code
      >&lt;apply&gt;</code
      > tag was empty. If we include data inside the body of the <code
      >&lt;apply&gt;</code
      > tag, the template being called can access this data with the <code
      >&lt;content&gt;</code
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
      &lt;content /&gt;
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
      >&lt;content&gt;</code
      > tag &quot;pulls in&quot; the page content from the calling template and inserts it into the content <code
      >&lt;div&gt;</code
      >.</p
    ><p
    >Now we have a template for our home page called home.tpl:</p
    ><pre class="html"
    ><code
      >&lt;apply template=&quot;default&quot;&gt;
  &lt;h1&gt;Home Page&lt;/h1&gt;
  &lt;p&gt;Welcome to XYZ Inc&lt;/p&gt;
&lt;/apply&gt;
</code
      ></pre
    ><p
    >And when Heist receives a request to <code
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
      >&lt;apply&gt;</code
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
  ><h3
    ><a href="#TOC"
      >Using Bind and Apply</a
      ></h3
    ><p
    >What if, in the above example, we decided that the contents of the header div should be different for different pages? To do this, we need a way to pass multiple parameters into a template. Heist provides this capability with the <code
      >&lt;bind&gt;</code
      > tag. Inside the body of a <code
      >&lt;apply&gt;</code
      > tag, you can have multiple bind tags surrounding data to be passed as separate parameters. Each <code
      >&lt;bind&gt;</code
      > tag must have a <code
      >tag</code
      > attribute that provides a name for its contents just as described above. Then, inside the template, those tags will be substituted with the appropriate data.</p
    ><p
    >The previous example only needs a few modifications to <code
      >default.tpl</code
      > to allow multiple parameters.</p
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
      &lt;main/&gt;
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
      >&lt;bind&gt;</code
      > tag with a name attribute to define values for the <code
      >&lt;header/&gt;</code
      > and <code
      >&lt;main/&gt;</code
      > tags:</p
    ><pre class="html"
    ><code
      >&lt;apply template=&quot;default&quot;&gt;
  &lt;bind tag=&quot;header&quot;&gt;
    &lt;h1&gt;XYZ Inc.&lt;/h1&gt;
  &lt;/bind&gt;
  Some in-between text.
  &lt;bind tag=&quot;main&quot;&gt;
    &lt;h1&gt;Home Page&lt;/h1&gt;
    &lt;p&gt;Welcome to XYZ Inc&lt;/p&gt;
  &lt;/bind&gt;
&lt;/apply&gt;
</code
      ></pre
    ><p
    >The result template for this example is the same as the previous example.</p
    ><p
    >NOTE: In this example the <code
      >&lt;content/&gt;</code
      > tag is still bound as described above. The <code
      >&lt;content/&gt;</code
      > tag is always bound to the complete contents of the calling <code
      >apply</code
      > tag. However, any <code
      >bind</code
      > tags inside the apply will disappear. If we changed <code
      >default.tpl</code
      > to the following:</p
    ><pre class="html"
    ><code
      >&lt;foo&gt;
  &lt;content/&gt;
&lt;/foo&gt;
</code
      ></pre
    ><p
    >Then the above <code
      >home.tpl</code
      > template would render like this:</p
    ><pre class="html"
    ><code
      >&lt;foo&gt;
  Some in-between text.
&lt;/foo&gt;
</code
      ></pre
    ></div
  ><div id="the-ignore-tag"
  ><h3
    ><a href="#TOC"
      >The <code
	>&lt;ignore&gt;</code
	> tag</a
      ></h3
    ><p
    >In some cases you may want to include example data in a Heist template that should not be rendered when the site is active. Heist provides the <code
      >&lt;ignore&gt;</code
      > tag for this purpose. All <code
      >&lt;ignore&gt;</code
      > tags and their contents will be eliminated in a template's output.</p
    ></div
  ><div id="the-children-tag"
  ><h3
    ><a href="#TOC"
      >The <code
	>&lt;children&gt;</code
	> tag</a
      ></h3
    ><p
    >XML requires that well-formed documents have a single root element. Sometimes you might want to make templates that don't have a single root element. In these situations the <code
      >&lt;children&gt;</code
      > tag is just what you want. When the children tag is rendered, it strips itself off and just returns its child nodes. This allows you to have a single root element where necessary, but have that tag disappear in the rendered output.</p
    ></div
  ></div
>
</apply>
