<bind tag="subtitle">: Blog: <post:title/></bind>
<apply template="page">

  <div id="blog-post">
    <h2><a href="/blog">Blog</a> <span style="color: #ccc;">&raquo;</span> <post:title/></h2>

    <div class="post-meta">
      <div class="post-date"><post:date/></div>
      <div class="post-summary"><post:summary/></div>
    </div>

    <div class="post-content">
      <post:content/>
    </div>
  </div>

</apply>
