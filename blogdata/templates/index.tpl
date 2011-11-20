
<bind tag="subtitle">: Blog</bind>
<bind tag="feed-autodiscovery-link"><link rel="alternate" type="application/atom+xml" href="/blog/feed.xml"/></bind>

<apply template="page">

  <div id="blog-index">

      <span itemscope="itemscope" itemtype="http://data-vocabulary.org/Breadcrumb">
        <h2 class="top-title"><a href="/blog/feed.xml"><img src="/media/img/rss.png" /></a>Blog</h2>
      </span>


      <posts:reverseChronological>
        <div class="post">
          <div class="title">
            <h3><a href="$(post:url)"><post:title/></a> <span class="date"><post:date/></span></h3>
          </div>
            <p class="summary"><post:summary/> <a class="readmore" href="$(post:url)">Read Post...</a></p>
        </div>
      </posts:reverseChronological>


    <div class="clear"/>
  </div>

</apply>

