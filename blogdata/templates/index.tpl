<bind tag="subtitle">: Blog</bind>
<bind tag="feed-autodiscovery-link"><link rel="alternate" type="application/atom+xml" href="/blog/feed.xml"/></bind>

<apply template="page">

  <div id="blog-index">
    <h2>
      <a href="/blog">Snap Framework Blog</a>
      <a href="/blog/feed.xml"><img src="/blog/i/feed.png"/></a>
    </h2>
    <table>
      <posts:reverseChronological>
        <tr class="post">
          <td class="date"><post:date/></td>
          <td class="title">
            <a href="$(post:url)"><post:title/></a>
          </td>
          <td class="summary"><post:summary/></td>
        </tr>
      </posts:reverseChronological>
    </table>

    <div class="clear"/>
  </div>

</apply>

