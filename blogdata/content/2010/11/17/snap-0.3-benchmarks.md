| title: Snap 0.3 Benchmarks with GHC 7.0.1
| author: Doug Beardsley <mightybyte@gmail.com>
| published: 2010-11-17T18:55:00-0500
| updated: 2010-11-17T18:55:00-0500
| summary: New benchmarking results for Snap 0.3 show a ~50% improvement since Snap 0.1 and a 6% improvement due to GHC 7.0.1.

EDIT: Updated the numbers for Apache/PHP using PHP 5.3.3 and using echo
instead of printf.  Also re-ran Rails with version 3.0.3.  Performance
improved in both cases, but not enough to change the rankings.

The recent release of GHC 7.0.1 features some significant performance
improvements, and because the Snap benchmarks have not been updated since we
released six months ago we thought it would be a good time to do so.  We kept
as many things constant as possible, including benchmarks (pong and file),
hardware, OS (although the kernel version is probably different), and httperf
options.

### Pong Benchmark

First let's look at the results for the pong benchmark.  The units are
replies/second, higher numbers are better.  We should note that we are
benchmarking Snap 0.3 which is still in development and scheduled for release
in January.

![](/media/img/pong-bench-20101117.png)

<table>
  <tr>
    <td>RoR</td>
    <td>Apache+PHP</td>
    <td>Happstack</td>
    <td>Snap (logging)</td>
    <td>Node.js</td>
    <td>Snap (no logging)</td>
  </tr>
  <tr>
    <td>288</td>
    <td>9769</td>
    <td>16299</td>
    <td>17800</td>
    <td>22733</td>
    <td>35316</td>
  </tr>
</table>
<br />

If you're comparing this with our [previous
results](/blog/2010/05/23/snap-0.1-benchmarks) you will notice that we left
out Grails.  We discovered that our previous results for Grails may have been
too low because the JVM had not been given time to warm up.  The problem is
that after the JVM warms up for some reason httperf isn't able to get any
samples from which to generate a replies/sec measurement, so it outputs 0.0
replies/sec.  There are also 1000 connreset errors, so we decided the Grails
numbers were not reliable enough to use.

Happstack has improved since the last benchmark, but it has a problem where
performance decreases under heavy loads.  We discovered that using an httperf
rate of 34 almost doubles happstack's performance on the pong benchmark
compared to the rate of 1000 used to test all the other frameworks.  This
brings it to within about 10% of Snap's numbers.  But on these benchmarks
we're more interested in a server's performance under heavy load than it's
peak throughput in optimal conditions, so our graphs show the numbers for a
rate of 1000.  The Happstack guys are planning to investigate this issue in
the near future.

One source of variation among applications is whether they do access logging
or not.  Ruby on Rails, Node.js, and Happstack do not do any access logging.
Our previous Snap benchmarks also did not have access logging turned on.  But
Apache does write access logs, so we decided to add a second entry for Snap
with access logging to facilitate a more accurate comparison.  

### File Benchmark

Here are the results for the file benchmark.  Again, higher numbers are
better.

![](/media/img/file-bench-20101117.png)

<table>
  <tr>
    <td>RoR</td>
    <td>Apache+PHP</td>
    <td>Node.js</td>
    <td>Snap (logging)</td>
    <td>Happstack</td>
    <td>Snap (no logging)</td>
  </tr>
  <tr>
    <td>450</td>
    <td>4308</td>
    <td>4540</td>
    <td>6152</td>
    <td>7554</td>
    <td>10832</td>
  </tr>
</table>
<br />

The relative performance of the frameworks stays pretty much the same, except
that Happstack is good at serving files, while Node.js is slower.  We didn't
try to figure out what rate would optimize Happstack's throughput on the file
benchmark, but it seems reasonable to assume that it won't make as big of a
difference here as it did with pong.

After we published the last results comments
[here](http://news.ycombinator.com/item?id=1380405) and
[here](http://news.ycombinator.com/item?id=1369852) suggested that it is
unfair to benchmark Rails with the Webrick server and that we should use
Thin/Mongrel/nginx+passenger instead.  Benchmarking best of breed against best
of breed is certainly a useful thing to do, but it is also useful to benchmark
a similar level of effort.  Webrick is the out-of-the-box server for Rails, so
it seems fair to compare this with Snap's out-of-the-box solution.  I was able
to benchmark Thin and it performed worse than webrick.  Another person got
similar results and also found that Webrick beat Mongrel as well.  We did not
benchmark nginx+passenger because that requires significant configuration and
doesn't qualify as an out-of-the-box solution.  We also tried to benchmark the
Play web framework but it generated an InternalError exception and stopped
responding to requests.

### Snap Comparison

So what has happened to Snap since our last benchmarks in May?  Here is a
chart comparing the performance of Snap 0.1 and 0.3.

![](/media/img/snap-0.1-0.3-bench.png)

On the pong benchmark we went from 23,997 to 35,316 (replies/sec), a 47%
improvement.  The file benchmark improved from 7,206 to 10,832, a 50%
improvement.  We were curious to see how much of this came from improvements
to Snap and how much was from better GHC code generation.  When compiled with
GHC 6.12.3 the pong benchmark gets 33196 replies/sec.  So upgrading to GHC
7.0.1 gave us a drop-in 6% performance improvement.

This time all of our Snap runs used the runtime options "+RTS -A4M -N4".  The
new version of GHC eliminated the need for the "-qg0 -qb -g1" that we used
before.  Also, we were really looking forward to the new IO manager that is in
GHC 7.0.1, and hoping that it would eliminate the need for our libev backend.
Unfortunately our libev backend still performs significantly better, so we
will continue to ship it.

For those who are interested, here are the versions of the software we used:

* Ruby on Rails 3.0.3
* Grails 1.2.2
* Apache 2.2.17
* PHP 5.3.3
* Node.js 0.2.4
* Snap 0.3
* Play 1.1

Full results are
[here](https://github.com/snapframework/snap-benchmarks/blob/51b1c5b24c09ced61e937d7d5b1d88f8e513663b/results.txt).
Or visit the [snap-benchmarks github
repository](https://github.com/snapframework/snap-benchmarks) if you want the
code.

