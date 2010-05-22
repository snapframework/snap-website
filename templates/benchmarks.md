Web Framework Benchmark Description
===================================

To compare the performance of Snap to that of other web servers/frameworks, we
ran some simple benchmarks using httperf: pong and file.  In the pong
benchmark, servers sent a 4-byte response of "PONG".  The file benchmark sent a
50KB PNG image file as its response.  Our results and nodes are described
below.

Most of the benchmarks were run with the following httperf command line
options:

~~~~~~~~~~~~~~~~~~
httperf --hog --num-conns 1000 --num-calls 1000 --burst-length 20 --port 3000 --rate 1000 --uri=/pong
~~~~~~~~~~~~~~~~~~

All benchmarks were run on a dual quad-core Xeon E5345 2.33 GHz workstation
with 8 gigs of RAM.  In all these tests, we compiled Snap with "-threaded" and
ran it with "+RTS -A4M -N4 -qg0 -qb -g1".  Using more than four threads caused
the performance to decrease.  This is certainly a good area for future
improvement in Snap.

Disclaimer: We are not experts at optimizing these web frameworks.  It
is almost certain that the results for other frameworks are unfairly
low.  If you're interested in the details, the code we used is in a
[github repository](http://github.com/snapframework/snap-benchmarks).
We always willing to update our benchmarks with optimizations that
don't take an inordinate amount of effort to set up.

### Pong Benchmark

The pong benchmark is quite simple, but there was a large amount of variation
in performance.  Here is a chart of the results:

![](/media/img/pong-bench.png)

#### The Numbers

<table>
  <tr>
    <td>RoR</td>
    <td>Grails</td>
    <td>Happstack</td>
    <td>Apache+PHP</td>
    <td>Snap</td>
  </tr>
  <tr>
    <td>258</td>
    <td>796</td>
    <td>8578</td>
    <td>8843</td>
    <td>23997</td>
  </tr>
</table>
<br />

It should be noted that the Happstack benchmark is not multithreaded.  We
compiled Happstack with -threaded and when run with "+RTS -N4", the test never
finished.  When we aborted httperf, it reported a rate of 12.6
requests/second.  We also tried running it with with the same "+RTS -A4M -N4
-qg0 -qb -g1" runtime options that we used with Snap.  This caused a stack
overflow.

### File Benchmark

The file benchmark tests the ability to serve static content from disk.  The
results here were a little closer together than the pong results, but we still
see the same general pattern.

![](/media/img/file-bench.png)

#### The Numbers

<table>
  <tr>
    <td>RoR</td>
    <td>Grails</td>
    <td>Happstack</td>
    <td>Apache</td>
    <td>Snap</td>
  </tr>
  <tr>
    <td>360</td>
    <td>1769</td>
    <td>2455</td>
    <td>4126</td>
    <td>7206</td>
  </tr>
</table>
<br />

In this test, we had more problems because Happstack does not support
pipelining.  So we switched to the following httperf line for Happstack:

~~~~~~~~~~~~~~~~~~
httperf --hog --num-conns 100000 --num-calls 1 --burst-length 1 --port 8080 --rate 10000 --uri=/FiringGeometry.png
~~~~~~~~~~~~~~~~~~

This means that Happstack's results in this test are not really an
apples-to-apples comparison with the other frameworks, but they're still
somewhat meaningful since they reflect a deficiency in the server.

See the [raw test output](/bench-raw-results.txt) for more detailed
httperf output.

