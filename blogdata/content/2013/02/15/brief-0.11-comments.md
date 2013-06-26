| title: A brief note on 0.11
| author: Doug Beardsley <mightybyte@gmail.com>
| published: 2013-02-15T17:10:00-0500
| updated:   2013-02-15T17:10:00-0500
| summary: Clarification on the significance of the 0.11 release

The end of last month saw the 0.11 release of the heist and snap packages.  It
was not accompanied by our traditional announcement and release notes blog
post, and some people have been asking about it.  Here is a brief description
of what is going on and how the 0.11 release affects you.

*tl;dr* The core functionality of compiled heist is stable, but the API is
still maturing.  0.11 is a significant step in the right direction, but it
didn't get a release announcement because we're making rapid progress and
might make more major releases in the near future.  If you want to start using
compiled heist today in production applications, then you should talk to us
directly on IRC so we can collaborate.

## 0.10

The 0.10 release completely redesigned heist, but those changes did not affect
snap-core and snap-server at all.  Prior to this, we had made a pattern of
making major releases of all our packages in lock step.  But since 0.10 made
no breaking changes to snap-core and snap-server, we decided after much debate
to not introduce a major version bump to -core and -server that did not
actually have any breaking changes.

The main motivator here was that we wanted to get the new Heist code out the
door so we could get more feedback.  We are working up to a 1.0 release and
wanted to have more experience working with the new compiled heist paradigm
before giving it 1.0 status.

## Maturing an API

When 0.10 was released we had been working on it for eight months.  The core
idea of compiled splices was fairly mature and well thought out, but since it
still had not been used in large-scale production applications the API was
immature.  This pattern is not unfamiliar.  The basic concept for interpreted
Heist was there in 0.1, but we didn't discover the incredibly useful
runChildrenWith pattern until 0.6.  It takes time to tease apart the higher level
patterns that make a more friendly API.

The compiled Heist paradigm is no different.  I recently started doing a lot
of work with compiled splices in a real world app.  That is giving me a lot of
feedback that I am using to improve the API.  I released 0.11 early so that
people could become aware of these patterns now, rather than getting used to
the less friendly 0.10 API.  However, the new API is still a work in progress.
I don't know what tomorrow will bring, so I can't predict how it will evolve.
But once I feel the API is more stable we will make full release notes for
everything that happened after 0.10.

