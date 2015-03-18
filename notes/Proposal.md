# Bio
Anthony Molinaro has been developing large-scale distributed systems
since the late 90s in many languages and environments. First at Goto.com,
a pioneering company in search advertising, where he helped to develop many
of the core serving pieces in Java, C and Perl. After Goto.com changed its
name to Overture.com and was acquired by Yahoo!, Anthony spent 5 years working
on the content match advertising system written in C. Upon leaving Yahoo! in
2008 he joined a small startup that used Erlang exclusively and extensively.
Later in 2009, Anthony was hired by OpenX where he has since introduced Erlang
and spearheaded its use across a large portion of OpenX’s Global Digital
Revenue Platform.

# Speaker Tagline
Architect/Generalist

# Talk Title
How to Pick a Pool in Erlang without Drowning

# Talk Abstract
When building a system in Erlang one often encounters situations
where a pool of processes or a pool of connections would be convenient
or necessary.

Unfortunately, searches for "connection pool" or "process pool" on a
website like Github can show more than 30 different libraries to use.
Some of these are more popular than others and have even been the subject
of talks or blog posts.  Others lack documentation but have been used in
production settings for years.   The architecture chosen for these pools
often work well under light loads, but can get bogged down under highly
concurrent, or highly volatile loads.  In many cases pooling is actually
not the answer and instead alternate architectures should be used.

So given all this, if you think you need a pool, and want to pick one,
which do you pick?

In this talk I hope to provide a framework for answering that question.

# Talk objectives
This talk aims to provide some amount of guidance around picking the
appropriate pool for an application by exploring some of the reasons
these pools exists, outlining the basic types of architectures used
for these pools, and providing some pros and cons about using the most
popular ones.
