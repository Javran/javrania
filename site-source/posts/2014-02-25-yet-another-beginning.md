---
layout: post
title: Yet another beginning
tags: thought
---

I think it\'s not surprising if I say I\'ve made my decision(again) to restart
writing blogs and keeping track of my personal pages.

This time I switch from [jekyll](https://github.com/jekyll/jekyll)
to [hakyll](http://jaspervdj.be/hakyll/).
It doesn\'t mean jekyll is not a good choice, this is just because
I feel like becoming a Haskell fans and of course I\'d like to get more involved
with Haskell stuffs. Hakyll, in this sense, fits me better.

These days I did some sort of work to
apply some themes to make my hakyll site looks better.
And the final choice I come up with is to use
[bootstrap](http://getbootstrap.com/) for a basic framework,
and use [lavish](http://www.lavishbootstrap.com/) to make my own theme.

After I felt good about how the website looks,
I moved my past posts from jekyll to hakyll,
which is actually quite easy and just quites few lines of changes.
It\'s great that Hakyll uses [pandoc](http://johnmacfarlane.net/pandoc/)
, which is another fascinating project written in Haskell, to deal with
HTML file generating. Pandoc just makes the data migration smooth and
in addition makes more source file formats available
(I think this is a big win comparing with jekyll).

And then some other stuffs like tags were implemented last weekends,
but unfortunately when I start searching for tutorials about how can
I enable tags in Hakyll, there is not much documents that I can refer to.
Hakyll does have some documents lying in Hackage, but I believe that
these documents are incomplete and insufficient to work out all the details.

The most helpful thing provided by Hakyll\'s website finally turns out
to be
[these example websites with source codes](http://jaspervdj.be/hakyll/examples.html).
I figured out how to add and organize tags by looking at some source codes
from these websites.

So the next thing I can think about, is to write a tutorial
about how I add tags to my own blog. I believe this will
familiarize myself with Hakyll, and make more features
available to Hakyll newbies like me.

However, you should never expect that tutorial coming out quickly,
as things are getting me really busy these days.
