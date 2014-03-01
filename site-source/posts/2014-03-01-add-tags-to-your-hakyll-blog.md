---
layout: post
title: Add tags to your Hakyll blog
tags: hakyll, haskell
---

As promised, here is a simple tutorial about adding tag supports
to your Hakyll blog.

Few things before we start:

* My Hakyll version used for this article is
`4.4.3.2`, other versions shouldn\'t have much difference.

* If this article looks too verbose, you can just look at
**bold sentences**.

* All the changes done below is contained [in this archive](/assets/add-tags-mondo.tar.bz2). You can download, run it and skip rest of this article.

Let\'s start from scratch to keep it as simple as possible.

So we initialize a new Hakyll website.

```bash
# initialize the website under dir `mondo`
$ hakyll-init mondo
$ cd mondo
# compile the code necessary,
# in order to see the website.
$ ghc site
```

# Write tags in posts

To create tags, we should first learn how to add tags to our posts,
this step is easy, look at
[the document for tags](https://hackage.haskell.org/package/hakyll-4.4.3.2/docs/Hakyll-Web-Tags.html), it should begin with `tags: ` followed with a comma-separated list.


Now, let\'s modify posts from `mondo/posts`:

`mondo/posts/2012-08-12-spqr.markdown`:

```
---
title: S.P.Q.R.
tags: foo, bar1, bar2
---
```

`mondo/posts/2012-10-07-rosa-rosa-rosam.markdown`:

```
---
title: Rosa Rosa Rosam
author: Ovidius
tags: bar1
---
```

`mondo/post/2012-11-28-carpe-diem.markdown`:

```
---
title: Carpe Diem
tags: bar2, foo
---
```

`mondo/posts/2012-12-07-tu-quoque.markdown`:

```
---
title: Tu Quoque
author: Julius
tags: bar1, bar2
---
```

# Fetch metadata from our posts

Now we\'ve assigned tags to the posts,
next thing is to make them accessible from Haskell codes.

Before we go through all the posts and generate pages,
we should build tags using
[`buildTags`](https://hackage.haskell.org/package/hakyll-4.4.3.2/docs/Hakyll-Web-Tags.html#v:buildTags).

Unforuntately this function is not well-documented,
a short explanation would be :
`buildTags pattern makeId` finds all tags from posts captured by `pattern`,
converts each tag to a corresponding
[`Identifier`](https://hackage.haskell.org/package/hakyll-4.4.3.2/docs/Hakyll-Core-Identifier.html#t:Identifier) by using `makeId`
and returns a value of type
[`Tags`](https://hackage.haskell.org/package/hakyll-4.4.3.2/docs/Hakyll-Web-Tags.html#t:Tags).

From `site.hs` file, **find these lines**:

```{.haskell .numberLines startFrom="24"}
    match "posts/*" $ do
        route $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/post.html"    postCtx
            >>= loadAndApplyTemplate "templates/default.html" postCtx
            >>= relativizeUrls
```

Insert the following code in front of it:

```haskell
-- build up tags
tags <- buildTags "posts/*" (fromCapture "tags/*.html")
```

The code region above says: find all tags by searching the metadate of
posts found by pattern `posts/*`,
and the corresponding path for each tag will be of form `tags/*.html`.
(e.g. for tag `foo`, you can generate a list of all posts that contains tag `foo`
under URL: `{your-website}/tags/foo.html`.)

After tags are generated, we need to tell the post generator to include
the corresponding tag informations for each tag, this is done by modifying `postCtx`.
(for now you don\'t have to understand the detail of `postCtx` if you just want to
setup up tags.)

Put the following definition somewhere in your `site.hs`,
I choose to put it right after the definition of `postCtx`:

```haskell
postCtxWithTags :: Tags -> Context String
postCtxWithTags tags = tagsField "tags" tags `mappend` postCtx
```

And then change all the occurrence of `postCtx` inside the code region mentioned above.

After this change, **the code region should look like**:

```{.haskell .numberLines startFrom="24"}
    -- build up tags
    tags <- buildTags "posts/*" (fromCapture "tags/*.html")

    match "posts/*" $ do
        route $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/post.html"    (postCtxWithTags tags)
            >>= loadAndApplyTemplate "templates/default.html" (postCtxWithTags tags)
            >>= relativizeUrls
```

# Template pages

Now we need to add some changes to our templates,
to make tags visible.

I think the following changes in this section
are self-explanatory even if you knows nothing about
how template works. So Let\'s go though them quickly.

**Modify your `templates/post.html` to make it looks like**:

```html
<div class="info">
    Posted on $date$
    $if(author)$
        by $author$
    $endif$
</div>
<div class="info">
    $if(tags)$
    Tags: $tags$
    $endif$
</div>

$body$
```

Now we create the template for tag pages which
lists all posts containing the corresponding tags.
Since we already have a template for listing posts(`template/post-list.html`),
we can simply reuse it.

This is done by **creating a new file: `templates/tag.html`,
with the following content**:

```html
$partial("templates/post-list.html")$
```

# Generate tag pages

This is the final step, we generate tag pages based on the templates we\'ve written.

**Put the following code somewhere after we build up `tags`,**
I choose to place it right after the line `tags <- buildTags`:

```haskell
    tagsRules tags $ \tag pattern -> do
        let title = "Posts tagged \"" ++ tag ++ "\""
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll pattern
            let ctx = constField "title" title
                      `mappend` listField "posts" postCtx (return posts)
                      `mappend` defaultContext

            makeItem ""
                >>= loadAndApplyTemplate "templates/tag.html" ctx
                >>= loadAndApplyTemplate "templates/default.html" ctx
                >>= relativizeUrls

```

Now it\'s done, recompile `site.hs` and have fun!

# Result

Few screenshots after we adding tags:

![Inside posts](/assets/add-tags-result-1.jpg)

![Tag page](/assets/add-tags-result-2.jpg)

I also provide the final version of `mondo` directory
[here](/assets/add-tags-mondo.tar.bz2).

# Acknowledgement

Thanks for the big help from
[the source code](https://github.com/eakron/variadic.me)
of [variadic.me](https://variadic.me/).

