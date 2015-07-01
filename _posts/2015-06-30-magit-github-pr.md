---
layout: post
title: Checking out github pull requests locally with magit
---

Github [offers the ref][] `/refs/pull` to download the pull requests
from a repository, this is really useful for reviewing changes,
testing locally, cherry picking, etc; and using it with [magit][]
makes the whole process really simple.

The following snippet automatically adds the refspec of `origin`
remote to the `.git/config` and when called interactively will ask you
for a remote and a local namespace for the refspec, this is useful
when you have a remote `upstream` and want to check its pull requests.

{% gist 21dd3581a3bc656890f4 %}

If you don’t want that automatically adds a refspec in a repository,
you can change disable it with:

```
git config core.disableprref true
```

and for disabling it to the `origin` remote:

```
git config remote.origin.disableprref true
```

p.s. This feature is also available in [Stash][] but [not in bitbucket][].


[magit]: http://magit.vc/
[Stash]: https://www.atlassian.com/software/stash
[offers the ref]: https://help.github.com/articles/checking-out-pull-requests-locally/#tips
[not in bitbucket]: https://bitbucket.org/site/master/issue/5814/reify-pull-requests-by-making-them-a-ref
