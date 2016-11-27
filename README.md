Multiblog
---------

This is a quasi-static blog engine supporting rendering in multiple languages
based on the user agent language.

It is currently not generic enough to be used for anything other than a
personal blog, but the goal is to fully separate the appearance control from
the engine.

Usage
=====

The compiled program expects to find all the content under `content/`
directory.

There are two types of content to display: articles and metas. Articles have a
publication date, and form the main stream of the blog. Metas are pages which
don't have a date, and are typically acessible from the header/footer menu.

All content is written in Markdown and is converted into HTML when rendering.
Each file must have the following attributes:

* Slug, a string to use in the URL.
* Language, for distinguishing between the translated versions of the same
  content.
* Date - required for articles only; the absense of a date makes the content
  into a meta.

Attributes can be specified by either:

* Including an attribute pair in the [YAML metadata block][yaml-metadata].
* For languages: ending a file or a directory name in `-la`, for example, `-en`
  for English.
* For dates: including in the file or directory name, for example,
  `content/2015-01-03-myarticle.md`.
* For slugs: if not explicitly specified in YAML metadata, the file name (or a
  directory name, starting from the most specific) is taken to be a slug, after
  taking out the parts of it parsed as a date or language.

Some elements of the blog other than content itself must also be translated.
The translations use the file `content/strings.yaml` which must contain hashes
of the following structure: string - language - translation.

Google Analytics is supported for the site. The ID must be specified in
`content/analytics.yaml`.

The sidebar links are specified in `content/links.yaml`. They can point either
to metas or to any external address.


Examples
========

All the examples assume `https://blog.example.com/` to be the root address of
the blog.

This article spread across two files for two languages will be displayed under
the URL `https://blog.example.com/2015-01-03/myarticle`:

`content/2015-01-03/article-en.md`:

```markdown
---
slug: myarticle
lang: en
---

# My article

This is the article content in English.
```

`content/2015-01-03/article-zh.md`:

```markdown
---
slug: myarticle
lang: zh
---

# 我的文章

这是该文章的中文内容。
```

Note that the slug and the date are used to match the different translations
of the article. The date is extracted from the directory name where the
article files are stored.

This file does not have a date (either in the name or in the metadata), so it
will be displayed under `https://blog.example.com/about`:

`content/about-en.md`

```markdown
---
slug: about
lang: en
---

# About the blog

This is the site introduction in English.
```

This is an example of string translation file, `content/strings.yaml`:

```yaml
home:
  en: Home
  zh: 首页
```

An example of links file, `content/links.yaml`:

```yaml
- page: about
- url: https://www.yandex.ru
  text:
    en: Yandex
    ru: Яндекс
- url: https://www.github.com
  text: GitHub
```

An example of the analytics file, `content/analytics.yaml`:

```yaml
google: UA-12345678-9
```

[yaml-metadata]: http://johnmacfarlane.net/pandoc/demo/example9/pandocs-markdown.html#extension-yaml_metadata_block
